library(ipumsr)
library(writexl)
library(magrittr)
library(dplyr)
library(stats)
library(utils)
library(effects)
library(ggplot2)

### Options ###

# Source the script (instead of run) to automatically set working directory to source file location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

do_save <- 0    # If 1 saves manipulated data set
do_load <- 1    # If 1 loads data set from source file location, put 0 to (re-)calculate number of children and family income
do_write <- 1   # If 1 writes csv data set with calculated participation probailities

### Fetch Data ### 

cps_ddi_file = '../CodeAndData/cps_00004.xml'
cps_data_file = '../CodeAndData/cps_00004.dat'

cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

if (do_load == 1) {
    load('cps_data_addedvars')
}


if (do_load == 0) {

    
    # Keep only relevant variables
    cps_data <- cps_data[,c('SERIAL','PERNUM','MOMLOC','MOMLOC2','POPLOC','POPLOC2','AGE','FAMUNIT','INCTOT','INCWAGE','EDUC','SEX','RACE')]
    
    ### Calculate for each person the number of children following IPUMS definition ###
    
    nchild_func <- function(.x,.y,age_above=-1, age_below = 18) {
        # counts for each person the number of children living in the same household 
        # with age smaller or equal to age_below (default is 18) and larger than age_above (default is -1)
        # Note: set age_above =-1 to include 0 year olds
        # Note: rename column after calling if want to use multiple times (with different cutoffs)
        # Definition of children: own children or children of married or unmarried partner
        # To be called on grouped data set with grouping over Household identifier
        
        nchld_1 <- .x %>% group_by(MOMLOC) %>% summarise(nchld_1=sum(AGE <= age_below & AGE> age_above)) %>% 
            rename(PERNUM = MOMLOC) %>% filter(PERNUM>0)    # MOMLOC gives PERNUM location of first mother
    
        nchld_2 <- .x %>% group_by(MOMLOC2) %>% summarise(nchld_2=sum(AGE <= age_below & AGE > age_above)) %>%
            rename(PERNUM = MOMLOC2) %>% filter(PERNUM>0)   # MOMLOC2 gives PERNUM location of second mother (i.e. same sex couple)
       
        nchld_3 <- .x %>% group_by(POPLOC) %>% summarise(nchld_3=sum(AGE <= age_below & AGE> age_above)) %>% 
            rename(PERNUM = POPLOC) %>% filter(PERNUM>0)    # POPLOC gives PERNUM location of first father
        
        nchld_4 <- .x %>% group_by(POPLOC2) %>% summarise(nchld_4=sum(AGE <= age_below & AGE> age_above)) %>% 
            rename(PERNUM = POPLOC2) %>% filter(PERNUM>0)    # POPLOC2 gives PERNUM location of second father (i.e. same sex couple)
        
        
        .x <- merge(.x,nchld_1,by.x = 'PERNUM', all=TRUE, sort=TRUE)
        .x <- merge(.x,nchld_2,by.x = 'PERNUM', all=TRUE,sort=TRUE)
        .x <- merge(.x,nchld_3,by.x = 'PERNUM', all=TRUE, sort=TRUE)
        .x <- merge(.x,nchld_4,by.x = 'PERNUM', all=TRUE,sort=TRUE)
        
        .x <- .x %>% rowwise() %>% mutate(nchld = sum(nchld_1,nchld_2,nchld_3,nchld_4,na.rm=TRUE)) # Create Output Column giving total number of children
        
    }
    
    # No of children between 0 and 1
    nchld1 <- {cps_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = -1,age_below = 1)}$nchld
    
    # No of children between 1 and 6
    nchld6 <- {cps_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = 1,age_below = 6)}$nchld
    
    # No of children between 6 and 18
    nchld18 <- {cps_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = 6,age_below = 18)}$nchld
    
    
    
    # Append to dataframe
    cps_data$nchld1 <- nchld1
    cps_data$nchld6 <- nchld6
    cps_data$nchld18 <- nchld18
    
    # Total number of children
    cps_data$nchldtot <- cps_data$nchld1 + cps_data$nchld6 + cps_data$nchld18

    ### Calculate Family Income as sum of INCTOT of family members (following IPUMS Family definition) ###
    
    # Make sure NIU (Not in Universe) and Missing are treated as 0
    
    cps_data$INCTOT[cps_data$INCTOT == 99999999] <- 0
    cps_data$INCTOT[cps_data$INCTOT == 99999998] <- 0
    
    cps_data$INCWAGE[cps_data$INCWAGE == 9999999] <- 0
    cps_data$INCWAGE[cps_data$INCWAGE == 9999998] <- 0
    
    
    fam_inc_func <- function(.x,.y) {
        # calculate for each person the respective value of family income
        # where family is identified as FAMUNIT (IPUMS definition)
        
        stopifnot(!(any(.x$INCTOT == 99999999,na.rm=TRUE) | any(.x$INCTOT == 99999998,na.rm=TRUE))) ## NIU or Missing value still numerically decoded
        
        .x <- .x %>% group_by(FAMUNIT) %>% mutate(famtotinc = sum(INCTOT))
    } # End of function
    
    # Append family income as new column to dataframe
    cps_data$faminc <- {cps_data %>% group_split(SERIAL) %>% purrr::map_dfr(fam_inc_func)}$famtotinc
    
    # Other family member's income excl. own income
    cps_data$other_faminc <- cps_data$faminc - cps_data$INCTOT
    
    
    ### Define years of Education ###
    
    educ <- numeric(length= dim(cps_data)[1])
    
    # Let's get ready to hardcode...
    educ[cps_data[,'EDUC'] == 11] <- 1
    educ[cps_data[,'EDUC'] == 12] <- 2
    educ[cps_data[,'EDUC'] == 13] <- 3
    educ[cps_data[,'EDUC'] == 14] <- 4
    educ[cps_data[,'EDUC'] == 10] <- 4 # Grades 1,2,3, or 4
    
    educ[cps_data[,'EDUC'] == 21] <- 5
    educ[cps_data[,'EDUC'] == 22] <- 6
    educ[cps_data[,'EDUC'] == 20] <- 6 # Grades 5 or 6
    
    educ[cps_data[,'EDUC'] == 31] <- 7
    educ[cps_data[,'EDUC'] == 32] <- 8
    educ[cps_data[,'EDUC'] == 30] <- 8 # Grades 7 or 8
    
    educ[cps_data[,'EDUC'] == 40] <- 9
    educ[cps_data[,'EDUC'] == 50] <- 10
    educ[cps_data[,'EDUC'] == 60] <- 11
    
    educ[cps_data[,'EDUC'] == 70] <- 12
    educ[cps_data[,'EDUC'] == 71] <- 12 # Grade 12, no diploma
    educ[cps_data[,'EDUC'] == 72] <- 12 # Grade 12, diploma unclear
    educ[cps_data[,'EDUC'] == 73] <- 12 # High School Diploma or equivalent
    
    educ[cps_data[,'EDUC'] == 80] <- 13
    educ[cps_data[,'EDUC'] == 90] <- 14
    educ[cps_data[,'EDUC'] == 100] <- 15
    educ[cps_data[,'EDUC'] == 110] <- 16
    educ[cps_data[,'EDUC'] == 121] <- 17
    educ[cps_data[,'EDUC'] == 122] <- 18
    
    educ[cps_data[,'EDUC'] == 81] <- 13   #Some college but no degree
    educ[cps_data[,'EDUC'] == 91] <- 14   # Associate's degree, occupational/vocational program
    educ[cps_data[,'EDUC'] == 92] <- 14   # Associate's degree, academic program
    educ[cps_data[,'EDUC'] == 111] <- 16  # Bachelor's degree
    educ[cps_data[,'EDUC'] == 123] <- 18  # Master's degree
    educ[cps_data[,'EDUC'] == 124] <- 18  # Progessional school degree (Postgraduate, MBA??)
    educ[cps_data[,'EDUC'] == 125] <- 20  # Doctorate degree (assuming 4 years on average, direct after Bachelor)
    
    cps_data$educ_yrs <- educ
    
    
    ## 2nd approach: Education as categorial
    
    
    # Recast educational attainnment to less categories
    recast_educ <- cps_data %>% 
        select(EDUC) %>%
        mutate(education = factor(ifelse(EDUC == 0 | EDUC == 1 | EDUC == 2 | EDUC == 10 |
                                             EDUC == 11 | EDUC == 12 | EDUC == 13 | EDUC == 14,'pre-school dropout',
                                         ifelse(EDUC == 20 | EDUC ==  21 | EDUC == 22 | EDUC == 30 | EDUC == 31 |
                                                    EDUC == 32 | EDUC == 32 | EDUC == 40 | EDUC ==  50 | EDUC == 60 | 
                                                    EDUC == 70 | EDUC ==  71 | EDUC == 72, 'High-School dropout',
                                                ifelse(EDUC == 73 | EDUC == 80 | EDUC == 81 | EDUC ==  90 | EDUC == 91 | EDUC == 92 |
                                                        EDUC == 100 | EDUC == 110,'High-School Grad',
                                                              ifelse(EDUC == 111 | EDUC == 120 | EDUC == 121 | EDUC == 122, 'Bachelor Degree',
                                                                     ifelse(EDUC == 123 | EDUC == 124, 'Master Degree','PhD'))))),
                                  levels = c('High-School Grad','pre-school dropout','High-School dropout',
                                             'Bachelor Degree','Master Degree','PhD')))
    
    # Note: High-School Grad is the reference category
    
    cps_data$education <- recast_educ$education   
    
    ### Other variables ### 
    
    # Personal nonearned income #
    cps_data$nonearninc <- cps_data$INCTOT - cps_data$INCWAGE
    
    # Participation Indicator 'Has worked for Pay'
    
    i_partic <- numeric(length= dim(cps_data)[1])
    i_partic[cps_data[,'INCWAGE'] > 1000] <- 1
    
    cps_data$i_partic <- i_partic
    
    # Dummy for children below 6
    cps_data$i_chldb6 <- as.numeric(cps_data$nchld6 > 0 | cps_data$nchld1 >0)
    
    # Female Dummy
    female <- numeric(length= dim(cps_data)[1])
    female[cps_data[,'SEX'] == 2 ] <- 1
    female[cps_data[,'SEX'] != 2 ] <- 0
    cps_data$i_female <- female
    
    # White Dummy
    white <- numeric(length= dim(cps_data)[1])
    white[cps_data[,'RACE'] == 100 ] <- 1
    white[cps_data[,'RACE'] != 100 ] <- 0
    cps_data$i_white <- white
    
    # Save data
    if (do_save == 1){
        save(cps_data,file='cps_data_addedvars')
    } # End of if do_save 

}  # End of if do_load



## Keep only adults between 18 and 64
cps_adults <- cps_data[!(cps_data$AGE < 24 | cps_data$AGE > 55),] 

# Potential Experience
# Calculate as min{AGE - years_of_educ - 6; AGE - 18}
cps_adults$potexp <- apply(data.frame(as.numeric(cps_adults$AGE - cps_adults$educ_yrs - 6), 
                           as.numeric(cps_adults$AGE -18)),1,FUN=min)




# Convert indicators into factors
cps_adults$i_partic <- factor(cps_adults$i_partic)
cps_adults$i_chldb6 <- factor(cps_adults$i_chldb6)
cps_adults$i_white <- factor(cps_adults$i_white)
cps_adults$i_female <- factor(cps_adults$i_female)


# Convert categorial into numeric
cps_adults$nchld1 <- as.numeric(cps_adults$nchld1)
cps_adults$nchld6 <- as.numeric(cps_adults$nchld6)
cps_adults$nchld18 <- as.numeric(cps_adults$nchld18)
cps_adults$nchldtot <- as.numeric(cps_adults$nchldtot)
cps_adults$AGE <- as.numeric(cps_adults$AGE)


## Run Probit on Participation
particprob <- glm(i_partic ~ nchldtot*i_chldb6*i_female  + nonearninc + 
                  other_faminc + poly(AGE,2) + education + i_white, family = binomial(link = 'probit'), 
                data = cps_adults)

particprob2 <- stats::glm(i_partic ~ nchld1*i_female + nchld6*i_female + nchld18*i_female  + nonearninc + 
                             other_faminc  + poly(AGE,2) + education + i_white, family = binomial(link = 'probit'), 
                         data = cps_adults)

particprob3 <- stats::glm(i_partic ~ nchldtot + i_chldb6 + i_female  + nonearninc + 
                             other_faminc + potexp + AGE + education + i_white, family = binomial(link = 'probit'), 
                         data = cps_adults)


print(summary(particprob))
print(summary(particprob2))
print(summary(particprob3))

# Append fitted probit values to dataset
cps_adults$pZ <- particprob$fitted.values

# Save data set
if (do_write == 1) {
    
    write.csv(cps_adults,file = 'cps_adults.csv')
}

y <- predict(particprob, newdata = data.frame('nchldtot' = c(2,2), 'i_chldb6' = factor(c(1,1)),'i_female' =factor(c(0,0)),'nonearninc' = c(10000,10000),
                                              'other_faminc' = c(10000,10000),'potexp' = c(5,5), 'AGE' = c(33,33),
                                              'education' = c('High-School Grad','Bachelor Degree'), 'i_white' = factor(c(1,1))))
    
