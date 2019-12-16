library(ipumsr)
library(writexl)
library(magrittr)
library(dplyr)
library(ggplot2)

setwd("~/Documents/Studium/ENSAE/Semester 1/Semi and Non-Parametric Econometrics/Paper/CodeAndData")

cps_ddi_file = 'cps_00004.xml'
cps_data_file = 'cps_00004.dat'

cps_ddi <- read_ipums_ddi(cps_ddi_file)
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

# Turn data (coded as list) into R dataframe
cps_df <- data.frame(cps_data)

#write_xlsx(cps_data,'cps2016.xlsx')

cps_small_data <- head(cps_data,-100000)

df_hh <- cps_small_data[cps_small_data[,'SERIAL'] == 472,]
df_hh <- tbl_df(df_hh[,c('SERIAL','PERNUM','MOMLOC','MOMLOC2','POPLOC','POPLOC2','AGE','FAMUNIT','INCTOT','INCWAGE')])

#chld_id <- (df_hh$MOMLOC %in% df_hh$PERNUM) & (df_hh$AGE <=18)

# df_hh %>% mutate(nchld18 = dim(df_hh[chld_id,])[1])
nchld_1 <- df_hh %>% group_by(MOMLOC) %>% summarise(nchld_1 = sum(AGE<=18)) %>% rename(PERNUM = MOMLOC) %>% filter(PERNUM >0)
nchld_2 <- df_hh %>% group_by(MOMLOC2) %>% summarise(nchld_2 = sum(AGE<=18)) %>% rename(PERNUM = MOMLOC2) %>% filter(PERNUM >0)
nchld_3 <- df_hh %>% group_by(POPLOC) %>% summarise(nchld_3 = sum(AGE<=18)) %>% rename(PERNUM = POPLOC) %>% filter(PERNUM >0)
nchld_4 <- df_hh %>% group_by(POPLOC2) %>% summarise(nchld_4 = sum(AGE<=18)) %>% rename(PERNUM = POPLOC2) %>% filter(PERNUM >0)


df_hh <- merge(df_hh,nchld_1,by.x = 'PERNUM',all=TRUE,sort=TRUE)
df_hh <- merge(df_hh,nchld_2,by.x = 'PERNUM',all=TRUE,sort=TRUE)
df_hh <- merge(df_hh,nchld_3,by.x = 'PERNUM',all=TRUE,sort=TRUE)
df_hh <- merge(df_hh,nchld_4,by.x = 'PERNUM',all=TRUE,sort=TRUE)
df_hh <- df_hh %>% rowwise() %>% mutate(nchld = sum(nchld_1,nchld_2,nchld_3,nchld_4,na.rm=TRUE))


## Family Income
cps_small_data$INCTOT[cps_small_data$INCTOT == 99999999] <- 0
cps_small_data$INCTOT[cps_small_data$INCTOT == 99999998] <- NaN

cps_small_data$INCWAGE[cps_small_data$INCWAGE == 9999999] <- 0
cps_small_data$INCWAGE[cps_small_data$INCWAGE == 9999998] <- NaN

df_fam <- cps_small_data[cps_small_data[,'SERIAL'] == 472,]
df_fam <- tbl_df(df_fam[,c('SERIAL','PERNUM','MOMLOC','MOMLOC2','POPLOC','POPLOC2','AGE','FAMUNIT','INCTOT','INCWAGE')])

print(df_fam %>% group_by(FAMUNIT) %>% mutate(famtotinc = sum(INCTOT)))


cps_small_data <- cps_small_data[,c('SERIAL','PERNUM','MOMLOC','MOMLOC2','POPLOC','POPLOC2','AGE','FAMUNIT','INCTOT','INCWAGE','EDUC','SEX','RACE')]

nchild_func <- function(.x,.y,age_above=0, age_below = 18) {
    # counts for each person the number of children living in the same household 
    # with age smaller or equal to age_cutoff (default is 18)
    # output: appends the result as a column (nchld) 
    # Note: rename column after calling if want to use multiple times (with different cutoffs)
    
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
    
    .x <- .x %>% rowwise() %>% mutate(nchld = sum(nchld_1,nchld_2,nchld_3,nchld_4,na.rm=TRUE)) # Create Output Column giving total number of OWN children
    
}

df_w_nchild <- cps_small_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = -1,age_below = 18) 

nchld1 <- {cps_small_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = -1,age_below = 1)}$nchld

nchld6 <- {cps_small_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = 1,age_below = 6)}$nchld

nchld18 <- {cps_small_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = 6,age_below = 18)}$nchld

cps_small_data$nchld1 <- nchld1
cps_small_data$nchld6 <- nchld6
cps_small_data$nchld18 <- nchld18

## Calculate total family income

fam_inc_func <- function(.x,.y) {
    stopifnot(!(any(.x$INCTOT == 99999999) | any(.x$INCTOT == 99999998))) ## NIU or Missing value still numerically decoded
    
    .x <- .x %>% group_by(FAMUNIT) %>% mutate(famtotinc = sum(INCTOT))
}

cps_small_data$faminc <- {cps_small_data %>% group_split(SERIAL) %>% purrr::map_dfr(fam_inc_func)}$famtotinc

# Other family member's income
cps_small_data$other_faminc <- cps_small_data$faminc - cps_small_data$INCTOT




## Education

educ <- numeric(length= dim(cps_small_data)[1])

# Let's get ready to hardcode...
educ[cps_small_data[,'EDUC'] == 11] <- 1
educ[cps_small_data[,'EDUC'] == 12] <- 2
educ[cps_small_data[,'EDUC'] == 13] <- 3
educ[cps_small_data[,'EDUC'] == 14] <- 4
educ[cps_small_data[,'EDUC'] == 10] <- 4 # Grades 1,2,3, or 4

educ[cps_small_data[,'EDUC'] == 21] <- 5
educ[cps_small_data[,'EDUC'] == 22] <- 6
educ[cps_small_data[,'EDUC'] == 20] <- 6 # Grades 5 or 6

educ[cps_small_data[,'EDUC'] == 31] <- 7
educ[cps_small_data[,'EDUC'] == 32] <- 8
educ[cps_small_data[,'EDUC'] == 30] <- 8 # Grades 7 or 8

educ[cps_small_data[,'EDUC'] == 40] <- 9
educ[cps_small_data[,'EDUC'] == 50] <- 10
educ[cps_small_data[,'EDUC'] == 60] <- 11

educ[cps_small_data[,'EDUC'] == 70] <- 12
educ[cps_small_data[,'EDUC'] == 71] <- 12 # Grade 12, no diploma
educ[cps_small_data[,'EDUC'] == 72] <- 12 # Grade 12, diploma unclear
educ[cps_small_data[,'EDUC'] == 73] <- 12 # High School Diploma or equivalent

educ[cps_small_data[,'EDUC'] == 80] <- 13
educ[cps_small_data[,'EDUC'] == 90] <- 14
educ[cps_small_data[,'EDUC'] == 100] <- 15
educ[cps_small_data[,'EDUC'] == 110] <- 16
educ[cps_small_data[,'EDUC'] == 121] <- 17
educ[cps_small_data[,'EDUC'] == 122] <- 18

educ[cps_small_data[,'EDUC'] == 81] <- 13   #Some college but no degree
educ[cps_small_data[,'EDUC'] == 91] <- 14   # Associate's degree, occupational/vocational program
educ[cps_small_data[,'EDUC'] == 92] <- 14   # Associate's degree, academic program
educ[cps_small_data[,'EDUC'] == 111] <- 16  # Bachelor's degree
educ[cps_small_data[,'EDUC'] == 123] <- 18  # Master's degree
educ[cps_small_data[,'EDUC'] == 124] <- 18  # Progessional school degree (Postgraduate, MBA??)
educ[cps_small_data[,'EDUC'] == 125] <- 20  # Doctorate degree (assuming 4 years on average, direct after Bachelor)

cps_small_data$educ_yrs <- educ

# Personal nonearned income
cps_small_data$nonearninc <- cps_small_data$INCTOT - cps_small_data$INCWAGE

## Participation Indicator 'Has worked for Pay'

I_partic <- numeric(length= dim(cps_small_data)[1])
I_partic[cps_small_data[,'INCWAGE'] > 0] <- 1

# Make sure missing/NIU data is handled as NaN:
I_partic[cps_small_data[,'INCWAGE'] == 9999999] <- NaN    # 9999999 = Not in Universe
I_partic[cps_small_data[,'INCWAGE'] == 9999998] <- NaN    # 9999998 = Missing

cps_small_data$I_partic <- I_partic

## Female Dummy
female <- numeric(length= dim(cps_small_data)[1])
female[cps_small_data[,'SEX'] == 2 ] <- 1
female[cps_small_data[,'SEX'] != 2 ] <- 0
cps_small_data$female <- female

## White Dummy
white <- numeric(length= dim(cps_small_data)[1])
white[cps_small_data[,'RACE'] == 100 ] <- 1
white[cps_small_data[,'RACE'] != 100 ] <- 0
cps_small_data$white <- white

# Save data
save(cps_small_data,file='cps_small_data')

## Keep only adults between 18 and 64
cps_small_adults <- cps_small_data[!(cps_small_data$AGE < 18 | cps_small_data$AGE > 64),] 


## Run Probit on Participation
probit <- glm(I_partic ~ nchld1 + nchld6 + nchld18 + nonearninc + 
                  other_faminc + educ_yrs + female + white, family = binomial(link = 'probit'), 
                data = cps_small_adults)

print(summary(probit))


