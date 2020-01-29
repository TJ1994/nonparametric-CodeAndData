library(ipumsr)
library(writexl)
library(magrittr)
library(dplyr)
library(stats)
library(utils)
library(effects)
library(fastDummies)



################################################################################
### Options ###
################################################################################

    # Source the script (instead of run) to automatically set working directory to source file location
        this.dir <- dirname(parent.frame(2)$ofile)
        setwd(this.dir)
        
        do_save <- 0    # If 1 saves manipulated data set
        do_load <- 1    # If 1 loads data set from source file location, put 0 to (re-)calculate number of children and family income
        do_write <- 1   # If 1 writes csv data set with calculated participation probabilities

################################################################################     
### Fetch Data ### 
################################################################################   

    
    cps_ddi_file = 'cps_00006.xml'
    cps_data_file = 'cps_00006.dat'
    
    cps_ddi <- read_ipums_ddi(cps_ddi_file)
    cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

    if (do_load == 1) {
        load('cps_data_addedvars')
    }
    
################################################################################ 
### Data Manipulation ###
################################################################################
    
    if (do_load == 0) {

    
        # Keep only relevant variables
        cps_data <- cps_data[,c('SERIAL','PERNUM','MOMLOC','MOMLOC2','POPLOC','POPLOC2','AGE',
                                'FAMUNIT','INCTOT','INCWAGE','EDUC','SEX','RACE','ASECWT',
                                'CLASSWKR','UHRSWORKLY','AHRSWORKT','UHRSWORKT','WKSWORK1')]
        
        ################################################################################
        
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
        
        # No of children between 0 and 6
        nchld6 <- {cps_data  %>% group_split(SERIAL) %>% purrr::map_dfr(nchild_func,age_above = -1,age_below = 6)}$nchld

        
        # Append to dataframe
        cps_data$nchld6 <- nchld6

        ################################################################################
        
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
        
        ################################################################################
        
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
        
        ################################################################################
        
        ## 2nd approach: Education as categorial
        
        
        # Recast educational attainnment to less categories
        recast_educ <- cps_data %>% 
            select(EDUC) %>%
            mutate(education = factor(ifelse(EDUC == 0 | EDUC == 1 | EDUC == 2 | EDUC == 10 |
                                                 EDUC == 11 | EDUC == 12 | EDUC == 13 | EDUC == 14 |
                                                    EDUC == 20 | EDUC ==  21 | EDUC == 22 | EDUC == 30 | EDUC == 31 |
                                                        EDUC == 32 | EDUC == 32 | EDUC == 40 | EDUC ==  50 | EDUC == 60 | 
                                                        EDUC == 70 | EDUC ==  71 | EDUC == 72, 'NoHighSchool',
                                                    ifelse(EDUC == 73 | EDUC == 80 | EDUC == 81 | EDUC ==  90 | EDUC == 91 | EDUC == 92 |
                                                            EDUC == 100 | EDUC == 110,'HighSchoolGrad',
                                                                  ifelse(EDUC == 111 | EDUC == 120 | EDUC == 121 | EDUC == 122, 'Bachelor',
                                                                          'Master'))),
                                      levels = c('HighSchoolGrad','NoHighSchool',
                                                 'Bachelor','Master')))
        
        # Note: High-School Grad is the reference category
        
        cps_data$education <- recast_educ$education   
        
        ################################################################################
        
        ### Other variables ### 
        
        # Personal nonearned income #
        cps_data$nonearninc <- cps_data$INCTOT - cps_data$INCWAGE
        
       
        # Dummy for children below 6
        cps_data$i_chldb6 <- as.numeric(cps_data$nchld6 > 0)
        
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
        
        ################################################################################
        
        # Save data
        if (do_save == 1){
            save(cps_data,file='cps_data_addedvars')
        } # End of if do_save 

    }  # End of if do_load

################################################################################
### Define sample ###
################################################################################

    ## Keep only adults between 20 and 64
        cps_adults <- cps_data[!(cps_data$AGE < 20 | cps_data$AGE > 64),] 
    
    # Buchinsky (2001) Participation Indicator:
    # Has worked for at least 2 weeks and has positive income
        i_partic <- numeric(length= dim(cps_adults)[1])
        i_partic[cps_adults[,'WKSWORK1']>2 & cps_adults[,'INCWAGE'] > 0] <- 1
        
        cps_adults$i_partic <- i_partic
        
    # Participation Indicator 'Has worked for Pay'
    
        i_partic_pay <- numeric(length= dim(cps_adults)[1])
        i_partic_pay[cps_adults[,'INCWAGE'] >= 10000] <- 1
        
        cps_adults$i_partic_pay <- i_partic_pay
        
    # Participation Indicator 'Has worked more than 35h last week'
        i_partic_hrs <- numeric(length= dim(cps_adults)[1])
        i_partic_hrs[cps_adults$AHRSWORKT >= 35] <- 1
        i_partic_hrs[cps_adults$AHRSWORKT == 999] <- 0
        cps_adults$i_partic_hrs <- i_partic_hrs
        
    
    # Potential Experience
    # Calculate as min{AGE - years_of_educ - 6; AGE - 18}
        cps_adults$potexp <- apply(data.frame(as.numeric(cps_adults$AGE - cps_adults$educ_yrs - 6), 
                                   as.numeric(cps_adults$AGE -18)),1,FUN=min)
        
        cps_adults$potexp2 <- cps_adults$potexp **2
        
################################################################################
### Data conversion ###
################################################################################
        
    # Convert indicators into factors
    # cps_adults$i_partic <- factor(cps_adults$i_partic)
        cps_adults$i_chldb6 <- factor(cps_adults$i_chldb6)
        cps_adults$i_white <- factor(cps_adults$i_white)
        cps_adults$i_female <- factor(cps_adults$i_female)
    
    
    # Convert categorial into numeric
        cps_adults$nchld6 <- as.numeric(cps_adults$nchld6)
        cps_adults$AGE <- as.numeric(cps_adults$AGE)
        
        
    ## Inverse Hyperbolic Sine Transformation on Incomes
        cps_adults$ihs_nonearninc <- log(cps_adults$nonearninc + (1 + cps_adults$nonearninc^2)^(1/2))
        cps_adults$ihs_otherfaminc <- log(cps_adults$other_faminc + (1 + cps_adults$other_faminc^2)^(1/2))
        
        cps_adults$ihs_incwage <- log(cps_adults$INCWAGE + (1 + cps_adults$INCWAGE^2)^(1/2))
        
################################################################################
### Refine Sample ### 
################################################################################
    
    ## Drop observations according to income thresholds ## 
        cps_adults <- cps_adults[cps_adults$nonearninc >= 0,] 
        cps_adults <- cps_adults[cps_adults$other_faminc >= 0,] 
        
        cps_adults <- cps_adults[cps_adults$nonearninc < quantile(cps_adults$nonearninc,0.99),]
        cps_adults <- cps_adults[cps_adults$other_faminc < quantile(cps_adults$other_faminc,0.99),]
    
    ## Drop self-employed ##
        cps_adults <- cps_adults[!(cps_adults$CLASSWKR == 10 | cps_adults$CLASSWKR == 13 | 
                                       cps_adults$CLASSWKR == 14),]
    

        
################################################################################
### Estimation ###
################################################################################        
    
    ## Run Probit on Participation
        particprob <- stats::glm(i_partic ~ i_chldb6*i_female + ihs_nonearninc + ihs_otherfaminc + 
                                     potexp + potexp2 + education + i_white, 
                                 family = binomial(link = 'probit'), 
                        data = cps_adults)
        

        print(summary(particprob))
    
################################################################################
    
    # Append fitted probit values to dataset
        cps_adults$pZ <- particprob$fitted.values
    
    # Create Dummy Columns for Matlab
        cps_adults <- fastDummies::dummy_cols(cps_adults,select_columns = 'education')
    
    # Save data set
        if (do_write == 1) {
            
            write.csv(cps_adults,file = 'cps_adults.csv')
        }
        
################################################################################
### Data Exploration ###
################################################################################ 
        
   
        print(cps_adults %>% group_by(i_female, i_white, i_chldb6,education, i_partic_hrs) %>% summarise(obs = n(),min_nonearninc = min(nonearninc),
                                                                                           max_nonearninc = max(nonearninc), 
                                                                                           min_otherfaminc = min(other_faminc),
                                                                                           max_otherfaminc = max(other_faminc)))
        print(cps_adults %>% group_by(i_partic) %>% summarise(min_nonearninc = min(nonearninc),max_nonearninc = max(nonearninc),
                                                              min_other_faminc = min(other_faminc),max_other_faminc = max(other_faminc)))
    
################################################################################
### Plots ###
################################################################################
        
    # # Plot ihs_nonearninc
    #     plot(x=cps_adults$ihs_nonearninc,
    #          pch = 20,
    #          y = cps_adults$i_partic,
    #          ylim = c(-0.4,1.4),
    #          cex.main = 0.9)
    # 
    # 
    # # Add estimated regression line for nonearninc
    #     x1 <- seq(0,max(cps_adults$ihs_nonearninc),0.1)
    #     y1<- predict(particprob,newdata = data.frame(i_chldb6 = factor(rep(1,length(x1))),i_female = factor(rep(1,length(x1))),
    #                                         ihs_nonearninc = x1, ihs_otherfaminc = rep(min(cps_adults$ihs_otherfaminc),length(x1)),
    #                                         AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                         i_white = factor(rep(1,length(x1)))),
    #                 type='response')
    # 
    #     y2<- predict(particprob,newdata = data.frame(i_chldb6 = factor(rep(0,length(x1))),i_female = factor(rep(1,length(x1))),
    #                                                  ihs_nonearninc = x1, ihs_otherfaminc = rep(min(cps_adults$ihs_otherfaminc),length(x1)),
    #                                                  AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                                  i_white = factor(rep(1,length(x1)))),
    #                  type='response')
    #     
    #     y3<- predict(particprob,newdata = data.frame(i_chldb6 = factor(rep(0,length(x1))),i_female = factor(rep(0,length(x1))),
    #                                                  ihs_nonearninc = x1, ihs_otherfaminc = rep(min(cps_adults$ihs_otherfaminc),length(x1)),
    #                                                  AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                                  i_white = factor(rep(1,length(x1)))),
    #                  type='response')
    #     
    #     lines(x1, y1, lwd = 1.5, col = "steelblue")
    #     lines(x1, y2, lwd = 1.5, col = "red")
    #     lines(x1, y3, lwd = 1.5, col = "green")
    # 
    # # Add horizontal dashed line and text
    #     abline(h = 1, lty = 2, col = "darkred")
    #     abline(h = 0, lty = 2, col = "darkred")
    #     text(max(x1), 0.9, cex = 0.5, "Working for wage")
    #     text(max(x1), -0.1, cex= 0.5, "Not working for wage")
    # 
    # 
    # # Add legend
    #     legend('topleft',
    #            horiz = TRUE,
    #            legend=c('Female and child below 6','Female, no child below 6','male,no child below 6'),
    #            col = c('steelblue','red','green'),
    #            lty = c(1,1,1),
    #            cex = 0.5)
    #     
    # 
    # ### Plot against nonearninc ###
    #     plot(x=cps_adults$nonearninc,
    #          pch = 20,
    #          y = cps_adults$i_partic_hrs,
    #          ylim = c(-0.4,1.4),
    #          cex.main = 0.9)
    # 
    # 
    # # Add estimated regression line for nonearninc
    #     x1 <- seq(0,90000,1000)
    #     y1<- predict(particprob3,newdata = data.frame(i_chldb6 = factor(rep(1,length(x1))),i_female = factor(rep(1,length(x1))),
    #                                                  nonearninc = x1, other_faminc = rep(min(cps_adults$other_faminc),length(x1)),
    #                                                  AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                                  i_white = factor(rep(1,length(x1)))),
    #                  type='response')
    #     
    #     y2<- predict(particprob3,newdata = data.frame(i_chldb6 = factor(rep(0,length(x1))),i_female = factor(rep(1,length(x1))),
    #                                                  nonearninc = x1, other_faminc = rep(min(cps_adults$other_faminc),length(x1)),
    #                                                  AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                                  i_white = factor(rep(1,length(x1)))),
    #                  type='response')
    #     
    #     y3<- predict(particprob3,newdata = data.frame(i_chldb6 = factor(rep(0,length(x1))),i_female = factor(rep(0,length(x1))),
    #                                                  nonearninc = x1, other_faminc = rep(min(cps_adults$other_faminc),length(x1)),
    #                                                  AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                                  i_white = factor(rep(1,length(x1)))),
    #                  type='response')
    #     
    #     y4 <- predict(particprob3,newdata = data.frame(i_chldb6 = factor(rep(1,length(x1))),i_female = factor(rep(0,length(x1))),
    #                                                    nonearninc = x1, other_faminc = rep(min(cps_adults$other_faminc),length(x1)),
    #                                                    AGE = rep(mean(cps_adults$AGE),length(x1)), education = rep('HighSchoolGrad',length(x1)),
    #                                                    i_white = factor(rep(1,length(x1)))),
    #                   type='response')
    #     
    #         lines(x1, y1, lwd = 1.5, col = "steelblue")
    #         lines(x1, y2, lwd = 1.5, col = "red")
    #         lines(x1, y3, lwd = 1.5, col = "green")
    #         lines(x1, y4, lwd = 1.5, lty = 1, col = "grey")
    #             
    #     # Add horizontal dashed line and text
    #         abline(h = 1, lty = 2, col = "darkred")
    #         abline(h = 0, lty = 2, col = "darkred")
    #         text(80000, 0.9, cex = 0.5, "Working for wage")
    #         text(80000, -0.1, cex= 0.5, "Not working for wage")
    #     
    #     
    #     # Add legend
    #         legend('top',
    #                legend = c('Female and child below 6','Female, no child below 6','male,no child below 6','male and child below 6'),
    #                col = c('steelblue','red','green','grey'),
    #                lty = c(1,1,1,1),
    #                cex = 0.5)
    # 

