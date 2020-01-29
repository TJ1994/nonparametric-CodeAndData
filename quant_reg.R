#### Estimate the wage equation using standard quantile regression ####

library(quantreg)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(sampleSelection)
library(stargazer)

#### Load data ####

    cps_adults <- read.csv('cps_adults.csv')
    AB_qreg <- read.delim('corrected_quantreg_beta.txt',sep=',')

#### Select Participants ####
    cps_partic <- cps_adults[cps_adults$i_partic==1,]

# Relevel education
    cps_partic <- within(cps_partic, education <- relevel(education, ref = 'HighSchoolGrad'))

#### Summary Statistics ####

# Select vars
    sum_vars <- c('ihs_incwage','i_female','education','i_white','potexp')
    print(summary(cps_partic[,sum_vars]))

# Create potexp^2
    cps_partic$potexp2 <- cps_partic$potexp **2
    cps_adults$potexp2 <- cps_adults$potexp **2

#### Run Quantile Regression ####
    ttau = seq(0.05,0.95,by=0.05)
    koenker <- rq(ihs_incwage ~ i_female +education_NoHighSchool + education_Bachelor + education_Master + 
                      i_white + potexp + potexp2,
                  data = cps_partic,tau = ttau,method = 'fn')
    # print(summary(koenker))


#### Run conditional mean regression ####

# Heckman type correction
# Syntax: selection equation first, then outcome equation
    heckman <- selection(i_partic ~ i_chldb6*i_female + ihs_nonearninc + ihs_otherfaminc + 
                             potexp + potexp2 + education_NoHighSchool + education_Bachelor + 
                             education_Master + i_white,
              ihs_incwage ~ i_female +education_NoHighSchool + education_Bachelor + education_Master + 
                  i_white + potexp + potexp2,
              data = cps_adults, method = 'ML')
    heckman_coef <- data.frame(coef(heckman,part='outcome'))
    colnames(heckman_coef) <- 'heckman_coef'
    #heckman_coef<- heckman_coef[-nrow(heckman_coef),]  # exclude inverse Mills ratio

# Usual OLS
    ls_wage <- lm(ihs_incwage ~ i_female +education_NoHighSchool + education_Bachelor + education_Master + 
                      i_white + potexp + potexp2,
                  data = cps_partic)
        
    ls_wage_coef <- as.data.frame(coef(ls_wage))
    colnames(ls_wage_coef) <- 'ls_wage_coef'

#### Some data massaging for plotting####    

# Collect uncorrected quantile regression coefficients
    betas <- as.data.frame(t(koenker$coefficients))

# Rename some columns to ensure consitency
    names(AB_qreg)[names(AB_qreg) == 'X.Intercept.'] <- '(Intercept)'
    names(AB_qreg)[names(AB_qreg) == 'female'] <- 'i_female'
    names(AB_qreg)[names(AB_qreg) == 'white'] <- 'i_white'
    names(AB_qreg)[names(AB_qreg) == 'educ_NoHighSchool'] <- 'education_noHighSchool'
    names(AB_qreg)[names(AB_qreg) == 'educ_Bachelor'] <- 'education_Bachelor'
    names(AB_qreg)[names(AB_qreg) == 'educ_Master'] <- 'education_Master'

# Stack beta coefficients of two regressions
    ld <- betas %>%
        tidyr::gather(factor, level)
    
    ld2 <- AB_qreg %>% 
     tidyr::gather(factor,level)

# Assign grouping to coefficients
    names(ld2)[names(ld2) == 'level'] <- 'corrected'
    names(ld)[names(ld) == 'level'] <- 'not_corrected'
    
    ld$corrected <- ld2$corrected

# Stack coefficients from both specifications
    ld3 <- ld %>% 
        pivot_longer(-factor,names_to = 'var',values_to = 'coeff')


# Make sure tau is repeated for each spec
    df_tau <- as.data.frame(rep(ttau,times = dim(betas)[2]))
    df_tau <- df_tau %>%
        slice(rep(1:n(),each=2))
    colnames(df_tau) <- 'tau'
    
    ld3$tau <- df_tau$tau

# Stack mean regression results 
# For each tau this should be the same coeff value
    df_meanreg <- data.frame(ls_wage_coef,heckman_coef)
    df_meanreg <- cbind(rownames(df_meanreg),df_meanreg)
    df_meanreg <- df_meanreg %>%
        slice(rep(1:n(),each=length(ttau)*2))
    
    ld3[c('LS','Heckman')] <- df_meanreg[c('ls_wage_coef','heckman_coef')]

#### Calculate differences between estimates for different specs #### 
    diff <- as.data.frame(ld3$coeff[ld3$var == 'corrected'] - ld3$coeff[ld3$var == 'not_corrected'])
    colnames(diff) <- 'diff'
    diff$factor <- ld$factor
    diff$tau <- ttau

#### Plots #### 

# Plot with grouping on spec
    ld3 %>%
        ggplot(aes(x= tau, y = coeff, group=var, color = var)) + geom_point() + 
        geom_line() + geom_hline(aes(yintercept= LS,color='LS')) + 
        geom_hline(aes(yintercept = Heckman, color='Heckman'))+ 
        facet_wrap(~factor,scales='free') + theme_light()
    
    diff %>%
        ggplot(aes(x= tau, y = diff)) + geom_point() + 
        geom_line() + facet_wrap(~factor,scales='free') + theme_light()
    
    

       