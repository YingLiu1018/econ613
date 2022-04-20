library(AER)
library(ggplot2)
library(texreg)
library(tidyr)
library(dplyr)
setwd("C:/Users/Veronica/Documents/hw4/Data")
NLSY97 <- read.csv(file="dat_A4.csv",row.names = 1,na.strings = c(" ", "NA", ""))

#Excercise 1===========================================================================================================================================
#1.1
NLSY97$age <- 2019-NLSY97$KEY_BDATE_Y_1997 #Create a new variable age to indicate the birthdate
NLSY97 <- NLSY97 %>%
  rowwise()%>%
  mutate(work_exp = sum(c_across(contains("CV_WKSWK_JOB_DLI"))/52,na.rm = T))

#1.2
#Use YSCH-3113 to record edu variable, assume None is 0, GED(equivalent to high school level) takes 12y, associate degree takes 14y, BA takes 16y, MA takes 18y, phd takes 22y, Professional degree takes 22y
NLSY97 <- NLSY97 %>% mutate(edu = as.numeric(recode(YSCH.3113_2019,'1' = 0, "2" = 12, "3" = 12, "4" = 14, "5" = 16, "6" = 18 , "7" = 22, "8" = 22, "-1" = 0, "-2" = 0)))
NLSY97 <- NLSY97 %>% rename(biod_edu = CV_HGC_BIO_DAD_1997)  %>%
    rename(biom_edu = CV_HGC_BIO_MOM_1997) %>% rename(resd_edu = CV_HGC_RES_DAD_1997) %>% rename(resm_edu = CV_HGC_RES_MOM_1997) 
NLSY97 = mutate_at(NLSY97,c(7:10), ~replace(., is.na(.), 0))
#Recode the parents who are ungraded to 0
NLSY97$biod_edu <- recode(NLSY97$biod_edu,"95"=0)
NLSY97$biom_edu <- recode(NLSY97$biom_edu,"95"=0)
NLSY97$resd_edu <- recode(NLSY97$resd_edu,"95"=0)
NLSY97$resd_edu <- recode(NLSY97$resd_edu,"95"=0)

#1.3.1
#i)
NLSY97_obs <- NLSY97 %>%
  filter(!is.na(YINC_1700_2019),YINC_1700_2019 > 0)# rule out the observations which income is NA or 0
pp1 <- NLSY97_obs %>%
  ggplot(aes(x = as.character(age), y = YINC_1700_2019)) +
  geom_boxplot() + xlab("age") + ylab("income")

#ii)
pp2 <- NLSY97_obs %>%
  ggplot(aes(x = as.character(KEY_SEX_1997) , y = YINC_1700_2019)) +
  geom_boxplot() + xlab("sex") + ylab("income")

#iii)
pp3 <- NLSY97_obs %>%
  ggplot(aes(x = as.character(CV_BIO_CHILD_HH_U18_2019) , y = YINC_1700_2019)) +
  geom_boxplot() + xlab("number of children") + ylab("income")

#1.3.2
#i)

income_zero = NLSY97 %>%
  filter(YINC_1700_2019 == 0)
table = as.table(table(income_zero$age))
table1 = prop.table(table)

#ii)
table = as.table(table(income_zero$KEY_SEX_1997))
table2 = prop.table(table)

#iii)
table = as.table(table(income_zero$CV_MARSTAT_COLLAPSED_2019,income_zero$CV_BIO_CHILD_HH_U18_2019))
table3 = prop.table(table)

#Excercise 2=========================================================================================================================================================
#2.1
NLSY97_full <- NLSY97 %>%
  filter(!is.na(YINC_1700_2019),!is.na(age),!is.na(work_exp),!is.na(edu))# get the full data without NA
observe_y = ifelse(NLSY97_full$YINC_1700_2019 > 0, TRUE,FALSE)
fit <- lm(formula = YINC_1700_2019 ~ age + work_exp + edu, data = NLSY97_full[observe_y,])
summary(fit)

#2.2

#2.3
#Two step approach
#STEP 1: first conducts a probit model regarding whether the individual is observed or not, in order to calculate the inverse mills ratio, or ‘nonselection hazard’
z = rnorm(nrow(NLSY97_full))
probit <- glm(observe_y ~ age + work_exp + edu + z,
              data   = NLSY97_full,
              family = binomial(link = 'probit'))

summary(probit)

probit_lp <- predict(probit)
mills0 <- dnorm(probit_lp)/pnorm(probit_lp)
summary(mills0)

# identical formulation 
imr <- mills0[observe_y]
summary(imr)
ggplot2::qplot(imr, geom = 'histogram')

#STEP 2: Estimate via Linear Regression
#Standard regression model using the inverse mills ratio as covariate
lm_select <- lm(YINC_1700_2019 ~ age + work_exp + edu + imr, data = NLSY97_full[observe_y,])
summary(lm_select)

#Maximum likehood===================================================================================================

#The following likelihood function takes arguments as follows:

# par: the regression coefficients pertaining to the two models, the residual standard error
# sigma and rho for the correlation estimate
# X: observed data model matrix for the linear regression model
# Z: complete data model matrix for the probit model
# y: the target variable
# observed_y: an index denoting whether y is observed
select_ll <- function(par, X, Z, y, observe_y) {
  gamma     = par[1:5]
  lp_probit = Z %*% gamma
  
  beta  = par[6:9]
  lp_lm = X %*% beta
  pr=dnorm(lp_lm)
  pr[pr>0.999999] = 0.9999
  pr[pr<0.000001] = 0.0001
  sigma = par[10]
  rho   = par[11]
  rho = min(rho, 0.999999)
  rho = max(rho, -0.999999)
  pb = 1-pnorm(lp_probit[!observe_y])
  pb[pb <0.000001] = 0.000001
  ll = sum(log(pb)) +
    - log(sigma) +
    sum(dnorm(y, mean = lp_lm, sd = sigma, log = TRUE)) +
    sum(pnorm((lp_probit[observe_y] + rho/sigma * (y-lp_lm)) / sqrt(1-rho^2), 
              log.p = TRUE))

  -ll
}

X = model.matrix(lm_select)
Z = model.matrix(probit)

# initial values
init = c(coef(probit), coef(lm_select)[-5], 1, 0)
#Estimate via optim
fit_unbounded = optim(
  init,
  select_ll,
  X = X[, -5],
  Z = Z,
  y = NLSY97_full$YINC_1700_2019[observe_y],
  observe_y = observe_y,
  method  = 'BFGS',
  control = list(maxit = 1000, reltol = 1e-15),
  hessian = T
)

#Excercise 3=======================================================================================================================
#3.1
ggplot(data= NLSY97_full,aes(x=YINC_1700_2019)) +
  geom_histogram(bins=30)

#3.2-3.4
#Tobit model is appropriate here to solve the censor problem
#Set initial value
initmod = lm(YINC_1700_2019 ~ age + work_exp + edu, data = NLSY97_full)
X = model.matrix(initmod)
init = c(coef(initmod), log_sigma = log(summary(initmod)$sigma))

tobit_ll <- function(par, X, y, ul = -Inf, ll = Inf) {
  
  # this function only takes a lower OR upper limit
  
  # parameters
  sigma = exp(par[length(par)]) 
  beta  = par[-length(par)]
  
  # create indicator depending on chosen limit(here we need upper limit 100000)
  if (!is.infinite(ll)) {
    limit = ll
    indicator = y > ll
  } else {
    limit = ul
    indicator = y < ul
  }

  # linear predictor
  beta = as.matrix(beta)
  lp = X %*% beta
  part1 = sum(indicator * log((1/sigma)*dnorm((y-lp)/sigma)))
  part2 = sum((1-indicator) * log(pnorm((lp-limit)/sigma)))
  pr= pnorm((lp-limit)/sigma)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  # log likelihood
  ll = part1 + part2
  -ll
}

fit_tobit = optim(
  par = init,
  tobit_ll,
  y  = NLSY97_full$YINC_1700_2019,
  X  = X,
  ll = 100000,  #upper limit here
  method  = 'BFGS',
  control = list(maxit = 2000, reltol = 1e-15)
)

#Excercise 4=======================================================================================================================
library(panelr)
Q4 <- read.csv(file="dat_A4_panel.csv",row.names = 1,na.strings = c(" ", "NA", ""))
#prepare the data for converting  to long
Q4 <- Q4 %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1998=CV_HIGHEST_DEGREE_9899_1998)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1999=CV_HIGHEST_DEGREE_9900_1999)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2000=CV_HIGHEST_DEGREE_0001_2000)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2001=CV_HIGHEST_DEGREE_0102_2001)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2002=CV_HIGHEST_DEGREE_0203_2002)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2003=CV_HIGHEST_DEGREE_0304_2003)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2004=CV_HIGHEST_DEGREE_0405_2004)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2005=CV_HIGHEST_DEGREE_0506_2005)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2006=CV_HIGHEST_DEGREE_0607_2006)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2007=CV_HIGHEST_DEGREE_0708_2007)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2008=CV_HIGHEST_DEGREE_0809_2008)%>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2009=CV_HIGHEST_DEGREE_0910_2009)
Q4_long <- long_panel(Q4,prefix='_',begin  = 1997, end = 2019,label_location = "end")
#change variable names
Q4_long <- Q4_long %>% rename(year=wave) %>%rename(income=YINC.1700) %>% rename(mar=CV_MARSTAT_COLLAPSED)
#Create variable we are interested in 
Q4_long<- Q4_long %>%
  rowwise()%>%
  mutate(work_exp = sum(c_across(contains("CV_WKSWK_JOB"))/52,na.rm = T))
Q4_long<- Q4_long %>%
  select(id,year,income,mar,work_exp,CV_HIGHEST_DEGREE_EVER_EDT) %>% na.omit()
Q4_long <- Q4_long %>% 
  mutate(edu = as.numeric(recode(CV_HIGHEST_DEGREE_EVER_EDT,'0' = "0","1"="12", "2" = "12", "3" = "12", "4" = "14", "5" = "16", "6" = "18" , "7" = "22", "8" = "22", "-1" = "0", "-2" = "0"))) %>%
  select(-CV_HIGHEST_DEGREE_EVER_EDT)
Q4_long <- Q4_long %>%
  mutate(martial= as.numeric( recode(mar, "0" = 0, "1" = 1, "2" = 0, "3" = 0, "4" = 0, "-1" = 0, "-2" = 0)))%>%
  select(-mar) #treat Separated Divorced and Widowed as not married

#Within Estimator==================================================================================================================================================================================================================================================================
#obtain demeaned data
ave=aggregate(Q4_long[,2:6], list(Q4_long$id), mean) %>% rename(id=Group.1)
Q4_demeaned = Q4_long %>% left_join(ave, by="id")
Q4_demeaned = Q4_demeaned %>% mutate(income_dif = income.x - income.y) %>% 
  mutate(workexp_dif = work_exp.x - work_exp.y)%>% 
  mutate(edu_dif = edu.x - edu.y)%>% 
  mutate(martial_dif = martial.x - martial.y)
within_model=lm(income_dif~edu_dif + martial_dif + workexp_dif, data= Q4_demeaned)
summary(within_model)

#Between estimator===============================================================================================================================================
between_model = lm(income~edu + martial + work_exp, data= ave)
summary(between_model)

#First difference estimator==================================================================================================================================================
FD = Q4_long %>% group_by(id) %>% mutate(income_fd= income-lag(income)) %>% mutate(edu_fd= edu-lag(edu)) %>% mutate(martial_fd= martial-lag(martial)) %>% mutate(workexp_fd= work_exp-lag(work_exp))
fd_model=lm(income_fd~ edu_fd + martial_fd + workexp_fd, data= FD)
summary(fd_model)
