library(dplyr)
library(tidyr)
library(data.table)
library(stringi)
library(dfidx)
library("mlogit")
setwd("C:/Users/Veronica/Documents/hw3")
# Exercise 1#
#1.1==============================================================================
datstu=read.csv(file="datstu_v2.csv",na.strings = c(" ", "NA", ""))
datsss=read.csv(file="datsss.csv")
datjss=read.csv(file="datjss.csv",row.names = 1)
#number of students
Number_of_stu1dents = length(datstu$V1)
##[340823]

#number of schools
number_of_schools = datstu %>% 
  select(V1,contains("school")) %>% 
  pivot_longer(cols = !V1, names_to = "School") %>% 
  drop_na() %>%
  distinct(value) 
Number_of_schools = nrow(number_of_schools)
##[640]

#number of programs
number_of_programs = datstu %>% 
  select(V1,contains("pgm")) %>% 
  pivot_longer(cols = !V1, names_to = "Program") %>% 
  distinct(value) %>% 
  na.omit()
Number_of_programs = nrow(number_of_programs)
##[32]

#1.2 =============================================================================================
#Convert the school/program chocie from wide to long
choice = datstu %>%
  select(V1,contains("school"),contains("pgm")) %>% 
  pivot_longer(cols = !V1, names_pattern = "(schoolcode|choicepgm)([[:digit:]])",
               names_to = c(".value","time")) 

number_of_choice = choice %>%
  select(schoolcode,choicepgm) %>%
  group_by(schoolcode,choicepgm) %>%
  distinct() %>% 
  na.omit()
number_of_choice = nrow(number_of_choice)
## the number of choice is 2773

#1.3========================================================================================
jssschool = datstu %>%
  select(V1,contains("school"),jssdistrict) %>%
  pivot_longer(cols = schoolcode1:schoolcode6, names_to = "Schoolorder",
               values_to = "schoolcode") 

datsss1 = datsss %>%
  na.omit(schoolname) %>%
  group_by(schoolcode) %>%
  filter(!duplicated(schoolcode))

jss_sss = datsss1 %>%
  select(schoolcode,sssdistrict) %>%
  left_join(jssschool,by = "schoolcode")

#check if the sssdistrict same to jssdistrict
check_sss_jss_same = jss_sss %>%
  mutate(loc=ifelse(sssdistrict == jssdistrict,1,0)) %>% #find the situation when jssdistrict is same to sssdistrict
  group_by(V1) %>%
  summarise(sum=sum(loc))

number_same_sssjss =
  nrow(check_sss_jss_same[check_sss_jss_same$sum > 0,])
#Number of students applying to at least one senior high school in the same district to home:262194

#1.4-1.6=======================================================================================================================
st_choice = datstu %>% 
  select(V1,score,contains("school"),rankplace) %>%
  na.omit(datstu$score) %>%
  pivot_longer(cols = schoolcode1:schoolcode6, names_to = "Schoolorder",
               values_to = "schoolcode")
st_choice = st_choice %>%
  mutate(schoolorder=substring(Schoolorder,11,12)) #add a new column to extract the order of the school

ss_info_sch = st_choice %>% 
  mutate(ad=ifelse(rankplace == st_choice$schoolorder,1,0)) %>%
  filter(ad == 1)  #find each student's admit information 

admit_cutoff_quality = ss_info_sch %>%
  group_by(schoolcode) %>%
  summarise(number_of_students_admitted = n(),cutoff = min(score),quality = mean(score))

#Exercise 2========================================================================================================================

#First find the admission information at school-program level
st_choice_new = datstu %>% 
  select(V1,score,contains("school"),contains("pgm"),jssdistrict,rankplace) %>%
  pivot_longer(cols = schoolcode1:choicepgm6,
               names_pattern = "(schoolcode|choicepgm)([[:digit:]])",
               names_to = c(".value","time")) %>%
  select(V1,score,schoolcode,choicepgm,time,rankplace,jssdistrict)

ss_info_pro = st_choice_new %>% 
  mutate(ad=ifelse(rankplace == time,1,0)) %>%
  filter(ad == 1) %>% #find each student's admit information 
  select(-time,-rankplace,-ad,-jssdistrict)


ad_cf_qly = ss_info_pro %>%
  group_by(schoolcode,choicepgm) %>%
  summarise(size = n(),cutoff = min(score),quality = mean(score))

#add the district and latitude and longtitude information
Q2_ANSWER = datsss1 %>%
  select(schoolcode,sssdistrict,ssslong,ssslat) %>%
  left_join(ad_cf_qly,by = "schoolcode") %>%
  select(schoolcode,choicepgm,sssdistrict,ssslong,ssslat,cutoff,quality,size)

#Excercise 3========================================================================================================
#First use the df st_choice created in Q2 which convert the wide to long of the schoolcode and pgmcode
#Merge the choice info for each student with jssdistrict info
Q3_data = st_choice_new %>%
  select(V1,time,schoolcode,choicepgm,jssdistrict) %>%
  left_join(datjss,by = "jssdistrict")
#Then merge the senior high school district info
Q3_data = datsss1 %>%
  select(schoolcode,ssslong,ssslat) %>%
  left_join(Q3_data,by = "schoolcode")
#calculate distance
Q3_data$dist = sqrt(
  (69.172*(Q3_data$ssslong- Q3_data$point_x)*cos(Q3_data$point_y/57.3))^2 + 
    (69.172*(Q3_data$ssslat = Q3_data$point_y))^2)
#reorder the column
Q3_answer = Q3_data %>%
  select(V1,time,schoolcode,choicepgm,dist)
  
#Exercise 4==============================================================================================================
#4.1
Q4_data = st_choice_new %>%
  select(-jssdistrict) 

Q4_data$scode_rev = substr(Q4_data$schoolcode,1,3)
 
#4.2
Q4_data$pgm_rev=NULL
Q4_data$pgm_rev[Q4_data$choicepgm == c("General Arts")] = "arts"
Q4_data$pgm_rev[Q4_data$choicepgm == c("Visual Arts")] = "arts"                      
Q4_data$pgm_rev[Q4_data$choicepgm == c("Business")] = "economics" 
Q4_data$pgm_rev[Q4_data$choicepgm == c("Home Economics")] = "economics" 
Q4_data$pgm_rev[Q4_data$choicepgm == c("General Science")] = "science"
Q4_data$pgm_rev[is.na(Q4_data$pgm_rev)==TRUE] = "others"

#4.3
Q4_data$choice_rev = paste0(Q4_data$scode_rev,sep = '_',Q4_data$pgm_rev)

#4.4
#get the admission info for each choice 
ad_info = Q4_data %>% 
  mutate(ad=ifelse(rankplace == time,1,0)) %>%
  filter(ad == 1) %>%
  group_by(choice_rev) %>%
  summarise(cutoff = min(score), quality = mean(score) )

Q4_data_all_chocie = Q4_data %>%
  left_join(ad_info, by = "choice_rev") %>%
  select(V1,score,rankplace,time,choice_rev,scode_rev,pgm_rev,cutoff,quality)

#4.5
Q4_data_wide = Q4_data_all_chocie %>%
  pivot_wider(
    names_from = "time",
    values_from = c("choice_rev","scode_rev","pgm_rev","cutoff","quality")
  ) 

Q4_data_wide = Q4_data_wide[order(-Q4_data_wide$score),]
Q4_data_wide_red = Q4_data_wide[1:20000,]  

#Exercise 5==========================================================================================================
#5.1
#Multinomial logit model is appropriate for this because score varies among students but is invariant across alternatives. For multinomial logit model, 
#Vij =x_j*beta_i,
#x_ij = x_i, so x don??t depend on choice??s characteristic

choice_rev1 = Q4_data_wide_red %>%
  group_by(choice_rev_1)%>%
  summarise() 

n = nrow(choice_rev1)

#form a matrix contains score information(student_i ranks from high score to low score)
score = matrix(rep(unlist(Q4_data_wide_red$score),n),ncol=n)

#form student choice matrix choice_matrix(choice_j ranks from small to big)
choice_matrix=list()
choicerev = as.data.frame(choice_rev1)
for(i in 1:n){
  choice = ifelse(Q4_data_wide_red$choice_rev_1 ==choicerev[i,],1,0)
  choice_matrix = cbind(choice_matrix,choice)
}
choice_matrix = apply(choice_matrix,2,as.numeric) 


# write log-likelihood function for multinomial logit model
MLogit <- function(theta,X,Y){
  alpha = c(0,theta[n:(2*(n-1))])
  alpha = sapply(alpha,rep,20000)
  beta = c(0,theta[1:n-1])
  beta = sapply(beta,rep,20000)
  Vij = X*beta + alpha
  Pij = prop.table(exp(Vij),1)
  Pij[Pij>0.999999] = 0.999999
  Pij[Pij<0.000001] = 0.000001
  logl = sum(Y*log(Pij))
  Y = -logl
  
  
  return(Y)
}

#5.2
#optimization

#package nmlogit is used here to find the vector of coefficients "coeff" but failled becuase of too large data
#Q5_data = Q4_data_wide_red %>%
#  select(score,choice_rev_1) 
#Q5=mlogit.data(Q5_data,shape = "wide",choice="choice_rev_1")
#m = mlogit(choice_rev_1 ~ 0 | score, Q5)

#Then use the random vector served as start value  
# optim(runif((2*(n-1)),-0.5,0.5), MLogit,X=score, Y=choice_matrix,method="BFGS")$par
#for beta(coefficient),which is theta[1:55]here
#for alpha(intercept), which is theta[56:110]here

#marginal effect
optim_result = optim(runif((2*(n-1)),-0.5,0.5), MLogit,X=score,Y=choice_matrix,method="BFGS")$par
theta_mul = as.vector(optim_result)
alpha_mul = c(0,theta_mul[n:(2*(n-1))])
alpha_mulij = sapply(alpha_mul,rep,20000)
beta_mul = c(0,theta_mul[1:n-1])
beta_mulij = sapply(beta_mul,rep,20000)
Vij_mul = score*beta_mulij+alpha_mulij
pij_mul = prop.table(exp(Vij_mul),1)
beta_i_bar = pij_mul[,1]*beta_mulij[,1]
for(i in 2:n){
  beta_i_bar1 = pij_mul[,i]*beta_mulij[,i]
  beta_i_bar = beta_i_bar + beta_i_bar1
}


#calculate marginal effects based on formula ME=pij(betaj-beta_i_bar)
ME_mlogit = pij_mul*(beta_mulij-beta_i_bar)
ME_mlogit_mean = as.matrix(colMeans(ME_mlogit))

#Exercise 6===================================================================================================
#form a quality matrix contains quality of first choice information
quality = as.data.frame(matrix(nrow=20000,ncol=n)) 
for(i in 1:n){
  names(quality)[i]=choicerev[i,]
}
choice_sum=ad_info %>%
  select(choice_rev,quality) %>%
  left_join(choice_rev1,by=c("choice_rev"="choice_rev_1"))

columns = names(quality)
for (column_name in columns){
  quality[column_name]= choice_sum$quality[choice_sum$choice_rev==column_name]
}

#set base choice and calculate tilde price
quality_tilde = NULL
for(i in 1:n){
  quality_til = quality[,i]-quality[,1]
  quality_tilde = cbind(quality_tilde,quality_til)
}

#6.1
#use the conditional logit model because I think quality of first choice here is invariant across alternatives. For the conditional model, 
#Vij = X_j*beta_i,
#X_ij = X_j, x don??t depend on i(individual) but on choice's characteristic. Here is the likelihood function:

CLogit <- function(theta, X){
  # print(X)
  alpha = c(0,theta[2:n])
  alpha = sapply(alpha,rep,20000)
  Vij = X*theta[1]+alpha
  Pij = prop.table(exp(Vij),1)
  # Pij[Pij>0.999999] = 0.999999
  # Pij[Pij<0.000001] = 0.000001
  logl = sum(choice_matrix*log(Pij))
  Y = -logl
  return(Y)
}

#6.2
#Optimization
quality_tilde[is.na(quality_tilde)] = 0

optim_result2 = optim(runif(n,-0.5,0.5), CLogit, X=quality_tilde, 
                      control=list(trace=6, maxit=100),
                      method = "BFGS")$par
#for beta(coefficient),which is theta[1]here, which is equals to 0.01596460, the sign is positive, then if the quality increases, the demand of choosing one of the alternatives will increase. 
theta_cml = as.vector(optim_result2)
alpha_cml = c(0,theta_cml[2:n])
alpha_cml = sapply(alpha_cml,rep,20000)
Vij2 = quality_tilde*theta_cml[1]+alpha_cml
Pij2 = prop.table(exp(Vij2),1)
P_j_bar = as.matrix(colMeans(Pij2))
P_j_bar = t(P_j_bar)
P_j_bar = sapply(P_j_bar,rep,n)
# create a indicator matrix
indicator = as.matrix(diag(c(rep(1,n))))
# calculate marginal effects based on formula
ME1 = P_j_bar *(indicator - P_j_bar)*theta_cml[1]

#Excercise 7 =============================================================================================================================================
#7.1
#I think MLogit model is appropriare because excluding "others" program here is a characteristic of choice but invariant for individual.

#7.2
#Form choice matrix subset excluding others
choice_rev_1_subject = strsplit(Q4_data_wide_red$choice_rev_1,'_')
no_others_index = lapply(choice_rev_1_subject, '[', 2) != 'others'
choice_no_others = Q4_data_wide_red[no_others_index, ]
choice_rev2 = choice_no_others %>%
  group_by(choice_rev_1)%>%
  summarise() 
n2 =nrow(choice_rev2 )
choice_matrix2=list()
choicerev2 = as.data.frame(choice_rev2)
for(i in 1:n2){
  choice = ifelse(choice_no_others$choice_rev_1 ==choicerev2[i,],1,0)
  choice_matrix2 = cbind(choice_matrix2,choice)
}
choice_matrix2 = apply(choice_matrix2,2,as.numeric) 

#Get quality subset from Q6 excluding others
Q7_quality_tilde = cbind.data.frame(Q4_data_wide_red$choice_rev_1,quality_tilde)
for(i in 1:n){
  names(Q7_quality_tilde)[i+1]=choicerev[i,]
}
others = sapply(names(Q7_quality_tilde[,2:(n+1)]), function(x) strsplit(x,'_')[[1]][2]=='others')
others_unique = sapply(choicerev$choice_rev_1, function(x) strsplit(x,"_")[[1]][2]=='others')
quality_no_others = Q7_quality_tilde[,(as.numeric(which(others==FALSE))+1)] %>%
  head(nrow(choice_matrix2))

#Write a new mlogit likehood function
CLogit2 <- function(theta, X){
  alpha = c(0,theta[2:n2])
  alpha = sapply(alpha,rep,nrow(choice_matrix2))
  Vij = X*theta[1]+alpha
  Pij = prop.table(exp(Vij),1)
  Pij[Pij>0.999999] = 0.999999
  Pij[Pij<0.000001] = 0.000001
  logl = sum(choice_matrix2*log(Pij))
  Y = -logl
  return(Y)
}

#Calculate choice probability
optim_result3 = optim(runif(n,-0.5,0.5), CLogit2, X=as.matrix(quality_no_others), 
                      control=list(trace=6, maxit=100),
                      method = "BFGS")$par
  
theta_cml2 = as.vector(optim_result3)
alpha_cml2 = c(0,theta_cml2[2:n2])
alpha_cml2 = sapply(alpha_cml2,rep,nrow(choice_matrix2))
Vij_2 =quality_no_others*theta_cml2[1]+alpha_cml2
Pij_2 = prop.table(as.matrix(exp(Vij_2),1))
P_j_bar_2 = as.matrix(colMeans(Pij_2))
P_j_bar_2 = t(P_j_bar_2)
P_j_bar_2 = sapply(P_j_bar_2,rep,n2)

#7.3 Simulate how these choice probailities change when these choices are excluded

Pij2_no_others = P_j_bar[,-as.numeric(which(others==TRUE))]
others_row = unlist(choice_rev_1_subject)
others_row = which(others_row[seq(2,length(others_row),2)]=='others')
Pij2_no_others = pij2_no_others[-others_row,]
pij_changes= Pij_2[1,]-Pij2_no_others[1,]
