setwd("C:/Users/Veronica/Documents/data")

library(pacman)
p_load(tidyverse,knitr)


for(i in 2004:2019){
  datth<-read.csv(paste('Data/dathh',i,'.csv',sep = ""))
  datth$idmen<-as.character(datth$idmen)
  datth$mstatus<-as.character(datth$mstatus)               
  assign(paste('datth_',i,sep = ""),datth)
  
  datind<-read.csv(paste('Data/datind',i,'.csv',sep = ""))
  datind$idind<-as.character(datind$idind)
  datind$idmen<-as.character(datind$idmen)
  assign(paste('datind_',i,sep = ""),datind)
  
}
```

## Exercise 1 Basic Statistics



```{r}
#### 1. Number of households surveyed in 2007
datth_2007 %>% select(idmen) %>% summarise(number=n())
```



```{r}
#### 2. Number of households with marital status ¡°Couple with kids¡± in 2005
datth_2005 %>% filter(mstatus=='Couple, with Kids') %>% summarise(number=n())

```



```{r}
#### 3. Number of individuals surveyed in 2008.
datind_2008 %>% select(idind) %>% summarise(number=n())

```



```{r}
#### 4. Number of individuals aged between 25 and 35 in 2016.
datind_2016 %>% filter(age>=25,age<=35) %>% summarise(number=n())

```



```{r}
#### 5. Cross-table gender/profession in 2009.
table(datind_2009$gender,datind_2009$profession)

```


#### 6. Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, the inter-decile ratio D9/D1 and the Gini coefficient

They are discrete distribution.

```{r}
#mean 2005
mean(datind_2005$wage,na.rm = TRUE)
#mean 2019
mean(datind_2019$wage,na.rm = TRUE)
#sd 2005
sd(datind_2005$wage,na.rm = TRUE)
#sd 2019
sd(datind_2019$wage,na.rm = TRUE)
#D9/D1
quantile(datind_2019$wage,na.rm = TRUE,0.9,names=F)/quantile(datind_2005$wage,na.rm = TRUE,0.9,names=F)
#the Gini coefficient 2005
getGini<-function(v){
  v<-na.omit(v)
  n <- length(v)
  s_v <- sort(v)
  gini <- 1 - ((2/(n+1)) * sum(cumsum(s_v))*(sum(s_v))^(-1))
  return(gini)
}
getGini(datind_2005$wage)
#the Gini coefficient 2019
getGini(datind_2019$wage)
```
#### 7. Distribution of age in 2010. Plot an histogram. Is there any difference between men and women?

It is a  discrete distribution.From the histogram,we can see the difference between men and women 
is that the count number of women bigger than men about age at 50.

```{r}
datind_2010 %>% group_by(gender,age) %>% ggplot(aes(x=age))+geom_histogram()+facet_grid(~gender)
```


```{r}
#### 8.  Number of individuals in Paris in 2011.
datind_2011 %>% inner_join(datth_2011,by='idmen')%>% 
  filter(location=='Paris') %>% summarise(number=n())
```

## Exercise 2 Merge Datasets

```{r}
#Read all individual datasets from 2004 to 2019. Append all these datasets.
datindall<-rbind(datind_2004,datind_2005,datind_2006,
                 datind_2007,datind_2008,datind_2009,
                 datind_2010,datind_2011,datind_2012,
                 datind_2013,datind_2014,datind_2015,
                 datind_2016,datind_2017,datind_2018,
                 datind_2019
)

```


```{r}
#Read all household datasets from 2004 to 2019. Append all these datasets.
datthall<-rbind(datth_2004,datth_2005,datth_2006,
                datth_2007,datth_2008,datth_2009,
                datth_2010,datth_2011,datth_2012,
                datth_2013,datth_2014,datth_2015,
                datth_2016,datth_2017,datth_2018,
                datth_2019
)
```


```{r}
#List the variables that are simultaneously present in the individual and household datasets
common_variables<-c()
for(i in 1:length(names(datindall))){
  tmp<-names(datindall)[i]
  for(j in 1:length(names(datthall))){
    if(tmp==names(datthall)[j]){
      common_variables<-c(common_variables,tmp)
    }
  }
}
print(common_variables)
```


```{r}
#Merge the appended individual and household datasets
merge_all<-datindall %>% inner_join(datthall,by=c('idmen','year'))
```


```{r}
#Number of households in which there are more than four family members
bigger_four<- merge_all %>% group_by(idmen,idind) %>% 
  summarise(number=n()) %>% filter(number>4) 
nrow(bigger_four)
```


```{r}
#Number of households in which at least one member is unemployed
at_leat_one_unemployed<- merge_all %>% group_by(idmen,empstat) %>% 
  filter(empstat=='Unemployed') %>% summarise(number=n()) %>% filter(number>=1) 
nrow(at_leat_one_unemployed)
```


```{r}
#Number of households in which at least two members are of the same profession
at_leat_two_profession<- merge_all %>% filter(profession!='') %>% 
  group_by(idmen,profession) %>% summarise(number=n()) %>% filter(number>=2) 
nrow(at_leat_two_profession)
```


```{r}
#Number of individuals in the panel that are from household-Couple with kids
household_Couple <-merge_all %>% group_by(idmen,idind,mstatus) %>% 
  filter(mstatus=='Couple, with Kids') %>% summarise(number=n())
nrow(household_Couple)
```


```{r}
#Number of individuals in the panel that are from Paris.
merge_all %>% filter(location=='Paris') %>% summarise(number=n())
```


```{r}
#Find the household with the most number of family members. Report its idmen
most_number<-merge_all %>% group_by(idmen,idind) %>% summarise(number=n()) %>% arrange(desc(number)) %>% head(1) 
most_number
most_number$idmen
```
The most number of family member household's idmen is 2202243098040100.


```{r}
#Number of households present in 2010 and 2011.
nrow(merge_all %>% group_by(idmen) %>% filter(year>=2010,year<=2021) %>% summarise(number=n()))
```
## Exercise 3  Migration

```{r}
# Find out the year each household enters and exit the panel. Report the distribution of the time spent
#in the survey for each household.
Migration_data<-merge_all %>% filter(!is.na(myear))
Migration_data <- Migration_data %>% mutate(spend_year=year-myear)
hist(Migration_data$spend_year,freq = F)
```


```{r}
#Based on datent, identify whether or not a household moved into its current dwelling at the year of
#survey. Report the first 10 rows of your result and plot the share of individuals in that situation across years.
merge_all %>% filter(year==datent) %>% head(10)
merge_all %>% filter(!is.na(year),!is.na(datent),!is.na(idind))%>% 
  ggplot(aes(x=(year==datent),y=idind))+geom_histogram(stat = "identity")
```

```{r}
#Based on myear and move, identify whether or not household migrated at the year of survey. Report
#the first 10 rows of your result and plot the share of individuals in that situation across years.
# move
merge_all %>% filter(!is.na(move)) %>% head(10)
# not move
merge_all %>% filter(myear<year) %>% filter(is.na(move)) %>% head(10)

barplot(table(merge_all$idind,merge_all$move))

```

```{r}
# Mix the two plots you created above in one graph, clearly label the graph. Do you prefer one method
#over the other? Justify
par(mfrow=c(2,2))
barplot(table(merge_all$idind,(merge_all$datent==merge_all$myear)))
barplot(table(merge_all$idind,merge_all$move))

```
We prefer the last method,because the method can see the two plots in contrast.

```{r}
# For households who migrate, find out how many households had at least one family member changed
#his/her profession or employment status.

nrow(merge_all %>% filter(!is.na(move),is.na(profession)) %>% group_by(idmen,idind)  %>% summarise(number=n()))

```

## Exercise 4 Attrition

```{r}
#Compute the attrition across each year, where attrition is defined as the reduction in the number 
#of individuals staying in the data panel. Report your final result as a table in proportions.
#Hint: Construct a year of entry and exit for each individual.


attrition_f<-function(year){
  
  temp<-assign(paste('attribution_',year,sep=''),0)
  
  
  datind<-read.csv(paste('Data/datind',year-1,'.csv',sep = ""))
  datind$idind<-as.character(datind$idind)
  datind$idmen<-as.character(datind$idmen)
  last_year<-assign(paste('datind_',year-1,sep = ""),datind)
  
  datind<-read.csv(paste('Data/datind',year,'.csv',sep = ""))
  datind$idind<-as.character(datind$idind)
  datind$idmen<-as.character(datind$idmen)
  this_year<-assign(paste('datind_',year,sep = ""),datind)
  
  for(i in 1:nrow(last_year)){
    
    if(last_year$idind[i]  %in% this_year$idind){
      next
    }else{
      temp<-temp+1
    }
      
    
  }
  return(temp)
}
#2005
attrition_f(2005)
#2006
attrition_f(2006)
#2007
attrition_f(2007)
#2008
attrition_f(2008)
#2009
attrition_f(2009)
#2010
attrition_f(2010)
#2011
attrition_f(2011)
#2012
attrition_f(2012)
#2013
attrition_f(2013)
#2014
attrition_f(2014)
#2015
attrition_f(2015)
#2016
attrition_f(2016)
#2017
attrition_f(2017)
#2018
attrition_f(2018)
#2019
attrition_f(2019)
```
