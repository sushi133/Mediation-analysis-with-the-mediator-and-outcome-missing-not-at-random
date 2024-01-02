library(dplyr)
library(scales)
library(table1)

#data
data <- read.csv("/Users/sushi5824907/Desktop/Mediation/JASA/Data/Jobcorpdata.csv")
data$x1<-as.factor(data$x1)
data$x2<-as.factor(data$x2)
data$x3<-as.factor(data$x3)
data$x4<-as.factor(data$x4)
data$x5<-as.factor(data$x5)
data$x6<-as.factor(data$x6)
data$x7<-as.factor(data$x7)
#indicator for outcome I(Y>0)
data$I_Y<-ifelse(data$Y==0,0,1)
data$I_Y<-ifelse(is.na(data$Y),NA,data$I_Y)
#annual to weekly 
data$Y<-data$Y/52.1429

#the percentages of subjects having the outcome Y observed among subjects with missing mediator values
#treatment
t_obsy<-data %>%
  filter(R_m==0,Z==1) %>%
  group_by(R_y) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(t_obsy,R_y==1)
#control
c_obsy<-data %>%
  filter(R_m==0,Z==0) %>%
  group_by(R_y) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(c_obsy,R_y==1)

#small portions of the missingness exist in covariates X
#missing educ: 4
educ<-data %>%
  group_by(x6) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(educ,x6==4)
#missing earnb: 5
earnb<-data %>%
  group_by(x7) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(earnb,x7==5)
#missing arrst: 2
arrst<-data %>%
  group_by(x5) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(arrst,x5==2)

#number of obs with Y=0
num_0<-data %>%
  group_by(I_Y) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(num_0,I_Y==0)

#distribution of the covariates
data_anno<-data.frame('sex'=factor(data$x1,levels=c(1,0),labels=c('Female','Male')),
                      'age'=factor(data$x2,levels=c(0,1,2),labels=c('Age: 16-17','Age: 18-19','Age: 20-24')),
                      'race'=factor(data$x3,levels=c(0,1,2,3),labels=c('Race: white','Race: black','Race: hispanic','Race: others')),
                      'children'=factor(data$x4,levels=c(0,1),labels=c('Had child: no','Had child: yes')),
                      'arrest'=factor(data$x5,levels=c(0,1,2),labels=c('Ever arrested: no','Ever arrested: yes','Ever arrested: missing')),
                      'education'=factor(data$x6,levels=c(1,2,3,4),labels=c('Education: no high school diploma / GED','Education: GED certificates','Education: high school diploma','Education: missing')),
                      'earnings'=factor(data$x7,levels=c(0,1,2,3,4,5),labels=c('Earnings (past year): 0','Earnings (past year): 0-1000','Earnings (past year): 1000-5000','Earnings (past year): 5000-10000','Earnings (past year): >=10000','Earnings (past year): missing')))
#ensure that numbers are displayed with two decimal points before the percentage sign (%).
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.2f %%)", FREQ, PCT))))
}
table1(~ sex+age+race+children+arrest+education+earnings, data=data_anno, render.categorical=my.render.cat)
