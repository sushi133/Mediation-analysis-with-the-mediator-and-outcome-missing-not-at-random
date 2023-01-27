library(dplyr)
library(scales)

data <- read.csv("/Users/sushi5824907/Desktop/Mediation/JASA/Data/Jobcorpdata.csv")
data$x1<-as.factor(data$x1)
data$x2<-as.factor(data$x2)
data$x3<-as.factor(data$x3)
data$x4<-as.factor(data$x4)
data$x5<-as.factor(data$x5)
data$x6<-as.factor(data$x6)
data$x7<-as.factor(data$x7)
#indicator for outcome when it is 0
data$zero<-ifelse(data$Y==0,0,1)
data$zero<-ifelse(is.na(data$Y),NA,data$zero)
#annual to weekly 
data$Y<-data$Y/52.1429

#missing patterns
data$pattern<-rep(NA,dim(data)[1])
data$pattern<-ifelse(data$R_m==0 & data$R_y==1,1,data$pattern)
data$pattern<-ifelse(data$R_m==1 & data$R_y==0,2,data$pattern)
data$pattern<-ifelse(data$R_m==0 & data$R_y==0,3,data$pattern)
data$pattern<-ifelse(data$R_m==1 & data$R_y==1,4,data$pattern)

data$pattern1<-rep(NA,dim(data)[1])
data$pattern1<-ifelse(data$R_m==0 | data$R_y==0,5,data$pattern1)
data$pattern1<-ifelse(data$R_m==1 & data$R_y==1,6,data$pattern1)

#treatment
tp1<-data %>%
  filter(Z==1) %>%
  group_by(pattern) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
tp1<-na.omit(tp1)

tp2<-data %>%
  filter(Z==1) %>%
  group_by(pattern1) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
colnames(tp2)[1] = "pattern"

tp3<-data %>%
  filter(Z==1) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
tp3$pattern<-7

tp<-rbind(tp1,tp2,tp3)

#control
cp1<-data %>%
  filter(Z==0) %>%
  group_by(pattern) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
cp1<-na.omit(cp1)

cp2<-data %>%
  filter(Z==0) %>%
  group_by(pattern1) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
colnames(cp2)[1] = "pattern"

cp3<-data %>%
  filter(Z==0) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
cp3$pattern<-7

cp<-rbind(cp1,cp2,cp3)
cp<-subset(cp,select=-c(pattern))

#table 1 (combine treatment and control)
cbind(tp,cp)

#the percentages of subjects having the outcome Y observed among subjects with missing mediator values
#treatment
t_ndo<-data %>%
  filter(R_m==0,Z==1) %>%
  group_by(R_y) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(t_ndo,R_y==1)
#control
c_ndo<-data %>%
  filter(R_m==0,Z==0) %>%
  group_by(R_y) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(c_ndo,R_y==1)

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

#check number of obs with 0
zero<-data %>%
  group_by(zero) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
subset(zero,zero==0)


