library(dplyr)
library(scales)

#table 1: missing patterns
data <- read.csv("/Users/sushi5824907/Desktop/Mediation/JASA/Data/Jobcorpdata.csv")
data$pattern<-rep(NA,dim(data)[1])
data$pattern<-ifelse(data$R_m==0 & data$R_y==1,1,data$pattern)
data$pattern<-ifelse(data$R_m==1 & data$R_y==0,2,data$pattern)
data$pattern<-ifelse(data$R_m==0 & data$R_y==0,3,data$pattern)
data$pattern<-ifelse(data$R_m==1 & data$R_y==1,4,data$pattern)
#treatment
tp1<-data %>%
  filter(Z==1) %>%
  group_by(pattern) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
tp2<-data %>%
  filter(Z==1) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
tp2$pattern<-5
tp<-rbind(tp1,tp2)
#control
cp1<-data %>%
  filter(Z==0) %>%
  group_by(pattern) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
cp2<-data %>%
  filter(Z==0) %>%
  summarise(n = n()) %>%
  mutate(freq = scales::percent(n/sum(n), accuracy = 0.01))
cp2$pattern<-5
cp<-rbind(cp1,cp2)
cp<-subset(cp,select=-c(pattern))
#combine treatment and control
cbind(tp,cp)

#table 2: model comparison
Gamma_II_D<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Gamma.xlsx', 1)
Gamma_II_B <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Gamma_B.xlsx', 1)
Gamma_II <-cbind(Gamma_II_D[107,3],Gamma_II_D[103,3],Gamma_II_B[103,c(11,12)],Gamma_II_D[104,3],Gamma_II_B[104,c(11,12)])
colnames(Gamma_II) <- c("LL","NIE","CIL","CIU","NDE","CIL","CIU")

Gamma_III_D<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/III_Job_Gamma.xlsx', 1)
Gamma_III_B<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/III_Job_Gamma_B.xlsx', 1)
Gamma_III <-cbind(Gamma_III_D[107,3],Gamma_III_D[103,3],Gamma_III_B[103,c(11,12)],Gamma_III_D[104,3],Gamma_III_B[104,c(11,12)])
colnames(Gamma_III) <- c("LL","NIE","CIL","CIU","NDE","CIL","CIU")

Gamma_IV_D<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/IV_Job_Gamma.xlsx', 1)
Gamma_IV_B<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/IV_Job_Gamma_B.xlsx', 1)
Gamma_IV <-cbind(Gamma_IV_D[107,3],Gamma_IV_D[103,3],Gamma_IV_B[103,c(11,12)],Gamma_IV_D[104,3],Gamma_IV_B[104,c(11,12)])
colnames(Gamma_IV) <- c("LL","NIE","CIL","CIU","NDE","CIL","CIU")

Lognormal_II_D<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Lnorm.xlsx', 1)
Lognormal_II_B<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Lnorm_B.xlsx', 1)
Lognormal_II <-cbind(Lognormal_II_D[107,3],Lognormal_II_D[103,3],Lognormal_II_B[103,c(11,12)],Lognormal_II_D[104,3],Lognormal_II_B[104,c(11,12)])
colnames(Lognormal_II) <- c("LL","NIE","CIL","CIU","NDE","CIL","CIU")

Lognormal_III_D<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/III_Job_Lnorm.xlsx', 1)
Lognormal_III_B<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/III_Job_Lnorm_B.xlsx', 1)
Lognormal_III <-cbind(Lognormal_III_D[107,3],Lognormal_III_D[103,3],Lognormal_III_B[103,c(11,12)],Lognormal_III_D[104,3],Lognormal_III_B[104,c(11,12)])
colnames(Lognormal_III) <- c("LL","NIE","CIL","CIU","NDE","CIL","CIU")

Lognormal_IV_D<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/IV_Job_Lnorm.xlsx', 1)
Lognormal_IV_B<-read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/IV_Job_Lnorm_B.xlsx', 1)
Lognormal_IV <-cbind(Lognormal_IV_D[107,3],Lognormal_IV_D[103,3],Lognormal_IV_B[103,c(11,12)],Lognormal_IV_D[104,3],Lognormal_IV_B[104,c(11,12)])
colnames(Lognormal_IV) <- c("LL","NIE","CIL","CIU","NDE","CIL","CIU")

T2<-as.data.frame(rbind(Gamma_II,Gamma_III,Gamma_IV,Lognormal_II,Lognormal_III,Lognormal_IV))
round(T2,2)

#table 3: data analysis 
#missing mechanism for MI is redundant and removed from table 3 in manuscript
D <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Gamma.xlsx', 1)
DB <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Gamma_B.xlsx', 1)
D<-rbind(D[64,],D[84,],D[103,],D[104,])
DBT<-rbind(DB[64,],DB[84,],DB[103,],DB[104,])
DBT1<-as.data.frame(cbind(D[,1],DBT[,c(3,4)],D[,2],DBT[,c(7,8)],D[,3],DBT[,c(11,12)]))
round(DBT1,2)

#table 4: sensitivity analysis 
S_2_2 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_-2_-2.xlsx', 1)
SB_2_2 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_-2_-2.xlsx', 1)
S0_2 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_0_-2.xlsx', 1)
SB0_2 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_0_-2.xlsx', 1)
S2_2 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_2_-2.xlsx', 1)
SB2_2 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_2_-2.xlsx', 1)

S_20 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_-2_0.xlsx', 1)
SB_20 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_-2_0.xlsx', 1)
S00 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_0_0.xlsx', 1)
SB00 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_0_0.xlsx', 1)
S20 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_2_0.xlsx', 1)
SB20 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_2_0.xlsx', 1)

S_22 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_-2_2.xlsx', 1)
SB_22 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_-2_2.xlsx', 1)
S02 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_0_2.xlsx', 1)
SB02 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_0_2.xlsx', 1)
S22 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_2_2.xlsx', 1)
SB22 <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Sensitivity_Analysis/II_Job_Gamma_S_B_2_2.xlsx', 1)

TSR1<-cbind(-2, S_2_2[103,1],SB_2_2[103,c(3,4)],S0_2[103,1],SB0_2[103,c(3,4)],S2_2[103,1],SB2_2[103,c(3,4)])
TSR2<-cbind(0, S_20[103,1],SB_20[103,c(3,4)],S00[103,1],SB00[103,c(3,4)],S20[103,1],SB20[103,c(3,4)])
TSR3<-cbind(2, S_22[103,1],SB_22[103,c(3,4)],S02[103,1],SB02[103,c(3,4)],S22[103,1],SB22[103,c(3,4)])

TSR4<-cbind(-2, S_2_2[104,1],SB_2_2[104,c(3,4)],S0_2[104,1],SB0_2[104,c(3,4)],S2_2[104,1],SB2_2[104,c(3,4)])
TSR5<-cbind(0, S_20[104,1],SB_20[104,c(3,4)],S00[104,1],SB00[104,c(3,4)],S20[104,1],SB20[104,c(3,4)])
TSR6<-cbind(2, S_22[104,1],SB_22[104,c(3,4)],S02[104,1],SB02[104,c(3,4)],S22[104,1],SB22[104,c(3,4)])

colnames(TSR1) <- c("sens","est","lower","upper","est","lower","upper","est","lower","upper")
colnames(TSR2) <- c("sens","est","lower","upper","est","lower","upper","est","lower","upper")
colnames(TSR3) <- c("sens","est","lower","upper","est","lower","upper","est","lower","upper")
colnames(TSR4) <- c("sens","est","lower","upper","est","lower","upper","est","lower","upper")
colnames(TSR5) <- c("sens","est","lower","upper","est","lower","upper","est","lower","upper")
colnames(TSR6) <- c("sens","est","lower","upper","est","lower","upper","est","lower","upper")

tab<-round(as.data.frame(rbind(TSR1,TSR2,TSR3,TSR4,TSR5,TSR6)),2)
colnames(tab) <- c("sens","est1","lower1","upper1","est2","lower2","upper2","est3","lower3","upper3")
tab$estCI1 <- paste0(tab$est1," (",tab$lower1,", ",tab$upper1,")")
tab$estCI2 <- paste0(tab$est2," (",tab$lower2,", ",tab$upper2,")")
tab$estCI3 <- paste0(tab$est3," (",tab$lower3,", ",tab$upper3,")")
subset(tab, select = c(sens,estCI1,estCI2,estCI3))

