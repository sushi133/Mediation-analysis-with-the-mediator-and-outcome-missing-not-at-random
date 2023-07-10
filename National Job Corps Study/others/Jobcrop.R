library(readstata13)

setwd("/Users/sushi5824907/Desktop/Mediation/JASA/Data")
data1<-read.dta13("mpr_jobcorps_team5_nrw_upd_r_nositeid.dta")
data2<-read.dta13("key_vars.dta")
data <- merge(data1,data2,by="mprid")

#random treatment variable:treatmnt.x
#mediator:anygainnew
#outcome:aearny4
#covariates to adjust for:arrst,educ,earnb,age,race,female,haschld,arrst,earnb

#preparation: if a categorical one has missingness, I simply treat the missingness as another category

data$arrst<-ifelse(data$miss_arrst==1,2,data$arrst)
data$educ<-ifelse(is.na(data$EDUC_GR),4,data$EDUC_GR)

data$earnb<-rep(NA,dim(data)[1])
data$earnb<-ifelse(data$earnb0==1,0,data$earnb)
data$earnb<-ifelse(data$earnb1==1,1,data$earnb)
data$earnb<-ifelse(data$earnb2==1,2,data$earnb)
data$earnb<-ifelse(data$earnb3==1,3,data$earnb)
data$earnb<-ifelse(data$earnb4==1,4,data$earnb)
data$earnb<-ifelse(is.na(data$earnb),5,data$earnb)

data$age<-data$age1819
data$age<-ifelse(data$age2024==1,2,data$age)
data$race<-data$race_b
data$race<-ifelse(data$race_h==1,2,data$race)
data$race<-ifelse(data$race_o==1,3,data$race)

datanew<-data.frame(Z=data$treatmnt.x,M=data$anygainnew,Y=data$aearny4,
                    x1=as.factor(data$female.x),x2=as.factor(data$age),
	                  x3=as.factor(data$race),x4=as.factor(data$haschld_upd),
                    x5=as.factor(data$arrst),x6=as.factor(data$educ),x7=as.factor(data$earnb))

#missing indicator for M and Y
datanew$R_m<-ifelse(is.na(datanew$M),0,1)
datanew$R_y<-ifelse(is.na(datanew$Y),0,1)

#output data in csv format
write.csv(datanew,"/Users/sushi5824907/Desktop/Mediation/JASA/Data/Jobcorpdata.csv",row.names = FALSE)

