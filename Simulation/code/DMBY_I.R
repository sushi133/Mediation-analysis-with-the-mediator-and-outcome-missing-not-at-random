library(tictoc)
library(mice)
library(dplyr)
library(parallel)
library(miscF)
library(nnet)
library("xlsx")
set.seed(12345)

#E[Y(t,M(t))]
int11<-function(data,b_0,b_m2,b_m3,b_t,b_mt2,b_mt3,b_x,a_02,a_t2,a_x2,a_03,a_t3,a_x3){
  mean((exp(b_0+b_m3+b_t*1+b_mt3*1+b_x*data$x)/(1+exp(b_0+b_m3+b_t*1+b_mt3*1+b_x*data$x)))*
       (exp(a_03+a_t3*1+a_x3*data$x)/(1+exp(a_02+a_t2*1+a_x2*data$x)+exp(a_03+a_t3*1+a_x3*data$x)))+
       (exp(b_0+b_m2+b_t*1+b_mt2*1+b_x*data$x)/(1+exp(b_0+b_m2+b_t*1+b_mt2*1+b_x*data$x)))*
       (exp(a_02+a_t2*1+a_x2*data$x)/(1+exp(a_02+a_t2*1+a_x2*data$x)+exp(a_03+a_t3*1+a_x3*data$x)))+
       (exp(b_0+b_t*1+b_x*data$x)/(1+exp(b_0+b_t*1+b_x*data$x)))*
       (1/(1+exp(a_02+a_t2*1+a_x2*data$x)+exp(a_03+a_t3*1+a_x3*data$x))))
}
int10<-function(data,b_0,b_m2,b_m3,b_t,b_mt2,b_mt3,b_x,a_02,a_t2,a_x2,a_03,a_t3,a_x3){
  mean((exp(b_0+b_m3+b_t*1+b_mt3*1+b_x*data$x)/(1+exp(b_0+b_m3+b_t*1+b_mt3*1+b_x*data$x)))*
       (exp(a_03+a_t3*0+a_x3*data$x)/(1+exp(a_02+a_t2*0+a_x2*data$x)+exp(a_03+a_t3*0+a_x3*data$x)))+
       (exp(b_0+b_m2+b_t*1+b_mt2*1+b_x*data$x)/(1+exp(b_0+b_m2+b_t*1+b_mt2*1+b_x*data$x)))*
       (exp(a_02+a_t2*0+a_x2*data$x)/(1+exp(a_02+a_t2*0+a_x2*data$x)+exp(a_03+a_t3*0+a_x3*data$x)))+
       (exp(b_0+b_t*1+b_x*data$x)/(1+exp(b_0+b_t*1+b_x*data$x)))*
       (1/(1+exp(a_02+a_t2*0+a_x2*data$x)+exp(a_03+a_t3*0+a_x3*data$x))))
}
int01<-function(data,b_0,b_m2,b_m3,b_t,b_mt2,b_mt3,b_x,a_02,a_t2,a_x2,a_03,a_t3,a_x3){
  mean((exp(b_0+b_m3+b_t*0+b_mt3*0+b_x*data$x)/(1+exp(b_0+b_m3+b_t*0+b_mt3*0+b_x*data$x)))*
       (exp(a_03+a_t3*1+a_x3*data$x)/(1+exp(a_02+a_t2*1+a_x2*data$x)+exp(a_03+a_t3*1+a_x3*data$x)))+
       (exp(b_0+b_m2+b_t*0+b_mt2*0+b_x*data$x)/(1+exp(b_0+b_m2+b_t*0+b_mt2*0+b_x*data$x)))*
       (exp(a_02+a_t2*1+a_x2*data$x)/(1+exp(a_02+a_t2*1+a_x2*data$x)+exp(a_03+a_t3*1+a_x3*data$x)))+
       (exp(b_0+b_t*0+b_x*data$x)/(1+exp(b_0+b_t*0+b_x*data$x)))*
       (1/(1+exp(a_02+a_t2*1+a_x2*data$x)+exp(a_03+a_t3*1+a_x3*data$x))))
}
int00<-function(data,b_0,b_m2,b_m3,b_t,b_mt2,b_mt3,b_x,a_02,a_t2,a_x2,a_03,a_t3,a_x3){
  mean((exp(b_0+b_m3+b_t*0+b_mt3*0+b_x*data$x)/(1+exp(b_0+b_m3+b_t*0+b_mt3*0+b_x*data$x)))*
       (exp(a_03+a_t3*0+a_x3*data$x)/(1+exp(a_02+a_t2*0+a_x2*data$x)+exp(a_03+a_t3*0+a_x3*data$x)))+
       (exp(b_0+b_m2+b_t*0+b_mt2*0+b_x*data$x)/(1+exp(b_0+b_m2+b_t*0+b_mt2*0+b_x*data$x)))*
       (exp(a_02+a_t2*0+a_x2*data$x)/(1+exp(a_02+a_t2*0+a_x2*data$x)+exp(a_03+a_t3*0+a_x3*data$x)))+
       (exp(b_0+b_t*0+b_x*data$x)/(1+exp(b_0+b_t*0+b_x*data$x)))*
       (1/(1+exp(a_02+a_t2*0+a_x2*data$x)+exp(a_03+a_t3*0+a_x3*data$x))))
}

#parameter set up
N<-1000
u_x<-0
sd_x<-1
p_t<-0.5
a_02<-0
a_t2<-1
a_x2<-1
a_03<-0
a_t3<-1
a_x3<-1
b_0<-0
b_m2<-2
b_m3<-1
b_t<-1
b_x<-1
b_mt2<-1
b_mt3<-0
c_0<-0
c_m2<-2
c_m3<-2
c_t<-0.2
c_x<-0.2

#complete data
x<-rnorm(N,u_x,sd_x)
t<-rbinom(N,1,p_t)
tx<-as.matrix(cbind(1,t,x))
a2<-as.matrix(rbind(a_02,a_t2,a_x2))
a3<-as.matrix(rbind(a_03,a_t3,a_x3))
p1<-1/(1+exp(tx%*%a2)+exp(tx%*%a3))
p2<-exp(tx%*%a2)/(1+exp(tx%*%a2)+exp(tx%*%a3))
p3<-exp(tx%*%a3)/(1+exp(tx%*%a2)+exp(tx%*%a3))
m<-as.factor(rMultinom(p=cbind(p1, p2, p3)))
mtmtx<-as.matrix(cbind(1,1,t,t,x))
b<-as.matrix(rbind(b_0,b_m2*I(m==2)+b_m3*I(m==3),b_t,b_mt2*I(m==2)+b_mt3*I(m==3),b_x))
mtmtx_b<-NULL
for (j in 1:N) {mtmtx_b[j]<-mtmtx[j,]%*%b[,j]}
y<-rbinom(N,1,exp(mtmtx_b)/(1+exp(mtmtx_b)))
  
#generate data with MNAR
mtx<-as.matrix(cbind(1,1,t,x))
c<-as.matrix(rbind(c_0,c_m2*I(m==2)+c_m3*I(m==3),c_t,c_x))
mtx_c<-NULL
for (j in 1:N) {mtx_c[j]<-mtx[j,]%*%c[,j]}
R_m<-rbinom(N,1,exp(mtx_c)/(1+exp(mtx_c)))
t_m<-m
m[R_m==0] <- NA
data<-as.data.frame(cbind(t,m,x,y,R_m,t_m))
data$m<-as.factor(data$m)
data$t_m<-as.factor(data$t_m)
miss_m<-dplyr::count(data[which(data$R_m==0),])/N
  
#oracle
m1<-glm(y~t_m+t+t_m*t+x,family=binomial(link='logit'),data=data)
m2<-multinom(t_m~t+x,data=data,trace=F)
m3<-glm(R_m~t_m+t+x,family=binomial(link='logit'),data=data)
tb_0<-m1$coef[1]
tb_m2<-m1$coef[2]
tb_m3<-m1$coef[3]
tb_t<-m1$coef[4]
tb_x<-m1$coef[5]
tb_mt2<-m1$coef[6]
tb_mt3<-m1$coef[7]
ta_02<-summary(m2)$coefficients[1,1]
ta_t2<-summary(m2)$coefficients[1,2]
ta_x2<-summary(m2)$coefficients[1,3]
ta_03<-summary(m2)$coefficients[2,1]
ta_t3<-summary(m2)$coefficients[2,2]
ta_x3<-summary(m2)$coefficients[2,3]
tc_0<-m3$coef[1]
tc_m2<-m3$coef[2]
tc_m3<-m3$coef[3]
tc_t<-m3$coef[4]
tc_x<-m3$coef[5]
  
#complete case analysis
ccm1<-glm(y~m+t+m*t+x,family=binomial(link='logit'),data=data,subset=(R_m==1))
ccm2<-multinom(m~t+x,data=data,subset=(R_m==1),trace=F)
ccb_0<-ccm1$coef[1]
ccb_m2<-ccm1$coef[2]
ccb_m3<-ccm1$coef[3]
ccb_t<-ccm1$coef[4]
ccb_x<-ccm1$coef[5]
ccb_mt2<-ccm1$coef[6]
ccb_mt3<-ccm1$coef[7]
cca_02<-summary(ccm2)$coefficients[1,1]
cca_t2<-summary(ccm2)$coefficients[1,2]
cca_x2<-summary(ccm2)$coefficients[1,3]
cca_03<-summary(ccm2)$coefficients[2,1]
cca_t3<-summary(ccm2)$coefficients[2,2]
cca_x3<-summary(ccm2)$coefficients[2,3]
ccc_0<-NA
ccc_m2<-NA
ccc_m3<-NA
ccc_t<-NA
ccc_x<-NA
  
#generate initial parameters for EM algorithm
dat_mi<-subset(data, select = c(y,m,t,x,R_m))
dat_mi$m<-as.factor(dat_mi$m)
imp <- mice(dat_mi, print = F)
#define predictor
pred <- imp$predictorMatrix
pred[c('m'), c('y','R_m')] <- 0
imp <- mice(dat_mi, m = 5, pred = pred, print=F, seed=123)
mim1<-summary(pool(with(imp, glm(y~m+t+m*t+x,family=binomial(link='logit')))))
mim2<-summary(pool(with(imp, multinom(m~t+x,trace=F))))
mim3<-summary(pool(with(imp, glm(R_m~m+t+x,family=binomial(link='logit')))))
mib_0<-mim1$estimate[1]
mib_m2<-mim1$estimate[2]
mib_m3<-mim1$estimate[3]
mib_t<-mim1$estimate[4]
mib_x<-mim1$estimate[5]
mib_mt2<-mim1$estimate[6]
mib_mt3<-mim1$estimate[7]
mia_02<-mim2$estimate[1]
mia_t2<-mim2$estimate[2]
mia_x2<-mim2$estimate[3]  
mia_03<-mim2$estimate[4]
mia_t3<-mim2$estimate[5]
mia_x3<-mim2$estimate[6] 
mic_0<-mim3$estimate[1]
mic_m2<-mim3$estimate[2]
mic_m3<-mim3$estimate[3]
mic_t<-mim3$estimate[4]
mic_x<-mim3$estimate[5] 
  
#EM
#possible value for discrete var
dat0<-subset(data,R_m==1)
dat1<-subset(data,R_m==0)
#missing m
dat11<-dat1
dat12<-dat1
dat13<-dat1
dat11$m<-1
dat12$m<-2
dat13$m<-3
  
dat<-rbind(dat0,dat11,dat12,dat13)
  
#initial parameters
emb_0<-ccb_0
emb_m2<-ccb_m2
emb_m3<-ccb_m3
emb_t<-ccb_t
emb_mt2<-ccb_mt2
emb_mt3<-ccb_mt3
emb_x<-ccb_x
ema_02<-cca_02
ema_t2<-cca_t2
ema_x2<-cca_x2
ema_03<-cca_03
ema_t3<-cca_t3
ema_x3<-cca_x3
emc_0<-mic_0
emc_m2<-mic_m2
emc_m3<-mic_m3
emc_t<-mic_t
emc_x<-mic_x
  
#weight of m when y is observed
m_weight<-function(y,m,t,x){
    
  cond_prob_3<-
    (exp(emb_0+emb_m3+emb_t*t+emb_mt3*t+emb_x*x)/(1+exp(emb_0+emb_m3+emb_t*t+emb_mt3*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_m3+emb_t*t+emb_mt3*t+emb_x*x)))^I(y==0)*
    (exp(ema_03+ema_t3*t+ema_x3*x)/(exp(ema_03+ema_t3*t+ema_x3*x)+exp(ema_02+ema_t2*t+ema_x2*x)+1))*
    (1/(1+exp(emc_0+emc_m3+emc_t*t+emc_x*x)))
    
  cond_prob_2<-
    (exp(emb_0+emb_m2+emb_t*t+emb_mt2*t+emb_x*x)/(1+exp(emb_0+emb_m2+emb_t*t+emb_mt2*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_m2+emb_t*t+emb_mt2*t+emb_x*x)))^I(y==0)*
    (exp(ema_02+ema_t2*t+ema_x2*x)/(exp(ema_03+ema_t3*t+ema_x3*x)+exp(ema_02+ema_t2*t+ema_x2*x)+1))*
    (1/(1+exp(emc_0+emc_m2+emc_t*t+emc_x*x)))
    
  cond_prob_1<-
    (exp(emb_0+emb_t*t+emb_x*x)/(1+exp(emb_0+emb_t*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_t*t+emb_x*x)))^I(y==0)*
    (1/(exp(ema_03+ema_t3*t+ema_x3*x)+exp(ema_02+ema_t2*t+ema_x2*x)+1))*
    (1/(1+exp(emc_0+emc_t*t+emc_x*x)))
    
  cond_prob<-
    (exp(emb_0+emb_m2*I(m==2)+emb_m3*I(m==3)+emb_t*t+emb_mt2*I(m==2)*t+emb_mt3*I(m==3)*t+emb_x*x)/(1+exp(emb_0+emb_m2*I(m==2)+emb_m3*I(m==3)+emb_t*t+emb_mt2*I(m==2)*t+emb_mt3*I(m==3)*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_m2*I(m==2)+emb_m3*I(m==3)+emb_t*t+emb_mt2*I(m==2)*t+emb_mt3*I(m==3)*t+emb_x*x)))^I(y==0)*
    (exp(ema_03+ema_t3*t+ema_x3*x)/(exp(ema_03+ema_t3*t+ema_x3*x)+exp(ema_02+ema_t2*t+ema_x2*x)+1))^I(m==3)*
    (exp(ema_02+ema_t2*t+ema_x2*x)/(exp(ema_03+ema_t3*t+ema_x3*x)+exp(ema_02+ema_t2*t+ema_x2*x)+1))^I(m==2)*
    (1/(exp(ema_03+ema_t3*t+ema_x3*x)+exp(ema_02+ema_t2*t+ema_x2*x)+1))^I(m==1)*
    (1/(1+exp(emc_0+emc_m2*I(m==2)+emc_m3*I(m==3)+emc_t*t+emc_x*x)))
    
  return(cond_prob/(cond_prob_3+cond_prob_2+cond_prob_1))
}
  
Q<-NULL
Q[[1]] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
Q[[2]] <- c(emb_0,emb_m2,emb_m3,emb_t,emb_mt2,emb_mt3,emb_x,ema_02,ema_t2,ema_x2,ema_03,ema_t3,ema_x3,emc_0,emc_m2,emc_m3,emc_t,emc_x)
k<-2
while (sum(abs(Q[[k]]-Q[[k-1]]))/sum(Q[[k-1]])>=1e-5) {
    
  dat0<-subset(dat, R_m==1)
  dat0$wt<-1
    
  #missing m
  dat1<-subset(dat, R_m==0)
  dat1$wt<-m_weight(dat1$y,dat1$m,dat1$t,dat1$x)
    
  dat<-rbind(dat0,dat1)
    
  #update parameters
  emm1<-glm(y~m+t+m*t+x,family = binomial(link='logit'),weights=wt,data=dat)
  emm2<-multinom(m~t+x,weights=wt,data=dat,trace=F)
  emm3<-glm(R_m~m+t+x,family = binomial(link='logit'),weights=wt,data=dat)
  emb_0<-emm1$coef[1]
  emb_m2<-emm1$coef[2]
  emb_m3<-emm1$coef[3]
  emb_t<-emm1$coef[4]
  emb_x<-emm1$coef[5]
  emb_mt2<-emm1$coef[6]
  emb_mt3<-emm1$coef[7]
  ema_02<-summary(emm2)$coefficients[1,1]
  ema_t2<-summary(emm2)$coefficients[1,2]
  ema_x2<-summary(emm2)$coefficients[1,3]
  ema_03<-summary(emm2)$coefficients[2,1]
  ema_t3<-summary(emm2)$coefficients[2,2]
  ema_x3<-summary(emm2)$coefficients[2,3]
  emc_0<-emm3$coef[1]
  emc_m2<-emm3$coef[2]
  emc_m3<-emm3$coef[3]
  emc_t<-emm3$coef[4]
  emc_x<-emm3$coef[5]
    
  k <- k + 1
    
  Q[[k]]<-c(emb_0,emb_m2,emb_m3,emb_t,emb_mt2,emb_mt3,emb_x,ema_02,ema_t2,ema_x2,ema_03,ema_t3,ema_x3,emc_0,emc_m2,emc_m3,emc_t,emc_x)
}
  
#EM
write.xlsx(Q,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/DMBY_I_EM.xlsx',row.names = FALSE)

#truth/complete cases/oracle
save<-rbind(c(b_0,b_m2,b_m3,b_t,b_mt2,b_mt3,b_x,a_02,a_t2,a_x2,a_03,a_t3,a_x3,c_0,c_m2,c_m3,c_t,c_x),
            c(ccb_0,ccb_m2,ccb_m3,ccb_t,ccb_mt2,ccb_mt3,ccb_x,cca_02,cca_t2,cca_x2,cca_03,cca_t3,cca_x3,ccc_0,ccc_m2,ccc_m3,ccc_t,ccc_x),
            c(tb_0,tb_m2,tb_m3,tb_t,tb_mt2,tb_mt3,tb_x,ta_02,ta_t2,ta_x2,ta_03,ta_t3,ta_x3,tc_0,tc_m2,tc_m3,tc_t,tc_x))
write.xlsx(save,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/DMBY_I.xlsx',row.names = FALSE)




