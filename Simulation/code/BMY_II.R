library("xlsx")
library(mice)
library(dplyr)
library(parallel)

#purpose: calculate E[Y(1,M(1))] 
#arguments: x: covariate, b_0-a_x: parameters in M and Y models
#output: E[Y(1,M(1))]
int11<-function(x,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x){
  mean((exp(b_0+b_m*1+b_t*1+b_mt*1*1+b_x*x)/(1+exp(b_0+b_m*1+b_t*1+b_mt*1*1+b_x*x)))*
       (exp(a_0+a_t*1+a_x*x)/(1+exp(a_0+a_t*1+a_x*x)))+
       (exp(b_0+b_m*0+b_t*1+b_mt*0*1+b_x*x)/(1+exp(b_0+b_m*0+b_t*1+b_mt*0*1+b_x*x)))*
       (1/(1+exp(a_0+a_t*1+a_x*x))))
}
#purpose: calculate E[Y(1,M(0))] 
#arguments: x: covariate, b_0-a_x: parameters in M and Y models
#output: E[Y(1,M(0))]
int10<-function(x,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x){
  mean((exp(b_0+b_m*1+b_t*1+b_mt*1*1+b_x*x)/(1+exp(b_0+b_m*1+b_t*1+b_mt*1*1+b_x*x)))*
       (exp(a_0+a_t*0+a_x*x)/(1+exp(a_0+a_t*0+a_x*x)))+
       (exp(b_0+b_m*0+b_t*1+b_mt*0*1+b_x*x)/(1+exp(b_0+b_m*0+b_t*1+b_mt*0*1+b_x*x)))*
       (1/(1+exp(a_0+a_t*0+a_x*x))))
}
#purpose: calculate E[Y(0,M(1))] 
#arguments: x: covariate, b_0-a_x: parameters in M and Y models
#output: E[Y(0,M(1))]
int01<-function(x,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x){
  mean((exp(b_0+b_m*1+b_t*0+b_mt*1*0+b_x*x)/(1+exp(b_0+b_m*1+b_t*0+b_mt*1*0+b_x*x)))*
       (exp(a_0+a_t*1+a_x*x)/(1+exp(a_0+a_t*1+a_x*x)))+
       (exp(b_0+b_m*0+b_t*0+b_mt*0*0+b_x*x)/(1+exp(b_0+b_m*0+b_t*0+b_mt*0*0+b_x*x)))*
       (1/(1+exp(a_0+a_t*1+a_x*x))))
}
#purpose: calculate E[Y(0,M(0))] 
#arguments: x: covariate, b_0-a_x: parameters in M and Y models
#output: E[Y(0,M(0))]
int00<-function(x,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x){
  mean((exp(b_0+b_m*1+b_t*0+b_mt*1*0+b_x*x)/(1+exp(b_0+b_m*1+b_t*0+b_mt*1*0+b_x*x)))*
       (exp(a_0+a_t*0+a_x*x)/(1+exp(a_0+a_t*0+a_x*x)))+
       (exp(b_0+b_m*0+b_t*0+b_mt*0*0+b_x*x)/(1+exp(b_0+b_m*0+b_t*0+b_mt*0*0+b_x*x)))*
       (1/(1+exp(a_0+a_t*0+a_x*x))))
}

#purpose: estimate parameters from simulated 500 datasets
#argument: simulated dataset
#output: estimated parameters from simulated 500 datasets
func<-function(i){
  
#complete data
x<-rnorm(N,u_x,sd_x)
t<-rbinom(N,1,p_t)
tx<-as.matrix(cbind(1,t,x))
a<-as.matrix(rbind(a_0,a_t,a_x))
m<-rbinom(N,1,exp(tx%*%a)/(1+exp(tx%*%a)))
mtmtx<-as.matrix(cbind(1,m,t,m*t,x))
b<-as.matrix(rbind(b_0,b_m,b_t,b_mt,b_x))
y<-rbinom(N,1,exp(mtmtx%*%b)/(1+exp(mtmtx%*%b)))

#generate data with MNAR
mtx<-as.matrix(cbind(1,m,t,x))
c<-as.matrix(rbind(c_0,c_m,c_t,c_x))
R_m<-rbinom(N,1,exp(mtx%*%c)/(1+exp(mtx%*%c)))
rmtx<-as.matrix(cbind(1,R_m,t,x))
d<-as.matrix(rbind(d_0,d_rm,d_t,d_x))
R_y<-rbinom(N,1,exp(rmtx%*%d)/(1+exp(rmtx%*%d)))
t_m<-m
t_y<-y
m[R_m==0] <- NA
y[R_y==0] <- NA
data<-as.data.frame(cbind(t,m,x,y,R_m,t_m,R_y,t_y))
miss_m<-dplyr::count(data[which(data$R_m==0),])/N
miss_y<-dplyr::count(data[which(data$R_y==0),])/N
miss_my<-dplyr::count(data[which(data$R_m==0 & data$R_y==0),])/N

#oracle
m1<-glm(t_y~t_m+t+t_m*t+x,family=binomial(link='logit'),data=data)
m2<-glm(t_m~t+x,family=binomial(link='logit'),data=data)
m3<-glm(R_m~t_m+t+x,family=binomial(link='logit'),data=data)
m4<-glm(R_y~R_m+t+x,family=binomial(link='logit'),data=data)
tb_0<-m1$coef[1]
tb_m<-m1$coef[2]
tb_t<-m1$coef[3]
tb_x<-m1$coef[4]
tb_mt<-m1$coef[5]
ta_0<-m2$coef[1]
ta_t<-m2$coef[2]
ta_x<-m2$coef[3]
tc_0<-m3$coef[1]
tc_m<-m3$coef[2]
tc_t<-m3$coef[3]
tc_x<-m3$coef[4]
td_0<-m4$coef[1]
td_rm<-m4$coef[2]
td_t<-m4$coef[3]
td_x<-m4$coef[4]

#complete case analysis
ccm1<-glm(y~m+t+m*t+x,family=binomial(link='logit'),data=data,subset=(R_m==1 & R_y==1))
ccm2<-glm(m~t+x,family=binomial(link='logit'),data=data,subset=(R_m==1 & R_y==1))
ccb_0<-ccm1$coef[1]
ccb_m<-ccm1$coef[2]
ccb_t<-ccm1$coef[3]
ccb_x<-ccm1$coef[4]
ccb_mt<-ccm1$coef[5]
cca_0<-ccm2$coef[1]
cca_t<-ccm2$coef[2]
cca_x<-ccm2$coef[3]
ccc_0<-NA
ccc_m<-NA
ccc_t<-NA
ccc_x<-NA
ccd_0<-NA
ccd_rm<-NA
ccd_t<-NA
ccd_x<-NA

#mice for MAR
dat_mi<-subset(data, select = c(y,m,t,x,R_y,R_m))
dat_mi$y<-as.factor(dat_mi$y)
dat_mi$m<-as.factor(dat_mi$m)
#define predictor
pred <- mice(dat_mi, print = F)$predictorMatrix
pred[c('m'), c('R_y','R_m')] <- 0
pred[c('y'), c('R_y','R_m')] <- 0
imp <- mice(dat_mi, m = 5, pred = pred, print=F, seed=123)
mim1<-summary(pool(with(imp, glm(y~m+t+m*t+x,family=binomial(link='logit')))))
mim2<-summary(pool(with(imp, glm(m~t+x,family=binomial(link='logit')))))
mim3<-summary(pool(with(imp, glm(R_m~m+t+x,family=binomial(link='logit')))))
mim4<-summary(pool(with(imp, glm(R_y~R_m+t+x,family=binomial(link='logit')))))
mib_0<-mim1$estimate[1]
mib_m<-mim1$estimate[2]
mib_t<-mim1$estimate[3]
mib_x<-mim1$estimate[4]
mib_mt<-mim1$estimate[5]
mia_0<-mim2$estimate[1]
mia_t<-mim2$estimate[2]
mia_x<-mim2$estimate[3]  
mic_0<-mim3$estimate[1]
mic_m<-mim3$estimate[2]
mic_t<-mim3$estimate[3]
mic_x<-mim3$estimate[4]
mid_0<-mim4$estimate[1]
mid_rm<-mim4$estimate[2]
mid_t<-mim4$estimate[3]
mid_x<-mim4$estimate[4]

#EM
#possible value for discrete var
dat0<-subset(data,R_m==1 & R_y==1)
dat1<-subset(data,R_m==0 & R_y==1)
dat2<-subset(data,R_m==1 & R_y==0)
dat3<-subset(data,R_m==0 & R_y==0)
#missing m
dat10<-dat1
dat11<-dat1
dat10$m<-0
dat11$m<-1
#missing m and y
dat30<-dat3
dat31<-dat3
dat30$m<-0
dat31$m<-1

dat<-rbind(dat0,dat10,dat11,dat2,dat30,dat31)

#initial parameters
emb_0<-ccb_0
emb_m<-ccb_m
emb_t<-ccb_t
emb_mt<-ccb_mt
emb_x<-ccb_x
ema_0<-cca_0
ema_t<-cca_t
ema_x<-cca_x
emc_0<-mic_0
emc_m<-mic_m
emc_t<-mic_t
emc_x<-mic_x
emd_0<-mid_0
emd_rm<-mid_rm
emd_t<-mid_t
emd_x<-mid_x

#purpose: calculate weight of m using estimated parameters and observed data
#arguments: y,t,x: observed data, m: possible value for m
#output: weight of m 
m_weight<-function(y,m,t,x){

cond_prob_1<-
    (exp(emb_0+emb_m*1+emb_t*t+emb_mt*1*t+emb_x*x)/(1+exp(emb_0+emb_m*1+emb_t*t+emb_mt*1*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_m*1+emb_t*t+emb_mt*1*t+emb_x*x)))^I(y==0)*
    (exp(ema_0+ema_t*t+ema_x*x)/(1+exp(ema_0+ema_t*t+ema_x*x)))*
    (1/(1+exp(emc_0+emc_m*1+emc_t*t+emc_x*x)))
  
cond_prob_0<-
    (exp(emb_0+emb_m*0+emb_t*t+emb_mt*0*t+emb_x*x)/(1+exp(emb_0+emb_m*0+emb_t*t+emb_mt*0*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_m*0+emb_t*t+emb_mt*0*t+emb_x*x)))^I(y==0)*
    (1/(1+exp(ema_0+ema_t*t+ema_x*x)))*
    (1/(1+exp(emc_0+emc_m*0+emc_t*t+emc_x*x)))
  
cond_prob<-
    (exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)/(1+exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)))^I(y==1)*
    (1/(1+exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)))^I(y==0)*
    (exp(ema_0+ema_t*t+ema_x*x)/(1+exp(ema_0+ema_t*t+ema_x*x)))^I(m==1)*(1/(1+exp(ema_0+ema_t*t+ema_x*x)))^I(m==0)*
    (1/(1+exp(emc_0+emc_m*m+emc_t*t+emc_x*x)))
  
  return(cond_prob/(cond_prob_1+cond_prob_0))
}

#purpose: calculate weight of m using estimated parameters and observed data
#arguments: t,x: observed data, m: possible value for m
#output: weight of m 
my_weight<-function(m,t,x){

cond_prob_1<-
    (exp(ema_0+ema_t*t+ema_x*x)/(1+exp(ema_0+ema_t*t+ema_x*x)))*
    (1/(1+exp(emc_0+emc_m*1+emc_t*t+emc_x*x)))
  
cond_prob_0<-
    (1/(1+exp(ema_0+ema_t*t+ema_x*x)))*
    (1/(1+exp(emc_0+emc_m*0+emc_t*t+emc_x*x)))
  
cond_prob<-
    (exp(ema_0+ema_t*t+ema_x*x)/(1+exp(ema_0+ema_t*t+ema_x*x)))^I(m==1)*(1/(1+exp(ema_0+ema_t*t+ema_x*x)))^I(m==0)*
    (1/(1+exp(emc_0+emc_m*m+emc_t*t+emc_x*x)))
  
  return(cond_prob/(cond_prob_1+cond_prob_0))
}

Q<-NULL
Q[[1]] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
Q[[2]] <- c(emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x,emc_0,emc_m,emc_t,emc_x,emd_0,emd_rm,emd_t,emd_x)
k<-2
#the EM algorithm updates the parameters iteratively until it converges
while (sum(abs(Q[[k]]-Q[[k-1]]))/sum(Q[[k-1]])>=1e-5) {
  
  dat0<-subset(dat, R_m==1 & R_y==1)
  dat0$wt<-1
  
  #missing m
  dat1<-subset(dat, R_m==0 & R_y==1)
  dat1$wt<-m_weight(dat1$y,dat1$m,dat1$t,dat1$x)
  
  #missing y
  dat2<-subset(dat, R_m==1 & R_y==0)
  dat2$wt<-1
  
  #missing m and y
  dat3<-subset(dat, R_m==0 & R_y==0)
  dat3$wt<-my_weight(dat3$m,dat3$t,dat3$x)
  
  dat<-rbind(dat0,dat1,dat2,dat3)
  
  #update parameters
  emm1<-glm(y~m+t+m*t+x,family = binomial(link='logit'),weights=wt,data=dat,subset=(R_y==1))
  emm2<-glm(m~t+x,family = binomial(link='logit'),weights=wt,data=dat)
  emm3<-glm(R_m~m+t+x,family = binomial(link='logit'),weights=wt,data=dat)
  emm4<-glm(R_y~R_m+t+x,family = binomial(link='logit'),weights=wt,data=dat)
  emb_0<-emm1$coef[1]
  emb_m<-emm1$coef[2]
  emb_t<-emm1$coef[3]
  emb_x<-emm1$coef[4]
  emb_mt<-emm1$coef[5]
  ema_0<-emm2$coef[1]
  ema_t<-emm2$coef[2]
  ema_x<-emm2$coef[3]
  emc_0<-emm3$coef[1]
  emc_m<-emm3$coef[2]
  emc_t<-emm3$coef[3]
  emc_x<-emm3$coef[4]
  emd_0<-emm4$coef[1]
  emd_rm<-emm4$coef[2]
  emd_t<-emm4$coef[3]
  emd_x<-emm4$coef[4]
  
  k <- k + 1
  
  Q[[k]]<-c(emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x,emc_0,emc_m,emc_t,emc_x,emd_0,emd_rm,emd_t,emd_x)
  
}
#calculate direct and indirect effects using the parameters estimated through the true values of the missing data
tTIE<-int11(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)-int10(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)
tPDE<-int10(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)-int00(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)
tPIE<-int01(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)-int00(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)
tTDE<-int11(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)-int01(x,tb_0,tb_m,tb_t,tb_mt,tb_x,ta_0,ta_t,ta_x)
#calculate direct and indirect effects using the parameters estimated through complete case analysis
ccTIE<-int11(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)-int10(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)
ccPDE<-int10(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)-int00(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)
ccPIE<-int01(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)-int00(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)
ccTDE<-int11(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)-int01(x,ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x,cca_0,cca_t,cca_x)
#calculate direct and indirect effects using the parameters estimated through multiple imputation
miTIE<-int11(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)-int10(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)
miPDE<-int10(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)-int00(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)
miPIE<-int01(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)-int00(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)
miTDE<-int11(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)-int01(x,mib_0,mib_m,mib_t,mib_mt,mib_x,mia_0,mia_t,mia_x)
#calculate direct and indirect effects using the parameters estimated through the EM algorithm
emTIE<-int11(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)-int10(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)
emPDE<-int10(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)-int00(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)
emPIE<-int01(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)-int00(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)
emTDE<-int11(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)-int01(x,emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x)

matrix(c(ta_0,cca_0,mia_0,ema_0,
         ta_t,cca_t,mia_t,ema_t,
         ta_x,cca_x,mia_x,ema_x,
         tb_0,ccb_0,mib_0,emb_0,
         tb_m,ccb_m,mib_m,emb_m,
         tb_t,ccb_t,mib_t,emb_t,
         tb_x,ccb_x,mib_x,emb_x,
         tb_mt,ccb_mt,mib_mt,emb_mt,
         tc_0,ccc_0,mic_0,emc_0,
         tc_m,ccc_m,mic_m,emc_m,
         tc_t,ccc_t,mic_t,emc_t,
         tc_x,ccc_x,mic_x,emc_x,
         td_0,ccd_0,mid_0,emd_0,
         td_rm,ccd_rm,mid_rm,emd_rm,
         td_t,ccd_t,mid_t,emd_t,
         td_x,ccd_x,mid_x,emd_x,
         tTIE,ccTIE,miTIE,emTIE,
         tPDE,ccPDE,miPDE,emPDE,
         tPIE,ccPIE,miPIE,emPIE,
         tTDE,ccTDE,miTDE,emTDE,
         (tTIE-TIE)/TIE,(ccTIE-TIE)/TIE,(miTIE-TIE)/TIE,(emTIE-TIE)/TIE,
         (tPDE-PDE)/PDE,(ccPDE-PDE)/PDE,(miPDE-PDE)/PDE,(emPDE-PDE)/PDE,
         (tPIE-PIE)/PIE,(ccPIE-PIE)/PIE,(miPIE-PIE)/PIE,(emPIE-PIE)/PIE,
         (tTDE-TDE)/TDE,(ccTDE-TDE)/TDE,(miTDE-TDE)/TDE,(emTDE-TDE)/TDE,
         miss_m,miss_m,miss_m,miss_m,
         miss_y,miss_y,miss_y,miss_y,
         miss_my,miss_my,miss_my,miss_my,
         k,k,k,k),byrow=T,28,4)
}

#parameter set up
N<-1000
u_x<-0
sd_x<-1
p_t<-0.5
a_0<-0
a_t<-1
a_x<-1
b_0<-0
b_m<--1
b_t<-1
b_x<-1
b_mt<--1
c_0<-0.3
c_m<-2
c_t<-1
c_x<-1
d_0<-0.4
d_rm<-1
d_t<-1
d_x<-1

#Monte Carlo approximation of the true effect
set.seed(123)
n_mc<-10000
x_s<-rnorm(n_mc,u_x,sd_x)
TIE<-int11(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x)-int10(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x) #NIE
PDE<-int10(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x)-int00(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x) #NDE
PIE<-int01(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x)-int00(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x)
TDE<-int11(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x)-int01(x_s,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x)

#time start
start <- Sys.time()
runs<-500
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
save <- mclapply(1:runs, func, mc.cores = 8, mc.set.seed = TRUE)
end <- Sys.time()
#time end
timediff <- as.numeric(difftime(end, start, units="hours"))
write.xlsx(timediff,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/Compute_time(hours).xlsx',
           row.names = FALSE,sheetName="BMY_II",append=TRUE)

write.xlsx(save,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_II.xlsx',row.names = FALSE)


