library(tictoc)
library(mice)
library(dplyr)
library(parallel)

#E[Y(t,M(t))|X]
int11 <- function(j,data,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x,sd_m){
  m <- rnorm(N_int,a_0+a_t*1+a_x*data[j,]$x,sd_m)
  int <- mean(exp(b_0+b_m*m+b_t*1+b_mt*m*1+b_x*data[j,]$x)/(exp(b_0+b_m*m+b_t*1+b_mt*m*1+b_x*data[j,]$x)+1))
  return(int)
}
int10 <- function(j,data,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x,sd_m){
  m <- rnorm(N_int,a_0+a_t*0+a_x*data[j,]$x,sd_m)
  int <- mean(exp(b_0+b_m*m+b_t*1+b_mt*m*1+b_x*data[j,]$x)/(exp(b_0+b_m*m+b_t*1+b_mt*m*1+b_x*data[j,]$x)+1))
  return(int)
}
int01 <- function(j,data,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x,sd_m){
  m <- rnorm(N_int,a_0+a_t*1+a_x*data[j,]$x,sd_m)
  int <- mean(exp(b_0+b_m*m+b_t*0+b_mt*m*0+b_x*data[j,]$x)/(exp(b_0+b_m*m+b_t*0+b_mt*m*0+b_x*data[j,]$x)+1))
  return(int)
}
int00 <- function(j,data,b_0,b_m,b_t,b_mt,b_x,a_0,a_t,a_x,sd_m){
  m <- rnorm(N_int,a_0+a_t*0+a_x*data[j,]$x,sd_m)
  int <- mean(exp(b_0+b_m*m+b_t*0+b_mt*m*0+b_x*data[j,]$x)/(exp(b_0+b_m*m+b_t*0+b_mt*m*0+b_x*data[j,]$x)+1))
  return(int)
}

func<-function(i){
  
#complete data
x<-rnorm(N,u_x,sd_x)
t<-rbinom(N,1,p_t)
m<-rnorm(N,a_0+a_t*t+a_x*x,sd_m)
mtmtx<-as.matrix(cbind(1,m,t,m*t,x))
b<-as.matrix(rbind(b_0,b_m,b_t,b_mt,b_x))
y<-rbinom(N,1,exp(mtmtx%*%b)/(1+exp(mtmtx%*%b)))
  
#generate data with MNAR
mtx<-as.matrix(cbind(1,m,t,x))
c<-as.matrix(rbind(c_0,c_m,c_t,c_x))
R_m<-rbinom(N,1,exp(mtx%*%c)/(1+exp(mtx%*%c)))
ytx<-as.matrix(cbind(1,y,t,x))
d<-as.matrix(rbind(d_0,d_y,d_t,d_x))
R_y<-rbinom(N,1,exp(ytx%*%d)/(1+exp(ytx%*%d)))
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
m2<-lm(t_m~t+x,data=data)
m3<-glm(R_m~t_m+t+x,family=binomial(link='logit'),data=data)
m4<-glm(R_y~t_y+t+x,family=binomial(link='logit'),data=data)
tb_0<-m1$coef[1]
tb_m<-m1$coef[2]
tb_t<-m1$coef[3]
tb_x<-m1$coef[4]
tb_mt<-m1$coef[5]
ta_0<-m2$coef[1]
ta_t<-m2$coef[2]
ta_x<-m2$coef[3]
tsd_m<-summary(m2)$sigma
tc_0<-m3$coef[1]
tc_m<-m3$coef[2]
tc_t<-m3$coef[3]
tc_x<-m3$coef[4]
td_0<-m4$coef[1]
td_y<-m4$coef[2]
td_t<-m4$coef[3]
td_x<-m4$coef[4]
  
#complete case analysis
ccm1<-glm(y~m+t+m*t+x,family=binomial(link='logit'),data=data,subset=(R_m==1 & R_y==1))
ccm2<-lm(m~t+x,data=data,subset=(R_m==1 & R_y==1))
ccb_0<-ccm1$coef[1]
ccb_m<-ccm1$coef[2]
ccb_t<-ccm1$coef[3]
ccb_x<-ccm1$coef[4]
ccb_mt<-ccm1$coef[5]
cca_0<-ccm2$coef[1]
cca_t<-ccm2$coef[2]
cca_x<-ccm2$coef[3]
ccsd_m<-summary(ccm2)$sigma
ccc_0<-NA
ccc_m<-NA
ccc_t<-NA
ccc_x<-NA
ccd_0<-NA
ccd_y<-NA
ccd_t<-NA
ccd_x<-NA
  
#generate initial parameters for EM algorithm
dat_mi<-subset(data, select = c(y,m,t,x,R_y,R_m))
dat_mi$y<-as.factor(dat_mi$y)
imp <- mice(dat_mi, print = F)
#define predictor
pred <- imp$predictorMatrix
pred[c('m'), c('y','R_y','R_m')] <- 0
pred[c('y'), c('R_y','R_m')] <- 0
imp <- mice(dat_mi, m = 5, pred = pred, print=F, seed=123)
mim1<-summary(pool(with(imp, glm(y~m+t+m*t+x,family=binomial(link='logit')))))
mim2<-summary(pool(with(imp, lm(m~t+x))))
mim3<-summary(pool(with(imp, glm(R_m~m+t+x,family=binomial(link='logit')))))
mim4<-summary(pool(with(imp, glm(R_y~y+t+x,family=binomial(link='logit')))))
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
mid_y<-mim4$estimate[2]
mid_t<-mim4$estimate[3]
mid_x<-mim4$estimate[4]
  
#EM
dat0<-subset(data,R_m==1 & R_y==1)
#missing m
dat1<-subset(data,R_m==0 & R_y==1)
dat1<-data.frame(lapply(dat1, rep, sample))
dat1$m<-rnorm(as.numeric(count(dat1)),cca_0+cca_t*dat1$t+cca_x*dat1$x,psd_m)
#missing y
dat2<-subset(data,R_m==1 & R_y==0)
dat20<-dat2
dat21<-dat2
dat20$y<-0
dat21$y<-1
#missing m and y
dat3<-subset(data,R_m==0 & R_y==0)
dat3<-data.frame(lapply(dat3, rep, sample))
dat3$m<-rnorm(as.numeric(count(dat3)),cca_0+cca_t*dat3$t+cca_x*dat3$x,psd_m)
dat30<-dat3
dat31<-dat3
dat30$y<-0
dat31$y<-1
  
dat<-rbind(dat0,dat1,dat20,dat21,dat30,dat31)
  
#initial parameters
emb_0<-ccb_0
emb_m<-ccb_m
emb_t<-ccb_t
emb_mt<-ccb_mt
emb_x<-ccb_x
ema_0<-cca_0
ema_t<-cca_t
ema_x<-cca_x
emsd_m<-ccsd_m
emc_0<-mic_0
emc_m<-mic_m
emc_t<-mic_t
emc_x<-mic_x
emd_0<-mid_0
emd_y<-mid_y
emd_t<-mid_t
emd_x<-mid_x
  
y_weight<-function(y,m,t,x){
    
    cond_prob_1<-
      (exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)/(1+exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)))*
      (1/(1+exp(emd_0+emd_y*1+emd_t*t+emd_x*x)))
    
    cond_prob_0<-
      (1/(1+exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)))*
      (1/(1+exp(emd_0+emd_y*0+emd_t*t+emd_x*x)))
    
    cond_prob<-
      (exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)/(1+exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)))^I(y==1)*
      (1/(1+exp(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x)))^I(y==0)*
      (1/(1+exp(emd_0+emd_y*y+emd_t*t+emd_x*x)))
    
    return(cond_prob/(cond_prob_1+cond_prob_0))
}
  
Q<-NULL
Q[[1]] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
Q[[2]] <-c(emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x,emsd_m,emc_0,emc_m,emc_t,emc_x,emd_0,emd_y,emd_t,emd_x)
k<-2
while (sum(abs(Q[[k]]-Q[[k-1]]))/sum(Q[[k-1]])>=0.01 & k < 100) {
    
    dat0<-subset(dat, R_m==1 & R_y==1)
    dat0$wt<-1
    
    #missing m
    dat1<-subset(dat, R_m==0 & R_y==1)
    
    cm_weight <- function(k1){
      cp <- function(m){
        cp <- 
          (exp(emb_0+emb_m*m+emb_t*dat1[k1,]$t+emb_mt*m*dat1[k1,]$t+emb_x*dat1[k1,]$x)/(1+exp(emb_0+emb_m*m+emb_t*dat1[k1,]$t+emb_mt*m*dat1[k1,]$t+emb_x*dat1[k1,]$x)))^I(dat1[k1,]$y==1)*
          (1/(1+exp(emb_0+emb_m*m+emb_t*dat1[k1,]$t+emb_mt*m*dat1[k1,]$t+emb_x*dat1[k1,]$x)))^I(dat1[k1,]$y==0)*
          (1/(emsd_m*sqrt(2*pi)))*exp((-0.5*((m-(ema_0+ema_t*dat1[k1,]$t+ema_x*dat1[k1,]$x))/emsd_m)^2))*
          (1/(1+exp(emc_0+emc_m*m+emc_t*dat1[k1,]$t+emc_x*dat1[k1,]$x)))
        return(cp)
      }
      pp <- function(m){
        pp <- 
          (1/(psd_m*sqrt(2*pi)))*exp((-0.5*((m-(cca_0+cca_t*dat1[k1,]$t+cca_x*dat1[k1,]$x))/psd_m)^2))
        return(pp)
      }
      #normalizing constant
      ps <- rnorm(sample_const,cca_0+cca_t*dat1[k1,]$t+cca_x*dat1[k1,]$x,psd_m)
      wt <- ((cp(dat1[k1,]$m)/pp(dat1[k1,]$m))/mean(cp(ps)/pp(ps)))/sample
    }
    dat1$wt<-as.numeric(unlist(lapply(1:as.numeric(dplyr::count(dat1)),FUN=cm_weight)))
    
    #missing y
    dat2<-subset(dat, R_m==1 & R_y==0)
    dat2$wt<-y_weight(dat2$y,dat2$m,dat2$t,dat2$x)
    
    #missing m and y
    dat3<-subset(dat, R_m==0 & R_y==0)
    
    cmy_weight <- function(k3){
      cp <- function(m){
        cp <- 
          (1/(emsd_m*sqrt(2*pi)))*exp((-0.5*((m-(ema_0+ema_t*dat3[k3,]$t+ema_x*dat3[k3,]$x))/emsd_m)^2))*
          (1/(1+exp(emc_0+emc_m*m+emc_t*dat3[k3,]$t+emc_x*dat3[k3,]$x)))
        return(cp)
      }
      pp <- function(m){
        pp <- 
          (1/(psd_m*sqrt(2*pi)))*exp((-0.5*((m-(cca_0+cca_t*dat3[k3,]$t+cca_x*dat3[k3,]$x))/psd_m)^2))
        return(pp)
      }
      #normalizing constant
      ps <- rnorm(sample_const,cca_0+cca_t*dat3[k3,]$t+cca_x*dat3[k3,]$x,psd_m)
      wt <- ((cp(dat3[k3,]$m)/pp(dat3[k3,]$m))/mean(cp(ps)/pp(ps)))/sample
    }
    dat3$wt<-as.numeric(unlist(lapply(1:as.numeric(dplyr::count(dat3)),FUN=cmy_weight)))*y_weight(dat3$y,dat3$m,dat3$t,dat3$x)
    
    dat<-rbind(dat0,dat1,dat2,dat3)
    
    #update parameters
    emm1<-glm(y~m+t+m*t+x,family = binomial(link='logit'),weights=wt,data=dat)
    emm2<-lm(m~t+x,weights=wt,data=dat)
    emm3<-glm(R_m~m+t+x,family = binomial(link='logit'),weights=wt,data=dat)
    emm4<-glm(R_y~y+t+x,family = binomial(link='logit'),weights=wt,data=dat)
    emb_0<-emm1$coef[1]
    emb_m<-emm1$coef[2]
    emb_t<-emm1$coef[3]
    emb_x<-emm1$coef[4]
    emb_mt<-emm1$coef[5]
    ema_0<-emm2$coef[1]
    ema_t<-emm2$coef[2]
    ema_x<-emm2$coef[3]
    emsd_m<-summary(emm2)$sigma
    emc_0<-emm3$coef[1]
    emc_m<-emm3$coef[2]
    emc_t<-emm3$coef[3]
    emc_x<-emm3$coef[4]
    emd_0<-emm4$coef[1]
    emd_y<-emm4$coef[2]
    emd_t<-emm4$coef[3]
    emd_x<-emm4$coef[4]
    
    k <- k + 1
    
    Q[[k]]<-c(emb_0,emb_m,emb_t,emb_mt,emb_x,ema_0,ema_t,ema_x,emsd_m,emc_0,emc_m,emc_t,emc_x,emd_0,emd_y,emd_t,emd_x)
    
}
  
  tTIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))-
        mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))
  tPDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))-
        mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))
  tPIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))-
        mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))
  tTDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))-
        mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=tb_0,b_m=tb_m,b_t=tb_t,b_mt=tb_mt,b_x=tb_x,a_0=ta_0,a_t=ta_t,a_x=ta_x,sd_m=tsd_m))))
  
  ccTIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))
  ccPDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))
  ccPIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))
  ccTDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=ccb_0,b_m=ccb_m,b_t=ccb_t,b_mt=ccb_mt,b_x=ccb_x,a_0=cca_0,a_t=cca_t,a_x=cca_x,sd_m=ccsd_m))))
  
  emTIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))
  emPDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))
  emPIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))
  emTDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))-
         mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=emb_0,b_m=emb_m,b_t=emb_t,b_mt=emb_mt,b_x=emb_x,a_0=ema_0,a_t=ema_t,a_x=ema_x,sd_m=emsd_m))))
  
  TIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))-
       mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))
  PDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int10,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))-
       mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))
  PIE<-mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))-
       mean(as.numeric(unlist(lapply(1:N,FUN=int00,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))
  TDE<-mean(as.numeric(unlist(lapply(1:N,FUN=int11,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))-
       mean(as.numeric(unlist(lapply(1:N,FUN=int01,data=data,b_0=b_0,b_m=b_m,b_t=b_t,b_mt=b_mt,b_x=b_x,a_0=a_0,a_t=a_t,a_x=a_x,sd_m=sd_m))))
  
  
matrix(c(ta_0,cca_0,ema_0,
         ta_t,cca_t,ema_t,
         ta_x,cca_x,ema_x,
         tb_0,ccb_0,emb_0,
         tb_m,ccb_m,emb_m,
         tb_t,ccb_t,emb_t,
         tb_x,ccb_x,emb_x,
         tb_mt,ccb_mt,emb_mt,
         tc_0,ccc_0,emc_0,
         tc_m,ccc_m,emc_m,
         tc_t,ccc_t,emc_t,
         tc_x,ccc_x,emc_x,
         td_0,ccd_0,emd_0,
         td_y,ccd_y,emd_y,
         td_t,ccd_t,emd_t,
         td_x,ccd_x,emd_x,
         (tTIE-TIE)/TIE,(ccTIE-TIE)/TIE,(emTIE-TIE)/TIE,
         (tPDE-PDE)/PDE,(ccPDE-PDE)/PDE,(emPDE-PDE)/PDE,
         (tPIE-PIE)/PIE,(ccPIE-PIE)/PIE,(emPIE-PIE)/PIE,
         (tTDE-TDE)/TDE,(ccTDE-TDE)/TDE,(emTDE-TDE)/TDE,
         miss_m,miss_m,miss_m,
         miss_y,miss_y,miss_y,
         miss_my,miss_my,miss_my),byrow=T,23,3)
  
}

#parameter set up
N<-1000
u_x<-0
sd_x<-1
p_t<-0.5
a_0<-0
a_t<-1
a_x<-1
sd_m<-0.5
b_0<-0
b_m<-2
b_t<-2
b_x<-1
b_mt<-1
c_0<-2
c_m<-4
c_t<-0.2
c_x<-0.2
d_0<-0.2
d_y<-2
d_t<-0.2
d_x<-0.2
sample<-20
sample_const<-10000
psd_m<-1
N_int<-10000

#time start
tic()
runs<-500
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
save <- mclapply(1:runs, func, mc.cores = 8, mc.set.seed = TRUE)
toc()
#time end

library("xlsx")
write.xlsx(save,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_III.xlsx',row.names = FALSE)




