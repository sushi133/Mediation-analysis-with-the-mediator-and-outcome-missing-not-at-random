library(tictoc)
library(mice)
library(dplyr)
library(parallel)

func<-function(i){
  
#complete data
x<-rnorm(N,u_x,sd_x)
t<-rbinom(N,1,p_t)
m<-rnorm(N,a_0+a_t*t+a_x*x,sd_m)
y<-rnorm(N,b_0+b_m*m+b_t*t+b_mt*m*t+b_x*x,sd_y)
  
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
dat<-as.data.frame(cbind(t,m,x,y,R_m,R_y,t_m,t_y))
miss_m<-dplyr::count(dat[which(dat$R_m==0),])/N
miss_y<-dplyr::count(dat[which(dat$R_y==0),])/N
miss_my<-dplyr::count(dat[which(dat$R_m==0 & dat$R_y==0),])/N
  
#oracle
m1<-lm(t_y~t_m+t+t_m*t+x,data=dat)
m2<-lm(t_m~t+x,data=dat)
m3<-glm(R_m~t_m+t+x,family=binomial(link='logit'),data=dat)
m4<-glm(R_y~t_y+t+x,family=binomial(link='logit'),data=dat)
tb_0<-m1$coef[1]
tb_m<-m1$coef[2]
tb_t<-m1$coef[3]
tb_x<-m1$coef[4]
tb_mt<-m1$coef[5]
tsd_y<-summary(m1)$sigma
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
ccm1<-lm(y~m+t+m*t+x,data=dat,subset=(R_m==1 & R_y==1))
ccm2<-lm(m~t+x,data=dat,subset=(R_m==1 & R_y==1))
ccb_0<-ccm1$coef[1]
ccb_m<-ccm1$coef[2]
ccb_t<-ccm1$coef[3]
ccb_x<-ccm1$coef[4]
ccb_mt<-ccm1$coef[5]
ccsd_y<-summary(ccm1)$sigma
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
dat_mi<-subset(dat, select = c(y,m,t,x,R_y,R_m))
imp <- mice(dat_mi, print = F)
#define predictor
pred <- imp$predictorMatrix
pred[c('m'), c('y','R_y','R_m')] <- 0
pred[c('y'), c('R_y','R_m')] <- 0
imp <- mice(dat_mi, m = 5, pred = pred, print=F, seed=123)
mim1<-summary(pool(with(imp, lm(y~m+t+m*t+x))))
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
dat0<-subset(dat,R_m==1 & R_y==1)
#missing m
dat1<-subset(dat,R_m==0 & R_y==1)
dat1<-data.frame(lapply(dat1, rep, sample))
dat1$m<-rnorm(as.numeric(count(dat1)),cca_0+cca_t*dat1$t+cca_x*dat1$x,psd_m)
#missing y
dat2<-subset(dat,R_m==1 & R_y==0)
dat2<-data.frame(lapply(dat2, rep, sample))
dat2$y<-rnorm(as.numeric(count(dat2)),ccb_0+ccb_m*dat2$m+ccb_t*dat2$t+ccb_mt*dat2$m*dat2$t+ccb_x*dat2$x,psd_y)
#missing m and y
dat3<-subset(dat,R_m==0 & R_y==0)
dat3<-data.frame(lapply(dat3, rep, sample))
dat3$m<-rnorm(as.numeric(count(dat3)),cca_0+cca_t*dat3$t+cca_x*dat3$x,psd_m)
dat3$y<-rnorm(as.numeric(count(dat3)),ccb_0+ccb_m*dat3$m+ccb_t*dat3$t+ccb_mt*dat3$m*dat3$t+ccb_x*dat3$x,psd_y)
  
#initial parameters
emb_0<-ccb_0
emb_m<-ccb_m
emb_t<-ccb_t
emb_mt<-ccb_mt
emb_x<-ccb_x
emsd_y<-ccsd_y
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
  
Q <- NULL
Q[[1]] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
Q[[2]] <- c(emb_0,emb_m,emb_t,emb_mt,emb_x,emsd_y,ema_0,ema_t,ema_x,emsd_m,emc_0,emc_m,emc_t,emc_x,emd_0,emd_y,emd_t,emd_x)
k<-2
while (sum(abs(Q[[k]]-Q[[k-1]]))/sum(Q[[k-1]])>=0.01 & k < 100) {
    
    dat0$wt<-1
    
    #missing m
    cm_weight <- function(k1){
      cp <- function(m){
        cp <- 
          (1/(emsd_y*sqrt(2*pi)))*exp((-0.5*((dat1[k1,]$y-(emb_0+emb_m*m+emb_t*dat1[k1,]$t+emb_mt*m*dat1[k1,]$t+emb_x*dat1[k1,]$x))/emsd_y)^2))*
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
    cy_weight <- function(k2){
      cp <- function(y){
        cp <- 
          (1/(emsd_y*sqrt(2*pi)))*exp((-0.5*((y-(emb_0+emb_m*dat2[k2,]$m+emb_t*dat2[k2,]$t+emb_mt*dat2[k2,]$m*dat2[k2,]$t+emb_x*dat2[k2,]$x))/emsd_y)^2))*
          (1/(1+exp(emd_0+emd_y*y+emd_t*dat2[k2,]$t+emd_x*dat2[k2,]$x)))
        return(cp)
      }
      pp <- function(y){
        pp <- 
          (1/(psd_y*sqrt(2*pi)))*exp((-0.5*((y-(ccb_0+ccb_m*dat2[k2,]$m+ccb_t*dat2[k2,]$t+ccb_mt*dat2[k2,]$m*dat2[k2,]$t+ccb_x*dat2[k2,]$x))/psd_y)^2))
        return(pp)
      }
      #normalizing constant
      ps <- rnorm(sample_const,ccb_0+ccb_m*dat2[k2,]$m+ccb_t*dat2[k2,]$t+ccb_mt*dat2[k2,]$m*dat2[k2,]$t+ccb_x*dat2[k2,]$x,psd_y)
      wt <- ((cp(dat2[k2,]$y)/pp(dat2[k2,]$y))/mean(cp(ps)/pp(ps)))/sample
    }
    dat2$wt<-as.numeric(unlist(lapply(1:as.numeric(dplyr::count(dat2)),FUN=cy_weight)))
    
    
    #missing m and y
    cmy_weight <- function(k3){
      cp <- function(m,y){
        cp <- 
          (1/(emsd_y*sqrt(2*pi)))*exp((-0.5*((y-(emb_0+emb_m*m+emb_t*dat3[k3,]$t+emb_mt*m*dat3[k3,]$t+emb_x*dat3[k3,]$x))/emsd_y)^2))*
          (1/(emsd_m*sqrt(2*pi)))*exp((-0.5*((m-(ema_0+ema_t*dat3[k3,]$t+ema_x*dat3[k3,]$x))/emsd_m)^2))*
          (1/(1+exp(emc_0+emc_m*m+emc_t*dat3[k3,]$t+emc_x*dat3[k3,]$x)))*
          (1/(1+exp(emd_0+emd_y*y+emd_t*dat3[k3,]$t+emd_x*dat3[k3,]$x)))
        return(cp)
      }
      pp <- function(m,y){
        pp <- 
          (1/(psd_y*sqrt(2*pi)))*exp((-0.5*((y-(ccb_0+ccb_m*m+ccb_t*dat3[k3,]$t+ccb_mt*m*dat3[k3,]$t+ccb_x*dat3[k3,]$x))/psd_y)^2))*
          (1/(psd_m*sqrt(2*pi)))*exp((-0.5*((m-(cca_0+cca_t*dat3[k3,]$t+cca_x*dat3[k3,]$x))/psd_m)^2))
        return(pp)
      }
      #normalizing constant
      ps_m <- rnorm(sample_const,cca_0+cca_t*dat3[k3,]$t+cca_x*dat3[k3,]$x,psd_m)
      ps_y <- rnorm(sample_const,ccb_0+ccb_m*ps_m+ccb_t*dat3[k3,]$t+ccb_mt*ps_m*dat3[k3,]$t+ccb_x*dat3[k3,]$x,psd_y)
      wt <- ((cp(dat3[k3,]$m,dat3[k3,]$y)/pp(dat3[k3,]$m,dat3[k3,]$y))/mean(cp(ps_m,ps_y)/pp(ps_m,ps_y)))/sample
    }
    
    dat3$wt<-as.numeric(unlist(lapply(1:as.numeric(dplyr::count(dat3)),FUN=cmy_weight)))
    
    
    dat<-rbind(dat0,dat1,dat2,dat3)
    
    #update parameters
    emm1<-lm(y~m+t+m*t+x,weights=wt,data=dat)
    emm2<-lm(m~t+x,weights=wt,data=dat)
    emm3<-glm(R_m~m+t+x,family = "binomial",weights=wt,data=dat)
    emm4<-glm(R_y~y+t+x,family = "binomial",weights=wt,data=dat)
    emb_0<-emm1$coef[1]
    emb_m<-emm1$coef[2]
    emb_t<-emm1$coef[3]
    emb_x<-emm1$coef[4]
    emb_mt<-emm1$coef[5]
    emsd_y<-summary(emm1)$sigma
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
    
    Q[[k]]<-c(emb_0,emb_m,emb_t,emb_mt,emb_x,emsd_y,ema_0,ema_t,ema_x,emsd_m,emc_0,emc_m,emc_t,emc_x,emd_0,emd_y,emd_t,emd_x)
    
}
  
tTIE<-ta_t*(tb_m+tb_mt)
tPDE<-tb_t+tb_mt*ta_0
tPIE<-ta_t*tb_m
tTDE<-tb_t+tb_mt*(ta_0+ta_t)
  
ccTIE<-cca_t*(ccb_m+ccb_mt)
ccPDE<-ccb_t+ccb_mt*cca_0
ccPIE<-cca_t*ccb_m
ccTDE<-ccb_t+ccb_mt*(cca_0+cca_t)
  
emTIE<-ema_t*(emb_m+emb_mt)
emPDE<-emb_t+emb_mt*ema_0
emPIE<-ema_t*emb_m
emTDE<-emb_t+emb_mt*(ema_0+ema_t)
  
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
         (tTIE-TIE)/PDE,(ccTIE-TIE)/PDE,(emTIE-TIE)/PDE,
         (tPDE-PDE)/PDE,(ccPDE-PDE)/PDE,(emPDE-PDE)/PDE,
         (tPIE-PIE)/TDE,(ccPIE-PIE)/TDE,(emPIE-PIE)/TDE,
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
b_m<-0
b_t<-2
b_x<-1
b_mt<-0
sd_y<-0.5
c_0<-2
c_m<-4
c_t<-0.2
c_x<-0.2
d_0<-2
d_y<-10
d_t<-0.2
d_x<-0.2
sample<-20
sample_const<-10000
psd_m<-1
psd_y<-1

TIE<-a_t*(b_m+b_mt)
PDE<-b_t+b_mt*a_0
PIE<-a_t*b_m
TDE<-b_t+b_mt*(a_0+a_t)

#time start
tic()
runs<-500
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
save <- mclapply(1:runs, func, mc.cores = 8, mc.set.seed = TRUE)
toc()
#time end

library("xlsx")
write.xlsx(save,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_III(0).xlsx',row.names = FALSE)

