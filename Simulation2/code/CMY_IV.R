library("xlsx")
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
d<-as.matrix(rbind(d_0,d_m,d_t,d_x))
R_m<-rbinom(N,1,exp(mtx%*%c)/(1+exp(mtx%*%c)))
R_y<-rbinom(N,1,exp(mtx%*%d)/(1+exp(mtx%*%d)))
t_m<-m
t_y<-y
m[R_m==0] <- NA
y[R_y==0] <- NA
data<-as.data.frame(cbind(t,m,x,y,R_m,t_m,R_y,t_y))
miss_m<-dplyr::count(data[which(data$R_m==0),])/N
miss_y<-dplyr::count(data[which(data$R_y==0),])/N
miss_my<-dplyr::count(data[which(data$R_m==0 & data$R_y==0),])/N
  
#oracle
m1<-lm(t_y~t_m+t+t_m*t+x,data=data)
m2<-lm(t_m~t+x,data=data)
m3<-glm(R_m~t_m+t+x,family=binomial(link='logit'),data=data)
m4<-glm(R_y~t_m+t+x,family=binomial(link='logit'),data=data)
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
td_m<-m4$coef[2]
td_t<-m4$coef[3]
td_x<-m4$coef[4]
  
#complete case analysis
ccm1<-lm(y~m+t+m*t+x,data=data,subset=(R_m==1 & R_y==1))
ccm2<-lm(m~t+x,data=data,subset=(R_m==1 & R_y==1))
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
ccd_m<-NA
ccd_t<-NA
ccd_x<-NA 
  
#mice for MAR
dat_mi<-subset(data, select = c(y,m,t,x,R_y,R_m))
#define predictor
pred <- mice(dat_mi, print = F)$predictorMatrix
pred[c('m'), c('R_y','R_m')] <- 0
pred[c('y'), c('R_y','R_m')] <- 0
imp <- mice(dat_mi, m = 5, pred = pred, print=F, seed=123)
mim1<-summary(pool(with(imp, lm(y~m+t+m*t+x))))
mim2<-summary(pool(with(imp, lm(m~t+x))))
mim3<-summary(pool(with(imp, glm(R_m~m+t+x,family=binomial(link='logit')))))
mim4<-summary(pool(with(imp, glm(R_y~m+t+x,family=binomial(link='logit')))))
mib_0<-mim1$estimate[1]
mib_m<-mim1$estimate[2]
mib_t<-mim1$estimate[3]
mib_x<-mim1$estimate[4]
mib_mt<-mim1$estimate[5]
#pooled residual standard deviation
sigma <- list()
for (i in 1:5) {
  imputed <- complete(imp, i)
  sigma[[i]] <- summary(lm(y~m+t+m*t+x, data = imputed))$sigma
}
misd_y<-mean(unlist(sigma))
mia_0<-mim2$estimate[1]
mia_t<-mim2$estimate[2]
mia_x<-mim2$estimate[3]  
#pooled residual standard deviation
sigma <- list()
for (i in 1:5) {
  imputed <- complete(imp, i)
  sigma[[i]] <- summary(lm(m~t+x, data = imputed))$sigma
}
misd_m<-mean(unlist(sigma)) 
mic_0<-mim3$estimate[1]
mic_m<-mim3$estimate[2]
mic_t<-mim3$estimate[3]
mic_x<-mim3$estimate[4]
mid_0<-mim4$estimate[1]
mid_m<-mim4$estimate[2]
mid_t<-mim4$estimate[3]
mid_x<-mim4$estimate[4]
  
#EM
#fractional imputation for continuous var
dat0<-subset(data,R_m==1 & R_y==1)
dat0$seq<-0
#missing m
dat1<-subset(data,R_m==0 & R_y==1)
dat1$seq<-1:nrow(dat1)
dat1<-data.frame(lapply(dat1, rep, sample))
dat1$m<-rnorm(as.numeric(count(dat1)),cca_0+cca_t*dat1$t+cca_x*dat1$x,psd_m)
#missing y
dat2<-subset(data,R_m==1 & R_y==0)
dat2$seq<-0
#missing m and y
dat3<-subset(data,R_m==0 & R_y==0)
dat3$seq<-1:nrow(dat3)
dat3<-data.frame(lapply(dat3, rep, sample))
dat3$m<-rnorm(as.numeric(count(dat3)),cca_0+cca_t*dat3$t+cca_x*dat3$x,psd_m)
  
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
emd_m<-mid_m
emd_t<-mid_t
emd_x<-mid_x

#weight of m when y is observed
cm_weight <- function(y,m,t,x){
  #joint dist  
  p <- 
    (1/(emsd_y*sqrt(2*pi)))*exp((-0.5*((y-(emb_0+emb_m*m+emb_t*t+emb_mt*m*t+emb_x*x))/emsd_y)^2))*
    (1/(emsd_m*sqrt(2*pi)))*exp((-0.5*((m-(ema_0+ema_t*t+ema_x*x))/emsd_m)^2))*
    (1/(1+exp(emc_0+emc_m*m+emc_t*t+emc_x*x)))*
    (exp(emd_0+emd_m*m+emd_t*t+emd_x*x)/(1+exp(emd_0+emd_m*m+emd_t*t+emd_x*x)))
  #proposed dist  
  h <- 
    (1/(psd_m*sqrt(2*pi)))*exp((-0.5*((m-(cca_0+cca_t*t+cca_x*x))/psd_m)^2))
  
  return(p/h)
}

#weight of m when y is missing
cmy_weight <- function(m,t,x){
  #joint dist  
  p <- 
    (1/(emsd_m*sqrt(2*pi)))*exp((-0.5*((m-(ema_0+ema_t*t+ema_x*x))/emsd_m)^2))*
    (1/(1+exp(emc_0+emc_m*m+emc_t*t+emc_x*x)))*
    (1/(1+exp(emd_0+emd_m*m+emd_t*t+emd_x*x)))
  #proposed dist  
  h <- 
    (1/(psd_m*sqrt(2*pi)))*exp((-0.5*((m-(cca_0+cca_t*t+cca_x*x))/psd_m)^2))
  
  return(p/h)
}
  
Q <- NULL
Q[[1]] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
Q[[2]] <- c(emb_0,emb_m,emb_t,emb_mt,emb_x,emsd_y,ema_0,ema_t,ema_x,emsd_m,emc_0,emc_m,emc_t,emc_x,emd_0,emd_m,emd_t,emd_x)
k<-2
while (sum(abs(Q[[k]]-Q[[k-1]]))/sum(Q[[k-1]])>=1e-5) {
    
    dat0$wt<-1
    
    #missing m
    dat1$wt<-cm_weight(dat1$y,dat1$m,dat1$t,dat1$x)
    dat1<-dat1 %>% group_by(seq) %>% mutate(wt=wt/sum(wt))
    
    #missing y
    dat2$wt<-1
    
    #missing m and y
    dat3$wt<-cmy_weight(dat3$m,dat3$t,dat3$x)
    dat3<-dat3 %>% group_by(seq) %>% mutate(wt=wt/sum(wt))
    
    dat<-rbind(dat0,dat1,dat2,dat3)
    
    #update parameters
    emm1<-lm(y~m+t+m*t+x,weights=wt,data=dat,subset=(R_m==1 & R_y==1))
    emm2<-lm(m~t+x,weights=wt,data=dat)
    emm3<-glm(R_m~m+t+x,family = "binomial",weights=wt,data=dat)
    emm4<-glm(R_y~m+t+x,family = "binomial",weights=wt,data=dat)
    emb_0<-emm1$coef[1]
    emb_m<-emm1$coef[2]
    emb_t<-emm1$coef[3]
    emb_x<-emm1$coef[4]
    emb_mt<-emm1$coef[5]
    emsd_y<-summary(emm1)$sigma
    ema_0<-emm2$coef[1]
    ema_t<-emm2$coef[2]
    ema_x<-emm2$coef[3]
    emsd_m<-sqrt(sum(dat$wt*(emm2$residuals)^2)/(sum(dat$wt)-emm2$rank))
    emc_0<-emm3$coef[1]
    emc_m<-emm3$coef[2]
    emc_t<-emm3$coef[3]
    emc_x<-emm3$coef[4]
    emd_0<-emm4$coef[1]
    emd_m<-emm4$coef[2]
    emd_t<-emm4$coef[3]
    emd_x<-emm4$coef[4]
    
    k <- k + 1
    
    Q[[k]]<-c(emb_0,emb_m,emb_t,emb_mt,emb_x,emsd_y,ema_0,ema_t,ema_x,emsd_m,emc_0,emc_m,emc_t,emc_x,emd_0,emd_m,emd_t,emd_x)
    
}
  
tTIE<-ta_t*(tb_m+tb_mt)
tPDE<-tb_t+tb_mt*(ta_0+ta_x*mean(x))
tPIE<-ta_t*tb_m
tTDE<-tb_t+tb_mt*(ta_0+ta_t+ta_x*mean(x))

ccTIE<-cca_t*(ccb_m+ccb_mt)
ccPDE<-ccb_t+ccb_mt*(cca_0+cca_x*mean(x))
ccPIE<-cca_t*ccb_m
ccTDE<-ccb_t+ccb_mt*(cca_0+cca_t+cca_x*mean(x))

miTIE<-mia_t*(mib_m+mib_mt)
miPDE<-mib_t+mib_mt*(mia_0+mia_x*mean(x))
miPIE<-mia_t*mib_m
miTDE<-mib_t+mib_mt*(mia_0+mia_t+mia_x*mean(x))

emTIE<-ema_t*(emb_m+emb_mt)
emPDE<-emb_t+emb_mt*(ema_0+ema_x*mean(x))
emPIE<-ema_t*emb_m
emTDE<-emb_t+emb_mt*(ema_0+ema_t+ema_x*mean(x))
  
matrix(c(ta_0,cca_0,mia_0,ema_0,
         ta_t,cca_t,mia_t,ema_t,
         ta_x,cca_x,mia_x,ema_x,
         tsd_m,ccsd_m,misd_m,emsd_m,
         tb_0,ccb_0,mib_0,emb_0,
         tb_m,ccb_m,mib_m,emb_m,
         tb_t,ccb_t,mib_t,emb_t,
         tb_x,ccb_x,mib_x,emb_x,
         tb_mt,ccb_mt,mib_mt,emb_mt,
         tsd_y,ccsd_y,misd_y,emsd_y,
         tc_0,ccc_0,mic_0,emc_0,
         tc_m,ccc_m,mic_m,emc_m,
         tc_t,ccc_t,mic_t,emc_t,
         tc_x,ccc_x,mic_x,emc_x,
         td_0,ccd_0,mid_0,emd_0,
         td_m,ccd_m,mid_m,emd_m,
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
         k,k,k,k),byrow=T,30,4)
}

#parameter set up
N<-1000
u_x<-0
sd_x<-1
p_t<-0.5
a_0<-0
a_t<-1
a_x<-1
sd_m<-1
b_0<-0
b_m<-1
b_t<-1
b_x<-1
b_mt<-1
sd_y<-1
c_0<-1.4
c_m<-1
c_t<-1
c_x<-1
d_0<-1.4
d_m<-1
d_t<-1
d_x<-1
sample<-100
psd_m<-1

TIE<-a_t*(b_m+b_mt) #NIE
PDE<-b_t+b_mt*a_0   #NDE
PIE<-a_t*b_m
TDE<-b_t+b_mt*(a_0+a_t)

#time start
start <- Sys.time()
runs<-500
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
save <- mclapply(1:runs, func, mc.cores = 8, mc.set.seed = TRUE)
end <- Sys.time()
#time end
timediff <- as.numeric(difftime(end, start, units="hours"))
write.xlsx(timediff,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation2/Compute_time(hours).xlsx',
           row.names = FALSE,sheetName="CMY_IV",append=TRUE)

write.xlsx(save,'/Users/sushi5824907/Desktop/Mediation/JASA/Simulation2/CMY_IV.xlsx',row.names = FALSE)
