library(dplyr)
library(mice)
library(parallel)
library(boot)
library(MASS)
set.seed(123)

#purpose: calculate E[Y(1,M(1))] 
#arguments: data: population of interest, a_0-b0_x75: parameters in M and Y models
#output: E[Y(1,M(1))]
int11<-function(data,
                a_0,a_t,a_x1,a_x21,a_x22,a_x31,a_x32,a_x33,a_x4,a_x51,a_x52,a_x62,a_x63,a_x64,a_x71,a_x72,a_x73,a_x74,a_x75,
                b_0,b_m,b_t,b_mt,b_x1,b_x21,b_x22,b_x31,b_x32,b_x33,b_x4,b_x51,b_x52,b_x62,b_x63,b_x64,b_x71,b_x72,b_x73,b_x74,b_x75,
                b0_0,b0_m,b0_t,b0_mt,b0_x1,b0_x21,b0_x22,b0_x31,b0_x32,b0_x33,b0_x4,b0_x51,b0_x52,b0_x62,b0_x63,b0_x64,b0_x71,b0_x72,b0_x73,b0_x74,b0_x75){
  
  mean(exp(b_0+b_m*1+b_t*1+b_mt*1*1+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*1+b0_t*1+b0_mt*1*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*1+b0_t*1+b0_mt*1*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (exp(a_0+a_t*1+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))/
      (1+exp(a_0+a_t*1+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))))+
      exp(b_0+b_m*0+b_t*1+b_mt*0*1+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*0+b0_t*1+b0_mt*0*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*0+b0_t*1+b0_mt*0*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (1/(1+exp(a_0+a_t*1+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5)))))
}
#purpose: calculate E[Y(1,M(0))] 
#arguments: data: population of interest, a_0-b0_x75: parameters in M and Y models
#output: E[Y(1,M(0))]
int10<-function(data,
                a_0,a_t,a_x1,a_x21,a_x22,a_x31,a_x32,a_x33,a_x4,a_x51,a_x52,a_x62,a_x63,a_x64,a_x71,a_x72,a_x73,a_x74,a_x75,
                b_0,b_m,b_t,b_mt,b_x1,b_x21,b_x22,b_x31,b_x32,b_x33,b_x4,b_x51,b_x52,b_x62,b_x63,b_x64,b_x71,b_x72,b_x73,b_x74,b_x75,
                b0_0,b0_m,b0_t,b0_mt,b0_x1,b0_x21,b0_x22,b0_x31,b0_x32,b0_x33,b0_x4,b0_x51,b0_x52,b0_x62,b0_x63,b0_x64,b0_x71,b0_x72,b0_x73,b0_x74,b0_x75){
  
  mean(exp(b_0+b_m*1+b_t*1+b_mt*1*1+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*1+b0_t*1+b0_mt*1*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*1+b0_t*1+b0_mt*1*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (exp(a_0+a_t*0+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))/
      (1+exp(a_0+a_t*0+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))))+
      exp(b_0+b_m*0+b_t*1+b_mt*0*1+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*0+b0_t*1+b0_mt*0*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*0+b0_t*1+b0_mt*0*1+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (1/(1+exp(a_0+a_t*0+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5)))))
}
#purpose: calculate E[Y(0,M(1))] 
#arguments: data: population of interest, a_0-b0_x75: parameters in M and Y models
#output: E[Y(0,M(1))]
int01<-function(data,
                a_0,a_t,a_x1,a_x21,a_x22,a_x31,a_x32,a_x33,a_x4,a_x51,a_x52,a_x62,a_x63,a_x64,a_x71,a_x72,a_x73,a_x74,a_x75,
                b_0,b_m,b_t,b_mt,b_x1,b_x21,b_x22,b_x31,b_x32,b_x33,b_x4,b_x51,b_x52,b_x62,b_x63,b_x64,b_x71,b_x72,b_x73,b_x74,b_x75,
                b0_0,b0_m,b0_t,b0_mt,b0_x1,b0_x21,b0_x22,b0_x31,b0_x32,b0_x33,b0_x4,b0_x51,b0_x52,b0_x62,b0_x63,b0_x64,b0_x71,b0_x72,b0_x73,b0_x74,b0_x75){
  
  mean(exp(b_0+b_m*1+b_t*0+b_mt*1*0+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*1+b0_t*0+b0_mt*1*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*1+b0_t*0+b0_mt*1*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (exp(a_0+a_t*1+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))/
      (1+exp(a_0+a_t*1+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))))+
      exp(b_0+b_m*0+b_t*0+b_mt*0*0+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*0+b0_t*0+b0_mt*0*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*0+b0_t*0+b0_mt*0*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (1/(1+exp(a_0+a_t*1+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5)))))
}
#purpose: calculate E[Y(0,M(0))] 
#arguments: data: population of interest, a_0-b0_x75: parameters in M and Y models
#output: E[Y(0,M(0))]
int00<-function(data,
                a_0,a_t,a_x1,a_x21,a_x22,a_x31,a_x32,a_x33,a_x4,a_x51,a_x52,a_x62,a_x63,a_x64,a_x71,a_x72,a_x73,a_x74,a_x75,
                b_0,b_m,b_t,b_mt,b_x1,b_x21,b_x22,b_x31,b_x32,b_x33,b_x4,b_x51,b_x52,b_x62,b_x63,b_x64,b_x71,b_x72,b_x73,b_x74,b_x75,
                b0_0,b0_m,b0_t,b0_mt,b0_x1,b0_x21,b0_x22,b0_x31,b0_x32,b0_x33,b0_x4,b0_x51,b0_x52,b0_x62,b0_x63,b0_x64,b0_x71,b0_x72,b0_x73,b0_x74,b0_x75){
  
  mean(exp(b_0+b_m*1+b_t*0+b_mt*1*0+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*1+b0_t*0+b0_mt*1*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*1+b0_t*0+b0_mt*1*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (exp(a_0+a_t*0+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))/
      (1+exp(a_0+a_t*0+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5))))+
      exp(b_0+b_m*0+b_t*0+b_mt*0*0+b_x1*I(data$x1==1)+b_x21*I(data$x2==1)+b_x22*I(data$x2==2)+b_x31*I(data$x3==1)+b_x32*I(data$x3==2)+b_x33*I(data$x3==3)+b_x4*I(data$x4==1)+b_x51*I(data$x5==1)+b_x52*I(data$x5==2)+b_x62*I(data$x6==2)+b_x63*I(data$x6==3)+b_x64*I(data$x6==4)+b_x71*I(data$x7==1)+b_x72*I(data$x7==2)+b_x73*I(data$x7==3)+b_x74*I(data$x7==4)+b_x75*I(data$x7==5))*
      (exp(b0_0+b0_m*0+b0_t*0+b0_mt*0*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))/
      (1+exp(b0_0+b0_m*0+b0_t*0+b0_mt*0*0+b0_x1*I(data$x1==1)+b0_x21*I(data$x2==1)+b0_x22*I(data$x2==2)+b0_x31*I(data$x3==1)+b0_x32*I(data$x3==2)+b0_x33*I(data$x3==3)+b0_x4*I(data$x4==1)+b0_x51*I(data$x5==1)+b0_x52*I(data$x5==2)+b0_x62*I(data$x6==2)+b0_x63*I(data$x6==3)+b0_x64*I(data$x6==4)+b0_x71*I(data$x7==1)+b0_x72*I(data$x7==2)+b0_x73*I(data$x7==3)+b0_x74*I(data$x7==4)+b0_x75*I(data$x7==5))))*
      (1/(1+exp(a_0+a_t*0+a_x1*I(data$x1==1)+a_x21*I(data$x2==1)+a_x22*I(data$x2==2)+a_x31*I(data$x3==1)+a_x32*I(data$x3==2)+a_x33*I(data$x3==3)+a_x4*I(data$x4==1)+a_x51*I(data$x5==1)+a_x52*I(data$x5==2)+a_x62*I(data$x6==2)+a_x63*I(data$x6==3)+a_x64*I(data$x6==4)+a_x71*I(data$x7==1)+a_x72*I(data$x7==2)+a_x73*I(data$x7==3)+a_x74*I(data$x7==4)+a_x75*I(data$x7==5)))))
}

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

#complete case analysis
ccm11<-glm(I_Y~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit'),data=data,subset=(R_m==1 & R_y==1))
ccm1<-glm(Y~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family=Gamma(link = "log"),data=data,subset=(R_m==1 & R_y==1 & I_Y==1))
ccm2<-glm(M~Z+x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit'),data=data,subset=(R_m==1 & R_y==1))

ccb0_0<-ccm11$coef[1]
ccb0_m<-ccm11$coef[2]
ccb0_t<-ccm11$coef[3]
ccb0_x1<-ccm11$coef[4]
ccb0_x21<-ccm11$coef[5]
ccb0_x22<-ccm11$coef[6]
ccb0_x31<-ccm11$coef[7]
ccb0_x32<-ccm11$coef[8]
ccb0_x33<-ccm11$coef[9]
ccb0_x4<-ccm11$coef[10]
ccb0_x51<-ccm11$coef[11]
ccb0_x52<-ccm11$coef[12]
ccb0_x62<-ccm11$coef[13]
ccb0_x63<-ccm11$coef[14]
ccb0_x64<-ccm11$coef[15]
ccb0_x71<-ccm11$coef[16]
ccb0_x72<-ccm11$coef[17]
ccb0_x73<-ccm11$coef[18]
ccb0_x74<-ccm11$coef[19]
ccb0_x75<-ccm11$coef[20]
ccb0_mt<-ccm11$coef[21]

ccb_0<-ccm1$coef[1]
ccb_m<-ccm1$coef[2]
ccb_t<-ccm1$coef[3]
ccb_x1<-ccm1$coef[4]
ccb_x21<-ccm1$coef[5]
ccb_x22<-ccm1$coef[6]
ccb_x31<-ccm1$coef[7]
ccb_x32<-ccm1$coef[8]
ccb_x33<-ccm1$coef[9]
ccb_x4<-ccm1$coef[10]
ccb_x51<-ccm1$coef[11]
ccb_x52<-ccm1$coef[12]
ccb_x62<-ccm1$coef[13]
ccb_x63<-ccm1$coef[14]
ccb_x64<-ccm1$coef[15]
ccb_x71<-ccm1$coef[16]
ccb_x72<-ccm1$coef[17]
ccb_x73<-ccm1$coef[18]
ccb_x74<-ccm1$coef[19]
ccb_x75<-ccm1$coef[20]
ccb_mt<-ccm1$coef[21]
cc_shape<-as.numeric(gamma.shape(ccm1)[1])

cca_0<-ccm2$coef[1]
cca_t<-ccm2$coef[2]
cca_x1<-ccm2$coef[3]
cca_x21<-ccm2$coef[4]
cca_x22<-ccm2$coef[5]
cca_x31<-ccm2$coef[6]
cca_x32<-ccm2$coef[7]
cca_x33<-ccm2$coef[8]
cca_x4<-ccm2$coef[9]
cca_x51<-ccm2$coef[10]
cca_x52<-ccm2$coef[11]
cca_x62<-ccm2$coef[12]
cca_x63<-ccm2$coef[13]
cca_x64<-ccm2$coef[14]
cca_x71<-ccm2$coef[15]
cca_x72<-ccm2$coef[16]
cca_x73<-ccm2$coef[17]
cca_x74<-ccm2$coef[18]
cca_x75<-ccm2$coef[19]

ccc_0<-NA
ccc_m<-NA
ccc_t<-NA
ccc_x1<-NA
ccc_x21<-NA
ccc_x22<-NA
ccc_x31<-NA
ccc_x32<-NA
ccc_x33<-NA
ccc_x4<-NA
ccc_x51<-NA
ccc_x52<-NA
ccc_x62<-NA
ccc_x63<-NA
ccc_x64<-NA
ccc_x71<-NA
ccc_x72<-NA
ccc_x73<-NA
ccc_x74<-NA
ccc_x75<-NA

ccd_0<-NA
ccd_rm<-NA
ccd_t<-NA
ccd_x1<-NA
ccd_x21<-NA
ccd_x22<-NA
ccd_x31<-NA
ccd_x32<-NA
ccd_x33<-NA
ccd_x4<-NA
ccd_x51<-NA
ccd_x52<-NA
ccd_x62<-NA
ccd_x63<-NA
ccd_x64<-NA
ccd_x71<-NA
ccd_x72<-NA
ccd_x73<-NA
ccd_x74<-NA
ccd_x75<-NA

#mice for MAR
data_mi<-subset(data, select = c(Y,M,Z,x1,x2,x3,x4,x5,x6,x7,R_y,R_m))
data_mi$M<-as.factor(data_mi$M)
#define predictor 
pred <- mice(data_mi, print = F)$predictorMatrix
pred[c('M'), c('R_y','R_m')] <- 0
pred[c('Y'), c('R_y','R_m')] <- 0
imp <- mice(data_mi, m = 5, pred = pred, print=F, seed=123)
mim11<-summary(pool(with(imp, glm(I(Y>0)~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit')))))
mim1<-summary(pool(with(imp, glm(Y~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family=Gamma(link = "log"),subset=(Y>0)))))
mim2<-summary(pool(with(imp, glm(M~Z+x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit')))))
mim3<-summary(pool(with(imp, glm(R_m~M+Z+x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit')))))
mim4<-summary(pool(with(imp, glm(R_y~R_m+Z+x1+x2+x3+x4+x5+x6+x7,family=binomial(link='logit')))))

mib0_0<-mim11$estimate[1]
mib0_m<-mim11$estimate[2]
mib0_t<-mim11$estimate[3]
mib0_x1<-mim11$estimate[4]
mib0_x21<-mim11$estimate[5]
mib0_x22<-mim11$estimate[6]
mib0_x31<-mim11$estimate[7]
mib0_x32<-mim11$estimate[8]
mib0_x33<-mim11$estimate[9]
mib0_x4<-mim11$estimate[10]
mib0_x51<-mim11$estimate[11]
mib0_x52<-mim11$estimate[12]
mib0_x62<-mim11$estimate[13]
mib0_x63<-mim11$estimate[14]
mib0_x64<-mim11$estimate[15]
mib0_x71<-mim11$estimate[16]
mib0_x72<-mim11$estimate[17]
mib0_x73<-mim11$estimate[18]
mib0_x74<-mim11$estimate[19]
mib0_x75<-mim11$estimate[20]
mib0_mt<-mim11$estimate[21]

mib_0<-mim1$estimate[1]
mib_m<-mim1$estimate[2]
mib_t<-mim1$estimate[3]
mib_x1<-mim1$estimate[4]
mib_x21<-mim1$estimate[5]
mib_x22<-mim1$estimate[6]
mib_x31<-mim1$estimate[7]
mib_x32<-mim1$estimate[8]
mib_x33<-mim1$estimate[9]
mib_x4<-mim1$estimate[10]
mib_x51<-mim1$estimate[11]
mib_x52<-mim1$estimate[12]
mib_x62<-mim1$estimate[13]
mib_x63<-mim1$estimate[14]
mib_x64<-mim1$estimate[15]
mib_x71<-mim1$estimate[16]
mib_x72<-mim1$estimate[17]
mib_x73<-mim1$estimate[18]
mib_x74<-mim1$estimate[19]
mib_x75<-mim1$estimate[20]
mib_mt<-mim1$estimate[21]
#pooled gamma shape parameter
shape <- list()
for (i in 1:5) {
  imputed <- complete(imp, i)
  shape[[i]] <- as.numeric(gamma.shape(glm(Y~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family=Gamma(link = "log"),data = imputed,subset=(Y>0)))[1]) 
}
mi_shape<-mean(unlist(shape)) 

mia_0<-mim2$estimate[1]
mia_t<-mim2$estimate[2]
mia_x1<-mim2$estimate[3]
mia_x21<-mim2$estimate[4]
mia_x22<-mim2$estimate[5]
mia_x31<-mim2$estimate[6]
mia_x32<-mim2$estimate[7]
mia_x33<-mim2$estimate[8]
mia_x4<-mim2$estimate[9]
mia_x51<-mim2$estimate[10]
mia_x52<-mim2$estimate[11]
mia_x62<-mim2$estimate[12]
mia_x63<-mim2$estimate[13]
mia_x64<-mim2$estimate[14]
mia_x71<-mim2$estimate[15]
mia_x72<-mim2$estimate[16]
mia_x73<-mim2$estimate[17]
mia_x74<-mim2$estimate[18]
mia_x75<-mim2$estimate[19]

mic_0<-mim3$estimate[1]
mic_m<-mim3$estimate[2]
mic_t<-mim3$estimate[3]
mic_x1<-mim3$estimate[4]
mic_x21<-mim3$estimate[5]
mic_x22<-mim3$estimate[6] 
mic_x31<-mim3$estimate[7] 
mic_x32<-mim3$estimate[8] 
mic_x33<-mim3$estimate[9] 
mic_x4<-mim3$estimate[10] 
mic_x51<-mim3$estimate[11] 
mic_x52<-mim3$estimate[12]
mic_x62<-mim3$estimate[13] 
mic_x63<-mim3$estimate[14]
mic_x64<-mim3$estimate[15]
mic_x71<-mim3$estimate[16] 
mic_x72<-mim3$estimate[17] 
mic_x73<-mim3$estimate[18] 
mic_x74<-mim3$estimate[19] 
mic_x75<-mim3$estimate[20] 

mid_0<-mim4$estimate[1]
mid_rm<-mim4$estimate[2]
mid_t<-mim4$estimate[3]
mid_x1<-mim4$estimate[4]
mid_x21<-mim4$estimate[5]
mid_x22<-mim4$estimate[6] 
mid_x31<-mim4$estimate[7] 
mid_x32<-mim4$estimate[8]
mid_x33<-mim4$estimate[9]
mid_x4<-mim4$estimate[10] 
mid_x51<-mim4$estimate[11] 
mid_x52<-mim4$estimate[12]
mid_x62<-mim4$estimate[13]
mid_x63<-mim4$estimate[14] 
mid_x64<-mim4$estimate[15] 
mid_x71<-mim4$estimate[16] 
mid_x72<-mim4$estimate[17] 
mid_x73<-mim4$estimate[18] 
mid_x74<-mim4$estimate[19] 
mid_x75<-mim4$estimate[20] 

#EM
#possible value for discrete var
dat0<-subset(data,R_m==1 & R_y==1)
dat1<-subset(data,R_m==0 & R_y==1)
dat2<-subset(data,R_m==1 & R_y==0)
dat3<-subset(data,R_m==0 & R_y==0)
#missing M
dat10<-dat1
dat11<-dat1
dat10$M<-0
dat11$M<-1
#missing M and Y
dat30<-dat3
dat31<-dat3
dat30$M<-0
dat31$M<-1

dat<-rbind(dat0,dat10,dat11,dat2,dat30,dat31)

#initial parameters
emb0_0<-ccb0_0
emb0_m<-ccb0_m
emb0_t<-ccb0_t
emb0_mt<-ccb0_mt
emb0_x1<-ccb0_x1
emb0_x21<-ccb0_x21
emb0_x22<-ccb0_x22
emb0_x31<-ccb0_x31
emb0_x32<-ccb0_x32
emb0_x33<-ccb0_x33
emb0_x4<-ccb0_x4
emb0_x51<-ccb0_x51
emb0_x52<-ccb0_x52
emb0_x62<-ccb0_x62
emb0_x63<-ccb0_x63
emb0_x64<-ccb0_x64
emb0_x71<-ccb0_x71
emb0_x72<-ccb0_x72
emb0_x73<-ccb0_x73
emb0_x74<-ccb0_x74
emb0_x75<-ccb0_x75

emb_0<-ccb_0
emb_m<-ccb_m
emb_t<-ccb_t
emb_mt<-ccb_mt
emb_x1<-ccb_x1
emb_x21<-ccb_x21
emb_x22<-ccb_x22
emb_x31<-ccb_x31
emb_x32<-ccb_x32
emb_x33<-ccb_x33
emb_x4<-ccb_x4
emb_x51<-ccb_x51
emb_x52<-ccb_x52
emb_x62<-ccb_x62
emb_x63<-ccb_x63
emb_x64<-ccb_x64
emb_x71<-ccb_x71
emb_x72<-ccb_x72
emb_x73<-ccb_x73
emb_x74<-ccb_x74
emb_x75<-ccb_x75
em_shape<-cc_shape

ema_0<-cca_0
ema_t<-cca_t
ema_x1<-cca_x1
ema_x21<-cca_x21
ema_x22<-cca_x22
ema_x31<-cca_x31
ema_x32<-cca_x32
ema_x33<-cca_x33
ema_x4<-cca_x4
ema_x51<-cca_x51
ema_x52<-cca_x52
ema_x62<-cca_x62
ema_x63<-cca_x63
ema_x64<-cca_x64
ema_x71<-cca_x71
ema_x72<-cca_x72
ema_x73<-cca_x73
ema_x74<-cca_x74
ema_x75<-cca_x75

emc_0<-mic_0
emc_m<-mic_m
emc_t<-mic_t
emc_x1<-mic_x1
emc_x21<-mic_x21
emc_x22<-mic_x22
emc_x31<-mic_x31
emc_x32<-mic_x32
emc_x33<-mic_x33
emc_x4<-mic_x4
emc_x51<-mic_x51
emc_x52<-mic_x52
emc_x62<-mic_x62
emc_x63<-mic_x63
emc_x64<-mic_x64
emc_x71<-mic_x71
emc_x72<-mic_x72
emc_x73<-mic_x73
emc_x74<-mic_x74
emc_x75<-mic_x75

emd_0<-mid_0
emd_rm<-mid_rm
emd_t<-mid_t
emd_x1<-mid_x1
emd_x21<-mid_x21
emd_x22<-mid_x22
emd_x31<-mid_x31
emd_x32<-mid_x32
emd_x33<-mid_x33
emd_x4<-mid_x4
emd_x51<-mid_x51
emd_x52<-mid_x52
emd_x62<-mid_x62
emd_x63<-mid_x63
emd_x64<-mid_x64
emd_x71<-mid_x71
emd_x72<-mid_x72
emd_x73<-mid_x73
emd_x74<-mid_x74
emd_x75<-mid_x75

#purpose: calculate weight of M using estimated parameters and observed data
#arguments: Y,I_Y,Z,x1,x2,x3,x4,x5,x6,x7: observed data, M: possible value for M
#output: weight of M 
m_weight<-function(Y,I_Y,M,Z,x1,x2,x3,x4,x5,x6,x7){
  
cond_prob_1<-
    ((((em_shape/exp(emb_0+emb_m*1+emb_t*Z+emb_mt*1*Z+emb_x1*I(x1==1)+emb_x21*I(x2==1)+emb_x22*I(x2==2)+emb_x31*I(x3==1)+emb_x32*I(x3==2)+emb_x33*I(x3==3)+emb_x4*I(x4==1)+emb_x51*I(x5==1)+emb_x52*I(x5==2)+emb_x62*I(x6==2)+emb_x63*I(x6==3)+emb_x64*I(x6==4)+emb_x71*I(x7==1)+emb_x72*I(x7==2)+emb_x73*I(x7==3)+emb_x74*I(x7==4)+emb_x75*I(x7==5)))^em_shape)/gamma(em_shape))*
    Y^(em_shape-1)*exp(-(em_shape/exp(emb_0+emb_m*1+emb_t*Z+emb_mt*1*Z+emb_x1*I(x1==1)+emb_x21*I(x2==1)+emb_x22*I(x2==2)+emb_x31*I(x3==1)+emb_x32*I(x3==2)+emb_x33*I(x3==3)+emb_x4*I(x4==1)+emb_x51*I(x5==1)+emb_x52*I(x5==2)+emb_x62*I(x6==2)+emb_x63*I(x6==3)+emb_x64*I(x6==4)+emb_x71*I(x7==1)+emb_x72*I(x7==2)+emb_x73*I(x7==3)+emb_x74*I(x7==4)+emb_x75*I(x7==5)))*Y)*
    (exp(emb0_0+emb0_m*1+emb0_t*Z+emb0_mt*1*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5))/(1+exp(emb0_0+emb0_m*1+emb0_t*Z+emb0_mt*1*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5)))))^I(I_Y==1)*
    (1/(1+exp(emb0_0+emb0_m*1+emb0_t*Z+emb0_mt*1*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5))))^I(I_Y==0)*
    (exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))*
    (1/(1+exp(emc_0+emc_m*1+emc_t*Z+emc_x1*I(x1==1)+emc_x21*I(x2==1)+emc_x22*I(x2==2)+emc_x31*I(x3==1)+emc_x32*I(x3==2)+emc_x33*I(x3==3)+emc_x4*I(x4==1)+emc_x51*I(x5==1)+emc_x52*I(x5==2)+emc_x62*I(x6==2)+emc_x63*I(x6==3)+emc_x64*I(x6==4)+emc_x71*I(x7==1)+emc_x72*I(x7==2)+emc_x73*I(x7==3)+emc_x74*I(x7==4)+emc_x75*I(x7==5))))
  
cond_prob_0<-
    ((((em_shape/exp(emb_0+emb_m*0+emb_t*Z+emb_mt*0*Z+emb_x1*I(x1==1)+emb_x21*I(x2==1)+emb_x22*I(x2==2)+emb_x31*I(x3==1)+emb_x32*I(x3==2)+emb_x33*I(x3==3)+emb_x4*I(x4==1)+emb_x51*I(x5==1)+emb_x52*I(x5==2)+emb_x62*I(x6==2)+emb_x63*I(x6==3)+emb_x64*I(x6==4)+emb_x71*I(x7==1)+emb_x72*I(x7==2)+emb_x73*I(x7==3)+emb_x74*I(x7==4)+emb_x75*I(x7==5)))^em_shape)/gamma(em_shape))*
    Y^(em_shape-1)*exp(-(em_shape/exp(emb_0+emb_m*0+emb_t*Z+emb_mt*0*Z+emb_x1*I(x1==1)+emb_x21*I(x2==1)+emb_x22*I(x2==2)+emb_x31*I(x3==1)+emb_x32*I(x3==2)+emb_x33*I(x3==3)+emb_x4*I(x4==1)+emb_x51*I(x5==1)+emb_x52*I(x5==2)+emb_x62*I(x6==2)+emb_x63*I(x6==3)+emb_x64*I(x6==4)+emb_x71*I(x7==1)+emb_x72*I(x7==2)+emb_x73*I(x7==3)+emb_x74*I(x7==4)+emb_x75*I(x7==5)))*Y)*
    (exp(emb0_0+emb0_m*0+emb0_t*Z+emb0_mt*0*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5))/(1+exp(emb0_0+emb0_m*0+emb0_t*Z+emb0_mt*0*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5)))))^I(I_Y==1)*
    (1/(1+exp(emb0_0+emb0_m*0+emb0_t*Z+emb0_mt*0*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5))))^I(I_Y==0)*
    (1/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))*
    (1/(1+exp(emc_0+emc_m*0+emc_t*Z+emc_x1*I(x1==1)+emc_x21*I(x2==1)+emc_x22*I(x2==2)+emc_x31*I(x3==1)+emc_x32*I(x3==2)+emc_x33*I(x3==3)+emc_x4*I(x4==1)+emc_x51*I(x5==1)+emc_x52*I(x5==2)+emc_x62*I(x6==2)+emc_x63*I(x6==3)+emc_x64*I(x6==4)+emc_x71*I(x7==1)+emc_x72*I(x7==2)+emc_x73*I(x7==3)+emc_x74*I(x7==4)+emc_x75*I(x7==5))))
  
cond_prob<-
    ((((em_shape/exp(emb_0+emb_m*M+emb_t*Z+emb_mt*M*Z+emb_x1*I(x1==1)+emb_x21*I(x2==1)+emb_x22*I(x2==2)+emb_x31*I(x3==1)+emb_x32*I(x3==2)+emb_x33*I(x3==3)+emb_x4*I(x4==1)+emb_x51*I(x5==1)+emb_x52*I(x5==2)+emb_x62*I(x6==2)+emb_x63*I(x6==3)+emb_x64*I(x6==4)+emb_x71*I(x7==1)+emb_x72*I(x7==2)+emb_x73*I(x7==3)+emb_x74*I(x7==4)+emb_x75*I(x7==5)))^em_shape)/gamma(em_shape))*
    Y^(em_shape-1)*exp(-(em_shape/exp(emb_0+emb_m*M+emb_t*Z+emb_mt*M*Z+emb_x1*I(x1==1)+emb_x21*I(x2==1)+emb_x22*I(x2==2)+emb_x31*I(x3==1)+emb_x32*I(x3==2)+emb_x33*I(x3==3)+emb_x4*I(x4==1)+emb_x51*I(x5==1)+emb_x52*I(x5==2)+emb_x62*I(x6==2)+emb_x63*I(x6==3)+emb_x64*I(x6==4)+emb_x71*I(x7==1)+emb_x72*I(x7==2)+emb_x73*I(x7==3)+emb_x74*I(x7==4)+emb_x75*I(x7==5)))*Y)*
    (exp(emb0_0+emb0_m*M+emb0_t*Z+emb0_mt*M*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5))/(1+exp(emb0_0+emb0_m*M+emb0_t*Z+emb0_mt*M*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5)))))^I(I_Y==1)*
    (1/(1+exp(emb0_0+emb0_m*M+emb0_t*Z+emb0_mt*M*Z+emb0_x1*I(x1==1)+emb0_x21*I(x2==1)+emb0_x22*I(x2==2)+emb0_x31*I(x3==1)+emb0_x32*I(x3==2)+emb0_x33*I(x3==3)+emb0_x4*I(x4==1)+emb0_x51*I(x5==1)+emb0_x52*I(x5==2)+emb0_x62*I(x6==2)+emb0_x63*I(x6==3)+emb0_x64*I(x6==4)+emb0_x71*I(x7==1)+emb0_x72*I(x7==2)+emb0_x73*I(x7==3)+emb0_x74*I(x7==4)+emb0_x75*I(x7==5))))^I(I_Y==0)*
    (exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))^I(M==1)*
    (1/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))^I(M==0)*
    (1/(1+exp(emc_0+emc_m*M+emc_t*Z+emc_x1*I(x1==1)+emc_x21*I(x2==1)+emc_x22*I(x2==2)+emc_x31*I(x3==1)+emc_x32*I(x3==2)+emc_x33*I(x3==3)+emc_x4*I(x4==1)+emc_x51*I(x5==1)+emc_x52*I(x5==2)+emc_x62*I(x6==2)+emc_x63*I(x6==3)+emc_x64*I(x6==4)+emc_x71*I(x7==1)+emc_x72*I(x7==2)+emc_x73*I(x7==3)+emc_x74*I(x7==4)+emc_x75*I(x7==5))))
  
  return(cond_prob/(cond_prob_1+cond_prob_0))
}

#purpose: calculate weight of M using estimated parameters and observed data
#arguments: Z,x1,x2,x3,x4,x5,x6,x7: observed data, M: possible value for M
#output: weight of M 
my_weight<-function(M,Z,x1,x2,x3,x4,x5,x6,x7){
  
cond_prob_1<-
    (exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))*
    (1/(1+exp(emc_0+emc_m*1+emc_t*Z+emc_x1*I(x1==1)+emc_x21*I(x2==1)+emc_x22*I(x2==2)+emc_x31*I(x3==1)+emc_x32*I(x3==2)+emc_x33*I(x3==3)+emc_x4*I(x4==1)+emc_x51*I(x5==1)+emc_x52*I(x5==2)+emc_x62*I(x6==2)+emc_x63*I(x6==3)+emc_x64*I(x6==4)+emc_x71*I(x7==1)+emc_x72*I(x7==2)+emc_x73*I(x7==3)+emc_x74*I(x7==4)+emc_x75*I(x7==5))))

cond_prob_0<-
    (1/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))*
    (1/(1+exp(emc_0+emc_m*0+emc_t*Z+emc_x1*I(x1==1)+emc_x21*I(x2==1)+emc_x22*I(x2==2)+emc_x31*I(x3==1)+emc_x32*I(x3==2)+emc_x33*I(x3==3)+emc_x4*I(x4==1)+emc_x51*I(x5==1)+emc_x52*I(x5==2)+emc_x62*I(x6==2)+emc_x63*I(x6==3)+emc_x64*I(x6==4)+emc_x71*I(x7==1)+emc_x72*I(x7==2)+emc_x73*I(x7==3)+emc_x74*I(x7==4)+emc_x75*I(x7==5))))

cond_prob<-
    (exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))^I(M==1)*
    (1/(1+exp(ema_0+ema_t*Z+ema_x1*I(x1==1)+ema_x21*I(x2==1)+ema_x22*I(x2==2)+ema_x31*I(x3==1)+ema_x32*I(x3==2)+ema_x33*I(x3==3)+ema_x4*I(x4==1)+ema_x51*I(x5==1)+ema_x52*I(x5==2)+ema_x62*I(x6==2)+ema_x63*I(x6==3)+ema_x64*I(x6==4)+ema_x71*I(x7==1)+ema_x72*I(x7==2)+ema_x73*I(x7==3)+ema_x74*I(x7==4)+ema_x75*I(x7==5))))^I(M==0)*
    (1/(1+exp(emc_0+emc_m*M+emc_t*Z+emc_x1*I(x1==1)+emc_x21*I(x2==1)+emc_x22*I(x2==2)+emc_x31*I(x3==1)+emc_x32*I(x3==2)+emc_x33*I(x3==3)+emc_x4*I(x4==1)+emc_x51*I(x5==1)+emc_x52*I(x5==2)+emc_x62*I(x6==2)+emc_x63*I(x6==3)+emc_x64*I(x6==4)+emc_x71*I(x7==1)+emc_x72*I(x7==2)+emc_x73*I(x7==3)+emc_x74*I(x7==4)+emc_x75*I(x7==5))))
  
  return(cond_prob/(cond_prob_1+cond_prob_0))
}

Q <- NULL
Q[[1]] <- rep(0,102) 
Q[[2]] <-c(ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
           emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,em_shape,
           emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75,
           emc_0,emc_m,emc_t,emc_x1,emc_x21,emc_x22,emc_x31,emc_x32,emc_x33,emc_x4,emc_x51,emc_x52,emc_x62,emc_x63,emc_x64,emc_x71,emc_x72,emc_x73,emc_x74,emc_x75,
           emd_0,emd_rm,emd_t,emd_x1,emd_x21,emd_x22,emd_x31,emd_x32,emd_x33,emd_x4,emd_x51,emd_x52,emd_x62,emd_x63,emd_x64,emd_x71,emd_x72,emd_x73,emd_x74,emd_x75)

k<-2
#the EM algorithm updates the parameters iteratively until it converges
while (sum(abs(Q[[k]]-Q[[k-1]]))/sum(Q[[k-1]])>=1e-5) {
  
  dat0<-subset(dat, R_m==1 & R_y==1)
  dat0$wt<-1
  
  #missing M
  dat1<-subset(dat, R_m==0 & R_y==1)
  dat1$wt<-m_weight(dat1$Y,dat1$I_Y,dat1$M,dat1$Z,dat1$x1,dat1$x2,dat1$x3,dat1$x4,dat1$x5,dat1$x6,dat1$x7)
  
  #missing Y
  dat2<-subset(dat, R_m==1 & R_y==0)
  dat2$wt<-1

  #missing M and Y
  dat3<-subset(dat, R_m==0 & R_y==0)
  dat3$wt<-my_weight(dat3$M,dat3$Z,dat3$x1,dat3$x2,dat3$x3,dat3$x4,dat3$x5,dat3$x6,dat3$x7)
  
  dat<-rbind(dat0,dat1,dat2,dat3)
  
  #update parameters
  #I(Y>0) model can be identified by complete cases under MNAR assumption 2,3 and 5
  emm11<-glm(I_Y~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family = binomial(link='logit'),weights=wt,data=dat,subset=(R_y==1 & R_m==1))
  #conditional on I(Y>0) and M, Y model can be identified by complete cases
  emm1<-glm(Y~M+Z+M*Z+x1+x2+x3+x4+x5+x6+x7,family = Gamma(link = "log"),weights=wt,data=dat,subset=(I_Y==1 & R_y==1 & R_m==1))
  emm2<-glm(M~Z+x1+x2+x3+x4+x5+x6+x7,family = binomial(link='logit'),weights=wt,data=dat)
  emm3<-glm(R_m~M+Z+x1+x2+x3+x4+x5+x6+x7,family = binomial(link='logit'),weights=wt,data=dat)
  emm4<-glm(R_y~R_m+Z+x1+x2+x3+x4+x5+x6+x7,family = binomial(link='logit'),weights=wt,data=dat)
  
  emb0_0<-emm11$coef[1]
  emb0_m<-emm11$coef[2]
  emb0_t<-emm11$coef[3]
  emb0_x1<-emm11$coef[4]
  emb0_x21<-emm11$coef[5]
  emb0_x22<-emm11$coef[6]
  emb0_x31<-emm11$coef[7]
  emb0_x32<-emm11$coef[8]
  emb0_x33<-emm11$coef[9]
  emb0_x4<-emm11$coef[10]
  emb0_x51<-emm11$coef[11]
  emb0_x52<-emm11$coef[12]
  emb0_x62<-emm11$coef[13]
  emb0_x63<-emm11$coef[14]
  emb0_x64<-emm11$coef[15]
  emb0_x71<-emm11$coef[16]
  emb0_x72<-emm11$coef[17]
  emb0_x73<-emm11$coef[18]
  emb0_x74<-emm11$coef[19]
  emb0_x75<-emm11$coef[20]
  emb0_mt<-emm11$coef[21]
  
  emb_0<-emm1$coef[1]
  emb_m<-emm1$coef[2]
  emb_t<-emm1$coef[3]
  emb_x1<-emm1$coef[4]
  emb_x21<-emm1$coef[5]
  emb_x22<-emm1$coef[6]
  emb_x31<-emm1$coef[7]
  emb_x32<-emm1$coef[8]
  emb_x33<-emm1$coef[9]
  emb_x4<-emm1$coef[10]
  emb_x51<-emm1$coef[11]
  emb_x52<-emm1$coef[12]
  emb_x62<-emm1$coef[13]
  emb_x63<-emm1$coef[14]
  emb_x64<-emm1$coef[15]
  emb_x71<-emm1$coef[16]
  emb_x72<-emm1$coef[17]
  emb_x73<-emm1$coef[18]
  emb_x74<-emm1$coef[19]
  emb_x75<-emm1$coef[20]
  emb_mt<-emm1$coef[21]
  em_shape<-as.numeric(gamma.shape(emm1)[1])
  
  ema_0<-emm2$coef[1]
  ema_t<-emm2$coef[2]
  ema_x1<-emm2$coef[3]
  ema_x21<-emm2$coef[4]
  ema_x22<-emm2$coef[5]
  ema_x31<-emm2$coef[6]
  ema_x32<-emm2$coef[7]
  ema_x33<-emm2$coef[8]
  ema_x4<-emm2$coef[9]
  ema_x51<-emm2$coef[10]
  ema_x52<-emm2$coef[11]
  ema_x62<-emm2$coef[12]
  ema_x63<-emm2$coef[13]
  ema_x64<-emm2$coef[14]
  ema_x71<-emm2$coef[15]
  ema_x72<-emm2$coef[16]
  ema_x73<-emm2$coef[17]
  ema_x74<-emm2$coef[18]
  ema_x75<-emm2$coef[19]
  
  emc_0<-emm3$coef[1]
  emc_m<-emm3$coef[2]
  emc_t<-emm3$coef[3]
  emc_x1<-emm3$coef[4]
  emc_x21<-emm3$coef[5]
  emc_x22<-emm3$coef[6]
  emc_x31<-emm3$coef[7]
  emc_x32<-emm3$coef[8]
  emc_x33<-emm3$coef[9]
  emc_x4<-emm3$coef[10]
  emc_x51<-emm3$coef[11]
  emc_x52<-emm3$coef[12]
  emc_x62<-emm3$coef[13]
  emc_x63<-emm3$coef[14]
  emc_x64<-emm3$coef[15]
  emc_x71<-emm3$coef[16]
  emc_x72<-emm3$coef[17]
  emc_x73<-emm3$coef[18]
  emc_x74<-emm3$coef[19]
  emc_x75<-emm3$coef[20]
  
  emd_0<-emm4$coef[1]
  emd_rm<-emm4$coef[2]
  emd_t<-emm4$coef[3]
  emd_x1<-emm4$coef[4]
  emd_x21<-emm4$coef[5]
  emd_x22<-emm4$coef[6]
  emd_x31<-emm4$coef[7]
  emd_x32<-emm4$coef[8]
  emd_x33<-emm4$coef[9]
  emd_x4<-emm4$coef[10]
  emd_x51<-emm4$coef[11]
  emd_x52<-emm4$coef[12]
  emd_x62<-emm4$coef[13]
  emd_x63<-emm4$coef[14]
  emd_x64<-emm4$coef[15]
  emd_x71<-emm4$coef[16]
  emd_x72<-emm4$coef[17]
  emd_x73<-emm4$coef[18]
  emd_x74<-emm4$coef[19]
  emd_x75<-emm4$coef[20]
  
  k <- k + 1
  
  Q[[k]]<-c(ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
            emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,em_shape,
            emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75,
            emc_0,emc_m,emc_t,emc_x1,emc_x21,emc_x22,emc_x31,emc_x32,emc_x33,emc_x4,emc_x51,emc_x52,emc_x62,emc_x63,emc_x64,emc_x71,emc_x72,emc_x73,emc_x74,emc_x75,
            emd_0,emd_rm,emd_t,emd_x1,emd_x21,emd_x22,emd_x31,emd_x32,emd_x33,emd_x4,emd_x51,emd_x52,emd_x62,emd_x63,emd_x64,emd_x71,emd_x72,emd_x73,emd_x74,emd_x75)

}
#calculate direct and indirect effects using the parameters estimated through complete case analysis
ccTIE<-int11(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)-
       int10(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)

ccPDE<-int10(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)-
       int00(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)

ccPIE<-int01(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)-
       int00(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)

ccTDE<-int11(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)-
       int01(data,
             cca_0,cca_t,cca_x1,cca_x21,cca_x22,cca_x31,cca_x32,cca_x33,cca_x4,cca_x51,cca_x52,cca_x62,cca_x63,cca_x64,cca_x71,cca_x72,cca_x73,cca_x74,cca_x75,
             ccb_0,ccb_m,ccb_t,ccb_mt,ccb_x1,ccb_x21,ccb_x22,ccb_x31,ccb_x32,ccb_x33,ccb_x4,ccb_x51,ccb_x52,ccb_x62,ccb_x63,ccb_x64,ccb_x71,ccb_x72,ccb_x73,ccb_x74,ccb_x75,
             ccb0_0,ccb0_m,ccb0_t,ccb0_mt,ccb0_x1,ccb0_x21,ccb0_x22,ccb0_x31,ccb0_x32,ccb0_x33,ccb0_x4,ccb0_x51,ccb0_x52,ccb0_x62,ccb0_x63,ccb0_x64,ccb0_x71,ccb0_x72,ccb0_x73,ccb0_x74,ccb0_x75)

#calculate direct and indirect effects using the parameters estimated through multiple imputation
miTIE<-int11(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)-
       int10(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)

miPDE<-int10(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)-
       int00(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)

miPIE<-int01(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)-
       int00(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)

miTDE<-int11(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)-
       int01(data,
             mia_0,mia_t,mia_x1,mia_x21,mia_x22,mia_x31,mia_x32,mia_x33,mia_x4,mia_x51,mia_x52,mia_x62,mia_x63,mia_x64,mia_x71,mia_x72,mia_x73,mia_x74,mia_x75,
             mib_0,mib_m,mib_t,mib_mt,mib_x1,mib_x21,mib_x22,mib_x31,mib_x32,mib_x33,mib_x4,mib_x51,mib_x52,mib_x62,mib_x63,mib_x64,mib_x71,mib_x72,mib_x73,mib_x74,mib_x75,
             mib0_0,mib0_m,mib0_t,mib0_mt,mib0_x1,mib0_x21,mib0_x22,mib0_x31,mib0_x32,mib0_x33,mib0_x4,mib0_x51,mib0_x52,mib0_x62,mib0_x63,mib0_x64,mib0_x71,mib0_x72,mib0_x73,mib0_x74,mib0_x75)

#calculate direct and indirect effects using the parameters estimated through the EM algorithm
emTIE<-int11(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)-
       int10(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)

emPDE<-int10(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)-
       int00(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)

emPIE<-int01(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)-
       int00(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)

emTDE<-int11(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)-
       int01(data,
             ema_0,ema_t,ema_x1,ema_x21,ema_x22,ema_x31,ema_x32,ema_x33,ema_x4,ema_x51,ema_x52,ema_x62,ema_x63,ema_x64,ema_x71,ema_x72,ema_x73,ema_x74,ema_x75,
             emb_0,emb_m,emb_t,emb_mt,emb_x1,emb_x21,emb_x22,emb_x31,emb_x32,emb_x33,emb_x4,emb_x51,emb_x52,emb_x62,emb_x63,emb_x64,emb_x71,emb_x72,emb_x73,emb_x74,emb_x75,
             emb0_0,emb0_m,emb0_t,emb0_mt,emb0_x1,emb0_x21,emb0_x22,emb0_x31,emb0_x32,emb0_x33,emb0_x4,emb0_x51,emb0_x52,emb0_x62,emb0_x63,emb0_x64,emb0_x71,emb0_x72,emb0_x73,emb0_x74,emb0_x75)

#calculate observed log-likelihood using the parameters estimated through the EM algorithm

#both m and y observed
l0<-((((em_shape/exp(emb_0+emb_m*dat0$M+emb_t*dat0$Z+emb_mt*dat0$M*dat0$Z+emb_x1*I(dat0$x1==1)+emb_x21*I(dat0$x2==1)+emb_x22*I(dat0$x2==2)+emb_x31*I(dat0$x3==1)+emb_x32*I(dat0$x3==2)+emb_x33*I(dat0$x3==3)+emb_x4*I(dat0$x4==1)+emb_x51*I(dat0$x5==1)+emb_x52*I(dat0$x5==2)+emb_x62*I(dat0$x6==2)+emb_x63*I(dat0$x6==3)+emb_x64*I(dat0$x6==4)+emb_x71*I(dat0$x7==1)+emb_x72*I(dat0$x7==2)+emb_x73*I(dat0$x7==3)+emb_x74*I(dat0$x7==4)+emb_x75*I(dat0$x7==5)))^em_shape)/gamma(em_shape))*
    dat0$Y^(em_shape-1)*exp(-(em_shape/exp(emb_0+emb_m*dat0$M+emb_t*dat0$Z+emb_mt*dat0$M*dat0$Z+emb_x1*I(dat0$x1==1)+emb_x21*I(dat0$x2==1)+emb_x22*I(dat0$x2==2)+emb_x31*I(dat0$x3==1)+emb_x32*I(dat0$x3==2)+emb_x33*I(dat0$x3==3)+emb_x4*I(dat0$x4==1)+emb_x51*I(dat0$x5==1)+emb_x52*I(dat0$x5==2)+emb_x62*I(dat0$x6==2)+emb_x63*I(dat0$x6==3)+emb_x64*I(dat0$x6==4)+emb_x71*I(dat0$x7==1)+emb_x72*I(dat0$x7==2)+emb_x73*I(dat0$x7==3)+emb_x74*I(dat0$x7==4)+emb_x75*I(dat0$x7==5)))*dat0$Y)*
    (exp(emb0_0+emb0_m*dat0$M+emb0_t*dat0$Z+emb0_mt*dat0$M*dat0$Z+emb0_x1*I(dat0$x1==1)+emb0_x21*I(dat0$x2==1)+emb0_x22*I(dat0$x2==2)+emb0_x31*I(dat0$x3==1)+emb0_x32*I(dat0$x3==2)+emb0_x33*I(dat0$x3==3)+emb0_x4*I(dat0$x4==1)+emb0_x51*I(dat0$x5==1)+emb0_x52*I(dat0$x5==2)+emb0_x62*I(dat0$x6==2)+emb0_x63*I(dat0$x6==3)+emb0_x64*I(dat0$x6==4)+emb0_x71*I(dat0$x7==1)+emb0_x72*I(dat0$x7==2)+emb0_x73*I(dat0$x7==3)+emb0_x74*I(dat0$x7==4)+emb0_x75*I(dat0$x7==5))/(1+exp(emb0_0+emb0_m*dat0$M+emb0_t*dat0$Z+emb0_mt*dat0$M*dat0$Z+emb0_x1*I(dat0$x1==1)+emb0_x21*I(dat0$x2==1)+emb0_x22*I(dat0$x2==2)+emb0_x31*I(dat0$x3==1)+emb0_x32*I(dat0$x3==2)+emb0_x33*I(dat0$x3==3)+emb0_x4*I(dat0$x4==1)+emb0_x51*I(dat0$x5==1)+emb0_x52*I(dat0$x5==2)+emb0_x62*I(dat0$x6==2)+emb0_x63*I(dat0$x6==3)+emb0_x64*I(dat0$x6==4)+emb0_x71*I(dat0$x7==1)+emb0_x72*I(dat0$x7==2)+emb0_x73*I(dat0$x7==3)+emb0_x74*I(dat0$x7==4)+emb0_x75*I(dat0$x7==5)))))^I(dat0$I_Y==1)*
    (1/(1+exp(emb0_0+emb0_m*dat0$M+emb0_t*dat0$Z+emb0_mt*dat0$M*dat0$Z+emb0_x1*I(dat0$x1==1)+emb0_x21*I(dat0$x2==1)+emb0_x22*I(dat0$x2==2)+emb0_x31*I(dat0$x3==1)+emb0_x32*I(dat0$x3==2)+emb0_x33*I(dat0$x3==3)+emb0_x4*I(dat0$x4==1)+emb0_x51*I(dat0$x5==1)+emb0_x52*I(dat0$x5==2)+emb0_x62*I(dat0$x6==2)+emb0_x63*I(dat0$x6==3)+emb0_x64*I(dat0$x6==4)+emb0_x71*I(dat0$x7==1)+emb0_x72*I(dat0$x7==2)+emb0_x73*I(dat0$x7==3)+emb0_x74*I(dat0$x7==4)+emb0_x75*I(dat0$x7==5))))^I(dat0$I_Y==0)*
    (exp(ema_0+ema_t*dat0$Z+ema_x1*I(dat0$x1==1)+ema_x21*I(dat0$x2==1)+ema_x22*I(dat0$x2==2)+ema_x31*I(dat0$x3==1)+ema_x32*I(dat0$x3==2)+ema_x33*I(dat0$x3==3)+ema_x4*I(dat0$x4==1)+ema_x51*I(dat0$x5==1)+ema_x52*I(dat0$x5==2)+ema_x62*I(dat0$x6==2)+ema_x63*I(dat0$x6==3)+ema_x64*I(dat0$x6==4)+ema_x71*I(dat0$x7==1)+ema_x72*I(dat0$x7==2)+ema_x73*I(dat0$x7==3)+ema_x74*I(dat0$x7==4)+ema_x75*I(dat0$x7==5))/(1+exp(ema_0+ema_t*dat0$Z+ema_x1*I(dat0$x1==1)+ema_x21*I(dat0$x2==1)+ema_x22*I(dat0$x2==2)+ema_x31*I(dat0$x3==1)+ema_x32*I(dat0$x3==2)+ema_x33*I(dat0$x3==3)+ema_x4*I(dat0$x4==1)+ema_x51*I(dat0$x5==1)+ema_x52*I(dat0$x5==2)+ema_x62*I(dat0$x6==2)+ema_x63*I(dat0$x6==3)+ema_x64*I(dat0$x6==4)+ema_x71*I(dat0$x7==1)+ema_x72*I(dat0$x7==2)+ema_x73*I(dat0$x7==3)+ema_x74*I(dat0$x7==4)+ema_x75*I(dat0$x7==5))))^I(dat0$M==1)*
    (1/(1+exp(ema_0+ema_t*dat0$Z+ema_x1*I(dat0$x1==1)+ema_x21*I(dat0$x2==1)+ema_x22*I(dat0$x2==2)+ema_x31*I(dat0$x3==1)+ema_x32*I(dat0$x3==2)+ema_x33*I(dat0$x3==3)+ema_x4*I(dat0$x4==1)+ema_x51*I(dat0$x5==1)+ema_x52*I(dat0$x5==2)+ema_x62*I(dat0$x6==2)+ema_x63*I(dat0$x6==3)+ema_x64*I(dat0$x6==4)+ema_x71*I(dat0$x7==1)+ema_x72*I(dat0$x7==2)+ema_x73*I(dat0$x7==3)+ema_x74*I(dat0$x7==4)+ema_x75*I(dat0$x7==5))))^I(dat0$M==0)*
    (exp(emc_0+emc_m*dat0$M+emc_t*dat0$Z+emc_x1*I(dat0$x1==1)+emc_x21*I(dat0$x2==1)+emc_x22*I(dat0$x2==2)+emc_x31*I(dat0$x3==1)+emc_x32*I(dat0$x3==2)+emc_x33*I(dat0$x3==3)+emc_x4*I(dat0$x4==1)+emc_x51*I(dat0$x5==1)+emc_x52*I(dat0$x5==2)+emc_x62*I(dat0$x6==2)+emc_x63*I(dat0$x6==3)+emc_x64*I(dat0$x6==4)+emc_x71*I(dat0$x7==1)+emc_x72*I(dat0$x7==2)+emc_x73*I(dat0$x7==3)+emc_x74*I(dat0$x7==4)+emc_x75*I(dat0$x7==5))/(1+exp(emc_0+emc_m*dat0$M+emc_t*dat0$Z+emc_x1*I(dat0$x1==1)+emc_x21*I(dat0$x2==1)+emc_x22*I(dat0$x2==2)+emc_x31*I(dat0$x3==1)+emc_x32*I(dat0$x3==2)+emc_x33*I(dat0$x3==3)+emc_x4*I(dat0$x4==1)+emc_x51*I(dat0$x5==1)+emc_x52*I(dat0$x5==2)+emc_x62*I(dat0$x6==2)+emc_x63*I(dat0$x6==3)+emc_x64*I(dat0$x6==4)+emc_x71*I(dat0$x7==1)+emc_x72*I(dat0$x7==2)+emc_x73*I(dat0$x7==3)+emc_x74*I(dat0$x7==4)+emc_x75*I(dat0$x7==5))))*
    (exp(emd_0+emd_rm*1+emd_t*dat0$Z+emd_x1*I(dat0$x1==1)+emd_x21*I(dat0$x2==1)+emd_x22*I(dat0$x2==2)+emd_x31*I(dat0$x3==1)+emd_x32*I(dat0$x3==2)+emd_x33*I(dat0$x3==3)+emd_x4*I(dat0$x4==1)+emd_x51*I(dat0$x5==1)+emd_x52*I(dat0$x5==2)+emd_x62*I(dat0$x6==2)+emd_x63*I(dat0$x6==3)+emd_x64*I(dat0$x6==4)+emd_x71*I(dat0$x7==1)+emd_x72*I(dat0$x7==2)+emd_x73*I(dat0$x7==3)+emd_x74*I(dat0$x7==4)+emd_x75*I(dat0$x7==5))/(1+exp(emd_0+emd_rm*1+emd_t*dat0$Z+emd_x1*I(dat0$x1==1)+emd_x21*I(dat0$x2==1)+emd_x22*I(dat0$x2==2)+emd_x31*I(dat0$x3==1)+emd_x32*I(dat0$x3==2)+emd_x33*I(dat0$x3==3)+emd_x4*I(dat0$x4==1)+emd_x51*I(dat0$x5==1)+emd_x52*I(dat0$x5==2)+emd_x62*I(dat0$x6==2)+emd_x63*I(dat0$x6==3)+emd_x64*I(dat0$x6==4)+emd_x71*I(dat0$x7==1)+emd_x72*I(dat0$x7==2)+emd_x73*I(dat0$x7==3)+emd_x74*I(dat0$x7==4)+emd_x75*I(dat0$x7==5))))
#missing m (m=0)
l10<-((((em_shape/exp(emb_0+emb_m*dat10$M+emb_t*dat10$Z+emb_mt*dat10$M*dat10$Z+emb_x1*I(dat10$x1==1)+emb_x21*I(dat10$x2==1)+emb_x22*I(dat10$x2==2)+emb_x31*I(dat10$x3==1)+emb_x32*I(dat10$x3==2)+emb_x33*I(dat10$x3==3)+emb_x4*I(dat10$x4==1)+emb_x51*I(dat10$x5==1)+emb_x52*I(dat10$x5==2)+emb_x62*I(dat10$x6==2)+emb_x63*I(dat10$x6==3)+emb_x64*I(dat10$x6==4)+emb_x71*I(dat10$x7==1)+emb_x72*I(dat10$x7==2)+emb_x73*I(dat10$x7==3)+emb_x74*I(dat10$x7==4)+emb_x75*I(dat10$x7==5)))^em_shape)/gamma(em_shape))*
     dat10$Y^(em_shape-1)*exp(-(em_shape/exp(emb_0+emb_m*dat10$M+emb_t*dat10$Z+emb_mt*dat10$M*dat10$Z+emb_x1*I(dat10$x1==1)+emb_x21*I(dat10$x2==1)+emb_x22*I(dat10$x2==2)+emb_x31*I(dat10$x3==1)+emb_x32*I(dat10$x3==2)+emb_x33*I(dat10$x3==3)+emb_x4*I(dat10$x4==1)+emb_x51*I(dat10$x5==1)+emb_x52*I(dat10$x5==2)+emb_x62*I(dat10$x6==2)+emb_x63*I(dat10$x6==3)+emb_x64*I(dat10$x6==4)+emb_x71*I(dat10$x7==1)+emb_x72*I(dat10$x7==2)+emb_x73*I(dat10$x7==3)+emb_x74*I(dat10$x7==4)+emb_x75*I(dat10$x7==5)))*dat10$Y)*
     (exp(emb0_0+emb0_m*dat10$M+emb0_t*dat10$Z+emb0_mt*dat10$M*dat10$Z+emb0_x1*I(dat10$x1==1)+emb0_x21*I(dat10$x2==1)+emb0_x22*I(dat10$x2==2)+emb0_x31*I(dat10$x3==1)+emb0_x32*I(dat10$x3==2)+emb0_x33*I(dat10$x3==3)+emb0_x4*I(dat10$x4==1)+emb0_x51*I(dat10$x5==1)+emb0_x52*I(dat10$x5==2)+emb0_x62*I(dat10$x6==2)+emb0_x63*I(dat10$x6==3)+emb0_x64*I(dat10$x6==4)+emb0_x71*I(dat10$x7==1)+emb0_x72*I(dat10$x7==2)+emb0_x73*I(dat10$x7==3)+emb0_x74*I(dat10$x7==4)+emb0_x75*I(dat10$x7==5))/(1+exp(emb0_0+emb0_m*dat10$M+emb0_t*dat10$Z+emb0_mt*dat10$M*dat10$Z+emb0_x1*I(dat10$x1==1)+emb0_x21*I(dat10$x2==1)+emb0_x22*I(dat10$x2==2)+emb0_x31*I(dat10$x3==1)+emb0_x32*I(dat10$x3==2)+emb0_x33*I(dat10$x3==3)+emb0_x4*I(dat10$x4==1)+emb0_x51*I(dat10$x5==1)+emb0_x52*I(dat10$x5==2)+emb0_x62*I(dat10$x6==2)+emb0_x63*I(dat10$x6==3)+emb0_x64*I(dat10$x6==4)+emb0_x71*I(dat10$x7==1)+emb0_x72*I(dat10$x7==2)+emb0_x73*I(dat10$x7==3)+emb0_x74*I(dat10$x7==4)+emb0_x75*I(dat10$x7==5)))))^I(dat10$I_Y==1)*
     (1/(1+exp(emb0_0+emb0_m*dat10$M+emb0_t*dat10$Z+emb0_mt*dat10$M*dat10$Z+emb0_x1*I(dat10$x1==1)+emb0_x21*I(dat10$x2==1)+emb0_x22*I(dat10$x2==2)+emb0_x31*I(dat10$x3==1)+emb0_x32*I(dat10$x3==2)+emb0_x33*I(dat10$x3==3)+emb0_x4*I(dat10$x4==1)+emb0_x51*I(dat10$x5==1)+emb0_x52*I(dat10$x5==2)+emb0_x62*I(dat10$x6==2)+emb0_x63*I(dat10$x6==3)+emb0_x64*I(dat10$x6==4)+emb0_x71*I(dat10$x7==1)+emb0_x72*I(dat10$x7==2)+emb0_x73*I(dat10$x7==3)+emb0_x74*I(dat10$x7==4)+emb0_x75*I(dat10$x7==5))))^I(dat10$I_Y==0)*
     (exp(ema_0+ema_t*dat10$Z+ema_x1*I(dat10$x1==1)+ema_x21*I(dat10$x2==1)+ema_x22*I(dat10$x2==2)+ema_x31*I(dat10$x3==1)+ema_x32*I(dat10$x3==2)+ema_x33*I(dat10$x3==3)+ema_x4*I(dat10$x4==1)+ema_x51*I(dat10$x5==1)+ema_x52*I(dat10$x5==2)+ema_x62*I(dat10$x6==2)+ema_x63*I(dat10$x6==3)+ema_x64*I(dat10$x6==4)+ema_x71*I(dat10$x7==1)+ema_x72*I(dat10$x7==2)+ema_x73*I(dat10$x7==3)+ema_x74*I(dat10$x7==4)+ema_x75*I(dat10$x7==5))/(1+exp(ema_0+ema_t*dat10$Z+ema_x1*I(dat10$x1==1)+ema_x21*I(dat10$x2==1)+ema_x22*I(dat10$x2==2)+ema_x31*I(dat10$x3==1)+ema_x32*I(dat10$x3==2)+ema_x33*I(dat10$x3==3)+ema_x4*I(dat10$x4==1)+ema_x51*I(dat10$x5==1)+ema_x52*I(dat10$x5==2)+ema_x62*I(dat10$x6==2)+ema_x63*I(dat10$x6==3)+ema_x64*I(dat10$x6==4)+ema_x71*I(dat10$x7==1)+ema_x72*I(dat10$x7==2)+ema_x73*I(dat10$x7==3)+ema_x74*I(dat10$x7==4)+ema_x75*I(dat10$x7==5))))^I(dat10$M==1)*
     (1/(1+exp(ema_0+ema_t*dat10$Z+ema_x1*I(dat10$x1==1)+ema_x21*I(dat10$x2==1)+ema_x22*I(dat10$x2==2)+ema_x31*I(dat10$x3==1)+ema_x32*I(dat10$x3==2)+ema_x33*I(dat10$x3==3)+ema_x4*I(dat10$x4==1)+ema_x51*I(dat10$x5==1)+ema_x52*I(dat10$x5==2)+ema_x62*I(dat10$x6==2)+ema_x63*I(dat10$x6==3)+ema_x64*I(dat10$x6==4)+ema_x71*I(dat10$x7==1)+ema_x72*I(dat10$x7==2)+ema_x73*I(dat10$x7==3)+ema_x74*I(dat10$x7==4)+ema_x75*I(dat10$x7==5))))^I(dat10$M==0)*
     (1/(1+exp(emc_0+emc_m*dat10$M+emc_t*dat10$Z+emc_x1*I(dat10$x1==1)+emc_x21*I(dat10$x2==1)+emc_x22*I(dat10$x2==2)+emc_x31*I(dat10$x3==1)+emc_x32*I(dat10$x3==2)+emc_x33*I(dat10$x3==3)+emc_x4*I(dat10$x4==1)+emc_x51*I(dat10$x5==1)+emc_x52*I(dat10$x5==2)+emc_x62*I(dat10$x6==2)+emc_x63*I(dat10$x6==3)+emc_x64*I(dat10$x6==4)+emc_x71*I(dat10$x7==1)+emc_x72*I(dat10$x7==2)+emc_x73*I(dat10$x7==3)+emc_x74*I(dat10$x7==4)+emc_x75*I(dat10$x7==5))))*
     (exp(emd_0+emd_rm*0+emd_t*dat10$Z+emd_x1*I(dat10$x1==1)+emd_x21*I(dat10$x2==1)+emd_x22*I(dat10$x2==2)+emd_x31*I(dat10$x3==1)+emd_x32*I(dat10$x3==2)+emd_x33*I(dat10$x3==3)+emd_x4*I(dat10$x4==1)+emd_x51*I(dat10$x5==1)+emd_x52*I(dat10$x5==2)+emd_x62*I(dat10$x6==2)+emd_x63*I(dat10$x6==3)+emd_x64*I(dat10$x6==4)+emd_x71*I(dat10$x7==1)+emd_x72*I(dat10$x7==2)+emd_x73*I(dat10$x7==3)+emd_x74*I(dat10$x7==4)+emd_x75*I(dat10$x7==5))/(1+exp(emd_0+emd_rm*0+emd_t*dat10$Z+emd_x1*I(dat10$x1==1)+emd_x21*I(dat10$x2==1)+emd_x22*I(dat10$x2==2)+emd_x31*I(dat10$x3==1)+emd_x32*I(dat10$x3==2)+emd_x33*I(dat10$x3==3)+emd_x4*I(dat10$x4==1)+emd_x51*I(dat10$x5==1)+emd_x52*I(dat10$x5==2)+emd_x62*I(dat10$x6==2)+emd_x63*I(dat10$x6==3)+emd_x64*I(dat10$x6==4)+emd_x71*I(dat10$x7==1)+emd_x72*I(dat10$x7==2)+emd_x73*I(dat10$x7==3)+emd_x74*I(dat10$x7==4)+emd_x75*I(dat10$x7==5))))
#missing m (m=1)
l11<-((((em_shape/exp(emb_0+emb_m*dat11$M+emb_t*dat11$Z+emb_mt*dat11$M*dat11$Z+emb_x1*I(dat11$x1==1)+emb_x21*I(dat11$x2==1)+emb_x22*I(dat11$x2==2)+emb_x31*I(dat11$x3==1)+emb_x32*I(dat11$x3==2)+emb_x33*I(dat11$x3==3)+emb_x4*I(dat11$x4==1)+emb_x51*I(dat11$x5==1)+emb_x52*I(dat11$x5==2)+emb_x62*I(dat11$x6==2)+emb_x63*I(dat11$x6==3)+emb_x64*I(dat11$x6==4)+emb_x71*I(dat11$x7==1)+emb_x72*I(dat11$x7==2)+emb_x73*I(dat11$x7==3)+emb_x74*I(dat11$x7==4)+emb_x75*I(dat11$x7==5)))^em_shape)/gamma(em_shape))*
     dat11$Y^(em_shape-1)*exp(-(em_shape/exp(emb_0+emb_m*dat11$M+emb_t*dat11$Z+emb_mt*dat11$M*dat11$Z+emb_x1*I(dat11$x1==1)+emb_x21*I(dat11$x2==1)+emb_x22*I(dat11$x2==2)+emb_x31*I(dat11$x3==1)+emb_x32*I(dat11$x3==2)+emb_x33*I(dat11$x3==3)+emb_x4*I(dat11$x4==1)+emb_x51*I(dat11$x5==1)+emb_x52*I(dat11$x5==2)+emb_x62*I(dat11$x6==2)+emb_x63*I(dat11$x6==3)+emb_x64*I(dat11$x6==4)+emb_x71*I(dat11$x7==1)+emb_x72*I(dat11$x7==2)+emb_x73*I(dat11$x7==3)+emb_x74*I(dat11$x7==4)+emb_x75*I(dat11$x7==5)))*dat11$Y)*
     (exp(emb0_0+emb0_m*dat11$M+emb0_t*dat11$Z+emb0_mt*dat11$M*dat11$Z+emb0_x1*I(dat11$x1==1)+emb0_x21*I(dat11$x2==1)+emb0_x22*I(dat11$x2==2)+emb0_x31*I(dat11$x3==1)+emb0_x32*I(dat11$x3==2)+emb0_x33*I(dat11$x3==3)+emb0_x4*I(dat11$x4==1)+emb0_x51*I(dat11$x5==1)+emb0_x52*I(dat11$x5==2)+emb0_x62*I(dat11$x6==2)+emb0_x63*I(dat11$x6==3)+emb0_x64*I(dat11$x6==4)+emb0_x71*I(dat11$x7==1)+emb0_x72*I(dat11$x7==2)+emb0_x73*I(dat11$x7==3)+emb0_x74*I(dat11$x7==4)+emb0_x75*I(dat11$x7==5))/(1+exp(emb0_0+emb0_m*dat11$M+emb0_t*dat11$Z+emb0_mt*dat11$M*dat11$Z+emb0_x1*I(dat11$x1==1)+emb0_x21*I(dat11$x2==1)+emb0_x22*I(dat11$x2==2)+emb0_x31*I(dat11$x3==1)+emb0_x32*I(dat11$x3==2)+emb0_x33*I(dat11$x3==3)+emb0_x4*I(dat11$x4==1)+emb0_x51*I(dat11$x5==1)+emb0_x52*I(dat11$x5==2)+emb0_x62*I(dat11$x6==2)+emb0_x63*I(dat11$x6==3)+emb0_x64*I(dat11$x6==4)+emb0_x71*I(dat11$x7==1)+emb0_x72*I(dat11$x7==2)+emb0_x73*I(dat11$x7==3)+emb0_x74*I(dat11$x7==4)+emb0_x75*I(dat11$x7==5)))))^I(dat11$I_Y==1)*
     (1/(1+exp(emb0_0+emb0_m*dat11$M+emb0_t*dat11$Z+emb0_mt*dat11$M*dat11$Z+emb0_x1*I(dat11$x1==1)+emb0_x21*I(dat11$x2==1)+emb0_x22*I(dat11$x2==2)+emb0_x31*I(dat11$x3==1)+emb0_x32*I(dat11$x3==2)+emb0_x33*I(dat11$x3==3)+emb0_x4*I(dat11$x4==1)+emb0_x51*I(dat11$x5==1)+emb0_x52*I(dat11$x5==2)+emb0_x62*I(dat11$x6==2)+emb0_x63*I(dat11$x6==3)+emb0_x64*I(dat11$x6==4)+emb0_x71*I(dat11$x7==1)+emb0_x72*I(dat11$x7==2)+emb0_x73*I(dat11$x7==3)+emb0_x74*I(dat11$x7==4)+emb0_x75*I(dat11$x7==5))))^I(dat11$I_Y==0)*
     (exp(ema_0+ema_t*dat11$Z+ema_x1*I(dat11$x1==1)+ema_x21*I(dat11$x2==1)+ema_x22*I(dat11$x2==2)+ema_x31*I(dat11$x3==1)+ema_x32*I(dat11$x3==2)+ema_x33*I(dat11$x3==3)+ema_x4*I(dat11$x4==1)+ema_x51*I(dat11$x5==1)+ema_x52*I(dat11$x5==2)+ema_x62*I(dat11$x6==2)+ema_x63*I(dat11$x6==3)+ema_x64*I(dat11$x6==4)+ema_x71*I(dat11$x7==1)+ema_x72*I(dat11$x7==2)+ema_x73*I(dat11$x7==3)+ema_x74*I(dat11$x7==4)+ema_x75*I(dat11$x7==5))/(1+exp(ema_0+ema_t*dat11$Z+ema_x1*I(dat11$x1==1)+ema_x21*I(dat11$x2==1)+ema_x22*I(dat11$x2==2)+ema_x31*I(dat11$x3==1)+ema_x32*I(dat11$x3==2)+ema_x33*I(dat11$x3==3)+ema_x4*I(dat11$x4==1)+ema_x51*I(dat11$x5==1)+ema_x52*I(dat11$x5==2)+ema_x62*I(dat11$x6==2)+ema_x63*I(dat11$x6==3)+ema_x64*I(dat11$x6==4)+ema_x71*I(dat11$x7==1)+ema_x72*I(dat11$x7==2)+ema_x73*I(dat11$x7==3)+ema_x74*I(dat11$x7==4)+ema_x75*I(dat11$x7==5))))^I(dat11$M==1)*
     (1/(1+exp(ema_0+ema_t*dat11$Z+ema_x1*I(dat11$x1==1)+ema_x21*I(dat11$x2==1)+ema_x22*I(dat11$x2==2)+ema_x31*I(dat11$x3==1)+ema_x32*I(dat11$x3==2)+ema_x33*I(dat11$x3==3)+ema_x4*I(dat11$x4==1)+ema_x51*I(dat11$x5==1)+ema_x52*I(dat11$x5==2)+ema_x62*I(dat11$x6==2)+ema_x63*I(dat11$x6==3)+ema_x64*I(dat11$x6==4)+ema_x71*I(dat11$x7==1)+ema_x72*I(dat11$x7==2)+ema_x73*I(dat11$x7==3)+ema_x74*I(dat11$x7==4)+ema_x75*I(dat11$x7==5))))^I(dat11$M==0)*
     (1/(1+exp(emc_0+emc_m*dat11$M+emc_t*dat11$Z+emc_x1*I(dat11$x1==1)+emc_x21*I(dat11$x2==1)+emc_x22*I(dat11$x2==2)+emc_x31*I(dat11$x3==1)+emc_x32*I(dat11$x3==2)+emc_x33*I(dat11$x3==3)+emc_x4*I(dat11$x4==1)+emc_x51*I(dat11$x5==1)+emc_x52*I(dat11$x5==2)+emc_x62*I(dat11$x6==2)+emc_x63*I(dat11$x6==3)+emc_x64*I(dat11$x6==4)+emc_x71*I(dat11$x7==1)+emc_x72*I(dat11$x7==2)+emc_x73*I(dat11$x7==3)+emc_x74*I(dat11$x7==4)+emc_x75*I(dat11$x7==5))))*
     (exp(emd_0+emd_rm*0+emd_t*dat11$Z+emd_x1*I(dat11$x1==1)+emd_x21*I(dat11$x2==1)+emd_x22*I(dat11$x2==2)+emd_x31*I(dat11$x3==1)+emd_x32*I(dat11$x3==2)+emd_x33*I(dat11$x3==3)+emd_x4*I(dat11$x4==1)+emd_x51*I(dat11$x5==1)+emd_x52*I(dat11$x5==2)+emd_x62*I(dat11$x6==2)+emd_x63*I(dat11$x6==3)+emd_x64*I(dat11$x6==4)+emd_x71*I(dat11$x7==1)+emd_x72*I(dat11$x7==2)+emd_x73*I(dat11$x7==3)+emd_x74*I(dat11$x7==4)+emd_x75*I(dat11$x7==5))/(1+exp(emd_0+emd_rm*0+emd_t*dat11$Z+emd_x1*I(dat11$x1==1)+emd_x21*I(dat11$x2==1)+emd_x22*I(dat11$x2==2)+emd_x31*I(dat11$x3==1)+emd_x32*I(dat11$x3==2)+emd_x33*I(dat11$x3==3)+emd_x4*I(dat11$x4==1)+emd_x51*I(dat11$x5==1)+emd_x52*I(dat11$x5==2)+emd_x62*I(dat11$x6==2)+emd_x63*I(dat11$x6==3)+emd_x64*I(dat11$x6==4)+emd_x71*I(dat11$x7==1)+emd_x72*I(dat11$x7==2)+emd_x73*I(dat11$x7==3)+emd_x74*I(dat11$x7==4)+emd_x75*I(dat11$x7==5))))
#missing y
l2<-(exp(ema_0+ema_t*dat2$Z+ema_x1*I(dat2$x1==1)+ema_x21*I(dat2$x2==1)+ema_x22*I(dat2$x2==2)+ema_x31*I(dat2$x3==1)+ema_x32*I(dat2$x3==2)+ema_x33*I(dat2$x3==3)+ema_x4*I(dat2$x4==1)+ema_x51*I(dat2$x5==1)+ema_x52*I(dat2$x5==2)+ema_x62*I(dat2$x6==2)+ema_x63*I(dat2$x6==3)+ema_x64*I(dat2$x6==4)+ema_x71*I(dat2$x7==1)+ema_x72*I(dat2$x7==2)+ema_x73*I(dat2$x7==3)+ema_x74*I(dat2$x7==4)+ema_x75*I(dat2$x7==5))/(1+exp(ema_0+ema_t*dat2$Z+ema_x1*I(dat2$x1==1)+ema_x21*I(dat2$x2==1)+ema_x22*I(dat2$x2==2)+ema_x31*I(dat2$x3==1)+ema_x32*I(dat2$x3==2)+ema_x33*I(dat2$x3==3)+ema_x4*I(dat2$x4==1)+ema_x51*I(dat2$x5==1)+ema_x52*I(dat2$x5==2)+ema_x62*I(dat2$x6==2)+ema_x63*I(dat2$x6==3)+ema_x64*I(dat2$x6==4)+ema_x71*I(dat2$x7==1)+ema_x72*I(dat2$x7==2)+ema_x73*I(dat2$x7==3)+ema_x74*I(dat2$x7==4)+ema_x75*I(dat2$x7==5))))^I(dat2$M==1)*
    (1/(1+exp(ema_0+ema_t*dat2$Z+ema_x1*I(dat2$x1==1)+ema_x21*I(dat2$x2==1)+ema_x22*I(dat2$x2==2)+ema_x31*I(dat2$x3==1)+ema_x32*I(dat2$x3==2)+ema_x33*I(dat2$x3==3)+ema_x4*I(dat2$x4==1)+ema_x51*I(dat2$x5==1)+ema_x52*I(dat2$x5==2)+ema_x62*I(dat2$x6==2)+ema_x63*I(dat2$x6==3)+ema_x64*I(dat2$x6==4)+ema_x71*I(dat2$x7==1)+ema_x72*I(dat2$x7==2)+ema_x73*I(dat2$x7==3)+ema_x74*I(dat2$x7==4)+ema_x75*I(dat2$x7==5))))^I(dat2$M==0)*
    (exp(emc_0+emc_m*dat2$M+emc_t*dat2$Z+emc_x1*I(dat2$x1==1)+emc_x21*I(dat2$x2==1)+emc_x22*I(dat2$x2==2)+emc_x31*I(dat2$x3==1)+emc_x32*I(dat2$x3==2)+emc_x33*I(dat2$x3==3)+emc_x4*I(dat2$x4==1)+emc_x51*I(dat2$x5==1)+emc_x52*I(dat2$x5==2)+emc_x62*I(dat2$x6==2)+emc_x63*I(dat2$x6==3)+emc_x64*I(dat2$x6==4)+emc_x71*I(dat2$x7==1)+emc_x72*I(dat2$x7==2)+emc_x73*I(dat2$x7==3)+emc_x74*I(dat2$x7==4)+emc_x75*I(dat2$x7==5))/(1+exp(emc_0+emc_m*dat2$M+emc_t*dat2$Z+emc_x1*I(dat2$x1==1)+emc_x21*I(dat2$x2==1)+emc_x22*I(dat2$x2==2)+emc_x31*I(dat2$x3==1)+emc_x32*I(dat2$x3==2)+emc_x33*I(dat2$x3==3)+emc_x4*I(dat2$x4==1)+emc_x51*I(dat2$x5==1)+emc_x52*I(dat2$x5==2)+emc_x62*I(dat2$x6==2)+emc_x63*I(dat2$x6==3)+emc_x64*I(dat2$x6==4)+emc_x71*I(dat2$x7==1)+emc_x72*I(dat2$x7==2)+emc_x73*I(dat2$x7==3)+emc_x74*I(dat2$x7==4)+emc_x75*I(dat2$x7==5))))*
    (1/(1+exp(emd_0+emd_rm*1+emd_t*dat2$Z+emd_x1*I(dat2$x1==1)+emd_x21*I(dat2$x2==1)+emd_x22*I(dat2$x2==2)+emd_x31*I(dat2$x3==1)+emd_x32*I(dat2$x3==2)+emd_x33*I(dat2$x3==3)+emd_x4*I(dat2$x4==1)+emd_x51*I(dat2$x5==1)+emd_x52*I(dat2$x5==2)+emd_x62*I(dat2$x6==2)+emd_x63*I(dat2$x6==3)+emd_x64*I(dat2$x6==4)+emd_x71*I(dat2$x7==1)+emd_x72*I(dat2$x7==2)+emd_x73*I(dat2$x7==3)+emd_x74*I(dat2$x7==4)+emd_x75*I(dat2$x7==5))))
#missing m and y (m=0)
l30<-(exp(ema_0+ema_t*dat30$Z+ema_x1*I(dat30$x1==1)+ema_x21*I(dat30$x2==1)+ema_x22*I(dat30$x2==2)+ema_x31*I(dat30$x3==1)+ema_x32*I(dat30$x3==2)+ema_x33*I(dat30$x3==3)+ema_x4*I(dat30$x4==1)+ema_x51*I(dat30$x5==1)+ema_x52*I(dat30$x5==2)+ema_x62*I(dat30$x6==2)+ema_x63*I(dat30$x6==3)+ema_x64*I(dat30$x6==4)+ema_x71*I(dat30$x7==1)+ema_x72*I(dat30$x7==2)+ema_x73*I(dat30$x7==3)+ema_x74*I(dat30$x7==4)+ema_x75*I(dat30$x7==5))/(1+exp(ema_0+ema_t*dat30$Z+ema_x1*I(dat30$x1==1)+ema_x21*I(dat30$x2==1)+ema_x22*I(dat30$x2==2)+ema_x31*I(dat30$x3==1)+ema_x32*I(dat30$x3==2)+ema_x33*I(dat30$x3==3)+ema_x4*I(dat30$x4==1)+ema_x51*I(dat30$x5==1)+ema_x52*I(dat30$x5==2)+ema_x62*I(dat30$x6==2)+ema_x63*I(dat30$x6==3)+ema_x64*I(dat30$x6==4)+ema_x71*I(dat30$x7==1)+ema_x72*I(dat30$x7==2)+ema_x73*I(dat30$x7==3)+ema_x74*I(dat30$x7==4)+ema_x75*I(dat30$x7==5))))^I(dat30$M==1)*
     (1/(1+exp(ema_0+ema_t*dat30$Z+ema_x1*I(dat30$x1==1)+ema_x21*I(dat30$x2==1)+ema_x22*I(dat30$x2==2)+ema_x31*I(dat30$x3==1)+ema_x32*I(dat30$x3==2)+ema_x33*I(dat30$x3==3)+ema_x4*I(dat30$x4==1)+ema_x51*I(dat30$x5==1)+ema_x52*I(dat30$x5==2)+ema_x62*I(dat30$x6==2)+ema_x63*I(dat30$x6==3)+ema_x64*I(dat30$x6==4)+ema_x71*I(dat30$x7==1)+ema_x72*I(dat30$x7==2)+ema_x73*I(dat30$x7==3)+ema_x74*I(dat30$x7==4)+ema_x75*I(dat30$x7==5))))^I(dat30$M==0)*
     (1/(1+exp(emc_0+emc_m*dat30$M+emc_t*dat30$Z+emc_x1*I(dat30$x1==1)+emc_x21*I(dat30$x2==1)+emc_x22*I(dat30$x2==2)+emc_x31*I(dat30$x3==1)+emc_x32*I(dat30$x3==2)+emc_x33*I(dat30$x3==3)+emc_x4*I(dat30$x4==1)+emc_x51*I(dat30$x5==1)+emc_x52*I(dat30$x5==2)+emc_x62*I(dat30$x6==2)+emc_x63*I(dat30$x6==3)+emc_x64*I(dat30$x6==4)+emc_x71*I(dat30$x7==1)+emc_x72*I(dat30$x7==2)+emc_x73*I(dat30$x7==3)+emc_x74*I(dat30$x7==4)+emc_x75*I(dat30$x7==5))))*
     (1/(1+exp(emd_0+emd_rm*0+emd_t*dat30$Z+emd_x1*I(dat30$x1==1)+emd_x21*I(dat30$x2==1)+emd_x22*I(dat30$x2==2)+emd_x31*I(dat30$x3==1)+emd_x32*I(dat30$x3==2)+emd_x33*I(dat30$x3==3)+emd_x4*I(dat30$x4==1)+emd_x51*I(dat30$x5==1)+emd_x52*I(dat30$x5==2)+emd_x62*I(dat30$x6==2)+emd_x63*I(dat30$x6==3)+emd_x64*I(dat30$x6==4)+emd_x71*I(dat30$x7==1)+emd_x72*I(dat30$x7==2)+emd_x73*I(dat30$x7==3)+emd_x74*I(dat30$x7==4)+emd_x75*I(dat30$x7==5))))
#missing m and y (m=1)
l31<-(exp(ema_0+ema_t*dat31$Z+ema_x1*I(dat31$x1==1)+ema_x21*I(dat31$x2==1)+ema_x22*I(dat31$x2==2)+ema_x31*I(dat31$x3==1)+ema_x32*I(dat31$x3==2)+ema_x33*I(dat31$x3==3)+ema_x4*I(dat31$x4==1)+ema_x51*I(dat31$x5==1)+ema_x52*I(dat31$x5==2)+ema_x62*I(dat31$x6==2)+ema_x63*I(dat31$x6==3)+ema_x64*I(dat31$x6==4)+ema_x71*I(dat31$x7==1)+ema_x72*I(dat31$x7==2)+ema_x73*I(dat31$x7==3)+ema_x74*I(dat31$x7==4)+ema_x75*I(dat31$x7==5))/(1+exp(ema_0+ema_t*dat31$Z+ema_x1*I(dat31$x1==1)+ema_x21*I(dat31$x2==1)+ema_x22*I(dat31$x2==2)+ema_x31*I(dat31$x3==1)+ema_x32*I(dat31$x3==2)+ema_x33*I(dat31$x3==3)+ema_x4*I(dat31$x4==1)+ema_x51*I(dat31$x5==1)+ema_x52*I(dat31$x5==2)+ema_x62*I(dat31$x6==2)+ema_x63*I(dat31$x6==3)+ema_x64*I(dat31$x6==4)+ema_x71*I(dat31$x7==1)+ema_x72*I(dat31$x7==2)+ema_x73*I(dat31$x7==3)+ema_x74*I(dat31$x7==4)+ema_x75*I(dat31$x7==5))))^I(dat31$M==1)*
     (1/(1+exp(ema_0+ema_t*dat31$Z+ema_x1*I(dat31$x1==1)+ema_x21*I(dat31$x2==1)+ema_x22*I(dat31$x2==2)+ema_x31*I(dat31$x3==1)+ema_x32*I(dat31$x3==2)+ema_x33*I(dat31$x3==3)+ema_x4*I(dat31$x4==1)+ema_x51*I(dat31$x5==1)+ema_x52*I(dat31$x5==2)+ema_x62*I(dat31$x6==2)+ema_x63*I(dat31$x6==3)+ema_x64*I(dat31$x6==4)+ema_x71*I(dat31$x7==1)+ema_x72*I(dat31$x7==2)+ema_x73*I(dat31$x7==3)+ema_x74*I(dat31$x7==4)+ema_x75*I(dat31$x7==5))))^I(dat31$M==0)*
     (1/(1+exp(emc_0+emc_m*dat31$M+emc_t*dat31$Z+emc_x1*I(dat31$x1==1)+emc_x21*I(dat31$x2==1)+emc_x22*I(dat31$x2==2)+emc_x31*I(dat31$x3==1)+emc_x32*I(dat31$x3==2)+emc_x33*I(dat31$x3==3)+emc_x4*I(dat31$x4==1)+emc_x51*I(dat31$x5==1)+emc_x52*I(dat31$x5==2)+emc_x62*I(dat31$x6==2)+emc_x63*I(dat31$x6==3)+emc_x64*I(dat31$x6==4)+emc_x71*I(dat31$x7==1)+emc_x72*I(dat31$x7==2)+emc_x73*I(dat31$x7==3)+emc_x74*I(dat31$x7==4)+emc_x75*I(dat31$x7==5))))*
     (1/(1+exp(emd_0+emd_rm*0+emd_t*dat31$Z+emd_x1*I(dat31$x1==1)+emd_x21*I(dat31$x2==1)+emd_x22*I(dat31$x2==2)+emd_x31*I(dat31$x3==1)+emd_x32*I(dat31$x3==2)+emd_x33*I(dat31$x3==3)+emd_x4*I(dat31$x4==1)+emd_x51*I(dat31$x5==1)+emd_x52*I(dat31$x5==2)+emd_x62*I(dat31$x6==2)+emd_x63*I(dat31$x6==3)+emd_x64*I(dat31$x6==4)+emd_x71*I(dat31$x7==1)+emd_x72*I(dat31$x7==2)+emd_x73*I(dat31$x7==3)+emd_x74*I(dat31$x7==4)+emd_x75*I(dat31$x7==5))))
#add together
logl<-sum(log(l0))+sum(log(l10+l11))+sum(log(l2))+sum(log(l30+l31))

save <- matrix(c(cca_0,mia_0,ema_0,
                 cca_t,mia_t,ema_t,
                 cca_x1,mia_x1,ema_x1,
                 cca_x21,mia_x21,ema_x21,
                 cca_x22,mia_x22,ema_x22,
                 cca_x31,mia_x31,ema_x31,
                 cca_x32,mia_x32,ema_x32,
                 cca_x33,mia_x33,ema_x33,
                 cca_x4,mia_x4,ema_x4,
                 cca_x51,mia_x51,ema_x51,
                 cca_x52,mia_x52,ema_x52,
                 cca_x62,mia_x62,ema_x62,
                 cca_x63,mia_x63,ema_x63,
                 cca_x64,mia_x64,ema_x64,
                 cca_x71,mia_x71,ema_x71,
                 cca_x72,mia_x72,ema_x72,
                 cca_x73,mia_x73,ema_x73,
                 cca_x74,mia_x74,ema_x74,
                 cca_x75,mia_x75,ema_x75,
                 ccb_0,mib_0,emb_0,
                 ccb_m,mib_m,emb_m,
                 ccb_t,mib_t,emb_t,
                 ccb_mt,mib_mt,emb_mt,
                 ccb_x1,mib_x1,emb_x1,
                 ccb_x21,mib_x21,emb_x21,
                 ccb_x22,mib_x22,emb_x22,
                 ccb_x31,mib_x31,emb_x31,
                 ccb_x32,mib_x32,emb_x32,
                 ccb_x33,mib_x33,emb_x33,
                 ccb_x4,mib_x4,emb_x4,
                 ccb_x51,mib_x51,emb_x51,
                 ccb_x52,mib_x52,emb_x52,
                 ccb_x62,mib_x62,emb_x62,
                 ccb_x63,mib_x63,emb_x63,
                 ccb_x64,mib_x64,emb_x64,
                 ccb_x71,mib_x71,emb_x71,
                 ccb_x72,mib_x72,emb_x72,
                 ccb_x73,mib_x73,emb_x73,
                 ccb_x74,mib_x74,emb_x74,
                 ccb_x75,mib_x75,emb_x75,
                 cc_shape,mi_shape,em_shape,
                 ccb0_0,mib0_0,emb0_0,
                 ccb0_m,mib0_m,emb0_m,
                 ccb0_t,mib0_t,emb0_t,
                 ccb0_mt,mib0_mt,emb0_mt,
                 ccb0_x1,mib0_x1,emb0_x1,
                 ccb0_x21,mib0_x21,emb0_x21,
                 ccb0_x22,mib0_x22,emb0_x22,
                 ccb0_x31,mib0_x31,emb0_x31,
                 ccb0_x32,mib0_x32,emb0_x32,
                 ccb0_x33,mib0_x33,emb0_x33,
                 ccb0_x4,mib0_x4,emb0_x4,
                 ccb0_x51,mib0_x51,emb0_x51,
                 ccb0_x52,mib0_x52,emb0_x52,
                 ccb0_x62,mib0_x62,emb0_x62,
                 ccb0_x63,mib0_x63,emb0_x63,
                 ccb0_x64,mib0_x64,emb0_x64,
                 ccb0_x71,mib0_x71,emb0_x71,
                 ccb0_x72,mib0_x72,emb0_x72,
                 ccb0_x73,mib0_x73,emb0_x73,
                 ccb0_x74,mib0_x74,emb0_x74,
                 ccb0_x75,mib0_x75,emb0_x75,
                 ccc_0,mic_0,emc_0,
                 ccc_m,mic_m,emc_m,
                 ccc_t,mic_t,emc_t,
                 ccc_x1,mic_x1,emc_x1,
                 ccc_x21,mic_x21,emc_x21,
                 ccc_x22,mic_x22,emc_x22,
                 ccc_x31,mic_x31,emc_x31,
                 ccc_x32,mic_x32,emc_x32,
                 ccc_x33,mic_x33,emc_x33,
                 ccc_x4,mic_x4,emc_x4,
                 ccc_x51,mic_x51,emc_x51,
                 ccc_x52,mic_x52,emc_x52,
                 ccc_x62,mic_x62,emc_x62,
                 ccc_x63,mic_x63,emc_x63,
                 ccc_x64,mic_x64,emc_x64,
                 ccc_x71,mic_x71,emc_x71,
                 ccc_x72,mic_x72,emc_x72,
                 ccc_x73,mic_x73,emc_x73,
                 ccc_x74,mic_x74,emc_x74,
                 ccc_x75,mic_x75,emc_x75,
                 ccd_0,mid_0,emd_0,
                 ccd_rm,mid_rm,emd_rm,
                 ccd_t,mid_t,emd_t,
                 ccd_x1,mid_x1,emd_x1,
                 ccd_x21,mid_x21,emd_x21,
                 ccd_x22,mid_x22,emd_x22,
                 ccd_x31,mid_x31,emd_x31,
                 ccd_x32,mid_x32,emd_x32,
                 ccd_x33,mid_x33,emd_x33,
                 ccd_x4,mid_x4,emd_x4,
                 ccd_x51,mid_x51,emd_x51,
                 ccd_x52,mid_x52,emd_x52,
                 ccd_x62,mid_x62,emd_x62,
                 ccd_x63,mid_x63,emd_x63,
                 ccd_x64,mid_x64,emd_x64,
                 ccd_x71,mid_x71,emd_x71,
                 ccd_x72,mid_x72,emd_x72,
                 ccd_x73,mid_x73,emd_x73,
                 ccd_x74,mid_x74,emd_x74,
                 ccd_x75,mid_x75,emd_x75,
                 ccTIE,miTIE,emTIE,
                 ccPDE,miPDE,emPDE,
                 ccPIE,miPIE,emPIE,
                 ccTDE,miTDE,emTDE,
                 logl,logl,logl),byrow=T,107,3)

tab<-round(save,3)

library("xlsx")
write.xlsx(tab,'/Users/sushi5824907/Desktop/Mediation/JASA/Data_Analysis/II_Job_Gamma.xlsx',row.names = FALSE)






