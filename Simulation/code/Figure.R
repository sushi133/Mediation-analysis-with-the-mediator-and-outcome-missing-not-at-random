library(xlsx)
library(ggplot2)
library(dplyr)

#Figure S8

#MNAR I
BMY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_I.xlsx', 1)
BMCY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_I.xlsx', 1)
CMY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_I.xlsx', 1)
CMBY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_I.xlsx', 1)

BMY_I<-array(as.numeric(unlist(BMY_I)), dim = c(22, 4, 500))
BMCY_I<-array(as.numeric(unlist(BMCY_I)), dim = c(23, 4, 500))
CMY_I<-array(as.numeric(unlist(CMY_I)), dim = c(24, 4, 500))
CMBY_I<-array(as.numeric(unlist(CMBY_I)), dim = c(23, 4, 500))

BMY_I_NIE_OR<-cbind(BMY_I[c(17),1,],'NIE','OR','A.I')
BMY_I_NIE_CC<-cbind(BMY_I[c(17),2,],'NIE','CC','A.I')
BMY_I_NIE_MI<-cbind(BMY_I[c(17),3,],'NIE','MI','A.I')
BMY_I_NIE_EM<-cbind(BMY_I[c(17),4,],'NIE','EM','A.I')
BMY_I_NDE_OR<-cbind(BMY_I[c(18),1,],'NDE','OR','A.I')
BMY_I_NDE_CC<-cbind(BMY_I[c(18),2,],'NDE','CC','A.I')
BMY_I_NDE_MI<-cbind(BMY_I[c(18),3,],'NDE','MI','A.I')
BMY_I_NDE_EM<-cbind(BMY_I[c(18),4,],'NDE','EM','A.I')

BMCY_I_NIE_OR<-cbind(BMCY_I[c(18),1,],'NIE','OR','B.I')
BMCY_I_NIE_CC<-cbind(BMCY_I[c(18),2,],'NIE','CC','B.I')
BMCY_I_NIE_MI<-cbind(BMCY_I[c(18),3,],'NIE','MI','B.I')
BMCY_I_NIE_EM<-cbind(BMCY_I[c(18),4,],'NIE','EM','B.I')
BMCY_I_NDE_OR<-cbind(BMCY_I[c(19),1,],'NDE','OR','B.I')
BMCY_I_NDE_CC<-cbind(BMCY_I[c(19),2,],'NDE','CC','B.I')
BMCY_I_NDE_MI<-cbind(BMCY_I[c(19),3,],'NDE','MI','B.I')
BMCY_I_NDE_EM<-cbind(BMCY_I[c(19),4,],'NDE','EM','B.I')

CMY_I_NIE_OR<-cbind(CMY_I[c(19),1,],'NIE','OR','C.I')
CMY_I_NIE_CC<-cbind(CMY_I[c(19),2,],'NIE','CC','C.I')
CMY_I_NIE_MI<-cbind(CMY_I[c(19),3,],'NIE','MI','C.I')
CMY_I_NIE_EM<-cbind(CMY_I[c(19),4,],'NIE','EM','C.I')
CMY_I_NDE_OR<-cbind(CMY_I[c(20),1,],'NDE','OR','C.I')
CMY_I_NDE_CC<-cbind(CMY_I[c(20),2,],'NDE','CC','C.I')
CMY_I_NDE_MI<-cbind(CMY_I[c(20),3,],'NDE','MI','C.I')
CMY_I_NDE_EM<-cbind(CMY_I[c(20),4,],'NDE','EM','C.I')

CMBY_I_NIE_OR<-cbind(CMBY_I[c(18),1,],'NIE','OR','D.I')
CMBY_I_NIE_CC<-cbind(CMBY_I[c(18),2,],'NIE','CC','D.I')
CMBY_I_NIE_MI<-cbind(CMBY_I[c(18),3,],'NIE','MI','D.I')
CMBY_I_NIE_EM<-cbind(CMBY_I[c(18),4,],'NIE','EM','D.I')
CMBY_I_NDE_OR<-cbind(CMBY_I[c(19),1,],'NDE','OR','D.I')
CMBY_I_NDE_CC<-cbind(CMBY_I[c(19),2,],'NDE','CC','D.I')
CMBY_I_NDE_MI<-cbind(CMBY_I[c(19),3,],'NDE','MI','D.I')
CMBY_I_NDE_EM<-cbind(CMBY_I[c(19),4,],'NDE','EM','D.I')

MNAR_I<-as.data.frame(rbind(BMY_I_NIE_OR,BMY_I_NIE_CC,BMY_I_NIE_MI,BMY_I_NIE_EM,
                            BMY_I_NDE_OR,BMY_I_NDE_CC,BMY_I_NDE_MI,BMY_I_NDE_EM,
                            BMCY_I_NIE_OR,BMCY_I_NIE_CC,BMCY_I_NIE_MI,BMCY_I_NIE_EM,
                            BMCY_I_NDE_OR,BMCY_I_NDE_CC,BMCY_I_NDE_MI,BMCY_I_NDE_EM,
                            CMY_I_NIE_OR,CMY_I_NIE_CC,CMY_I_NIE_MI,CMY_I_NIE_EM,
                            CMY_I_NDE_OR,CMY_I_NDE_CC,CMY_I_NDE_MI,CMY_I_NDE_EM,
                            CMBY_I_NIE_OR,CMBY_I_NIE_CC,CMBY_I_NIE_MI,CMBY_I_NIE_EM,
                            CMBY_I_NDE_OR,CMBY_I_NDE_CC,CMBY_I_NDE_MI,CMBY_I_NDE_EM))

colnames(MNAR_I) <- c("bias","effect","method","case")
MNAR_I$bias<-as.numeric(MNAR_I$bias)
MNAR_I$case <- factor(MNAR_I$case, levels=c("A.I","B.I","C.I","D.I"))
MNAR_I$effect <- factor(MNAR_I$effect, levels=c("NIE","NDE"))
MNAR_I$method <- factor(MNAR_I$method, levels=c("CC","MI","EM","OR"))

#MNAR II
BMY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_II.xlsx', 1)
BMCY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_II.xlsx', 1)
CMY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_II.xlsx', 1)
CMBY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_II.xlsx', 1)

BMY_II<-array(as.numeric(unlist(BMY_II)), dim = c(28, 4, 500))
BMCY_II<-array(as.numeric(unlist(BMCY_II)), dim = c(29, 4, 500))
CMY_II<-array(as.numeric(unlist(CMY_II)), dim = c(30, 4, 500))
CMBY_II<-array(as.numeric(unlist(CMBY_II)), dim = c(29, 4, 500))

BMY_II_NIE_OR<-cbind(BMY_II[c(21),1,],'NIE','OR','A.II')
BMY_II_NIE_CC<-cbind(BMY_II[c(21),2,],'NIE','CC','A.II')
BMY_II_NIE_MI<-cbind(BMY_II[c(21),3,],'NIE','MI','A.II')
BMY_II_NIE_EM<-cbind(BMY_II[c(21),4,],'NIE','EM','A.II')
BMY_II_NDE_OR<-cbind(BMY_II[c(22),1,],'NDE','OR','A.II')
BMY_II_NDE_CC<-cbind(BMY_II[c(22),2,],'NDE','CC','A.II')
BMY_II_NDE_MI<-cbind(BMY_II[c(22),3,],'NDE','MI','A.II')
BMY_II_NDE_EM<-cbind(BMY_II[c(22),4,],'NDE','EM','A.II')

BMCY_II_NIE_OR<-cbind(BMCY_II[c(22),1,],'NIE','OR','B.II')
BMCY_II_NIE_CC<-cbind(BMCY_II[c(22),2,],'NIE','CC','B.II')
BMCY_II_NIE_MI<-cbind(BMCY_II[c(22),3,],'NIE','MI','B.II')
BMCY_II_NIE_EM<-cbind(BMCY_II[c(22),4,],'NIE','EM','B.II')
BMCY_II_NDE_OR<-cbind(BMCY_II[c(23),1,],'NDE','OR','B.II')
BMCY_II_NDE_CC<-cbind(BMCY_II[c(23),2,],'NDE','CC','B.II')
BMCY_II_NDE_MI<-cbind(BMCY_II[c(23),3,],'NDE','MI','B.II')
BMCY_II_NDE_EM<-cbind(BMCY_II[c(23),4,],'NDE','EM','B.II')

CMY_II_NIE_OR<-cbind(CMY_II[c(23),1,],'NIE','OR','C.II')
CMY_II_NIE_CC<-cbind(CMY_II[c(23),2,],'NIE','CC','C.II')
CMY_II_NIE_MI<-cbind(CMY_II[c(23),3,],'NIE','MI','C.II')
CMY_II_NIE_EM<-cbind(CMY_II[c(23),4,],'NIE','EM','C.II')
CMY_II_NDE_OR<-cbind(CMY_II[c(24),1,],'NDE','OR','C.II')
CMY_II_NDE_CC<-cbind(CMY_II[c(24),2,],'NDE','CC','C.II')
CMY_II_NDE_MI<-cbind(CMY_II[c(24),3,],'NDE','MI','C.II')
CMY_II_NDE_EM<-cbind(CMY_II[c(24),4,],'NDE','EM','C.II')

CMBY_II_NIE_OR<-cbind(CMBY_II[c(22),1,],'NIE','OR','D.II')
CMBY_II_NIE_CC<-cbind(CMBY_II[c(22),2,],'NIE','CC','D.II')
CMBY_II_NIE_MI<-cbind(CMBY_II[c(22),3,],'NIE','MI','D.II')
CMBY_II_NIE_EM<-cbind(CMBY_II[c(22),4,],'NIE','EM','D.II')
CMBY_II_NDE_OR<-cbind(CMBY_II[c(23),1,],'NDE','OR','D.II')
CMBY_II_NDE_CC<-cbind(CMBY_II[c(23),2,],'NDE','CC','D.II')
CMBY_II_NDE_MI<-cbind(CMBY_II[c(23),3,],'NDE','MI','D.II')
CMBY_II_NDE_EM<-cbind(CMBY_II[c(23),4,],'NDE','EM','D.II')

MNAR_II<-as.data.frame(rbind(BMY_II_NIE_OR,BMY_II_NIE_CC,BMY_II_NIE_MI,BMY_II_NIE_EM,
                             BMY_II_NDE_OR,BMY_II_NDE_CC,BMY_II_NDE_MI,BMY_II_NDE_EM,
                             BMCY_II_NIE_OR,BMCY_II_NIE_CC,BMCY_II_NIE_MI,BMCY_II_NIE_EM,
                             BMCY_II_NDE_OR,BMCY_II_NDE_CC,BMCY_II_NDE_MI,BMCY_II_NDE_EM,
                             CMY_II_NIE_OR,CMY_II_NIE_CC,CMY_II_NIE_MI,CMY_II_NIE_EM,
                             CMY_II_NDE_OR,CMY_II_NDE_CC,CMY_II_NDE_MI,CMY_II_NDE_EM,
                             CMBY_II_NIE_OR,CMBY_II_NIE_CC,CMBY_II_NIE_MI,CMBY_II_NIE_EM,
                             CMBY_II_NDE_OR,CMBY_II_NDE_CC,CMBY_II_NDE_MI,CMBY_II_NDE_EM))

colnames(MNAR_II) <- c("bias","effect","method","case")
MNAR_II$bias <- as.numeric(MNAR_II$bias)
MNAR_II$case <- factor(MNAR_II$case, levels=c("A.II","B.II","C.II","D.II"))
MNAR_II$effect <- factor(MNAR_II$effect, levels=c("NIE","NDE"))
MNAR_II$method <- factor(MNAR_II$method, levels=c("CC","MI","EM","OR"))

#MNAR III
BMY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_III.xlsx', 1)
BMCY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_III.xlsx', 1)
CMY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_III.xlsx', 1)
CMBY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_III.xlsx', 1)

BMY_III<-array(as.numeric(unlist(BMY_III)), dim = c(28, 4, 500))
BMCY_III<-array(as.numeric(unlist(BMCY_III)), dim = c(29, 4, 500))
CMY_III<-array(as.numeric(unlist(CMY_III)), dim = c(30, 4, 500))
CMBY_III<-array(as.numeric(unlist(CMBY_III)), dim = c(29, 4, 500))

BMY_III_NIE_OR<-cbind(BMY_III[c(21),1,],'NIE','OR','A.III')
BMY_III_NIE_CC<-cbind(BMY_III[c(21),2,],'NIE','CC','A.III')
BMY_III_NIE_MI<-cbind(BMY_III[c(21),3,],'NIE','MI','A.III')
BMY_III_NIE_EM<-cbind(BMY_III[c(21),4,],'NIE','EM','A.III')
BMY_III_NDE_OR<-cbind(BMY_III[c(22),1,],'NDE','OR','A.III')
BMY_III_NDE_CC<-cbind(BMY_III[c(22),2,],'NDE','CC','A.III')
BMY_III_NDE_MI<-cbind(BMY_III[c(22),3,],'NDE','MI','A.III')
BMY_III_NDE_EM<-cbind(BMY_III[c(22),4,],'NDE','EM','A.III')

BMCY_III_NIE_OR<-cbind(BMCY_III[c(22),1,],'NIE','OR','B.III')
BMCY_III_NIE_CC<-cbind(BMCY_III[c(22),2,],'NIE','CC','B.III')
BMCY_III_NIE_MI<-cbind(BMCY_III[c(22),3,],'NIE','MI','B.III')
BMCY_III_NIE_EM<-cbind(BMCY_III[c(22),4,],'NIE','EM','B.III')
BMCY_III_NDE_OR<-cbind(BMCY_III[c(23),1,],'NDE','OR','B.III')
BMCY_III_NDE_CC<-cbind(BMCY_III[c(23),2,],'NDE','CC','B.III')
BMCY_III_NDE_MI<-cbind(BMCY_III[c(23),3,],'NDE','MI','B.III')
BMCY_III_NDE_EM<-cbind(BMCY_III[c(23),4,],'NDE','EM','B.III')

CMY_III_NIE_OR<-cbind(CMY_III[c(23),1,],'NIE','OR','C.III')
CMY_III_NIE_CC<-cbind(CMY_III[c(23),2,],'NIE','CC','C.III')
CMY_III_NIE_MI<-cbind(CMY_III[c(23),3,],'NIE','MI','C.III')
CMY_III_NIE_EM<-cbind(CMY_III[c(23),4,],'NIE','EM','C.III')
CMY_III_NDE_OR<-cbind(CMY_III[c(24),1,],'NDE','OR','C.III')
CMY_III_NDE_CC<-cbind(CMY_III[c(24),2,],'NDE','CC','C.III')
CMY_III_NDE_MI<-cbind(CMY_III[c(24),3,],'NDE','MI','C.III')
CMY_III_NDE_EM<-cbind(CMY_III[c(24),4,],'NDE','EM','C.III')

CMBY_III_NIE_OR<-cbind(CMBY_III[c(22),1,],'NIE','OR','D.III')
CMBY_III_NIE_CC<-cbind(CMBY_III[c(22),2,],'NIE','CC','D.III')
CMBY_III_NIE_MI<-cbind(CMBY_III[c(22),3,],'NIE','MI','D.III')
CMBY_III_NIE_EM<-cbind(CMBY_III[c(22),4,],'NIE','EM','D.III')
CMBY_III_NDE_OR<-cbind(CMBY_III[c(23),1,],'NDE','OR','D.III')
CMBY_III_NDE_CC<-cbind(CMBY_III[c(23),2,],'NDE','CC','D.III')
CMBY_III_NDE_MI<-cbind(CMBY_III[c(23),3,],'NDE','MI','D.III')
CMBY_III_NDE_EM<-cbind(CMBY_III[c(23),4,],'NDE','EM','D.III')

MNAR_III<-as.data.frame(rbind(BMY_III_NIE_OR,BMY_III_NIE_CC,BMY_III_NIE_MI,BMY_III_NIE_EM,
                             BMY_III_NDE_OR,BMY_III_NDE_CC,BMY_III_NDE_MI,BMY_III_NDE_EM,
                             BMCY_III_NIE_OR,BMCY_III_NIE_CC,BMCY_III_NIE_MI,BMCY_III_NIE_EM,
                             BMCY_III_NDE_OR,BMCY_III_NDE_CC,BMCY_III_NDE_MI,BMCY_III_NDE_EM,
                             CMY_III_NIE_OR,CMY_III_NIE_CC,CMY_III_NIE_MI,CMY_III_NIE_EM,
                             CMY_III_NDE_OR,CMY_III_NDE_CC,CMY_III_NDE_MI,CMY_III_NDE_EM,
                             CMBY_III_NIE_OR,CMBY_III_NIE_CC,CMBY_III_NIE_MI,CMBY_III_NIE_EM,
                             CMBY_III_NDE_OR,CMBY_III_NDE_CC,CMBY_III_NDE_MI,CMBY_III_NDE_EM))

colnames(MNAR_III) <- c("bias","effect","method","case")
MNAR_III$bias<-as.numeric(MNAR_III$bias)
MNAR_III$case <- factor(MNAR_III$case, levels=c("A.III","B.III","C.III","D.III"))
MNAR_III$effect <- factor(MNAR_III$effect, levels=c("NIE","NDE"))
MNAR_III$method <- factor(MNAR_III$method, levels=c("CC","MI","EM","OR"))

#MNAR IV
BMY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_IV.xlsx', 1)
BMCY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_IV.xlsx', 1)
CMY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_IV.xlsx', 1)
CMBY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_IV.xlsx', 1)

BMY_IV<-array(as.numeric(unlist(BMY_IV)), dim = c(28, 4, 500))
BMCY_IV<-array(as.numeric(unlist(BMCY_IV)), dim = c(29, 4, 500))
CMY_IV<-array(as.numeric(unlist(CMY_IV)), dim = c(30, 4, 500))
CMBY_IV<-array(as.numeric(unlist(CMBY_IV)), dim = c(29, 4, 500))

BMY_IV_NIE_OR<-cbind(BMY_IV[c(21),1,],'NIE','OR','A.IV')
BMY_IV_NIE_CC<-cbind(BMY_IV[c(21),2,],'NIE','CC','A.IV')
BMY_IV_NIE_MI<-cbind(BMY_IV[c(21),3,],'NIE','MI','A.IV')
BMY_IV_NIE_EM<-cbind(BMY_IV[c(21),4,],'NIE','EM','A.IV')
BMY_IV_NDE_OR<-cbind(BMY_IV[c(22),1,],'NDE','OR','A.IV')
BMY_IV_NDE_CC<-cbind(BMY_IV[c(22),2,],'NDE','CC','A.IV')
BMY_IV_NDE_MI<-cbind(BMY_IV[c(22),3,],'NDE','MI','A.IV')
BMY_IV_NDE_EM<-cbind(BMY_IV[c(22),4,],'NDE','EM','A.IV')

BMCY_IV_NIE_OR<-cbind(BMCY_IV[c(22),1,],'NIE','OR','B.IV')
BMCY_IV_NIE_CC<-cbind(BMCY_IV[c(22),2,],'NIE','CC','B.IV')
BMCY_IV_NIE_MI<-cbind(BMCY_IV[c(22),3,],'NIE','MI','B.IV')
BMCY_IV_NIE_EM<-cbind(BMCY_IV[c(22),4,],'NIE','EM','B.IV')
BMCY_IV_NDE_OR<-cbind(BMCY_IV[c(23),1,],'NDE','OR','B.IV')
BMCY_IV_NDE_CC<-cbind(BMCY_IV[c(23),2,],'NDE','CC','B.IV')
BMCY_IV_NDE_MI<-cbind(BMCY_IV[c(23),3,],'NDE','MI','B.IV')
BMCY_IV_NDE_EM<-cbind(BMCY_IV[c(23),4,],'NDE','EM','B.IV')

CMY_IV_NIE_OR<-cbind(CMY_IV[c(23),1,],'NIE','OR','C.IV')
CMY_IV_NIE_CC<-cbind(CMY_IV[c(23),2,],'NIE','CC','C.IV')
CMY_IV_NIE_MI<-cbind(CMY_IV[c(23),3,],'NIE','MI','C.IV')
CMY_IV_NIE_EM<-cbind(CMY_IV[c(23),4,],'NIE','EM','C.IV')
CMY_IV_NDE_OR<-cbind(CMY_IV[c(24),1,],'NDE','OR','C.IV')
CMY_IV_NDE_CC<-cbind(CMY_IV[c(24),2,],'NDE','CC','C.IV')
CMY_IV_NDE_MI<-cbind(CMY_IV[c(24),3,],'NDE','MI','C.IV')
CMY_IV_NDE_EM<-cbind(CMY_IV[c(24),4,],'NDE','EM','C.IV')

CMBY_IV_NIE_OR<-cbind(CMBY_IV[c(22),1,],'NIE','OR','D.IV')
CMBY_IV_NIE_CC<-cbind(CMBY_IV[c(22),2,],'NIE','CC','D.IV')
CMBY_IV_NIE_MI<-cbind(CMBY_IV[c(22),3,],'NIE','MI','D.IV')
CMBY_IV_NIE_EM<-cbind(CMBY_IV[c(22),4,],'NIE','EM','D.IV')
CMBY_IV_NDE_OR<-cbind(CMBY_IV[c(23),1,],'NDE','OR','D.IV')
CMBY_IV_NDE_CC<-cbind(CMBY_IV[c(23),2,],'NDE','CC','D.IV')
CMBY_IV_NDE_MI<-cbind(CMBY_IV[c(23),3,],'NDE','MI','D.IV')
CMBY_IV_NDE_EM<-cbind(CMBY_IV[c(23),4,],'NDE','EM','D.IV')

MNAR_IV<-as.data.frame(rbind(BMY_IV_NIE_OR,BMY_IV_NIE_CC,BMY_IV_NIE_MI,BMY_IV_NIE_EM,
                             BMY_IV_NDE_OR,BMY_IV_NDE_CC,BMY_IV_NDE_MI,BMY_IV_NDE_EM,
                             BMCY_IV_NIE_OR,BMCY_IV_NIE_CC,BMCY_IV_NIE_MI,BMCY_IV_NIE_EM,
                             BMCY_IV_NDE_OR,BMCY_IV_NDE_CC,BMCY_IV_NDE_MI,BMCY_IV_NDE_EM,
                             CMY_IV_NIE_OR,CMY_IV_NIE_CC,CMY_IV_NIE_MI,CMY_IV_NIE_EM,
                             CMY_IV_NDE_OR,CMY_IV_NDE_CC,CMY_IV_NDE_MI,CMY_IV_NDE_EM,
                             CMBY_IV_NIE_OR,CMBY_IV_NIE_CC,CMBY_IV_NIE_MI,CMBY_IV_NIE_EM,
                             CMBY_IV_NDE_OR,CMBY_IV_NDE_CC,CMBY_IV_NDE_MI,CMBY_IV_NDE_EM))

colnames(MNAR_IV) <- c("bias","effect","method","case")
MNAR_IV$bias<-as.numeric(MNAR_IV$bias)
MNAR_IV$case <- factor(MNAR_IV$case, levels=c("A.IV","B.IV","C.IV","D.IV"))
MNAR_IV$effect <- factor(MNAR_IV$effect, levels=c("NIE","NDE"))
MNAR_IV$method <- factor(MNAR_IV$method, levels=c("CC","MI","EM","OR"))

#purpose: manually set the y-axis tick values
count <- 0
breaks_fun <- function(x) {
  count <<- count + 1L
  switch(
    count,
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20),
    seq(-600, 600, 20)
  )
}
MNAR <- rbind(MNAR_I,MNAR_II,MNAR_III,MNAR_IV)
MNAR$label <- paste0(MNAR$method,'\n',MNAR$effect)
MNAR$label <- factor(MNAR$label, levels=c("CC\nNIE","MI\nNIE","EM\nNIE","OR\nNIE","CC\nNDE","MI\nNDE","EM\nNDE","OR\nNDE"))
ggplot(MNAR, aes(x=label, y=bias*100, color=method)) + geom_boxplot(outlier.shape = 1, outlier.color = NULL) +
  stat_summary(fun="mean", geom="point", shape=5, position = position_dodge2(width = 0.75, preserve = "single")) +
  labs(x = '', y = 'Bias (%)') +
  scale_color_manual(values = c("#0099f8","#E69F00","red3","green4")) +
  geom_hline(yintercept = 0, linetype="dashed") +
  facet_wrap(vars(case), nrow = 4, scales="free") +
  theme_bw() +
  scale_y_continuous(breaks = breaks_fun) +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.8),
        #legend.position = 'bottom',
        legend.position = "none",
        legend.text = element_text(size = 20),
        legend.key.size = unit(2, 'lines'),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank())


#Figure S9

#MNAR I (NULL)
BMY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_I(0).xlsx', 1)
BMCY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_I(0).xlsx', 1)
CMY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_I(0).xlsx', 1)
CMBY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_I(0).xlsx', 1)

BMY0_I<-array(as.numeric(unlist(BMY0_I)), dim = c(22, 4, 500))
BMCY0_I<-array(as.numeric(unlist(BMCY0_I)), dim = c(23, 4, 500))
CMY0_I<-array(as.numeric(unlist(CMY0_I)), dim = c(24, 4, 500))
CMBY0_I<-array(as.numeric(unlist(CMBY0_I)), dim = c(23, 4, 500))

BMY0_I_NIE_OR<-cbind(BMY0_I[c(17),1,],'NIE','OR','A.I (0)')
BMY0_I_NIE_CC<-cbind(BMY0_I[c(17),2,],'NIE','CC','A.I (0)')
BMY0_I_NIE_MI<-cbind(BMY0_I[c(17),3,],'NIE','MI','A.I (0)')
BMY0_I_NIE_EM<-cbind(BMY0_I[c(17),4,],'NIE','EM','A.I (0)')
BMY0_I_NDE_OR<-cbind(BMY0_I[c(18),1,],'NDE','OR','A.I (0)')
BMY0_I_NDE_CC<-cbind(BMY0_I[c(18),2,],'NDE','CC','A.I (0)')
BMY0_I_NDE_MI<-cbind(BMY0_I[c(18),3,],'NDE','MI','A.I (0)')
BMY0_I_NDE_EM<-cbind(BMY0_I[c(18),4,],'NDE','EM','A.I (0)')

BMCY0_I_NIE_OR<-cbind(BMCY0_I[c(18),1,],'NIE','OR','B.I (0)')
BMCY0_I_NIE_CC<-cbind(BMCY0_I[c(18),2,],'NIE','CC','B.I (0)')
BMCY0_I_NIE_MI<-cbind(BMCY0_I[c(18),3,],'NIE','MI','B.I (0)')
BMCY0_I_NIE_EM<-cbind(BMCY0_I[c(18),4,],'NIE','EM','B.I (0)')
BMCY0_I_NDE_OR<-cbind(BMCY0_I[c(19),1,],'NDE','OR','B.I (0)')
BMCY0_I_NDE_CC<-cbind(BMCY0_I[c(19),2,],'NDE','CC','B.I (0)')
BMCY0_I_NDE_MI<-cbind(BMCY0_I[c(19),3,],'NDE','MI','B.I (0)')
BMCY0_I_NDE_EM<-cbind(BMCY0_I[c(19),4,],'NDE','EM','B.I (0)')

CMY0_I_NIE_OR<-cbind(CMY0_I[c(19),1,],'NIE','OR','C.I (0)')
CMY0_I_NIE_CC<-cbind(CMY0_I[c(19),2,],'NIE','CC','C.I (0)')
CMY0_I_NIE_MI<-cbind(CMY0_I[c(19),3,],'NIE','MI','C.I (0)')
CMY0_I_NIE_EM<-cbind(CMY0_I[c(19),4,],'NIE','EM','C.I (0)')
CMY0_I_NDE_OR<-cbind(CMY0_I[c(20),1,],'NDE','OR','C.I (0)')
CMY0_I_NDE_CC<-cbind(CMY0_I[c(20),2,],'NDE','CC','C.I (0)')
CMY0_I_NDE_MI<-cbind(CMY0_I[c(20),3,],'NDE','MI','C.I (0)')
CMY0_I_NDE_EM<-cbind(CMY0_I[c(20),4,],'NDE','EM','C.I (0)')

CMBY0_I_NIE_OR<-cbind(CMBY0_I[c(18),1,],'NIE','OR','D.I (0)')
CMBY0_I_NIE_CC<-cbind(CMBY0_I[c(18),2,],'NIE','CC','D.I (0)')
CMBY0_I_NIE_MI<-cbind(CMBY0_I[c(18),3,],'NIE','MI','D.I (0)')
CMBY0_I_NIE_EM<-cbind(CMBY0_I[c(18),4,],'NIE','EM','D.I (0)')
CMBY0_I_NDE_OR<-cbind(CMBY0_I[c(19),1,],'NDE','OR','D.I (0)')
CMBY0_I_NDE_CC<-cbind(CMBY0_I[c(19),2,],'NDE','CC','D.I (0)')
CMBY0_I_NDE_MI<-cbind(CMBY0_I[c(19),3,],'NDE','MI','D.I (0)')
CMBY0_I_NDE_EM<-cbind(CMBY0_I[c(19),4,],'NDE','EM','D.I (0)')

MNAR0_I<-as.data.frame(rbind(BMY0_I_NIE_OR,BMY0_I_NIE_CC,BMY0_I_NIE_MI,BMY0_I_NIE_EM,
                             BMY0_I_NDE_OR,BMY0_I_NDE_CC,BMY0_I_NDE_MI,BMY0_I_NDE_EM,
                             BMCY0_I_NIE_OR,BMCY0_I_NIE_CC,BMCY0_I_NIE_MI,BMCY0_I_NIE_EM,
                             BMCY0_I_NDE_OR,BMCY0_I_NDE_CC,BMCY0_I_NDE_MI,BMCY0_I_NDE_EM,
                             CMY0_I_NIE_OR,CMY0_I_NIE_CC,CMY0_I_NIE_MI,CMY0_I_NIE_EM,
                             CMY0_I_NDE_OR,CMY0_I_NDE_CC,CMY0_I_NDE_MI,CMY0_I_NDE_EM,
                             CMBY0_I_NIE_OR,CMBY0_I_NIE_CC,CMBY0_I_NIE_MI,CMBY0_I_NIE_EM,
                             CMBY0_I_NDE_OR,CMBY0_I_NDE_CC,CMBY0_I_NDE_MI,CMBY0_I_NDE_EM))

colnames(MNAR0_I) <- c("bias","effect","method","case")
MNAR0_I$bias<-as.numeric(MNAR0_I$bias)
MNAR0_I$case <- factor(MNAR0_I$case, levels=c("A.I (0)","B.I (0)","C.I (0)","D.I (0)"))
MNAR0_I$effect <- factor(MNAR0_I$effect, levels=c("NIE","NDE"))
MNAR0_I$method <- factor(MNAR0_I$method, levels=c("CC","MI","EM","OR"))

#MNAR II (NULL)
BMY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_II(0).xlsx', 1)
BMCY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_II(0).xlsx', 1)
CMY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_II(0).xlsx', 1)
CMBY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_II(0).xlsx', 1)

BMY0_II<-array(as.numeric(unlist(BMY0_II)), dim = c(28, 4, 500))
BMCY0_II<-array(as.numeric(unlist(BMCY0_II)), dim = c(29, 4, 500))
CMY0_II<-array(as.numeric(unlist(CMY0_II)), dim = c(30, 4, 500))
CMBY0_II<-array(as.numeric(unlist(CMBY0_II)), dim = c(29, 4, 500))

BMY0_II_NIE_OR<-cbind(BMY0_II[c(21),1,],'NIE','OR','A.II (0)')
BMY0_II_NIE_CC<-cbind(BMY0_II[c(21),2,],'NIE','CC','A.II (0)')
BMY0_II_NIE_MI<-cbind(BMY0_II[c(21),3,],'NIE','MI','A.II (0)')
BMY0_II_NIE_EM<-cbind(BMY0_II[c(21),4,],'NIE','EM','A.II (0)')
BMY0_II_NDE_OR<-cbind(BMY0_II[c(22),1,],'NDE','OR','A.II (0)')
BMY0_II_NDE_CC<-cbind(BMY0_II[c(22),2,],'NDE','CC','A.II (0)')
BMY0_II_NDE_MI<-cbind(BMY0_II[c(22),3,],'NDE','MI','A.II (0)')
BMY0_II_NDE_EM<-cbind(BMY0_II[c(22),4,],'NDE','EM','A.II (0)')

BMCY0_II_NIE_OR<-cbind(BMCY0_II[c(22),1,],'NIE','OR','B.II (0)')
BMCY0_II_NIE_CC<-cbind(BMCY0_II[c(22),2,],'NIE','CC','B.II (0)')
BMCY0_II_NIE_MI<-cbind(BMCY0_II[c(22),3,],'NIE','MI','B.II (0)')
BMCY0_II_NIE_EM<-cbind(BMCY0_II[c(22),4,],'NIE','EM','B.II (0)')
BMCY0_II_NDE_OR<-cbind(BMCY0_II[c(23),1,],'NDE','OR','B.II (0)')
BMCY0_II_NDE_CC<-cbind(BMCY0_II[c(23),2,],'NDE','CC','B.II (0)')
BMCY0_II_NDE_MI<-cbind(BMCY0_II[c(23),3,],'NDE','MI','B.II (0)')
BMCY0_II_NDE_EM<-cbind(BMCY0_II[c(23),4,],'NDE','EM','B.II (0)')

CMY0_II_NIE_OR<-cbind(CMY0_II[c(23),1,],'NIE','OR','C.II (0)')
CMY0_II_NIE_CC<-cbind(CMY0_II[c(23),2,],'NIE','CC','C.II (0)')
CMY0_II_NIE_MI<-cbind(CMY0_II[c(23),3,],'NIE','MI','C.II (0)')
CMY0_II_NIE_EM<-cbind(CMY0_II[c(23),4,],'NIE','EM','C.II (0)')
CMY0_II_NDE_OR<-cbind(CMY0_II[c(24),1,],'NDE','OR','C.II (0)')
CMY0_II_NDE_CC<-cbind(CMY0_II[c(24),2,],'NDE','CC','C.II (0)')
CMY0_II_NDE_MI<-cbind(CMY0_II[c(24),3,],'NDE','MI','C.II (0)')
CMY0_II_NDE_EM<-cbind(CMY0_II[c(24),4,],'NDE','EM','C.II (0)')

CMBY0_II_NIE_OR<-cbind(CMBY0_II[c(22),1,],'NIE','OR','D.II (0)')
CMBY0_II_NIE_CC<-cbind(CMBY0_II[c(22),2,],'NIE','CC','D.II (0)')
CMBY0_II_NIE_MI<-cbind(CMBY0_II[c(22),3,],'NIE','MI','D.II (0)')
CMBY0_II_NIE_EM<-cbind(CMBY0_II[c(22),4,],'NIE','EM','D.II (0)')
CMBY0_II_NDE_OR<-cbind(CMBY0_II[c(23),1,],'NDE','OR','D.II (0)')
CMBY0_II_NDE_CC<-cbind(CMBY0_II[c(23),2,],'NDE','CC','D.II (0)')
CMBY0_II_NDE_MI<-cbind(CMBY0_II[c(23),3,],'NDE','MI','D.II (0)')
CMBY0_II_NDE_EM<-cbind(CMBY0_II[c(23),4,],'NDE','EM','D.II (0)')

MNAR0_II<-as.data.frame(rbind(BMY0_II_NIE_OR,BMY0_II_NIE_CC,BMY0_II_NIE_MI,BMY0_II_NIE_EM,
                              BMY0_II_NDE_OR,BMY0_II_NDE_CC,BMY0_II_NDE_MI,BMY0_II_NDE_EM,
                              BMCY0_II_NIE_OR,BMCY0_II_NIE_CC,BMCY0_II_NIE_MI,BMCY0_II_NIE_EM,
                              BMCY0_II_NDE_OR,BMCY0_II_NDE_CC,BMCY0_II_NDE_MI,BMCY0_II_NDE_EM,
                              CMY0_II_NIE_OR,CMY0_II_NIE_CC,CMY0_II_NIE_MI,CMY0_II_NIE_EM,
                              CMY0_II_NDE_OR,CMY0_II_NDE_CC,CMY0_II_NDE_MI,CMY0_II_NDE_EM,
                              CMBY0_II_NIE_OR,CMBY0_II_NIE_CC,CMBY0_II_NIE_MI,CMBY0_II_NIE_EM,
                              CMBY0_II_NDE_OR,CMBY0_II_NDE_CC,CMBY0_II_NDE_MI,CMBY0_II_NDE_EM))

colnames(MNAR0_II) <- c("bias","effect","method","case")
MNAR0_II$bias<-as.numeric(MNAR0_II$bias)
MNAR0_II$case <- factor(MNAR0_II$case, levels=c("A.II (0)","B.II (0)","C.II (0)","D.II (0)"))
MNAR0_II$effect <- factor(MNAR0_II$effect, levels=c("NIE","NDE"))
MNAR0_II$method <- factor(MNAR0_II$method, levels=c("CC","MI","EM","OR"))

#MNAR III (NULL)
BMY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_III(0).xlsx', 1)
BMCY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_III(0).xlsx', 1)
CMY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_III(0).xlsx', 1)
CMBY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_III(0).xlsx', 1)

BMY0_III<-array(as.numeric(unlist(BMY0_III)), dim = c(28, 4, 500))
BMCY0_III<-array(as.numeric(unlist(BMCY0_III)), dim = c(29, 4, 500))
CMY0_III<-array(as.numeric(unlist(CMY0_III)), dim = c(30, 4, 500))
CMBY0_III<-array(as.numeric(unlist(CMBY0_III)), dim = c(29, 4, 500))

BMY0_III_NIE_OR<-cbind(BMY0_III[c(21),1,],'NIE','OR','A.III (0)')
BMY0_III_NIE_CC<-cbind(BMY0_III[c(21),2,],'NIE','CC','A.III (0)')
BMY0_III_NIE_MI<-cbind(BMY0_III[c(21),3,],'NIE','MI','A.III (0)')
BMY0_III_NIE_EM<-cbind(BMY0_III[c(21),4,],'NIE','EM','A.III (0)')
BMY0_III_NDE_OR<-cbind(BMY0_III[c(22),1,],'NDE','OR','A.III (0)')
BMY0_III_NDE_CC<-cbind(BMY0_III[c(22),2,],'NDE','CC','A.III (0)')
BMY0_III_NDE_MI<-cbind(BMY0_III[c(22),3,],'NDE','MI','A.III (0)')
BMY0_III_NDE_EM<-cbind(BMY0_III[c(22),4,],'NDE','EM','A.III (0)')

BMCY0_III_NIE_OR<-cbind(BMCY0_III[c(22),1,],'NIE','OR','B.III (0)')
BMCY0_III_NIE_CC<-cbind(BMCY0_III[c(22),2,],'NIE','CC','B.III (0)')
BMCY0_III_NIE_MI<-cbind(BMCY0_III[c(22),3,],'NIE','MI','B.III (0)')
BMCY0_III_NIE_EM<-cbind(BMCY0_III[c(22),4,],'NIE','EM','B.III (0)')
BMCY0_III_NDE_OR<-cbind(BMCY0_III[c(23),1,],'NDE','OR','B.III (0)')
BMCY0_III_NDE_CC<-cbind(BMCY0_III[c(23),2,],'NDE','CC','B.III (0)')
BMCY0_III_NDE_MI<-cbind(BMCY0_III[c(23),3,],'NDE','MI','B.III (0)')
BMCY0_III_NDE_EM<-cbind(BMCY0_III[c(23),4,],'NDE','EM','B.III (0)')

CMY0_III_NIE_OR<-cbind(CMY0_III[c(23),1,],'NIE','OR','C.III (0)')
CMY0_III_NIE_CC<-cbind(CMY0_III[c(23),2,],'NIE','CC','C.III (0)')
CMY0_III_NIE_MI<-cbind(CMY0_III[c(23),3,],'NIE','MI','C.III (0)')
CMY0_III_NIE_EM<-cbind(CMY0_III[c(23),4,],'NIE','EM','C.III (0)')
CMY0_III_NDE_OR<-cbind(CMY0_III[c(24),1,],'NDE','OR','C.III (0)')
CMY0_III_NDE_CC<-cbind(CMY0_III[c(24),2,],'NDE','CC','C.III (0)')
CMY0_III_NDE_MI<-cbind(CMY0_III[c(24),3,],'NDE','MI','C.III (0)')
CMY0_III_NDE_EM<-cbind(CMY0_III[c(24),4,],'NDE','EM','C.III (0)')

CMBY0_III_NIE_OR<-cbind(CMBY0_III[c(22),1,],'NIE','OR','D.III (0)')
CMBY0_III_NIE_CC<-cbind(CMBY0_III[c(22),2,],'NIE','CC','D.III (0)')
CMBY0_III_NIE_MI<-cbind(CMBY0_III[c(22),3,],'NIE','MI','D.III (0)')
CMBY0_III_NIE_EM<-cbind(CMBY0_III[c(22),4,],'NIE','EM','D.III (0)')
CMBY0_III_NDE_OR<-cbind(CMBY0_III[c(23),1,],'NDE','OR','D.III (0)')
CMBY0_III_NDE_CC<-cbind(CMBY0_III[c(23),2,],'NDE','CC','D.III (0)')
CMBY0_III_NDE_MI<-cbind(CMBY0_III[c(23),3,],'NDE','MI','D.III (0)')
CMBY0_III_NDE_EM<-cbind(CMBY0_III[c(23),4,],'NDE','EM','D.III (0)')

MNAR0_III<-as.data.frame(rbind(BMY0_III_NIE_OR,BMY0_III_NIE_CC,BMY0_III_NIE_MI,BMY0_III_NIE_EM,
                               BMY0_III_NDE_OR,BMY0_III_NDE_CC,BMY0_III_NDE_MI,BMY0_III_NDE_EM,
                               BMCY0_III_NIE_OR,BMCY0_III_NIE_CC,BMCY0_III_NIE_MI,BMCY0_III_NIE_EM,
                               BMCY0_III_NDE_OR,BMCY0_III_NDE_CC,BMCY0_III_NDE_MI,BMCY0_III_NDE_EM,
                               CMY0_III_NIE_OR,CMY0_III_NIE_CC,CMY0_III_NIE_MI,CMY0_III_NIE_EM,
                               CMY0_III_NDE_OR,CMY0_III_NDE_CC,CMY0_III_NDE_MI,CMY0_III_NDE_EM,
                               CMBY0_III_NIE_OR,CMBY0_III_NIE_CC,CMBY0_III_NIE_MI,CMBY0_III_NIE_EM,
                               CMBY0_III_NDE_OR,CMBY0_III_NDE_CC,CMBY0_III_NDE_MI,CMBY0_III_NDE_EM))

colnames(MNAR0_III) <- c("bias","effect","method","case")
MNAR0_III$bias<-as.numeric(MNAR0_III$bias)
MNAR0_III$case <- factor(MNAR0_III$case, levels=c("A.III (0)","B.III (0)","C.III (0)","D.III (0)"))
MNAR0_III$effect <- factor(MNAR0_III$effect, levels=c("NIE","NDE"))
MNAR0_III$method <- factor(MNAR0_III$method, levels=c("CC","MI","EM","OR"))

#MNAR IV (NULL)
BMY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_IV(0).xlsx', 1)
BMCY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_IV(0).xlsx', 1)
CMY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_IV(0).xlsx', 1)
CMBY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_IV(0).xlsx', 1)

BMY0_IV<-array(as.numeric(unlist(BMY0_IV)), dim = c(28, 4, 500))
BMCY0_IV<-array(as.numeric(unlist(BMCY0_IV)), dim = c(29, 4, 500))
CMY0_IV<-array(as.numeric(unlist(CMY0_IV)), dim = c(30, 4, 500))
CMBY0_IV<-array(as.numeric(unlist(CMBY0_IV)), dim = c(29, 4, 500))

BMY0_IV_NIE_OR<-cbind(BMY0_IV[c(21),1,],'NIE','OR','A.IV (0)')
BMY0_IV_NIE_CC<-cbind(BMY0_IV[c(21),2,],'NIE','CC','A.IV (0)')
BMY0_IV_NIE_MI<-cbind(BMY0_IV[c(21),3,],'NIE','MI','A.IV (0)')
BMY0_IV_NIE_EM<-cbind(BMY0_IV[c(21),4,],'NIE','EM','A.IV (0)')
BMY0_IV_NDE_OR<-cbind(BMY0_IV[c(22),1,],'NDE','OR','A.IV (0)')
BMY0_IV_NDE_CC<-cbind(BMY0_IV[c(22),2,],'NDE','CC','A.IV (0)')
BMY0_IV_NDE_MI<-cbind(BMY0_IV[c(22),3,],'NDE','MI','A.IV (0)')
BMY0_IV_NDE_EM<-cbind(BMY0_IV[c(22),4,],'NDE','EM','A.IV (0)')

BMCY0_IV_NIE_OR<-cbind(BMCY0_IV[c(22),1,],'NIE','OR','B.IV (0)')
BMCY0_IV_NIE_CC<-cbind(BMCY0_IV[c(22),2,],'NIE','CC','B.IV (0)')
BMCY0_IV_NIE_MI<-cbind(BMCY0_IV[c(22),3,],'NIE','MI','B.IV (0)')
BMCY0_IV_NIE_EM<-cbind(BMCY0_IV[c(22),4,],'NIE','EM','B.IV (0)')
BMCY0_IV_NDE_OR<-cbind(BMCY0_IV[c(23),1,],'NDE','OR','B.IV (0)')
BMCY0_IV_NDE_CC<-cbind(BMCY0_IV[c(23),2,],'NDE','CC','B.IV (0)')
BMCY0_IV_NDE_MI<-cbind(BMCY0_IV[c(23),3,],'NDE','MI','B.IV (0)')
BMCY0_IV_NDE_EM<-cbind(BMCY0_IV[c(23),4,],'NDE','EM','B.IV (0)')

CMY0_IV_NIE_OR<-cbind(CMY0_IV[c(23),1,],'NIE','OR','C.IV (0)')
CMY0_IV_NIE_CC<-cbind(CMY0_IV[c(23),2,],'NIE','CC','C.IV (0)')
CMY0_IV_NIE_MI<-cbind(CMY0_IV[c(23),3,],'NIE','MI','C.IV (0)')
CMY0_IV_NIE_EM<-cbind(CMY0_IV[c(23),4,],'NIE','EM','C.IV (0)')
CMY0_IV_NDE_OR<-cbind(CMY0_IV[c(24),1,],'NDE','OR','C.IV (0)')
CMY0_IV_NDE_CC<-cbind(CMY0_IV[c(24),2,],'NDE','CC','C.IV (0)')
CMY0_IV_NDE_MI<-cbind(CMY0_IV[c(24),3,],'NDE','MI','C.IV (0)')
CMY0_IV_NDE_EM<-cbind(CMY0_IV[c(24),4,],'NDE','EM','C.IV (0)')

CMBY0_IV_NIE_OR<-cbind(CMBY0_IV[c(22),1,],'NIE','OR','D.IV (0)')
CMBY0_IV_NIE_CC<-cbind(CMBY0_IV[c(22),2,],'NIE','CC','D.IV (0)')
CMBY0_IV_NIE_MI<-cbind(CMBY0_IV[c(22),3,],'NIE','MI','D.IV (0)')
CMBY0_IV_NIE_EM<-cbind(CMBY0_IV[c(22),4,],'NIE','EM','D.IV (0)')
CMBY0_IV_NDE_OR<-cbind(CMBY0_IV[c(23),1,],'NDE','OR','D.IV (0)')
CMBY0_IV_NDE_CC<-cbind(CMBY0_IV[c(23),2,],'NDE','CC','D.IV (0)')
CMBY0_IV_NDE_MI<-cbind(CMBY0_IV[c(23),3,],'NDE','MI','D.IV (0)')
CMBY0_IV_NDE_EM<-cbind(CMBY0_IV[c(23),4,],'NDE','EM','D.IV (0)')

MNAR0_IV<-as.data.frame(rbind(BMY0_IV_NIE_OR,BMY0_IV_NIE_CC,BMY0_IV_NIE_MI,BMY0_IV_NIE_EM,
                              BMY0_IV_NDE_OR,BMY0_IV_NDE_CC,BMY0_IV_NDE_MI,BMY0_IV_NDE_EM,
                              BMCY0_IV_NIE_OR,BMCY0_IV_NIE_CC,BMCY0_IV_NIE_MI,BMCY0_IV_NIE_EM,
                              BMCY0_IV_NDE_OR,BMCY0_IV_NDE_CC,BMCY0_IV_NDE_MI,BMCY0_IV_NDE_EM,
                              CMY0_IV_NIE_OR,CMY0_IV_NIE_CC,CMY0_IV_NIE_MI,CMY0_IV_NIE_EM,
                              CMY0_IV_NDE_OR,CMY0_IV_NDE_CC,CMY0_IV_NDE_MI,CMY0_IV_NDE_EM,
                              CMBY0_IV_NIE_OR,CMBY0_IV_NIE_CC,CMBY0_IV_NIE_MI,CMBY0_IV_NIE_EM,
                              CMBY0_IV_NDE_OR,CMBY0_IV_NDE_CC,CMBY0_IV_NDE_MI,CMBY0_IV_NDE_EM))

colnames(MNAR0_IV) <- c("bias","effect","method","case")
MNAR0_IV$bias<-as.numeric(MNAR0_IV$bias)
MNAR0_IV$case <- factor(MNAR0_IV$case, levels=c("A.IV (0)","B.IV (0)","C.IV (0)","D.IV (0)"))
MNAR0_IV$effect <- factor(MNAR0_IV$effect, levels=c("NIE","NDE"))
MNAR0_IV$method <- factor(MNAR0_IV$method, levels=c("CC","MI","EM","OR"))

#purpose: manually set the y-axis tick values
count <- 0
breaks_fun <- function(x) {
  count <<- count + 1L
  switch(
    count,
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10),
    seq(-200, 600, 10)
  )
}

MNAR0<-rbind(MNAR0_I,MNAR0_II,MNAR0_III,MNAR0_IV)
MNAR0$label <- paste0(MNAR0$method,'\n',MNAR0$effect)
MNAR0$label <- factor(MNAR0$label, levels=c("CC\nNIE","MI\nNIE","EM\nNIE","OR\nNIE","CC\nNDE","MI\nNDE","EM\nNDE","OR\nNDE"))
ggplot(MNAR0, aes(x=label, y=bias*100, color=method)) + geom_boxplot(outlier.shape = 1, outlier.color = NULL) +
  stat_summary(fun="mean", geom="point", shape=5, position = position_dodge2(width = 0.75, preserve = "single")) +
  labs(x = '', y = 'Bias (%)') +
  scale_color_manual(values = c("#0099f8","#E69F00","red3","green4")) +
  geom_hline(yintercept = 0, linetype="dashed") +
  facet_wrap(vars(case), nrow = 4, scales="free") +
  theme_bw() +
  scale_y_continuous(breaks = breaks_fun) +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.8),
        #legend.position = 'bottom',
        legend.position = "none",
        legend.text = element_text(size = 20),
        legend.key.size = unit(2, 'lines'),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank())


#Figure S10

DMBY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/DMBY_I.xlsx', 1)
DMBY_I <- array(as.numeric(unlist(DMBY_I)), dim = c(28, 4, 500))
DMBY_I_OR <- as.data.frame(t(DMBY_I[1:28,1,]))
DMBY_I_EM <- as.data.frame(t(DMBY_I[1:28,4,]))
#oracle
orpar1<-cbind(DMBY_I_OR[1],"ta_02")
colnames(orpar1)<-c("value","par")
orpar2<-cbind(DMBY_I_OR[2],"ta_t2")
colnames(orpar2)<-c("value","par")
orpar3<-cbind(DMBY_I_OR[3],"ta_x2")
colnames(orpar3)<-c("value","par")
orpar4<-cbind(DMBY_I_OR[4],"ta_03")
colnames(orpar4)<-c("value","par")
orpar5<-cbind(DMBY_I_OR[5],"ta_t3")
colnames(orpar5)<-c("value","par")
orpar6<-cbind(DMBY_I_OR[6],"ta_x3")
colnames(orpar6)<-c("value","par")
orpar7<-cbind(DMBY_I_OR[7],"tb_0")
colnames(orpar7)<-c("value","par")
orpar8<-cbind(DMBY_I_OR[8],"tb_m2")
colnames(orpar8)<-c("value","par")
orpar9<-cbind(DMBY_I_OR[9],"tb_m3")
colnames(orpar9)<-c("value","par")
orpar10<-cbind(DMBY_I_OR[10],"tb_t")
colnames(orpar10)<-c("value","par")
orpar11<-cbind(DMBY_I_OR[11],"tb_x")
colnames(orpar11)<-c("value","par")
orpar12<-cbind(DMBY_I_OR[12],"tb_mt2")
colnames(orpar12)<-c("value","par")
orpar13<-cbind(DMBY_I_OR[13],"tb_mt3")
colnames(orpar13)<-c("value","par")
orpar14<-cbind(DMBY_I_OR[14],"tc_0")
colnames(orpar14)<-c("value","par")
orpar15<-cbind(DMBY_I_OR[15],"tc_m2")
colnames(orpar15)<-c("value","par")
orpar16<-cbind(DMBY_I_OR[16],"tc_m3")
colnames(orpar16)<-c("value","par")
orpar17<-cbind(DMBY_I_OR[17],"tc_t")
colnames(orpar17)<-c("value","par")
orpar18<-cbind(DMBY_I_OR[18],"tc_x")
colnames(orpar18)<-c("value","par")
orpar19<-cbind(DMBY_I_OR[19],"tNIE")
colnames(orpar19)<-c("value","par")
orpar20<-cbind(DMBY_I_OR[20],"tNDE")
colnames(orpar20)<-c("value","par")
orpar<-rbind(orpar1,orpar2,orpar3,orpar4,orpar5,orpar6,orpar7,orpar8,orpar9,orpar10,orpar11,orpar12,orpar13,orpar14,orpar15,orpar16,orpar17,orpar18,orpar19,orpar20)
orpar$par<-factor(orpar$par, 
                  levels=c("ta_02","ta_t2","ta_x2","ta_03","ta_t3","ta_x3","tb_0","tb_m2","tb_m3","tb_t","tb_x","tb_mt2","tb_mt3","tc_0","tc_m2","tc_m3","tc_t","tc_x","tNIE","tNDE"), 
                  labels=c(expression(OR:alpha["10"]), expression(OR:alpha["1t"]), expression(OR:alpha["1x"]),
                           expression(OR:alpha["20"]), expression(OR:alpha["2t"]), expression(OR:alpha["2x"]),
                           expression(OR:beta["0"]), expression(OR:beta["m1"]), expression(OR:beta["m2"]), expression(OR:beta["t"]), expression(OR:beta["x"]), expression(OR:beta["mt1"]), expression(OR:beta["mt2"]),
                           expression(OR:lambda["0"]), expression(OR:lambda["m1"]), expression(OR:lambda["m2"]), expression(OR:lambda["t"]), expression(OR:lambda["x"]),
                           expression(OR:NIE), expression(OR:NDE)))
#em
empar1<-cbind(DMBY_I_EM[1],"ema_02")
colnames(empar1)<-c("value","par")
empar2<-cbind(DMBY_I_EM[2],"ema_t2")
colnames(empar2)<-c("value","par")
empar3<-cbind(DMBY_I_EM[3],"ema_x2")
colnames(empar3)<-c("value","par")
empar4<-cbind(DMBY_I_EM[4],"ema_03")
colnames(empar4)<-c("value","par")
empar5<-cbind(DMBY_I_EM[5],"ema_t3")
colnames(empar5)<-c("value","par")
empar6<-cbind(DMBY_I_EM[6],"ema_x3")
colnames(empar6)<-c("value","par")
empar7<-cbind(DMBY_I_EM[7],"emb_0")
colnames(empar7)<-c("value","par")
empar8<-cbind(DMBY_I_EM[8],"emb_m2")
colnames(empar8)<-c("value","par")
empar9<-cbind(DMBY_I_EM[9],"emb_m3")
colnames(empar9)<-c("value","par")
empar10<-cbind(DMBY_I_EM[10],"emb_t")
colnames(empar10)<-c("value","par")
empar11<-cbind(DMBY_I_EM[11],"emb_x")
colnames(empar11)<-c("value","par")
empar12<-cbind(DMBY_I_EM[12],"emb_mt2")
colnames(empar12)<-c("value","par")
empar13<-cbind(DMBY_I_EM[13],"emb_mt3")
colnames(empar13)<-c("value","par")
empar14<-cbind(DMBY_I_EM[14],"emc_0")
colnames(empar14)<-c("value","par")
empar15<-cbind(DMBY_I_EM[15],"emc_m2")
colnames(empar15)<-c("value","par")
empar16<-cbind(DMBY_I_EM[16],"emc_m3")
colnames(empar16)<-c("value","par")
empar17<-cbind(DMBY_I_EM[17],"emc_t")
colnames(empar17)<-c("value","par")
empar18<-cbind(DMBY_I_EM[18],"emc_x")
colnames(empar18)<-c("value","par")
empar19<-cbind(DMBY_I_EM[19],"emNIE")
colnames(empar19)<-c("value","par")
empar20<-cbind(DMBY_I_EM[20],"emNDE")
colnames(empar20)<-c("value","par")
empar<-rbind(empar1,empar2,empar3,empar4,empar5,empar6,empar7,empar8,empar9,empar10,empar11,empar12,empar13,empar14,empar15,empar16,empar17,empar18,empar19,empar20)
empar$par<-factor(empar$par, 
                  levels=c("ema_02","ema_t2","ema_x2","ema_03","ema_t3","ema_x3","emb_0","emb_m2","emb_m3","emb_t","emb_x","emb_mt2","emb_mt3","emc_0","emc_m2","emc_m3","emc_t","emc_x","emNIE","emNDE"), 
                  labels=c(expression(EM:alpha["10"]), expression(EM:alpha["1t"]), expression(EM:alpha["1x"]),
                           expression(EM:alpha["20"]), expression(EM:alpha["2t"]), expression(EM:alpha["2x"]),
                           expression(EM:beta["0"]), expression(EM:beta["m1"]), expression(EM:beta["m2"]), expression(EM:beta["t"]), expression(EM:beta["x"]), expression(EM:beta["mt1"]), expression(EM:beta["mt2"]),
                           expression(EM:lambda["0"]), expression(EM:lambda["m1"]), expression(EM:lambda["m2"]), expression(EM:lambda["t"]), expression(EM:lambda["x"]),
                           expression(EM:NIE), expression(EM:NDE)))
#combinded
par <- rbind(orpar,empar)
#true value of the parameter
ref <- data.frame(par = c("ta_02","ta_t2","ta_x2","ta_03","ta_t3","ta_x3","tb_0","tb_m2","tb_m3","tb_t","tb_x","tb_mt2","tb_mt3","tc_0","tc_m2","tc_m3","tc_t","tc_x","tNIE","tNDE",
                          "ema_02","ema_t2","ema_x2","ema_03","ema_t3","ema_x3","emb_0","emb_m2","emb_m3","emb_t","emb_x","emb_mt2","emb_mt3","emc_0","emc_m2","emc_m3","emc_t","emc_x","emNIE","emNDE"), 
                  int = c(0,1,1,0,1,1,0,1,-1,1,1,1,-1,0,2,2,1,1,-0.01034228,0.13788584,0,1,1,0,1,1,0,1,-1,1,1,1,-1,0,2,2,1,1,-0.01034228,0.13788584))
ref$par <- factor(ref$par, 
                  levels=c("ta_02","ta_t2","ta_x2","ta_03","ta_t3","ta_x3","tb_0","tb_m2","tb_m3","tb_t","tb_x","tb_mt2","tb_mt3","tc_0","tc_m2","tc_m3","tc_t","tc_x","tNIE","tNDE",
                           "ema_02","ema_t2","ema_x2","ema_03","ema_t3","ema_x3","emb_0","emb_m2","emb_m3","emb_t","emb_x","emb_mt2","emb_mt3","emc_0","emc_m2","emc_m3","emc_t","emc_x","emNIE","emNDE"), 
                  labels=c(expression(OR:alpha["10"]), expression(OR:alpha["1t"]), expression(OR:alpha["1x"]),
                           expression(OR:alpha["20"]), expression(OR:alpha["2t"]), expression(OR:alpha["2x"]),
                           expression(OR:beta["0"]), expression(OR:beta["m1"]), expression(OR:beta["m2"]), expression(OR:beta["t"]), expression(OR:beta["x"]), expression(OR:beta["mt1"]), expression(OR:beta["mt2"]),
                           expression(OR:lambda["0"]), expression(OR:lambda["m1"]), expression(OR:lambda["m2"]), expression(OR:lambda["t"]), expression(OR:lambda["x"]),
                           expression(OR:NIE), expression(OR:NDE), 
                           expression(EM:alpha["10"]), expression(EM:alpha["1t"]), expression(EM:alpha["1x"]),
                           expression(EM:alpha["20"]), expression(EM:alpha["2t"]), expression(EM:alpha["2x"]),
                           expression(EM:beta["0"]), expression(EM:beta["m1"]), expression(EM:beta["m2"]), expression(EM:beta["t"]), expression(EM:beta["x"]), expression(EM:beta["mt1"]), expression(EM:beta["mt2"]),
                           expression(EM:lambda["0"]), expression(EM:lambda["m1"]), expression(EM:lambda["m2"]), expression(EM:lambda["t"]), expression(EM:lambda["x"]),
                           expression(EM:NIE), expression(EM:NDE)))

ref$type <- "True value of the parameter"
#mean of the parameter estimates
ref_est <- par %>% group_by(par) %>% summarise_at(vars(value), list(int = mean))
ref_est$type <- "Mean of the parameter estimates"
#combinded
ref1 <- rbind(ref,ref_est)
ref1$type <- factor(ref1$type, levels=c("True value of the parameter","Mean of the parameter estimates"))

ggplot(par, aes(x=value, fill=par)) + 
  geom_density(aes(y = after_stat(scaled)), alpha = 0.5) +
  scale_fill_manual(values = c(rep("green4",20), rep("red3",20)), guide = "none") +
  labs(x = 'Estimates', y = 'Density') +
  facet_wrap(vars(par), nrow = 10, scales="free", labeller = label_parsed) +
  #scale_x_continuous(breaks = breaks_fun) +
  geom_vline(data = ref1, aes(xintercept = int, linetype = type, color = par), linewidth = 1) +
  scale_color_manual(values = c(rep("green4",20), rep("red3",20)), guide = "none") +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.8),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.key.size = unit(2, 'lines'),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin)


#Figure S11

DMBY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/DMBY_IV.xlsx', 1)
DMBY_IV <- array(as.numeric(unlist(DMBY_IV)), dim = c(35, 4, 500))
DMBY_IV_OR <- as.data.frame(t(DMBY_IV[1:35,1,]))
DMBY_IV_EM <- as.data.frame(t(DMBY_IV[1:35,4,]))
#oracle
orpar1<-cbind(DMBY_IV_OR[1],"ta_02")
colnames(orpar1)<-c("value","par")
orpar2<-cbind(DMBY_IV_OR[2],"ta_t2")
colnames(orpar2)<-c("value","par")
orpar3<-cbind(DMBY_IV_OR[3],"ta_x2")
colnames(orpar3)<-c("value","par")
orpar4<-cbind(DMBY_IV_OR[4],"ta_03")
colnames(orpar4)<-c("value","par")
orpar5<-cbind(DMBY_IV_OR[5],"ta_t3")
colnames(orpar5)<-c("value","par")
orpar6<-cbind(DMBY_IV_OR[6],"ta_x3")
colnames(orpar6)<-c("value","par")
orpar7<-cbind(DMBY_IV_OR[7],"tb_0")
colnames(orpar7)<-c("value","par")
orpar8<-cbind(DMBY_IV_OR[8],"tb_m2")
colnames(orpar8)<-c("value","par")
orpar9<-cbind(DMBY_IV_OR[9],"tb_m3")
colnames(orpar9)<-c("value","par")
orpar10<-cbind(DMBY_IV_OR[10],"tb_t")
colnames(orpar10)<-c("value","par")
orpar11<-cbind(DMBY_IV_OR[11],"tb_x")
colnames(orpar11)<-c("value","par")
orpar12<-cbind(DMBY_IV_OR[12],"tb_mt2")
colnames(orpar12)<-c("value","par")
orpar13<-cbind(DMBY_IV_OR[13],"tb_mt3")
colnames(orpar13)<-c("value","par")
orpar14<-cbind(DMBY_IV_OR[14],"tc_0")
colnames(orpar14)<-c("value","par")
orpar15<-cbind(DMBY_IV_OR[15],"tc_m2")
colnames(orpar15)<-c("value","par")
orpar16<-cbind(DMBY_IV_OR[16],"tc_m3")
colnames(orpar16)<-c("value","par")
orpar17<-cbind(DMBY_IV_OR[17],"tc_t")
colnames(orpar17)<-c("value","par")
orpar18<-cbind(DMBY_IV_OR[18],"tc_x")
colnames(orpar18)<-c("value","par")
orpar19<-cbind(DMBY_IV_OR[19],"td_0")
colnames(orpar19)<-c("value","par")
orpar20<-cbind(DMBY_IV_OR[20],"td_m2")
colnames(orpar20)<-c("value","par")
orpar21<-cbind(DMBY_IV_OR[21],"td_m3")
colnames(orpar21)<-c("value","par")
orpar22<-cbind(DMBY_IV_OR[22],"td_t")
colnames(orpar22)<-c("value","par")
orpar23<-cbind(DMBY_IV_OR[23],"td_x")
colnames(orpar23)<-c("value","par")
orpar24<-cbind(DMBY_IV_OR[24],"tNIE")
colnames(orpar24)<-c("value","par")
orpar25<-cbind(DMBY_IV_OR[25],"tNDE")
colnames(orpar25)<-c("value","par")
orpar<-rbind(orpar1,orpar2,orpar3,orpar4,orpar5,orpar6,orpar7,orpar8,orpar9,orpar10,orpar11,orpar12,orpar13,orpar14,orpar15,orpar16,orpar17,orpar18,orpar19,orpar20,orpar21,orpar22,orpar23,orpar24,orpar25)
orpar$par<-factor(orpar$par, 
                  levels=c("ta_02","ta_t2","ta_x2","ta_03","ta_t3","ta_x3","tb_0","tb_m2","tb_m3","tb_t","tb_x","tb_mt2","tb_mt3","tc_0","tc_m2","tc_m3","tc_t","tc_x","td_0","td_m2","td_m3","td_t","td_x","tNIE","tNDE"), 
                  labels=c(expression(OR:alpha["10"]), expression(OR:alpha["1t"]), expression(OR:alpha["1x"]),
                           expression(OR:alpha["20"]), expression(OR:alpha["2t"]), expression(OR:alpha["2x"]),
                           expression(OR:beta["0"]), expression(OR:beta["m1"]), expression(OR:beta["m2"]), expression(OR:beta["t"]), expression(OR:beta["x"]), expression(OR:beta["mt1"]), expression(OR:beta["mt2"]),
                           expression(OR:lambda["0"]), expression(OR:lambda["m1"]), expression(OR:lambda["m2"]), expression(OR:lambda["t"]), expression(OR:lambda["x"]),
                           expression(OR:gamma["0"]), expression(OR:gamma["m1"]), expression(OR:gamma["m2"]), expression(OR:gamma["t"]), expression(OR:gamma["x"]),
                           expression(OR:NIE), expression(OR:NDE)))
#em
empar1<-cbind(DMBY_IV_EM[1],"ema_02")
colnames(empar1)<-c("value","par")
empar2<-cbind(DMBY_IV_EM[2],"ema_t2")
colnames(empar2)<-c("value","par")
empar3<-cbind(DMBY_IV_EM[3],"ema_x2")
colnames(empar3)<-c("value","par")
empar4<-cbind(DMBY_IV_EM[4],"ema_03")
colnames(empar4)<-c("value","par")
empar5<-cbind(DMBY_IV_EM[5],"ema_t3")
colnames(empar5)<-c("value","par")
empar6<-cbind(DMBY_IV_EM[6],"ema_x3")
colnames(empar6)<-c("value","par")
empar7<-cbind(DMBY_IV_EM[7],"emb_0")
colnames(empar7)<-c("value","par")
empar8<-cbind(DMBY_IV_EM[8],"emb_m2")
colnames(empar8)<-c("value","par")
empar9<-cbind(DMBY_IV_EM[9],"emb_m3")
colnames(empar9)<-c("value","par")
empar10<-cbind(DMBY_IV_EM[10],"emb_t")
colnames(empar10)<-c("value","par")
empar11<-cbind(DMBY_IV_EM[11],"emb_x")
colnames(empar11)<-c("value","par")
empar12<-cbind(DMBY_IV_EM[12],"emb_mt2")
colnames(empar12)<-c("value","par")
empar13<-cbind(DMBY_IV_EM[13],"emb_mt3")
colnames(empar13)<-c("value","par")
empar14<-cbind(DMBY_IV_EM[14],"emc_0")
colnames(empar14)<-c("value","par")
empar15<-cbind(DMBY_IV_EM[15],"emc_m2")
colnames(empar15)<-c("value","par")
empar16<-cbind(DMBY_IV_EM[16],"emc_m3")
colnames(empar16)<-c("value","par")
empar17<-cbind(DMBY_IV_EM[17],"emc_t")
colnames(empar17)<-c("value","par")
empar18<-cbind(DMBY_IV_EM[18],"emc_x")
colnames(empar18)<-c("value","par")
empar19<-cbind(DMBY_IV_EM[19],"emd_0")
colnames(empar19)<-c("value","par")
empar20<-cbind(DMBY_IV_EM[20],"emd_m2")
colnames(empar20)<-c("value","par")
empar21<-cbind(DMBY_IV_EM[21],"emd_m3")
colnames(empar21)<-c("value","par")
empar22<-cbind(DMBY_IV_EM[22],"emd_t")
colnames(empar22)<-c("value","par")
empar23<-cbind(DMBY_IV_EM[23],"emd_x")
colnames(empar23)<-c("value","par")
empar24<-cbind(DMBY_IV_EM[24],"emNIE")
colnames(empar24)<-c("value","par")
empar25<-cbind(DMBY_IV_EM[25],"emNDE")
colnames(empar25)<-c("value","par")
empar<-rbind(empar1,empar2,empar3,empar4,empar5,empar6,empar7,empar8,empar9,empar10,empar11,empar12,empar13,empar14,empar15,empar16,empar17,empar18,empar19,empar20,empar21,empar22,empar23,empar24,empar25)
empar$par<-factor(empar$par, 
                  levels=c("ema_02","ema_t2","ema_x2","ema_03","ema_t3","ema_x3","emb_0","emb_m2","emb_m3","emb_t","emb_x","emb_mt2","emb_mt3","emc_0","emc_m2","emc_m3","emc_t","emc_x","emd_0","emd_m2","emd_m3","emd_t","emd_x","emNIE","emNDE"), 
                  labels=c(expression(EM:alpha["10"]), expression(EM:alpha["1t"]), expression(EM:alpha["1x"]),
                           expression(EM:alpha["20"]), expression(EM:alpha["2t"]), expression(EM:alpha["2x"]),
                           expression(EM:beta["0"]), expression(EM:beta["m1"]), expression(EM:beta["m2"]), expression(EM:beta["t"]), expression(EM:beta["x"]), expression(EM:beta["mt1"]), expression(EM:beta["mt2"]),
                           expression(EM:lambda["0"]), expression(EM:lambda["m1"]), expression(EM:lambda["m2"]), expression(EM:lambda["t"]), expression(EM:lambda["x"]),
                           expression(EM:gamma["0"]), expression(EM:gamma["m1"]), expression(EM:gamma["m2"]), expression(EM:gamma["t"]), expression(EM:gamma["x"]),
                           expression(EM:NIE), expression(EM:NDE)))
#combinded
par <- rbind(orpar,empar)
#true value of the parameter
ref <- data.frame(par = c("ta_02","ta_t2","ta_x2","ta_03","ta_t3","ta_x3","tb_0","tb_m2","tb_m3","tb_t","tb_x","tb_mt2","tb_mt3","tc_0","tc_m2","tc_m3","tc_t","tc_x","td_0","td_m2","td_m3","td_t","td_x","tNIE","tNDE",
                          "ema_02","ema_t2","ema_x2","ema_03","ema_t3","ema_x3","emb_0","emb_m2","emb_m3","emb_t","emb_x","emb_mt2","emb_mt3","emc_0","emc_m2","emc_m3","emc_t","emc_x","emd_0","emd_m2","emd_m3","emd_t","emd_x","emNIE","emNDE"), 
                  int = c(0,1,1,0,1,1,0,1,-1,1,1,1,-1,0,2,2,1,1,0,2,2,1,1,-0.01034228,0.13788584,0,1,1,0,1,1,0,1,-1,1,1,1,-1,0,2,2,1,1,0,2,2,1,1,-0.01034228,0.13788584))
ref$par <- factor(ref$par, 
                  levels=c("ta_02","ta_t2","ta_x2","ta_03","ta_t3","ta_x3","tb_0","tb_m2","tb_m3","tb_t","tb_x","tb_mt2","tb_mt3","tc_0","tc_m2","tc_m3","tc_t","tc_x","td_0","td_m2","td_m3","td_t","td_x","tNIE","tNDE",
                           "ema_02","ema_t2","ema_x2","ema_03","ema_t3","ema_x3","emb_0","emb_m2","emb_m3","emb_t","emb_x","emb_mt2","emb_mt3","emc_0","emc_m2","emc_m3","emc_t","emc_x","emd_0","emd_m2","emd_m3","emd_t","emd_x","emNIE","emNDE"), 
                  labels=c(expression(OR:alpha["10"]), expression(OR:alpha["1t"]), expression(OR:alpha["1x"]),
                           expression(OR:alpha["20"]), expression(OR:alpha["2t"]), expression(OR:alpha["2x"]),
                           expression(OR:beta["0"]), expression(OR:beta["m1"]), expression(OR:beta["m2"]), expression(OR:beta["t"]), expression(OR:beta["x"]), expression(OR:beta["mt1"]), expression(OR:beta["mt2"]),
                           expression(OR:lambda["0"]), expression(OR:lambda["m1"]), expression(OR:lambda["m2"]), expression(OR:lambda["t"]), expression(OR:lambda["x"]),
                           expression(OR:gamma["0"]), expression(OR:gamma["m1"]), expression(OR:gamma["m2"]), expression(OR:gamma["t"]), expression(OR:gamma["x"]),
                           expression(OR:NIE), expression(OR:NDE), 
                           expression(EM:alpha["10"]), expression(EM:alpha["1t"]), expression(EM:alpha["1x"]),
                           expression(EM:alpha["20"]), expression(EM:alpha["2t"]), expression(EM:alpha["2x"]),
                           expression(EM:beta["0"]), expression(EM:beta["m1"]), expression(EM:beta["m2"]), expression(EM:beta["t"]), expression(EM:beta["x"]), expression(EM:beta["mt1"]), expression(EM:beta["mt2"]),
                           expression(EM:lambda["0"]), expression(EM:lambda["m1"]), expression(EM:lambda["m2"]), expression(EM:lambda["t"]), expression(EM:lambda["x"]),
                           expression(EM:gamma["0"]), expression(EM:gamma["m1"]), expression(EM:gamma["m2"]), expression(EM:gamma["t"]), expression(EM:gamma["x"]),
                           expression(EM:NIE), expression(EM:NDE)))

ref$type <- "True value of the parameter"
#mean of the parameter estimates
ref_est <- par %>% group_by(par) %>% summarise_at(vars(value), list(int = mean))
ref_est$type <- "Mean of the parameter estimates"
#combinded
ref1 <- rbind(ref,ref_est)
ref1$type <- factor(ref1$type, levels=c("True value of the parameter","Mean of the parameter estimates"))

ggplot(par, aes(x=value, fill=par)) + 
  geom_density(aes(y = after_stat(scaled)), alpha = 0.5) +
  scale_fill_manual(values = c(rep("green4",25), rep("red3",25)), guide = "none") +
  labs(x = 'Estimates', y = 'Density') +
  facet_wrap(vars(par), nrow = 10, scales="free", labeller = label_parsed) +
  #scale_x_continuous(breaks = breaks_fun) +
  geom_vline(data = ref1, aes(xintercept = int, linetype = type, color = par), linewidth = 1) +
  scale_color_manual(values = c(rep("green4",25), rep("red3",25)), guide = "none") +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.8),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.key.size = unit(2, 'lines'),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin)

#Figure S12

CMBY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_I.xlsx', 1)
CMBY_I <- array(as.numeric(unlist(CMBY_I)), dim = c(23, 4, 500))
CMBY_I_OR <- as.data.frame(t(CMBY_I[1:23,1,]))
CMBY_I_EM <- as.data.frame(t(CMBY_I[1:23,4,]))
#oracle
orpar1<-cbind(CMBY_I_OR[1],"ta_0")
colnames(orpar1)<-c("value","par")
orpar2<-cbind(CMBY_I_OR[2],"ta_t")
colnames(orpar2)<-c("value","par")
orpar3<-cbind(CMBY_I_OR[3],"ta_x")
colnames(orpar3)<-c("value","par")
orpar4<-cbind(CMBY_I_OR[4],"tsd_m")
colnames(orpar4)<-c("value","par")
orpar5<-cbind(CMBY_I_OR[5],"tb_0")
colnames(orpar5)<-c("value","par")
orpar6<-cbind(CMBY_I_OR[6],"tb_m")
colnames(orpar6)<-c("value","par")
orpar7<-cbind(CMBY_I_OR[7],"tb_t")
colnames(orpar7)<-c("value","par")
orpar8<-cbind(CMBY_I_OR[8],"tb_x")
colnames(orpar8)<-c("value","par")
orpar9<-cbind(CMBY_I_OR[9],"tb_mt")
colnames(orpar9)<-c("value","par")
orpar10<-cbind(CMBY_I_OR[10],"tc_0")
colnames(orpar10)<-c("value","par")
orpar11<-cbind(CMBY_I_OR[11],"tc_m")
colnames(orpar11)<-c("value","par")
orpar12<-cbind(CMBY_I_OR[12],"tc_t")
colnames(orpar12)<-c("value","par")
orpar13<-cbind(CMBY_I_OR[13],"tc_x")
colnames(orpar13)<-c("value","par")
orpar14<-cbind(CMBY_I_OR[14],"tNIE")
colnames(orpar14)<-c("value","par")
orpar15<-cbind(CMBY_I_OR[15],"tNDE")
colnames(orpar15)<-c("value","par")
orpar<-rbind(orpar1,orpar2,orpar3,orpar4,orpar5,orpar6,orpar7,orpar8,orpar9,orpar10,orpar11,orpar12,orpar13,orpar14,orpar15)
orpar$par<-factor(orpar$par, 
                  levels = c("ta_0","ta_t","ta_x","tsd_m","tb_0","tb_m","tb_t","tb_x","tb_mt","tc_0","tc_m","tc_t","tc_x","tNIE","tNDE"), 
                  labels = c(expression(OR:alpha["0"]), expression(OR:alpha["t"]), expression(OR:alpha["x"]), expression(OR:sigma["m"]),
                             expression(OR:beta["0"]), expression(OR:beta["m"]), expression(OR:beta["t"]), expression(OR:beta["x"]), expression(OR:beta["mt"]),
                             expression(OR:lambda["0"]), expression(OR:lambda["m"]), expression(OR:lambda["t"]), expression(OR:lambda["x"]),
                             expression(OR:NIE), expression(OR:NDE)))
#em
empar1<-cbind(CMBY_I_EM[1],"ema_0")
colnames(empar1)<-c("value","par")
empar2<-cbind(CMBY_I_EM[2],"ema_t")
colnames(empar2)<-c("value","par")
empar3<-cbind(CMBY_I_EM[3],"ema_x")
colnames(empar3)<-c("value","par")
empar4<-cbind(CMBY_I_EM[4],"emsd_m")
colnames(empar4)<-c("value","par")
empar5<-cbind(CMBY_I_EM[5],"emb_0")
colnames(empar5)<-c("value","par")
empar6<-cbind(CMBY_I_EM[6],"emb_m")
colnames(empar6)<-c("value","par")
empar7<-cbind(CMBY_I_EM[7],"emb_t")
colnames(empar7)<-c("value","par")
empar8<-cbind(CMBY_I_EM[8],"emb_x")
colnames(empar8)<-c("value","par")
empar9<-cbind(CMBY_I_EM[9],"emb_mt")
colnames(empar9)<-c("value","par")
empar10<-cbind(CMBY_I_EM[10],"emc_0")
colnames(empar10)<-c("value","par")
empar11<-cbind(CMBY_I_EM[11],"emc_m")
colnames(empar11)<-c("value","par")
empar12<-cbind(CMBY_I_EM[12],"emc_t")
colnames(empar12)<-c("value","par")
empar13<-cbind(CMBY_I_EM[13],"emc_x")
colnames(empar13)<-c("value","par")
empar14<-cbind(CMBY_I_EM[14],"emNIE")
colnames(empar14)<-c("value","par")
empar15<-cbind(CMBY_I_EM[15],"emNDE")
colnames(empar15)<-c("value","par")
empar<-rbind(empar1,empar2,empar3,empar4,empar5,empar6,empar7,empar8,empar9,empar10,empar11,empar12,empar13,empar14,empar15)
empar$par<-factor(empar$par, 
                  levels = c("ema_0","ema_t","ema_x","emsd_m","emb_0","emb_m","emb_t","emb_x","emb_mt","emc_0","emc_m","emc_t","emc_x","emNIE","emNDE"), 
                  labels = c(expression(EM:alpha["0"]), expression(EM:alpha["t"]), expression(EM:alpha["x"]), expression(EM:sigma["m"]),
                             expression(EM:beta["0"]), expression(EM:beta["m"]), expression(EM:beta["t"]), expression(EM:beta["x"]), expression(EM:beta["mt"]), 
                             expression(EM:lambda["0"]), expression(EM:lambda["m"]), expression(EM:lambda["t"]), expression(EM:lambda["x"]),
                             expression(EM:NIE), expression(EM:NDE)))
#combinded
par <- rbind(orpar,empar)
#true value of the parameter
ref <- data.frame(par = c("ta_0","ta_t","ta_x","tsd_m","tb_0","tb_m","tb_t","tb_x","tb_mt","tc_0","tc_m","tc_t","tc_x","tNIE","tNDE",
                          "ema_0","ema_t","ema_x","emsd_m","emb_0","emb_m","emb_t","emb_x","emb_mt","emc_0","emc_m","emc_t","emc_x","emNIE","emNDE"), 
                  int = c(0,1,1,1,0,1,1,1,1,1.4,1,1,1,0.17247242,0.09797326,0,1,1,1,0,1,1,1,1,1.4,1,1,1,0.17247242,0.09797326))
ref$par <- factor(ref$par, 
                  levels = c("ta_0","ta_t","ta_x","tsd_m","tb_0","tb_m","tb_t","tb_x","tb_mt","tc_0","tc_m","tc_t","tc_x","tNIE","tNDE",
                             "ema_0","ema_t","ema_x","emsd_m","emb_0","emb_m","emb_t","emb_x","emb_mt","emc_0","emc_m","emc_t","emc_x","emNIE","emNDE"), 
                  labels = c(expression(OR:alpha["0"]), expression(OR:alpha["t"]), expression(OR:alpha["x"]),
                             expression(OR:sigma["m"]), 
                             expression(OR:beta["0"]), expression(OR:beta["m"]), expression(OR:beta["t"]), expression(OR:beta["x"]), expression(OR:beta["mt"]), 
                             expression(OR:lambda["0"]), expression(OR:lambda["m"]), expression(OR:lambda["t"]), expression(OR:lambda["x"]),
                             expression(OR:NIE), expression(OR:NDE), 
                             expression(EM:alpha["0"]), expression(EM:alpha["t"]), expression(EM:alpha["x"]),
                             expression(EM:sigma["m"]),
                             expression(EM:beta["0"]), expression(EM:beta["m"]), expression(EM:beta["t"]), expression(EM:beta["x"]), expression(EM:beta["mt"]), 
                             expression(EM:lambda["0"]), expression(EM:lambda["m"]), expression(EM:lambda["t"]), expression(EM:lambda["x"]),
                             expression(EM:NIE), expression(EM:NDE)))
ref$type <- "True value of the parameter"
#mean of the parameter estimates
ref_est <- par %>% group_by(par) %>% summarise_at(vars(value), list(int = mean))
ref_est$type <- "Mean of the parameter estimates"
#combinded
ref1 <- rbind(ref,ref_est)
ref1$type <- factor(ref1$type, levels=c("True value of the parameter","Mean of the parameter estimates"))

ggplot(par, aes(x=value, fill=par)) + 
  geom_density(aes(y = after_stat(scaled)), alpha = 0.5) +
  scale_fill_manual(values = c(rep("green4",15), rep("red3",15)), guide = "none") +
  labs(x = 'Estimates', y = 'Density') +
  facet_wrap(vars(par), nrow = 6, scales="free", labeller = label_parsed) +
  #scale_x_continuous(breaks = breaks_fun) +
  geom_vline(data = ref1, aes(xintercept = int, linetype = type, color = par), linewidth = 1) +
  scale_color_manual(values = c(rep("green4",15), rep("red3",15)), guide = "none") +
  scale_linetype_manual(values = c("solid","dashed")) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.8),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.key.size = unit(2, 'lines'),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin)




