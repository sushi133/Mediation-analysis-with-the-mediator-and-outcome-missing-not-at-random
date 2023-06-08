library(xlsx)
library(ggplot2)
library(dplyr)

#MNAR I
BMY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_I.xlsx', 1)
BMCY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_I.xlsx', 1)
CMY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_I.xlsx', 1)
CMBY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_I.xlsx', 1)

BMY_I<-array(as.numeric(unlist(BMY_I)), dim = c(18, 4, 500))
BMCY_I<-array(as.numeric(unlist(BMCY_I)), dim = c(19, 4, 500))
CMY_I<-array(as.numeric(unlist(CMY_I)), dim = c(20, 4, 500))
CMBY_I<-array(as.numeric(unlist(CMBY_I)), dim = c(19, 4, 500))

BMY_I_NIE_OR<-cbind(BMY_I[c(13),1,],'NIE','OR','A.I')
BMY_I_NIE_CC<-cbind(BMY_I[c(13),2,],'NIE','CC','A.I')
BMY_I_NIE_MI<-cbind(BMY_I[c(13),3,],'NIE','MI','A.I')
BMY_I_NIE_EM<-cbind(BMY_I[c(13),4,],'NIE','EM','A.I')
BMY_I_NDE_OR<-cbind(BMY_I[c(14),1,],'NDE','OR','A.I')
BMY_I_NDE_CC<-cbind(BMY_I[c(14),2,],'NDE','CC','A.I')
BMY_I_NDE_MI<-cbind(BMY_I[c(14),3,],'NDE','MI','A.I')
BMY_I_NDE_EM<-cbind(BMY_I[c(14),4,],'NDE','EM','A.I')

BMCY_I_NIE_OR<-cbind(BMCY_I[c(14),1,],'NIE','OR','B.I')
BMCY_I_NIE_CC<-cbind(BMCY_I[c(14),2,],'NIE','CC','B.I')
BMCY_I_NIE_MI<-cbind(BMCY_I[c(14),3,],'NIE','MI','B.I')
BMCY_I_NIE_EM<-cbind(BMCY_I[c(14),4,],'NIE','EM','B.I')
BMCY_I_NDE_OR<-cbind(BMCY_I[c(15),1,],'NDE','OR','B.I')
BMCY_I_NDE_CC<-cbind(BMCY_I[c(15),2,],'NDE','CC','B.I')
BMCY_I_NDE_MI<-cbind(BMCY_I[c(15),3,],'NDE','MI','B.I')
BMCY_I_NDE_EM<-cbind(BMCY_I[c(15),4,],'NDE','EM','B.I')

CMY_I_NIE_OR<-cbind(CMY_I[c(15),1,],'NIE','OR','C.I')
CMY_I_NIE_CC<-cbind(CMY_I[c(15),2,],'NIE','CC','C.I')
CMY_I_NIE_MI<-cbind(CMY_I[c(15),3,],'NIE','MI','C.I')
CMY_I_NIE_EM<-cbind(CMY_I[c(15),4,],'NIE','EM','C.I')
CMY_I_NDE_OR<-cbind(CMY_I[c(16),1,],'NDE','OR','C.I')
CMY_I_NDE_CC<-cbind(CMY_I[c(16),2,],'NDE','CC','C.I')
CMY_I_NDE_MI<-cbind(CMY_I[c(16),3,],'NDE','MI','C.I')
CMY_I_NDE_EM<-cbind(CMY_I[c(16),4,],'NDE','EM','C.I')

CMBY_I_NIE_OR<-cbind(CMBY_I[c(14),1,],'NIE','OR','D.I')
CMBY_I_NIE_CC<-cbind(CMBY_I[c(14),2,],'NIE','CC','D.I')
CMBY_I_NIE_MI<-cbind(CMBY_I[c(14),3,],'NIE','MI','D.I')
CMBY_I_NIE_EM<-cbind(CMBY_I[c(14),4,],'NIE','EM','D.I')
CMBY_I_NDE_OR<-cbind(CMBY_I[c(15),1,],'NDE','OR','D.I')
CMBY_I_NDE_CC<-cbind(CMBY_I[c(15),2,],'NDE','CC','D.I')
CMBY_I_NDE_MI<-cbind(CMBY_I[c(15),3,],'NDE','MI','D.I')
CMBY_I_NDE_EM<-cbind(CMBY_I[c(15),4,],'NDE','EM','D.I')

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

BMY_II<-array(as.numeric(unlist(BMY_II)), dim = c(24, 4, 500))
BMCY_II<-array(as.numeric(unlist(BMCY_II)), dim = c(25, 4, 500))
CMY_II<-array(as.numeric(unlist(CMY_II)), dim = c(26, 4, 500))
CMBY_II<-array(as.numeric(unlist(CMBY_II)), dim = c(25, 4, 500))

BMY_II_NIE_OR<-cbind(BMY_II[c(17),1,],'NIE','OR','A.II')
BMY_II_NIE_CC<-cbind(BMY_II[c(17),2,],'NIE','CC','A.II')
BMY_II_NIE_MI<-cbind(BMY_II[c(17),3,],'NIE','MI','A.II')
BMY_II_NIE_EM<-cbind(BMY_II[c(17),4,],'NIE','EM','A.II')
BMY_II_NDE_OR<-cbind(BMY_II[c(18),1,],'NDE','OR','A.II')
BMY_II_NDE_CC<-cbind(BMY_II[c(18),2,],'NDE','CC','A.II')
BMY_II_NDE_MI<-cbind(BMY_II[c(18),3,],'NDE','MI','A.II')
BMY_II_NDE_EM<-cbind(BMY_II[c(18),4,],'NDE','EM','A.II')

BMCY_II_NIE_OR<-cbind(BMCY_II[c(18),1,],'NIE','OR','B.II')
BMCY_II_NIE_CC<-cbind(BMCY_II[c(18),2,],'NIE','CC','B.II')
BMCY_II_NIE_MI<-cbind(BMCY_II[c(18),3,],'NIE','MI','B.II')
BMCY_II_NIE_EM<-cbind(BMCY_II[c(18),4,],'NIE','EM','B.II')
BMCY_II_NDE_OR<-cbind(BMCY_II[c(19),1,],'NDE','OR','B.II')
BMCY_II_NDE_CC<-cbind(BMCY_II[c(19),2,],'NDE','CC','B.II')
BMCY_II_NDE_MI<-cbind(BMCY_II[c(19),3,],'NDE','MI','B.II')
BMCY_II_NDE_EM<-cbind(BMCY_II[c(19),4,],'NDE','EM','B.II')

CMY_II_NIE_OR<-cbind(CMY_II[c(19),1,],'NIE','OR','C.II')
CMY_II_NIE_CC<-cbind(CMY_II[c(19),2,],'NIE','CC','C.II')
CMY_II_NIE_MI<-cbind(CMY_II[c(19),3,],'NIE','MI','C.II')
CMY_II_NIE_EM<-cbind(CMY_II[c(19),4,],'NIE','EM','C.II')
CMY_II_NDE_OR<-cbind(CMY_II[c(20),1,],'NDE','OR','C.II')
CMY_II_NDE_CC<-cbind(CMY_II[c(20),2,],'NDE','CC','C.II')
CMY_II_NDE_MI<-cbind(CMY_II[c(20),3,],'NDE','MI','C.II')
CMY_II_NDE_EM<-cbind(CMY_II[c(20),4,],'NDE','EM','C.II')

CMBY_II_NIE_OR<-cbind(CMBY_II[c(18),1,],'NIE','OR','D.II')
CMBY_II_NIE_CC<-cbind(CMBY_II[c(18),2,],'NIE','CC','D.II')
CMBY_II_NIE_MI<-cbind(CMBY_II[c(18),3,],'NIE','MI','D.II')
CMBY_II_NIE_EM<-cbind(CMBY_II[c(18),4,],'NIE','EM','D.II')
CMBY_II_NDE_OR<-cbind(CMBY_II[c(19),1,],'NDE','OR','D.II')
CMBY_II_NDE_CC<-cbind(CMBY_II[c(19),2,],'NDE','CC','D.II')
CMBY_II_NDE_MI<-cbind(CMBY_II[c(19),3,],'NDE','MI','D.II')
CMBY_II_NDE_EM<-cbind(CMBY_II[c(19),4,],'NDE','EM','D.II')

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

BMY_III<-array(as.numeric(unlist(BMY_III)), dim = c(24, 4, 500))
BMCY_III<-array(as.numeric(unlist(BMCY_III)), dim = c(25, 4, 500))
CMY_III<-array(as.numeric(unlist(CMY_III)), dim = c(26, 4, 500))
CMBY_III<-array(as.numeric(unlist(CMBY_III)), dim = c(25, 4, 500))

BMY_III_NIE_OR<-cbind(BMY_III[c(17),1,],'NIE','OR','A.III')
BMY_III_NIE_CC<-cbind(BMY_III[c(17),2,],'NIE','CC','A.III')
BMY_III_NIE_MI<-cbind(BMY_III[c(17),3,],'NIE','MI','A.III')
BMY_III_NIE_EM<-cbind(BMY_III[c(17),4,],'NIE','EM','A.III')
BMY_III_NDE_OR<-cbind(BMY_III[c(18),1,],'NDE','OR','A.III')
BMY_III_NDE_CC<-cbind(BMY_III[c(18),2,],'NDE','CC','A.III')
BMY_III_NDE_MI<-cbind(BMY_III[c(18),3,],'NDE','MI','A.III')
BMY_III_NDE_EM<-cbind(BMY_III[c(18),4,],'NDE','EM','A.III')

BMCY_III_NIE_OR<-cbind(BMCY_III[c(18),1,],'NIE','OR','B.III')
BMCY_III_NIE_CC<-cbind(BMCY_III[c(18),2,],'NIE','CC','B.III')
BMCY_III_NIE_MI<-cbind(BMCY_III[c(18),3,],'NIE','MI','B.III')
BMCY_III_NIE_EM<-cbind(BMCY_III[c(18),4,],'NIE','EM','B.III')
BMCY_III_NDE_OR<-cbind(BMCY_III[c(19),1,],'NDE','OR','B.III')
BMCY_III_NDE_CC<-cbind(BMCY_III[c(19),2,],'NDE','CC','B.III')
BMCY_III_NDE_MI<-cbind(BMCY_III[c(19),3,],'NDE','MI','B.III')
BMCY_III_NDE_EM<-cbind(BMCY_III[c(19),4,],'NDE','EM','B.III')

CMY_III_NIE_OR<-cbind(CMY_III[c(19),1,],'NIE','OR','C.III')
CMY_III_NIE_CC<-cbind(CMY_III[c(19),2,],'NIE','CC','C.III')
CMY_III_NIE_MI<-cbind(CMY_III[c(19),3,],'NIE','MI','C.III')
CMY_III_NIE_EM<-cbind(CMY_III[c(19),4,],'NIE','EM','C.III')
CMY_III_NDE_OR<-cbind(CMY_III[c(20),1,],'NDE','OR','C.III')
CMY_III_NDE_CC<-cbind(CMY_III[c(20),2,],'NDE','CC','C.III')
CMY_III_NDE_MI<-cbind(CMY_III[c(20),3,],'NDE','MI','C.III')
CMY_III_NDE_EM<-cbind(CMY_III[c(20),4,],'NDE','EM','C.III')

CMBY_III_NIE_OR<-cbind(CMBY_III[c(18),1,],'NIE','OR','D.III')
CMBY_III_NIE_CC<-cbind(CMBY_III[c(18),2,],'NIE','CC','D.III')
CMBY_III_NIE_MI<-cbind(CMBY_III[c(18),3,],'NIE','MI','D.III')
CMBY_III_NIE_EM<-cbind(CMBY_III[c(18),4,],'NIE','EM','D.III')
CMBY_III_NDE_OR<-cbind(CMBY_III[c(19),1,],'NDE','OR','D.III')
CMBY_III_NDE_CC<-cbind(CMBY_III[c(19),2,],'NDE','CC','D.III')
CMBY_III_NDE_MI<-cbind(CMBY_III[c(19),3,],'NDE','MI','D.III')
CMBY_III_NDE_EM<-cbind(CMBY_III[c(19),4,],'NDE','EM','D.III')


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

BMY_IV<-array(as.numeric(unlist(BMY_IV)), dim = c(24, 4, 500))
BMCY_IV<-array(as.numeric(unlist(BMCY_IV)), dim = c(25, 4, 500))
CMY_IV<-array(as.numeric(unlist(CMY_IV)), dim = c(26, 4, 500))
CMBY_IV<-array(as.numeric(unlist(CMBY_IV)), dim = c(25, 4, 500))

BMY_IV_NIE_OR<-cbind(BMY_IV[c(17),1,],'NIE','OR','A.IV')
BMY_IV_NIE_CC<-cbind(BMY_IV[c(17),2,],'NIE','CC','A.IV')
BMY_IV_NIE_MI<-cbind(BMY_IV[c(17),3,],'NIE','MI','A.IV')
BMY_IV_NIE_EM<-cbind(BMY_IV[c(17),4,],'NIE','EM','A.IV')
BMY_IV_NDE_OR<-cbind(BMY_IV[c(18),1,],'NDE','OR','A.IV')
BMY_IV_NDE_CC<-cbind(BMY_IV[c(18),2,],'NDE','CC','A.IV')
BMY_IV_NDE_MI<-cbind(BMY_IV[c(18),3,],'NDE','MI','A.IV')
BMY_IV_NDE_EM<-cbind(BMY_IV[c(18),4,],'NDE','EM','A.IV')

BMCY_IV_NIE_OR<-cbind(BMCY_IV[c(18),1,],'NIE','OR','B.IV')
BMCY_IV_NIE_CC<-cbind(BMCY_IV[c(18),2,],'NIE','CC','B.IV')
BMCY_IV_NIE_MI<-cbind(BMCY_IV[c(18),3,],'NIE','MI','B.IV')
BMCY_IV_NIE_EM<-cbind(BMCY_IV[c(18),4,],'NIE','EM','B.IV')
BMCY_IV_NDE_OR<-cbind(BMCY_IV[c(19),1,],'NDE','OR','B.IV')
BMCY_IV_NDE_CC<-cbind(BMCY_IV[c(19),2,],'NDE','CC','B.IV')
BMCY_IV_NDE_MI<-cbind(BMCY_IV[c(19),3,],'NDE','MI','B.IV')
BMCY_IV_NDE_EM<-cbind(BMCY_IV[c(19),4,],'NDE','EM','B.IV')

CMY_IV_NIE_OR<-cbind(CMY_IV[c(19),1,],'NIE','OR','C.IV')
CMY_IV_NIE_CC<-cbind(CMY_IV[c(19),2,],'NIE','CC','C.IV')
CMY_IV_NIE_MI<-cbind(CMY_IV[c(19),3,],'NIE','MI','C.IV')
CMY_IV_NIE_EM<-cbind(CMY_IV[c(19),4,],'NIE','EM','C.IV')
CMY_IV_NDE_OR<-cbind(CMY_IV[c(20),1,],'NDE','OR','C.IV')
CMY_IV_NDE_CC<-cbind(CMY_IV[c(20),2,],'NDE','CC','C.IV')
CMY_IV_NDE_MI<-cbind(CMY_IV[c(20),3,],'NDE','MI','C.IV')
CMY_IV_NDE_EM<-cbind(CMY_IV[c(20),4,],'NDE','EM','C.IV')

CMBY_IV_NIE_OR<-cbind(CMBY_IV[c(18),1,],'NIE','OR','D.IV')
CMBY_IV_NIE_CC<-cbind(CMBY_IV[c(18),2,],'NIE','CC','D.IV')
CMBY_IV_NIE_MI<-cbind(CMBY_IV[c(18),3,],'NIE','MI','D.IV')
CMBY_IV_NIE_EM<-cbind(CMBY_IV[c(18),4,],'NIE','EM','D.IV')
CMBY_IV_NDE_OR<-cbind(CMBY_IV[c(19),1,],'NDE','OR','D.IV')
CMBY_IV_NDE_CC<-cbind(CMBY_IV[c(19),2,],'NDE','CC','D.IV')
CMBY_IV_NDE_MI<-cbind(CMBY_IV[c(19),3,],'NDE','MI','D.IV')
CMBY_IV_NDE_EM<-cbind(CMBY_IV[c(19),4,],'NDE','EM','D.IV')


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

#Figure
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


#MNAR I (NULL)
BMY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_I(0).xlsx', 1)
BMCY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_I(0).xlsx', 1)
CMY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_I(0).xlsx', 1)
CMBY0_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_I(0).xlsx', 1)

BMY0_I<-array(as.numeric(unlist(BMY0_I)), dim = c(18, 4, 500))
BMCY0_I<-array(as.numeric(unlist(BMCY0_I)), dim = c(19, 4, 500))
CMY0_I<-array(as.numeric(unlist(CMY0_I)), dim = c(20, 4, 500))
CMBY0_I<-array(as.numeric(unlist(CMBY0_I)), dim = c(19, 4, 500))

BMY0_I_NIE_OR<-cbind(BMY0_I[c(13),1,],'NIE','OR','A.I (0)')
BMY0_I_NIE_CC<-cbind(BMY0_I[c(13),2,],'NIE','CC','A.I (0)')
BMY0_I_NIE_MI<-cbind(BMY0_I[c(13),3,],'NIE','MI','A.I (0)')
BMY0_I_NIE_EM<-cbind(BMY0_I[c(13),4,],'NIE','EM','A.I (0)')
BMY0_I_NDE_OR<-cbind(BMY0_I[c(14),1,],'NDE','OR','A.I (0)')
BMY0_I_NDE_CC<-cbind(BMY0_I[c(14),2,],'NDE','CC','A.I (0)')
BMY0_I_NDE_MI<-cbind(BMY0_I[c(14),3,],'NDE','MI','A.I (0)')
BMY0_I_NDE_EM<-cbind(BMY0_I[c(14),4,],'NDE','EM','A.I (0)')

BMCY0_I_NIE_OR<-cbind(BMCY0_I[c(14),1,],'NIE','OR','B.I (0)')
BMCY0_I_NIE_CC<-cbind(BMCY0_I[c(14),2,],'NIE','CC','B.I (0)')
BMCY0_I_NIE_MI<-cbind(BMCY0_I[c(14),3,],'NIE','MI','B.I (0)')
BMCY0_I_NIE_EM<-cbind(BMCY0_I[c(14),4,],'NIE','EM','B.I (0)')
BMCY0_I_NDE_OR<-cbind(BMCY0_I[c(15),1,],'NDE','OR','B.I (0)')
BMCY0_I_NDE_CC<-cbind(BMCY0_I[c(15),2,],'NDE','CC','B.I (0)')
BMCY0_I_NDE_MI<-cbind(BMCY0_I[c(15),3,],'NDE','MI','B.I (0)')
BMCY0_I_NDE_EM<-cbind(BMCY0_I[c(15),4,],'NDE','EM','B.I (0)')

CMY0_I_NIE_OR<-cbind(CMY0_I[c(15),1,],'NIE','OR','C.I (0)')
CMY0_I_NIE_CC<-cbind(CMY0_I[c(15),2,],'NIE','CC','C.I (0)')
CMY0_I_NIE_MI<-cbind(CMY0_I[c(15),3,],'NIE','MI','C.I (0)')
CMY0_I_NIE_EM<-cbind(CMY0_I[c(15),4,],'NIE','EM','C.I (0)')
CMY0_I_NDE_OR<-cbind(CMY0_I[c(16),1,],'NDE','OR','C.I (0)')
CMY0_I_NDE_CC<-cbind(CMY0_I[c(16),2,],'NDE','CC','C.I (0)')
CMY0_I_NDE_MI<-cbind(CMY0_I[c(16),3,],'NDE','MI','C.I (0)')
CMY0_I_NDE_EM<-cbind(CMY0_I[c(16),4,],'NDE','EM','C.I (0)')

CMBY0_I_NIE_OR<-cbind(CMBY0_I[c(14),1,],'NIE','OR','D.I (0)')
CMBY0_I_NIE_CC<-cbind(CMBY0_I[c(14),2,],'NIE','CC','D.I (0)')
CMBY0_I_NIE_MI<-cbind(CMBY0_I[c(14),3,],'NIE','MI','D.I (0)')
CMBY0_I_NIE_EM<-cbind(CMBY0_I[c(14),4,],'NIE','EM','D.I (0)')
CMBY0_I_NDE_OR<-cbind(CMBY0_I[c(15),1,],'NDE','OR','D.I (0)')
CMBY0_I_NDE_CC<-cbind(CMBY0_I[c(15),2,],'NDE','CC','D.I (0)')
CMBY0_I_NDE_MI<-cbind(CMBY0_I[c(15),3,],'NDE','MI','D.I (0)')
CMBY0_I_NDE_EM<-cbind(CMBY0_I[c(15),4,],'NDE','EM','D.I (0)')

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

BMY0_II<-array(as.numeric(unlist(BMY0_II)), dim = c(24, 4, 500))
BMCY0_II<-array(as.numeric(unlist(BMCY0_II)), dim = c(25, 4, 500))
CMY0_II<-array(as.numeric(unlist(CMY0_II)), dim = c(26, 4, 500))
CMBY0_II<-array(as.numeric(unlist(CMBY0_II)), dim = c(25, 4, 500))

BMY0_II_NIE_OR<-cbind(BMY0_II[c(17),1,],'NIE','OR','A.II (0)')
BMY0_II_NIE_CC<-cbind(BMY0_II[c(17),2,],'NIE','CC','A.II (0)')
BMY0_II_NIE_MI<-cbind(BMY0_II[c(17),3,],'NIE','MI','A.II (0)')
BMY0_II_NIE_EM<-cbind(BMY0_II[c(17),4,],'NIE','EM','A.II (0)')
BMY0_II_NDE_OR<-cbind(BMY0_II[c(18),1,],'NDE','OR','A.II (0)')
BMY0_II_NDE_CC<-cbind(BMY0_II[c(18),2,],'NDE','CC','A.II (0)')
BMY0_II_NDE_MI<-cbind(BMY0_II[c(18),3,],'NDE','MI','A.II (0)')
BMY0_II_NDE_EM<-cbind(BMY0_II[c(18),4,],'NDE','EM','A.II (0)')

BMCY0_II_NIE_OR<-cbind(BMCY0_II[c(18),1,],'NIE','OR','B.II (0)')
BMCY0_II_NIE_CC<-cbind(BMCY0_II[c(18),2,],'NIE','CC','B.II (0)')
BMCY0_II_NIE_MI<-cbind(BMCY0_II[c(18),3,],'NIE','MI','B.II (0)')
BMCY0_II_NIE_EM<-cbind(BMCY0_II[c(18),4,],'NIE','EM','B.II (0)')
BMCY0_II_NDE_OR<-cbind(BMCY0_II[c(19),1,],'NDE','OR','B.II (0)')
BMCY0_II_NDE_CC<-cbind(BMCY0_II[c(19),2,],'NDE','CC','B.II (0)')
BMCY0_II_NDE_MI<-cbind(BMCY0_II[c(19),3,],'NDE','MI','B.II (0)')
BMCY0_II_NDE_EM<-cbind(BMCY0_II[c(19),4,],'NDE','EM','B.II (0)')

CMY0_II_NIE_OR<-cbind(CMY0_II[c(19),1,],'NIE','OR','C.II (0)')
CMY0_II_NIE_CC<-cbind(CMY0_II[c(19),2,],'NIE','CC','C.II (0)')
CMY0_II_NIE_MI<-cbind(CMY0_II[c(19),3,],'NIE','MI','C.II (0)')
CMY0_II_NIE_EM<-cbind(CMY0_II[c(19),4,],'NIE','EM','C.II (0)')
CMY0_II_NDE_OR<-cbind(CMY0_II[c(20),1,],'NDE','OR','C.II (0)')
CMY0_II_NDE_CC<-cbind(CMY0_II[c(20),2,],'NDE','CC','C.II (0)')
CMY0_II_NDE_MI<-cbind(CMY0_II[c(20),3,],'NDE','MI','C.II (0)')
CMY0_II_NDE_EM<-cbind(CMY0_II[c(20),4,],'NDE','EM','C.II (0)')

CMBY0_II_NIE_OR<-cbind(CMBY0_II[c(18),1,],'NIE','OR','D.II (0)')
CMBY0_II_NIE_CC<-cbind(CMBY0_II[c(18),2,],'NIE','CC','D.II (0)')
CMBY0_II_NIE_MI<-cbind(CMBY0_II[c(18),3,],'NIE','MI','D.II (0)')
CMBY0_II_NIE_EM<-cbind(CMBY0_II[c(18),4,],'NIE','EM','D.II (0)')
CMBY0_II_NDE_OR<-cbind(CMBY0_II[c(19),1,],'NDE','OR','D.II (0)')
CMBY0_II_NDE_CC<-cbind(CMBY0_II[c(19),2,],'NDE','CC','D.II (0)')
CMBY0_II_NDE_MI<-cbind(CMBY0_II[c(19),3,],'NDE','MI','D.II (0)')
CMBY0_II_NDE_EM<-cbind(CMBY0_II[c(19),4,],'NDE','EM','D.II (0)')

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

BMY0_III<-array(as.numeric(unlist(BMY0_III)), dim = c(24, 4, 500))
BMCY0_III<-array(as.numeric(unlist(BMCY0_III)), dim = c(25, 4, 500))
CMY0_III<-array(as.numeric(unlist(CMY0_III)), dim = c(26, 4, 500))
CMBY0_III<-array(as.numeric(unlist(CMBY0_III)), dim = c(25, 4, 500))

BMY0_III_NIE_OR<-cbind(BMY0_III[c(17),1,],'NIE','OR','A.III (0)')
BMY0_III_NIE_CC<-cbind(BMY0_III[c(17),2,],'NIE','CC','A.III (0)')
BMY0_III_NIE_MI<-cbind(BMY0_III[c(17),3,],'NIE','MI','A.III (0)')
BMY0_III_NIE_EM<-cbind(BMY0_III[c(17),4,],'NIE','EM','A.III (0)')
BMY0_III_NDE_OR<-cbind(BMY0_III[c(18),1,],'NDE','OR','A.III (0)')
BMY0_III_NDE_CC<-cbind(BMY0_III[c(18),2,],'NDE','CC','A.III (0)')
BMY0_III_NDE_MI<-cbind(BMY0_III[c(18),3,],'NDE','MI','A.III (0)')
BMY0_III_NDE_EM<-cbind(BMY0_III[c(18),4,],'NDE','EM','A.III (0)')

BMCY0_III_NIE_OR<-cbind(BMCY0_III[c(18),1,],'NIE','OR','B.III (0)')
BMCY0_III_NIE_CC<-cbind(BMCY0_III[c(18),2,],'NIE','CC','B.III (0)')
BMCY0_III_NIE_MI<-cbind(BMCY0_III[c(18),3,],'NIE','MI','B.III (0)')
BMCY0_III_NIE_EM<-cbind(BMCY0_III[c(18),4,],'NIE','EM','B.III (0)')
BMCY0_III_NDE_OR<-cbind(BMCY0_III[c(19),1,],'NDE','OR','B.III (0)')
BMCY0_III_NDE_CC<-cbind(BMCY0_III[c(19),2,],'NDE','CC','B.III (0)')
BMCY0_III_NDE_MI<-cbind(BMCY0_III[c(19),3,],'NDE','MI','B.III (0)')
BMCY0_III_NDE_EM<-cbind(BMCY0_III[c(19),4,],'NDE','EM','B.III (0)')

CMY0_III_NIE_OR<-cbind(CMY0_III[c(19),1,],'NIE','OR','C.III (0)')
CMY0_III_NIE_CC<-cbind(CMY0_III[c(19),2,],'NIE','CC','C.III (0)')
CMY0_III_NIE_MI<-cbind(CMY0_III[c(19),3,],'NIE','MI','C.III (0)')
CMY0_III_NIE_EM<-cbind(CMY0_III[c(19),4,],'NIE','EM','C.III (0)')
CMY0_III_NDE_OR<-cbind(CMY0_III[c(20),1,],'NDE','OR','C.III (0)')
CMY0_III_NDE_CC<-cbind(CMY0_III[c(20),2,],'NDE','CC','C.III (0)')
CMY0_III_NDE_MI<-cbind(CMY0_III[c(20),3,],'NDE','MI','C.III (0)')
CMY0_III_NDE_EM<-cbind(CMY0_III[c(20),4,],'NDE','EM','C.III (0)')

CMBY0_III_NIE_OR<-cbind(CMBY0_III[c(18),1,],'NIE','OR','D.III (0)')
CMBY0_III_NIE_CC<-cbind(CMBY0_III[c(18),2,],'NIE','CC','D.III (0)')
CMBY0_III_NIE_MI<-cbind(CMBY0_III[c(18),3,],'NIE','MI','D.III (0)')
CMBY0_III_NIE_EM<-cbind(CMBY0_III[c(18),4,],'NIE','EM','D.III (0)')
CMBY0_III_NDE_OR<-cbind(CMBY0_III[c(19),1,],'NDE','OR','D.III (0)')
CMBY0_III_NDE_CC<-cbind(CMBY0_III[c(19),2,],'NDE','CC','D.III (0)')
CMBY0_III_NDE_MI<-cbind(CMBY0_III[c(19),3,],'NDE','MI','D.III (0)')
CMBY0_III_NDE_EM<-cbind(CMBY0_III[c(19),4,],'NDE','EM','D.III (0)')

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

BMY0_IV<-array(as.numeric(unlist(BMY0_IV)), dim = c(24, 4, 500))
BMCY0_IV<-array(as.numeric(unlist(BMCY0_IV)), dim = c(25, 4, 500))
CMY0_IV<-array(as.numeric(unlist(CMY0_IV)), dim = c(26, 4, 500))
CMBY0_IV<-array(as.numeric(unlist(CMBY0_IV)), dim = c(25, 4, 500))

BMY0_IV_NIE_OR<-cbind(BMY0_IV[c(17),1,],'NIE','OR','A.IV (0)')
BMY0_IV_NIE_CC<-cbind(BMY0_IV[c(17),2,],'NIE','CC','A.IV (0)')
BMY0_IV_NIE_MI<-cbind(BMY0_IV[c(17),3,],'NIE','MI','A.IV (0)')
BMY0_IV_NIE_EM<-cbind(BMY0_IV[c(17),4,],'NIE','EM','A.IV (0)')
BMY0_IV_NDE_OR<-cbind(BMY0_IV[c(18),1,],'NDE','OR','A.IV (0)')
BMY0_IV_NDE_CC<-cbind(BMY0_IV[c(18),2,],'NDE','CC','A.IV (0)')
BMY0_IV_NDE_MI<-cbind(BMY0_IV[c(18),3,],'NDE','MI','A.IV (0)')
BMY0_IV_NDE_EM<-cbind(BMY0_IV[c(18),4,],'NDE','EM','A.IV (0)')

BMCY0_IV_NIE_OR<-cbind(BMCY0_IV[c(18),1,],'NIE','OR','B.IV (0)')
BMCY0_IV_NIE_CC<-cbind(BMCY0_IV[c(18),2,],'NIE','CC','B.IV (0)')
BMCY0_IV_NIE_MI<-cbind(BMCY0_IV[c(18),3,],'NIE','MI','B.IV (0)')
BMCY0_IV_NIE_EM<-cbind(BMCY0_IV[c(18),4,],'NIE','EM','B.IV (0)')
BMCY0_IV_NDE_OR<-cbind(BMCY0_IV[c(19),1,],'NDE','OR','B.IV (0)')
BMCY0_IV_NDE_CC<-cbind(BMCY0_IV[c(19),2,],'NDE','CC','B.IV (0)')
BMCY0_IV_NDE_MI<-cbind(BMCY0_IV[c(19),3,],'NDE','MI','B.IV (0)')
BMCY0_IV_NDE_EM<-cbind(BMCY0_IV[c(19),4,],'NDE','EM','B.IV (0)')

CMY0_IV_NIE_OR<-cbind(CMY0_IV[c(19),1,],'NIE','OR','C.IV (0)')
CMY0_IV_NIE_CC<-cbind(CMY0_IV[c(19),2,],'NIE','CC','C.IV (0)')
CMY0_IV_NIE_MI<-cbind(CMY0_IV[c(19),3,],'NIE','MI','C.IV (0)')
CMY0_IV_NIE_EM<-cbind(CMY0_IV[c(19),4,],'NIE','EM','C.IV (0)')
CMY0_IV_NDE_OR<-cbind(CMY0_IV[c(20),1,],'NDE','OR','C.IV (0)')
CMY0_IV_NDE_CC<-cbind(CMY0_IV[c(20),2,],'NDE','CC','C.IV (0)')
CMY0_IV_NDE_MI<-cbind(CMY0_IV[c(20),3,],'NDE','MI','C.IV (0)')
CMY0_IV_NDE_EM<-cbind(CMY0_IV[c(20),4,],'NDE','EM','C.IV (0)')

CMBY0_IV_NIE_OR<-cbind(CMBY0_IV[c(18),1,],'NIE','OR','D.IV (0)')
CMBY0_IV_NIE_CC<-cbind(CMBY0_IV[c(18),2,],'NIE','CC','D.IV (0)')
CMBY0_IV_NIE_MI<-cbind(CMBY0_IV[c(18),3,],'NIE','MI','D.IV (0)')
CMBY0_IV_NIE_EM<-cbind(CMBY0_IV[c(18),4,],'NIE','EM','D.IV (0)')
CMBY0_IV_NDE_OR<-cbind(CMBY0_IV[c(19),1,],'NDE','OR','D.IV (0)')
CMBY0_IV_NDE_CC<-cbind(CMBY0_IV[c(19),2,],'NDE','CC','D.IV (0)')
CMBY0_IV_NDE_MI<-cbind(CMBY0_IV[c(19),3,],'NDE','MI','D.IV (0)')
CMBY0_IV_NDE_EM<-cbind(CMBY0_IV[c(19),4,],'NDE','EM','D.IV (0)')

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


#Figure
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


#unidentified case (discrete M and binary Y)
DMBY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/DMBY_I.xlsx', 1)
DMBY_I <- array(as.numeric(unlist(DMBY_I)), dim = c(24, 4, 500))
DMBY_I_OR <- as.data.frame(t(DMBY_I[1:24,1,]))
DMBY_I_EM <- as.data.frame(t(DMBY_I[1:24,4,]))
#oracle
orpar14<-cbind(DMBY_I_OR[14],"tc_0")
colnames(orpar14)<-c("value","par")
orpar15<-cbind(DMBY_I_OR[15],"tc_m2")
colnames(orpar15)<-c("value","par")
orpar16<-cbind(DMBY_I_OR[16],"tc_m3")
colnames(orpar16)<-c("value","par")
orpar<-rbind(orpar14,orpar15,orpar16)
orpar$par<-factor(orpar$par, labels=c(expression(OR:lambda["0"]),expression(OR:lambda["m1"]), expression(OR:lambda["m2"])))
#em
empar14<-cbind(DMBY_I_EM[14],"emc_0")
colnames(empar14)<-c("value","par")
empar15<-cbind(DMBY_I_EM[15],"emc_m2")
colnames(empar15)<-c("value","par")
empar16<-cbind(DMBY_I_EM[16],"emc_m3")
colnames(empar16)<-c("value","par")
empar<-rbind(empar14,empar15,empar16)
empar$par<-factor(empar$par, labels=c(expression(EM:lambda["0"]), expression(EM:lambda["m1"]), expression(EM:lambda["m2"])))
#combinded
par <- rbind(orpar,empar)
#true value of the parameter
ref <- data.frame(par = c("tc_0","tc_m2","tc_m3","emc_0","emc_m2","emc_m3"), int = c(0,2,2,0,2,2))
ref$par <- factor(ref$par, labels=c(expression(OR:lambda["0"]), expression(OR:lambda["m1"]), expression(OR:lambda["m2"]), expression(EM:lambda["0"]), expression(EM:lambda["m1"]), expression(EM:lambda["m2"])))
ref$type <- "True value of the parameter"
#mean of the parameter estimates
ref_est <- par %>% group_by(par) %>% summarise_at(vars(value), list(int = mean))
ref_est$type <- "Mean of the parameter estimates"
#combinded
ref1 <- rbind(ref,ref_est)
ref1$type <- factor(ref1$type, levels=c("True value of the parameter","Mean of the parameter estimates"))

#Figure
count <- 0
breaks_fun <- function(x) {
  count <<- count + 1L
  switch(
    count,
    seq(-2, 4, 0.2),
    seq(-2, 4, 0.2),
    seq(-2, 4, 0.5),
    seq(-2, 4, 0.5),
    seq(-2, 4, 0.5),
    seq(-2, 4, 0.5),
    seq(-20, 20, 0.5),
    seq(-20, 20, 0.5),
    seq(-20, 20, 2),
    seq(-20, 20, 2),
    seq(-20, 20, 2),
    seq(-20, 20, 2)
  )
}

ggplot(par, aes(x=value, fill=par)) + 
  geom_density(aes(y = after_stat(scaled)), alpha = 0.5) +
  scale_fill_manual(values = c("green4","green4","green4","red3","red3","red3"), guide = "none") +
  labs(x = 'Estimates', y = 'Density') +
  facet_wrap(vars(par), nrow = 2, scales="free", labeller = label_parsed) +
  scale_x_continuous(breaks = breaks_fun) +
  geom_vline(data = ref1, aes(xintercept = int, linetype = type, color = par), linewidth = 1) +
  scale_color_manual(values = c("green4","green4","green4","red3","red3","red3"), guide = "none") +
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
        panel.grid.major.x = element_blank())

