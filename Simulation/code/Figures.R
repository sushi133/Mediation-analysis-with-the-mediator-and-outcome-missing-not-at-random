library(xlsx)
library(ggplot2)

#MNAR I
BMY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_I.xlsx', 1)
BMCY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_I.xlsx', 1)
CMY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_I.xlsx', 1)
CMBY_I <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_I.xlsx', 1)

BMY_I<-array(as.numeric(unlist(BMY_I)), dim = c(18, 3, 500))
BMCY_I<-array(as.numeric(unlist(BMCY_I)), dim = c(19, 3, 500))
CMY_I<-array(as.numeric(unlist(CMY_I)), dim = c(20, 3, 500))
CMBY_I<-array(as.numeric(unlist(CMBY_I)), dim = c(19, 3, 500))

BMY_I_NIE_OR<-cbind(BMY_I[c(13),1,],'NIE','Oracle','A.I')
BMY_I_NIE_CC<-cbind(BMY_I[c(13),2,],'NIE','CC','A.I')
BMY_I_NIE_EM<-cbind(BMY_I[c(13),3,],'NIE','EM','A.I')
BMY_I_NDE_OR<-cbind(BMY_I[c(14),1,],'NDE','Oracle','A.I')
BMY_I_NDE_CC<-cbind(BMY_I[c(14),2,],'NDE','CC','A.I')
BMY_I_NDE_EM<-cbind(BMY_I[c(14),3,],'NDE','EM','A.I')

BMCY_I_NIE_OR<-cbind(BMCY_I[c(14),1,],'NIE','Oracle','B.I')
BMCY_I_NIE_CC<-cbind(BMCY_I[c(14),2,],'NIE','CC','B.I')
BMCY_I_NIE_EM<-cbind(BMCY_I[c(14),3,],'NIE','EM','B.I')
BMCY_I_NDE_OR<-cbind(BMCY_I[c(15),1,],'NDE','Oracle','B.I')
BMCY_I_NDE_CC<-cbind(BMCY_I[c(15),2,],'NDE','CC','B.I')
BMCY_I_NDE_EM<-cbind(BMCY_I[c(15),3,],'NDE','EM','B.I')

CMY_I_NIE_OR<-cbind(CMY_I[c(15),1,],'NIE','Oracle','C.I')
CMY_I_NIE_CC<-cbind(CMY_I[c(15),2,],'NIE','CC','C.I')
CMY_I_NIE_EM<-cbind(CMY_I[c(15),3,],'NIE','EM','C.I')
CMY_I_NDE_OR<-cbind(CMY_I[c(16),1,],'NDE','Oracle','C.I')
CMY_I_NDE_CC<-cbind(CMY_I[c(16),2,],'NDE','CC','C.I')
CMY_I_NDE_EM<-cbind(CMY_I[c(16),3,],'NDE','EM','C.I')

CMBY_I_NIE_OR<-cbind(CMBY_I[c(14),1,],'NIE','Oracle','D.I')
CMBY_I_NIE_CC<-cbind(CMBY_I[c(14),2,],'NIE','CC','D.I')
CMBY_I_NIE_EM<-cbind(CMBY_I[c(14),3,],'NIE','EM','D.I')
CMBY_I_NDE_OR<-cbind(CMBY_I[c(15),1,],'NDE','Oracle','D.I')
CMBY_I_NDE_CC<-cbind(CMBY_I[c(15),2,],'NDE','CC','D.I')
CMBY_I_NDE_EM<-cbind(CMBY_I[c(15),3,],'NDE','EM','D.I')

MNAR_I<-as.data.frame(rbind(BMY_I_NIE_OR,BMY_I_NIE_CC,BMY_I_NIE_EM,
                            BMY_I_NDE_OR,BMY_I_NDE_CC,BMY_I_NDE_EM,
                            BMCY_I_NIE_OR,BMCY_I_NIE_CC,BMCY_I_NIE_EM,
                            BMCY_I_NDE_OR,BMCY_I_NDE_CC,BMCY_I_NDE_EM,
                            CMY_I_NIE_OR,CMY_I_NIE_CC,CMY_I_NIE_EM,
                            CMY_I_NDE_OR,CMY_I_NDE_CC,CMY_I_NDE_EM,
                            CMBY_I_NIE_OR,CMBY_I_NIE_CC,CMBY_I_NIE_EM,
                            CMBY_I_NDE_OR,CMBY_I_NDE_CC,CMBY_I_NDE_EM))

colnames(MNAR_I) <- c("bias","effect","method","case")
MNAR_I$bias<-as.numeric(MNAR_I$bias)
MNAR_I$case <- factor(MNAR_I$case, levels=c("A.I","B.I","C.I","D.I"))
MNAR_I$effect <- factor(MNAR_I$effect, levels=c("NIE","NDE"))
MNAR_I$method <- factor(MNAR_I$method, levels=c("CC","EM","Oracle"))


#MNAR II
BMY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_II.xlsx', 1)
BMCY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_II.xlsx', 1)
CMY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_II.xlsx', 1)
CMBY_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_II.xlsx', 1)

BMY_II<-array(as.numeric(unlist(BMY_II)), dim = c(24, 3, 500))
BMCY_II<-array(as.numeric(unlist(BMCY_II)), dim = c(25, 3, 500))
CMY_II<-array(as.numeric(unlist(CMY_II)), dim = c(26, 3, 500))
CMBY_II<-array(as.numeric(unlist(CMBY_II)), dim = c(25, 3, 500))

BMY_II_NIE_OR<-cbind(BMY_II[c(17),1,],'NIE','Oracle','A.II')
BMY_II_NIE_CC<-cbind(BMY_II[c(17),2,],'NIE','CC','A.II')
BMY_II_NIE_EM<-cbind(BMY_II[c(17),3,],'NIE','EM','A.II')
BMY_II_NDE_OR<-cbind(BMY_II[c(18),1,],'NDE','Oracle','A.II')
BMY_II_NDE_CC<-cbind(BMY_II[c(18),2,],'NDE','CC','A.II')
BMY_II_NDE_EM<-cbind(BMY_II[c(18),3,],'NDE','EM','A.II')

BMCY_II_NIE_OR<-cbind(BMCY_II[c(18),1,],'NIE','Oracle','B.II')
BMCY_II_NIE_CC<-cbind(BMCY_II[c(18),2,],'NIE','CC','B.II')
BMCY_II_NIE_EM<-cbind(BMCY_II[c(18),3,],'NIE','EM','B.II')
BMCY_II_NDE_OR<-cbind(BMCY_II[c(19),1,],'NDE','Oracle','B.II')
BMCY_II_NDE_CC<-cbind(BMCY_II[c(19),2,],'NDE','CC','B.II')
BMCY_II_NDE_EM<-cbind(BMCY_II[c(19),3,],'NDE','EM','B.II')

CMY_II_NIE_OR<-cbind(CMY_II[c(19),1,],'NIE','Oracle','C.II')
CMY_II_NIE_CC<-cbind(CMY_II[c(19),2,],'NIE','CC','C.II')
CMY_II_NIE_EM<-cbind(CMY_II[c(19),3,],'NIE','EM','C.II')
CMY_II_NDE_OR<-cbind(CMY_II[c(20),1,],'NDE','Oracle','C.II')
CMY_II_NDE_CC<-cbind(CMY_II[c(20),2,],'NDE','CC','C.II')
CMY_II_NDE_EM<-cbind(CMY_II[c(20),3,],'NDE','EM','C.II')

CMBY_II_NIE_OR<-cbind(CMBY_II[c(18),1,],'NIE','Oracle','D.II')
CMBY_II_NIE_CC<-cbind(CMBY_II[c(18),2,],'NIE','CC','D.II')
CMBY_II_NIE_EM<-cbind(CMBY_II[c(18),3,],'NIE','EM','D.II')
CMBY_II_NDE_OR<-cbind(CMBY_II[c(19),1,],'NDE','Oracle','D.II')
CMBY_II_NDE_CC<-cbind(CMBY_II[c(19),2,],'NDE','CC','D.II')
CMBY_II_NDE_EM<-cbind(CMBY_II[c(19),3,],'NDE','EM','D.II')

MNAR_II<-as.data.frame(rbind(BMY_II_NIE_OR,BMY_II_NIE_CC,BMY_II_NIE_EM,
                             BMY_II_NDE_OR,BMY_II_NDE_CC,BMY_II_NDE_EM,
                             BMCY_II_NIE_OR,BMCY_II_NIE_CC,BMCY_II_NIE_EM,
                             BMCY_II_NDE_OR,BMCY_II_NDE_CC,BMCY_II_NDE_EM,
                             CMY_II_NIE_OR,CMY_II_NIE_CC,CMY_II_NIE_EM,
                             CMY_II_NDE_OR,CMY_II_NDE_CC,CMY_II_NDE_EM,
                             CMBY_II_NIE_OR,CMBY_II_NIE_CC,CMBY_II_NIE_EM,
                             CMBY_II_NDE_OR,CMBY_II_NDE_CC,CMBY_II_NDE_EM))

colnames(MNAR_II) <- c("bias","effect","method","case")
MNAR_II$bias <- as.numeric(MNAR_II$bias)
MNAR_II$case <- factor(MNAR_II$case, levels=c("A.II","B.II","C.II","D.II"))
MNAR_II$effect <- factor(MNAR_II$effect, levels=c("NIE","NDE"))
MNAR_II$method <- factor(MNAR_II$method, levels=c("CC","EM","Oracle"))

#MNAR III
BMY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_III.xlsx', 1)
BMCY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_III.xlsx', 1)
CMY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_III.xlsx', 1)
CMBY_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_III.xlsx', 1)

BMY_III<-array(as.numeric(unlist(BMY_III)), dim = c(24, 3, 500))
BMCY_III<-array(as.numeric(unlist(BMCY_III)), dim = c(25, 3, 500))
CMY_III<-array(as.numeric(unlist(CMY_III)), dim = c(26, 3, 500))
CMBY_III<-array(as.numeric(unlist(CMBY_III)), dim = c(25, 3, 500))

BMY_III_NIE_OR<-cbind(BMY_III[c(17),1,],'NIE','Oracle','A.III')
BMY_III_NIE_CC<-cbind(BMY_III[c(17),2,],'NIE','CC','A.III')
BMY_III_NIE_EM<-cbind(BMY_III[c(17),3,],'NIE','EM','A.III')
BMY_III_NDE_OR<-cbind(BMY_III[c(18),1,],'NDE','Oracle','A.III')
BMY_III_NDE_CC<-cbind(BMY_III[c(18),2,],'NDE','CC','A.III')
BMY_III_NDE_EM<-cbind(BMY_III[c(18),3,],'NDE','EM','A.III')

BMCY_III_NIE_OR<-cbind(BMCY_III[c(18),1,],'NIE','Oracle','B.III')
BMCY_III_NIE_CC<-cbind(BMCY_III[c(18),2,],'NIE','CC','B.III')
BMCY_III_NIE_EM<-cbind(BMCY_III[c(18),3,],'NIE','EM','B.III')
BMCY_III_NDE_OR<-cbind(BMCY_III[c(19),1,],'NDE','Oracle','B.III')
BMCY_III_NDE_CC<-cbind(BMCY_III[c(19),2,],'NDE','CC','B.III')
BMCY_III_NDE_EM<-cbind(BMCY_III[c(19),3,],'NDE','EM','B.III')

CMY_III_NIE_OR<-cbind(CMY_III[c(19),1,],'NIE','Oracle','C.III')
CMY_III_NIE_CC<-cbind(CMY_III[c(19),2,],'NIE','CC','C.III')
CMY_III_NIE_EM<-cbind(CMY_III[c(19),3,],'NIE','EM','C.III')
CMY_III_NDE_OR<-cbind(CMY_III[c(20),1,],'NDE','Oracle','C.III')
CMY_III_NDE_CC<-cbind(CMY_III[c(20),2,],'NDE','CC','C.III')
CMY_III_NDE_EM<-cbind(CMY_III[c(20),3,],'NDE','EM','C.III')

CMBY_III_NIE_OR<-cbind(CMBY_III[c(18),1,],'NIE','Oracle','D.III')
CMBY_III_NIE_CC<-cbind(CMBY_III[c(18),2,],'NIE','CC','D.III')
CMBY_III_NIE_EM<-cbind(CMBY_III[c(18),3,],'NIE','EM','D.III')
CMBY_III_NDE_OR<-cbind(CMBY_III[c(19),1,],'NDE','Oracle','D.III')
CMBY_III_NDE_CC<-cbind(CMBY_III[c(19),2,],'NDE','CC','D.III')
CMBY_III_NDE_EM<-cbind(CMBY_III[c(19),3,],'NDE','EM','D.III')


MNAR_III<-as.data.frame(rbind(BMY_III_NIE_OR,BMY_III_NIE_CC,BMY_III_NIE_EM,
                             BMY_III_NDE_OR,BMY_III_NDE_CC,BMY_III_NDE_EM,
                             BMCY_III_NIE_OR,BMCY_III_NIE_CC,BMCY_III_NIE_EM,
                             BMCY_III_NDE_OR,BMCY_III_NDE_CC,BMCY_III_NDE_EM,
                             CMY_III_NIE_OR,CMY_III_NIE_CC,CMY_III_NIE_EM,
                             CMY_III_NDE_OR,CMY_III_NDE_CC,CMY_III_NDE_EM,
                             CMBY_III_NIE_OR,CMBY_III_NIE_CC,CMBY_III_NIE_EM,
                             CMBY_III_NDE_OR,CMBY_III_NDE_CC,CMBY_III_NDE_EM))

colnames(MNAR_III) <- c("bias","effect","method","case")
MNAR_III$bias<-as.numeric(MNAR_III$bias)
MNAR_III$case <- factor(MNAR_III$case, levels=c("A.III","B.III","C.III","D.III"))
MNAR_III$effect <- factor(MNAR_III$effect, levels=c("NIE","NDE"))
MNAR_III$method <- factor(MNAR_III$method, levels=c("CC","EM","Oracle"))

#MNAR IV
BMY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_IV.xlsx', 1)
BMCY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_IV.xlsx', 1)
CMY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_IV.xlsx', 1)
CMBY_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_IV.xlsx', 1)

BMY_IV<-array(as.numeric(unlist(BMY_IV)), dim = c(24, 3, 500))
BMCY_IV<-array(as.numeric(unlist(BMCY_IV)), dim = c(25, 3, 500))
CMY_IV<-array(as.numeric(unlist(CMY_IV)), dim = c(26, 3, 500))
CMBY_IV<-array(as.numeric(unlist(CMBY_IV)), dim = c(25, 3, 500))

BMY_IV_NIE_OR<-cbind(BMY_IV[c(17),1,],'NIE','Oracle','A.IV')
BMY_IV_NIE_CC<-cbind(BMY_IV[c(17),2,],'NIE','CC','A.IV')
BMY_IV_NIE_EM<-cbind(BMY_IV[c(17),3,],'NIE','EM','A.IV')
BMY_IV_NDE_OR<-cbind(BMY_IV[c(18),1,],'NDE','Oracle','A.IV')
BMY_IV_NDE_CC<-cbind(BMY_IV[c(18),2,],'NDE','CC','A.IV')
BMY_IV_NDE_EM<-cbind(BMY_IV[c(18),3,],'NDE','EM','A.IV')

BMCY_IV_NIE_OR<-cbind(BMCY_IV[c(18),1,],'NIE','Oracle','B.IV')
BMCY_IV_NIE_CC<-cbind(BMCY_IV[c(18),2,],'NIE','CC','B.IV')
BMCY_IV_NIE_EM<-cbind(BMCY_IV[c(18),3,],'NIE','EM','B.IV')
BMCY_IV_NDE_OR<-cbind(BMCY_IV[c(19),1,],'NDE','Oracle','B.IV')
BMCY_IV_NDE_CC<-cbind(BMCY_IV[c(19),2,],'NDE','CC','B.IV')
BMCY_IV_NDE_EM<-cbind(BMCY_IV[c(19),3,],'NDE','EM','B.IV')

CMY_IV_NIE_OR<-cbind(CMY_IV[c(19),1,],'NIE','Oracle','C.IV')
CMY_IV_NIE_CC<-cbind(CMY_IV[c(19),2,],'NIE','CC','C.IV')
CMY_IV_NIE_EM<-cbind(CMY_IV[c(19),3,],'NIE','EM','C.IV')
CMY_IV_NDE_OR<-cbind(CMY_IV[c(20),1,],'NDE','Oracle','C.IV')
CMY_IV_NDE_CC<-cbind(CMY_IV[c(20),2,],'NDE','CC','C.IV')
CMY_IV_NDE_EM<-cbind(CMY_IV[c(20),3,],'NDE','EM','C.IV')

CMBY_IV_NIE_OR<-cbind(CMBY_IV[c(18),1,],'NIE','Oracle','D.IV')
CMBY_IV_NIE_CC<-cbind(CMBY_IV[c(18),2,],'NIE','CC','D.IV')
CMBY_IV_NIE_EM<-cbind(CMBY_IV[c(18),3,],'NIE','EM','D.IV')
CMBY_IV_NDE_OR<-cbind(CMBY_IV[c(19),1,],'NDE','Oracle','D.IV')
CMBY_IV_NDE_CC<-cbind(CMBY_IV[c(19),2,],'NDE','CC','D.IV')
CMBY_IV_NDE_EM<-cbind(CMBY_IV[c(19),3,],'NDE','EM','D.IV')


MNAR_IV<-as.data.frame(rbind(BMY_IV_NIE_OR,BMY_IV_NIE_CC,BMY_IV_NIE_EM,
                             BMY_IV_NDE_OR,BMY_IV_NDE_CC,BMY_IV_NDE_EM,
                             BMCY_IV_NIE_OR,BMCY_IV_NIE_CC,BMCY_IV_NIE_EM,
                             BMCY_IV_NDE_OR,BMCY_IV_NDE_CC,BMCY_IV_NDE_EM,
                             CMY_IV_NIE_OR,CMY_IV_NIE_CC,CMY_IV_NIE_EM,
                             CMY_IV_NDE_OR,CMY_IV_NDE_CC,CMY_IV_NDE_EM,
                             CMBY_IV_NIE_OR,CMBY_IV_NIE_CC,CMBY_IV_NIE_EM,
                             CMBY_IV_NDE_OR,CMBY_IV_NDE_CC,CMBY_IV_NDE_EM))

colnames(MNAR_IV) <- c("bias","effect","method","case")
MNAR_IV$bias<-as.numeric(MNAR_IV$bias)
MNAR_IV$case <- factor(MNAR_IV$case, levels=c("A.IV","B.IV","C.IV","D.IV"))
MNAR_IV$effect <- factor(MNAR_IV$effect, levels=c("NIE","NDE"))
MNAR_IV$method <- factor(MNAR_IV$method, levels=c("CC","EM","Oracle"))

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
MNAR$label <- factor(MNAR$label, levels=c("CC\nNIE","EM\nNIE","Oracle\nNIE","CC\nNDE","EM\nNDE","Oracle\nNDE"))
ggplot(MNAR, aes(x=label, y=bias*100, color=method)) + geom_boxplot(outlier.shape = 1, outlier.color = NULL) +
  stat_summary(fun="mean", geom="point", shape=5, position = position_dodge2(width = 0.75, preserve = "single")) +
  labs(x = '', y = 'Bias (%)') +
  scale_color_manual(values = c("#0099f8","red3","green4")) +
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

BMY0_I<-array(as.numeric(unlist(BMY0_I)), dim = c(18, 3, 500))
BMCY0_I<-array(as.numeric(unlist(BMCY0_I)), dim = c(19, 3, 500))
CMY0_I<-array(as.numeric(unlist(CMY0_I)), dim = c(20, 3, 500))
CMBY0_I<-array(as.numeric(unlist(CMBY0_I)), dim = c(19, 3, 500))

BMY0_I_NIE_OR<-cbind(BMY0_I[c(13),1,],'NIE','Oracle','A.I (0)')
BMY0_I_NIE_CC<-cbind(BMY0_I[c(13),2,],'NIE','CC','A.I (0)')
BMY0_I_NIE_EM<-cbind(BMY0_I[c(13),3,],'NIE','EM','A.I (0)')
BMY0_I_NDE_OR<-cbind(BMY0_I[c(14),1,],'NDE','Oracle','A.I (0)')
BMY0_I_NDE_CC<-cbind(BMY0_I[c(14),2,],'NDE','CC','A.I (0)')
BMY0_I_NDE_EM<-cbind(BMY0_I[c(14),3,],'NDE','EM','A.I (0)')

BMCY0_I_NIE_OR<-cbind(BMCY0_I[c(14),1,],'NIE','Oracle','B.I (0)')
BMCY0_I_NIE_CC<-cbind(BMCY0_I[c(14),2,],'NIE','CC','B.I (0)')
BMCY0_I_NIE_EM<-cbind(BMCY0_I[c(14),3,],'NIE','EM','B.I (0)')
BMCY0_I_NDE_OR<-cbind(BMCY0_I[c(15),1,],'NDE','Oracle','B.I (0)')
BMCY0_I_NDE_CC<-cbind(BMCY0_I[c(15),2,],'NDE','CC','B.I (0)')
BMCY0_I_NDE_EM<-cbind(BMCY0_I[c(15),3,],'NDE','EM','B.I (0)')

CMY0_I_NIE_OR<-cbind(CMY0_I[c(15),1,],'NIE','Oracle','C.I (0)')
CMY0_I_NIE_CC<-cbind(CMY0_I[c(15),2,],'NIE','CC','C.I (0)')
CMY0_I_NIE_EM<-cbind(CMY0_I[c(15),3,],'NIE','EM','C.I (0)')
CMY0_I_NDE_OR<-cbind(CMY0_I[c(16),1,],'NDE','Oracle','C.I (0)')
CMY0_I_NDE_CC<-cbind(CMY0_I[c(16),2,],'NDE','CC','C.I (0)')
CMY0_I_NDE_EM<-cbind(CMY0_I[c(16),3,],'NDE','EM','C.I (0)')

CMBY0_I_NIE_OR<-cbind(CMBY0_I[c(14),1,],'NIE','Oracle','D.I (0)')
CMBY0_I_NIE_CC<-cbind(CMBY0_I[c(14),2,],'NIE','CC','D.I (0)')
CMBY0_I_NIE_EM<-cbind(CMBY0_I[c(14),3,],'NIE','EM','D.I (0)')
CMBY0_I_NDE_OR<-cbind(CMBY0_I[c(15),1,],'NDE','Oracle','D.I (0)')
CMBY0_I_NDE_CC<-cbind(CMBY0_I[c(15),2,],'NDE','CC','D.I (0)')
CMBY0_I_NDE_EM<-cbind(CMBY0_I[c(15),3,],'NDE','EM','D.I (0)')

MNAR0_I<-as.data.frame(rbind(BMY0_I_NIE_OR,BMY0_I_NIE_CC,BMY0_I_NIE_EM,
                             BMY0_I_NDE_OR,BMY0_I_NDE_CC,BMY0_I_NDE_EM,
                             BMCY0_I_NIE_OR,BMCY0_I_NIE_CC,BMCY0_I_NIE_EM,
                             BMCY0_I_NDE_OR,BMCY0_I_NDE_CC,BMCY0_I_NDE_EM,
                             CMY0_I_NIE_OR,CMY0_I_NIE_CC,CMY0_I_NIE_EM,
                             CMY0_I_NDE_OR,CMY0_I_NDE_CC,CMY0_I_NDE_EM,
                             CMBY0_I_NIE_OR,CMBY0_I_NIE_CC,CMBY0_I_NIE_EM,
                             CMBY0_I_NDE_OR,CMBY0_I_NDE_CC,CMBY0_I_NDE_EM))

colnames(MNAR0_I) <- c("bias","effect","method","case")
MNAR0_I$bias<-as.numeric(MNAR0_I$bias)
MNAR0_I$case <- factor(MNAR0_I$case, levels=c("A.I (0)","B.I (0)","C.I (0)","D.I (0)"))
MNAR0_I$effect <- factor(MNAR0_I$effect, levels=c("NIE","NDE"))
MNAR0_I$method <- factor(MNAR0_I$method, levels=c("CC","EM","Oracle"))

#MNAR II (NULL)
BMY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_II(0).xlsx', 1)
BMCY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_II(0).xlsx', 1)
CMY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_II(0).xlsx', 1)
CMBY0_II <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_II(0).xlsx', 1)

BMY0_II<-array(as.numeric(unlist(BMY0_II)), dim = c(24, 3, 500))
BMCY0_II<-array(as.numeric(unlist(BMCY0_II)), dim = c(25, 3, 500))
CMY0_II<-array(as.numeric(unlist(CMY0_II)), dim = c(26, 3, 500))
CMBY0_II<-array(as.numeric(unlist(CMBY0_II)), dim = c(25, 3, 500))

BMY0_II_NIE_OR<-cbind(BMY0_II[c(17),1,],'NIE','Oracle','A.II (0)')
BMY0_II_NIE_CC<-cbind(BMY0_II[c(17),2,],'NIE','CC','A.II (0)')
BMY0_II_NIE_EM<-cbind(BMY0_II[c(17),3,],'NIE','EM','A.II (0)')
BMY0_II_NDE_OR<-cbind(BMY0_II[c(18),1,],'NDE','Oracle','A.II (0)')
BMY0_II_NDE_CC<-cbind(BMY0_II[c(18),2,],'NDE','CC','A.II (0)')
BMY0_II_NDE_EM<-cbind(BMY0_II[c(18),3,],'NDE','EM','A.II (0)')

BMCY0_II_NIE_OR<-cbind(BMCY0_II[c(18),1,],'NIE','Oracle','B.II (0)')
BMCY0_II_NIE_CC<-cbind(BMCY0_II[c(18),2,],'NIE','CC','B.II (0)')
BMCY0_II_NIE_EM<-cbind(BMCY0_II[c(18),3,],'NIE','EM','B.II (0)')
BMCY0_II_NDE_OR<-cbind(BMCY0_II[c(19),1,],'NDE','Oracle','B.II (0)')
BMCY0_II_NDE_CC<-cbind(BMCY0_II[c(19),2,],'NDE','CC','B.II (0)')
BMCY0_II_NDE_EM<-cbind(BMCY0_II[c(19),3,],'NDE','EM','B.II (0)')

CMY0_II_NIE_OR<-cbind(CMY0_II[c(19),1,],'NIE','Oracle','C.II (0)')
CMY0_II_NIE_CC<-cbind(CMY0_II[c(19),2,],'NIE','CC','C.II (0)')
CMY0_II_NIE_EM<-cbind(CMY0_II[c(19),3,],'NIE','EM','C.II (0)')
CMY0_II_NDE_OR<-cbind(CMY0_II[c(20),1,],'NDE','Oracle','C.II (0)')
CMY0_II_NDE_CC<-cbind(CMY0_II[c(20),2,],'NDE','CC','C.II (0)')
CMY0_II_NDE_EM<-cbind(CMY0_II[c(20),3,],'NDE','EM','C.II (0)')

CMBY0_II_NIE_OR<-cbind(CMBY0_II[c(18),1,],'NIE','Oracle','D.II (0)')
CMBY0_II_NIE_CC<-cbind(CMBY0_II[c(18),2,],'NIE','CC','D.II (0)')
CMBY0_II_NIE_EM<-cbind(CMBY0_II[c(18),3,],'NIE','EM','D.II (0)')
CMBY0_II_NDE_OR<-cbind(CMBY0_II[c(19),1,],'NDE','Oracle','D.II (0)')
CMBY0_II_NDE_CC<-cbind(CMBY0_II[c(19),2,],'NDE','CC','D.II (0)')
CMBY0_II_NDE_EM<-cbind(CMBY0_II[c(19),3,],'NDE','EM','D.II (0)')

MNAR0_II<-as.data.frame(rbind(BMY0_II_NIE_OR,BMY0_II_NIE_CC,BMY0_II_NIE_EM,
                              BMY0_II_NDE_OR,BMY0_II_NDE_CC,BMY0_II_NDE_EM,
                              BMCY0_II_NIE_OR,BMCY0_II_NIE_CC,BMCY0_II_NIE_EM,
                              BMCY0_II_NDE_OR,BMCY0_II_NDE_CC,BMCY0_II_NDE_EM,
                              CMY0_II_NIE_OR,CMY0_II_NIE_CC,CMY0_II_NIE_EM,
                              CMY0_II_NDE_OR,CMY0_II_NDE_CC,CMY0_II_NDE_EM,
                              CMBY0_II_NIE_OR,CMBY0_II_NIE_CC,CMBY0_II_NIE_EM,
                              CMBY0_II_NDE_OR,CMBY0_II_NDE_CC,CMBY0_II_NDE_EM))

colnames(MNAR0_II) <- c("bias","effect","method","case")
MNAR0_II$bias<-as.numeric(MNAR0_II$bias)
MNAR0_II$case <- factor(MNAR0_II$case, levels=c("A.II (0)","B.II (0)","C.II (0)","D.II (0)"))
MNAR0_II$effect <- factor(MNAR0_II$effect, levels=c("NIE","NDE"))
MNAR0_II$method <- factor(MNAR0_II$method, levels=c("CC","EM","Oracle"))

#MNAR III (NULL)
BMY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_III(0).xlsx', 1)
BMCY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_III(0).xlsx', 1)
CMY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_III(0).xlsx', 1)
CMBY0_III <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_III(0).xlsx', 1)

BMY0_III<-array(as.numeric(unlist(BMY0_III)), dim = c(24, 3, 500))
BMCY0_III<-array(as.numeric(unlist(BMCY0_III)), dim = c(25, 3, 500))
CMY0_III<-array(as.numeric(unlist(CMY0_III)), dim = c(26, 3, 500))
CMBY0_III<-array(as.numeric(unlist(CMBY0_III)), dim = c(25, 3, 500))

BMY0_III_NIE_OR<-cbind(BMY0_III[c(17),1,],'NIE','Oracle','A.III (0)')
BMY0_III_NIE_CC<-cbind(BMY0_III[c(17),2,],'NIE','CC','A.III (0)')
BMY0_III_NIE_EM<-cbind(BMY0_III[c(17),3,],'NIE','EM','A.III (0)')
BMY0_III_NDE_OR<-cbind(BMY0_III[c(18),1,],'NDE','Oracle','A.III (0)')
BMY0_III_NDE_CC<-cbind(BMY0_III[c(18),2,],'NDE','CC','A.III (0)')
BMY0_III_NDE_EM<-cbind(BMY0_III[c(18),3,],'NDE','EM','A.III (0)')

BMCY0_III_NIE_OR<-cbind(BMCY0_III[c(18),1,],'NIE','Oracle','B.III (0)')
BMCY0_III_NIE_CC<-cbind(BMCY0_III[c(18),2,],'NIE','CC','B.III (0)')
BMCY0_III_NIE_EM<-cbind(BMCY0_III[c(18),3,],'NIE','EM','B.III (0)')
BMCY0_III_NDE_OR<-cbind(BMCY0_III[c(19),1,],'NDE','Oracle','B.III (0)')
BMCY0_III_NDE_CC<-cbind(BMCY0_III[c(19),2,],'NDE','CC','B.III (0)')
BMCY0_III_NDE_EM<-cbind(BMCY0_III[c(19),3,],'NDE','EM','B.III (0)')

CMY0_III_NIE_OR<-cbind(CMY0_III[c(19),1,],'NIE','Oracle','C.III (0)')
CMY0_III_NIE_CC<-cbind(CMY0_III[c(19),2,],'NIE','CC','C.III (0)')
CMY0_III_NIE_EM<-cbind(CMY0_III[c(19),3,],'NIE','EM','C.III (0)')
CMY0_III_NDE_OR<-cbind(CMY0_III[c(20),1,],'NDE','Oracle','C.III (0)')
CMY0_III_NDE_CC<-cbind(CMY0_III[c(20),2,],'NDE','CC','C.III (0)')
CMY0_III_NDE_EM<-cbind(CMY0_III[c(20),3,],'NDE','EM','C.III (0)')

CMBY0_III_NIE_OR<-cbind(CMBY0_III[c(18),1,],'NIE','Oracle','D.III (0)')
CMBY0_III_NIE_CC<-cbind(CMBY0_III[c(18),2,],'NIE','CC','D.III (0)')
CMBY0_III_NIE_EM<-cbind(CMBY0_III[c(18),3,],'NIE','EM','D.III (0)')
CMBY0_III_NDE_OR<-cbind(CMBY0_III[c(19),1,],'NDE','Oracle','D.III (0)')
CMBY0_III_NDE_CC<-cbind(CMBY0_III[c(19),2,],'NDE','CC','D.III (0)')
CMBY0_III_NDE_EM<-cbind(CMBY0_III[c(19),3,],'NDE','EM','D.III (0)')

MNAR0_III<-as.data.frame(rbind(BMY0_III_NIE_OR,BMY0_III_NIE_CC,BMY0_III_NIE_EM,
                               BMY0_III_NDE_OR,BMY0_III_NDE_CC,BMY0_III_NDE_EM,
                               BMCY0_III_NIE_OR,BMCY0_III_NIE_CC,BMCY0_III_NIE_EM,
                               BMCY0_III_NDE_OR,BMCY0_III_NDE_CC,BMCY0_III_NDE_EM,
                               CMY0_III_NIE_OR,CMY0_III_NIE_CC,CMY0_III_NIE_EM,
                               CMY0_III_NDE_OR,CMY0_III_NDE_CC,CMY0_III_NDE_EM,
                               CMBY0_III_NIE_OR,CMBY0_III_NIE_CC,CMBY0_III_NIE_EM,
                               CMBY0_III_NDE_OR,CMBY0_III_NDE_CC,CMBY0_III_NDE_EM))

colnames(MNAR0_III) <- c("bias","effect","method","case")
MNAR0_III$bias<-as.numeric(MNAR0_III$bias)
MNAR0_III$case <- factor(MNAR0_III$case, levels=c("A.III (0)","B.III (0)","C.III (0)","D.III (0)"))
MNAR0_III$effect <- factor(MNAR0_III$effect, levels=c("NIE","NDE"))
MNAR0_III$method <- factor(MNAR0_III$method, levels=c("CC","EM","Oracle"))


#MNAR IV (NULL)
BMY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMY_IV(0).xlsx', 1)
BMCY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/BMCY_IV(0).xlsx', 1)
CMY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMY_IV(0).xlsx', 1)
CMBY0_IV <- read.xlsx('/Users/sushi5824907/Desktop/Mediation/JASA/Simulation/CMBY_IV(0).xlsx', 1)

BMY0_IV<-array(as.numeric(unlist(BMY0_IV)), dim = c(24, 3, 500))
BMCY0_IV<-array(as.numeric(unlist(BMCY0_IV)), dim = c(25, 3, 500))
CMY0_IV<-array(as.numeric(unlist(CMY0_IV)), dim = c(26, 3, 500))
CMBY0_IV<-array(as.numeric(unlist(CMBY0_IV)), dim = c(25, 3, 500))

BMY0_IV_NIE_OR<-cbind(BMY0_IV[c(17),1,],'NIE','Oracle','A.IV (0)')
BMY0_IV_NIE_CC<-cbind(BMY0_IV[c(17),2,],'NIE','CC','A.IV (0)')
BMY0_IV_NIE_EM<-cbind(BMY0_IV[c(17),3,],'NIE','EM','A.IV (0)')
BMY0_IV_NDE_OR<-cbind(BMY0_IV[c(18),1,],'NDE','Oracle','A.IV (0)')
BMY0_IV_NDE_CC<-cbind(BMY0_IV[c(18),2,],'NDE','CC','A.IV (0)')
BMY0_IV_NDE_EM<-cbind(BMY0_IV[c(18),3,],'NDE','EM','A.IV (0)')

BMCY0_IV_NIE_OR<-cbind(BMCY0_IV[c(18),1,],'NIE','Oracle','B.IV (0)')
BMCY0_IV_NIE_CC<-cbind(BMCY0_IV[c(18),2,],'NIE','CC','B.IV (0)')
BMCY0_IV_NIE_EM<-cbind(BMCY0_IV[c(18),3,],'NIE','EM','B.IV (0)')
BMCY0_IV_NDE_OR<-cbind(BMCY0_IV[c(19),1,],'NDE','Oracle','B.IV (0)')
BMCY0_IV_NDE_CC<-cbind(BMCY0_IV[c(19),2,],'NDE','CC','B.IV (0)')
BMCY0_IV_NDE_EM<-cbind(BMCY0_IV[c(19),3,],'NDE','EM','B.IV (0)')

CMY0_IV_NIE_OR<-cbind(CMY0_IV[c(19),1,],'NIE','Oracle','C.IV (0)')
CMY0_IV_NIE_CC<-cbind(CMY0_IV[c(19),2,],'NIE','CC','C.IV (0)')
CMY0_IV_NIE_EM<-cbind(CMY0_IV[c(19),3,],'NIE','EM','C.IV (0)')
CMY0_IV_NDE_OR<-cbind(CMY0_IV[c(20),1,],'NDE','Oracle','C.IV (0)')
CMY0_IV_NDE_CC<-cbind(CMY0_IV[c(20),2,],'NDE','CC','C.IV (0)')
CMY0_IV_NDE_EM<-cbind(CMY0_IV[c(20),3,],'NDE','EM','C.IV (0)')

CMBY0_IV_NIE_OR<-cbind(CMBY0_IV[c(18),1,],'NIE','Oracle','D.IV (0)')
CMBY0_IV_NIE_CC<-cbind(CMBY0_IV[c(18),2,],'NIE','CC','D.IV (0)')
CMBY0_IV_NIE_EM<-cbind(CMBY0_IV[c(18),3,],'NIE','EM','D.IV (0)')
CMBY0_IV_NDE_OR<-cbind(CMBY0_IV[c(19),1,],'NDE','Oracle','D.IV (0)')
CMBY0_IV_NDE_CC<-cbind(CMBY0_IV[c(19),2,],'NDE','CC','D.IV (0)')
CMBY0_IV_NDE_EM<-cbind(CMBY0_IV[c(19),3,],'NDE','EM','D.IV (0)')

MNAR0_IV<-as.data.frame(rbind(BMY0_IV_NIE_OR,BMY0_IV_NIE_CC,BMY0_IV_NIE_EM,
                              BMY0_IV_NDE_OR,BMY0_IV_NDE_CC,BMY0_IV_NDE_EM,
                              BMCY0_IV_NIE_OR,BMCY0_IV_NIE_CC,BMCY0_IV_NIE_EM,
                              BMCY0_IV_NDE_OR,BMCY0_IV_NDE_CC,BMCY0_IV_NDE_EM,
                              CMY0_IV_NIE_OR,CMY0_IV_NIE_CC,CMY0_IV_NIE_EM,
                              CMY0_IV_NDE_OR,CMY0_IV_NDE_CC,CMY0_IV_NDE_EM,
                              CMBY0_IV_NIE_OR,CMBY0_IV_NIE_CC,CMBY0_IV_NIE_EM,
                              CMBY0_IV_NDE_OR,CMBY0_IV_NDE_CC,CMBY0_IV_NDE_EM))

colnames(MNAR0_IV) <- c("bias","effect","method","case")
MNAR0_IV$bias<-as.numeric(MNAR0_IV$bias)
MNAR0_IV$case <- factor(MNAR0_IV$case, levels=c("A.IV (0)","B.IV (0)","C.IV (0)","D.IV (0)"))
MNAR0_IV$effect <- factor(MNAR0_IV$effect, levels=c("NIE","NDE"))
MNAR0_IV$method <- factor(MNAR0_IV$method, levels=c("CC","EM","Oracle"))


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
MNAR0$label <- factor(MNAR0$label, levels=c("CC\nNIE","EM\nNIE","Oracle\nNIE","CC\nNDE","EM\nNDE","Oracle\nNDE"))
ggplot(MNAR0, aes(x=label, y=bias*100, color=method)) + geom_boxplot(outlier.shape = 1, outlier.color = NULL) +
  stat_summary(fun="mean", geom="point", shape=5, position = position_dodge2(width = 0.75, preserve = "single")) +
  labs(x = '', y = 'Bias (%)') +
  scale_color_manual(values = c("#0099f8","red3","green4")) +
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

