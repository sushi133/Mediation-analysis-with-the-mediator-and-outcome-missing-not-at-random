# Mediation analysis with the mediator and outcome missing not at random

# Author Contributions Checklist Form

## Data Analysis

### Abstract 

The data describes 8707 eligible applicants in the mid-1990s who lived in the areas selected for in-person interviews at the baseline. The subjects were randomized either to the experimental group where they could join the Job Corps program soon after randomization, or to the control group where they were not provided the Job Corps program for three years. The mediator was collected at the 30-months follow-up describing subject’s educational and vocational attainment, measured by whether or not the subject obtained an education credential or vocational certificate after randomization. The outcome was collected at the 48-months follow-up describing the subject’s weekly earnings in the fourth year after randomization. The measured covariates include information on gender, age, race, education level, earnings in the year before participating in the study, whether the subject had a child or not, and whether the subject had ever been arrested or not.

### Data

Jobcorpdata.csv

### Code

Two-part Gamma model under MNAR Assumptions 3/4/5:

II_Job_Gamma.R/III_Job_Gamma.R/IV_Job_Gamma.R 

Two-part log-normal model under MNAR Assumptions 3/4/5:

II_Job_Lnorm.R/III_Job_Lnorm.R/IV_Job_Lnorm.R 

Data analysis results from two-part Gamma model under MNAR Assumptions 3:

II_Job_Gamma.R/II_Job_Gamma_B.R

Sensitivity analysis results from two-part Gamma model under MNAR Assumptions 3+5:

II_Job_Gamma_S_IV.R/II_Job_Gamma_S_B_IV.R

Sensitivity analysis results from two-part Gamma model under MNAR Assumptions 3+4+5:

II_Job_Gamma_S_III+IV.R/II_Job_Gamma_S_B_III+IV.R

Table 4 and Table 5:

Table.R

### Result

Two-part Gamma model under MNAR Assumptions 3/4/5:

II_Job_Gamma.xlsx/III_Job_Gamma.xlsx/IV_Job_Gamma.xlsx

Two-part log-normal model under MNAR Assumptions 3/4/5:

II_Job_Lnorm.xlsx/III_Job_Lnorm.xlsx/IV_Job_Lnorm.xlsx

Data analysis results from two-part Gamma model under MNAR Assumptions 3:

II_Job_Gamma.xlsx/II_Job_Gamma_B_Param.xlsx/II_Job_Gamma_B.xlsx

Sensitivity analysis results from two-part Gamma model under MNAR Assumptions 3+5:

II_Job_Gamma_S_-2_0.xlsx/II_Job_Gamma_S_B_-2_0_Param.xlsx/II_Job_Gamma_S_B_-2_0.xlsx

II_Job_Gamma_S_0_0.xlsx/II_Job_Gamma_S_B_0_0_Param.xlsx/II_Job_Gamma_S_B_0_0.xlsx

II_Job_Gamma_S_2_0.xlsx/II_Job_Gamma_S_B_2_0_Param.xlsx/II_Job_Gamma_S_B_2_0.xlsx 

Sensitivity analysis results from two-part Gamma model under MNAR Assumptions 3+4+5: 

II_Job_Gamma_S_-2_-2.xlsx/II_Job_Gamma_S_B_-2_-2_Param.xlsx/II_Job_Gamma_S_B_-2_-2.xlsx

II_Job_Gamma_S_0_-2.xlsx/II_Job_Gamma_S_B_0_-2_Param.xlsx/II_Job_Gamma_S_B_0_-2.xlsx

II_Job_Gamma_S_2_-2.xlsx/II_Job_Gamma_S_B_2_-2_Param.xlsx/II_Job_Gamma_S_B_2_-2.xlsx

II_Job_Gamma_S_-2_2.xlsx/II_Job_Gamma_S_B_-2_2_Param.xlsx/II_Job_Gamma_S_B_-2_2.xlsx

II_Job_Gamma_S_0_2.xlsx/II_Job_Gamma_S_B_0_2_Param.xlsx/II_Job_Gamma_S_B_0_2.xlsx

II_Job_Gamma_S_2_2.xlsx/II_Job_Gamma_S_B_2_2_Param.xlsx/II_Job_Gamma_S_B_2_2.xlsx


## Simulation

### Abstract 

We considered a sample size of 1000, and simulated 500 data sets for each simulation scenario. We applied the following three methods to compare their results on the estimations of the NIE and NDE: 1) complete case analysis, which provides consistent estimates under
MCAR; 2) our proposed methods using the Expectation-Maximization algorithm, which are designed to deal with the MNAR assumptions under concern; and 3) oracle estimators, which are obtained by using the true values of the missing data.

### Code

Setting A, binary M and binary Y under MNAR Assumptions 2/3/4/5 when M and Y are not independent conditioning on T and X, and when M and Y are independent conditioning on T and X:

BMY_I.R/BMY_II.R/BMY_III.R/BMY_IV.R and BMY_I(0).R/BMY_II(0).R/BMY_III(0).R/BMY_IV(0).R

Setting B, binary M and continuous Y under MNAR Assumptions 2/3/4/5 when M and Y are not independent conditioning on T and X, and when M and Y are independent conditioning on T and X:

BMCY_I.R/BMCY_II.R/BMCY_III.R/BMCY_IV.R and BMCY_I(0).R/BMCY_II(0).R/BMCY_III(0).R/BMCY_IV(0).R

Setting C, continuous M and continuous Y under MNAR Assumptions 2/3/4/5 when M and Y are not independent conditioning on T and X, and when M and Y are independent conditioning on T and X:

CMY_I.R/CMY_II.R/CMY_III.R/CMY_IV.R and CMY_I(0).R/CMY_II(0).R/CMY_III(0).R/CMY_IV(0).R

Setting D, continuous M and binary Y under MNAR Assumptions 2/3/4/5 when M and Y are not independent conditioning on T and X, and when M and Y are independent conditioning on T and X:

CMBY_I.R/CMBY_II.R/CMBY_III.R/CMBY_IV.R and CMBY_I(0).R/CMBY_II(0).R/CMBY_III(0).R/CMBY_IV(0).R

Figure 4 and Figure 5:

Figures.R

### Result

Simulation results (.xlsx) with the same filenames as the corresponding codes.

Figure 4 and Figure 5:

SimF1.png and SimF2.png

