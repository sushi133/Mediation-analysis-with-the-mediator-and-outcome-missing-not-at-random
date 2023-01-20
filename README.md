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

### Code

Setting A, binary M and binary Y under MNAR Assumptions 2/3/4/5 when M and Y are not independent conditioning on T and X, and when M and Y are independent conditioning on T and X:

BMY_I.R/BMY_II.R/BMY_III.R/BMY_IV.R and BMY_I(0).R/BMY_II(0).R/BMY_III(0).R/BMY_IV(0).R





