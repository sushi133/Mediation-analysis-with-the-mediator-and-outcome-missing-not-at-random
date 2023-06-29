# Mediation analysis with the mediator and outcome missing not at random

# Author Contributions Checklist Form

## National Job Corps Study

### Abstract 

The data describes eligible applicants in the mid-1990s who lived in the areas selected for in-person interviews at the baseline. The subjects were randomized either to the experimental group where they could join the Job Corps program soon after randomization, or to the control group where they were not provided the Job Corps program for three years. The mediator was collected at the 30-months follow-up describing subject’s educational and vocational attainment, measured by whether or not the subject obtained an education credential or vocational certificate after randomization. The outcome was collected at the 48-months follow-up describing the subject’s weekly earnings in the fourth year after randomization. The measured covariates include information on gender, age, race, education level, earnings in the year before participating in the study, whether the subject had a child or not, and whether the subject had ever been arrested or not.

### Availability

Data is available.

### Data 

Source data: mpr_jobcorps_team5_nrw_upd_r_nositeid.dta, key_vars.dta

Analysis data: Jobcorpdata.csv

### Code

Analysis data: Jobcrop.R

Other information in the manuscript: Addtional_Information.R

Table 1-4 in the manuscript: Table.R

Data analysis results from Gamma and Lognormal model under MNAR Assumptions 3/4/5: 

II_Job_Gamma.R, II_Job_Gamma_B.R, III_Job_Gamma.R, III_Job_Gamma_B.R, IV_Job_Gamma.R, IV_Job_Gamma_B.R

II_Job_Lnorm.R, II_Job_Lnorm_B.R, III_Job_Lnorm.R, III_Job_Lnorm_B.R, IV_Job_Lnorm.R, IV_Job_Lnorm_B.R

Sensitivity analysis results from Gamma model under MNAR Assumptions 3+5: 

II_Job_Gamma_S_IV.R, II_Job_Gamma_S_B_IV.R

Sensitivity analysis results from Gamma model under MNAR Assumptions 3+4+5: 

II_Job_Gamma_S_III+IV.R, II_Job_Gamma_S_B_III+IV.R

### Output

Data analysis results from Gamma and Lognormal model under MNAR Assumptions 3/4/5: 

II_Job_Gamma.xlsx, II_Job_Gamma_B.xlsx, III_Job_Gamma.xlsx/III_Job_Gamma_B.xlsx, IV_Job_Gamma.xlsx/IV_Job_Gamma_B.xlsx

II_Job_Lnorm.xlsx, II_Job_Lnorm_B.xlsx, III_Job_Lnorm.xlsx, III_Job_Lnorm_B.xlsx, IV_Job_Lnorm.xlsx/IV_Job_Lnorm_B.xlsx

Sensitivity analysis results from Gamma model under MNAR Assumptions 3+5:

II_Job_Gamma_S_-2_0.xlsx, II_Job_Gamma_S_B_-2_0.xlsx, II_Job_Gamma_S_0_0.xlsx/II_Job_Gamma_S_B_0_0.xlsx, II_Job_Gamma_S_2_0.xlsx/II_Job_Gamma_S_B_2_0.xlsx

Sensitivity analysis results from Gamma model under MNAR Assumptions 3+4+5: 

II_Job_Gamma_S_-2_-2.xlsx, II_Job_Gamma_S_B_-2_-2.xlsx, II_Job_Gamma_S_0_-2.xlsx, II_Job_Gamma_S_B_0_-2.xlsx, II_Job_Gamma_S_2_-2.xlsx, II_Job_Gamma_S_B_2_-2.xlsx, II_Job_Gamma_S_-2_2.xlsx, II_Job_Gamma_S_B_-2_2.xlsx, II_Job_Gamma_S_0_2.xlsx, II_Job_Gamma_S_B_0_2.xlsx, II_Job_Gamma_S_2_2.xlsx, II_Job_Gamma_S_B_2_2.xlsx

## Simulation

### Code

#### Binary M and binary Y under MNAR Assumptions

BMY_I.R, BMY_II.R, BMY_III.R, BMY_IV.R, BMY_I(0).R, BMY_II(0).R, BMY_III(0).R, BMY_IV(0).R

#### Binary M and continuous Y under MNAR Assumptions

BMCY_I.R/BMCY_II.R/BMCY_III.R/BMCY_IV.R

BMCY_I(0).R/BMCY_II(0).R/BMCY_III(0).R/BMCY_IV(0).R

#### Continuous M and continuous Y under MNAR Assumptions

CMY_I.R/CMY_II.R/CMY_III.R/CMY_IV.R

CMY_I(0).R/CMY_II(0).R/CMY_III(0).R/CMY_IV(0).R

#### Continuous M and binary Y under MNAR Assumptions

CMBY_I.R/CMBY_II.R/CMBY_III.R/CMBY_IV.R

CMBY_I(0).R/CMBY_II(0).R/CMBY_III(0).R/CMBY_IV(0).R

#### Categorical M with three categories and binary Y under MNAR Assumptions

DMBY_I.R/DMBY_IV.R

#### Figures

Figure 4, Figure 5 and Figure S6 in the manuscript: Figures.R

### Output

Simulation results (.xlsx) with the same filenames as the corresponding codes. 

## Simulation2 (alternative approach)

The outcome model is identifiable using the complete cases under MNAR Assumption 2, 3 and 5. Therefore, an alternative approach for those scenarios is to estimate the outcome model first using the complete cases, then estimate the parameters in other models through the Expectation-Maximization algorithm by plugging in the estimated outcome model. We tried those two slightly different approaches to our simulation settings, both provided consistent results, with the alternative approach enjoying higher computation efficiency as expected. However, under MNAR Assumption 4, the alternative approach does not work because the outcome model can not be identified by the complete cases. So, the R code under MNAR Assumption 4 remain the same.

