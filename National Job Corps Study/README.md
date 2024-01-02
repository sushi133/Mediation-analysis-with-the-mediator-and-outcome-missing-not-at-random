R version 4.1.3

Platform: x86_64-apple-darwin17.0 (64-bit)

R packages: dplyr (1.0.8), mice (3.14.0), parallel (4.1.3), boot (1.3-28), MASS(7.3-55), xlsx (0.6.5), scales (1.2.1), table1 (1.4.2)

Pre-processing of the National Job Corps Study data in all the code files: 

  1. Create a binary indicator describing whether the earning is greater than 0 or not.
  2. Convert the annual earnings to the weekly earnings.

Data analysis:

The code files generate estimates of parameters and log-likelihoods. The estimation is performed using complete case analysis, multiple imputation, and the EM algorithm under different MNAR Assumptions. The output folder includes the corresponding output for each code file.

Two-part Gamma model for the outcome:

  Point estimates and log-likelihoods based on original data:
  
    MNAR Assumption 2: II_Job_Gamma.R (II_Job_Gamma.xlsx)
    
    MNAR Assumption 3: III_Job_Gamma.R (III_Job_Gamma.xlsx)
    
    MNAR Assumption 4: IV_Job_Gamma.R (IV_Job_Gamma.xlsx)
    
  Confidence intervals based on bootstrap samples:
  
    MNAR Assumption 2: II_Job_Gamma_B.R (II_Job_Gamma_B_Param.xlsx, II_Job_Gamma_B.xlsx)
    
    MNAR Assumption 3: III_Job_Gamma_B.R (III_Job_Gamma_B_Param.xlsx, III_Job_Gamma_B.xlsx)
    
    MNAR Assumption 4: IV_Job_Gamma_B.R (IV_Job_Gamma_B_Param.xlsx, IV_Job_Gamma_B.xlsx)
    
Two-part Log-normal model for the outcome:

  Point estimates and log-likelihoods based on original data:
  
    MNAR Assumption 2: II_Job_Lnorm.R (II_Job_Lnorm.xlsx)
    
    MNAR Assumption 3: III_Job_Lnorm.R (III_Job_Lnorm.xlsx)
    
    MNAR Assumption 4: IV_Job_Lnorm.R (IV_Job_Lnorm.xlsx)
    
  Confidence intervals based on bootstrap samples:
  
    MNAR Assumption 2: II_Job_Lnorm_B.R (II_Job_Lnorm_B_Param.xlsx, II_Job_Lnorm_B.xlsx)
    
    MNAR Assumption 3: III_Job_Lnorm_B.R (III_Job_Lnorm_B_Param.xlsx, III_Job_Lnorm_B.xlsx)
    
    MNAR Assumption 4: IV_Job_Lnorm_B.R (IV_Job_Lnorm_B_Param.xlsx, IV_Job_Lnorm_B.xlsx)
  
Note: Compute_time(hours).xlsx records the computation time for each code file.

Sensitivity analysis:

The code files generate estimates of parameters using two-part Gamma model for the outcome. The estimation is performed using the EM algorithm under MNAR Assumption 2 with different sensitivity parameters. The output folder includes the corresponding output for each code file.

The sensitivity parameter describing the impact of the mediator to missingness of the outcome to vary among -2, 0 and 2: 

  Point estimates based on original data:
  
    The sensitivity parameter is -2: II_Job_Gamma_S_IV.R (II_Job_Gamma_S_0_-2.xlsx)
    
    The sensitivity parameter is 0: II_Job_Gamma_S_IV.R (II_Job_Gamma_S_0_0.xlsx)
    
    The sensitivity parameter is 2: II_Job_Gamma_S_IV.R (II_Job_Gamma_S_0_2.xlsx)
    
  Confidence intervals based on bootstrap samples:
  
    The sensitivity parameter is -2: II_Job_Gamma_S_B_IV.R (II_Job_Gamma_S_B_0_-2_Param.xlsx, II_Job_Gamma_S_B_0_-2.xlsx)
    
    The sensitivity parameter is 0: II_Job_Gamma_S_B_IV.R (II_Job_Gamma_S_B_0_0_Param.xlsx, II_Job_Gamma_S_B_0_0.xlsx)
    
    The sensitivity parameter is 2: II_Job_Gamma_S_B_IV.R (II_Job_Gamma_S_B_0_2_Param.xlsx, II_Job_Gamma_S_B_0_2.xlsx) 

The sensitivity parameters describing the impact of the outcome to missingness of the outcome to vary among -2, 0 and 2, and the impact of the mediator to missingness of the outcome to vary among -2, 0 and 2:

  Point estimates based on original data:
  
    The sensitivity parameters are -2 and -2: II_Job_Gamma_S_III+IV.R (II_Job_Gamma_S_-2_-2.xlsx)
    
    The sensitivity parameters are -2 and 0: II_Job_Gamma_S_III+IV.R (II_Job_Gamma_S_-2_0.xlsx)
    
    The sensitivity parameters are -2 and 2: II_Job_Gamma_S_III+IV.R (II_Job_Gamma_S_-2_2.xlsx)
    
    The sensitivity parameters are 2 and -2: II_Job_Gamma_S_III+IV.R (II_Job_Gamma_S_2_-2.xlsx)
    
    The sensitivity parameters are 2 and 0: II_Job_Gamma_S_III+IV.R (II_Job_Gamma_S_2_0.xlsx)
    
    The sensitivity parameters are 2 and 2: II_Job_Gamma_S_III+IV.R (II_Job_Gamma_S_2_2.xlsx)
    
  Confidence intervals based on bootstrap samples:
  
    The sensitivity parameters are -2 and -2: II_Job_Gamma_S_B_III+IV.R (II_Job_Gamma_S_B_-2_-2_Param.xlsx, II_Job_Gamma_S_B_-2_-2.xlsx)
    
    The sensitivity parameters are -2 and 0: II_Job_Gamma_S_B_III+IV.R (II_Job_Gamma_S_B_-2_0_Param.xlsx, II_Job_Gamma_S_B_-2_0.xlsx)
    
    The sensitivity parameters are -2 and 2: II_Job_Gamma_S_B_III+IV.R (II_Job_Gamma_S_B_-2_2_Param.xlsx, II_Job_Gamma_S_B_-2_2.xlsx)
    
    The sensitivity parameters are 2 and -2: II_Job_Gamma_S_B_III+IV.R (II_Job_Gamma_S_B_2_-2_Param.xlsx, II_Job_Gamma_S_B_2_-2.xlsx)
    
    The sensitivity parameters are 2 and 0: II_Job_Gamma_S_B_III+IV.R (II_Job_Gamma_S_B_2_0_Param.xlsx, II_Job_Gamma_S_B_2_0.xlsx)
    
    The sensitivity parameters are 2 and 2: II_Job_Gamma_S_B_III+IV.R (II_Job_Gamma_S_B_2_2_Param.xlsx, II_Job_Gamma_S_B_2_2.xlsx)
    
Note: Compute_time(hours).xlsx records the computation time for each code file.
 
Others:

The code file Addtional_Information.R provides additional information for the National Job Corps Study including the percentages of subjects having the outcome observed among subjects with missing mediator values, the percentage of the missingness exist in covariates, the percentage of zero values of the earnings, and the distribution of the covariates.

The code file Table.R provides missingness patterns in the mediator and outcome (Table 1), model comparison (Table 2), data analysis results (Table 3), and sensitivity analysis results (Table S2).
