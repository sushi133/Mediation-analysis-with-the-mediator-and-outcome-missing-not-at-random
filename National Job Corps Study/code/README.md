R version 4.1.3.

Platform: x86_64-apple-darwin17.0 (64-bit)

R packages: readstata13 (V 0.10.0), dplyr (V 1.0.8), mice (V 3.14.0), tictoc (V 1.0.1), parallel (V 4.1.3), boot (V 1.3-28), MASS(V 7.3-55), xlsx (V 0.6.5)

All the analysis are conducted using the analysis data (Jobcorpdata.csv) created by Jobcrop.R.

In II_Job_Gamma.R/III_Job_Gamma.R/IV_Job_Gamma.R and II_Job_Lnorm.R/III_Job_Lnorm.R/IV_Job_Lnorm.R,  the log-likelihoods evaluated at the MLEs for those six models are calculated. (Table 3)

In II_Job_Gamma_S_IV.R and II_Job_Gamma_S_B_IV.R, sensitivity parameter (gamma z) is 0, and set sensitivity parameter (gamma m) to vary among -2, 0 and 2.

In II_Job_Gamma_S_III+IV.R and II_Job_Gamma_S_B_III+IV.R, set sensitivity parameter (gamma z) to vary between -2 and 2, and set sensitivity parameter (gamma m) to vary among -2, 0 and 2.