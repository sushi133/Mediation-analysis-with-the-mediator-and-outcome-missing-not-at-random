R version 4.1.3

Platform: x86_64-apple-darwin17.0 (64-bit)

R packages: mice (3.14.0), dplyr (1.0.8), parallel (4.1.3), xlsx (0.6.5), ggplot2 (3.3.5), nnet (7.3-17), miscF (0.1-5)

The following code files generate parameter estimates from simulated datasets for various scenarios. Estimations are conducted using complete case analysis, multiple imputation, and the EM algorithm under different MNAR assumptions. The output folder contains the corresponding output for each code file.

M is not independent of Y:

  Binary M and binary Y:
  
    MNAR Assumption 1: BMY_I.R (BMY_I.xlsx)
    
    MNAR Assumption 2: BMY_II.R (BMY_II.xlsx)
    
    MNAR Assumption 3: BMY_III.R (BMY_III.xlsx)
    
    MNAR Assumption 4: BMY_IV.R (BMY_IV.xlsx)
    
  Binary M and continuous Y:
  
    MNAR Assumption 1: BMCY_I.R (BMCY_I.xlsx)
    
    MNAR Assumption 2: BMCY_II.R (BMCY_II.xlsx)
    
    MNAR Assumption 3: BMCY_III.R (BMCY_III.xlsx)
    
    MNAR Assumption 4: BMCY_IV.R (BMCY_IV.xlsx)
    
  Continuous M and continuous Y:
  
    MNAR Assumption 1: CMY_I.R (CMY_I.xlsx)
    
    MNAR Assumption 2: CMY_II.R (CMY_II.xlsx)
    
    MNAR Assumption 3: CMY_III.R (CMY_III.xlsx)
    
    MNAR Assumption 4: CMY_IV.R (CMY_IV.xlsx)
    
  Continuous M and binary Y:
  
    MNAR Assumption 1: CMBY_I.R (CMBY_I.xlsx)
    
    MNAR Assumption 2: CMBY_II.R (CMBY_II.xlsx)
    
    MNAR Assumption 3: CMBY_III.R (CMBY_III.xlsx)
    
    MNAR Assumption 4: CMBY_IV.R (CMBY_IV.xlsx)
    
  Categorical M with three categories and binary Y:
  
    MNAR Assumption 1: DMBY_I.R (DMBY_I.xlsx)
    
    MNAR Assumption 4: DMBY_IV.R (DMBY_IV.xlsx)
  
M is independent of Y:

  Binary M and binary Y:
  
    MNAR Assumption 1: BMY_I(0).R (BMY_I(0).xlsx)
    
    MNAR Assumption 2: BMY_II(0).R (BMY_II(0).xlsx)
    
    MNAR Assumption 3: BMY_III(0).R (BMY_III(0).xlsx)
    
    MNAR Assumption 4: BMY_IV(0).R (BMY_IV(0).xlsx)
    
  Binary M and continuous Y:
  
    MNAR Assumption 1: BMCY_I(0).R (BMCY_I(0).xlsx)
    
    MNAR Assumption 2: BMCY_II(0).R (BMCY_II(0).xlsx)
    
    MNAR Assumption 3: BMCY_III(0).R (BMCY_III(0).xlsx)
    
    MNAR Assumption 4: BMCY_IV(0).R (BMCY_IV(0).xlsx)
    
  Continuous M and continuous Y:
  
    MNAR Assumption 1: CMY_I(0).R (CMY_I(0).xlsx)
    
    MNAR Assumption 2: CMY_II(0).R (CMY_II(0).xlsx)
    
    MNAR Assumption 3: CMY_III(0).R (CMY_III(0).xlsx)
    
    MNAR Assumption 4: CMY_IV(0).R (CMY_IV(0).xlsx)
    
  Continuous M and binary Y:
  
    MNAR Assumption 1: CMBY_I(0).R (CMBY_I(0).xlsx)
    
    MNAR Assumption 2: CMBY_II(0).R (CMBY_II(0).xlsx)
    
    MNAR Assumption 3: CMBY_III(0).R (CMBY_III(0).xlsx)
    
    MNAR Assumption 4: CMBY_IV(0).R (CMBY_IV(0).xlsx)
    
Note: Compute_time(hours).xlsx records the computation time for each code file.

The code file Figure.R provides simulation results in Figure 3, Figure S9, Figure S10, Figure S11 and Figure S12.
