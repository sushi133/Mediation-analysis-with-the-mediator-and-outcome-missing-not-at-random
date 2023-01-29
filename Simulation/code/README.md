R version 4.1.3.

Platform: x86_64-apple-darwin17.0 (64-bit)

R packages: tictoc (V 1.0.1), mice (V 3.14.0), dplyr (V 1.0.8), parallel (V 4.1.3), xlsx (V 0.6.5), ggplot2 (V 3.3.5)

In BMCY_III, CMY_I. CMY_II, CMY_III, CMY_IV, CMBY_I, CMBY_II, CMBY_III, CMBY_IV,  we need to approximate the conditional expectation of complete-data log likelihood, and to be consistent, we set the number of iteration in the EM algorithm to be less than 100 times until convergence (add k<100 in stopping criteria) in both identifiable and unidentifiable cases. The execution time for the unidentifiable cases is much longer than the identifiable cases.