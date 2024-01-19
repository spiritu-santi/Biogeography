# revBayes scripts.

The scripts run under revBayes v.1.1.1 (last checked November 2023). 

- The joint-inference is performed with the script **mcmc_A.Rev**.  
  - This script contains all the global settings needed for the analyses and is constructed in order to perform a full joint analysis (i.e., FBD + DEC).  However, in the current implementarion the FBD option is turned *off* and instead the script sources a file with a posterior sample of trees; the script can also be run with a fixed tree.  
  - This script also contains the code for summarising the results of the modelling.  
- The FBD dating is performed with the scripts **model_FBDP_A.Rev**,  **model_UExp.Rev**, and **model_GTR.Rev**.  
- The DEC modelling is performed with the script **model_biogeo.Rev**.  
- The data necessary is read with the script **model_DATA.Rev**.  
