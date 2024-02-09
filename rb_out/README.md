# revBayes scripts

The scripts run under revBayes v.1.1.1 (last checked November 2023). 

The folder **ARE/** (*Ancestral Range Estimation*) contains the necessary scripts to perform the biogeographic analyses.  
- The biogeographical modelling is performed with the script **mcmc_A.Rev**.  
  - This script contains all the global settings needed for the analyses and is constructed in order to perform a full joint analysis (i.e., FBD + DEC).  However, in the current implementarion the FBD option is turned *off* and instead this script sources a file with a posterior sample of trees (generated from the code in the **FBD/** folder). The script can also be run with a fixed tree.  
  - This script also contains the code for summarising the results of the modelling.  
- The FBD dating is performed with the scripts **model_FBDP_A.Rev**,  **model_UExp.Rev**, and **model_GTR.Rev**.  
- The DEC modelling is performed with the script **model_biogeo.Rev**.  
- The data needed to run the analyses are read with the script **model_DATA.Rev**.  

The folder **FBD/** (*Fossilised Birth Death*) contains the necessary scripts to perform the FBD dating analyses.  
- The tree dating is performed with the script **mcmc_A.Rev**.
  - This script contains all the global settings needed and sources all other scripts (**model_DATA.Rev**, **model_FBDP_A.Rev**, **model_GTR.Rev**, and **model_UExp.Rev**).
- The results are summarised with the script **summarize.Rev**.  
