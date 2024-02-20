# Tree Fern Biogeography

Files needed to run the biogeographic analyses presented in "Ramírez-Barahona S (2024) Incorporating fossils into the joint inference of phylogeny and biogeography of the tree fern order Cyatheales. *under review*"  
A pre-print of a previous version of the paper is available here:   https://doi.org/10.1101/2023.02.13.528358  

The repository follows the file system of the R project (R_project.Rproj). All directories are needed to properly run the functions.  
All plots and processing of output was performed in R; the core analyses were performed in RevBayes.   
The script is fully functional, but some of the functions are not yet fully automated and may require additional input.  
The code was last checked and updated February 20, 2024.  

- The 'rb_out' directory contains the RevBayes scripts.  
- The 'input_data' directory contains files to parse area codings and the *Excel* file with the list of fossil specimens used for the dating analyses.
  - The molecular and geographic data are located in the **rb_out/data** directory.  
- The 'code' directory contains the R scripts to process and visualise the results of the modelling.
- The 'BAMM' directory contains the code and results of diversification rate analyses of tree ferns, which are presented in the pre-print of the previous version of the paper.

The R and RevBayes code was last updated November 10, 2023 and contains all the necessary functions.

Code and data are available at Zenodo:  
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8239494.svg)](https://doi.org/10.5281/zenodo.8239494)

When using the RevBayes code please cite:
- Ramírez-Barahona S (2024) Incorporating fossils into the joint inference of phylogeny and biogeography of the tree fern order Cyatheales. *under review*
