# MRF_ISMRM_NIST_phantom
data and R code for MRI MRF project using ISMRM/NIST System phantom

# File structure

### code\
*Data_import.r*\
contains code to read in raw data\
\
*linear_regression.r*\
contains code to run linear regression models on the temperature dependence data\
\
*plot_Bland_Altman.r*\
code which creates functions to calculate Bland Altmans stats plus create the plots\
\
*plot_correlation.r*\
code to create the correlation plots for the MRF, VFA, MESE against the reference values\
\
*plot_CV_bar.r*\
code to create the bar plots of coefficient of variation for MRF, VFA and MESE\
\
*plot_rel_dev.r*\
code to create a plot showing mean relative deviation with error bars between the MRF, VFA, MESE for each of the spheres\
\
*plot_temp.r*\
code to create the scatter plots for the linear regression of the MRF, VFA and MESE with the temperature of the phantom\
\

### data\
contains input raw data from temperature depedence study, accuracy and repeatability study and reference values from NIST\
\

### output\
contains output of results tables created from code in "code" directory\
\

### plots\
contains outputs of plots created from code in "code" directory\
\
\

### MRF_ISMRM_NIST_phantom.Pproj\
main R project file\
\

### README.md
