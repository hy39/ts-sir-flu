# ts-sir-flu-hk
Code related to the paper: *Impact of early phase COVID-19 precautionary behaviours on seasonal influenza in Hong Kong: a time-series modelling approach*

**R_code_n_data** folder contains the source code and the data file (**dat0.csv**) with flu cases from Apr 12, 2015 to Mar 22, 2020, extracted from Flu Express published by Centre for Health Protection in Hong Kong.

**Survey_data** folder contains the survey result (**survey_Phase 1_social_dist_n_Mask.xlsx**) about the behaviours in Phase 1 period.

## Usage
Run the following files in RStudio (an integrated development environment (IDE) for R).

<br/>
To generate the model predictions from Time-Series Susceptible-Infectious-Recovered (TS-SIR) model and obtain required variables for plotting figures:

 run **Code for TS-SIR model with INLA.Rmd** 

<br/>
To generate Fig. 1:
    
 run **plot_Fig1.R** 

<br/>
To generate Fig. 2:

  run **plot_Fig2.R**

<br/>
To generate Fig. 3:

 run **plot_Fig3.R** 

<br/>
To generate Fig. 4:

 run **plot_Fig4.R** 
