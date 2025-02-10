## Introduction #######
# This script sets up all the parameters of the simulation study

## general approach
# for all sets of multistate parameters given in "Define scenarios.xlsx"
# create data set which includes the target and non-target indications
# fit the appropriate set of statistical models (also given in "Define scenarios.xlsx")
# generate target OS predications for each of the scenarios and record performance
# repeat for a number of iterations where iterations take account of 
# 1) stochastic variation when simulating trial results and 
# 2) stochastic variation when simulating between trial heterogeneity

# number of data sets to simulate i.e. iterations. 
# Captures sampling uncertainty in trial results and heterogeneity in true parameter values (within and between indications)
# where to save and read results
 
if(user == "David"){
    # number of iterations
    n_sim <- 2
} else if(user == "Viking"){
    # n_sim determined by array in .sh file
    n_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
}  


# set working directory (if on cluster)
# if local, working directory is "Simulation files" folder
if(user == "Viking"){
  setwd("/mnt/scratch/users/dpg504/MultiIndication/")
}

# where to save and read results
results_location <- 
  if(user == "David"){
    "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/Desktop simulation results David/"
} else if(user == "Viking"){
    "SimulationResults/"
}   


# record this so it can be read by CalculateResults.R
write.csv(n_sim, file = paste0(results_location, "/n_sim.csv"))

## set seed for reproducibilty of dataset generation

# master seed: used to generate all other seeds using random numbers 
set.seed(11)

# need a new seed in every study s and iteration i 
# samples integers from 1 to 1 million the .Machine$integer.max = 2147483647
# rows = seeds for each iteration
# columns = seeds for each study

# generated with set.seed(11)
# m_seeds_studies <- matrix(sample.int(1000000, 31*n_sim, replace = FALSE),
#                   nrow = n_sim, ncol = 31)
# write.csv(m_seeds_studies, "m_seeds_studies.csv", row.names = FALSE)
m_seeds_studies <- read.csv("m_seeds_studies.csv")

# when simulating heterogeneous parameters values, within and between indications
# need a different seed for each iteration i
# generated with set.seed(11)
# v_seeds_iteration <- sample.int(1000000, n_sim, replace = FALSE)
# write.csv(v_seeds_iteration, "v_seeds_iteration.csv", row.names = FALSE)
v_seeds_iteration <- unlist(as.vector(read.csv("v_seeds_iteration.csv")))
  
## remove scientific notation
options(scipen=999)

## load packages
library(readxl) # read in the scenarios Excel file
library(simIDM) # simulate from multistate model
library(survival) # fit Cox ph model curves
library(gsDesign) # power calculation
library(plotrix) # for elipses plots
library(coda) # calculate GR Rhat diagnostic
library(stableGR) # calculate stable Rhat
library(purrr) # used in creating plots
library(stringr) # used to name models
library(rjags)

## high level input parameters

if(user == "David"){
  # small enough for testing on desktop
  df_scenarios <-  read.csv("Define scenarios DESKTOP.csv")
} else {
  df_scenarios <-  read.csv("Define scenarios VIKING.csv")
}

# apply non-linear adjustment when calculating mean of within indication LHR OS 
# in the presence of within indication heterogeneity?
# note: makes dataset generation quite slow
if(user == "David"){
  # small enough for testing on desktop
  apply_non_linear_adjustment <- FALSE
} else if(user == "Viking"){
  # *change this to TRUE (FALSE saves time for testing)
  apply_non_linear_adjustment <- TRUE
  # if this is applied, number of simulations to draw:
  # x1000 gets values OK to 2nd decimal point e.g. LHR OS 0.26
  n_sim2 <- 4000
}  

# settings for BUGS models

if(user == "David"){
  # small enough for testing on desktop
  nBurnin     <- 5000 # 5000 # 20K in Singh et al
  nIterations <- 20000 # 20000 # 80K in Singh et al
} else if(user == "Viking"){
  nBurnin     <- 50000 # give ample time for convergence
  nIterations <- 150000 
}   

 
## source functions
# functions required to create datsets and run simulations
source("Simulation functions.R") 
# functions to plot datasets etc
source("Plotting functions.R") 
# Univariate BUGS models: Jan's functions which I have modified
source("JAGS BUGS models/OSUnivariateModelFunctions.R") 
source("JAGS BUGS models/PFSUnivariateModelFunctions.R") 
source("JAGS BUGS models/BivariateModelFunctions.R") 

### Create dataframe of all scenarios to predict OS for #####
# This dataframe describes all the scenarios which we will predict OS for
# This also defines the models used in each prediction

# this function defines all predictions as specified in the excel "Define scenarios" file
# it also approximates the required computation time and storage space requried
df_prediction_full <- fn_define_prediction_scenarios()
# save(df_prediction_full, file = paste0(results_location, "df_prediction_full.rda"))

### Create vector of models to fit #####
# some models can be used in different predictions scenarios so only fit the unique models
# this is the order in which models will be fit
v_Model_to_fit <- unique(df_prediction_full$Model_to_fit)


