## Introduction ######
# This script 1) simulates datasets, 2) fits models and 3) records performance
# Called once on desktop, called repeatedly on Viking
# need to create folders to accept reults - this code will write over results in existing folders

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

## Identify user of code 
# will store results in different locations, will take different actions
user <-if(Sys.info()["user"] == "dpg504" & grepl("viking", Sys.info()["nodename"], fixed = TRUE)){
      "Viking"} else 
        if(Sys.info()["user"] == "ms602"){"Marta"} else
          "David"

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

# This script sets up all the parameters of the simulation study
# loads packages, loads scripts, sets seed, etc
source("SimulationSetUp.R")

### Loop over iterations #####
if(user == "Viking"){
  # assign iteration number in Viking
  args <- commandArgs(trailingOnly=TRUE)
  viking_iteration <- as.integer(args[1])
  
  # create datasets for iteration i
  fn_run_dataset_loop_i(i = viking_iteration)
  # fit models for iteration i
  fn_run_model_loop_i(i = viking_iteration)
  
} else {
  for(i in 1:n_sim){
    # create datasets for iteration i
    fn_run_dataset_loop_i(i = i)
    # fit models for iteration i
    fn_run_model_loop_i(i = i)
  }
}


