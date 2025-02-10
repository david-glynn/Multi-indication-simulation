## Introduction ##########
# calculate all performance masures for all evidence sets for the following:

library(stringr)

user <- if(Sys.info()["user"] == "dpg504" & Sys.info()["sysname"] == "Windows"){
  "David"} else 
    if(Sys.info()["user"] == "dpg504" & grepl("viking", Sys.info()["nodename"], fixed = TRUE)){
      "Viking"} 

# set working directory (if on cluster)
# if local, working directory is "Simulation files" folder
if(user == "Viking"){
  setwd("/mnt/scratch/users/dpg504/MultiIndication/")
} 

## Set these variables
# where are the results stored

## Set these variables

if(user == "Viking"){
  ## calculations on Viking - calculate for all parameters and models defined
  # df_scenarios <-  read.csv("Define scenarios VIKING.csv")
  # param_set <- df_scenarios$Parameter_set
  param_set <- c(
                 #1026:1050,
                 #2026:2050,
                 #3026:3050,
                 #4026:4050, # with hetero 
                 #5026:5050,
                 #6026:6050, # with hetero
                 #7026:7050
                 #8026:8050 # with hetero
                 3030, 3047
                 )
  model_set <-c(
    "uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
    "bi_ce_matched",   "bi_re_matched", 
    "bi_ce_unmatched", "bi_re_unmatched",
    "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
    "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
    "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share") 
  
} else {
  ## calculations on local machine
  # parameter sets to calculate results for
  df_scenarios <-  read.csv("Define scenarios DESKTOP.csv")
  param_set <- df_scenarios$Parameter_set
  # models to calculate results for
  model_set <- c(
    "uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
    "bi_ce_matched",   "bi_re_matched", 
    "bi_ce_unmatched", "bi_re_unmatched",
    "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
    "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
    "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share") 
}


## other settings
# where are the results stored
results_location <- if(user == "David"){
  "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/Desktop simulation results David/"
} else if(user == "Viking"){
  "SimulationResults/"
}   

# how many dataset iterations to calculate results for
n_sim <- read.csv(paste0(results_location, "n_sim.csv"))[1,2]

# evidence sets to calculate results for:
evidence_set <- c("early_with OS", "mid_with OS", "late_with OS", "early_no OS", "mid_no OS", "late_no OS")
# evidence_set <- c("early_with OS", "mid_with OS", "late_with OS")

## Performance ############

## function to loop over models and iterations in parameter set p and evidence set e
# returns result as table

# p = 1050
# e = "early_with OS"

fn_calc_results_p_e <- function(p, e){
  # initiaise dataframe to hold results for parameter set p and evidence set e
  df_results <- data.frame(
    Model = rep(NA, length(model_set)),
    RelBias = rep(NA, length(model_set)),
    RelBias_MCSE = rep(NA, length(model_set)),
    Bias = rep(NA, length(model_set)),
    Bias_PFS = rep(NA, length(model_set)),
    Bias_MCSE = rep(NA, length(model_set)),
    Bias_PFS_MCSE = rep(NA, length(model_set)),
    Bias_median = rep(NA, length(model_set)),
    Cover = rep(NA, length(model_set)),
    Cover_PFS = rep(NA, length(model_set)),
    Cover_MCSE = rep(NA, length(model_set)),
    Cover_PFS_MCSE = rep(NA, length(model_set)),
    MSE = rep(NA, length(model_set)),
    MSE_MCSE = rep(NA, length(model_set)),
    RMSE = rep(NA, length(model_set)),
    RMSE_MCSE = rep(NA, length(model_set)),
    MAE = rep(NA, length(model_set)),
    EmpSE = rep(NA, length(model_set)),
    EmpSE_MCSE = rep(NA, length(model_set)),
    EmpSE_PFS = rep(NA, length(model_set)),
    EmpSE_PFS_MCSE = rep(NA, length(model_set)),
    PED_IE = rep(NA, length(model_set)), # point estimate divergence vs IE model
    PED_IE_MCSE = rep(NA, length(model_set)),
    PED_obs = rep(NA, length(model_set)), # point estimate divergence vs observed OS LHR in the target indication
    PED_obs_MCSE = rep(NA, length(model_set)),
    PED_IE_w_share = rep(NA, length(model_set)), # point estimate divergence vs IE model
    PED_IE_w_share_MCSE = rep(NA, length(model_set)),
    RelPED_IE = rep(NA, length(model_set)),
    RelPED_IE_MCSE = rep(NA, length(model_set)),
    RelPED_obs = rep(NA, length(model_set)),
    RelPED_obs_MCSE = rep(NA, length(model_set)),
    RelPED_IE_w_share = rep(NA, length(model_set)),
    RelPED_IE_w_share_MCSE = rep(NA, length(model_set)),
    splittingModSeR_IE = rep(NA, length(model_set)), # splitting SeR vs IE model
    splittingModSeR_IE_MCSE = rep(NA, length(model_set)),
    splittingModSeR_IE_w_share = rep(NA, length(model_set)), # splitting SeR vs IE model
    splittingModSeR_IE_w_share_MCSE = rep(NA, length(model_set)),
    splittingModSeR_IE_w_share_PFS = rep(NA, length(model_set)), # splitting SeR vs IE model
    splittingModSeR_IE_w_share_PFS_MCSE = rep(NA, length(model_set)),
    splittingEmpSeR_IE = rep(NA, length(model_set)),
    splittingEmpSeR_IE_MCSE = rep(NA, length(model_set)),
    splittingEmpSeR_IE_w_share = rep(NA, length(model_set)),
    splittingEmpSeR_IE_w_share_MCSE = rep(NA, length(model_set)),
    splittingModSeR_obs = rep(NA, length(model_set)), # splitting SeR vs observed OS LHR in the target indication
    splittingModSeR_obs_MCSE = rep(NA, length(model_set)),
    splittingEmpSeR_obs = rep(NA, length(model_set)),
    splittingEmpSeR_obs_MCSE = rep(NA, length(model_set)),
    #Prec_increase_precision_model_vs_IE_w_share = rep(NA, length(model_set)),
    #Prec_increase_precision_model_vs_IE_w_share_MCSE = rep(NA, length(model_set)),
    ModSE = rep(NA, length(model_set)),
    ModSE_MCSE = rep(NA, length(model_set)),
    ModSE_PFS = rep(NA, length(model_set)),
    ModSE_PFS_MCSE = rep(NA, length(model_set)),
    Ratio_E_ModSE_sq_over_EmpSE_sq = rep(NA, length(model_set)), # pg 2087 Morris check that modSE predicts empSE
    Prop_Rhat_1.2_option1 = rep(NA, length(model_set)),
    Prop_Rhat_1.1_option1 = rep(NA, length(model_set)),
    Prop_Rhat_1.05_option1 = rep(NA, length(model_set)),
    Prop_Rhat_1.01_option1 = rep(NA, length(model_set)),
    Prop_Rhat_1.2_option2 = rep(NA, length(model_set)),
    Prop_Rhat_1.1_option2 = rep(NA, length(model_set)),
    Prop_Rhat_1.05_option2 = rep(NA, length(model_set)),
    Prop_Rhat_1.01_option2 = rep(NA, length(model_set)),
    mean_observed_OS_LHR = rep(NA, length(model_set)),
    sd_observed_OS_LHR = rep(NA, length(model_set)),
    sd_true_OS_LHR_restricted = rep(NA, length(model_set)),
    sd_true_OS_LHR_unrestricted = rep(NA, length(model_set)),
    CV_LHROS_observed = rep(NA, length(model_set)), # coefficient of variation
    CV_LHROS_true_restricted = rep(NA, length(model_set)),
    CV_LHRPFS_observed = rep(NA, length(model_set)),
    CV_LHRPFS_true_restricted = rep(NA, length(model_set)),
    Num_target_posterior_exist = rep(NA, length(model_set)), # what proportion of the target posterior datasets exist (and are not empty)
    Num_underlying_dataset_exist = rep(NA, length(model_set)),
    Num_Rhat_exist = rep(NA, length(model_set))
    )
  # save results per data set
  Bias_per_iteration <- 
    point_estimate_per_iteration <- 
    ModSE_per_iteration <- 
    Rhat_1.1_option1_per_iteration <- 
    Rhat_1.1_option2_per_iteration <- as.data.frame(matrix(NA, nrow = n_sim, ncol = length(model_set)))
  names(Bias_per_iteration) <- 
    names(point_estimate_per_iteration) <- 
    names(ModSE_per_iteration) <- 
    names(Rhat_1.1_option1_per_iteration) <- 
    names(Rhat_1.1_option2_per_iteration) <- 
    model_set
  
  # loop over model sets
  for(m in 1:length(model_set)){
    
    # the mixture models are not fit in sets 5,  6, 7 or 8 so skip in this case
    skip_model <- (round(p/1000,0) == 5 | round(p/1000,0) == 6 | round(p/1000,0) == 7 | round(p/1000,0) == 8) & 
      (grepl("mcie", model_set[m], fixed = TRUE) | grepl("mrie", model_set[m], fixed = TRUE))

    if(skip_model == FALSE){
      ## calculate performance measures
      # formulas in overleaf
      
      ## Posterior summaries ####
      ## create vector of target posterior summaries for: 
      ## parameter set p (e.g. 1001), for model m (e.g. uni_ie) and evidence set e (e.g. early_with OS)
      # target posterior exists
      target_posterior_exist <- rep(FALSE, n_sim)
      # expected posterior for all i iterations
      theta_hat_mean <- rep(NA, n_sim)
      # lower and upper 95% interval of posterior for all i iterations
      theta_hat_low_95 <- rep(NA, n_sim)
      theta_hat_upper_95 <- rep(NA, n_sim)
      # variance of posterior for all i iterations
      theta_hat_var <- rep(NA, n_sim)
      # expected ie posterior for all i iterations (used in calculating PED)
      theta_hat_mean_ie <- rep(NA, n_sim) 
      theta_hat_var_ie <- rep(NA, n_sim)
      theta_hat_mean_ie_w_share <- rep(NA, n_sim) 
      theta_hat_var_ie_w_share <- rep(NA, n_sim) 
      # for PFS
      theta_hat_mean_PFS <- rep(NA, n_sim)
      theta_hat_low_95_PFS <- rep(NA, n_sim)
      theta_hat_upper_95_PFS <- rep(NA, n_sim)
      theta_hat_var_PFS <- rep(NA, n_sim)
      for(i in 1:n_sim){
        # load posteriors (theta_hat) for all models and all evidence sets
        path_to_file_temp <- paste0(results_location, "TargetOS_posterior_summary/", "df_post_summary_", p, "_", e, "_", model_set[m], "_i" , i, ".csv")
        if(file.exists(path_to_file_temp) & class(try(read.csv(path_to_file_temp))) != "try-error"){
          # if non-empty file exists, record status of file in iteration i
          target_posterior_exist[i] <- TRUE
          
          # load target posterior
          target_posterior_summary <- read.csv(path_to_file_temp)[,-1]
          # compute posterior summaries for iteration i 
          # (parameter set p, model m, evidence set e)
          theta_hat_mean[i] <- target_posterior_summary$Mean 
          theta_hat_low_95[i] <- target_posterior_summary$q_0.025
          theta_hat_upper_95[i] <- target_posterior_summary$q_0.975 
          theta_hat_var[i] <- target_posterior_summary$SD^2
          
          # load target posterior for PFS (only in surrogacy models)
          # *for now, only for CE matched (w and without share) and RE matched (w and without share) 
          # + with OS in the target
          if(startsWith(model_set[m], "bi") & 
             grepl("_matched", model_set[m], fixed = TRUE) &
             grepl("with OS", e, fixed = TRUE)
             ){
            target_posterior_summary_PFS <- read.csv(paste0(results_location, "Model_posterior_summary/", 
                                                            "MCMC_Output_PFS_matched",
                                                            ifelse(grepl("_w_share", model_set[m], fixed = TRUE), "_w_share", ""),
                                                            p, "_" ,
                                                            str_extract(model_set[m], "[^_]*_[^_]*"), "_", 
                                                            str_extract(e, "[^_]*"),
                                                            "_i" , i, ".csv"))[,-1]
            if(grepl("_ce_", model_set[m], fixed = TRUE)){
              # CE target posterior always based on mu
              theta_hat_mean_PFS[i] <- target_posterior_summary_PFS$Mean[target_posterior_summary_PFS$Parameter == "mu"]
              theta_hat_low_95_PFS[i] <- target_posterior_summary_PFS$q_0.025[target_posterior_summary_PFS$Parameter == "mu"]
              theta_hat_upper_95_PFS[i] <- target_posterior_summary_PFS$q_0.975[target_posterior_summary_PFS$Parameter == "mu"]
              theta_hat_var_PFS[i] <- target_posterior_summary_PFS$SD[target_posterior_summary_PFS$Parameter == "mu"]^2
            } else {
              # RE target posterior based on mu.theta[1] when there is OS in the target 
              theta_hat_mean_PFS[i] <- target_posterior_summary_PFS$Mean[target_posterior_summary_PFS$Parameter == "mu.theta[1]"]
              theta_hat_low_95_PFS[i] <- target_posterior_summary_PFS$q_0.025[target_posterior_summary_PFS$Parameter == "mu.theta[1]"]
              theta_hat_upper_95_PFS[i] <- target_posterior_summary_PFS$q_0.975[target_posterior_summary_PFS$Parameter == "mu.theta[1]"]
              theta_hat_var_PFS[i] <- target_posterior_summary_PFS$SD[target_posterior_summary_PFS$Parameter == "mu.theta[1]"]^2
            }
          }

          # Identify the IP target posterior in parameter set p, evidence set e
          # This is used in calculating the point estimate divergence (PED)
          IP_target_posterior_summary <- read.csv(paste0(results_location, "TargetOS_posterior_summary/", "df_post_summary_", p, "_", e, "_uni_ie_i" , i, ".csv"))[,-1]
          theta_hat_mean_ie[i] <- IP_target_posterior_summary$Mean 
          theta_hat_var_ie[i] <- IP_target_posterior_summary$SD^2
          # # IP posterior with sharing
          IP_w_share_target_posterior_summary <- read.csv(paste0(results_location, "TargetOS_posterior_summary/", "df_post_summary_", p, "_", e, "_uni_ie_w_share_i" , i, ".csv"))[,-1]
          theta_hat_mean_ie_w_share[i] <- IP_w_share_target_posterior_summary$Mean
          theta_hat_var_ie_w_share[i] <- IP_w_share_target_posterior_summary$SD^2
          
        } else {
          print(path_to_file_temp)
        }
      }
      
      ## True parameter values ####
      ## create vector of true parameter values (theta) for: 
      ## parameter set p (e.g. 1.1), for model m (e.g. uni_ie) and evidence set e (e.g. early_with OS)
      underlying_dataset_exist <- rep(FALSE, n_sim)
      theta <- rep(NA, n_sim) # "restricted" LHR OS
      theta_PFS <- rep(NA, n_sim)
      theta_unrestricted <- rep(NA, n_sim)
      obs_target_OS_LHR_mean <- rep(NA, n_sim) # observed point estimate of LHR OS in target indication
      obs_target_OS_LHR_se <- rep(NA, n_sim) # observed SE of LHR OS in target indication
      mean_observed_OS_LHR <- rep(NA, n_sim) # mean of the observed effect in the trials across the late dataset
      sd_observed_OS_LHR <- rep(NA, n_sim) # standard deviation of the observed effect in the trials across the late dataset
      sd_true_OS_LHR_restricted <- rep(NA, n_sim) # standard deviation of the true effect in the trials across the late dataset (restricted to trial duration)
      sd_true_OS_LHR_unrestricted <- rep(NA, n_sim) # standard deviation of the true effect in the trials across the late dataset (unrestricted)
      CV_LHROS_observed <- rep(NA, n_sim) # coefficient of variation
      CV_LHROS_true_restricted <- rep(NA, n_sim)
      CV_LHRPFS_observed <- rep(NA, n_sim)
      CV_LHRPFS_true_restricted <- rep(NA, n_sim)
      for(i in 1:n_sim){
        path_to_file_temp <- paste0(results_location, "Datasets/Data/df_data_set_", p, "_i", i, ".csv")
        # load datasets with true values (theta) for all evidence sets
        if(file.exists(path_to_file_temp) & class(try(read.csv(path_to_file_temp))) != "try-error"){
          # record that the dataset exists in this iteration
          underlying_dataset_exist[i] <- TRUE
          # load dataset in param set p, iteration i
          underlying_dataset <- read.csv(path_to_file_temp)
          
          mean_observed_OS_LHR[i] <- mean(underlying_dataset$OS_LHR_mean)
          sd_observed_OS_LHR[i] <- sd(underlying_dataset$OS_LHR_mean)
          sd_true_OS_LHR_restricted[i] <- sd(underlying_dataset$OS_partial_ALHR_study_true)
          sd_true_OS_LHR_unrestricted[i] <- sd(underlying_dataset$OS_ALHR_study_true)
          
          CV_LHROS_observed[i] <- sd(underlying_dataset$OS_LHR_mean)/abs(mean(underlying_dataset$OS_LHR_mean))
          CV_LHROS_true_restricted[i] <- sd(underlying_dataset$OS_partial_ALHR_study_true)/abs(mean(underlying_dataset$OS_partial_ALHR_study_true))
          CV_LHRPFS_observed[i] <- sd(underlying_dataset$PFS_LHR_mean)/abs(mean(underlying_dataset$PFS_LHR_mean))
          CV_LHRPFS_true_restricted[i] <- sd(underlying_dataset$PFS_LHR_study_true)/abs(mean(underlying_dataset$PFS_LHR_study_true))
          
          if(grepl("with OS", e, fixed = TRUE)){
            ## with OS => 1st study/indication 
            # long run true target OS
            theta_unrestricted[i] <- underlying_dataset$OS_ALHR_indication_true[1]
            # short run true target OS
            theta[i] <- underlying_dataset$OS_partial_ALHR_indication_true[1]
            # observed LHR OS in target indication (with OS)
            obs_target_OS_LHR_mean[i] <- underlying_dataset$OS_LHR_mean[1]
            obs_target_OS_LHR_se[i] <- underlying_dataset$OS_LHR_se[1]
            # PFS
            theta_PFS[i] <- underlying_dataset$PFS_LHR_indication_true[1]
            
          } else {
            ## no OS => last study/indication 
            # long run true target OS
            theta_unrestricted[i] <- underlying_dataset$OS_ALHR_indication_true[which.max(underlying_dataset$indication_index)]
            # short run true target OS
            theta[i] <- underlying_dataset$OS_partial_ALHR_indication_true[which.max(underlying_dataset$indication_index)]
            # PFS
            theta_PFS[i] <- underlying_dataset$PFS_LHR_indication_true[which.max(underlying_dataset$indication_index)]
          }
        } else {
          print(path_to_file_temp)
        }
      }
      
      ## Convergence statistics ####
      ## create vector of convergence statistics for: 
      ## parameter set p (e.g. 1001), for model m (e.g. uni_ie) and evidence set e (e.g. early_with OS)
      Rhat_exist <- rep(FALSE, n_sim) # record whether Rhat data exists in this case
      Rhat_1.2_option1 <- rep(NA, n_sim) # indicator: 1 if all GR Rhat values are below 1.2 in iteration i
      Rhat_1.1_option1 <- rep(NA, n_sim) # indicator: 1 if all GR Rhat values are below 1.1 in iteration i
      Rhat_1.05_option1 <- rep(NA, n_sim) # indicator: 1 if all GR Rhat values are below 1.05 in iteration i
      Rhat_1.01_option1 <- rep(NA, n_sim) # as above for Rhat 1.01
      Rhat_1.2_option2 <- rep(NA, n_sim) # indicator: 1 if all GR Rhat values are below 1.2 in iteration i
      Rhat_1.1_option2 <- rep(NA, n_sim) # indicator: 1 if all GR Rhat values are below 1.1 in iteration i
      Rhat_1.05_option2 <- rep(NA, n_sim) # indicator: 1 if all GR Rhat values are below 1.05 in iteration i
      Rhat_1.01_option2 <- rep(NA, n_sim) # as above for Rhat 1.01
      
      for(i in 1:n_sim){
        path_to_file_temp <-paste0(results_location, "Convergence/", "df_Rhat_", p, "_", e, "_", model_set[m], "_i" , i, ".csv")
        # load datasets with true values (theta) for all evidence sets
        if(file.exists(path_to_file_temp) & class(try(read.csv(path_to_file_temp))) != "try-error"){
          # record that file exists        
          Rhat_exist[i] <- TRUE
          # load Rhat statistics for parameter set p, model m and evidence set e
          Rhat_p_m_e <- read.csv(path_to_file_temp)
          
          ## Option 1 convergence: just ignore Rhats for c's and ifBranch for mixture moodels
          # ignore Rhats for some components of mixture models
          if(grepl("mcie", model_set[m], fixed = TRUE) | grepl("mrie", model_set[m], fixed = TRUE)  ){
            element_index_c <- grepl("c[", Rhat_p_m_e$X, fixed = TRUE) 
            element_index_ifBranch <- grepl("ifBranch[", Rhat_p_m_e$X, fixed = TRUE) 
            # keep elements with no c and no ifBranch
            element_index_keep <- !(element_index_c + element_index_ifBranch)
            Rhats_option1 <- Rhat_p_m_e[element_index_keep,]
          } else {
            Rhats_option1 <- Rhat_p_m_e
          }
          
          # Option 1: are all Rhat values for the relevant parameters < limit   
          Rhat_1.2_option1[i] <- ifelse(anyNA(Rhats_option1$Point.est.), FALSE, all(Rhats_option1$Point.est. < 1.2))
          Rhat_1.1_option1[i] <- ifelse(anyNA(Rhats_option1$Point.est.), FALSE, all(Rhats_option1$Point.est. < 1.1))
          Rhat_1.05_option1[i] <- ifelse(anyNA(Rhats_option1$Point.est.), FALSE, all(Rhats_option1$Point.est. < 1.05))
          Rhat_1.01_option1[i] <- ifelse(anyNA(Rhats_option1$Point.est.), FALSE, all(Rhats_option1$Point.est. < 1.01))
          
          ## Option 2 convergence: only consider convergence in parameters used in prediction
          # depends on the model
          
          # models which only require monitoring "mu.theta[1]"
          # Uni IE with OS
          # Uni RE with OS
          if((grepl("uni_ie", model_set[m], fixed = TRUE) & grepl("with OS", e, fixed = TRUE)) | 
             (grepl("uni_re", model_set[m], fixed = TRUE) & grepl("with OS", e, fixed = TRUE))  ){
            element_index_keep <- Rhat_p_m_e$X == "mu.theta[1]"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # models which only require monitoring "mu"
          # Uni CE with OS
          # Uni CE no OS
          # Uni MCIE no OS
          if((grepl("uni_ce", model_set[m], fixed = TRUE) & grepl("with OS", e, fixed = TRUE)) | 
             (grepl("uni_ce", model_set[m], fixed = TRUE) & grepl("no OS", e, fixed = TRUE)) | 
             (grepl("uni_mcie", model_set[m], fixed = TRUE) & grepl("no OS", e, fixed = TRUE))  ){
            element_index_keep <- Rhat_p_m_e$X == "mu"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # models which require monitoring mu.branch[1,1] and mu.branch[1,2]   
          # Uni MCIE with OS
          # Uni MRIE with OS
          if((grepl("uni_mcie", model_set[m], fixed = TRUE) & grepl("with OS", e, fixed = TRUE)) | 
             (grepl("uni_mrie", model_set[m], fixed = TRUE) & grepl("with OS", e, fixed = TRUE))){
            element_index_keep <- Rhat_p_m_e$X == "mu.branch[1,1]" | Rhat_p_m_e$X == "mu.branch[1,2]"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # models which require monitoring mMu and mTau   
          # Uni RE no OS
          # Uni MRIE no OS
          if((grepl("uni_re", model_set[m], fixed = TRUE) & grepl("no OS", e, fixed = TRUE)) | 
             (grepl("uni_mrie", model_set[m], fixed = TRUE) & grepl("no OS", e, fixed = TRUE))){
            element_index_keep <- Rhat_p_m_e$X == "mMu" | Rhat_p_m_e$X == "mTau"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # no model fit for uni ie with no OS, so assume perfect convergence
          if((grepl("uni_ie", model_set[m], fixed = TRUE) & grepl("no OS", e, fixed = TRUE))  ){
            Rhats_option2 <- Rhat_p_m_e[1,]
            Rhats_option2$Point.est. <- 1}
          
          ## Surrogate models
          
          # Bi CE with OS unmatched (IE)
          # PFS model: mu.theta[1], 
          # Surrogacy model: mLambda0 and mLambda1
          if(model_set[m] == "bi_ce_unmatched" & grepl("with OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == "mu.theta[1]" | 
              Rhat_p_m_e$X == "mLambda0" |
              Rhat_p_m_e$X == "mLambda1"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Need to define j* the target indication. This is required there is no OS in the target indication
          # j*: if late j* = 8, if mid j* = 6, if early j* = 4
          j_star <- if(grepl("late", e, fixed = TRUE)){
            8
          } else if(grepl("mid", e, fixed = TRUE)){
            6
          } else {
            4
          }
          
          # Bi CE no OS unmatched (IE)
          # PFS model: mu.theta[j*]
          # Surrogacy model: mLambda0 and mLambda1
          if(model_set[m] == "bi_ce_unmatched" & grepl("no OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == paste0("mu.theta[", j_star, "]") | 
              Rhat_p_m_e$X == "mLambda0" |
              Rhat_p_m_e$X == "mLambda1"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Bi CE with OS matched (CE)
          # PFS model: mu
          # Surrogacy model: mLambda0 and mLambda1
          if(model_set[m] == "bi_ce_matched" & grepl("with OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == "mu" | 
              Rhat_p_m_e$X == "mLambda0" |
              Rhat_p_m_e$X == "mLambda1"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Bi CE no OS matched (CE)
          # PFS model: mu
          # Surrogacy model: mLambda0 and mLambda1
          if(model_set[m] == "bi_ce_matched" & grepl("no OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == "mu" | 
              Rhat_p_m_e$X == "mLambda0" |
              Rhat_p_m_e$X == "mLambda1"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Bi RE with OS unmatched (IE)
          # PFS model: mu.theta[1]
          # Surrogacy model: Lambda0[1] and Lambda1[1]
          if(model_set[m] == "bi_re_unmatched" & grepl("with OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == "mu.theta[1]" | 
              Rhat_p_m_e$X == "mLambda0[1]" |
              Rhat_p_m_e$X == "mLambda1[1]"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Bi RE no OS unmatched (IE)
          # PFS model: mu.theta[j*]
          # Surrogacy model: beta0, prec0, beta1, prec1 (prec0 and prec1 are transformations of xi0 and xi1)
          if(model_set[m] == "bi_re_unmatched" & grepl("no OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == paste0("mu.theta[", j_star, "]") | 
              Rhat_p_m_e$X == "beta0" | Rhat_p_m_e$X == "beta1" |
              Rhat_p_m_e$X == "xi0" | Rhat_p_m_e$X == "xi1"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Bi RE with OS matched (RE)
          # PFS model: mu.theta[1]
          # Surrogacy model: Lambda0[1] and Lambda1[1]
          if(model_set[m] == "bi_re_matched" & grepl("with OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == "mu.theta[1]" | 
              Rhat_p_m_e$X == "mLambda0[1]" |
              Rhat_p_m_e$X == "mLambda1[1]"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Bi RE no OS matched (RE)
          # PFS model: mu.theta[j*]
          # Surrogacy model: beta0, prec0, beta1, prec1 (prec0 and prec1 are transformations of xi0 and xi1)
          if(model_set[m] == "bi_re_matched" & grepl("no OS", e, fixed = TRUE)){
            element_index_keep <- Rhat_p_m_e$X == paste0("mu.theta[", j_star, "]") | 
              Rhat_p_m_e$X == "beta0" | Rhat_p_m_e$X == "beta1" |
              Rhat_p_m_e$X == "xi0" | Rhat_p_m_e$X == "xi1"
            Rhats_option2 <- Rhat_p_m_e[element_index_keep,]}
          
          # Option 2: are all Rhat values for the relevant parameters < limit   
          Rhat_1.2_option2[i] <- ifelse(anyNA(Rhats_option2$Point.est.), FALSE, all(Rhats_option2$Point.est. < 1.2))
          Rhat_1.1_option2[i] <- ifelse(anyNA(Rhats_option2$Point.est.), FALSE, all(Rhats_option2$Point.est. < 1.1))
          Rhat_1.05_option2[i] <- ifelse(anyNA(Rhats_option2$Point.est.), FALSE, all(Rhats_option2$Point.est. < 1.05))
          Rhat_1.01_option2[i] <- ifelse(anyNA(Rhats_option2$Point.est.), FALSE, all(Rhats_option2$Point.est. < 1.01))
          
        } else {
          print(path_to_file_temp)
        }
      }
      
      ## Assign missingness if non-convergence ####
      # based on Rhat_1.1_option2
      theta_hat_mean[!Rhat_1.1_option2] <- NA
      theta_hat_low_95 [!Rhat_1.1_option2] <- NA
      theta_hat_upper_95 [!Rhat_1.1_option2] <- NA
      theta_hat_var [!Rhat_1.1_option2] <- NA
      theta_hat_mean_ie [!Rhat_1.1_option2] <- NA 
      theta_hat_var_ie [!Rhat_1.1_option2] <- NA
      theta_hat_mean_ie_w_share [!Rhat_1.1_option2] <- NA 
      theta_hat_var_ie_w_share [!Rhat_1.1_option2] <- NA 
      
      ## Assign results ####
      
      ## assign name of model m in parameter set p and evidence set e
      df_results$Model[m] <- model_set[m]
      
      ## calculate performance metrics for: 
      ## for model m (e.g. uni_ie) and element el (parameter set p and evidence set e)
      
      ### Relative bias for model ####
      df_results$RelBias[m] <- mean((theta_hat_mean - theta)/theta, na.rm = TRUE)
      
      ## relative bias MC standard error
      df_results$RelBias_MCSE[m] <- sqrt( (1/(n_sim*(n_sim - 1)))*mean( ((theta_hat_mean - theta)/theta)^2, na.rm = TRUE ) )
      
      ### Bias #####
      df_results$Bias[m] <- mean(theta_hat_mean - theta, na.rm = TRUE)
      ## bias MC standard error
      df_results$Bias_MCSE[m] <- sqrt( (1/(n_sim*(n_sim - 1)))*mean( (theta_hat_mean - theta)^2, na.rm = TRUE ) )
      ## median bias
      df_results$Bias_median[m] <- median(theta_hat_mean - theta, na.rm = TRUE)
      ## bias for PFS and MC standard error
      df_results$Bias_PFS[m] <- mean(theta_hat_mean_PFS - theta_PFS, na.rm = TRUE)
      df_results$Bias_PFS_MCSE[m] <- sqrt( (1/(n_sim*(n_sim - 1)))*mean( (theta_hat_mean - theta)^2, na.rm = TRUE ) )

      ### Coverage #####
      cover_hat  <- mean( theta >= theta_hat_low_95 & theta <= theta_hat_upper_95   , na.rm = TRUE)
      df_results$Cover[m] <- cover_hat
      ## coverage MC standard error
      df_results$Cover_MCSE[m] <- sqrt( (cover_hat*(1 - cover_hat))/n_sim  )
      # PFS
      cover_hat_PFS  <- mean( theta_PFS >= theta_hat_low_95_PFS & theta_PFS <= theta_hat_upper_95_PFS   , na.rm = TRUE)
      df_results$Cover_PFS[m] <- cover_hat_PFS
      df_results$Cover_PFS_MCSE[m] <- sqrt( (cover_hat_PFS*(1 - cover_hat_PFS))/n_sim  )
      
      ### MSE #####
      MSE_hat  <- mean(  (theta_hat_mean - theta)^2, na.rm = TRUE  )
      df_results$MSE[m] <- MSE_hat
      ## MSE MC standard error
      df_results$MSE_MCSE[m] <- sqrt(  mean(((theta_hat_mean - theta)^2 - MSE_hat)^2, na.rm = TRUE)/(n_sim*(n_sim - 1)) )
      
      ### RMSE #####
      df_results$RMSE[m] <- sqrt(MSE_hat)
      
      ### MAE #####
      df_results$MAE[m]  <- mean(  abs(theta_hat_mean - theta), na.rm = TRUE  )
      
      ### EmpSE ####
      EmpSE_hat <- sqrt( mean( (theta_hat_mean - mean(theta_hat_mean, na.rm = TRUE))^2, na.rm = TRUE ) )
      df_results$EmpSE[m] <- EmpSE_hat
      df_results$EmpSE_MCSE[m] <- EmpSE_hat/sqrt( 2*(n_sim - 1))
      # PFS
      EmpSE_hat_PFS <- sqrt( mean( (theta_hat_mean_PFS - mean(theta_hat_mean_PFS, na.rm = TRUE))^2, na.rm = TRUE ) )
      df_results$EmpSE_PFS[m] <- EmpSE_hat_PFS
      df_results$EmpSE_PFS_MCSE[m] <- EmpSE_hat_PFS/sqrt( 2*(n_sim - 1))
      
      ### PED ####
      # compare prediction from IE model to the other models
      # Can only interpret this when there is OS in the target indication
      if(grepl("with OS", e, fixed = TRUE)){
        PED_IE_hat <- mean( abs(theta_hat_mean - theta_hat_mean_ie), na.rm = TRUE )
      } else {
        PED_IE_hat <- NA
      }
      df_results$PED_IE[m] <- PED_IE_hat
      ## PED MC standard error
      
      # compare observed OS LHR to the model predictions
      # Can only interpret this when there is OS in the target indication
      if(grepl("with OS", e, fixed = TRUE)){
        PED_obs_hat <- mean( abs(theta_hat_mean - obs_target_OS_LHR_mean), na.rm = TRUE )
      } else {
        PED_obs_hat <- NA
      }
      df_results$PED_obs[m] <- PED_obs_hat
      
      # compare prediction from IE model (with sharing) to the other models
      if(grepl("with OS", e, fixed = TRUE)){
        PED_IE_w_share_hat <- mean( abs(theta_hat_mean - theta_hat_mean_ie_w_share), na.rm = TRUE )
      } else {
        PED_IE_w_share_hat <- NA
      }
      df_results$PED_IE_w_share[m] <- PED_IE_w_share_hat
      
      ### relative PED ####
      # compare prediction from IE model to the other models
      # Can only interpret this when there is OS in the target indication
      if(grepl("with OS", e, fixed = TRUE)){
        RelPED_IE_hat <- mean( abs(theta_hat_mean - theta_hat_mean_ie)/abs(theta_hat_mean_ie), na.rm = TRUE )
      } else {
        RelPED_IE_hat <- NA
      }
      df_results$RelPED_IE[m] <- RelPED_IE_hat
      ## PED MC standard error
      
      # compare observed target LHR OS to the other models
      # Can only interpret this when there is OS in the target indication
      if(grepl("with OS", e, fixed = TRUE)){
        RelPED_obs_hat <- mean( abs(theta_hat_mean - obs_target_OS_LHR_mean)/abs(obs_target_OS_LHR_mean), na.rm = TRUE )
      } else {
        RelPED_obs_hat <- NA
      }
      df_results$RelPED_obs[m] <- RelPED_obs_hat
      
      # compare prediction from IE model (with sharing) to the other models
      # Can only interpret this when there is OS in the target indication
      if(grepl("with OS", e, fixed = TRUE)){
        RelPED_IE_w_share_hat <- mean( abs(theta_hat_mean - theta_hat_mean_ie_w_share)/abs(theta_hat_mean_ie_w_share), na.rm = TRUE )
      } else {
        RelPED_IE_w_share_hat <- NA
      }
      df_results$RelPED_IE_w_share[m] <- RelPED_IE_w_share_hat
      
      ### Average ModSE  ####
      ModSE_hat <- sqrt( mean(theta_hat_var, na.rm = TRUE)  )
      df_results$ModSE[m]  <- ModSE_hat
      ## Average ModSE MC standard error
      var_var_theta_hat <- (1/(n_sim - 1))*(mean( (theta_hat_var - mean(theta_hat_var, na.rm = TRUE))^2, na.rm = TRUE ))
      df_results$ModSE_MCSE[m]  <- sqrt( var_var_theta_hat/(4*n_sim*(ModSE_hat)^2 ) )
      # IE model
      var_var_theta_hat_ie <- (1/(n_sim - 1))*(mean( (theta_hat_var_ie - mean(theta_hat_var_ie, na.rm = TRUE))^2, na.rm = TRUE ))
      ModSE_hat_ie <- sqrt(mean(theta_hat_var_ie, na.rm = TRUE))
      # PFS
      ModSE_hat_PFS <- sqrt( mean(theta_hat_var_PFS, na.rm = TRUE)  )
      df_results$ModSE_PFS[m]  <- ModSE_hat_PFS
      var_var_theta_hat_PFS <- (1/(n_sim - 1))*(mean( (theta_hat_var_PFS - mean(theta_hat_var_PFS, na.rm = TRUE))^2, na.rm = TRUE ))
      df_results$ModSE_PFS_MCSE[m]  <- sqrt( var_var_theta_hat_PFS/(4*n_sim*(ModSE_hat_PFS)^2 ) )
      
      ### splittingModSeR  ####
      # compare prediction from IE model to the other models
      # Can only interpret this when there is OS in the target indication
      # based on the model SE: ModSE_share/ModSE_IP
      # note I have switched around the ratio - the sharing methods SE is a proportion of the IP method
      if(grepl("with OS", e, fixed = TRUE)){
        splittingModSeR_IE_hat <- mean( ModSE_hat/ModSE_hat_ie, na.rm = TRUE )
      } else {
        splittingModSeR_IE_hat <- NA
      }
      df_results$splittingModSeR_IE[m] <- splittingModSeR_IE_hat
      ## splittingSeR MC standard error
      # Delta method : Beyene Methods for confidence interval estimation of a ratio parameter with application to location quotients
      # gamma = alpha/beta, gamma_hat = alpha_hat/beta_hat
      # vcov(alpha_hat, beta_hat) = [V_11, V_12]
      #                             [V_12, V_22]
      # where V_11 = var(alpha_hat), V_12 = cov(alpha_hat, beta_hat), V_22 = var(beta_hat)
      # From delta method, var(gamma_hat) = 1/beta_hat^2 (V_11 - 2.gamma_hat.V_12 + gamma_hat^2.V_22) 
      # in our application, gamma_hat = splittingModSeR_IE_hat = ModSE_share_hat/ModSE_IP_hat
      gamma_hat <- splittingModSeR_IE_hat # ratio of interest
      alpha_hat <- ModSE_hat 
      V_11 <- sqrt( var_var_theta_hat/(4*n_sim*(ModSE_hat)^2 ) )
      beta_hat <- ModSE_hat_ie
      V_22 <- sqrt( var_var_theta_hat_ie/(4*n_sim*(ModSE_hat_ie)^2 ) )
      # estimate covariance between alpha_hat and beta_hat using bootstrap
      # i.e. correlation between ModSE_hat and ModSE_hat_ie
      alpha_hat_boot <- rep(NA, 1000)
      beta_hat_boot <- rep(NA, 1000)
      for(boot in 1:1000){
        theta_hat_var_boot <- sample(theta_hat_var, size = length(theta_hat_var), replace = TRUE)
        theta_hat_var_ie_boot <- sample(theta_hat_var_ie, size = length(theta_hat_var_ie), replace = TRUE)
        
        alpha_hat_boot[boot] <- sqrt( mean(theta_hat_var_boot, na.rm = TRUE)  )
        beta_hat_boot[boot] <- sqrt( mean(theta_hat_var_ie_boot, na.rm = TRUE)  )
      }
      try({
        V_12 <- cov(data.frame(alpha_hat_boot,beta_hat_boot), use = "complete.obs")[1,2]
        var_gamma_hat <- (1/(beta_hat^2))*( V_11 - 2*gamma_hat*V_12 + (gamma_hat^2)*V_22 )
        df_results$splittingModSeR_IE_MCSE[m] <- sqrt(var_gamma_hat)}
      )
      
      # compare observed target LHR OS to the other models
      if(grepl("with OS", e, fixed = TRUE)){
        splittingModSeR_obs_hat <- mean( ModSE_hat/obs_target_OS_LHR_se, na.rm = TRUE )
      } else {
        splittingModSeR_obs_hat <- NA
      }
      df_results$splittingModSeR_obs[m] <- splittingModSeR_obs_hat
      
      # compare prediction from IE model (with sharing) to the other models
      if(grepl("with OS", e, fixed = TRUE)){
        splittingModSeR_IE_w_share_hat <- mean( ModSE_hat/sqrt(mean(theta_hat_var_ie_w_share, na.rm = TRUE)), na.rm = TRUE )
      } else {
        splittingModSeR_IE_w_share_hat <- NA
      }
      df_results$splittingModSeR_IE_w_share[m] <- splittingModSeR_IE_w_share_hat
      # PFS
      if(grepl("with OS", e, fixed = TRUE)){
        splittingModSeR_IE_w_share_hat_PFS <- mean( ModSE_hat_PFS/sqrt(mean(theta_hat_var_ie_w_share, na.rm = TRUE)), na.rm = TRUE )
      } else {
        splittingModSeR_IE_w_share_hat_PFS <- NA
      }
      df_results$splittingModSeR_IE_w_share_PFS[m] <- splittingModSeR_IE_w_share_hat_PFS
      
      # ###
      # if(grepl("with OS", e, fixed = TRUE)){
      #   Prec_increase_precision_model_vs_IE_w_share_hat <- mean( ModSE_hat/sqrt(mean(theta_hat_var_ie, na.rm = TRUE)), na.rm = TRUE )
      # } else {
      #   Prec_increase_precision_model_vs_IE_w_share_hat <- NA
      # }
      # df_results$Prec_increase_precision_model_vs_IE_w_share[m] <- 
      # ## MC standard error
      # df_results$Prec_increase_precision_model_vs_IE_w_share_MCSE[m] <- 
      
      
      ### splittingEmpSeR  ####
      # compare prediction from IE model to the other models
      # Can only interpret this when there is OS in the target indication
      # based on the empirical SE: EmpSE_share/EmpSE_IP
      # note I have switched around the ratio - the sharing methods SE is a proportion of the IP method
      if(grepl("with OS", e, fixed = TRUE)){
        splittingEmpSeR_IE_hat <- mean( EmpSE_hat/sqrt(mean((theta_hat_mean_ie - mean(theta_hat_mean_ie, na.rm = TRUE))^2, na.rm = TRUE)), na.rm = TRUE )
      } else {
        splittingEmpSeR_IE_hat <- NA
      }
      df_results$splittingEmpSeR_IE[m] <- splittingEmpSeR_IE_hat
      ## MC standard error
      
      # compare observed target LHR OS to the other models
      if(grepl("with OS", e, fixed = TRUE)){
        splittingEmpSeR_obs_hat <- mean( EmpSE_hat/sqrt(mean((obs_target_OS_LHR_mean - mean(obs_target_OS_LHR_mean))^2)), na.rm = TRUE )
      } else {
        splittingEmpSeR_obs_hat <- NA
      }
      df_results$splittingEmpSeR_obs[m] <- splittingEmpSeR_obs_hat
      
      # compare prediction from IE model (with sharing) to the other models
      if(grepl("with OS", e, fixed = TRUE)){
        splittingEmpSeR_IE_w_share_hat <- mean( EmpSE_hat/sqrt(mean((theta_hat_mean_ie_w_share - mean(theta_hat_mean_ie_w_share, na.rm = TRUE))^2, na.rm = TRUE)), na.rm = TRUE )
      } else {
        splittingEmpSeR_IE_w_share_hat <- NA
      }
      df_results$splittingEmpSeR_IE_w_share[m] <- splittingEmpSeR_IE_w_share_hat
      
      ### Ratio of E(ModSE^2)/EmpSE^2  ####
      df_results$Ratio_E_ModSE_sq_over_EmpSE_sq[m] <- mean(ModSE_hat^2, na.rm = TRUE)/(EmpSE_hat^2)
      
      ### Convergence  ####
      df_results$Prop_Rhat_1.2_option1[m]  <- mean(Rhat_1.2_option1, na.rm = TRUE)
      df_results$Prop_Rhat_1.1_option1[m]  <- mean(Rhat_1.1_option1, na.rm = TRUE)
      df_results$Prop_Rhat_1.05_option1[m]  <- mean(Rhat_1.05_option1, na.rm = TRUE)
      df_results$Prop_Rhat_1.01_option1[m]  <- mean(Rhat_1.01_option1, na.rm = TRUE)
      
      df_results$Prop_Rhat_1.2_option2[m]  <- mean(Rhat_1.2_option2, na.rm = TRUE)
      df_results$Prop_Rhat_1.1_option2[m]  <- mean(Rhat_1.1_option2, na.rm = TRUE)
      df_results$Prop_Rhat_1.05_option2[m]  <- mean(Rhat_1.05_option2, na.rm = TRUE)
      df_results$Prop_Rhat_1.01_option2[m]  <- mean(Rhat_1.01_option2, na.rm = TRUE)
      
      
      ### Description of dataset  ####
      # mean of the observed effect in the trials across the late dataset
      df_results$mean_observed_OS_LHR[m]  <- mean(mean_observed_OS_LHR, na.rm = TRUE)
      # standard deviation of the observed effect in the trials across the late dataset
      df_results$sd_observed_OS_LHR[m]  <- mean(sd_observed_OS_LHR, na.rm = TRUE)
      # standard deviation of the true effect in the trials across the late dataset (restricted to trial duration)
      df_results$sd_true_OS_LHR_restricted[m]  <- mean(sd_true_OS_LHR_restricted, na.rm = TRUE)
      # standard deviation of the true effect in the trials across the late dataset (unrestricted)
      df_results$sd_true_OS_LHR_unrestricted[m]  <- mean(sd_true_OS_LHR_unrestricted, na.rm = TRUE)
      # coefficient of variation
      df_results$CV_LHROS_observed[m] <- mean(CV_LHROS_observed, na.rm = TRUE)
      df_results$CV_LHROS_true_restricted[m] <- mean(CV_LHROS_true_restricted, na.rm = TRUE)
      df_results$CV_LHRPFS_observed[m] <- mean(CV_LHRPFS_observed, na.rm = TRUE)
      df_results$CV_LHRPFS_true_restricted[m] <- mean(CV_LHRPFS_true_restricted, na.rm = TRUE)
      
      ### Existence of datasets  ####
      df_results$Num_target_posterior_exist[m]  <- sum(target_posterior_exist)
      df_results$Num_underlying_dataset_exist[m]  <- sum(underlying_dataset_exist)
      df_results$Num_Rhat_exist[m]  <- sum(Rhat_exist)
      
      ### Results per iteration ####
      Bias_per_iteration[,m] <-  theta_hat_mean - theta
      point_estimate_per_iteration[,m] <- theta_hat_mean
      ModSE_per_iteration[,m] <- sqrt(theta_hat_var)
      Rhat_1.1_option1_per_iteration[,m] <- Rhat_1.1_option1
      Rhat_1.1_option2_per_iteration[,m] <- Rhat_1.1_option2
      
    }
    print(paste("finished model", m, model_set[m]))
    }
  
  # save results
  # name of result depends on parameter set p and evidence set e
  name_p_e <- paste0(p,"_", e)
  write.csv(df_results, file = paste0(results_location, "Result_elements/", name_p_e, ".csv"), row.names = FALSE)
  
  write.csv(Bias_per_iteration, file = paste0(results_location, "Result_elements/Bias_per_iteration", name_p_e, ".csv"), row.names = FALSE)
  write.csv(point_estimate_per_iteration, file = paste0(results_location, "Result_elements/point_estimate_per_iteration", name_p_e, ".csv"), row.names = FALSE)
  write.csv(ModSE_per_iteration, file = paste0(results_location, "Result_elements/ModSE_per_iteration", name_p_e, ".csv"), row.names = FALSE)
  write.csv(Rhat_1.1_option1_per_iteration, file = paste0(results_location, "Result_elements/Rhat_1.1_option1_per_iteration", name_p_e, ".csv"), row.names = FALSE)
  write.csv(Rhat_1.1_option2_per_iteration, file = paste0(results_location, "Result_elements/Rhat_1.1_option2_per_iteration", name_p_e, ".csv"), row.names = FALSE)
  
  #print(paste0(results_location, "Result_elements/", name_p_e, ".csv"))

}


## loop over parameter sets (p) and evidence sets (e)  #####
# to calculate the performance metrics escribed in the overleaf
# fills in the list of results

## each combination of p and e is called an element (el)
# create matrix of elements
m_el <- expand.grid(param_set, evidence_set)
colnames(m_el) <- c("p", "e")
# for set 5 and 6, remove "with OS" scenarios 
row_set56 <- round(m_el$p/1000,0) == 5 | round(m_el$p/1000,0) == 6
row_remove <- row_set56 & (m_el$e == "early_with OS" | m_el$e == "mid_with OS" | m_el$e == "late_with OS")
m_el <- m_el[!row_remove,]
# for set 7 and 8, remove "no OS" scenarios 
row_set78 <- round(m_el$p/1000,0) == 7 | round(m_el$p/1000,0) == 8
row_remove <- row_set78 & (m_el$e == "early_no OS" | m_el$e == "mid_no OS" | m_el$e == "late_no OS")
m_el <- m_el[!row_remove,]

## loop over elements to calculate results
if(user == "Viking"){
  # assign iteration number in Viking
  args <- commandArgs(trailingOnly=TRUE)
  viking_iteration <- as.integer(args[1])
  
  m_el
  print(paste("viking iteration", viking_iteration))
  print(paste("Parameter set",m_el$p[viking_iteration]))
  print(paste("Evidence set",m_el$e[viking_iteration]))
  
  # calculate and save results for element el
  fn_calc_results_p_e(p = m_el$p[viking_iteration], e = m_el$e[viking_iteration])
  
} else {
  for(el in 1:nrow(m_el)){
    # calculate and save results for element el
    fn_calc_results_p_e(p = m_el$p[el], e = m_el$e[el])
    
    # update on progress
    cat('\r', paste0(round(el/nrow(m_el)*100,0), "% complete"))
  }
}





