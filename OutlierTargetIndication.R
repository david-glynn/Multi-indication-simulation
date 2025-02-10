
user <- if(Sys.info()["user"] == "dpg504" & Sys.info()["sysname"] == "Windows"){
  "David"} else 
    if(Sys.info()["user"] == "dpg504" & grepl("viking", Sys.info()["nodename"], fixed = TRUE)){
      "Viking"} 

## for iteration i (each array in Viking does one iteration)
if(user == "Viking"){
  # assign iteration number in Viking
  args <- commandArgs(trailingOnly=TRUE)
  viking_iteration <- as.integer(args[1])
  i <- viking_iteration
} else {
  i <- 1
}

# setwd("G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/Simulation files")
source("SimulationSetUp.R")
v_seeds_iteration <- unlist(as.vector(read.csv("v_seeds_iteration.csv")))

  
for(set in c(5, 7)){
  # no OS in target
  # set 5 = set 1 (all parameters homogeneous expect for M) with the target being an outlier
  # set 6 = set 4 (all parameters heterogeneous) with the target being an outlier
  
  # with OS in target
  # set 7 = set 1 (all parameters homogeneous expect for M) with the target being an outlier
  # set 8 = set 4 (all parameters heterogeneous) with the target being an outlier
  
  # start with non outlier target scenario and modify it
  # load up an existing dataset (without target outlier). from df_set1_scenario.csv etc
  # this will provide the parameter values for the different scenarios
  if(set == 5 | set == 7){
    # no hetero in MSM parameters
    # Sets 5 and 7 use the same parameters as Set 1 (but adds an outlier target indication)
    df_scenarios <- read.csv(paste0("df_set",1 ,"_scenarios.csv"))
    df_scenarios$Parameter_set <- 5000 + 1:50
  } else if(set == 6 | set == 8){
    # with hetero in MSM parameters
    # Sets 6 and 8 use the same parameters as Set 4 (but adds an outlier target indication)
    df_scenarios <- read.csv(paste0("df_set",4 ,"_scenarios.csv"))
    df_scenarios$Parameter_set <- 6000 + 1:50
  }
  
  # all sets (5, 6, 7, 8) have the same outlier values
  df_scenarios$M_01_outlier_SD <- 1.96 # moderate outlier
  df_scenarios$lambda_01_outlier_SD <- 0
  df_scenarios$lambda_02_outlier_SD <- 0
  df_scenarios$delta_outlier_SD <- 0
  # outlier within indication hetero same as non-outliers
  df_scenarios$log_lambda_01_outlier_CV_w <- df_scenarios$log_lambda_01_CV_w
  df_scenarios$log_lambda_02_outlier_CV_w <- df_scenarios$log_lambda_02_CV_w
  df_scenarios$log_delta_outlier_CV_w <- df_scenarios$log_delta_CV_w
  df_scenarios$log_M_01_outlier_CV_w <- df_scenarios$log_M_01_CV_w
  
  # when simulating heterogeneous parameters values, within and between indications
  # need a different seed for each iteration i
  # use seed for iteration i
  set.seed(v_seeds_iteration[i])
  
  ## initialise data frame to hold result for the chosen set
  # example dataset just used to get the names of the columns
  example_dataset <- read.csv(paste0(results_location, "Datasets/Data/df_data_set_", 1026, "_i", 1, ".csv"))[1,]
  df_outlier_target_data_set_i <- as.data.frame(matrix(NA, nrow = 50, ncol = ncol(example_dataset)))
  colnames(df_outlier_target_data_set_i) <- names(example_dataset)
  
  # p = 26
  # for each scenario (5026, 5027,...5050 or 6026,...6050) 
  # note: miss out 1-25 as these are for small effect
  for(p in 26:50){
    
    # start.time <- Sys.time()
    
    # for the settings in parameter set p
    scenario <- df_scenarios[p,]
    
    ## initialise data set for iteration i and scenario p
    # only simulates the (outlier) target indication (i.e. one row)
    df_data_set_i_p <- data.frame(
      # scenario
      scenario = as.numeric(gsub('[^[:alnum:] ]','',scenario$Parameter_set)),
      # indication level parameters (log scale)
      log_lambda_01_indication = rep(NA, 1),
      log_lambda_02_indication = rep(NA, 1),
      log_delta_indication = rep(NA, 1),
      log_M_01_indication = rep(NA, 1),
      # true study level parameter values used in the simulation (log scale)
      log_lambda_01_ctrl = rep(NA, 1),
      log_lambda_02_ctrl = rep(NA, 1),
      log_delta_ctrl = rep(NA, 1),
      log_M_01 = rep(NA, 1),
      # indication level parameters (natural scale)
      lambda_01_indication = rep(NA, 1),
      lambda_02_indication = rep(NA, 1),
      delta_indication = rep(NA, 1),
      M_01_indication = rep(NA, 1),
      # true study level parameter values used in the simulation (natural scale)
      lambda_01_ctrl = rep(NA, 1),
      lambda_02_ctrl = rep(NA, 1),
      delta_ctrl = rep(NA, 1),
      M_01 = rep(NA, 1),
      lambda_12_ctrl = rep(NA, 1),
      lambda_01_trt = rep(NA, 1),
      lambda_02_trt = rep(NA, 1),
      lambda_12_trt = rep(NA, 1),
      # true study level outcome values used in the simulation
      PFS_LHR_study_true = rep(NA, 1), # true value of log hazard ratio for PFS based on multistate parameters
      OS_ALHR_study_true = rep(NA, 1), # true long run true LHR OS value based on multistate parameters
      OS_partial_ALHR_study_true = rep(NA, 1), # average LHR OS value over the relevant trial duration
      # true indication level outcome values (used in performance measurement)
      PFS_LHR_indication_true = rep(NA, 1), 
      OS_ALHR_indication_true = rep(NA, 1), # long run true LHR OS value
      OS_partial_ALHR_indication_true = rep(NA, 1), # average LHR OS value over the relevant trial duration
      PFS_LHR_indication_true_MCerror = rep(NA, 1), # MC error when calculating indication level PFS LHR in the presence of within indication heterogeneity
      OS_ALHR_indication_true_MCerror = rep(NA, 1), 
      OS_partial_ALHR_indication_true_MCerror = rep(NA, 1), 
      # study details
      TimefromStart_M = rep(NA, 1),  # Full trial duration (from start of recruitment to reporting results) The time difference between the ‘StartDate’ and ‘TimePoint’, measured in months. 
      Arm1_N = rep(NA, 1), # Control. Number of randomised patients in Arm 1 of the trial
      Arm2_N = rep(NA, 1), # Treatment. Number of randomised patients in Arm 2 of the trial     
      # observed outcomes in each study
      PFS_LHR_mean = rep(NA, 1), # observed mean log hazard ratio for PFS in study s
      PFS_LHR_se = rep(NA, 1), # observed standard error for log hazard ratio for PFS in study s
      OS_LHR_mean = rep(NA, 1), # observed mean log hazard ratio for OS in study s
      OS_LHR_se = rep(NA, 1), # observed standard error for log hazard ratio for OS in study s
      Prop_PFS_events_ctrl = rep(NA, 1), # proportion of PFS events observed within the trial followup in the control arm
      Prop_PFS_events_trt = rep(NA, 1), # proportion of PFS events observed within the trial followup in the treatment arm
      Prop_OS_events_ctrl = rep(NA, 1),
      Prop_OS_events_trt = rep(NA, 1)
    )
    
    ### Generate multistate parameters ####
    
    ## assign parameter values in this scenario
    # inputs all on log scale to simulate from normal distributions
    # grand mean for intervention
    log_lambda_01_mu <- scenario$log_lambda_01_mu
    log_lambda_02_mu <- scenario$log_lambda_02_mu
    log_delta_mu <- scenario$log_delta_mu
    log_M_01_mu <- scenario$log_M_01_mu
    # within indication hetero (defined in terms of coefficient of variation, CV)
    log_lambda_01_tau_w <- scenario$log_lambda_01_CV_w*abs(scenario$log_lambda_01_mu)
    log_lambda_02_tau_w <- scenario$log_lambda_02_CV_w*abs(scenario$log_lambda_02_mu)
    log_delta_tau_w <- scenario$log_delta_CV_w*abs(scenario$log_delta_mu) 
    log_M_01_tau_w <- scenario$log_M_01_CV_w*abs(scenario$log_M_01_mu)
    # between indication hetero (defined in terms of coefficient of variation, CV)
    log_lambda_01_tau_bw <- scenario$log_lambda_01_CV_bw*abs(scenario$log_lambda_01_mu)  
    log_lambda_02_tau_bw <- scenario$log_lambda_02_CV_bw*abs(scenario$log_lambda_02_mu)
    log_delta_tau_bw <- scenario$log_delta_CV_bw*abs(scenario$log_delta_mu)   
    log_M_01_tau_bw <- scenario$log_M_01_CV_bw*abs(scenario$log_M_01_mu)
    # outlier specific within indication heterogeneity (defined in terms of coefficient of variation, CV)
    log_lambda_01_tau_w_outlier	<- scenario$log_lambda_01_outlier_CV_w*abs(scenario$log_lambda_01_mu)
    log_lambda_02_tau_w_outlier 	<- scenario$log_lambda_02_outlier_CV_w*abs(scenario$log_lambda_02_mu)
    log_delta_tau_w_outlier	<- scenario$log_delta_outlier_CV_w*abs(scenario$log_delta_mu)
    log_M_01_tau_w_outlier <- scenario$log_M_01_outlier_CV_w*abs(scenario$log_M_01_mu)
    
    
    ## simulate multi state parameters taking account of and within and between indication heterogeneity
    # note the same assumptions are used for the target and non-target datasets
    
    # Assign true INDICATION LEVEL multi state parameter values
    # This reflects the between indication heterogeneity in parameter values (if there is any in the simulation scenario)
    
    #### Draw indication level parameters for the OUTLIER indication ####
    # define mean of outlier indicaiton
    # the outlier mean will be a specific distance from the main effect
    # choose multipler to put outlier mean X SDs higher than the non-outlier mean (less effective)
    # where the SDs are the between indication heterogeneiy in the non-outlier indications
    above <- TRUE # 
    df_data_set_i_p$log_lambda_01_indication[1] <- log_lambda_01_mu + ifelse(above, 1, -1)*scenario$lambda_01_outlier_SD*log_lambda_01_tau_bw
    df_data_set_i_p$log_lambda_02_indication[1] <- log_lambda_02_mu + ifelse(above, 1, -1)*scenario$lambda_02_outlier_SD*log_lambda_02_tau_bw
    df_data_set_i_p$log_delta_indication[1] <- log_delta_mu + ifelse(above, 1, -1)*scenario$delta_outlier_SD*log_delta_tau_bw
    df_data_set_i_p$log_M_01_indication[1] <- log_M_01_mu + ifelse(above, 1, -1)*scenario$M_01_outlier_SD*log_M_01_tau_bw
    
    
    # assign true STUDY LEVEL multi state parameter values
    # this reflects within indication heterogeneity in parameter values (if there is any in the simulation scenario)
    
    # draw study level parameters for the outlier indication (log scale)
    # done separately because the outlier indication may have different within indication heterogeneity than non-outliers
    
    # lambda 01
    df_data_set_i_p$log_lambda_01_ctrl[1] <- rnorm(1, df_data_set_i_p$log_lambda_01_indication[1], # indication specific mean for study s
                                                   log_lambda_01_tau_w_outlier) # within indication heterogeneity: tau_w
    # lambda 02  
    df_data_set_i_p$log_lambda_02_ctrl[1] <- rnorm(1, df_data_set_i_p$log_lambda_02_indication[1], # indication specific mean for study s
                                                   log_lambda_02_tau_w_outlier) # within indication heterogeneity: tau_w
    # M01
    df_data_set_i_p$log_M_01[1] <- rnorm(1, df_data_set_i_p$log_M_01_indication[1], # indication specific mean for study s
                                         log_M_01_tau_w_outlier) # within indication heterogeneity: tau_w
    # delta
    df_data_set_i_p$log_delta_ctrl[1] <- rnorm(1, df_data_set_i_p$log_delta_indication[1], # indication specific mean for study s
                                               log_delta_tau_w_outlier) # within indication heterogeneity: tau_w
    
    # convert log to natural scale for use in formulas
    df_data_set_i_p$lambda_01_indication = exp(df_data_set_i_p$log_lambda_01_indication)
    df_data_set_i_p$lambda_02_indication = exp(df_data_set_i_p$log_lambda_02_indication)
    df_data_set_i_p$delta_indication = exp(df_data_set_i_p$log_delta_indication)
    df_data_set_i_p$M_01_indication = exp(df_data_set_i_p$log_M_01_indication)
    df_data_set_i_p$lambda_01_ctrl = exp(df_data_set_i_p$log_lambda_01_ctrl)
    df_data_set_i_p$lambda_02_ctrl = exp(df_data_set_i_p$log_lambda_02_ctrl)
    df_data_set_i_p$delta_ctrl = exp(df_data_set_i_p$log_delta_ctrl)
    df_data_set_i_p$M_01 = exp(df_data_set_i_p$log_M_01)
    
    # assign values of derived multistate parameters
    df_data_set_i_p$lambda_12_ctrl <- df_data_set_i_p$lambda_02_ctrl*df_data_set_i_p$delta_ctrl
    df_data_set_i_p$lambda_01_trt <- df_data_set_i_p$lambda_01_ctrl*df_data_set_i_p$M_01
    df_data_set_i_p$lambda_02_trt <- df_data_set_i_p$lambda_02_ctrl
    df_data_set_i_p$lambda_12_trt <- df_data_set_i_p$lambda_12_ctrl
    
    ### Calculate the sample size in each trial arm ####
    
    # calculate multistate output for study s in iteration i
    multistate_output_s_i <- fn_HR_OS_PFS(
      lambda_01_ctrl = df_data_set_i_p$lambda_01_ctrl[1],
      lambda_02_ctrl = df_data_set_i_p$lambda_02_ctrl[1], 
      lambda_12_ctrl = df_data_set_i_p$lambda_12_ctrl[1],
      lambda_01_trt = df_data_set_i_p$lambda_01_trt[1],
      lambda_02_trt = df_data_set_i_p$lambda_02_trt[1], 
      lambda_12_trt = df_data_set_i_p$lambda_12_trt[1],
      max_time_months_calculate = 12*100 # 100 years
    )
    
    ### Approximate exponential OS hazard ####
    # from multistate OS (non-proportional) hazard
    # for study s in interation i
    
    # for an exponential distribution 
    # rate = log(2)/median
    # plot survival curve
    # plot(multistate_output_s_i$time_points_calculate, multistate_output_s_i$S_OS_t_ctrl, xlim = c(0, 50))
    # note that the 1st element is month 0 so subtract one month
    # power caculation based on true control arm + 0.7 HR
    time_to_median_OS_ctrl <- multistate_output_s_i$time_points_calculate[which.min(abs(multistate_output_s_i$S_OS_t_ctrl - 0.5)) - 1]
    # old approach assumed that power calculation is based on the true effect size
    # time_to_median_OS_trt <- which.min(abs(multistate_output_s_i$S_OS_t_trt - 0.5)) - 1
    
    ### Calculate the trial duration ####
    # this depends on the maturity assumption and the prognosis
    # assume no recuritment period
    # Power on OS, set trial duration assuming X% of OS events within trial duration in control arm
    # where X is set by scenario
    multistate_output_s_i$time_points_calculate[which.min(abs(multistate_output_s_i$S_OS_t_ctrl - (1 -0.8) )) - 1]
    time_to_Xprec_OS_target <- multistate_output_s_i$time_points_calculate[which.min(abs(multistate_output_s_i$S_OS_t_ctrl - (1 -scenario$Prop_OS_event_target) )) - 1]
    time_to_Xprec_OS_non_target <- multistate_output_s_i$time_points_calculate[which.min(abs(multistate_output_s_i$S_OS_t_ctrl - (1 -scenario$Prop_OS_event_non_target) )) - 1]
    
    
    # if s target indication
    df_data_set_i_p$TimefromStart_M[1] <- time_to_Xprec_OS_target
    
    ### Calculate sample size ####
    # based on prognosis and trial duration
    sample_size_s_i <- nSurvival(
      lambda1 = log(2)/time_to_median_OS_ctrl,  # event hazard rate OS in control arm
      lambda2 = (log(2)/time_to_median_OS_ctrl)*0.7,  # required hazard ratio of 0.7
      eta = 0,  # equal dropout hazard rate for both groups. Assume zero
      Ts = df_data_set_i_p$TimefromStart_M[1], # adjusted to reflect the maturity assumption
      Tr = 0.001, # Aprox instant accrual assumed. 
      sided = 2,
      alpha = .05,
      beta = 0.10 # 1-beta = 90% power
    )
    
    ## assign sample size for study s
    # reduce sample size to X% of fully powered sample size to model imprecision
    
    # target indications
    df_data_set_i_p$Arm1_N[1] <- df_data_set_i_p$Arm2_N[1] <- as.integer(sample_size_s_i$n*scenario$Prop_sample_size_target)
    
    
    
    ### Generate true PFS and OS outcomes ####
    
    ## STUDY LEVEL true PFS and OS
    
    ##  calculate multistate output for study s in iteration i
    # use to calculate the long run LHR OS (100 years)
    multistate_output_s_i <- fn_HR_OS_PFS(
      lambda_01_ctrl = df_data_set_i_p$lambda_01_ctrl[1],
      lambda_02_ctrl = df_data_set_i_p$lambda_02_ctrl[1], 
      lambda_12_ctrl = df_data_set_i_p$lambda_12_ctrl[1],
      lambda_01_trt = df_data_set_i_p$lambda_01_trt[1],
      lambda_02_trt = df_data_set_i_p$lambda_02_trt[1], 
      lambda_12_trt = df_data_set_i_p$lambda_12_trt[1],
      max_time_months_calculate = 12*100 # 100 years - long run ALHR
    )
    ## assign true STUDY LEVEL values for PFS and OS for study s
    df_data_set_i_p$PFS_LHR_study_true[1] <- log(multistate_output_s_i$HR_PFS)
    df_data_set_i_p$OS_ALHR_study_true[1] <- log(multistate_output_s_i$AHR_OS)
    
    # calculate the ALHR OS over the trial duration
    multistate_output_s_i <- fn_HR_OS_PFS(
      lambda_01_ctrl = df_data_set_i_p$lambda_01_ctrl[1],
      lambda_02_ctrl = df_data_set_i_p$lambda_02_ctrl[1], 
      lambda_12_ctrl = df_data_set_i_p$lambda_12_ctrl[1],
      lambda_01_trt = df_data_set_i_p$lambda_01_trt[1],
      lambda_02_trt = df_data_set_i_p$lambda_02_trt[1], 
      lambda_12_trt = df_data_set_i_p$lambda_12_trt[1],
      max_time_months_calculate = df_data_set_i_p$TimefromStart_M[1] # ALHR over the trial duration
    )
    df_data_set_i_p$OS_partial_ALHR_study_true[1] <- log(multistate_output_s_i$AHR_OS)
    
    
    ## INDICATION LEVEL true PFS and OS
    # Calculate true INDICATION LEVEL PFS and OS LHR
    
    # the LHR OS and PFS calculation is non-linear in lambda01, lambda02 etc 
    # need to take account of this non-linearity in calculating indication expectation for LHR OS
    if(apply_non_linear_adjustment & log_lambda_01_tau_w + log_lambda_02_tau_w + log_delta_tau_w + log_M_01_tau_w > 0){
      print(paste0("Begin non-linear adjustment")) 
    }
    
    if(apply_non_linear_adjustment & log_lambda_01_tau_w + log_lambda_02_tau_w + log_delta_tau_w + log_M_01_tau_w > 0){
      # if there is within indication heterogeneity, need to estimate the true LHR using simulation 
      # drawing a number of simulations from the (gaussian) distribution of within indication heterogeneity,
      # calculating LHR OS and PFS for each, then taking the mean.
      
      v_ALHR_OS <- rep(NA, n_sim2) # vector to hold long run ALHR OS (100 years) results
      v_LHR_PFS <- rep(NA, n_sim2)
      v_partial_ALHR_OS <- rep(NA, n_sim2) # vector to hold trial duration ALHR OS results
      
      # simulate values of lambda01 etc
      # mean is indication level mean, SD is the within indication SD
      # simulate on log scale
      v_log_lambda_01_ctrl_temp <- rnorm(n_sim2, df_data_set_i_p$log_lambda_01_indication[1], log_lambda_01_tau_w) 
      v_log_lambda_02_ctrl_temp <- rnorm(n_sim2, df_data_set_i_p$log_lambda_02_indication[1], log_lambda_02_tau_w) 
      v_log_delta_temp <- rnorm(n_sim2, df_data_set_i_p$log_delta_indication[1], log_delta_tau_w)
      v_log_M_01_temp <- rnorm(n_sim2, df_data_set_i_p$log_M_01_indication[1], log_M_01_tau_w)
      
      # convert to natural scale
      v_lambda_01_ctrl_temp <- exp(v_log_lambda_01_ctrl_temp)
      v_lambda_02_ctrl_temp <- exp(v_log_lambda_02_ctrl_temp)
      v_delta_temp <- exp(v_log_delta_temp)
      v_M_01_temp <- exp(v_log_M_01_temp)
      
      # derive other multistate parameters
      v_lambda_12_ctrl_temp = v_lambda_02_ctrl_temp*v_delta_temp
      v_lambda_01_trt_temp = v_lambda_01_ctrl_temp*v_M_01_temp
      v_lambda_02_trt_temp = v_lambda_02_ctrl_temp
      v_lambda_12_trt_temp = v_lambda_12_ctrl_temp
      
      for(n in 1:n_sim2){
        # use the values above to calculate LHR OS (long run) and PFS
        multistate_output_temp <- fn_HR_OS_PFS(
          lambda_01_ctrl = v_lambda_01_ctrl_temp[n],
          lambda_02_ctrl = v_lambda_02_ctrl_temp[n],
          lambda_12_ctrl = v_lambda_12_ctrl_temp[n],
          lambda_01_trt = v_lambda_01_trt_temp[n],
          lambda_02_trt = v_lambda_02_trt_temp[n],
          lambda_12_trt = v_lambda_12_trt_temp[n],
          max_time_months_calculate = 12*100 # 100 years
        )
        
        # store results for iteration n
        v_LHR_PFS[n] <-   log(multistate_output_temp$HR_PFS)
        v_ALHR_OS[n] <-   log(multistate_output_temp$AHR_OS) 
        
        # use the values above to calculate LHR OS for trial duration
        multistate_output_temp <- fn_HR_OS_PFS(
          lambda_01_ctrl = v_lambda_01_ctrl_temp[n],
          lambda_02_ctrl = v_lambda_02_ctrl_temp[n],
          lambda_12_ctrl = v_lambda_12_ctrl_temp[n],
          lambda_01_trt = v_lambda_01_trt_temp[n],
          lambda_02_trt = v_lambda_02_trt_temp[n],
          lambda_12_trt = v_lambda_12_trt_temp[n],
          max_time_months_calculate = as.integer(mean(df_data_set_i_p$TimefromStart_M[1]))
        )
        v_partial_ALHR_OS[n] <-   log(multistate_output_temp$AHR_OS) 
        # cat('\r', paste0("Non-linear adjustment ", round((n/n_sim2)*100, 0), "% complete"))  
      }
      
      ## assign true INDICATION LEVEL values for PFS and OS HR for indication j
      df_data_set_i_p$PFS_LHR_indication_true[1] <- mean(v_LHR_PFS)
      df_data_set_i_p$OS_ALHR_indication_true[1] <- mean(v_ALHR_OS)
      df_data_set_i_p$OS_partial_ALHR_indication_true[1] <- mean(v_partial_ALHR_OS)
      
      ## assign MC error resulting from simulation
      df_data_set_i_p$PFS_LHR_indication_true_MCerror[1] <- sd(v_LHR_PFS)/sqrt(n_sim2)
      df_data_set_i_p$OS_ALHR_indication_true_MCerror[1] <- sd(v_ALHR_OS)/sqrt(n_sim2)
      df_data_set_i_p$OS_partial_ALHR_indication_true_MCerror[1] <- sd(v_partial_ALHR_OS)/sqrt(n_sim2)
      
    } else {
      
      # if all parameters are homogeneous within indications can just calculate based on indication level grand mean for lambda01, lambda02 etc
      lambda_01_ctrl_temp = df_data_set_i_p$lambda_01_indication[1]
      lambda_02_ctrl_temp = df_data_set_i_p$lambda_02_indication[1] 
      lambda_12_ctrl_temp = lambda_02_ctrl_temp*df_data_set_i_p$delta_indication[1] 
      lambda_01_trt_temp = lambda_01_ctrl_temp*df_data_set_i_p$M_01_indication[1] 
      lambda_02_trt_temp = lambda_02_ctrl_temp
      lambda_12_trt_temp = lambda_12_ctrl_temp
      
      # long run ALHR (100 years)
      multistate_output_temp <- fn_HR_OS_PFS(
        lambda_01_ctrl = lambda_01_ctrl_temp,
        lambda_02_ctrl = lambda_02_ctrl_temp,
        lambda_12_ctrl = lambda_12_ctrl_temp,
        lambda_01_trt = lambda_01_trt_temp,
        lambda_02_trt = lambda_02_trt_temp,
        lambda_12_trt = lambda_12_trt_temp,
        max_time_months_calculate = 12*100 # 100 years
      )
      
      ## assign true INDICATION LEVEL values for PFS and OS HR for indication j
      df_data_set_i_p$PFS_LHR_indication_true[1] <- log(multistate_output_temp$HR_PFS)
      df_data_set_i_p$OS_ALHR_indication_true[1] <- log(multistate_output_temp$AHR_OS)
      
      # trial duration ALHR (average duration of trials in the indication)
      multistate_output_temp <- fn_HR_OS_PFS(
        lambda_01_ctrl = lambda_01_ctrl_temp,
        lambda_02_ctrl = lambda_02_ctrl_temp,
        lambda_12_ctrl = lambda_12_ctrl_temp,
        lambda_01_trt = lambda_01_trt_temp,
        lambda_02_trt = lambda_02_trt_temp,
        lambda_12_trt = lambda_12_trt_temp,
        max_time_months_calculate = as.integer(mean(df_data_set_i_p$TimefromStart_M[1])) # average duration of trials in indication
      )
      df_data_set_i_p$OS_partial_ALHR_indication_true[1] <- log(multistate_output_temp$AHR_OS)
    }
    
    
    ### Simulate survival data to populate data set ####
    
    ##  calculate multistate output for study s in iteration i
    multistate_output_s_i <- fn_HR_OS_PFS(
      lambda_01_ctrl = df_data_set_i_p$lambda_01_ctrl[1],
      lambda_02_ctrl = df_data_set_i_p$lambda_02_ctrl[1], 
      lambda_12_ctrl = df_data_set_i_p$lambda_12_ctrl[1],
      lambda_01_trt = df_data_set_i_p$lambda_01_trt[1],
      lambda_02_trt = df_data_set_i_p$lambda_02_trt[1], 
      lambda_12_trt = df_data_set_i_p$lambda_12_trt[1],
      max_time_months_calculate = 12*100 # 100 years
    )
    
    ## simulate study data (using simIDM) and calculate outcomes
    
    # A = transitionGroup1 = treatment group
    # B = transitionGroup2 = control group
    # A (group 1, treatment)
    transitionGroup1 <- exponential_transition(h01 = df_data_set_i_p$lambda_01_trt[1], 
                                               h02 = df_data_set_i_p$lambda_02_trt[1], 
                                               h12 = df_data_set_i_p$lambda_12_trt[1]) 
    # B (group 2, control)
    transitionGroup2 <- exponential_transition(h01 = df_data_set_i_p$lambda_01_ctrl[1], 
                                               h02 = df_data_set_i_p$lambda_02_ctrl[1], 
                                               h12 = df_data_set_i_p$lambda_12_ctrl[1])
    transitionbyArm <- list(transitionGroup1, transitionGroup2)
    
    # sample size in control and treatment arms
    nPat <- c(df_data_set_i_p$Arm1_N[1], # arm 1, control
              df_data_set_i_p$Arm2_N[1]) # arm 2, treatment 
    n <- sum(nPat) # total sample size
    
    ### Define rate of trial dropout
    # dropout$rate specifies the drop-out probability per dropout$time time units.
    # list(rate = 0.1, time = 12) => 10% of the patients drop-out within 12 months per treatment group (we take month as the time unit of our trial)
    # If dropout$rate is equal to 0, then no censoring due to dropout is applied
    dropout <- list(rate = 0, time = 12)
    
    ### Model rate at which patients enter the trial
    # Uniformly distributed random variables are used to generate staggered study entry times. 
    # There are two possibilities for the specification:
    # 1) param = "time": the length of the accrual period is specified, e.g. 12 month, in which case the staggered entry times are generated in each treatment group using random variables U Unif(0,12)
    # 2) param = "intensity": it is specified how many patients are recruited per time unit in each treatment group. Thus, if 10 patients are recruited per month, then the staggered entry times are generated using random variables U Unif(0,30/10)
    accrual <- list(param = "time", value = 0.001) # Assume almost instant accrual
    
    ### Run the clinical trial simulations
    # nRep: defines the number of trials to simulate
    # datType: defines the form of the data 
    # "1rowTransition" will give entry and exit time for each transition (meaning some patients have more than one row [i.e. transition] in the dataset)
    # "1rowPatient" will give PFS and OS time at the individual level for each treatment option. Each row is one patient
    
    # could interpret as the number of studies in a data set or as the repetitions in a sim study
    # here I just do one simulation per study.
    nRep <- 1
    
    ### simulate study results for study s
    # note: need a new seed in every study s and iteration i 
    # otherwise there would be no variability in results between trials with the same underlying parameters
    # or between iterations, use matrix of seed values
    
    # gsDesign guide
    # https://search.r-project.org/CRAN/refmans/gsDesign/html/nSurvival.html
    
    ## NOTE: seed chosen to replace study 31 i.e. indication 8 i.e. the "no OS" target indication
    # * use different seed depending on with os or no os?
    simStudies <- getClinicalTrials(
      nRep = nRep, nPat = nPat, seed = m_seeds_studies[i, 31], datType = "1rowPatient",
      transitionByArm = transitionbyArm, dropout = dropout,
      accrual = accrual
    )
    
    ### Define censoring time for study s
    # this is the time in months from trial start to end of follow up
    # If this is Inf then there is no censoring and this gives the long run true HR for PFS and OS
    censoring_time = df_data_set_i_p$TimefromStart_M[1] # censoring time in months 
    
    ### Estimate HR for PFS and OS for study s
    
    # Set up empty dataframe to save study outcomes (HRs)
    O <- 3 # the three study outcomes will be the treatment indicator, the survival time and the censoring indicator.
    x_PFS <- data.frame(matrix(NA, nrow = n, ncol = O))
    x_OS <- data.frame(matrix(NA, nrow = n, ncol = O))
    colnames(x_PFS) <- c("Treatment", "Survival", "Censoring_Ind")
    colnames(x_OS) <- c("Treatment", "Survival", "Censoring_Ind")
    # Set up empty vector for simulated summary statistic
    Wx_PFS <- numeric(length = 1) # LHR for PFS with censoring at censoring_time
    Wx_OS <- numeric(length = 1) # LHR for OS with censoring at censoring_time
    Wx_PFS_SE <- numeric(length = 1) 
    Wx_OS_SE <- numeric(length = 1) 
    
    # assign treatment indicators 
    # switch to 0 and 1 for maths to work
    x_PFS[ , "Treatment"] <- c(rep(1, nPat[1]), rep(0, nPat[2]))
    x_OS[ , "Treatment"] <- c(rep(1, nPat[1]), rep(0, nPat[2]))
    
    # assign indiviudal survival times for PFS
    x_PFS[ , "Survival"] <- simStudies[[1]]$PFStime
    x_OS[ , "Survival"] <- simStudies[[1]]$OStime
    
    # Censor individuals after the censoring time.
    # NOTE: In flexsurv the censoring indicator is equal to 1 if the time is observed and 0 if the time is censored. 
    censoring_indicator_PFS <- (simStudies[[1]]$PFStimeCal < censoring_time)
    censoring_indicator_OS <- (simStudies[[1]]$OStimeCal < censoring_time)
    x_PFS[ , "Censoring_Ind"] <- censoring_indicator_PFS
    x_OS[ , "Censoring_Ind"] <- censoring_indicator_OS
    # If the censoring indicator is FALSE then we replace with the censoring time,
    # i.e. !censoring_indicator is TRUE.
    x_PFS[!censoring_indicator_PFS , "Survival"] <- censoring_time
    x_OS[!censoring_indicator_OS , "Survival"] <- censoring_time
    
    # Calculate the log hazard ratio using a Cox PH model
    m_PFS <- coxph(Surv(Survival, Censoring_Ind) ~ Treatment, 
                   data = x_PFS)
    m_OS <- coxph(Surv(Survival, Censoring_Ind) ~ Treatment, 
                  data = x_OS)
    
    # Save the hazard ratio from the flexservreg model for study s
    # Note these are log HRs
    df_data_set_i_p$PFS_LHR_mean[1] <- m_PFS$coefficients
    df_data_set_i_p$PFS_LHR_se[1] <- sqrt(m_PFS$var)
    df_data_set_i_p$OS_LHR_mean[1] <- m_OS$coefficients
    df_data_set_i_p$OS_LHR_se[1] <- sqrt(m_OS$var)
    
    # record the percentage of OS and PFS events observed within followup
    df_data_set_i_p$Prop_PFS_events_ctrl[1] <- mean(x_PFS$Censoring_Ind[x_PFS$Treatment == 0])
    df_data_set_i_p$Prop_PFS_events_trt[1] <- mean(x_PFS$Censoring_Ind[x_PFS$Treatment == 1])
    
    df_data_set_i_p$Prop_OS_events_ctrl[1] <- mean(x_OS$Censoring_Ind[x_OS$Treatment == 0])
    df_data_set_i_p$Prop_OS_events_trt[1] <- mean(x_OS$Censoring_Ind[x_OS$Treatment == 1])
    
    ## assign result for parameter set p (e.g. 5001, 6009 etc)
    df_outlier_target_data_set_i$scenario[p] <- df_data_set_i_p$scenario
    df_outlier_target_data_set_i[p, 4:43] <- df_data_set_i_p[2:41]
    
    # end.time <- Sys.time()
    # time.taken <- end.time - start.time
    # time.taken
    
    ## Replace prediction in "no OS" target indication with outlier ####
    
    ## read in dataset 
    # set 5, 7 => modified set 1
    # set 6, 8 => modified set 4
    original_dataset <- read.csv(paste0(results_location, 
                                        "Datasets/Data/df_data_set_", 
                                        ifelse(set == 5 | set == 7, 1000, 4000) + p,
                                        "_i", i, ".csv"))
    
    ## sets 5 and 6 => "no OS" in target
    if(set == 5 | set == 6){
      # replace indication 8 (target when "no OS") with the outlier target
      row_index <- which(original_dataset$indication_index == 8)
      modified_dataset <- original_dataset
      modified_dataset[row_index,] <- df_outlier_target_data_set_i[p,]
      modified_dataset$indication_index[row_index] <- 8
      modified_dataset$target_indication[row_index] <- TRUE
      modified_dataset$scenario <- set*1000 + p
    }
    ## sets 7 and 8 => "with OS" in target
    if(set == 7 | set == 8){
      # replace indication 1 (target when "with OS") with the outlier target
      row_index <- 1
      modified_dataset <- original_dataset
      modified_dataset[row_index,] <- df_outlier_target_data_set_i[p,]
      modified_dataset$indication_index[row_index] <- 1
      modified_dataset$target_indication[row_index] <- TRUE
      modified_dataset$scenario <- set*1000 + p
    }
    
    ## save the modified dataset
    write.csv(modified_dataset,
              file = paste0(results_location, "Datasets/Data/df_data_set_", set*1000 + p,"_i", i, ".csv"),
              row.names = FALSE)
    
    ## update
    # cat('\r', paste0(round(p/50*100,0), "% complete"))  
    
  }

}

# ## Checks #####
# df_outlier_target_data_set_i$log_lambda_01_indication
# df_outlier_target_data_set_i$log_M_01
# # theta long run follow up
# df_outlier_target_data_set_i$OS_ALHR_indication_true # theta - log scale
# exp(df_outlier_target_data_set_i$OS_ALHR_indication_true) # theta - natural scale
# # theta short run follow up (target in paper)
# df_outlier_target_data_set_i$OS_partial_ALHR_indication_true
# exp(df_outlier_target_data_set_i$OS_partial_ALHR_indication_true)
# # observed OS HR
# df_outlier_target_data_set_i$OS_LHR_mean # log scale
# exp(df_outlier_target_data_set_i$OS_LHR_mean) # natural scale
# 
# ## compare to set 1 target indication values
# # target PFS and OS when non-outlier
# df_compare <- read.csv(paste0(results_location, "Datasets/data/df_data_set_", 
#                 1001, "_i", 1 ,".csv"))
# 
# df_compare$OS_partial_ALHR_indication_true[1] # log scale
# df_compare$OS_partial_ALHR_indication_true[31] # log scale
# 
# exp(df_compare$OS_partial_ALHR_indication_true[1]) # natural scale
# exp(df_compare$OS_partial_ALHR_indication_true[31]) # natural scale
# exp(df_compare$OS_LHR_mean[1]) # natural scale
# exp(df_compare$OS_LHR_mean[31]) # natural scale
# 
# 
# 
# ## Save csv ####
# # save in Datasets/Outlier_indications folder (so that it can be called by CalculateResults.R)
# write.csv(df_outlier_target_data_set_i,
#           file = paste0(results_location, "Datasets/Outlier_indications/df_outlier_target_data_set", set ,"_i", i, ".csv"),
#           row.names = FALSE)
