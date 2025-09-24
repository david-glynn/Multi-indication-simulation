
## GENERATE DATASET #########

### Calculate PFS/OS outputs from multistate ###########
# based on simIDM

# simIDM Quick get started guide
# https://cran.r-project.org/web/packages/simIDM/vignettes/quickstart.html
# more indepth documentation 
# https://cran.r-project.org/web/packages/simIDM/simIDM.pdf 

## INPUTS: 
# lambda_01_ctrl = rate progression with control 
# lambda_02_ctrl = rate pre-progression death with control 
# lambda_12_ctrl = rate post-progression death with control 
# lambda_01_trt = rate progression with treatment 
# lambda_02_trt = rate pre-progression death with treatment 
# lambda_12_trt = rate post-progression death with treatment 
# max_time_months_calculate = how many time periods to calculate: should be large enough so that all OS events have occured
## OUTPUTS:
# h_OS_t_trt = OS hazard over time with treatment
# h_OS_t_ctrl  
# h_PFS_trt = PFS hazard with treatment (constant over time)
# h_PFS_ctrl 
# S_PFS_t_trt = PFS survival function over time with treatment
# S_PFS_t_ctrl 
# S_OS_t_trt = OS survival function over time with treatment
# S_OS_t_ctrl 
# HR_PFS = HR for PFS (constant over time)
# HR_OS_t = Instantaneous HR for OS over time
# AHR_OS = Average HR for OS

# lambda_01_ctrl = 0.12
# lambda_02_ctrl = 0.005
# lambda_12_ctrl = 0.0475
# lambda_01_trt = 0.06
# lambda_02_trt = 0.005
# lambda_12_trt = 0.0475
# max_time_months_calculate = 11

fn_HR_OS_PFS <- function(
    lambda_01_ctrl,
    lambda_02_ctrl, 
    lambda_12_ctrl,
    lambda_01_trt,
    lambda_02_trt, 
    lambda_12_trt,
    max_time_months_calculate
  ){
  
  # B (group 2, control)
  transitionGroup2 <- exponential_transition(h01 = lambda_01_ctrl, 
                                             h02 = lambda_02_ctrl, 
                                             h12 = lambda_12_ctrl) 
  
  # A (group 1, treatment)
  transitionGroup1 <- exponential_transition(h01 = lambda_01_trt, 
                                             h02 = lambda_02_trt, 
                                             h12 = lambda_12_trt) 
  
  transitionbyArm <- list(transitionGroup1, transitionGroup2)
  
  # define hazards
  # A (group 1, treatment)
  lambda_01_trt <- transitionGroup1$hazards$h01
  lambda_12_trt <- transitionGroup1$hazards$h12
  lambda_02_trt <- transitionGroup1$hazards$h02
  lambda_012_trt <- lambda_12_trt - lambda_01_trt - lambda_02_trt
  # B (group 2, control)
  lambda_01_ctrl <- transitionGroup2$hazards$h01
  lambda_12_ctrl <- transitionGroup2$hazards$h12
  lambda_02_ctrl <- transitionGroup2$hazards$h02
  lambda_012_ctrl <- lambda_12_ctrl - lambda_01_ctrl - lambda_02_ctrl
  
  # implied PFS (eqn 3 in Erdmann)
  h_PFS_trt <- lambda_01_trt + lambda_02_trt
  h_PFS_ctrl <- lambda_01_ctrl + lambda_02_ctrl
  
  # implied PFS HR (eqn 4 in Erdmann)
  HR_PFS <- h_PFS_trt/h_PFS_ctrl

  ### calculate average hazard ratio OS (static)
  
  # survival function for OS
  # HR changes with time so need to specify t 
  time_points_calculate  <- seq(from = 0.05, to = max_time_months_calculate, by = 0.05) 
  
  # Instantaneous OS HR (eqn 5 in Erdmann)
  # HR changes with time so need to specify t 
  HR_OS_t <- rep(NA, length(time_points_calculate ))
  h_OS_t_trt <- rep(NA, length(time_points_calculate ))
  h_OS_t_ctrl <- rep(NA, length(time_points_calculate ))
  
  for(t in 1:length(time_points_calculate )){
    
    # OS hazard at time t = i for A (group 1)
    # note its theoretically possible to get a zero value in which the ratio hazard is undefined - remove this.
    h_OS_ti_trt <- ((lambda_12_trt - lambda_02_trt)*(lambda_01_trt + lambda_02_trt) - lambda_01_trt*lambda_12_trt*exp(-lambda_012_trt*time_points_calculate[t]))/((lambda_12_trt - lambda_02_trt) - lambda_01_trt*exp(-lambda_012_trt*time_points_calculate[t]))
    h_OS_ti_trt <- ifelse(h_OS_ti_trt != 0,
           h_OS_ti_trt, NA)
    h_OS_t_trt[t] <- h_OS_ti_trt
    
    # OS hazard at time t = i for B (group 2)
    h_OS_ti_ctrl <- ((lambda_12_ctrl - lambda_02_ctrl)*(lambda_01_ctrl + lambda_02_ctrl) - lambda_01_ctrl*lambda_12_ctrl*exp(-lambda_012_ctrl*time_points_calculate[t]))/((lambda_12_ctrl - lambda_02_ctrl) - lambda_01_ctrl*exp(-lambda_012_ctrl*time_points_calculate[t]))
    h_OS_ti_ctrl <- ifelse(h_OS_ti_ctrl !=0,
           h_OS_ti_ctrl, NA)
    h_OS_t_ctrl[t] <- h_OS_ti_ctrl
    
    # HR
    # note its possible that 
    HR_OS_t[t] <- h_OS_ti_trt/h_OS_ti_ctrl 
  }
  
  # survival function for PFS
  S_PFS_t_trt = exp(-(lambda_01_trt + lambda_02_trt)*time_points_calculate )
  S_PFS_t_ctrl = exp(-(lambda_01_ctrl + lambda_02_ctrl)*time_points_calculate )
  
  # survival function for OS
  P_01_0_t_trt = lambda_01_trt*(1/lambda_012_trt)*(exp(-1*(lambda_01_trt + lambda_02_trt)*time_points_calculate) - exp(-lambda_12_trt*time_points_calculate))
  P_01_0_t_ctrl = lambda_01_ctrl*(1/lambda_012_ctrl)*(exp(-1*(lambda_01_ctrl + lambda_02_ctrl)*time_points_calculate) - exp(-lambda_12_ctrl*time_points_calculate))
  S_OS_t_trt = S_PFS_t_trt + P_01_0_t_trt
  S_OS_t_ctrl = S_PFS_t_ctrl + P_01_0_t_ctrl
  
  ## modified version of MUKHOPADHYAY
  # piece wise calculation, break time period into 1 month chunks
  # average LHR = summation LHR*pi, where pi = the proportion of events in time p
  # assuming all patients are in the trial at the beginning (ignore issues of recruitment)
  # Take average of proportion of events in each arm
  # in time period i, the proportion of events (pi) = change in events (subtract p at time t+1 from p at time t)
  # 100% events at time 1
  
  p_t_trt <- c(1,S_OS_t_trt[-length(S_OS_t_trt)]) - S_OS_t_trt
  p_t_ctrl <-c(1,S_OS_t_ctrl[-length(S_OS_t_ctrl)]) - S_OS_t_ctrl
  p_t <- (p_t_trt + p_t_ctrl)/2
  # normalise to sum to 1
  p_t_norm <- p_t/sum(p_t)
  # sum(p_t_norm)
  
  AHR_OS <- exp(sum(log(HR_OS_t)*p_t_norm, na.rm = TRUE))
  
  # return results 
  results <- list(
    h_OS_t_trt = h_OS_t_trt,
    h_OS_t_ctrl = h_OS_t_ctrl,
    h_PFS_trt = h_PFS_trt,
    h_PFS_ctrl = h_PFS_ctrl,
    S_PFS_t_trt = S_PFS_t_trt,
    S_PFS_t_ctrl = S_PFS_t_ctrl,
    S_OS_t_trt = S_OS_t_trt,
    S_OS_t_ctrl = S_OS_t_ctrl,
    HR_PFS = HR_PFS,
    HR_OS_t = HR_OS_t,
    AHR_OS = AHR_OS,
    time_points_calculate = time_points_calculate
  )
  
  results
  
}


### Create dataset data frame #########
# for iteration i create an analysis data set of studies within indications
# this is later analysed using the models and the performance recorded

# scenario <<- df_scenarios[1,]
# i <- 1

fn_create_dataset_df <- function(i){
  
  ## create "late" non-target dataset (if the dataset is "early/mid" we prune off studies later so that they stay consistent)
  # simulate each trial individually and add results to the data set 
  
  # (study outcomes for non-target indication trials in iteration i)
  # (potential) target indication studies
  total_target_studies <- 2
  
  # non-target indication studies (late dataset) note we subtract studies from this to get "early", and "mid" datasets
  # 3 indications have 9, 8 and 6 studies
  # 3 have 3, 2 and 1
  # order indications from largest number to smallest number of studies
  # indication index from 1 to 7 (target index is 1)
  v_studies_per_indication_non_target <- c(9, 8, 6, 3, 2, 1)
  total_non_target_studies <- sum(v_studies_per_indication_non_target)
    
    ### Create dataframe ####
    ## initialise data set for iteration i
    df_data_set_non_target_i <- data.frame(
      # scenario
      scenario = as.numeric(gsub('[^[:alnum:] ]','',scenario$Parameter_set)),
      # index values
      indication_index = rep(2:7, v_studies_per_indication_non_target), # target indication is index 1
      target_indication = rep(FALSE, total_non_target_studies),
      # indication level parameters (log scale)
      log_lambda_01_indication = rep(NA, total_non_target_studies),
      log_lambda_02_indication = rep(NA, total_non_target_studies),
      log_delta_indication = rep(NA, total_non_target_studies),
      log_M_01_indication = rep(NA, total_non_target_studies),
      # true study level parameter values used in the simulation (log scale)
      log_lambda_01_ctrl = rep(NA, total_non_target_studies),
      log_lambda_02_ctrl = rep(NA, total_non_target_studies),
      log_delta_ctrl = rep(NA, total_non_target_studies),
      log_M_01 = rep(NA, total_non_target_studies),
      # indication level parameters (natural scale)
      lambda_01_indication = rep(NA, total_non_target_studies),
      lambda_02_indication = rep(NA, total_non_target_studies),
      delta_indication = rep(NA, total_non_target_studies),
      M_01_indication = rep(NA, total_non_target_studies),
      # true study level parameter values used in the simulation (natural scale)
      lambda_01_ctrl = rep(NA, total_non_target_studies),
      lambda_02_ctrl = rep(NA, total_non_target_studies),
      delta_ctrl = rep(NA, total_non_target_studies),
      M_01 = rep(NA, total_non_target_studies),
      lambda_12_ctrl = rep(NA, total_non_target_studies),
      lambda_01_trt = rep(NA, total_non_target_studies),
      lambda_02_trt = rep(NA, total_non_target_studies),
      lambda_12_trt = rep(NA, total_non_target_studies),
      # true study level outcome values used in the simulation
      PFS_LHR_study_true = rep(NA, total_non_target_studies), # true value of log hazard ratio for PFS based on multistate parameters
      OS_ALHR_study_true = rep(NA, total_non_target_studies), # true long run true LHR OS value based on multistate parameters
      OS_partial_ALHR_study_true = rep(NA, total_non_target_studies), # average LHR OS value over the relevant trial duration
      # true indication level outcome values (used in performance measurement)
      PFS_LHR_indication_true = rep(NA, total_non_target_studies), 
      OS_ALHR_indication_true = rep(NA, total_non_target_studies), # long run true LHR OS value
      OS_partial_ALHR_indication_true = rep(NA, total_non_target_studies), # average LHR OS value over the relevant trial duration
      PFS_LHR_indication_true_MCerror = rep(NA, total_non_target_studies), # MC error when calculating indication level PFS LHR in the presence of within indication heterogeneity
      OS_ALHR_indication_true_MCerror = rep(NA, total_non_target_studies), 
      OS_partial_ALHR_indication_true_MCerror = rep(NA, total_non_target_studies), 
      # study details
      TimefromStart_M = rep(NA, total_non_target_studies),  # Full trial duration (from start of recruitment to reporting results) The time difference between the ‘StartDate’ and ‘TimePoint’, measured in months. 
      Arm1_N = rep(NA, total_non_target_studies), # Control. Number of randomised patients in Arm 1 of the trial
      Arm2_N = rep(NA, total_non_target_studies), # Treatment. Number of randomised patients in Arm 2 of the trial     
      # observed outcomes in each study
      PFS_LHR_mean = rep(NA, total_non_target_studies), # observed mean log hazard ratio for PFS in study s
      PFS_LHR_se = rep(NA, total_non_target_studies), # observed standard error for log hazard ratio for PFS in study s
      OS_LHR_mean = rep(NA, total_non_target_studies), # observed mean log hazard ratio for OS in study s
      OS_LHR_se = rep(NA, total_non_target_studies), # observed standard error for log hazard ratio for OS in study s
      Prop_PFS_events_ctrl = rep(NA, total_non_target_studies), # proportion of PFS events observed within the trial followup in the control arm
      Prop_PFS_events_trt = rep(NA, total_non_target_studies), # proportion of PFS events observed within the trial followup in the treatment arm
      Prop_OS_events_ctrl = rep(NA, total_non_target_studies),
      Prop_OS_events_trt = rep(NA, total_non_target_studies)
    )
    
    ### 1st Target indication (index 1): with OS in the target indication "with OS"
    df_data_set_target1_i <- df_data_set_non_target_i[1,]
    df_data_set_target1_i$indication_index <- 1
    df_data_set_target1_i$target_indication <- TRUE
    
    ### 2nd Target indication (index 8): PFS only in the target indication "no OS"
    # put this at the end (index 8) to maintain non-target index ordering
    df_data_set_target2_i <- df_data_set_non_target_i[1,]
    df_data_set_target2_i$indication_index <- 8
    df_data_set_target2_i$target_indication <- TRUE
    
    ### Combine target and non-target datas sets
    # append to the above data set
    df_data_set_i <- rbind(df_data_set_target1_i, df_data_set_non_target_i, df_data_set_target2_i)
    
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
    
    # loop through each of the indications and assign true INDICATION LEVEL multi state parameter values
    # This reflects the between indication heterogeneity in parameter values (if there is any in the simulation scenario)
    for(j in 1:max(df_data_set_i$indication_index)){
      
      row_index_j <- which(df_data_set_i$indication_index == j) # row index for indication j
      
      if(!(j == 3 & scenario$Outlier == TRUE)){
        # draw indication level parameters where there is NO outlier
        # note log scale
        df_data_set_i$log_lambda_01_indication[row_index_j] <- rnorm(1, log_lambda_01_mu, # Overall mean for bev: mu
                                                                 log_lambda_01_tau_bw) # Between indication heterogeneity: tau_bw
        df_data_set_i$log_lambda_02_indication[row_index_j] <- rnorm(1, log_lambda_02_mu, # Overall mean for bev: mu
                                                                 log_lambda_02_tau_bw) # Between indication heterogeneity: tau_bw
        df_data_set_i$log_delta_indication[row_index_j] <- rnorm(1, log_delta_mu, # Overall mean for bev: mu
                                                             log_delta_tau_bw) # Between indication heterogeneity: tau_bw
        df_data_set_i$log_M_01_indication[row_index_j] <- rnorm(1, log_M_01_mu, # Overall mean for bev: mu
                                                            log_M_01_tau_bw) # Between indication heterogeneity: tau_bw
        
      } else {
        # draw indication level parameters for the OUTLIER indication
        
        # define mean of outlier indicaiton
        # the outlier mean will be a specific distance from the main effect
        # choose multipler to put outlier mean X SDs higher than the non-outlier mean (less effective)
        # where the SDs are the between indication heterogeneiy in the non-outlier indications
        above <- TRUE # rbinom(1, 1, 0.5) # old code randomised above and below
        df_data_set_i$log_lambda_01_indication[row_index_j] <- log_lambda_01_mu + ifelse(above, 1, -1)*scenario$lambda_01_outlier_SD*log_lambda_01_tau_bw
        df_data_set_i$log_lambda_02_indication[row_index_j] <- log_lambda_02_mu + ifelse(above, 1, -1)*scenario$lambda_02_outlier_SD*log_lambda_02_tau_bw
        df_data_set_i$log_delta_indication[row_index_j] <- log_delta_mu + ifelse(above, 1, -1)*scenario$delta_outlier_SD*log_delta_tau_bw
        df_data_set_i$log_M_01_indication[row_index_j] <- log_M_01_mu + ifelse(above, 1, -1)*scenario$M_01_outlier_SD*log_M_01_tau_bw
        
      }
      
    }
    
    # loop through each of the studies and assign true STUDY LEVEL multi state parameter values
    # this reflects within indication heterogeneity in parameter values (if there is any in the simulation scenario)
    for(s in 1:dim(df_data_set_i)[1]){
      
      if(!(df_data_set_i$indication_index[s] == 3 & scenario$Outlier == TRUE)){
        # draw study level parameters where there is no outlier (log scale)
        
        # lambda 01
        df_data_set_i$log_lambda_01_ctrl[s] <- rnorm(1, df_data_set_i$log_lambda_01_indication[s], # indication specific mean for study s
                                                 log_lambda_01_tau_w) # within indication heterogeneity: tau_w
        # lambda 02  
        df_data_set_i$log_lambda_02_ctrl[s] <- rnorm(1, df_data_set_i$log_lambda_02_indication[s], # indication specific mean for study s
                                                 log_lambda_02_tau_w) # within indication heterogeneity: tau_w
        # M01
        df_data_set_i$log_M_01[s] <- rnorm(1, df_data_set_i$log_M_01_indication[s], # indication specific mean for study s
                                       log_M_01_tau_w) # within indication heterogeneity: tau_w
        # delta
        df_data_set_i$log_delta_ctrl[s] <- rnorm(1, df_data_set_i$log_delta_indication[s], # indication specific mean for study s
                                             log_delta_tau_w) # within indication heterogeneity: tau_w
      } else {
        # draw study level parameters for the outlier indication (log scale)
        # done separately because the outlier indication may have different within indication heterogeneity than non-outliers

        # lambda 01
        df_data_set_i$log_lambda_01_ctrl[s] <- rnorm(1, df_data_set_i$log_lambda_01_indication[s], # indication specific mean for study s
                                                     log_lambda_01_tau_w_outlier) # within indication heterogeneity: tau_w
        # lambda 02  
        df_data_set_i$log_lambda_02_ctrl[s] <- rnorm(1, df_data_set_i$log_lambda_02_indication[s], # indication specific mean for study s
                                                     log_lambda_02_tau_w_outlier) # within indication heterogeneity: tau_w
        # M01
        df_data_set_i$log_M_01[s] <- rnorm(1, df_data_set_i$log_M_01_indication[s], # indication specific mean for study s
                                           log_M_01_tau_w_outlier) # within indication heterogeneity: tau_w
        # delta
        df_data_set_i$log_delta_ctrl[s] <- rnorm(1, df_data_set_i$log_delta_indication[s], # indication specific mean for study s
                                                 log_delta_tau_w_outlier) # within indication heterogeneity: tau_w
      }
    }
    
    # convert log to natural scale for use in formulas
    df_data_set_i$lambda_01_indication = exp(df_data_set_i$log_lambda_01_indication)
    df_data_set_i$lambda_02_indication = exp(df_data_set_i$log_lambda_02_indication)
    df_data_set_i$delta_indication = exp(df_data_set_i$log_delta_indication)
    df_data_set_i$M_01_indication = exp(df_data_set_i$log_M_01_indication)
    df_data_set_i$lambda_01_ctrl = exp(df_data_set_i$log_lambda_01_ctrl)
    df_data_set_i$lambda_02_ctrl = exp(df_data_set_i$log_lambda_02_ctrl)
    df_data_set_i$delta_ctrl = exp(df_data_set_i$log_delta_ctrl)
    df_data_set_i$M_01 = exp(df_data_set_i$log_M_01)
    
    # assign values of derived multistate parameters
    df_data_set_i$lambda_12_ctrl <- df_data_set_i$lambda_02_ctrl*df_data_set_i$delta_ctrl
    df_data_set_i$lambda_01_trt <- df_data_set_i$lambda_01_ctrl*df_data_set_i$M_01
    df_data_set_i$lambda_02_trt <- df_data_set_i$lambda_02_ctrl
    df_data_set_i$lambda_12_trt <- df_data_set_i$lambda_12_ctrl
    
    ### Calculate the sample size in each trial arm ####
    
    for(s in 1:dim(df_data_set_i)[1]){
      
      # calculate multistate output for study s in iteration i
      multistate_output_s_i <- fn_HR_OS_PFS(
        lambda_01_ctrl = df_data_set_i$lambda_01_ctrl[s],
        lambda_02_ctrl = df_data_set_i$lambda_02_ctrl[s], 
        lambda_12_ctrl = df_data_set_i$lambda_12_ctrl[s],
        lambda_01_trt = df_data_set_i$lambda_01_trt[s],
        lambda_02_trt = df_data_set_i$lambda_02_trt[s], 
        lambda_12_trt = df_data_set_i$lambda_12_trt[s],
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
      
      # old approach
      # calculate OS survival curve for combined treatment and control arms
      # S_OS_t_total <- (multistate_output_s_i$S_OS_t_ctrl + multistate_output_s_i$S_OS_t_trt)/2
      # plot(seq(1:length(multistate_output_s_i$S_OS_t_ctrl)), S_OS_t_total)
      # time_to_Xprec_OS_target <- which.min(abs(S_OS_t_total - (1 - scenario$Prop_OS_event_target) )) - 1
      # time_to_Xprec_OS_non_target <- which.min(abs(S_OS_t_total - (1 - scenario$Prop_OS_event_non_target) )) - 1

      ## assign duration of trial for study s
      if(df_data_set_i$target_indication[s] == FALSE){
        # if s non-target indication
        df_data_set_i$TimefromStart_M[s] <- time_to_Xprec_OS_non_target
      } else {
        # if s target indication
        df_data_set_i$TimefromStart_M[s] <- time_to_Xprec_OS_target
      }
      
      ### Calculate sample size ####
      # based on prognosis and trial duration
      sample_size_s_i <- nSurvival(
        lambda1 = log(2)/time_to_median_OS_ctrl,  # event hazard rate OS in control arm
        lambda2 = (log(2)/time_to_median_OS_ctrl)*0.7,  # required hazard ratio of 0.7
        eta = 0,  # equal dropout hazard rate for both groups. Assume zero
        Ts = df_data_set_i$TimefromStart_M[s], # adjusted to reflect the maturity assumption
        Tr = 0.001, # Aprox instant accrual assumed. 
        sided = 2,
        alpha = .05,
        beta = 0.10 # 1-beta = 90% power
      )
      
      ## assign sample size for study s
      # reduce sample size to X% of fully powered sample size to model imprecision
      if(df_data_set_i$target_indication[s] == FALSE){
        # non-target indications
        df_data_set_i$Arm1_N[s] <- df_data_set_i$Arm2_N[s] <- as.integer(sample_size_s_i$n*scenario$Prop_sample_size_non_target)
      } else {
        # target indications
        df_data_set_i$Arm1_N[s] <- df_data_set_i$Arm2_N[s] <- as.integer(sample_size_s_i$n*scenario$Prop_sample_size_target)
      }
      
    }
    
    ### Generate true PFS and OS outcomes ####
    
    ## STUDY LEVEL true PFS and OS
    for(s in 1:dim(df_data_set_i)[1]){
      
      ##  calculate multistate output for study s in iteration i
      # use to calculate the long run LHR OS (100 years)
      multistate_output_s_i <- fn_HR_OS_PFS(
        lambda_01_ctrl = df_data_set_i$lambda_01_ctrl[s],
        lambda_02_ctrl = df_data_set_i$lambda_02_ctrl[s], 
        lambda_12_ctrl = df_data_set_i$lambda_12_ctrl[s],
        lambda_01_trt = df_data_set_i$lambda_01_trt[s],
        lambda_02_trt = df_data_set_i$lambda_02_trt[s], 
        lambda_12_trt = df_data_set_i$lambda_12_trt[s],
        max_time_months_calculate = 12*100 # 100 years - long run ALHR
      )
      ## assign true STUDY LEVEL values for PFS and OS for study s
      df_data_set_i$PFS_LHR_study_true[s] <- log(multistate_output_s_i$HR_PFS)
      df_data_set_i$OS_ALHR_study_true[s] <- log(multistate_output_s_i$AHR_OS)
      
      # calculate the ALHR OS over the trial duration
      multistate_output_s_i <- fn_HR_OS_PFS(
        lambda_01_ctrl = df_data_set_i$lambda_01_ctrl[s],
        lambda_02_ctrl = df_data_set_i$lambda_02_ctrl[s], 
        lambda_12_ctrl = df_data_set_i$lambda_12_ctrl[s],
        lambda_01_trt = df_data_set_i$lambda_01_trt[s],
        lambda_02_trt = df_data_set_i$lambda_02_trt[s], 
        lambda_12_trt = df_data_set_i$lambda_12_trt[s],
        max_time_months_calculate = df_data_set_i$TimefromStart_M[s] # ALHR over the trial duration
      )
      df_data_set_i$OS_partial_ALHR_study_true[s] <- log(multistate_output_s_i$AHR_OS)
      
    }
    
    ## INDICATION LEVEL true PFS and OS
    # Calculate true INDICATION LEVEL PFS and OS LHR
    # only for the target indications: 
    for(j in df_data_set_i$indication_index[df_data_set_i$target_indication]){
      
      row_index_j <- which(df_data_set_i$indication_index == j) # row index for indication j
      
      # the LHR OS and PFS calculation is non-linear in lambda01, lambda02 etc 
      # need to take account of this non-linearity in calculating indication expectation for LHR OS
      if(apply_non_linear_adjustment & log_lambda_01_tau_w + log_lambda_02_tau_w + log_delta_tau_w + log_M_01_tau_w > 0){
        print(paste0("Begin non-linear adjustment for indication ", ifelse(j == 1, 1, 2), " of 2." ))
      }
      
      if(apply_non_linear_adjustment & log_lambda_01_tau_w + log_lambda_02_tau_w + log_delta_tau_w + log_M_01_tau_w > 0){
        # if there is within indication heterogeneity, need to estimate the true LHR using simulation 
        # drawing a number of simulations from the (gaussian) distribution of within indication heterogeneity,
        # calculating LHR OS and PFS for each, then taking the mean.
        # note: only need to calculate for the target indications, so no need to consider outliers
        
        v_ALHR_OS <- rep(NA, n_sim2) # vector to hold long run ALHR OS (100 years) results
        v_LHR_PFS <- rep(NA, n_sim2)
        v_partial_ALHR_OS <- rep(NA, n_sim2) # vector to hold trial duration ALHR OS results
        
        # simulate values of lambda01 etc
        # mean is indication level mean, SD is the within indication SD
        # simulate on log scale
        v_log_lambda_01_ctrl_temp <- rnorm(n_sim2, df_data_set_i$log_lambda_01_indication[row_index_j][1], log_lambda_01_tau_w) 
        v_log_lambda_02_ctrl_temp <- rnorm(n_sim2, df_data_set_i$log_lambda_02_indication[row_index_j][1], log_lambda_02_tau_w) 
        v_log_delta_temp <- rnorm(n_sim2, df_data_set_i$log_delta_indication[row_index_j][1], log_delta_tau_w)
        v_log_M_01_temp <- rnorm(n_sim2, df_data_set_i$log_M_01_indication[row_index_j][1], log_M_01_tau_w)
        
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
            max_time_months_calculate = as.integer(mean(df_data_set_i$TimefromStart_M[row_index_j]))
          )
          v_partial_ALHR_OS[n] <-   log(multistate_output_temp$AHR_OS) 
          # cat('\r', paste0("Non-linear adjustment ", round((n/n_sim2)*100, 0), "% complete for indication ", ifelse(j == 1, 1, 2), " of 2." ))
        }
        
        ## assign true INDICATION LEVEL values for PFS and OS HR for indication j
        df_data_set_i$PFS_LHR_indication_true[row_index_j] <- mean(v_LHR_PFS)
        df_data_set_i$OS_ALHR_indication_true[row_index_j] <- mean(v_ALHR_OS)
        df_data_set_i$OS_partial_ALHR_indication_true[row_index_j] <- mean(v_partial_ALHR_OS)
        
        ## assign MC error resulting from simulation
        df_data_set_i$PFS_LHR_indication_true_MCerror[row_index_j] <- sd(v_LHR_PFS)/sqrt(n_sim2)
        df_data_set_i$OS_ALHR_indication_true_MCerror[row_index_j] <- sd(v_ALHR_OS)/sqrt(n_sim2)
        df_data_set_i$OS_partial_ALHR_indication_true_MCerror[row_index_j] <- sd(v_partial_ALHR_OS)/sqrt(n_sim2)
        
      } else {
        
        # if all parameters are homogeneous within indications can just calculate based on indication level grand mean for lambda01, lambda02 etc
        lambda_01_ctrl_temp = df_data_set_i$lambda_01_indication[row_index_j][1]
        lambda_02_ctrl_temp = df_data_set_i$lambda_02_indication[row_index_j][1] 
        lambda_12_ctrl_temp = lambda_02_ctrl_temp*df_data_set_i$delta_indication[row_index_j][1] 
        lambda_01_trt_temp = lambda_01_ctrl_temp*df_data_set_i$M_01_indication[row_index_j][1] 
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
        df_data_set_i$PFS_LHR_indication_true[row_index_j] <- log(multistate_output_temp$HR_PFS)
        df_data_set_i$OS_ALHR_indication_true[row_index_j] <- log(multistate_output_temp$AHR_OS)
        
        # trial duration ALHR (average duration of trials in the indication)
        multistate_output_temp <- fn_HR_OS_PFS(
          lambda_01_ctrl = lambda_01_ctrl_temp,
          lambda_02_ctrl = lambda_02_ctrl_temp,
          lambda_12_ctrl = lambda_12_ctrl_temp,
          lambda_01_trt = lambda_01_trt_temp,
          lambda_02_trt = lambda_02_trt_temp,
          lambda_12_trt = lambda_12_trt_temp,
          max_time_months_calculate = as.integer(mean(df_data_set_i$TimefromStart_M[row_index_j])) # average duration of trials in indication
        )
        df_data_set_i$OS_partial_ALHR_indication_true[row_index_j] <- log(multistate_output_temp$AHR_OS)
      }
    }
    
    
    ### Simulate survival data to populate data set ####
    
    for(s in 1:dim(df_data_set_i)[1]){
      
      ##  calculate multistate output for study s in iteration i
      multistate_output_s_i <- fn_HR_OS_PFS(
        lambda_01_ctrl = df_data_set_i$lambda_01_ctrl[s],
        lambda_02_ctrl = df_data_set_i$lambda_02_ctrl[s], 
        lambda_12_ctrl = df_data_set_i$lambda_12_ctrl[s],
        lambda_01_trt = df_data_set_i$lambda_01_trt[s],
        lambda_02_trt = df_data_set_i$lambda_02_trt[s], 
        lambda_12_trt = df_data_set_i$lambda_12_trt[s],
        max_time_months_calculate = 12*100 # 100 years
      )
      
      ## simulate study data (using simIDM) and calculate outcomes
      
      # A = transitionGroup1 = treatment group
      # B = transitionGroup2 = control group
      # A (group 1, treatment)
      transitionGroup1 <- exponential_transition(h01 = df_data_set_i$lambda_01_trt[s], 
                                                 h02 = df_data_set_i$lambda_02_trt[s], 
                                                 h12 = df_data_set_i$lambda_12_trt[s]) 
      # B (group 2, control)
      transitionGroup2 <- exponential_transition(h01 = df_data_set_i$lambda_01_ctrl[s], 
                                                 h02 = df_data_set_i$lambda_02_ctrl[s], 
                                                 h12 = df_data_set_i$lambda_12_ctrl[s])
      transitionbyArm <- list(transitionGroup1, transitionGroup2)
      
      # sample size in control and treatment arms
      nPat <- c(df_data_set_i$Arm1_N[s], # arm 1, control
                df_data_set_i$Arm2_N[s]) # arm 2, treatment 
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
      
      simStudies <- getClinicalTrials(
        nRep = nRep, nPat = nPat, seed = m_seeds_studies[i, s], datType = "1rowPatient",
        transitionByArm = transitionbyArm, dropout = dropout,
        accrual = accrual
      )
      
      ### Define censoring time for study s
      # this is the time in months from trial start to end of follow up
      # If this is Inf then there is no censoring and this gives the long run true HR for PFS and OS
      censoring_time = df_data_set_i$TimefromStart_M[s] # censoring time in months 
      
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
      
      df_data_set_i$PFS_LHR_mean[s] <- m_PFS$coefficients
      df_data_set_i$PFS_LHR_se[s] <- sqrt(m_PFS$var)
      df_data_set_i$OS_LHR_mean[s] <- m_OS$coefficients
      df_data_set_i$OS_LHR_se[s] <- sqrt(m_OS$var)
      
      # record the percentage of OS and PFS events observed within followup
      df_data_set_i$Prop_PFS_events_ctrl[s] <- mean(x_PFS$Censoring_Ind[x_PFS$Treatment == 0])
      df_data_set_i$Prop_PFS_events_trt[s] <- mean(x_PFS$Censoring_Ind[x_PFS$Treatment == 1])
      
      df_data_set_i$Prop_OS_events_ctrl[s] <- mean(x_OS$Censoring_Ind[x_OS$Treatment == 0])
      df_data_set_i$Prop_OS_events_trt[s] <- mean(x_OS$Censoring_Ind[x_OS$Treatment == 1])
      
    }

    # return object
    df_data_set_i
}

# # test
# n_sim <- 1
# names(scenario)
# 
# # default values
# scenario$Early_dataset =          c(FALSE) 
# scenario$lambda01_Hetero_bw =     c(FALSE)
# scenario$lambda02_Hetero_bw =     c(FALSE)
# scenario$delta_Hetero_bw =        c(FALSE)
# scenario$M01_Hetero_bw =          c(FALSE)
# scenario$lambda01_Hetero_w =      c(FALSE)
# scenario$lambda02_Hetero_w =      c(FALSE)
# scenario$delta_Hetero_w =         c(FALSE)
# scenario$M01_Hetero_w =           c(FALSE)  
# scenario$Immature_non_target =    c(FALSE)
# scenario$Immature_target =        c(FALSE)
# scenario$Imprecise_non_target =   c(FALSE)
# scenario$Imprecise_target =       c(TRUE) 
# scenario$OS_data_for_target =     c(TRUE)
# 
# # observe HR for PFS and OS missing for the correct studies
# scenario$Early_dataset <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$Early_dataset <- FALSE
# 
# # lambda01 (treat and control) is the same within indications but different between
# # this feeds into changing OS HR and PFS HR, trial duration, sample size
# # all other parameters remain unchanged (lambda02, delta, M...)
# scenario$lambda01_Hetero_bw <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$lambda01_Hetero_bw <- FALSE
# 
# # lambda02 (treat and control) is the same within indications but different between
# # this feeds into changing lambda12, OS HR and PFS HR, trial duration, sample size
# # all other parameters remain unchanged (lambda02, delta, M...)
# scenario$lambda02_Hetero_bw <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$lambda02_Hetero_bw <- FALSE
# 
# # lambda01 (treat and control) differs within indications but similar between
# scenario$lambda01_Hetero_w <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$lambda01_Hetero_w <- FALSE
# 
# # lambda01 (treat and control) differs within and between indications
# scenario$lambda01_Hetero_w <- TRUE
# scenario$lambda01_Hetero_bw <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$lambda01_Hetero_w <- FALSE
# scenario$lambda01_Hetero_bw <- FALSE
# 
# # with default prognosis (and mature data) for the non-target indications
# # the default trial duration is 115 months and sample sizes are 593
# # with immature data the trial duration is 25 months and sample sizes are 3563 (need to compensate for shorter duration to maintain precision)
# scenario$Immature_non_target <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$Immature_non_target <- FALSE
# 
# # with default prognosis (and mature data) for the target indications
# # the default trial duration is 115 months and sample sizes are 296 (half of the target indications as imprecise by default)
# # with immature data the trial duration is 25 months and sample sizes are 1781 (need to compensate for shorter duration to maintain precision)
# scenario$Immature_target <- TRUE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$Immature_target <- FALSE
# 
# # no observed OS in the target indication
# scenario$OS_data_for_target <- FALSE
# dataset_output <- fn_create_dataset_array()
# View(as.data.frame(dataset_output)) 
# scenario$OS_data_for_target <- TRUE


### Create dataframe of predictions from df_scenarios ###########
# OUTPUT: df_prediction_full
# = dataframe in which each row is a combination of a model and a dataset implied by the user (through the excel input file "Define scenarios.xlsx)
# each of these rows will be run n_sim times in the simulation study

fn_define_prediction_scenarios <- function(){
  
  # initialise empty dataframe to store scenarios
  df_prediction_full <- as.data.frame(NULL)
  
  for(j in 1:dim(df_scenarios)[1]){
    
    # for dataset j, how mamy time periods to evaluate
    if(df_scenarios$Times[j] == "all"){
      Times <- c("early", "mid", "late")
    } else{
      Times <- df_scenarios$Times[j]
    }
    
    # for dataset j,how mamy models to evaluate
    if(df_scenarios$Models[j] == "Jan_uni"){
      # all Jans univariate models
      Models <- c("uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie")
    } else if(df_scenarios$Models[j] == "all_minus_mix"){
      Models <- c("uni_ie", "uni_ce", "uni_re",
                  "bi_ce_matched",   "bi_re_matched", 
                  "bi_ce_unmatched", "bi_re_unmatched",
                  "uni_ie_w_share", "uni_ce_w_share", "uni_re_w_share",
                  # Surrogate models which share on within indication heterogeneity for the matched analysis of PFS
                  "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                  "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share") 
    } else if(df_scenarios$Models[j] == "all"){
      Models <- c(
        # Jan's univariate models
        "uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
        # Jan's surrogate models (not including mixtures)
        "bi_ce_matched",   "bi_re_matched", 
        "bi_ce_unmatched", "bi_re_unmatched",
        # Jan's univariate models but sharing on within indication heterogeneity
        "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
        # Surrogate models which share on within indication heterogeneity for the matched analysis of PFS
        "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
        "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share") 
    }
    
    # for dataset j,is there OS data in the target indication or not
    if(df_scenarios$Target_OS[j] == "both"){
      Target_OS <- c("with OS", "no OS")
    } else{
      Target_OS <- df_scenarios$Target_OS[j]
    }
    
    df_scenarios_j <- expand.grid(
      Times = Times, 
      Target_OS = Target_OS,
      Models = Models)
    
    ## add the name of the parameter set
    Parameter_set <- df_scenarios$Parameter_set[j]
    df_scenarios_j <- cbind(Parameter_set, df_scenarios_j)
    
    ## add the name of the model
    # not all scenarios in df_prediction_full require a separate model
    # e.g. need one univariate model per sub scenario (1001, 1002) and time (early, mid, late) 
    # use this same model to predict with and without OS in target
    univariate_row_index <- grepl("uni_", df_scenarios_j$Models, fixed = TRUE)
    univariate_models_to_fit <- paste0(df_scenarios_j$Parameter_set[univariate_row_index],
                                       "_", df_scenarios_j$Models[univariate_row_index], "_",
                                       df_scenarios_j$Times[univariate_row_index])
    df_scenarios_j$Model_to_fit[univariate_row_index] <- univariate_models_to_fit

    # Need one set of bivariate models per sub scenario (1001, 1002), time (early, mid, late)
    # use one set of 4 models to predict bi_ce and another set to predict bi_re
    # this will work for: matched, unmatched, with OS, no OS, share and no share
    bivariate_row_index <- grepl("bi_", df_scenarios_j$Models, fixed = TRUE)
    bivariate_models_to_fit <- paste0(df_scenarios_j$Parameter_set[bivariate_row_index],
                                      "_",     str_extract(df_scenarios_j$Models[bivariate_row_index], "[^_]*_[^_]*"),
                                      "_",
                                       df_scenarios_j$Times[bivariate_row_index])
    df_scenarios_j$Model_to_fit[bivariate_row_index] <- bivariate_models_to_fit
    
    # append to the main data frame
    df_prediction_full <- rbind(df_prediction_full, df_scenarios_j)
  }
  
  # simualtion time
  # message(paste("Fit", length(unique(df_prediction_full$Model_to_fit)), "models, each with", n_sim, "iterations. Resulting in",length(unique(df_prediction_full$Model_to_fit))*n_sim ,"models to fit in total."))
  # print estimate of computation time required
  # message(paste("Estimated total computation time required:", length(unique(df_prediction_full$Model_to_fit))*n_sim*(nIterations)*0.0000096, "minutes (serial on Davids laptop)." ))
  # print estimate of memory required
  # message(paste("Estimated total memory storage required:", length(unique(df_prediction_full$Model_to_fit))*n_sim*(nIterations)*0.00000203666, "GB. 880 GB avaiable on David's Google drive." ))
  
  
  #return result
  df_prediction_full
  
}

### Run dataset loop #######

fn_run_dataset_loop_i <- function(i){
  
  # set a unique seed for this iteration: ensures samples of heterogeneous parameters differ across iterations
  set.seed(v_seeds_iteration[i])
  
  start_time_i <- Sys.time()
  
  for(p in 1:dim(df_scenarios)[1]){
    # for the settings in parameter set p
    scenario <<- df_scenarios[p,]
    
    # create data set array object for iteration i in parameter set p
    df_data_set_p_i <- fn_create_dataset_df(i = i)
    
    # name of object
    df_data_set_p_i_name <- paste0("df_data_set_", scenario$Parameter_set, "_i", i)
    
    # save result
    write.csv(df_data_set_p_i,
              file = paste0(results_location, "Datasets/Data/",df_data_set_p_i_name, ".csv"),
              row.names = FALSE)
    
    # print examples for iterations 1 and 2
    if(i <= 2){
      # total dataset
      png(filename=paste0(results_location, "Datasets/Figures/Full_dataset_", df_data_set_p_i_name, ".png"))
      fn_plot_full_dataset(df_data_set_p_i, 
                           df_data_set_p_i_name)
      dev.off()
      # surrogacy in largest indication (indication 2)
      png(filename=paste0(results_location, "Datasets/Figures/Surrogacy_", df_data_set_p_i_name, ".png"))
      fn_plot_indication_surrogacy(df_data_set = df_data_set_p_i[df_data_set_p_i$indication_index == 2,],
                                   plot_title = paste0(df_data_set_p_i_name, " largest indication"))
      dev.off()
    }
      
    # report on progress
    print(paste("Finished dataset for parameter set", scenario$Parameter_set, ":", p, "out of", dim(df_scenarios)[1]))
    
  }
  # time taken for iteration i
  end_time_i <- Sys.time()
  time_taken_i <- difftime(end_time_i, start_time_i, units = "mins")
  print(paste(round(time_taken_i, 2), "minutes required to generate datasets for iteration", i))
  
}

# test
# fn_run_dataset_loop_i(i = 2)



## FIT MODELS #########

### Fit model m to dataset i #############

fn_fit_model <- function(m, i){
    
    ## name of model m which will be fitted
    name_model_to_fit <- v_Model_to_fit[m]
    
    ## Assign dataset for iteration i and model m ####

    ## index of dataset to fit model m to
    # extract everything before the first underscore
    dataset_index <- sub("\\_.*", "", name_model_to_fit)
    
    ### read in iteration i of dataset_index 
    # double assignment arrow as this needs to be used in other functions
    df_fit <- read.csv(paste0(results_location, "Datasets/Data/",
                    "df_data_set_", dataset_index, "_i", i, ".csv"))

    ### adjust dataset
    
    ## Remove OS data from the "second" target indication (indication index 8)
    df_fit$OS_LHR_mean[df_fit$indication_index == 8] <- NA
    df_fit$OS_LHR_se[df_fit$indication_index == 8] <- NA
    
    ## Early, mid or late dataset
    # if its a late time period then leave the dataset as it is
    # if its a mid time period then remove some studies
    # if its an early dataset then remove more studies
    
    if(grepl("mid", name_model_to_fit, fixed = TRUE)){
        ## If mid dataset Fit on:
        # 4 non-target indications + target indication
        #                 1 study (index 1 - target, 7 studies (index 2), 3 studies (index 3), 3 studies (index 4) 2 studies (index 5), 1 study (2nd target, index 8))
        rows_to_keep <- c(1,                         2:8,                11:13,               19:21,               25:26,               31)
        rows_to_remove <- setdiff(1:30, rows_to_keep)

        # assign missing values to the OS and PFS data in the rows to remove
        df_fit$PFS_LHR_mean[rows_to_remove] <- NA
        df_fit$PFS_LHR_se[rows_to_remove] <- NA
        df_fit$OS_LHR_mean[rows_to_remove] <- NA
        df_fit$OS_LHR_se[rows_to_remove] <- NA
      } else if(grepl("early", name_model_to_fit, fixed = TRUE)){
        # if early dataset Fit on:
        # 2 non-target indications + target indication
        #                 1 study (index 1 - target, 3 studies (index 2) 2 studies (index 3), 1 study (2nd target, index 8)))
        rows_to_keep <- c(1,                         2:4,                11:12,               31)
        rows_to_remove <- setdiff(1:30, rows_to_keep)

        # assign missing values to the OS and PFS data in the rows to remove
        df_fit$PFS_LHR_mean[rows_to_remove] <- NA
        df_fit$PFS_LHR_se[rows_to_remove] <- NA
        df_fit$OS_LHR_mean[rows_to_remove] <- NA
        df_fit$OS_LHR_se[rows_to_remove] <- NA
      }
    
    ### Fit models ####
    
    # duplicated_columns <- duplicated(t(as.data.frame(MCMC_Output[[1]])))
    # duplicated_columns
    # sum(duplicated_columns)
    # head(MCMC_Output[[1]])
    
    ##### Univariate #####
    
    ## Jan univariate models
    # IE
    if(grepl("uni_ie", name_model_to_fit, fixed = TRUE)){
      if(grepl("w_share", name_model_to_fit, fixed = TRUE)){
        # share on within indication heterogeneity
        MCMC_Output <- applyIe_JAGS(dataset = df_fit, w_share = TRUE)
      } else {
        # independent within indication heterogeneity
        MCMC_Output <- applyIe_JAGS(dataset = df_fit, w_share = FALSE)
      }
    }
    
    # CE
    if(grepl("uni_ce", name_model_to_fit, fixed = TRUE)){
      if(grepl("w_share", name_model_to_fit, fixed = TRUE)){
        # share on within indication heterogeneity
        MCMC_Output <- applyCe_JAGS(dataset = df_fit, w_share = TRUE)
      } else {
        # independent within indication heterogeneity
        MCMC_Output <- applyCe_JAGS(dataset = df_fit, w_share = FALSE)
      }
    }
    
    # MCIE
    if(grepl("uni_mcie", name_model_to_fit, fixed = TRUE)){
      if(grepl("w_share", name_model_to_fit, fixed = TRUE)){
        # share on within indication heterogeneity
        MCMC_Output <- applyMcie_JAGS(dataset = df_fit, w_share = TRUE)
      } else {
        # independent within indication heterogeneity
        MCMC_Output <- applyMcie_JAGS(dataset = df_fit, w_share = FALSE)
      }
    }
    
    # RE 
    if(grepl("uni_re", name_model_to_fit, fixed = TRUE)){
      if(grepl("w_share", name_model_to_fit, fixed = TRUE)){
        # share on within indication heterogeneity
        MCMC_Output <- applyRe_JAGS(dataset = df_fit, w_share = TRUE)
      } else {
        # independent within indication heterogeneity
        MCMC_Output <- applyRe_JAGS(dataset = df_fit, w_share = FALSE)
      }
    }
    
    # MRIE
    if(grepl("uni_mrie", name_model_to_fit, fixed = TRUE)){
      if(grepl("w_share", name_model_to_fit, fixed = TRUE)){
        # share on within indication heterogeneity
        MCMC_Output <- applyMrie_JAGS(dataset = df_fit, w_share = TRUE)
      } else {
        # independent within indication heterogeneity
        MCMC_Output <- applyMrie_JAGS(dataset = df_fit, w_share = FALSE)
      }
    }
    
    ##### Bivariate #####
    
    ## Jan bivariate models
    # dont fit:
    # IE - cannot fit IE surrogacy model
    # MCIE - too complex
    # MRIE - too complex
    
    # CE
    if(grepl("bi_ce", name_model_to_fit, fixed = TRUE)){
      MCMC_Output <- vector(mode = "list", length = 5)
      names(MCMC_Output) <- c("codaSamples_surrogate", 
                              "codaSamples_PFS_matched",
                              "codaSamples_PFS_matched_w_share", 
                              "codaSamples_PFS_unmatched",
                              "codaSamples_PFS_unmatched_w_share")
      # run surrogate model
      MCMC_Output[[1]] <- apply_bi_ce(dataset = df_fit)
      # Univariate PFS model
      # matched => CE model for PFS
      MCMC_Output[[2]] <- applyCe_JAGS_PFS(dataset = df_fit, w_share = FALSE)
      # matched => CE model for PFS + share on within indication heterogeneity
      MCMC_Output[[3]] <- applyCe_JAGS_PFS(dataset = df_fit, w_share = TRUE)
      # unmatched => IE model for PFS
      MCMC_Output[[4]] <- applyIe_JAGS_PFS(dataset = df_fit, w_share = FALSE)
      # unmatched => IE model for PFS + share on within indication heterogeneity
      MCMC_Output[[5]] <- applyIe_JAGS_PFS(dataset = df_fit, w_share = TRUE)
    }
    
    # RE
    if(grepl("bi_re", name_model_to_fit, fixed = TRUE)){
      MCMC_Output <- vector(mode = "list", length = 5)
      names(MCMC_Output) <- c("codaSamples_surrogate", 
                              "codaSamples_PFS_matched",
                              "codaSamples_PFS_matched_w_share", 
                              "codaSamples_PFS_unmatched",
                              "codaSamples_PFS_unmatched_w_share")
      # run surrogate model
      MCMC_Output[[1]] <- apply_bi_re(dataset = df_fit)
      # Univariate PFS model
      # if matched => RE model for PFS
      MCMC_Output[[2]] <- applyRe_JAGS_PFS(dataset = df_fit, w_share = FALSE)
      # if matched => RE model for PFS + share on within indication heterogeneity
      MCMC_Output[[3]] <- applyRe_JAGS_PFS(dataset = df_fit, w_share = TRUE)
      # if unmatched => IE model for PFS
      MCMC_Output[[4]] <- applyIe_JAGS_PFS(dataset = df_fit, w_share = FALSE)
      # if unmatched => IE model for PFS + share on within indication heterogeneity
      MCMC_Output[[5]] <- applyIe_JAGS_PFS(dataset = df_fit, w_share = TRUE)
    }
    
    ## save MCMC posterior summaries (means)
    if(grepl("uni_", name_model_to_fit, fixed = TRUE)){
      # for univariate models
      
      # convert MCMC object to matrix 
      MCMC_temp <- as.matrix(MCMC_Output$codaSamples_OS, iters = FALSE)
      # initialize dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Post_means <- colMeans(MCMC_temp)
      Model_posterior_summary <- data.frame(matrix(NA, nrow = length(Post_means),
                                                   ncol = 1+ length(Post_summaries)))
      colnames(Model_posterior_summary) <- c("Parameter", Post_summaries)
      Model_posterior_summary$Parameter <- names(Post_means)

      # calculate results
      Model_posterior_summary$Mean <- apply(MCMC_temp, 2, mean)  
      Model_posterior_summary$SD <- apply(MCMC_temp, 2, sd)  
      Model_posterior_summary$q_0.025 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.025))  
      Model_posterior_summary$q_0.25 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.25))  
      Model_posterior_summary$q_0.50 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.50))  
      Model_posterior_summary$q_0.75 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.75))  
      Model_posterior_summary$q_0.975 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.975))  
      
      write.csv(Model_posterior_summary, file = paste0(results_location, "Model_posterior_summary/MCMC_Output_", name_model_to_fit,"_i",i ,".csv"))
    } else {
      # for bivariate models: save summary of both the surrogate model and the PFS univariate model
      # - MCMC_Output_surrogate_3034_bi_ce_matched_late_i1.csv
      # - MCMC_Output_PFS_3034_bi_ce_matched_late_i1.csv
      
      ## bivariate model
      # convert MCMC object to matrix 
      MCMC_temp <- as.matrix(MCMC_Output$codaSamples_surrogate, iters = FALSE)
      # initialsie dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Post_means <- colMeans(MCMC_temp)
      Model_posterior_summary <- data.frame(matrix(NA, nrow = length(Post_means),
                                                   ncol = 1+ length(Post_summaries)))
      colnames(Model_posterior_summary) <- c("Parameter", Post_summaries)
      Model_posterior_summary$Parameter <- names(Post_means)
      # calculate results
      Model_posterior_summary$Mean <- apply(MCMC_temp, 2, mean)  
      Model_posterior_summary$SD <- apply(MCMC_temp, 2, sd)  
      Model_posterior_summary$q_0.025 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.025))  
      Model_posterior_summary$q_0.25 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.25))  
      Model_posterior_summary$q_0.50 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.50))  
      Model_posterior_summary$q_0.75 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.75))  
      Model_posterior_summary$q_0.975 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.975))  
      write.csv(Model_posterior_summary, file = paste0(results_location, "Model_posterior_summary/MCMC_Output_surrogate_", name_model_to_fit,"_i",i ,".csv"))

      ## PFS matched (no within indication sharing)
      # convert MCMC object to matrix 
      MCMC_temp <- as.matrix(MCMC_Output$codaSamples_PFS_matched, iters = FALSE)
      # initialsie dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Post_means <- colMeans(MCMC_temp)
      Model_posterior_summary <- data.frame(matrix(NA, nrow = length(Post_means),
                                                   ncol = 1+ length(Post_summaries)))
      colnames(Model_posterior_summary) <- c("Parameter", Post_summaries)
      Model_posterior_summary$Parameter <- names(Post_means)
      # calculate results
      Model_posterior_summary$Mean <- apply(MCMC_temp, 2, mean)  
      Model_posterior_summary$SD <- apply(MCMC_temp, 2, sd)  
      Model_posterior_summary$q_0.025 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.025))  
      Model_posterior_summary$q_0.25 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.25))  
      Model_posterior_summary$q_0.50 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.50))  
      Model_posterior_summary$q_0.75 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.75))  
      Model_posterior_summary$q_0.975 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.975))  
      write.csv(Model_posterior_summary, file = paste0(results_location, "Model_posterior_summary/MCMC_Output_PFS_matched", name_model_to_fit,"_i",i ,".csv"))
      
      ## PFS matched (WITH within indication sharing)
      # convert MCMC object to matrix 
      MCMC_temp <- as.matrix(MCMC_Output$codaSamples_PFS_matched_w_share, iters = FALSE)
      # initialsie dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Post_means <- colMeans(MCMC_temp)
      Model_posterior_summary <- data.frame(matrix(NA, nrow = length(Post_means),
                                                   ncol = 1+ length(Post_summaries)))
      colnames(Model_posterior_summary) <- c("Parameter", Post_summaries)
      Model_posterior_summary$Parameter <- names(Post_means)
      # calculate results
      Model_posterior_summary$Mean <- apply(MCMC_temp, 2, mean)  
      Model_posterior_summary$SD <- apply(MCMC_temp, 2, sd)  
      Model_posterior_summary$q_0.025 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.025))  
      Model_posterior_summary$q_0.25 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.25))  
      Model_posterior_summary$q_0.50 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.50))  
      Model_posterior_summary$q_0.75 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.75))  
      Model_posterior_summary$q_0.975 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.975))  
      write.csv(Model_posterior_summary, file = paste0(results_location, "Model_posterior_summary/MCMC_Output_PFS_matched_w_share", name_model_to_fit,"_i",i ,".csv"))
      
      ## PFS unmatched (no within indication sharing)
      # convert MCMC object to matrix 
      MCMC_temp <- as.matrix(MCMC_Output$codaSamples_PFS_unmatched, iters = FALSE)
      # initialsie dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Post_means <- colMeans(MCMC_temp)
      Model_posterior_summary <- data.frame(matrix(NA, nrow = length(Post_means),
                                                   ncol = 1+ length(Post_summaries)))
      colnames(Model_posterior_summary) <- c("Parameter", Post_summaries)
      Model_posterior_summary$Parameter <- names(Post_means)
      # calculate results
      Model_posterior_summary$Mean <- apply(MCMC_temp, 2, mean)  
      Model_posterior_summary$SD <- apply(MCMC_temp, 2, sd)  
      Model_posterior_summary$q_0.025 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.025))  
      Model_posterior_summary$q_0.25 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.25))  
      Model_posterior_summary$q_0.50 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.50))  
      Model_posterior_summary$q_0.75 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.75))  
      Model_posterior_summary$q_0.975 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.975))  
      write.csv(Model_posterior_summary, file = paste0(results_location, "Model_posterior_summary/MCMC_Output_PFS_unmatched", name_model_to_fit,"_i",i ,".csv"))
      
      ## PFS unmatched (WITH within indication sharing)
      # convert MCMC object to matrix 
      MCMC_temp <- as.matrix(MCMC_Output$codaSamples_PFS_unmatched_w_share, iters = FALSE)
      # initialsie dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Post_means <- colMeans(MCMC_temp)
      Model_posterior_summary <- data.frame(matrix(NA, nrow = length(Post_means),
                                                   ncol = 1+ length(Post_summaries)))
      colnames(Model_posterior_summary) <- c("Parameter", Post_summaries)
      Model_posterior_summary$Parameter <- names(Post_means)
      # calculate results
      Model_posterior_summary$Mean <- apply(MCMC_temp, 2, mean)  
      Model_posterior_summary$SD <- apply(MCMC_temp, 2, sd)  
      Model_posterior_summary$q_0.025 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.025))  
      Model_posterior_summary$q_0.25 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.25))  
      Model_posterior_summary$q_0.50 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.50))  
      Model_posterior_summary$q_0.75 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.75))  
      Model_posterior_summary$q_0.975 <- apply(MCMC_temp, 2, function(x) quantile(x, 0.975))  
      write.csv(Model_posterior_summary, file = paste0(results_location, "Model_posterior_summary/MCMC_Output_PFS_unmatched_w_share", name_model_to_fit,"_i",i ,".csv"))
      
    }
    
    # double assignment arrow as this needs to be used in other functions
    df_fit <<- df_fit
    
    ## return result
    MCMC_Output
}

### OS for prediction r ################# 
# output MCMC sample for OS in target
# results based on MCMC_Output object

fn_OS_posterior <- function(r, i){
  
  ### Draw posterior estimate for target indication in simulation r
  # Specific to each model (e.g. univariate independent effects "uni_ie") 
  # and whether or no there is OS in the target indication (e.g. "with OS")
  
  ### Univariate #####
  
  # note:
  # bugs orders the study specific effect (theta[j]) in the order that studies appear in the dataset 
  # theta[1] is the 1st study in the dataset 
  # appears to be the same for the indication ordering 
  # => mu.theta[1] is the 1st indication in the dataset
  
  # When there is OS in the target ("with OS") the target indication is indication 1 
  # => the 1st study (theta[1]) and 1st indication (mu.theta[1]) in the dataset
  # When there is no OS in the target ("no OS") the target indication is indication 8 (the last in the dataset)
  # => the last study (??) and last indication (??) in the dataset
  
  ## IE/IP
  if(grepl("uni_ie", df_prediction_full$Models[r], fixed = TRUE) & df_prediction_full$Target_OS[r] == "with OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # in this case the posterior for the target indication is simply 
    # mu.theta for the target indication (with OS => 1st indication in dataset)
    # this is the posterior estimate for the true effect in the target indication
    # (note we do not consider the within indication SD as we do not want to predict study specific effects, just the indication effect)    
    
    # the code below extracts posteriors for mu.theta for the 1st indication
    #target_posterior <- MCMC_Output$sims.list$mu.theta[,1] 
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mu.theta[1]"]
    
  }
  if(grepl("uni_ie", df_prediction_full$Models[r], fixed = TRUE) & df_prediction_full$Target_OS[r] == "no OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # in this case there is no possible prediction for the target indication
    # we draw from the prior distribution in this case
    # note that we use a different seed in each iteration so this will differ across iterations
    target_posterior <- rnorm((nIterations - nBurnin)*3, 0, 10)
  }  
  
  ## CE/CP
  if(grepl("uni_ce", df_prediction_full$Models[r], fixed = TRUE)){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # for common effects models the posterior for mean across all indications is common and is stored in mu
    # mu is appropriate whether there is OS target data or not (the specific target indication is irrelevant)
    # target_posterior <- MCMC_Output$sims.list$mu
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mu"]
  }
  
  ## MCIE/MCIP
  if(grepl("uni_mcie", df_prediction_full$Models[r], fixed = TRUE) & 
     df_prediction_full$Target_OS[r] == "with OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # mu.theta embodies both components of the mixture model: common and independent
    # in this case the posterior for the target indication is simply 
    # mu.theta for the target indication (with OS => 1st indication in dataset)
    # target_posterior <- MCMC_Output$sims.list$mu.theta[,1] 
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mu.theta[1]"]

  }
  if(grepl("uni_mcie", df_prediction_full$Models[r], fixed = TRUE) & 
     df_prediction_full$Target_OS[r] == "no OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # in this case assume that the new indication is the same as the most common other indications
    # i.e. the best prediction is from the "sharing component" - assumes a mixture probability of 100%
    #target_posterior <- MCMC_Output$sims.list$mu
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mu"]
  }  
  
  ## RE/RP
  if(grepl("uni_re", df_prediction_full$Models[r], fixed = TRUE) & 
     df_prediction_full$Target_OS[r] == "with OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # shurnken estimate for the target indication (with OS => 1st indication in dataset)
    #target_posterior <- MCMC_Output$sims.list$mu.theta[,1] 
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mu.theta[1]"]
    
  }
  if(grepl("uni_re", df_prediction_full$Models[r], fixed = TRUE) & 
     df_prediction_full$Target_OS[r] == "no OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # predictive distribution for a new indication, mPred ~ dnorm(mMu, mPrec)
    # target_posterior <- MCMC_Output$sims.list$mPred
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mPred"]
    
  }
  
  ## MRIE/MRIP
  if(grepl("uni_mrie", df_prediction_full$Models[r], fixed = TRUE) & 
     df_prediction_full$Target_OS[r] == "with OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # use shurnken estimate for the target indication (with OS => 1st indication in dataset)
    # target_posterior <- MCMC_Output$sims.list$mu.theta[,1] 
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mu.theta[1]"]
    
  }
  if(grepl("uni_mrie", df_prediction_full$Models[r], fixed = TRUE) & 
     df_prediction_full$Target_OS[r] == "no OS"){
    # Note: Same approach for models which share within indication heterogeneity (w_share) and those that dont
    # in this case assume that the new indication is the same as the most common other indications
    # i.e. the best prediction is from the "sharing component" - assumes a mixture probability of 100%
    # target_posterior <- MCMC_Output$sims.list$mPred
    target_posterior <- do.call(rbind, MCMC_Output$codaSamples_OS)[,"mPred"]
  }
  
  ### Bivariate ######
  
  ## define surrogate index in JAGS output for PFS analysis 
  # used when choosing the correct PFS target below
  if(df_prediction_full$Target_OS[r] == "with OS"){
    # in the "with OS" case then the target indication is indexed by [1] in the JAGS output
    JAGS_target_index <- 1
  } else {
    # in the "no OS" case then the target indication is at the end of dataset used to fit the model
    JAGS_target_index <- length(unique(df_fit[complete.cases(df_fit$PFS_LHR_mean), "indication_index"]))
  }
  
  ## CE/CP matched and unmatched
  if(any(df_prediction_full$Models[r] == c("bi_ce_matched", "bi_ce_matched_w_share", "bi_ce_unmatched", "bi_ce_unmatched_w_share"))){ 
    # the prediction for OS is lambda0_j + lambda1_j*delta_i_j  ( old: ~ N(lambda0_j + lambda1_j*delta_i_j, psi_j^2) )
    
    ## draw samples from PFS LHR in target indication: delta_i_j 
    if(grepl("unmatched", df_prediction_full$Models[r], fixed = TRUE)){
      # if unmatched => need to draw PFS LHR samples from IE PFS estimate
      # for the appropriate target indication
      # may be with our without sharing within indicaiton heterogeneity
      if(grepl("w_share", df_prediction_full$Models[r], fixed = TRUE)){
        # sharing within indication hetero
        delta_i_j <-do.call(rbind, MCMC_Output$codaSamples_PFS_unmatched_w_share)[,paste0("mu.theta[",JAGS_target_index,"]")]
      } else {
        # no sharing within indication hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_unmatched)[,paste0("mu.theta[",JAGS_target_index,"]")]
      }
    } else {
      # if matched => draw from CE univariate PFS model
      # may be with our without sharing within indicaiton heterogeneity
      if(grepl("w_share", df_prediction_full$Models[r], fixed = TRUE)){
        # sharing within indicaiton hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_matched_w_share)[,"mu"]
      } else {
        # no sharing within indication hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_matched)[,"mu"]
      }
    }
    
    ## draw the MCMC samples for the surrogacy model for the target indication for CE
    # lambda0_j, lambda1_j
    lambda0_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,"mLambda0"]
    lambda1_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,"mLambda1"]
    # psi_j <-     do.call(rbind, MCMC_Output$codaSamples_surrogate)[,"mPsi"]
    
    ## draw from posterior for OS
    # = lambda0_j + lambda1_j*delta_i_j
    # thinning can result in small difference in size of posterior 
    target_posterior_length <- min(length(lambda0_j), length(lambda1_j), length(delta_i_j))
    target_posterior <- lambda0_j[1:target_posterior_length] + 
      lambda1_j[1:target_posterior_length]*delta_i_j[1:target_posterior_length]
  }
  
  ## RE matched / unmatched
  # The matched PFS comes from the RE target indication specific estimate e.g. "mu.theta[j]
  # also for each model the surrogate model parameters also come from the target indication specific estimate e.g. "lambda0[1], lambda1[1] and psi[1]
  if(any(df_prediction_full$Models[r] == c("bi_re_matched","bi_re_matched_w_share" ,"bi_re_unmatched", "bi_re_unmatched_w_share"))){
    # the prediction for OS is lambda0_j + lambda1_j*delta_i_j  ( old: ~ N(lambda0_j + lambda1_j*delta_i_j, psi_j^2) )
    
    ## draw samples from PFS LHR in target indication: delta_i_j 
    if(grepl("unmatched", df_prediction_full$Models[r], fixed = TRUE)){
      # if unmatched => need to draw PFS LHR samples from IE PFS estimate
      # for the appropriate target indication
      
      # may be with our without sharing within indicaiton heterogeneity
      if(grepl("w_share", df_prediction_full$Models[r], fixed = TRUE)){
        # sharing within indicaiton hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_unmatched_w_share)[,paste0("mu.theta[",JAGS_target_index,"]")]
      } else {
        # no sharing within indication hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_unmatched)[,paste0("mu.theta[",JAGS_target_index,"]")]
      }
      
    } else {
      # if matched => draw from RE univariate PFS model
      # there is always PFS in the target distribution 
      # so use the shurnken estimate for PFS effect in the target distribution

      # may be with our without sharing within indicaiton heterogeneity
      if(grepl("w_share", df_prediction_full$Models[r], fixed = TRUE)){
        # sharing within indicaiton hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_matched_w_share)[,paste0("mu.theta[",JAGS_target_index,"]")]
      } else {
        # no sharing within indication hetero
        delta_i_j <- do.call(rbind, MCMC_Output$codaSamples_PFS_matched)[,paste0("mu.theta[",JAGS_target_index,"]")]
      }
    }
    
    ## draw the MCMC samples for the surrogacy model for the target indication
    # lambda0_j, lambda1_j
    if(df_prediction_full$Target_OS[r] == "no OS"){
      # predict the surrogacy relationship in the target indication
      # mPred equivalent for lambda0_j, lambda1_j
      
      # lambda0_j ~ N(beta0, xi0^2)
      lambda0_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,paste0("lambda0Pred")]
      # lambda1_j ~ N(beta1, xi1^2)
      lambda1_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,paste0("lambda1Pred")]
      # # psi ~ |N(0, h)|
      #psi_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,paste0("psiPred")]
      
    } else if(df_prediction_full$Target_OS[r] == "with OS"){
      # use the shurnken estimate for the target indication
      # when "with OS" the target is the first indiction in the dataset
      lambda0_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,paste0("lambda0[1]")]
      lambda1_j <- do.call(rbind, MCMC_Output$codaSamples_surrogate)[,paste0("lambda1[1]")]
      # psi_j <-     do.call(rbind, MCMC_Output$codaSamples_surrogate)[,paste0("psi[1]")]
    }
  
    ## draw from posterior for OS
    # = lambda0_j + lambda1_j*delta_i_j
    # thinning can result in small difference in size of posterior 
    target_posterior_length <- min(length(lambda0_j), length(lambda1_j), length(delta_i_j))
    target_posterior <- lambda0_j[1:target_posterior_length] + 
      lambda1_j[1:target_posterior_length]*delta_i_j[1:target_posterior_length]
  }
  
  ## return result
  target_posterior
}


### Run model loop for all iterations  ################

fn_run_model_loop_i <- function(i){
  
  start_time_i <- Sys.time()
  
  ## Loop over models to fit ###########
  ## number of models to run
  
  m_total <- length(v_Model_to_fit)
  # loop over models in v_Model_to_fit in order
  
  for(m in 1:m_total){
    ### Fit model m to dataset i #####
    cat('\r', paste("Start model", m, "of", m_total ,"(" ,v_Model_to_fit[m], ")"))
    
    start_time_m <- Sys.time()
    MCMC_Output <<- try(fn_fit_model(m=m, i=i))
    end_time_m <- Sys.time()
    time_taken_m <- difftime(end_time_m, start_time_m, units = "mins")
    print(paste(round(time_taken_m, 2), "minutes required to fit model", m, "of", m_total))
    
    ### Note: one model can be used to make predictions for more than one scenario
    # e.g. "1001_uni_ce_early" is used to predict outcomes for both
    # 1001 early   with OS uni_ce 
    # 1001 early     no OS uni_ce
    common_scenario_index <- which(df_prediction_full$Model_to_fit == v_Model_to_fit[m])
    
    # names of the scenarios - for naming objects
    common_scenario_index_names <- paste0(df_prediction_full$Parameter_set[1:nrow(df_prediction_full)], "_",
                                          df_prediction_full$Times[1:nrow(df_prediction_full)], "_",
                                          df_prediction_full$Target_OS[1:nrow(df_prediction_full)], "_",
                                          df_prediction_full$Models[1:nrow(df_prediction_full)])
    
    # loop over all relevant scenarios for model m and iteration i
    for(r in common_scenario_index){
      
      ### Calcualte target OS predictions for scenario r (model m to dataset i) #####
      df_target_posteriors_r_i <- try(fn_OS_posterior(r = r, i = i))
      
      if(user != "Viking"){
        # save full posterior if on desktop - too large to save on viking
        try(write.csv(df_target_posteriors_r_i,
                      file = paste0(results_location, "TargetOS/", "df_post_", common_scenario_index_names[r], "_i" , i, ".csv"),
                      row.names = FALSE))
      }

      # save summary of posterior
      # initialsie dataframe to hold results
      Post_summaries <- c("Mean", "SD",  
                          "q_0.025", "q_0.25", "q_0.50", "q_0.75", "q_0.975")
      Target_posterior_summary <- data.frame(matrix(NA, nrow = 1,
                                                   ncol = length(Post_summaries)))
      colnames(Target_posterior_summary) <- Post_summaries
      # calculate results
      Target_posterior_summary$Mean <- mean(df_target_posteriors_r_i)  
      Target_posterior_summary$SD <- sd(df_target_posteriors_r_i)  
      Target_posterior_summary$q_0.025 <- quantile(df_target_posteriors_r_i, 0.025) 
      Target_posterior_summary$q_0.25 <- quantile(df_target_posteriors_r_i, 0.25) 
      Target_posterior_summary$q_0.50 <- quantile(df_target_posteriors_r_i, 0.50) 
      Target_posterior_summary$q_0.75 <- quantile(df_target_posteriors_r_i, 0.75) 
      Target_posterior_summary$q_0.975 <- quantile(df_target_posteriors_r_i, 0.975) 
      # save results
      write.csv(Target_posterior_summary, 
                file = paste0(results_location, "TargetOS_posterior_summary/", "df_post_summary_", common_scenario_index_names[r], "_i" , i, ".csv"))
                
      ### Calcualte convergence metrics for scenario r (model m to dataset i) #####
      # calculate Gelman Rubin Rhat metric using coda package
      # set multivariate to false as there is an issue with this statistic in this package when parameters are correlated
      
      ## Rhat univariate models
      if(grepl("_uni", common_scenario_index_names[r], fixed = TRUE)){
        # Rhat values for the univariate model
        df_Rhat_r_i <- try(gelman.diag(MCMC_Output$codaSamples_OS, multivariate = FALSE)[[1]])
      } 
      
      ## Rhat surrogate models
      if(grepl("_bi", common_scenario_index_names[r], fixed = TRUE)){
        if(grepl("_matched", common_scenario_index_names[r], fixed = TRUE)){
          # matched surrogacy (CE or RE PFS model)
          if(grepl("_w_share", common_scenario_index_names[r], fixed = TRUE)){
            # Rhats for surrogacy and the PFS model (matched WITH share on within indication hetero)
            Rhat_surrogacy_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_surrogate, multivariate = FALSE)[[1]], "surrogate"))
            Rhat_PFS_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_PFS_matched_w_share, multivariate = FALSE)[[1]], "PFS"))
            df_Rhat_r_i <- try(rbind(Rhat_surrogacy_r_i, Rhat_PFS_r_i))
          } else {
            # Rhats for surrogacy and the PFS model (matched NO share on within indication hetero)
            Rhat_surrogacy_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_surrogate, multivariate = FALSE)[[1]], "surrogate"))
            Rhat_PFS_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_PFS_matched, multivariate = FALSE)[[1]], "PFS"))
            df_Rhat_r_i <- try(rbind(Rhat_surrogacy_r_i, Rhat_PFS_r_i))
          }
          
        } else {
          # unmatched surrogacy (IE model)
          if(grepl("_w_share", common_scenario_index_names[r], fixed = TRUE)){
            # Rhats for surrogacy and the PFS model (unmatched WITH share on within indication hetero)
            Rhat_surrogacy_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_surrogate, multivariate = FALSE)[[1]], "surrogate"))
            Rhat_PFS_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_PFS_unmatched_w_share, multivariate = FALSE)[[1]], "PFS"))
            df_Rhat_r_i <- try(rbind(Rhat_surrogacy_r_i, Rhat_PFS_r_i))
            
          } else {
            # Rhats for surrogacy and the PFS model (unmatched NO share on within indication hetero)
            Rhat_surrogacy_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_surrogate, multivariate = FALSE)[[1]], "surrogate"))
            Rhat_PFS_r_i <- try(cbind(gelman.diag(MCMC_Output$codaSamples_PFS_unmatched, multivariate = FALSE)[[1]], "PFS"))
            df_Rhat_r_i <- try(rbind(Rhat_surrogacy_r_i, Rhat_PFS_r_i))
          }
          
        }
      }

      # save result
      try(write.csv(df_Rhat_r_i,
                file = paste0(results_location, "Convergence/", "df_Rhat_", common_scenario_index_names[r], "_i" , i, ".csv"),
                row.names = TRUE))
      
    }
  }
  
  # time taken for iteration i
  end_time_i <- Sys.time()
  time_taken_i <- difftime(end_time_i, start_time_i, units = "mins")
  print(paste(round(time_taken_i, 2), "minutes required to fit models for iteration", i))
  
}

