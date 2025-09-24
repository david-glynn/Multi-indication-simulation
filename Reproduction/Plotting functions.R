

### Plot dataset - forest plot ################
# take in the database in an iteration (i) outputs a forest plot

fn_plot_full_dataset <- function(df_data_set, plot_title){
  
  # initialise plot
  x_axis_bounds <- c(
    -0.6, 0.6
    # # lower bound
    # min(-0.1, min(df_data_set$OS_LHR_mean) - max(df_data_set$OS_LHR_se)*2),
    # # upper bound
    # max( 0.2, max(df_data_set$OS_LHR_mean) - max(df_data_set$OS_LHR_se)*3)
  )
  
  # initialise plot
  plot(1, type="n", 
       xlab="OS log hazard ratio", xlim=x_axis_bounds, 
       ylab="", ylim=c(0, 10), yaxt='n',
       main = plot_title)
  
  # add line of no effect
  abline(v = 0)
  
  # define the y axis values for:
  # each of the studies observed studies + 
  # the unobserved study in indication 8 +
  # the prediction for the target
  number_of_studies <- nrow(df_data_set)
  y_increments <- rep(9.9/(number_of_studies), number_of_studies) # full height is 10
  y_values <- rev(cumsum(y_increments))
  
  ## approximate restricted ALHR OS for each indication
  # dont consider non-linear adjustment
  v_OS_partial_ALHR_indication_true <- rep(NA, nrow(df_data_set))
  for(j in unique(df_data_set$indication_index)){
    # row index for indication j
    row_index_j <- which(df_data_set$indication_index == j) 
    
    # if all parameters are homogeneous within indications can just calculate based on indication level grand mean for lambda01, lambda02 etc
    lambda_01_ctrl_temp <- df_data_set$lambda_01_indication[row_index_j][1]
    lambda_02_ctrl_temp <- df_data_set$lambda_02_indication[row_index_j][1] 
    lambda_12_ctrl_temp <- lambda_02_ctrl_temp*df_data_set$delta_indication[row_index_j][1] 
    lambda_01_trt_temp <- lambda_01_ctrl_temp*df_data_set$M_01_indication[row_index_j][1] 
    lambda_02_trt_temp <- lambda_02_ctrl_temp
    lambda_12_trt_temp <- lambda_12_ctrl_temp
    
    # trial duration ALHR (average duration of trials in the indication)
    multistate_output_temp <- fn_HR_OS_PFS(
      lambda_01_ctrl = lambda_01_ctrl_temp,
      lambda_02_ctrl = lambda_02_ctrl_temp,
      lambda_12_ctrl = lambda_12_ctrl_temp,
      lambda_01_trt = lambda_01_trt_temp,
      lambda_02_trt = lambda_02_trt_temp,
      lambda_12_trt = lambda_12_trt_temp,
      max_time_months_calculate = as.integer(mean(df_data_set$TimefromStart_M[row_index_j])) # average duration of trials in indication
    )
    v_OS_partial_ALHR_indication_true[row_index_j] <- log(multistate_output_temp$AHR_OS)
  }
  
  # plot observed results from all observed indications 
  # and the prediction for the target indication (Pred)
  # The graph should indicate which indcation is the target with a (T) after the indication number
  # with OS => indication 1 is target
  # no OS => indicaiton 8 is target
  labels=c(df_data_set$indication_index)
  labels[which(labels == "1")] <- "1(T)"
  labels[which(labels == "8")] <- "8(T)"

  # add y axis labels to indication indication
  axis(side = 2, at=y_values, labels=labels, las = 2, cex.axis=0.5)
  
  # plot the observed OS (point est and 95 interval), true study OS (partial) and true indication OS (partial)
  # plot for all observed indications
  for(s in 1:nrow(df_data_set)){
    # observed OS point estimate in study s
    points(df_data_set$OS_LHR_mean[s], y_values[s], pch = 16)
    # observed OS 95% interval for study s
    lines(x = c(
      df_data_set$OS_LHR_mean[s] - df_data_set$OS_LHR_se[s]*1.96,
      df_data_set$OS_LHR_mean[s] + df_data_set$OS_LHR_se[s]*1.96),
      y = rep(y_values[s],2))
    # true study OS (partial)
    points(df_data_set$OS_partial_ALHR_study_true[s], y_values[s], pch = 5)
    
    # true indication OS (partial)
    # note: no non-linear adjustment
    points(v_OS_partial_ALHR_indication_true[s], y_values[s], pch = 4, col = "blue")
  }

  # add legend
  legend("topright", legend=c("Observed", "True study", "True indication"),
         pch=c(16, 5, 4), col= c("black", "black", "blue"), cex=0.7)
}

# fn_plot_full_dataset(df_data_set = df_data_set_p_i, plot_title = "a")
  


### Plot joint distribution of OS and PFS ################
# input the rows of a dataframe with correspond to an indication
# output the joint distribution of PFS and OS

fn_plot_indication_surrogacy <- function(df_data_set, plot_title){

  # initialise plot
  y_axis_bounds <- c(
    # lower bound
    min(-0.1, min(df_data_set$OS_LHR_mean) - max(df_data_set$OS_LHR_se)*2),
    # upper bound
    max( 0.1, max(df_data_set$OS_LHR_mean) - max(df_data_set$OS_LHR_se)*2)
  )
  x_axis_bounds <- c(
    # lower bound
    min(-0.1, min(df_data_set$PFS_LHR_mean) - max(df_data_set$PFS_LHR_se)*2),
    # upper bound
    max( 0.1, max(df_data_set$PFS_LHR_mean) - max(df_data_set$PFS_LHR_se)*2)
  )
  
  plot(df_data_set$PFS_LHR_mean , df_data_set$OS_LHR_mean,
       xlim = x_axis_bounds,
       ylim = y_axis_bounds,
       xlab = "PFS LHR",
       ylab = "OS LHR",
       main = plot_title, 
       col = "red")
  abline(h = 0)
  abline(v = 0)
  # surrogate model based on observed studies
  abline(lm(df_data_set$OS_LHR_mean ~ df_data_set$PFS_LHR_mean), col = "red")
  # add true study values
  points(df_data_set$PFS_LHR_study_true, df_data_set$OS_partial_ALHR_study_true, pch = 5, col = "blue")
  try(abline(lm(df_data_set$OS_partial_ALHR_study_true ~ df_data_set$PFS_LHR_study_true), col = "blue"), silent = TRUE)

  # * not showing up!
  # add uncertainty intervals aound observed outcomes
  draw.ellipse(x = df_data_set$PFS_LHR_mean, # midpoint on x axis
               y = df_data_set$OS_LHR_mean, # midpoint on y axis
               a = df_data_set$PFS_LHR_se*1.96, # radius on x axis 
               b = df_data_set$OS_LHR_se*1.96, # radius on y axis
               border=c(2),
               angle=c(0), segment=rbind(rep(45),c(360)))
  # add legend
  legend("bottomright", legend=c("Observed", "True study"),
         pch=c(1, 5), col= c("red", "blue"), cex=0.7)
}

# fn_plot_indication_surrogacy(df_data_set = df_data_set_j_i[df_data_set_j_i$indication_index == 2,],
#                          plot_title = "a")

## Special add legend ############
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

## Multiplot: single tile ###########
# creates a plot and plots results for each of the models in v_models
# (repeatedly call this function when creating the full tile plot for bias, coverage, no outlier etc)
# for a given plot (e.g. bias no outlier = plot 1) 
# and evidence set (e.g. "early_with OS")

# requires l_results to be loaded (see ResultsForPaper.R)

## testing
# simulation_set_number <- 1 # no outlier
# v_models <- c("uni_ce", "uni_re") #  vector of models to plot
# level_of_evidence = "early"
# metric <- "Bias"
# v_model_OS_in_target <- c(TRUE, TRUE)
# v_model_col <- c("red", "green")
# v_model_pch <- c(1, 2)
# ylim_for_metric <- c(-0.02, 0.1)

fn_plot_single_multiplot_tile <- function(simulation_set_number, 
                                          metric, 
                                          level_of_evidence, 
                                          v_models,
                                          v_model_OS_in_target,
                                          v_model_col,
                                          v_model_pch,
                                          v_model_bg,
                                          ylim_for_metric){
  
  ## load key linking scenario code e.g. "2002" to CV within and CV between
  # load parameters used in each scenario (e.g. simulation set 1)
  if(simulation_set_number <= 4){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",simulation_set_number,"_scenarios.csv"))
  } else if(simulation_set_number == 5){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",1,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 5000
  }else if(simulation_set_number == 6){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",4,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 6000
  } else if(simulation_set_number == 7){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",1,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 7000
  } else if(simulation_set_number == 8){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",4,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 8000
  }
  
  # Levels of CV within and between indications to plot
  log_M_01_CV_w <- c(0, 0.07, 0.15, 0.30, 0.50) # CV within options
  log_M_01_CV_bw <- c(0, 0.07, 0.15, 0.30, 0.50) # CV between options
  log_M_01_mu <- -0.511 #  ifelse(effect_size == "small", -0.223, -0.511) # effect size small or large
  # create dataset of all combinations of CV and effect size for plot
  data <- expand.grid(log_M_01_CV_w = log_M_01_CV_w, 
                      log_M_01_CV_bw = log_M_01_CV_bw, 
                      log_M_01_mu = log_M_01_mu) 
  # note this is in the correct order for plotting:
  # on x-axis we want bw0w0, bw0w7, bw0w15,....bw50w50
  
  # for each combination of parameters, find the corresponding row_index in df_simulation_set
  data$row_index <- NA # initialize
  for(s in 1:nrow(data)){
    data$row_index[s] <- which(df_simulation_set$log_M_01_mu == data$log_M_01_mu[s] &
                                 df_simulation_set$log_M_01_CV_w == data$log_M_01_CV_w[s] &
                                 df_simulation_set$log_M_01_CV_bw == data$log_M_01_CV_bw[s])
  }
  # assign the parameter sets associated with these row indexes
  data$Parameter_set <- df_simulation_set$Parameter_set[data$row_index]
  
  
  # set up plot
  plot(1, type="n", axes = FALSE, xlim = c(0, 25), ylim = ylim_for_metric)
  
  # lines to indicate different levels of between indication heterogeneity
  abline(v = 5.5, col = "lightgray")
  abline(v = 5.5 + 5, col = "lightgray")
  abline(v = 5.5 + 10, col = "lightgray")
  abline(v = 5.5 + 15, col = "lightgray")
  
  # add points for each model in v_models for chosen metric
  for(model_i in 1:length(v_models)){
    
    # which model is being plotted?
    model <- v_models[model_i]
    # what is the evidence set it is being evaluated on?
    evidence_set <- paste0(level_of_evidence, ifelse(v_model_OS_in_target[model_i] == TRUE, "_with OS", "_no OS"))
    
    # For the chosen model (e.g. uni_ce), look up the metric (e.g. Bias) value using l_results list
    # for each parameter set in data (e.g. 2001)
    data$model_result <- NA # initialize
    for(s in 1:nrow(data)){
      if(!is.na(data$Parameter_set[s])){
        # point estimate (not taking into account Monte Carlo standard error)
        point_estimate_s <- l_results[[paste0(data$Parameter_set[s], 
                                              "_", evidence_set, ".csv")]][[metric]][model == 
                                                                                       c("uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                                                                         "bi_ce_matched",   "bi_re_matched", 
                                                                                         "bi_ce_unmatched", "bi_re_unmatched",
                                                                                         "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                                                                         "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                                                                         "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share")]
        data$model_result[s] <- ifelse(is.null(point_estimate_s), NA, point_estimate_s)
      }
    }                                                   
    # record the monte carlo standard error
    data$model_result_MCSE <- NA
    for(s in 1:nrow(data)){
      if(!is.na(data$Parameter_set[s])){
        data$model_result_MCSE[s] <- l_results[[paste0(data$Parameter_set[s], 
                                                       "_", evidence_set, ".csv")]][[paste0(metric, "_MCSE")]][model == c(
                                                         "uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                                         "bi_ce_matched",   "bi_re_matched", 
                                                         "bi_ce_unmatched", "bi_re_unmatched",
                                                         "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                                         "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                                         "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share")]
      }
    }
    
    ## add the points to the graph
    # don't add IE models to the splitting SE graph
    skip_plotting <- (model == "uni_ie" | model == "uni_ie_w_share") &
      metric == "splittingModSeR_IE_w_share"
    
    if(skip_plotting == FALSE){
      # point estimate and interval
      plotCI(1:25, data$model_result, ui= data$model_result + 1.96*data$model_result_MCSE, 
             li= data$model_result - 1.96*data$model_result_MCSE,
             add = TRUE,
             , pch = NA, col = v_model_col[model_i])
      points(1:25, data$model_result, pch = v_model_pch[model_i], bg = v_model_bg[model_i], col = v_model_col[model_i])
    }
  }
}


### Spread single tile #########
# 0% , 7%, 15%, ..50% are spread apart to represent distance

## testing
# simulation_set_number <- 1 # no outlier
# v_models <- c("uni_ce", "uni_re") #  vector of models to plot
# level_of_evidence = "early"
# metric <- "Bias"
# v_model_OS_in_target <- c(TRUE, TRUE)
# v_model_col <- c("red", "green")
# v_model_pch <- c(1, 2)
# ylim_for_metric <- c(-0.02, 0.1)

fn_plot_single_multiplot_tile_spread <- function(simulation_set_number, 
                                          metric, 
                                          level_of_evidence, 
                                          v_models,
                                          v_model_OS_in_target,
                                          v_model_col,
                                          v_model_pch,
                                          v_model_bg,
                                          ylim_for_metric){
  
  ## load key linking scenario code e.g. "2002" to CV within and CV between
  # load parameters used in each scenario (e.g. simulation set 1)
  if(simulation_set_number <= 4){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",simulation_set_number,"_scenarios.csv"))
  } else if(simulation_set_number == 5){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",1,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 5000
  }else if(simulation_set_number == 6){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",4,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 6000
  } else if(simulation_set_number == 7){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",1,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 7000
  } else if(simulation_set_number == 8){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",4,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 8000
  }
  
  # Levels of CV within and between indications to plot
  log_M_01_CV_w <- c(0, 0.07, 0.15, 0.30, 0.50) # CV within options
  log_M_01_CV_bw <- c(0, 0.07, 0.15, 0.30, 0.50) # CV between options
  log_M_01_mu <- -0.511 #  ifelse(effect_size == "small", -0.223, -0.511) # effect size small or large
  # create dataset of all combinations of CV and effect size for plot
  data <- expand.grid(log_M_01_CV_w = log_M_01_CV_w, 
                      log_M_01_CV_bw = log_M_01_CV_bw, 
                      log_M_01_mu = log_M_01_mu) 
  # note this is in the correct order for plotting:
  # on x-axis we want bw0w0, bw0w7, bw0w15,....bw50w50
  
  # for each combination of parameters, find the corresponding row_index in df_simulation_set
  data$row_index <- NA # initialize
  for(s in 1:nrow(data)){
    data$row_index[s] <- which(df_simulation_set$log_M_01_mu == data$log_M_01_mu[s] &
                                 df_simulation_set$log_M_01_CV_w == data$log_M_01_CV_w[s] &
                                 df_simulation_set$log_M_01_CV_bw == data$log_M_01_CV_bw[s])
  }
  # assign the parameter sets associated with these row indexes
  data$Parameter_set <- df_simulation_set$Parameter_set[data$row_index]
  
  
  # set up plot
  plot(1, type="n", axes = FALSE, xlim = c(0, 30), ylim = ylim_for_metric)
  
  # new spread out x values for location of within indication heterogeneity
  x_values_within_indications <- (c(0, 0.7, 1.5, 3, 5) + 1) 
  x_values <- c(x_values_within_indications, 
                x_values_within_indications + 6,
                x_values_within_indications + 6*2,
                x_values_within_indications + 6*3,
                x_values_within_indications + 6*4)
  # lines to indicate different levels of between indication heterogeneity
  abline(v = 6 + 0.5, col = "lightgray")
  abline(v = 6*2 + 0.5, col = "lightgray")
  abline(v = 6*3 + 0.5, col = "lightgray")
  abline(v = 6*4 + 0.5, col = "lightgray")
  
  # add points for each model in v_models for chosen metric
  for(model_i in 1:length(v_models)){
    
    # which model is being plotted?
    model <- v_models[model_i]
    # what is the evidence set it is being evaluated on?
    evidence_set <- paste0(level_of_evidence, ifelse(v_model_OS_in_target[model_i] == TRUE, "_with OS", "_no OS"))
    
    # For the chosen model (e.g. uni_ce), look up the metric (e.g. Bias) value using l_results list
    # for each parameter set in data (e.g. 2001)
    data$model_result <- NA # initialize
    for(s in 1:nrow(data)){
      if(!is.na(data$Parameter_set[s])){
        # point estimate (not taking into account Monte Carlo standard error)
        point_estimate_s <- l_results[[paste0(data$Parameter_set[s], 
                                              "_", evidence_set, ".csv")]][[metric]][model == 
                                                                                       c("uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                                                                         "bi_ce_matched",   "bi_re_matched", 
                                                                                         "bi_ce_unmatched", "bi_re_unmatched",
                                                                                         "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                                                                         "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                                                                         "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share")]
        data$model_result[s] <- ifelse(is.null(point_estimate_s), NA, point_estimate_s)
      }
    }                                                   
    # record the monte carlo standard error
    data$model_result_MCSE <- NA
    for(s in 1:nrow(data)){
      if(!is.na(data$Parameter_set[s])){
        data$model_result_MCSE[s] <- l_results[[paste0(data$Parameter_set[s], 
                                                       "_", evidence_set, ".csv")]][[paste0(metric, "_MCSE")]][model == c(
                                                         "uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                                         "bi_ce_matched",   "bi_re_matched", 
                                                         "bi_ce_unmatched", "bi_re_unmatched",
                                                         "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                                         "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                                         "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share")]
      }
    }
    
    ## add the points to the graph
    # don't add IE models to the splitting SE graph
    skip_plotting <- (model == "uni_ie" | model == "uni_ie_w_share") &
      metric == "splittingModSeR_IE_w_share"
    
    if(skip_plotting == FALSE){
      # point estimate and interval
      plotCI(x_values, data$model_result, ui= data$model_result + 1.96*data$model_result_MCSE, 
             li= data$model_result - 1.96*data$model_result_MCSE,
             add = TRUE,
             , pch = NA, col = v_model_col[model_i])
      points(x_values, data$model_result, pch = v_model_pch[model_i], bg = v_model_bg[model_i], col = v_model_col[model_i])
    }
  }
}


### 4x3 multiplot #########
# uses fn_plot_single_multiplot_tile() to plot 4 metrics (rows) and three scenarios (columns)
# e.g. bias, coverage, emp SE and splitting SE for no outlier, moderate outlier and extreme outlier
# example inputs:
# level_of_evidence = "late" # common across scenarios
# v_models = c("uni_ce", "uni_ce_w_share") # common across scenarios
# v_model_OS_in_target = c(TRUE, TRUE) # one for each model models
# v_model_col = c("indianred", "indianred") # one for each model
# v_model_pch = c(1, 2) # one for each model
# v_model_legend_name = c("CE", "CE share") # one for each model
# ylim_for_row1 = c(-0.02, 0.1) # y axis limits for metric 1 (row 1) old = ylim_for_Bias
# ylim_for_row2 = c(0.7, 1) # old = ylim_for_Cover
# ylim_for_row3 = c(0, 0.15)
# ylim_for_row4 = c(0, 1.5)
# v_metric = c("Bias", "Cover", "EmpSE", "splittingModSeR_IE_w_share") # as they appear in l_results
# v_metric_names = c("Bias", "Coverage", "Empirical SE", "Splitting SE ratio") # as you want them to appear in graph
# v_metric_dotted_line = c(0, 0.95, NA, 1) # where the dotted reference line is for each metric
# main_for_col1 = "No outlier"
# main_for_col2 = "One moderate outlier"
# main_for_col3 = "One extreme outlier"
# simulation_set_number_col1 = 1 # no outlier
# simulation_set_number_col2 = 2 # moderate outlier
# simulation_set_number_col3 = 3 # extreme outlier

fn_plot_4x3_multiplot <- function(level_of_evidence, 
                              v_models,  
                              v_model_OS_in_target,
                              v_model_col, 
                              v_model_pch ,
                              v_model_bg ,
                              v_model_legend_name, 
                              v_metric,
                              v_metric_names,
                              v_metric_dotted_line,
                              ylim_for_row1, 
                              ylim_for_row2, 
                              ylim_for_row3,
                              ylim_for_row4,
                              main_for_col1,  
                              main_for_col2,
                              main_for_col3,
                              simulation_set_number_col1,
                              simulation_set_number_col2,
                              simulation_set_number_col3
){
  ## set up plot
  plot.new()
  par(mfcol = c(4, 3), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 2: Row/metric 2, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 3: Row/metric 3, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 4: Row/metric 4, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add y-axis
  axis(2L)
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.80, las = 2)
  
  ## Plot 5: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)

  ## Plot 6: Row/metric 2, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 7: Row/metric 3, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 8: Row/metric 4, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.80, las = 2)
  
  ## Plot 9: Row/metric 1, column/scenario 3
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col3, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 10: Row/metric 2, column/scenario 3
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col3, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 11: Row/metric 3, column/scenario 3
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col3,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 12: Row/metric 4, column/scenario 3
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col3, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.80, las = 2)
  
  
  ### Column text
  # runs from 0 (left) to 1 (right), split into 6 segments of 1/6 each and place labels evenly
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 1/6), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 3/6), line = 2.5)
  mtext(text = main_for_col3, outer = TRUE, at = c(3, 5/6), line = 2.5)
  
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[2], side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[3], side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[4], side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.5), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.5), line = 1.5, cex = 0.7, side = 1)

  ### legend
  add_legend(x = 0.87, y = 0.5,
             cex = 1.2,
             legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg, 
             bty='n', y.intersp=2.5)

}


### Spread 4x3 multiplot #########

fn_plot_4x3_multiplot_spread <- function(level_of_evidence, 
                                  v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg ,
                                  v_model_legend_name, 
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  ylim_for_row4,
                                  main_for_col1,  
                                  main_for_col2,
                                  main_for_col3,
                                  simulation_set_number_col1,
                                  simulation_set_number_col2,
                                  simulation_set_number_col3
){
  ## set up plot
  plot.new()
  par(mfcol = c(4, 3), 
      #mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      mar = c(1.5, 1, 1, 1), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  # new spread out x values for location of within indication heterogeneity
  x_values_within_indications <- (c(0, 0.7, 1.5, 3, 5) + 1) 
  x_values <- c(x_values_within_indications, 
                x_values_within_indications + 6,
                x_values_within_indications + 6*2,
                x_values_within_indications + 6*3,
                x_values_within_indications + 6*4)
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 2: Row/metric 2, column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 3: Row/metric 3, column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 4: Row/metric 4, column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add y-axis
  axis(2L)
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 5: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 6: Row/metric 2, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 7: Row/metric 3, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 8: Row/metric 4, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 9: Row/metric 1, column/scenario 3
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col3, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 10: Row/metric 2, column/scenario 3
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col3, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 11: Row/metric 3, column/scenario 3
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col3,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 12: Row/metric 4, column/scenario 3
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col3, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  
  ### Column text
  # runs from 0 (left) to 1 (right), split into 6 segments of 1/6 each and place labels evenly
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 1/6), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 3/6), line = 2.5)
  mtext(text = main_for_col3, outer = TRUE, at = c(3, 5/6), line = 2.5)
  
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[2], side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[3], side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[4], side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.5), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.5), line = 1.5, cex = 0.7, side = 1)
  
  ### legend
  add_legend(x = 0.75, y = 0.5,
             cex = 1.2,
             legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg, 
             bty='n', y.intersp=2.5)
}


### Time point 4x3 multiplot #########
# plots one simulation set for the three time points (early, mid late) for all 4 metrics
fn_plot_4x3_multiplot_time_points <- function(v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg ,
                                  v_model_legend_name, 
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  ylim_for_row4,
                                  main_for_col1,  
                                  main_for_col2,
                                  main_for_col3,
                                  simulation_set_number
){
  ## set up plot
  plot.new()
  par(mfcol = c(4, 3), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  ## Plot 1: Row/metric 1, Column/scenario "early"
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = "early", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 2: Row/metric 2, column/scenario "early"
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = "early", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 3: Row/metric 3, column/scenario "early"
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = "early", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 4: Row/metric 4, column/scenario "early"
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = "early", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add y-axis
  axis(2L)
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.80, las = 2)
  
  ## Plot 5: Row/metric 1, column/scenario "mid"
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = "mid", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 6: Row/metric 2, column/scenario mid
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = "mid", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 7: Row/metric 3, column/scenario mid
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = "mid", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 8: Row/metric 4, column/scenario mid
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = "mid", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.80, las = 2)
  
  ## Plot 9: Row/metric 1, column/scenario late
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = "late", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 10: Row/metric 2, column/scenario late
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = "late", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 11: Row/metric 3, column/scenario late
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = "late", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 12: Row/metric 4, column/scenario late
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = "late", 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.80, las = 2)
  
  
  ### Column text
  # runs from 0 (left) to 1 (right), split into 6 segments of 1/6 each and place labels evenly
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 1/6), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 3/6), line = 2.5)
  mtext(text = main_for_col3, outer = TRUE, at = c(3, 5/6), line = 2.5)
  
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[2], side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[3], side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[4], side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.5), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.5), line = 1.5, cex = 0.7, side = 1)
  
  ### legend
  add_legend(x = 0.87, y = 0.5,
             cex = 1.2,
             legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg, 
             bty='n', y.intersp=2.5)
  
}


### 4x2 multiplot #########
# uses fn_plot_single_multiplot_tile() to plot 4 metrics (rows) and two scenarios (columns)
# e.g. bias, coverage, emp SE and splitting SE for no outlier and one outlier
# example inputs:
# level_of_evidence = "late" # common across scenarios
# v_models = c("uni_ce", "uni_ce_w_share") # common across scenarios
# v_model_OS_in_target = c(TRUE, TRUE) # one for each model models
# v_model_col = c("indianred", "indianred") # one for each model
# v_model_pch = c(1, 2) # one for each model
# v_model_legend_name = c("CE", "CE share") # one for each model
# ylim_for_row1 = c(-0.02, 0.1) # y axis limits for metric 1 (row 1) old = ylim_for_Bias
# ylim_for_row2 = c(0.7, 1) # old = ylim_for_Cover
# ylim_for_row3 = c(0, 0.15)
# ylim_for_row4 = c(0, 1.5)
# v_metric = c("Bias", "Cover", "EmpSE", "splittingModSeR_IE_w_share") # as they appear in l_results
# v_metric_names = c("Bias", "Coverage", "Empirical SE", "Splitting SE ratio") # as you want them to appear in graph
# v_metric_dotted_line = c(0, 0.95, NA, 1) # where the dotted reference line is for each metric
# main_for_col1 = "No outlier"
# main_for_col2 = "One outlier"
# simulation_set_number_col1 = 1 # no outlier
# simulation_set_number_col2 = 3 # extreme outlier

fn_plot_4x2_multiplot <- function(level_of_evidence, 
                                  v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg,
                                  v_model_legend_name, 
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  ylim_for_row4,
                                  main_for_col1,  
                                  main_for_col2,
                                  simulation_set_number_col1,
                                  simulation_set_number_col2
){
  ## set up plot
  plot.new()
  par(mfcol = c(4, 2), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 2: Row/metric 2, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 3: Row/metric 3, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 4: Row/metric 4, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add y-axis
  axis(2L)
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 5: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 6: Row/metric 2, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 7: Row/metric 3, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  ## Plot 8: Row/metric 4, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[4], # row 4 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row4)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[4], lty = 3, col = "gray2", lwd = 1) # row 4 metric
  box()
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ### Column text
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 0.25), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 0.75), line = 2.5)
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[2], side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[3], side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[4], side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.25), line = 1.5, cex = 0.7, side = 1)
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.75), line = 1.5, cex = 0.7, side = 1)
  
  ### legend
  add_legend(x = 0.84, y = 0.5,
             legend=v_model_legend_name, 
             pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg,
             cex = 1.2,
             bty='n', y.intersp=2.5)
  
  # note which evidence set
  # *change this
  # mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
  
}


### 3x2 multiplot #########

fn_plot_3x2_multiplot <- function(level_of_evidence, 
                                  v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg,
                                  v_model_legend_name, 
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  main_for_col1,  
                                  main_for_col2,
                                  simulation_set_number_col1,
                                  simulation_set_number_col2
){
  ## set up plot
  plot.new()
  par(mfcol = c(3, 2), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 2: Row/metric 2, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 3: Row/metric 3, column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  # add y-axis
  axis(2L)
  
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 4: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 5: Row/metric 2, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 6: Row/metric 3, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ### Column text
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 0.25), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 0.75), line = 2.5)
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 6 segments of 1/6 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, (1/6)*5 ), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[2], side = 2, outer = TRUE, at = c(3, (1/6)*3 ), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[3], side = 2, outer = TRUE, at = c(3, (1/6)*1 ), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.25), line = 1.5, cex = 0.7, side = 1)
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.75), line = 1.5, cex = 0.7, side = 1)
  
  ### legend
  add_legend(x = 0.8, y = 0.5,
             legend=v_model_legend_name, 
             pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg,
             cex = 1.2,
             bty='n', y.intersp=2.5)
  
  # note which evidence set
  # *change this
  # mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
  
}


### Spread 3x2 multiplot #########
level_of_evidence = "late" # common across scenarios
v_models = c("uni_ce", "uni_ce_w_share") # common across scenarios
v_model_OS_in_target = c(TRUE, TRUE) # one for each model models
v_model_col = c("indianred", "indianred") # one for each model
v_model_pch = c(1, 2) # one for each model
v_model_legend_name = c("CE", "CE share") # one for each model
ylim_for_row1 = c(-0.02, 0.1) # y axis limits for metric 1 (row 1) old = ylim_for_Bias
ylim_for_row2 = c(0.7, 1) # old = ylim_for_Cover
ylim_for_row3 = c(0, 1.5)
v_metric = c("Bias", "Cover",  "splittingModSeR_IE_w_share") # as they appear in l_results
v_metric_names = c("Bias", "Coverage",  "Splitting SE ratio") # as you want them to appear in graph
v_metric_dotted_line = c(0, 0.95, 1) # where the dotted reference line is for each metric
main_for_col1 = "No outlier"
main_for_col2 = "One moderate outlier"
simulation_set_number_col1 = 1 # no outlier
simulation_set_number_col2 = 2 # moderate outlier

fn_plot_3x2_multiplot_spread <- function(level_of_evidence, 
                                  v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg,
                                  v_model_legend_name, 
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  main_for_col1,  
                                  main_for_col2,
                                  simulation_set_number_col1,
                                  simulation_set_number_col2
){
  ## set up plot
  plot.new()
  par(mfcol = c(3, 2), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  # new spread out x values for location of within indication heterogeneity
  x_values_within_indications <- (c(0, 0.7, 1.5, 3, 5) + 1) 
  x_values <- c(x_values_within_indications, 
                x_values_within_indications + 6,
                x_values_within_indications + 6*2,
                x_values_within_indications + 6*3,
                x_values_within_indications + 6*4)
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 2: Row/metric 2, column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  # add y-axis
  axis(2L)
  
  ## Plot 3: Row/metric 3, column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1, 
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  # add y-axis
  axis(2L)
  
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 4: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  ## Plot 5: Row/metric 2, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[2], # row 2 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row2)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[2], lty = 3, col = "gray2", lwd = 1) # row 2 metric
  box()
  
  ## Plot 6: Row/metric 3, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2,  
                                metric = v_metric[3], # row 3 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_pch = v_model_pch,
                                v_model_bg = v_model_bg,
                                ylim_for_metric = ylim_for_row3)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[3], lty = 3, col = "gray2", lwd = 1) # row 3 metric
  box()
  
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ### Column text
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 0.25), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 0.75), line = 2.5)
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 6 segments of 1/6 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, (1/6)*5 ), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[2], side = 2, outer = TRUE, at = c(3, (1/6)*3 ), line = 2.2, cex = 0.8)
  mtext(text = v_metric_names[3], side = 2, outer = TRUE, at = c(3, (1/6)*1 ), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.25), line = 1.5, cex = 0.7, side = 1)
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.75), line = 1.5, cex = 0.7, side = 1)
  
  ### legend
  add_legend(x = 0.85, y = 0.5,
             legend=v_model_legend_name, 
             pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg,
             cex = 1.2,
             bty='n', y.intersp=2.5)
}


### 1x2 multiplot #########
# uses fn_plot_single_multiplot_tile() to plot 1 metrics (row) and two scenarios (columns)
# e.g. bias for no outlier and one outlier
# example inputs:
# level_of_evidence = "late" # common across scenarios
# v_models = c("uni_ce", "uni_ce_w_share") # common across scenarios
# v_model_OS_in_target = c(TRUE, TRUE) # one for each model models
# v_model_col = c("indianred", "indianred") # one for each model
# v_model_pch = c(1, 2) # one for each model
# v_model_legend_name = c("CE", "CE share") # one for each model
# ylim_for_row1 = c(-0.02, 0.1) # y axis limits for metric 1 (row 1) old = ylim_for_Bias
# v_metric = c("Bias") # as they appear in l_results
# v_metric_names = c("Bias") # as you want them to appear in graph
# v_metric_dotted_line = c(0) # where the dotted reference line is for the metric
# main_for_col1 = "No outlier"
# main_for_col2 = "One outlier"
# simulation_set_number_col1 = 1 # no outlier
# simulation_set_number_col2 = 3 # extreme outlier

fn_plot_1x2_multiplot <- function(level_of_evidence, 
                                  v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg,
                                  v_model_legend_name,
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  ylim_for_row4,
                                  main_for_col1,  
                                  main_for_col2,
                                  simulation_set_number_col1,
                                  simulation_set_number_col2
){
  ## set up plot
  plot.new()
  par(mfcol = c(1, 2), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_bg = v_model_bg,
                                v_model_pch = v_model_pch,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 2: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_bg = v_model_bg,
                                v_model_pch = v_model_pch,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)

  # add x-axis: within indication heterogeneity
  axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ### Column text
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 0.25), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 0.75), line = 2.5)
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 2 segments of 0.5 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, 0.5), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.5), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.5), line = 1.5, cex = 0.7, side = 1)

  ### legend
  add_legend(x = 0.74, y = 0.5,
             legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg,
             bty='n', y.intersp=2.5, cex = 0.9)
  
  # note which evidence set
  # *change this
  # mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
  
}


### Spread 1x2 multiplot #########
# uses fn_plot_single_multiplot_tile() to plot 1 metrics (row) and two scenarios (columns)
# e.g. bias for no outlier and one outlier
# example inputs:
# level_of_evidence = "late" # common across scenarios
# v_models = c("uni_ce", "uni_ce_w_share") # common across scenarios
# v_model_OS_in_target = c(TRUE, TRUE) # one for each model models
# v_model_col = c("indianred", "indianred") # one for each model
# v_model_pch = c(1, 2) # one for each model
# v_model_legend_name = c("CE", "CE share") # one for each model
# ylim_for_row1 = c(-0.02, 0.1) # y axis limits for metric 1 (row 1) old = ylim_for_Bias
# v_metric = c("Bias") # as they appear in l_results
# v_metric_names = c("Bias") # as you want them to appear in graph
# v_metric_dotted_line = c(0) # where the dotted reference line is for the metric
# main_for_col1 = "No outlier"
# main_for_col2 = "One outlier"
# simulation_set_number_col1 = 1 # no outlier
# simulation_set_number_col2 = 3 # extreme outlier

fn_plot_1x2_multiplot_spread <- function(level_of_evidence, 
                                  v_models,  
                                  v_model_OS_in_target,
                                  v_model_col, 
                                  v_model_pch ,
                                  v_model_bg,
                                  v_model_legend_name,
                                  v_metric,
                                  v_metric_names,
                                  v_metric_dotted_line,
                                  ylim_for_row1, 
                                  ylim_for_row2, 
                                  ylim_for_row3,
                                  ylim_for_row4,
                                  main_for_col1,  
                                  main_for_col2,
                                  simulation_set_number_col1,
                                  simulation_set_number_col2
){
  ## set up plot
  plot.new()
  par(mfcol = c(1, 2), 
      mar = c(0.5, 0, 0, 0), # only include small space between the three rows
      oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
      mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
  
  ## Plot 1: Row/metric 1, Column/scenario 1
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col1,  
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_bg = v_model_bg,
                                v_model_pch = v_model_pch,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevant reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # add y-axis
  axis(2L)
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  # new spread out x values for location of within indication heterogeneity
  x_values_within_indications <- (c(0, 0.7, 1.5, 3, 5) + 1) 
  x_values <- c(x_values_within_indications, 
                x_values_within_indications + 6,
                x_values_within_indications + 6*2,
                x_values_within_indications + 6*3,
                x_values_within_indications + 6*4)
  
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ## Plot 2: Row/metric 1, column/scenario 2
  fn_plot_single_multiplot_tile_spread(simulation_set_number = simulation_set_number_col2, 
                                metric = v_metric[1], # row 1 metric
                                level_of_evidence = level_of_evidence, 
                                v_models = v_models,
                                v_model_OS_in_target = v_model_OS_in_target,
                                v_model_col = v_model_col,
                                v_model_bg = v_model_bg,
                                v_model_pch = v_model_pch,
                                ylim_for_metric = ylim_for_row1)
  # add gray dotted line at relevenat reference point
  abline(h = v_metric_dotted_line[1], lty = 3, col = "gray2", lwd = 1) # row 1 metric
  box()
  # between indication hetero labels
  axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
       at = c(3, 3 + 6, 3 + 6*2, 3 + 6*3, 3 + 6*4),  # between indication heterogeneity
       tick = FALSE, cex.axis = 0.75,
       line = -0.5)
  
  # add x-axis: within indication heterogeneity
  axis(1, at = x_values, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
       cex.axis = 0.60, las = 2)
  
  ### Column text
  mtext(text = main_for_col1, outer = TRUE, at = c(3, 0.25), line = 2.5)
  mtext(text = main_for_col2, outer = TRUE, at = c(3, 0.75), line = 2.5)
  ### Row text
  # left hand side runs from 0 (bottom) to 1 (top), split into 2 segments of 0.5 each and place labels evenly
  mtext(text = v_metric_names[1], side = 2, outer = TRUE, at = c(3, 0.5), line = 2.2, cex = 0.8)
  ### Between indication heterogneity labels
  mtext(text = "Between indication heterogeneity in M (CV)", outer = TRUE, at = c(3, 0.5), line = 1, cex = 0.7)
  ### Within indication heterogneity labels
  mtext(text = "Within indication heterogeneity in M (CV)", outer = TRUE, at = c(0.5), line = 1.5, cex = 0.7, side = 1)
  
  ### legend
  add_legend(x = 0.8, y = 0.5,
             legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, pt.bg = v_model_bg,
             bty='n', y.intersp=2.5, cex = 0.9)
}





## (Single) Plot performance score for a specific model, metric, time point effect size ########
# used in loop defined below
# requires l_results to be loaded (see ResultsForPaper.R)

# simulation_set_number <- 2
# model <- "uni_ce"
# evidence_set <- "early_no OS"
# metric <- "Bias"
# effect_size <- "small"
# decimal_places <- 3
# CI_interval <- FALSE
# google_drive_path <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/"

fn_CV_result_plot <- function(simulation_set_number,
                              model,
                              evidence_set ,
                              metric,
                              effect_size,
                              decimal_places,
                              CI_interval = FALSE,
                              google_drive_path){
  # turn off scientific notation
  options(scipen=999)
  
  ## load key linking scenario code e.g. "2002" to CV within and CV between
  # load parameters used in each scenario (e.g. simulation set 1)
  if(simulation_set_number <= 4){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",simulation_set_number,"_scenarios.csv"))
  } else if(simulation_set_number == 5){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",1,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 5000
  }else if(simulation_set_number == 6){
    df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",4,"_scenarios.csv"))
    df_simulation_set$Parameter_set <- 1:50 + 6000
  }
  
  # Levels of CV within and between indications to plot
  log_M_01_CV_w <- c(0, 0.07, 0.15, 0.30, 0.50) # CV within options
  log_M_01_CV_bw <- c(0, 0.07, 0.15, 0.30, 0.50) # CV between options
  log_M_01_mu <- -0.511 #  ifelse(effect_size == "small", -0.223, -0.511) # effect size small or large
  # create dataset of all combinations of CV and effect size for plot
  data <- expand.grid(log_M_01_CV_w = log_M_01_CV_w, 
                      log_M_01_CV_bw = log_M_01_CV_bw, 
                      log_M_01_mu = log_M_01_mu) 
  
  # for each combination of parameters, find the corresponding row_index in df_simulation_set
  data$row_index <- NA # initialize
  for(s in 1:nrow(data)){
    data$row_index[s] <- which(df_simulation_set$log_M_01_mu == data$log_M_01_mu[s] &
                                 df_simulation_set$log_M_01_CV_w == data$log_M_01_CV_w[s] &
                                 df_simulation_set$log_M_01_CV_bw == data$log_M_01_CV_bw[s])
  }
  # assign the parameter sets associated with these row indexes
  data$Parameter_set <- df_simulation_set$Parameter_set[data$row_index]
  
  # For the chosen model (e.g. uni_ce), look up the metric (e.g. Bias) value using l_results list
  # for each parameter set in data (e.g. 2001)
  data$model_result <- NA # initialize
  for(s in 1:nrow(data)){
    if(!is.na(data$Parameter_set[s])){
      # point estimate (not taking into account Monte Carlo standard error)
      point_estimate_s <- l_results[[paste0(data$Parameter_set[s], 
                                                "_", evidence_set, ".csv")]][[metric]][model == 
                                                                                         c("uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                                                                           "bi_ce_matched",   "bi_re_matched", 
                                                                                           "bi_ce_unmatched", "bi_re_unmatched",
                                                                                           "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                                                                           "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                                                                           "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share")]
    data$model_result[s] <- ifelse(is.null(point_estimate_s), NA, point_estimate_s)
    }
  }                                                   
  
  # round reults
  data$model_result_round <- round(data$model_result, decimal_places)
  
  # plot
  plot(1, type="n", xaxt = "n", yaxt = "n", xlab="CV within", ylab="CV between", 
       xlim=c(-0.05, 0.55), ylim=c(-0.05, 0.55),
       main = paste(metric, "result with", model , "\n For", evidence_set, "and", effect_size, "effect size")
  )
  axis(1, at = log_M_01_CV_w , las=2, labels = paste0(round(log_M_01_CV_w*100, 0), "%"))
  axis(2, at = log_M_01_CV_bw , las=2, labels = paste0(round(log_M_01_CV_w*100, 0), "%"))
  # plot model results at each combination of CV within and between
  if(CI_interval == FALSE){
    # just plot point estimates
    for(s in 1:nrow(data)){
      text(data$log_M_01_CV_w[s], 
           data$log_M_01_CV_bw[s], 
           data$model_result_round[s], cex = 1.1) 
    }
  } else {
    # calculate MCSE and plot
    data$model_result_MCSE <- NA
    for(s in 1:nrow(data)){
      if(!is.na(data$Parameter_set[s])){
        data$model_result_MCSE[s] <- l_results[[paste0(data$Parameter_set[s], 
                                                       "_", evidence_set, ".csv")]][[paste0(metric, "_MCSE")]][model == c(
                                                         "uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                                         "bi_ce_matched",   "bi_re_matched", 
                                                         "bi_ce_unmatched", "bi_re_unmatched",
                                                         "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                                         "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                                         "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share")]
      }
    }
    # plot point estimates and interval
    for(s in 1:nrow(data)){
      if(!is.na(data$model_result_round[s])){
        text(data$log_M_01_CV_w[s], 
             data$log_M_01_CV_bw[s], 
             paste0(data$model_result_round[s], 
                    "(",round(data$model_result[s] - 1.96*100*data$model_result_MCSE[s], decimal_places),
                    ",", round(data$model_result[s] + 1.96*100*data$model_result_MCSE[s], decimal_places),
                    ")"), 
             cex = 0.7) 
      }
    }
  }
}

# fn_CV_result_plot(simulation_set_number = 2,
#                   model = "uni_ce",
#                   evidence_set = "early_no OS",
#                   metric = "Bias",
#                   effect_size = "small",
#                   decimal_places = 2,
#                   CI_interval = TRUE,
#                   google_drive_path = "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/")

## (Loop) Plot performance score for each model, time point and effect size ########
# plots the value of a metric for all the relevant combinations 
# and saves the result


# simulation_set_number = 2
# metric = "Bias"
# decimal_places = 3 
# CI_interval = FALSE
# figures_location = paste0("C:/MultiIndication_results/Figures/Model_values/",
#                           "Set2")
# google_drive_path = "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/"

fn_CV_result_loop_plot <- function(simulation_set_number,
                                   metric, 
                                   decimal_places, 
                                   CI_interval = FALSE,
                                   figures_location,
                                   google_drive_path){
  
  # create combinations of evidence sets, effect sizes and models for plotting
  plot_combinations_0 <- expand.grid(evidence_set = c("early_no OS", "mid_no OS", "late_no OS",
                                                      "early_with OS", "mid_with OS", "late_with OS"),
                                     effect_size = "large", #  c("small", "large"),
                                     model = c("uni_ie", "uni_ce", "uni_mcie",  "uni_re",  "uni_mrie",
                                       "bi_ce_matched",   "bi_re_matched", 
                                       "bi_ce_unmatched", "bi_re_unmatched",
                                       "uni_ie_w_share", "uni_ce_w_share", "uni_mcie_w_share",  "uni_re_w_share",  "uni_mrie_w_share",
                                       "bi_ce_matched_w_share",   "bi_re_matched_w_share", 
                                       "bi_ce_unmatched_w_share", "bi_re_unmatched_w_share"))

  # cannot have uni_ie models with no OS, remove
  remove_rows1 <- grepl("no OS", plot_combinations_0$evidence_set, fixed = TRUE) & 
    plot_combinations_0$model == "uni_ie"
  # only calculate "no OS" in set 5 and 6
  remove_rows2 <- simulation_set_number >= 5 & 
    grepl("with OS", plot_combinations_0$evidence_set, fixed = TRUE)
  # cannot have PED/splittingser with no OS
  remove_rows3 <- grepl("no OS", plot_combinations_0$evidence_set, fixed = TRUE) & 
    (metric == "splittingModSeR_IE" | 
       metric == "PED_IE" | 
       metric == "splittingModSeR_IE_w_share" | 
       metric == "PED_IE_w_share" | 
       metric == "splittingModSeR_obs")
  # mixture models not fit in sets 4 and 5
  remove_rows4 <- simulation_set_number >= 5 & 
    (plot_combinations_0$model == "uni_mcie" | 
       plot_combinations_0$model ==  "uni_mrie" |
       plot_combinations_0$model == "uni_mcie_w_share" | 
       plot_combinations_0$model == "uni_mrie_w_share")
  
  # remove the rows
  remove_rows <- remove_rows1 | remove_rows2 | remove_rows3
  plot_combinations_1 <- plot_combinations_0[!remove_rows,]
  
  # in some cases there will be nothing to plot, so skip plotting in this case
  if(nrow(plot_combinations_1) > 0){
    # early, mid and late always go together
    # => create plotes in groups of three
    number_of_plots <- nrow(plot_combinations_1)/3
    plot_combinations_1$plot_group <- rep(1:number_of_plots, each = 3)
    
    # loop over plot groups
    for(c in 1:number_of_plots){
      # set up ploting 
      width <- 1180
      height <- 220
      # first row in plot_combinations_1 which corresponds to c
      # contains all the information required
      plot_group_row <- which(plot_combinations_1$plot_group == c)[1]
      png(paste0(figures_location , # save location 
                 plot_combinations_1$model[plot_group_row], # ,model name
                 "_", metric, # metric
                 "_Set_", simulation_set_number, "_", # set number
                 plot_combinations_1$effect_size[plot_group_row], "_effect_", #effect size
                 ifelse(grepl("no OS", plot_combinations_1$evidence_set[plot_group_row], fixed = TRUE), "no_OS", "with_OS"),
                 ".png"),
          width = width, height = height)
      
      layout(mat=matrix(c(1,2,3),nrow=1,ncol=3,byrow=T))
      
      # create the three plots: early mid late
      # early is in plot_group_row + 0
      fn_CV_result_plot(simulation_set_number = simulation_set_number,
                        model = plot_combinations_1$model[plot_group_row + 0],
                        evidence_set = plot_combinations_1$evidence_set[plot_group_row + 0],
                        metric = metric,
                        effect_size = plot_combinations_1$effect_size[plot_group_row + 0],
                        decimal_places = decimal_places,
                        CI_interval = CI_interval,
                        google_drive_path = google_drive_path)
      # mid is in plot_group_row + 1
      fn_CV_result_plot(simulation_set_number = simulation_set_number,
                        model = plot_combinations_1$model[plot_group_row + 1],
                        evidence_set = plot_combinations_1$evidence_set[plot_group_row + 1],
                        metric = metric,
                        effect_size = plot_combinations_1$effect_size[plot_group_row + 1],
                        decimal_places = decimal_places,
                        CI_interval = CI_interval,
                        google_drive_path = google_drive_path)
      # late is in plot_group_row + 2
      fn_CV_result_plot(simulation_set_number = simulation_set_number,
                        model = plot_combinations_1$model[plot_group_row + 2],
                        evidence_set = plot_combinations_1$evidence_set[plot_group_row + 2],
                        metric = metric,
                        effect_size = plot_combinations_1$effect_size[plot_group_row + 2],
                        decimal_places = decimal_places,
                        CI_interval = CI_interval,
                        google_drive_path = google_drive_path)
      dev.off()
    }
  }
}

# fn_CV_result_loop_plot(simulation_set_number = 2,
#                        metric = "Bias", 
#                        decimal_places = 3, 
#                        CI_interval = FALSE,
#                        figures_location = paste0("C:/MultiIndication_results/Figures/Model_values/",
#                                                  "Set2/"),
#                        google_drive_path = "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/")





  
  
  



  
  






## Old functions ###############

# ### OLD multiplot: bias, coverage, splitting SE
# # uses fn_plot_single_multiplot_tile() to calculate:
# # bias, coverage and splitting SE for no outlier and one outlier
# # inputs:
# # level_of_evidence = "early"
# # v_models = vector of models to plot
# # v_model_col = vector of colours for each of the models in v_models (keep ordering)
# # v_model_legend_name = vector of names which will be displayed for each of the models in v_models (keep ordering)
# # ylim_for_Bias, ylim_for_Cover, ylim_for_splittingModSeR_IE_w_share = limits for the yaxis for each of these metrics
# 
# # level_of_evidence = "late"
# # v_models = c("uni_ce", "uni_ce_w_share")
# # v_model_OS_in_target = c(TRUE, TRUE)
# # v_model_col = c("indianred", "indianred")
# # v_model_pch = c(1, 2)
# # v_model_legend_name = c("CE", "CE share") 
# # ylim_for_Bias = c(-0.02, 0.1)
# # ylim_for_Cover = c(0.7, 1)
# # ylim_for_EmpSE = c(0, 0.15)
# # ylim_for_splittingModSeR_IE_w_share = c(0, 1.5)
# 
# fn_plot_multiplot <- function(level_of_evidence, 
#                               v_models,  
#                               v_model_OS_in_target,
#                               v_model_col, 
#                               v_model_pch ,
#                               v_model_legend_name, 
#                               ylim_for_Bias, 
#                               ylim_for_Cover, 
#                               ylim_for_EmpSE,
#                               ylim_for_splittingModSeR_IE_w_share
# ){
#   ## set up plot
#   plot.new()
#   par(mfcol = c(4, 2), 
#       mar = c(0.5, 0, 0, 0), # only include small space between the three rows
#       oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
#       mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
#   ## Plot 1: Bias no outlier 
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   box()
#   ## Plot 2: Coverage no outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "Cover", # row 2 is coverage 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Cover)
#   # add gray dotted line at 0.95 coverage
#   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 3: EmpSE no outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "EmpSE", # row 3 is empse 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_EmpSE)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 4: SOS no outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "splittingModSeR_IE_w_share", # row 4 is strength of sharing 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
#        cex.axis = 0.60, las = 2)
#   
#   # add gray dotted line at 1 splitting SE
#   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 5: Bias one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   box()
#   ### Plot 6: Coverage one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "Cover", # row 2 is coverage 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Cover)
#   # add gray dotted line at 0.95 coverage
#   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
#   box()
#   ### Plot 7: empse one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "EmpSE", # row 3 is empse 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_EmpSE)
#   box()
#   ### Plot 8: SOS one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "splittingModSeR_IE_w_share", # row 4 is strength of sharing 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, tick = FALSE, labels = paste0(rep(c(0, 7, 15, 30, 50), 5), "%"),
#        cex.axis = 0.60, las = 2)
#   
#   # add gray dotted line at 1 splitting SE
#   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
#   box()
#   
#   ### Column text
#   mtext(text = "No outlier", outer = TRUE, at = c(3, 0.25), line = 2.5)
#   mtext(text = "One outlier", outer = TRUE, at = c(3, 0.75), line = 2.5)
#   ### Row text
#   # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
#   mtext(text = "Bias", side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
#   mtext(text = "Coverage", side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
#   mtext(text = "Empirical SE", side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
#   mtext(text = "Splitting SE", side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
#   ### Between indication heterogneity labels
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
#   ### Within indication heterogneity labels
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.25), line = 1, cex = 0.7, side = 1)
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.75), line = 1, cex = 0.7, side = 1)
#   
#   ### legend
#   add_legend("right", legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, 
#              bty='n')
#   
#   # note which evidence set
#   # *change this
#   mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
#   
# }
# 
# 
# 
# 
# ### mixture multiplot: bias for different outliers
# # uses fn_plot_single_multiplot_tile() to calculate:
# # bias for no outlier, mid outlier and extreme outlier
# # same inputs as fn_plot_multiplot
# 
# level_of_evidence = "early"
# v_models = c("uni_ie_w_share",
#              "uni_ce",
#              "uni_mcie_w_share",
#              "uni_re",
#              "uni_mrie_w_share")
# v_model_OS_in_target = rep(TRUE, 5)     
# v_model_col = c(
#   "black", "indianred","darkred",
#   "springgreen3","darkgreen")
# v_model_pch = rep(19, 5)
# v_model_legend_name = c("IE common",
#                         "CE common",
#                         "MCIE common",
#                         "MRIE common")
# ylim_for_Bias = c(-0.02, 0.12)
# ylim_for_Cover = c(0.3, 1)
# ylim_for_EmpSE = c(0, 0.15)
# ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# 
# 
# 
# fn_plot_multiplot_mixture <- function(level_of_evidence, 
#                                       v_models,  
#                                       v_model_OS_in_target,
#                                       v_model_col, 
#                                       v_model_pch ,
#                                       v_model_legend_name, 
#                                       ylim_for_Bias, 
#                                       ylim_for_Cover, 
#                                       ylim_for_EmpSE,
#                                       ylim_for_splittingModSeR_IE_w_share
# ){
#   ## set up plot
#   plot.new()
#   par(mfcol = c(1, 3), 
#       mar = c(0.5, 0, 0, 0), # only include small space between the three rows
#       oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
#       mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
#   ## Plot 1: Bias no outlier 
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   box()
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   
#   ## Plot 2: Bias moderate outlier 
#   fn_plot_single_multiplot_tile(simulation_set_number = 2, # moderate outlier 
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   box()
#   
#   ## Plot 3: Bias extreme outlier 
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # moderate outlier 
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   box()
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   
#   ### Column text
#   mtext(text = "No outlier", outer = TRUE, at = c(3, (1/6)*1 ), line = 2.5)
#   mtext(text = "Moderate outlier", outer = TRUE, at = c(3, (1/6)*3), line = 2.5)
#   mtext(text = "Extreme outlier", outer = TRUE, at = c(3, (1/6)*5), line = 2.5)
#   
#   ### Row text
#   # left hand side runs from 0 (bottom) to 1 (top)
#   mtext(text = "Bias", side = 2, outer = TRUE, at = c(3, 0.5), line = 2.2, cex = 0.8)
#   ### Between indication heterogneity labels
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.5), line = 1, cex = 0.7)
#   ### Within indication heterogneity labels
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.5), line = 1, cex = 0.7, side = 1)
#   
#   ### legend
#   add_legend("right", legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, 
#              bty='n')
#   
#   # note which evidence set
#   # *change this
#   mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
#   
# }
# 
# 
# 
# 
# # ## old multiplot with only bias, coverage and splitting SE
# # fn_plot_multiplot <- function(level_of_evidence, 
# #                               v_models,  
# #                               v_model_OS_in_target,
# #                               v_model_col, 
# #                               v_model_pch ,
# #                               v_model_legend_name, 
# #                               ylim_for_Bias, ylim_for_Cover, ylim_for_splittingModSeR_IE_w_share
# # ){
# #   ## set up plot
# #   plot.new()
# #   par(mfcol = c(3, 2), 
# #       mar = c(0.5, 0, 0, 0), # only include small space between the three rows
# #       oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
# #       mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
# #   ## Plot 1: Bias no outlier 
# #   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
# #                                 metric = "Bias", # row 1 is bias 
# #                                 level_of_evidence = level_of_evidence, 
# #                                 v_models = v_models,
# #                                 v_model_OS_in_target = v_model_OS_in_target,
# #                                 v_model_col = v_model_col,
# #                                 v_model_pch = v_model_pch,
# #                                 ylim_for_metric = ylim_for_Bias)
# #   # add gray dotted line at 0 bias
# #   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
# #   # add y-axis
# #   axis(2L)
# #   # between indication hetero labels
# #   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
# #        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
# #        tick = FALSE, cex.axis = 0.75,
# #        line = -0.5)
# #   box()
# #   ## Plot 2: Coverage no outlier
# #   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
# #                                 metric = "Cover", # row 2 is coverage 
# #                                 level_of_evidence = level_of_evidence, 
# #                                 v_models = v_models,
# #                                 v_model_OS_in_target = v_model_OS_in_target,
# #                                 v_model_col = v_model_col,
# #                                 v_model_pch = v_model_pch,
# #                                 ylim_for_metric = ylim_for_Cover)
# #   # add gray dotted line at 0.95 coverage
# #   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
# #   # add y-axis
# #   axis(2L)
# #   box()
# #   ### Plot 3: SOS no outlier
# #   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
# #                                 metric = "splittingModSeR_IE_w_share", # row 3 is strength of sharing 
# #                                 level_of_evidence = level_of_evidence, 
# #                                 v_models = v_models,
# #                                 v_model_OS_in_target = v_model_OS_in_target,
# #                                 v_model_col = v_model_col,
# #                                 v_model_pch = v_model_pch,
# #                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
# #   # add x-axis: within indication heterogeneity
# #   axis(1, at = 1:25, labels = FALSE) 
# #   axis(1, at = c(2,4, 
# #                  2+5,4+5, 
# #                  2+10,4+10,
# #                  2+15,4+15,
# #                  2+20,4+20),
# #        tick = FALSE,
# #        labels = paste0(rep(c(7, 30), 5), "%"),
# #        cex.axis = 0.60) 
# #   # add gray dotted line at 1 splitting SE
# #   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
# #   # add y-axis
# #   axis(2L)
# #   box()
# #   ### Plot 4: Bias one outlier
# #   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
# #                                 metric = "Bias", # row 1 is bias 
# #                                 level_of_evidence = level_of_evidence, 
# #                                 v_models = v_models,
# #                                 v_model_OS_in_target = v_model_OS_in_target,
# #                                 v_model_col = v_model_col,
# #                                 v_model_pch = v_model_pch,
# #                                 ylim_for_metric = ylim_for_Bias)
# #   # between indication hetero labels
# #   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
# #        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
# #        tick = FALSE, cex.axis = 0.75,
# #        line = -0.5)
# #   # add gray dotted line at 0 bias
# #   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
# #   box()
# #   ### Plot 5: Coverage one outlier
# #   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
# #                                 metric = "Cover", # row 2 is coverage 
# #                                 level_of_evidence = level_of_evidence, 
# #                                 v_models = v_models,
# #                                 v_model_OS_in_target = v_model_OS_in_target,
# #                                 v_model_col = v_model_col,
# #                                 v_model_pch = v_model_pch,
# #                                 ylim_for_metric = ylim_for_Cover)
# #   # add gray dotted line at 0.95 coverage
# #   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
# #   box()
# #   ### Plot 6: SOS one outlier
# #   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
# #                                 metric = "splittingModSeR_IE_w_share", # row 3 is strength of sharing 
# #                                 level_of_evidence = level_of_evidence, 
# #                                 v_models = v_models,
# #                                 v_model_OS_in_target = v_model_OS_in_target,
# #                                 v_model_col = v_model_col,
# #                                 v_model_pch = v_model_pch,
# #                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
# #   # add x-axis: within indication heterogeneity
# #   axis(1, at = 1:25, labels = FALSE) 
# #   axis(1, at = c(2,4, 
# #                  2+5,4+5, 
# #                  2+10,4+10,
# #                  2+15,4+15,
# #                  2+20,4+20),
# #        tick = FALSE,
# #        labels = paste0(rep(c(7, 30), 5), "%"),
# #        cex.axis = 0.60) 
# #   # add gray dotted line at 1 splitting SE
# #   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
# #   box()
# #   
# #   ### Column text
# #   mtext(text = "No outlier", outer = TRUE, at = c(3, 0.25), line = 2.5)
# #   mtext(text = "One outlier", outer = TRUE, at = c(3, 0.75), line = 2.5)
# #   ### Row text
# #   mtext(text = "Bias", side = 2, outer = TRUE, at = c(3, 0.8), line = 2.2, cex = 0.8)
# #   mtext(text = "Coverage", side = 2, outer = TRUE, at = c(3, 0.5), line = 2.2, cex = 0.8)
# #   mtext(text = "Splitting SE", side = 2, outer = TRUE, at = c(3, 0.15), line = 2.2, cex = 0.8)
# #   ### Between indication heterogneity labels
# #   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
# #   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
# #   ### Within indication heterogneity labels
# #   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.25), line = 1, cex = 0.7, side = 1)
# #   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.75), line = 1, cex = 0.7, side = 1)
# #   
# #   ### legend
# #   add_legend <- function(...) {
# #     opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
# #                 mar=c(0, 0, 0, 0), new=TRUE)
# #     on.exit(par(opar))
# #     plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
# #     legend(...)
# #   }
# #   add_legend("right", legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, 
# #              bty='n')
# #   
# #   # note which evidence set
# #   # *change this
# #   mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
# #   
# # }
# # 
# # fn_plot_multiplot(
# #   level_of_evidence = "early",
# #   v_models = c("uni_ie", 
# #                "uni_ie_w_share", 
# #                "uni_ce", 
# #                "uni_ce_w_share", 
# #                "uni_re", 
# #                "uni_re_w_share",
# #                "uni_re", 
# #                "uni_re_w_share"), 
# #   v_model_OS_in_target = c(rep(TRUE, 6), FALSE, FALSE),
# #   v_model_col = c(
# #     "steelblue", "steelblue", 
# #     "indianred","indianred", 
# #     "springgreen3","springgreen3",
# #     "springgreen3","springgreen3"), 
# #   v_model_pch = c(1, 19, 1, 19, 1, 19, 0, 15),
# #   v_model_legend_name = c("IE", "IE common", 
# #                           "CE", "CE common",
# #                           "RE", "RE common",
# #                           "RE no OS", "RE common \n no OS"), 
# #   ylim_for_Bias = c(-0.02, 0.12), 
# #   ylim_for_Cover = c(0.5, 1), 
# #   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# # )
# 
# 
# 
# # # which models to plot
# # v_models <- c("uni_ie", 
# #               "uni_ie_w_share", 
# #               "uni_ce", 
# #               "uni_ce_w_share", 
# #               "uni_re", 
# #               "uni_re_w_share")
# # v_model_col <- c(
# #   "steelblue", "steelblue", 
# #   "indianred","indianred", 
# #   "springgreen3","springgreen3") # one for each sharing assumption
# # v_model_pch <- c(1, 19, 1, 19, 1, 19) # shape of point for each model
# # v_model_legend_name <- c("IE", "IE common", 
# #                          "CE", "CE common",
# #                          "RE", "RE common")
# # ylim_for_Bias <- c(-0.02, 0.12)
# # ylim_for_Cover <- c(0.5, 1)
# # ylim_for_splittingModSeR_IE_w_share <- c(0, 3.5)
# 
# 
# ## multiplot: outlier indication
# 
# fn_plot_multiplot_outlier_indication <- function(level_of_evidence, 
#                                                  v_models,  
#                                                  v_model_OS_in_target,
#                                                  v_model_col, 
#                                                  v_model_pch ,
#                                                  v_model_legend_name, 
#                                                  ylim_for_Bias, 
#                                                  ylim_for_Cover, 
#                                                  ylim_for_EmpSE,
#                                                  ylim_for_splittingModSeR_IE_w_share
# ){
#   ## set up plot
#   plot.new()
#   par(mfcol = c(4, 2), 
#       mar = c(0.5, 0, 0, 0), # only include small space between the three rows
#       oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
#       mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
#   ## Plot 1: Bias no noise 
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 7, 5), # no noise, with os in target => set 7, no os in target => set 5
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   box()
#   ## Plot 2: Coverage no noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 7, 5), # no noise, with os in target => set 7, no os in target => set 5
#                                 metric = "Cover", # row 2 is coverage 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Cover)
#   # add gray dotted line at 0.95 coverage
#   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 3: EmpSE no noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 7, 5), # no noise, with os in target => set 7, no os in target => set 5
#                                 metric = "EmpSE", # row 3 is empse 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_EmpSE)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 4: SOS no noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 7, 5), # no noise, with os in target => set 7, no os in target => set 5
#                                 metric = "splittingModSeR_IE_w_share", # row 4 is strength of sharing 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   # add gray dotted line at 1 splitting SE
#   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 5: Bias with noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 8, 6), # with noise, with os in target => set 8, no os in target => set 6
#                                 metric = "Bias", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   box()
#   ### Plot 6: Coverage with noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 8, 6), # with noise, with os in target => set 8, no os in target => set 6  
#                                 metric = "Cover", # row 2 is coverage 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Cover)
#   # add gray dotted line at 0.95 coverage
#   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
#   box()
#   ### Plot 7: empse with noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 8, 6), # with noise, with os in target => set 8, no os in target => set 6
#                                 metric = "EmpSE", # row 3 is empse 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_EmpSE)
#   box()
#   ### Plot 8: SOS with noise
#   fn_plot_single_multiplot_tile(simulation_set_number = ifelse(v_model_OS_in_target[1], 8, 6), # with noise, with os in target => set 8, no os in target => set 6
#                                 metric = "splittingModSeR_IE_w_share", # row 4 is strength of sharing 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   # add gray dotted line at 1 splitting SE
#   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
#   box()
#   
#   ### Column text
#   mtext(text = "No noise", outer = TRUE, at = c(3, 0.25), line = 2.5)
#   mtext(text = "With noise", outer = TRUE, at = c(3, 0.75), line = 2.5)
#   ### Row text
#   # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
#   mtext(text = "Bias", side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
#   mtext(text = "Coverage", side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
#   mtext(text = "Empirical SE", side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
#   mtext(text = "Splitting SE", side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
#   ### Between indication heterogneity labels
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
#   ### Within indication heterogneity labels
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.25), line = 1, cex = 0.7, side = 1)
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.75), line = 1, cex = 0.7, side = 1)
#   
#   ### legend
#   add_legend("right", legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, 
#              bty='n')
#   
#   # note which evidence set
#   # *change this
#   mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
#   
# }
# 
# 
# ## multiplot: PFS bias etc
# 
# fn_plot_multiplot_PFS <- function(level_of_evidence, 
#                                   v_models,  
#                                   v_model_OS_in_target,
#                                   v_model_col, 
#                                   v_model_pch ,
#                                   v_model_legend_name, 
#                                   ylim_for_Bias, 
#                                   ylim_for_Cover, 
#                                   ylim_for_EmpSE,
#                                   ylim_for_splittingModSeR_IE_w_share
# ){
#   ## set up plot
#   plot.new()
#   par(mfcol = c(4, 2), 
#       mar = c(0.5, 0, 0, 0), # only include small space between the three rows
#       oma = c(4, 4, 4, 8), # create space for the column headings and the lengend on the right hand side of the plot
#       mgp = c(2, .6, 0)) # mgp is a numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. Default value : c(3,1,0). First value : location the labels (xlab and ylab in plot). Second value : location of the tick-mark labels. Third Value : position of the tick marks
#   ## Plot 1: Bias no outlier 
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "Bias_PFS", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   box()
#   ## Plot 2: Coverage no outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "Cover_PFS", # row 2 is coverage 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Cover)
#   # add gray dotted line at 0.95 coverage
#   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 3: EmpSE no outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "EmpSE_PFS", # row 3 is empse 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_EmpSE)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 4: SOS no outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 1, # no outlier 
#                                 metric = "splittingModSeR_IE_w_share_PFS", # row 4 is strength of sharing 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   # add gray dotted line at 1 splitting SE
#   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
#   # add y-axis
#   axis(2L)
#   box()
#   ### Plot 5: Bias one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "Bias_PFS", # row 1 is bias 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Bias)
#   # between indication hetero labels
#   axis(3, labels = paste0(c(0, 7, 15, 30, 50), "%"), 
#        at = c(2.5, 2.5 + 5, 2.5 + 10, 2.5 + 15, 2.5 + 20),  # between indication heterogeneity
#        tick = FALSE, cex.axis = 0.75,
#        line = -0.5)
#   # add gray dotted line at 0 bias
#   abline(h = 0, lty = 3, col = "gray2", lwd = 1)
#   box()
#   ### Plot 6: Coverage one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "Cover_PFS", # row 2 is coverage 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_Cover)
#   # add gray dotted line at 0.95 coverage
#   abline(h = 0.95, lty = 3, col = "gray2", lwd = 1)
#   box()
#   ### Plot 7: empse one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "EmpSE_PFS", # row 3 is empse 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_EmpSE)
#   box()
#   ### Plot 8: SOS one outlier
#   fn_plot_single_multiplot_tile(simulation_set_number = 3, # one outlier 
#                                 metric = "splittingModSeR_IE_w_share_PFS", # row 4 is strength of sharing 
#                                 level_of_evidence = level_of_evidence, 
#                                 v_models = v_models,
#                                 v_model_OS_in_target = v_model_OS_in_target,
#                                 v_model_col = v_model_col,
#                                 v_model_pch = v_model_pch,
#                                 ylim_for_metric = ylim_for_splittingModSeR_IE_w_share)
#   # add x-axis: within indication heterogeneity
#   axis(1, at = 1:25, labels = FALSE) 
#   axis(1, at = c(2,4, 
#                  2+5,4+5, 
#                  2+10,4+10,
#                  2+15,4+15,
#                  2+20,4+20),
#        tick = FALSE,
#        labels = paste0(rep(c(7, 30), 5), "%"),
#        cex.axis = 0.60) 
#   # add gray dotted line at 1 splitting SE
#   abline(h = 1, lty = 3, col = "gray2", lwd = 1)
#   box()
#   
#   ### Column text
#   mtext(text = "No outlier", outer = TRUE, at = c(3, 0.25), line = 2.5)
#   mtext(text = "One outlier", outer = TRUE, at = c(3, 0.75), line = 2.5)
#   ### Row text
#   # left hand side runs from 0 (bottom) to 1 (top), split into 8 segments of 0.125 each and place labels evenly
#   mtext(text = "Bias", side = 2, outer = TRUE, at = c(3, 0.875), line = 2.2, cex = 0.8)
#   mtext(text = "Coverage", side = 2, outer = TRUE, at = c(3, 0.625), line = 2.2, cex = 0.8)
#   mtext(text = "Empirical SE", side = 2, outer = TRUE, at = c(3, 0.375), line = 2.2, cex = 0.8)
#   mtext(text = "Splitting SE", side = 2, outer = TRUE, at = c(3, 0.125), line = 2.2, cex = 0.8)
#   ### Between indication heterogneity labels
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.25), line = 1, cex = 0.7)
#   mtext(text = "Between indication heterogeneity (CV)", outer = TRUE, at = c(3, 0.75), line = 1, cex = 0.7)
#   ### Within indication heterogneity labels
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.25), line = 1, cex = 0.7, side = 1)
#   mtext(text = "Within indication heterogeneity (CV)", outer = TRUE, at = c(0.75), line = 1, cex = 0.7, side = 1)
#   
#   ### legend
#   add_legend("right", legend=v_model_legend_name, pch=v_model_pch, col = v_model_col, 
#              bty='n')
#   
#   # note which evidence set
#   # *change this
#   mtext(text = level_of_evidence, outer = TRUE, at = c(1.1), line = 1, cex = 0.7, side = 1)
#   
# }


## Plot performance for paper ########

# # Bias
# 
# # Add extra space to right of plot area; change clipping to figure
# par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
# 
# # univariate non mixture
# df_performance <- data.frame(
#   model = c("IE", "IE_common", "CE", "CE_common", "RE", "RE_common"),
#   prec_7 = c(0,0,0.02,0.02,0.02,0.01),
#   prec_30 = c(0,0,0.05,0.07,0.04,0.02),
#   prec_50 = c(0,0,0.06,0.10,0.05,0.02),
#   model_colour = c("black", "black", "red", "red", "green", "green")
# )
# plot(1, type="n", xlab="Heterogeneity within and between indications", 
#      ylab="Bias", xlim=c(0.07, 0.5), ylim=c(-0.02, 0.1),
#      main = "Univariate non-mixture models \n One outlier indication")
# # add the model results
# for(model_num in 1:nrow(df_performance)){
#   lines(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], lty=model_num, col = df_performance$model_colour[model_num])
#   points(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], col = df_performance$model_colour[model_num])
# }
# # Add legend to top right, outside plot region
# legend("topright", inset=c(-0.5,0), 
#        legend= df_performance$model, 
#        lty=c(1:nrow(df_performance)), col = df_performance$model_colour, bty = "n")                           
# 
# # univariate mixture
# df_performance <- data.frame(
#   model = c("MCIE", "MCIE_common", "MRIE", "MRIE_common"),
#   prec_7 = c(0.02,0.02,0.02,0.01),
#   prec_30 = c(0.06,0.07,0.04,0.03),
#   prec_50 = c(0.07,0.10,0.06,0.03),
#   model_colour = c("red", "red", "green", "green")
# )
# plot(1, type="n", xlab="Heterogeneity within and between indications", 
#      ylab="Bias", xlim=c(0.07, 0.5), ylim=c(-0.02, 0.1),
#      main = "Univariate mixture models \n One outlier indication")
# # add the model results
# for(model_num in 1:nrow(df_performance)){
#   lines(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], lty=model_num, col = df_performance$model_colour[model_num])
#   points(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], col = df_performance$model_colour[model_num])
# }
# # Add legend to top right, outside plot region
# legend("topright", inset=c(-0.5,0), 
#        legend= df_performance$model, 
#        lty=c(1:nrow(df_performance)), col = df_performance$model_colour, bty = "n")                           
# 
# # surrogate unmatched
# df_performance <- data.frame(
#   model = c("CE", "CE_common", "RE", "RE_common"),
#   prec_7 = c(-0.01),
#   prec_30 = c(0),
#   prec_50 = c(0),
#   model_colour = c("red", "red", "green", "green")
# )
# plot(1, type="n", xlab="Heterogeneity within and between indications", 
#      ylab="Bias", xlim=c(0.07, 0.5), ylim=c(-0.02, 0.1),
#      main = "Unmatched surrogate models \n One outlier indication")
# # add the model results
# for(model_num in 1:nrow(df_performance)){
#   lines(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], lty=model_num, col = df_performance$model_colour[model_num])
#   points(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], col = df_performance$model_colour[model_num])
# }
# # Add legend to top right, outside plot region
# legend("topright", inset=c(-0.5,0), 
#        legend= df_performance$model, 
#        lty=c(1:nrow(df_performance)), col = df_performance$model_colour, bty = "n")                           
# 
# # surrogate matched
# df_performance <- data.frame(
#   model = c("CE", "CE_common", "RE", "RE_common"),
#   prec_7 = c(0.01),
#   prec_30 = c(0.01,0.06,0.02,0.02),
#   prec_50 = c(0.02,0.09,0.03,0.03),
#   model_colour = c("red", "red", "green", "green")
# )
# plot(1, type="n", xlab="Heterogeneity within and between indications", 
#      ylab="Bias", xlim=c(0.07, 0.5), ylim=c(-0.02, 0.1),
#      main = "Matched surrogate models \n One outlier indication")
# # add the model results
# for(model_num in 1:nrow(df_performance)){
#   lines(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], lty=model_num, col = df_performance$model_colour[model_num])
#   points(x = c(0.07, 0.3, 0.5), y = df_performance[model_num,2:4], col = df_performance$model_colour[model_num])
# }
# # Add legend to top right, outside plot region
# legend("topright", inset=c(-0.5,0), 
#        legend= df_performance$model, 
#        lty=c(1:nrow(df_performance)), col = df_performance$model_colour, bty = "n")                           
# 



# ## (Single) Plot winning model for a specific model, metric, time point effect size
# # applied to Viking results
# # plots the winning model for each combination of CV within and between
# # at each time point 
# # requires l_results to be loaded (see ResultsForPaper.R)
# 
# # simulation_set_number <- 2
# # model_set <- "uni_non_mixture" # "all" * extend when adding new models
# # lower_better <- TRUE
# # evidence_set <- "early_no OS"
# # metric <- "Bias"
# # effect_size <- "small"
# # google_drive_path <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/"
# 
# fn_CV_winner_plot <- function(simulation_set_number,
#                               model_set,
#                               lower_better,
#                               evidence_set ,
#                               metric,
#                               effect_size,
#                               google_drive_path){
#   # turn off scientific notation
#   options(scipen=999)
#   
#   ## load key linking scenario code e.g. "2002" to CV within and CV between
#   # load parameters used in each scenario (e.g. simulation set 1)
#   df_simulation_set <- read.csv(paste0(google_drive_path, "Simulation files/df_set",simulation_set_number,"_scenarios.csv"))
#   
#   # Levels of CV within and between indications to plot
#   log_M_01_CV_w <- c(0, 0.07, 0.15, 0.30, 0.50) # CV within options
#   log_M_01_CV_bw <- c(0, 0.07, 0.15, 0.30, 0.50) # CV between options
#   log_M_01_mu <- ifelse(effect_size == "small", -0.223, -0.511) # effect size small or large
#   # create dataset of all combinations of CV and effect size for plot
#   data <- expand.grid(log_M_01_CV_w = log_M_01_CV_w, 
#                       log_M_01_CV_bw = log_M_01_CV_bw, 
#                       log_M_01_mu = log_M_01_mu) 
#   
#   # for each combination of parameters, find the corresponding row_index in df_simulation_set
#   data$row_index <- NA # initialize
#   for(s in 1:nrow(data)){
#     data$row_index[s] <- which(df_simulation_set$log_M_01_mu == data$log_M_01_mu[s] &
#                                  df_simulation_set$log_M_01_CV_w == data$log_M_01_CV_w[s] &
#                                  df_simulation_set$log_M_01_CV_bw == data$log_M_01_CV_bw[s])
#   }
#   # assign the parameter sets associated with these row indexes
#   data$Parameter_set <- df_simulation_set$Parameter_set[data$row_index]
#   
#   # find winning model for each combination
#   data$best_model <- NA # initialize
#   
#   # the models considered depend on model_set input
#   # only consider the rows of the relevant models
#   if(model_set == "uni_non_mixture"){
#     model_name <-                             c("uni_ie", "uni_ce",             "uni_re",                                                                                     "uni_ce_w_share",                     "uni_re_w_share")
#     relevant_model_rows <- c(1,2,4,10, 12)    # "uni_ie", "uni_ce", "uni_mcie", "uni_re", "uni_mrie", "bi_ce_matched", "bi_re_matched", "bi_ce_unmatched", "bi_re_unmatched", "uni_ce_w_share", "uni_mcie_w_share", "uni_re_w_share", "uni_mrie_w_share", "bi_ce_matched_w_share", "bi_re_matched_w_share"
#   } else if(model_set == "uni_all"){
#     model_name <-                             c("uni_ie", "uni_ce", "uni_mcie", "uni_re", "uni_mrie",                                                                         "uni_ce_w_share", "uni_mcie_w_share", "uni_re_w_share", "uni_mrie_w_share")
#     relevant_model_rows <- c(1:5, 10:13  )    # "uni_ie", "uni_ce", "uni_mcie", "uni_re", "uni_mrie", "bi_ce_matched", "bi_re_matched", "bi_ce_unmatched", "bi_re_unmatched", "uni_ce_w_share", "uni_mcie_w_share", "uni_re_w_share", "uni_mrie_w_share", "bi_ce_matched_w_share", "bi_re_matched_w_share"
#   } else if(model_set == "all"){
#     model_name <- c("uni_ie", "uni_ce", "uni_mcie", "uni_re", "uni_mrie", "bi_ce_matched", "bi_re_matched", "bi_ce_unmatched", "bi_re_unmatched", "uni_ce_w_share", "uni_mcie_w_share", "uni_re_w_share", "uni_mrie_w_share", "bi_ce_matched_w_share", "bi_re_matched_w_share")
#     relevant_model_rows <- 1:length(model_name)
#   }
#   
#   # For the given metrics find the best model in each row of the dataset
#   for(s in 1:nrow(data)){
#     if(!is.na(data$Parameter_set[s])){
#       if(lower_better == TRUE){
#         data$best_model[s] <- model_name[which.min(l_results[[paste0(data$Parameter_set[s], 
#                                                                      "_", evidence_set, ".csv")]][[metric]][relevant_model_rows] 
#         )]
#       } else {
#         data$best_model[s] <- model_name[which.max(l_results[[paste0(data$Parameter_set[s], 
#                                                                      "_", evidence_set, ".csv")]][[metric]][relevant_model_rows] 
#         )] 
#       }
#     }
#   }                                                   
#   # dont report anything if the data is missing
#   data$best_model[is.na(data$best_model)] <- ""
#   
#   # plot
#   plot(1, type="n", xaxt = "n", yaxt = "n", xlab="CV within", ylab="CV between", 
#        xlim=c(-0.05, 0.55), ylim=c(-0.05, 0.55),
#        main = paste("Models with best",metric , "\n For", evidence_set, "and", effect_size, "effect size")
#   )
#   axis(1, at = log_M_01_CV_w , las=2, labels = paste0(round(log_M_01_CV_w*100, 0), "%"))
#   axis(2, at = log_M_01_CV_bw , las=2, labels = paste0(round(log_M_01_CV_w*100, 0), "%"))
#   # plot which sharing assumption is best at each combination of CV within and between
#   for(s in 1:nrow(data)){
#     text(data$log_M_01_CV_w[s], 
#          data$log_M_01_CV_bw[s], 
#          data$best_model[s], cex = 1.1) 
#   }
# }
# 
# 
# 
# # simulation_set_number <- 2
# # model_set <- "uni_non_mixture" # "all" * extend when adding new models
# # lower_better <- TRUE
# # evidence_set <- "early_no OS"
# # metric <- "Bias"
# # effect_size <- "small"
# # google_drive_path <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/"
# 
# # fn_CV_winner_plot(simulation_set_number = 2,
# #                   model_set = "uni_non_mixture", # "uni_non_mixture","all"
# #                   lower_better = TRUE,
# #                   evidence_set = "late_with OS",
# #                   metric = "splittingModSeR_IE",
# #                   effect_size = "large",
# #                   google_drive_path = "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/")
# # 
# 
# ## (Loop) Plot winning model for each time point and effect size
# # plots the winning model for all the relevant combinations 
# # and saves the result
# 
# fn_CV_winner_loop_plot <- function(simulation_set_number,
#                                    metric, 
#                                    model_set,
#                                    lower_better,
#                                    figures_location,
#                                    google_drive_path){
#   
#   # create combinations of evidence sets and effect sizes for plotting
#   plot_combinations_0 <- expand.grid(evidence_set = c("early_no OS", "mid_no OS", "late_no OS",
#                                                       "early_with OS", "mid_with OS", "late_with OS"),
#                                      effect_size = c("small", "large"))
# 
#   # cannot compute strength of sharing metrics with no OS, remove
#   strength_of_sharing_metric <- any(metric == c("splittingModSeR_IE" ,"PED_IE" ,"splittingModSeR_obs" ,"PED_obs",
#                                                 "splittingEmpSeR_IE", "splittingEmpSeR_obs"))
#   if(strength_of_sharing_metric){
#     remove_rows <- grepl("no OS", plot_combinations_0$evidence_set, fixed = TRUE) 
#     plot_combinations_1 <- plot_combinations_0[!remove_rows,]
#   } else {
#     plot_combinations_1 <- plot_combinations_0
#   }
#   
#   # early, mid and late always go together
#   # => create plotes in groups of three
#   number_of_plots <- nrow(plot_combinations_1)/3
#   plot_combinations_1$plot_group <- rep(1:number_of_plots, each = 3)
#   
#   # loop over plot groups
#   for(c in 1:number_of_plots){
#     # set up ploting 
#     width <- 1180
#     height <- 220
#     # first row in plot_combinations_1 which corresponds to c
#     # contains all the information required
#     plot_group_row <- which(plot_combinations_1$plot_group == c)[1]
#     png(paste0(figures_location , # save location 
#                model_set, "_", # model set
#                metric, # metric
#                "_Set_", simulation_set_number, "_", # set number
#                plot_combinations_1$effect_size[plot_group_row], "_effect_", #effect size
#                ifelse(grepl("no OS", plot_combinations_1$evidence_set[plot_group_row], fixed = TRUE), "no_OS", "with_OS"),
#                ".png"),
#         width = width, height = height)
#     
#     layout(mat=matrix(c(1,2,3),nrow=1,ncol=3,byrow=T))
#     
#     # create the three plots: early mid late
#     # early is in plot_group_row + 0
#     fn_CV_winner_plot(simulation_set_number= simulation_set_number,
#                       model_set = model_set,
#                       lower_better = lower_better,
#                       evidence_set = plot_combinations_1$evidence_set[plot_group_row + 0],
#                       metric = metric,
#                       effect_size = plot_combinations_1$effect_size[plot_group_row + 0],
#                       google_drive_path = google_drive_path)
#     # mid is in plot_group_row + 1
#     fn_CV_winner_plot(simulation_set_number= simulation_set_number,
#                       model_set = model_set,
#                       lower_better = lower_better,
#                       evidence_set = plot_combinations_1$evidence_set[plot_group_row + 1],
#                       metric = metric,
#                       effect_size = plot_combinations_1$effect_size[plot_group_row + 1],
#                       google_drive_path = google_drive_path)
#     # late is in plot_group_row + 2
#     fn_CV_winner_plot(simulation_set_number= simulation_set_number,
#                       model_set = model_set,
#                       lower_better = lower_better,
#                       evidence_set = plot_combinations_1$evidence_set[plot_group_row + 2],
#                       metric = metric,
#                       effect_size = plot_combinations_1$effect_size[plot_group_row + 2],
#                       google_drive_path = google_drive_path)
#     dev.off()
#   }
# }
# 
# # fn_CV_winner_loop_plot(simulation_set_number = 2,
# #                                    metric = "Bias", 
# #                                    model_set = "all",
# #                                    lower_better = TRUE,
# #                                    figures_location = paste0("C:/MultiIndication_results/Figures/",
# #                                                              "Model_winner/", "Set2/"),
# #                                    google_drive_path = "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/")


### Plot posterior for target OS ################
# INPUTs: 
#       prediction number (r) and iteration number (i)
#       xlim, ylim for the plot
# output: plot density against the true value
# note: will not output prediction if target_posterior is just NAs

fn_plot_posterior <- function(r, i,
                              xlim, ylim ){
  
  ## Extract the target posterior for predication r iteration i
  target_posterior <- list_df_target_posteriors[[r]][,i]
  
  ## Define observed and true OS in the target indication for predication r iteration i
  
  ## name of model which will be fitted for prediction r
  name_model_to_fit <- df_prediction_full$Model_to_fit[r]
  # index of dataset the model is fit to
  # extract everything before the first underscore
  dataset_index <- sub("\\_.*", "", name_model_to_fit)
  # full dataset underlying predication r in iteration i
  underlying_dataset <- as.data.frame(list_df_data_sets[[dataset_index]][,])
  
  if(df_prediction_full$Target_OS[r] == "with OS"){
    ## with OS => 1st study/indication 
    # observed target OS
    observed_target_OS <- underlying_dataset$OS_LHR_mean[1]
    # long run true target OS
    true_OS_target <- underlying_dataset$OS_ALHR_indication_true[1]
    # short run true target OS
    partial_true_OS_target <- underlying_dataset$OS_partial_ALHR_indication_true[1]
  } else {
    ## no OS => last study/indication 
    # observed target OS
    observed_target_OS <- underlying_dataset$OS_LHR_mean[which.max(underlying_dataset$indication_index)]
    # long run true target OS
    true_OS_target <- underlying_dataset$OS_ALHR_indication_true[which.max(underlying_dataset$indication_index)]
    # short run true target OS
    partial_true_OS_target <- underlying_dataset$OS_partial_ALHR_indication_true[which.max(underlying_dataset$indication_index)]
  }
  
  ## plot posterior for predication r, indication i
  if(!is.na(target_posterior)[1]){
    
    # posterior 95% interval
    target_OS_95_post <-quantile(target_posterior, probs = c(0.025, 0.975))
    plot(density(target_posterior),
         main = paste0(df_prediction_full$Parameter_set[r], ": ",
                       df_prediction_full$Times[r], ", ",
                       df_prediction_full$Target_OS[r], ", ",
                       df_prediction_full$Models[r], " i=", i),
         xlab = paste0("ALHR OS ", round(mean(target_posterior), 2), "(",
                       round(target_OS_95_post[1], 2), ",",
                       round(target_OS_95_post[2], 2),
                       ")" ), 
         xlim = xlim,
         ylim = ylim)
    # observed OS in the target indication study 
    points(observed_target_OS, 0) 
    # true OS in the target indication
    points(true_OS_target, 0, pch = 16)
    # short run true OS in the target indicaiton
    points(partial_true_OS_target, 0, pch = 4)
    # posterior 95% interval
    abline(v = target_OS_95_post[1], lty = 2, col = "blue")
    abline(v = target_OS_95_post[2], lty = 2, col = "blue")
    # posterior expectation
    abline(v = mean(target_posterior), lty = 2, col = "red")
    # legend
    legend("topright", legend = c(paste0("Observed:", round(observed_target_OS, 2)), 
                                  paste0("Truth:", round(true_OS_target, 2)),  
                                  paste0("Partial truth:", round(partial_true_OS_target, 2))), 
           pch = c(1, 16, 4))
    
  }
}



# ### Plot observed vs predicted OS - forest plot ################
# # take in the simulation (r) and iteration (i) number and outputs a forest plot
# 
# # note that the target indication depends on whether:
# # with OS => indication 1 is target
# # no OS => indicaiton 8 is target
# 
# fn_plot_dataset <- function(r, i){
#   
#   # load df of target posteriors for iteration i
#   df_target_posterior_i <- read.csv(paste0(results_location, "TargetOS/df_target_posteriors_", i, ".csv"),
#                                     check.names=FALSE)
#   
#   ## Target posterior summary for predication r iteration i
#   target_posterior <- df_target_posterior_i[,r]
#   target_OS_95_post <-quantile(target_posterior, probs = c(0.025, 0.975), na.rm = TRUE)
#   
#   ## load the underlying dataset with all values for all studies
#   # name of model which will be fitted for prediction r
#   name_model_to_fit <- df_prediction_full$Model_to_fit[r]
#   # index of dataset the model is fit to
#   # extract everything before the first underscore
#   dataset_index <- sub("\\_.*", "", name_model_to_fit)
#   # full dataset underlying predication r in iteration i
#   # list_df_data_sets <- (paste0(results_location,"Datasets/list_df_data_sets_", i, ".rda"))
#   underlying_dataset <- as.data.frame(list_df_data_sets[[dataset_index]][,])
#   
#   ## **Fix this - need underlying dataset for r and i ######
#   # ## load the specific dataset dataset used to fit model
#   # # predication r, iteration i - includes d
#   # bugsOutput <- readRDS(paste0(results_location, "Models/",
#   #                              df_prediction_full$Model_to_fit[r], 
#   #                              "/",i ,"/",
#   #                              "bugsOutput.rds"))
#   
#   # # invert dataframe so that indication 1 is at the bottom
#   #df_plot <- map_df(bugsOutput$dataset, rev) # need to load purr
#   df_plot <- underlying_dataset
#   
#   # initialise plot
#   x_axis_bounds <- c(
#     # lower bound
#     min(-0.1, min(df_plot$OS_LHR_mean) - max(df_plot$OS_LHR_se)*2, target_OS_95_post[1]),
#     # upper bound
#     max( 0.2, max(df_plot$OS_LHR_mean) - max(df_plot$OS_LHR_se)*3, target_OS_95_post[2])
#   )
#   
#   # create plot title
#   plot_title <- paste0(df_prediction_full$Parameter_set[r], ":",
#                        df_prediction_full$Times[r], ",",
#                        df_prediction_full$Target_OS[r], ",",
#                        df_prediction_full$Models[r], ",i=", i)
#   
#   # initialise plot
#   plot(1, type="n", 
#        xlab="OS log hazard ratio", xlim=x_axis_bounds, 
#        ylab="", ylim=c(0, 10), yaxt='n',
#        main = plot_title)
#   
#   # add line of no effect
#   abline(v = 0)
#   
#   # define the y axis values for:
#   # each of the studies observed studies + 
#   # the unobserved study in indication 8 +
#   # the prediction for the target
#   number_of_studies <- nrow(df_plot)
#   y_increments <- rep(9.7/(number_of_studies + 2), (number_of_studies + 2)) # full height is 10
#   y_values <- rev(cumsum(y_increments))
#   
#   # plot observed results from all observed indications 
#   # and the prediction for the target indication (Pred)
#   # The graph should indicate which indcation is the target with a (T) after the indication number
#   # with OS => indication 1 is target
#   # no OS => indicaiton 8 is target
#   labels=c(df_plot$indication_index, 8, "Pred")
#   if(df_prediction_full$Target_OS[r] == "with OS"){
#     labels[which(labels == "1")] <- "1(T)"
#   } else{
#     labels[which(labels == "8")] <- "8(T)"
#   }
#   
#   # add y axis labels to indication indication
#   axis(side = 2, at=y_values, labels=labels, las = 2, cex.axis=0.5)
#   
#   # plot the observed OS (point est and 95 interval), true study OS (partial) and true indication OS (partial)
#   # plot for all observed indications
#   for(i in 1:nrow(df_plot)){
#     # observed OS point estimate in study i
#     points(df_plot$OS_LHR_mean[i], y_values[i])
#     # observed OS 95% interval for study i
#     lines(x = c(
#       df_plot$OS_LHR_mean[i] - df_plot$OS_LHR_se[i]*1.96,
#       df_plot$OS_LHR_mean[i] + df_plot$OS_LHR_se[i]*1.96),
#       y = rep(y_values[i],2))
#     # true study OS (partial)
#     points(df_plot$OS_partial_ALHR_study_true[i], y_values[i], pch = 5)
#     # true indication OS (partial)
#     points(df_plot$OS_partial_ALHR_indication_true[i], y_values[i], pch = 4, col = "blue")
#   }
#   
#   # add the true values in the unobserved indication 8 (for "no OS" only)
#   if(df_prediction_full$Target_OS[r] == "no OS"){
#     # true values are stored in the underlying dataset only, load this
#     # no OS => indicaiton 8 is target
#     index_indication_8_underlying <- which(underlying_dataset$indication_index == 8)
#     # the y value in the plot for indication 8, always the second last
#     points(underlying_dataset$OS_partial_ALHR_study_true[index_indication_8_underlying], tail(y_values,2)[1], pch = 5)
#     points(underlying_dataset$OS_partial_ALHR_indication_true[index_indication_8_underlying], tail(y_values,2)[1], pch = 4, col = "blue")
#   }
#   
#   # add the posterior prediction in red
#   points(mean(target_posterior), tail(y_values,1), col = "red")
#   lines(x = target_OS_95_post,
#         y = rep(tail(y_values,1),2), col = "red")
#   
#   # add the true values for the target indication against the prediction
#   if(df_prediction_full$Target_OS[r] == "with OS"){
#     # with OS => indication 1 is target
#     index_target_indication <- which(df_plot$indication_index == 1)
#     # the y value in the plot for the prediction is always the last
#     points(df_plot$OS_partial_ALHR_study_true[index_target_indication], tail(y_values,1), pch = 5)
#     points(df_plot$OS_partial_ALHR_indication_true[index_target_indication], tail(y_values,1), pch = 4, col = "blue")
#   } else{
#     # no OS => indicaiton 8 is target
#     # true values are stored in the underlying dataset only, load this
#     index_indication_8_underlying <- which(underlying_dataset$indication_index == 8)
#     # the y value in the plot for the prediction is always the last
#     points(underlying_dataset$OS_partial_ALHR_study_true[index_indication_8_underlying], tail(y_values,1), pch = 5)
#     points(underlying_dataset$OS_partial_ALHR_indication_true[index_indication_8_underlying], tail(y_values,1), pch = 4, col = "blue")
#   }
#   
#   # # overlay the target indication in bold
#   # points(df_plot$OS_LHR_mean[index_target_indication], y_values[index_target_indication], lwd = 1.5)
#   # lines(x = c(
#   #   df_plot$OS_LHR_mean[index_target_indication] - df_plot$OS_LHR_se[index_target_indication]*1.96,
#   #   df_plot$OS_LHR_mean[index_target_indication] + df_plot$OS_LHR_se[index_target_indication]*1.96),
#   #   y = rep(y_values[index_target_indication],2), lwd = 1.5)
#   # points(df_plot$OS_partial_ALHR_study_true[index_target_indication], y_values[index_target_indication], pch = 5)
#   # points(df_plot$OS_partial_ALHR_indication_true[index_target_indication], y_values[index_target_indication], pch = 4, col = "blue")
#   
#   # add legend
#   legend("topright", legend=c("Observed", "True study", "True indication"),
#          pch=c(1, 5, 4), col= c("black", "black", "blue"), cex=0.7)
# }

# ### Plot joint distribution of OS and PFS ################
# # input the rows of a dataframe with correspond to an indication
# # output the joint distribution of PFS and OS
# 
# #df_input <- list_df_data_sets[["4.1.1"]][2:10,]
# 
# fn_indication_joint_plot <- function(df_input, plot_title){
#   # save input as dataframe
#   df_plot <- as.data.frame(df_input)
#   
#   # initialise plot
#   y_axis_bounds <- c(
#     # lower bound
#     min(-0.1, min(df_plot$OS_LHR_mean) - max(df_plot$OS_LHR_se)*2),
#     # upper bound
#     max( 0.1, max(df_plot$OS_LHR_mean) - max(df_plot$OS_LHR_se)*2)
#   )
#   x_axis_bounds <- c(
#     # lower bound
#     min(-0.1, min(df_plot$PFS_LHR_mean) - max(df_plot$PFS_LHR_se)*2),
#     # upper bound
#     max( 0.1, max(df_plot$PFS_LHR_mean) - max(df_plot$PFS_LHR_se)*2)
#   )
#   
#   plot(df_plot$PFS_LHR_mean , df_plot$OS_LHR_mean,
#        xlim = x_axis_bounds,
#        ylim = y_axis_bounds,
#        xlab = "PFS LHR",
#        ylab = "OS LHR",
#        main = plot_title, 
#        col = "red")
#   abline(h = 0)
#   abline(v = 0)
#   # surrogate model based on observed studies
#   abline(lm(df_plot$OS_LHR_mean ~ df_plot$PFS_LHR_mean), col = "red")
#   # add true study values
#   points(df_plot$PFS_LHR_study_true, df_plot$OS_partial_ALHR_study_true, pch = 5, col = "blue")
#   abline(lm(df_plot$OS_partial_ALHR_study_true ~ df_plot$PFS_LHR_study_true), col = "blue")
#   # # add true mature study values
#   # points(df_plot$PFS_LHR_study_true, df_plot$OS_ALHR_study_true, pch = 16, col = "green")
#   # abline(lm(df_plot$OS_ALHR_study_true ~ df_plot$PFS_LHR_study_true), col = "green")
#   
#   # add uncertainty intervals aound observed outcomes
#   draw.ellipse(x = df_plot$PFS_LHR_mean, # midpoint on x axis
#                y = df_plot$OS_LHR_mean, # midpoint on y axis
#                a = df_plot$PFS_LHR_se*1.96, # radius on x axis 
#                b = df_plot$OS_LHR_se*1.96, # radius on y axis
#                border=c(2),
#                angle=c(0), segment=rbind(rep(45),c(360)))
#   # add legend
#   legend("bottomright", legend=c("Observed", "True study"),
#          pch=c(1, 5), col= c("red", "blue"), cex=0.7)
# }
