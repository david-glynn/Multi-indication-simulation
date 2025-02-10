
# Plot results #####

## Load functions and data ####

library(plotrix) # required for CI on plots

## Path go Shared google drive
# This is where everything else is stored
google_drive_path <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/"

## load the required functions
source(paste0(google_drive_path, "Simulation files/Plotting functions.R"))

## load list of all results
l_results <- readRDS(paste0(google_drive_path, "/l_results.RData"))

# ## Calculate results - only when results updated
# 
# ## load all the Viking results: for all sets and scenarios in the folder e.g. 2046 early not OS etc
# file_list <- list.files(paste0(google_drive_path, "Viking simulation results/Result_elements/"))
# # remove the archive file as it is not relevant
# # file_list <- file_list[! file_list %in% c("Archive")]
# # remove the extra metrics calculated, just keep the main results
# file_list <- file_list[!grepl("^Bias_", file_list) & 
#                           !grepl("^ModSE_", file_list) &
#                           !grepl("^Rhat", file_list) &
#                           !grepl("^point_est", file_list) &
#                           !(grepl("^70", file_list) & grepl("no OS", file_list))]
# 
# 
# # create a list containing the results in the folder
# l_results <- vector(mode = "list", length = length(file_list))
# for(el in 1:length(file_list)){
# 
#   l_results[[el]] <- read.csv(paste0(google_drive_path, "Viking simulation results/Result_elements/", file_list[el]))
# 
#   # file_name <- file_list[el]
#   # assign(file_name, value = read.csv(file_list[el]))
#   cat('\r', paste0(round(el/length(file_list)*100,0), "% complete"))
# }
# names(l_results) <- file_list
# 
# ## calculate splittingModSeR_IE_w_share for all models when is no OS in the target
# # compare the ModSE with no OS to ModSE for IE_w_share when there is OS
# # note, dont need to do this when there is always os in the target (sets 7 and 8) 
# v_scenario_number <- c(1026:1050,
#                        2026:2050,
#                        3026:3050,
#                        #4026:4050, # with hetero
#                        5026:5050
#                        #6026:6050 # with hetero
#                        )
# for(el in 1:length(v_scenario_number)){
# 
#   row_index_IE_w_share <- which(l_results$`1026_early_no OS.csv`$Model == "uni_ie_w_share")
# 
#   if(v_scenario_number[el] < 5000){
#     # for sets 1 to 4
#     l_results[[paste0(v_scenario_number[el], "_early_no OS.csv")]]["splittingModSeR_IE_w_share"] <-
#       l_results[[paste0(v_scenario_number[el], "_early_no OS.csv")]][,"ModSE"]/
#       l_results[[paste0(v_scenario_number[el], "_early_with OS.csv")]][row_index_IE_w_share,"ModSE"]
# 
#     l_results[[paste0(v_scenario_number[el], "_mid_no OS.csv")]]["splittingModSeR_IE_w_share"] <-
#       l_results[[paste0(v_scenario_number[el], "_mid_no OS.csv")]][,"ModSE"]/
#       l_results[[paste0(v_scenario_number[el], "_mid_with OS.csv")]][row_index_IE_w_share,"ModSE"]
# 
#     l_results[[paste0(v_scenario_number[el], "_late_no OS.csv")]]["splittingModSeR_IE_w_share"] <-
#       l_results[[paste0(v_scenario_number[el], "_late_no OS.csv")]][,"ModSE"]/
#       l_results[[paste0(v_scenario_number[el], "_late_with OS.csv")]][row_index_IE_w_share,"ModSE"]
#   } else {
#     # for sets 5 and 6 (not that sets 7 and 8 always have OS in the target by definition)
#     # Sets 5 and 6 have no OS in the target by definition
# 
#     l_results[[paste0(v_scenario_number[el], "_early_no OS.csv")]]["splittingModSeR_IE_w_share"] <-
#       l_results[[paste0(v_scenario_number[el], "_early_no OS.csv")]][,"ModSE"]/
#       # if 6050 has no os, 8050 is the same with os. Similarly, if 5050 has no os, 7050 has os
#       l_results[[paste0(v_scenario_number[el] + 2000, "_early_with OS.csv")]][row_index_IE_w_share,"ModSE"]
# 
#     l_results[[paste0(v_scenario_number[el], "_mid_no OS.csv")]]["splittingModSeR_IE_w_share"] <-
#       l_results[[paste0(v_scenario_number[el], "_mid_no OS.csv")]][,"ModSE"]/
#       l_results[[paste0(v_scenario_number[el] + 2000, "_mid_with OS.csv")]][row_index_IE_w_share,"ModSE"]
# 
#     l_results[[paste0(v_scenario_number[el], "_late_no OS.csv")]]["splittingModSeR_IE_w_share"] <-
#       l_results[[paste0(v_scenario_number[el], "_late_no OS.csv")]][,"ModSE"]/
#       l_results[[paste0(v_scenario_number[el] + 2000, "_late_with OS.csv")]][row_index_IE_w_share,"ModSE"]
#   }
# 
#   cat('\r', paste0(round(el/length(v_scenario_number)*100,0), "% complete"))
# }


# This is where everything else is stored
# google_drive_path <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/"
# saveRDS(l_results, file= paste0(google_drive_path, "/l_results.RData"))
# to read
# l_results <- readRDS(paste0(google_drive_path, "/l_results.RData"))


## Main paper #####

### Uni non mix late #####
fn_plot_3x2_multiplot_spread(level_of_evidence = "late", 
                      v_models = c("uni_ie",
                                   "uni_ie_w_share",
                                   "uni_ce",
                                   "uni_ce_w_share",
                                   "uni_re",            
                                   "uni_re_w_share"),    
                      v_model_OS_in_target = c(TRUE, TRUE,     # IE independent, common, with OS
                                               TRUE, TRUE,     # CE independent, common, with OS
                                               TRUE, TRUE),     # RE independent, common, with OS
                      v_model_col = c( # boarder color
                        "black", 
                        "black",
                        "indianred", # CE indpendent: red boarder, white background
                        "black", # CE common: black boarder, red background
                        "springgreen3", # RE indpendent: green boarder, white background
                        "black"), # RE common: black boarder, green background
                      v_model_pch = c(1,       # IE independent
                                      21,      # IE common
                                      1,       # CE independent, red boarder, white background
                                      21,      # CE common, black boarder, red background
                                      1,     # RE independent, green boarder, white background
                                      21),      # RE common, black boarder, green background
                      v_model_bg = c( # BACKGROUND COLOUR
                        "black", 
                        "black",
                        "indianred", # "white",# CE indpendent: red boarder, white background
                        "indianred",# CE common: black boarder, red background
                        "springgreen3", # "white",# RE indpendent: red boarder, white background
                        "springgreen3"),# RE common: black boarder, red background
                      v_model_legend_name = c(expression(paste("IP", tau, "j")),
                                              expression(paste("IP", tau)),
                                              expression(paste("CP", tau, "j")),
                                              expression(paste("CP", tau)),
                                              expression(paste("RP", tau, "j")),
                                              expression(paste("RP", tau))),
                      v_metric = c("Bias", "Cover", "splittingModSeR_IE_w_share"),
                      v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                      v_metric_dotted_line = c(0, 0.95, 1),
                      ylim_for_row1 = c(-0.02, 0.2),
                      ylim_for_row2 = c(0.2, 1),
                      ylim_for_row3 = c(0, 3),
                      main_for_col1 = "No divergent indication",  
                      main_for_col2 = "One extremily divergent indication",
                      simulation_set_number_col1 = 1,
                      simulation_set_number_col2 = 3
)

### Uni non mix early #####
fn_plot_3x2_multiplot_spread(level_of_evidence = "early", 
                      v_models = c("uni_ie",
                                   "uni_ie_w_share",
                                   "uni_ce",
                                   "uni_ce_w_share",
                                   "uni_re",            
                                   "uni_re_w_share"),    
                      v_model_OS_in_target = c(TRUE, TRUE,     # IE independent, common, with OS
                                               TRUE, TRUE,     # CE independent, common, with OS
                                               TRUE, TRUE),     # RE independent, common, with OS
                      v_model_col = c( # boarder color
                        "black", 
                        "black",
                        "indianred", # CE indpendent: red boarder, white background
                        "black", # CE common: black boarder, red background
                        "springgreen3", # RE indpendent: green boarder, white background
                        "black"), # RE common: black boarder, green background
                      v_model_pch = c(1,       # IE independent
                                      21,      # IE common
                                      1,       # CE independent, red boarder, white background
                                      21,      # CE common, black boarder, red background
                                      1,     # RE independent, green boarder, white background
                                      21),      # RE common, black boarder, green background
                      v_model_bg = c( # BACKGROUND COLOUR
                        "black", 
                        "black",
                        "indianred", # "white",# CE indpendent: red boarder, white background
                        "indianred",# CE common: black boarder, red background
                        "springgreen3", # "white",# RE indpendent: red boarder, white background
                        "springgreen3"),# RE common: black boarder, red background
                      v_model_legend_name = c(expression(paste( "IP", tau, "j")),
                                              expression(paste( "IP", tau)),
                                              expression(paste( "CP", tau, "j")),
                                              expression(paste( "CP", tau)),
                                              expression(paste( "RP", tau, "j")),
                                              expression(paste( "RP", tau))),
                      v_metric = c("Bias", "Cover", "splittingModSeR_IE_w_share"),
                      v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                      v_metric_dotted_line = c(0, 0.95, 1),
                      ylim_for_row1 = c(-0.02, 0.2),
                      ylim_for_row2 = c(0.2, 1),
                      ylim_for_row3 = c(0, 3),
                      main_for_col1 = "No divergent indication",  
                      main_for_col2 = "One extremily divergent indication",
                      simulation_set_number_col1 = 1,
                      simulation_set_number_col2 = 3
)

### Uni mix late #####
fn_plot_1x2_multiplot_spread(level_of_evidence = "late", 
                      v_models = c("uni_re_w_share",
                                   "uni_mrie_w_share",
                                   "uni_ce_w_share",
                                   "uni_mcie_w_share"),
                      v_model_OS_in_target = rep(TRUE, 5),     
                      v_model_col = c(
                        "black",
                        "springgreen3",
                        "black",
                        "indianred"),
                      v_model_bg = c(
                        "springgreen3",
                        "springgreen3",
                        "indianred",
                        "indianred"),
                      v_model_pch = c(21, 15, 21, 15),
                      v_model_legend_name = c(expression(paste(  "RP", tau)),
                                              expression(paste( "MRIP", tau)),
                                              expression(paste( "CP", tau)),
                                              expression(paste( "MCIP", tau))
                      ),
                      v_metric = c("Bias"),
                      v_metric_names = c("Bias"), 
                      v_metric_dotted_line = c(0),
                      ylim_for_row1 = c(-0.02, 0.2),
                      main_for_col1 = "One moderately divergent indication",  
                      main_for_col2 = "One extremily divergent indication",
                      simulation_set_number_col1 = 2,
                      simulation_set_number_col2 = 3
)

### Uni mix mid #####
fn_plot_1x2_multiplot_spread(level_of_evidence = "mid", 
                      v_models = c("uni_re_w_share",
                                   "uni_mrie_w_share",
                                   "uni_ce_w_share",
                                   "uni_mcie_w_share"),
                      v_model_OS_in_target = rep(TRUE, 5),     
                      v_model_col = c(
                        "black",
                        "springgreen3",
                        "black",
                        "indianred"),
                      v_model_bg = c(
                        "springgreen3",
                        "springgreen3",
                        "indianred",
                        "indianred"),
                      v_model_pch = c(21, 15, 21, 15),
                      v_model_legend_name = c(expression(paste(  "RP", tau)),
                                              expression(paste( "MRIP", tau)),
                                              expression(paste( "CP", tau)),
                                              expression(paste( "MCIP", tau))
                      ),
                      v_metric = c("Bias"),
                      v_metric_names = c("Bias"), 
                      v_metric_dotted_line = c(0),
                      ylim_for_row1 = c(-0.02, 0.2),
                      main_for_col1 = "One moderately divergent indication",  
                      main_for_col2 = "One extremily divergent indication",
                      simulation_set_number_col1 = 2,
                      simulation_set_number_col2 = 3
)

### Uni mix early #####
fn_plot_1x2_multiplot_spread(level_of_evidence = "early", 
                      v_models = c("uni_re_w_share",
                                   "uni_mrie_w_share",
                                   "uni_ce_w_share",
                                   "uni_mcie_w_share"),
                      v_model_OS_in_target = rep(TRUE, 5),     
                      v_model_col = c(
                        "black",
                        "springgreen3",
                        "black",
                        "indianred"),
                      v_model_bg = c(
                        "springgreen3",
                        "springgreen3",
                        "indianred",
                        "indianred"),
                      v_model_pch = c(21, 15, 21, 15),
                      v_model_legend_name = c(expression(paste(  "RP", tau)),
                                              expression(paste( "MRIP", tau)),
                                              expression(paste( "CP", tau)),
                                              expression(paste( "MCIP", tau))
                      ),
                      v_metric = c("Bias"),
                      v_metric_names = c("Bias"), 
                      v_metric_dotted_line = c(0),
                      ylim_for_row1 = c(-0.02, 0.20),
                      main_for_col1 = "One moderately divergent indication",  
                      main_for_col2 = "One extremily divergent indication",
                      simulation_set_number_col1 = 2,
                      simulation_set_number_col2 = 3
)

### Bi unmatched late #####
# One extreme outlier vs target outlier with OS and no noise
fn_plot_3x2_multiplot_spread(level_of_evidence = "late", 
                      v_models = c("uni_ie_w_share",
                                   "uni_re_w_share",
                                   "bi_ce_unmatched_w_share",
                                   "bi_re_unmatched_w_share"),
                      v_model_OS_in_target = c(TRUE,     
                                               TRUE,      
                                               TRUE, TRUE),  
                      # boarder colour
                      v_model_col = c("black", 
                                      "black",
                                      "indianred",
                                      "springgreen3"),
                      v_model_pch = c(19, # ie uni common
                                      21, # re uni common  
                                      17, # bi common   
                                      17),# bi common    
                      # fill colour
                      v_model_bg = c("black", 
                                     "springgreen3",
                                     "indianred",
                                     "springgreen3"),
                      v_model_legend_name = c(expression(paste("IP", tau)),
                                              paste0("RP\U1D70F"),                    
                                              paste0("Bi-CP\U1D70F \n", "unmatched"),  
                                              paste0("Bi-RP\U1D70F \n", "unmatched")), 
                      v_metric = c("Bias", "Cover", "splittingModSeR_IE_w_share"),
                      v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                      v_metric_dotted_line = c(0, 0.95, 1),
                      ylim_for_row1 = c(-0.15, 0.07),
                      ylim_for_row2 = c(0.7, 1),
                      ylim_for_row3 = c(0, 1.5),
                      main_for_col1 = "One extremily divergent indication",
                      main_for_col2 = "Moderately divergent target indication",
                      simulation_set_number_col1 = 3,
                      simulation_set_number_col2 = 7
)

### Bi unmatched early #####
# One extreme outlier vs target outlier with OS and no noise
fn_plot_3x2_multiplot_spread(level_of_evidence = "early", 
                      v_models = c("uni_ie_w_share",
                                   "uni_re_w_share",
                                   "bi_ce_unmatched_w_share",
                                   "bi_re_unmatched_w_share"),
                      v_model_OS_in_target = c(TRUE,     
                                               TRUE,      
                                               TRUE, TRUE),  
                      # boarder colour
                      v_model_col = c("black", 
                                      "black",
                                      "indianred",
                                      "springgreen3"),
                      v_model_pch = c(19, # ie uni common
                                      21, # re uni common  
                                      17, # bi common   
                                      17),# bi common    
                      # fill colour
                      v_model_bg = c("black", 
                                     "springgreen3",
                                     "indianred",
                                     "springgreen3"),
                      v_model_legend_name = c(expression(paste(  "IP", tau)),
                                              paste0("RP\U1D70F"),                    
                                              paste0("Bi-CP\U1D70F \n", "unmatched"),  
                                              paste0("Bi-RP\U1D70F \n", "unmatched")), 
                      v_metric = c("Bias", "Cover",  "splittingModSeR_IE_w_share"),
                      v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                      v_metric_dotted_line = c(0, 0.95, 1),
                      ylim_for_row1 = c(-0.15, 0.07),
                      ylim_for_row2 = c(0.7, 1),
                      ylim_for_row3 = c(0, 1.5),
                      main_for_col1 = "One extremily divergent indication",
                      main_for_col2 = "Moderately divergent target indication",
                      simulation_set_number_col1 = 3,
                      simulation_set_number_col2 = 7
)


# Generate and save all results for appendix #######
# save results to local machine (required for automatic document generation)

## Path to Davids local drive
# This is where images will be stored and report will be generated
# must be on LOCAL drive because cant create automatic documents on syched google drive!
david_local_path <- "C:/MultiIndication_results/"
figures_location <- paste0(david_local_path, "Appendix figures/")

# define plots to generate (when there is OS in the target)
Target_outlier = c(FALSE)
OS = c("with_os", "no_os")
Model = c("uni_nonmix", "uni_mix", "bi_unmatched", "bi_matched")
data_time = c("late", "mid", "early")
df_graphs_to_generate <- expand.grid(
  Target_outlier, OS, Model, data_time
)
names(df_graphs_to_generate) <- c("Target_outlier", "OS", "Model", "data_time")

# add the plots when there is no OS in the target
Target_outlier = c(TRUE)
OS = c("with_os", "no_os")
Model = c("uni_nonmix", "bi_unmatched", "bi_matched")
data_time = c(NA)
df_graphs_to_generate_no_OS <- expand.grid(
  Target_outlier, OS, Model, data_time
)
names(df_graphs_to_generate_no_OS) <- c("Target_outlier", "OS", "Model", "data_time")

# add to main dataframe
df_graphs_to_generate <- rbind(df_graphs_to_generate,
                               df_graphs_to_generate_no_OS)

# set up ploting 2000 x 1600 and res = 200
width <- 2000
height <- 1600

for(g in 1:nrow(df_graphs_to_generate)){
  
  # model set
  v_models_set <- if(df_graphs_to_generate$Model[g] == "uni_nonmix"){
    c("uni_ie",
      "uni_ie_w_share",
      "uni_ce",
      "uni_ce_w_share",
      "uni_re",            
      "uni_re_w_share")
  } else if(df_graphs_to_generate$Model[g] == "uni_mix"){
    c("uni_mrie",
      "uni_mrie_w_share",
      "uni_mcie",
      "uni_mcie_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_unmatched"){
    c("bi_ce_unmatched",        
      "bi_ce_unmatched_w_share",
      "bi_re_unmatched",        
      "bi_re_unmatched_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_matched"){
    c("bi_ce_matched",        
      "bi_ce_matched_w_share",
      "bi_re_matched",        
      "bi_re_matched_w_share")
  }
  
  # model colour set
  v_models_col_set <- if(df_graphs_to_generate$Model[g] == "uni_nonmix"){
    c("black", # "uni_ie",
      "black", #  "uni_ie_w_share",
      "indianred", # "uni_ce",
      "indianred", # "uni_ce_w_share",
      "springgreen3", # "uni_re",            
      "springgreen3") # "uni_re_w_share")
  } else if(df_graphs_to_generate$Model[g] == "uni_mix"){
    c("springgreen3" , # "uni_mrie",
      "springgreen3", # "uni_mrie_w_share",
      "indianred", # "uni_mcie",
      "indianred") # "uni_mcie_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_unmatched"){
    c("indianred", # "bi_ce_unmatched",        
      "indianred", # "bi_ce_unmatched_w_share",
      "springgreen3", # "bi_re_unmatched",        
      "springgreen3") # "bi_re_unmatched_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_matched"){
    c("indianred", # "bi_ce_matched",        
      "indianred", # "bi_ce_matched_w_share",
      "springgreen3", # "bi_re_matched",        
      "springgreen3") # "bi_re_matched_w_share")
  }
  
  # model bg set
  # (boarder and background the same colour)
  v_models_bg_set <- v_models_col_set
  
  # model pch set
  v_models_pch_set <- if(df_graphs_to_generate$Model[g] == "uni_nonmix"){
    c(1, # "uni_ie",
      19, # "uni_ie_w_share",
      1, # "uni_ce",
      19, # "uni_ce_w_share",
      1, # "uni_re",            
      19) # "uni_re_w_share")
  } else if(df_graphs_to_generate$Model[g] == "uni_mix"){
    c(0, # "uni_mrie",
      15, # "uni_mrie_w_share",
      0, # "uni_mcie",
      15) # "uni_mcie_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_unmatched"){
    c(2, # "bi_ce_unmatched",        
      17, # "bi_ce_unmatched_w_share",
      2, # "bi_re_unmatched",        
      17) # "bi_re_unmatched_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_matched"){
    c(2, # "bi_ce_matched",        
      17, # "bi_ce_matched_w_share",
      2, # "bi_re_matched",        
      17) # "bi_re_matched_w_share")
  }
  
  # model names set
  v_models_names_set <- if(df_graphs_to_generate$Model[g] == "uni_nonmix"){
    c(expression(paste(  "IP", tau, "j")),
      expression(paste(  "IP", tau)),
      expression(paste(  "CP", tau, "j")),
      expression(paste(  "CP", tau)),
      expression(paste(  "RP", tau, "j")),
      expression(paste(  "RP", tau)))
  } else if(df_graphs_to_generate$Model[g] == "uni_mix"){
    c(expression(paste(  "MRIP", tau, "j")), # "uni_mrie",
      expression(paste(  "MRIE", tau)), # "uni_mrie_w_share",
      expression(paste(  "MCIP", tau, "j")), # "uni_mcie",
      expression(paste(  "MCIP", tau))) #  "uni_mcie_w_share")
  }else if(df_graphs_to_generate$Model[g] == "bi_unmatched"){
    c(paste0("Bi-CP\U1D70F", "j \n", "unmatched"),                      
    paste0("Bi-CP\U1D70F \n", "unmatched"), 
    paste0("Bi-RP\U1D70F", "j \n", "unmatched", "j"), 
    paste0("Bi-RP\U1D70F \n", "unmatched"))
  }else if(df_graphs_to_generate$Model[g] == "bi_matched"){
    c(paste0("Bi-CP\U1D70F", "j \n", "matched"),                      
    paste0("Bi-CP\U1D70F \n", "matched"), 
    paste0("Bi-RP\U1D70F", "j \n", "matched", "j"), 
    paste0("Bi-RP\U1D70F \n", "matched"))
  }
  
  # remove IP models from uni_nonmix when there is no os in the target
  if(df_graphs_to_generate$OS[g] == "no_os" & 
     df_graphs_to_generate$Model[g] == "uni_nonmix"){
    v_models_set <- v_models_set[-c(1,2)]
    v_models_col_set <- v_models_col_set[-c(1,2)]
    v_models_pch_set <- v_models_pch_set[-c(1,2)]
    v_models_names_set <- v_models_names_set[-c(1,2)]
  }
  
  if(df_graphs_to_generate$Target_outlier[g] == FALSE){
    
    ## target indication is not an outlier
    # create graph
    png(paste0(figures_location , # save location 
               paste0(
                 df_graphs_to_generate$OS[g], "_",
                 df_graphs_to_generate$Model[g], "_",
                 df_graphs_to_generate$data_time[g]
               ),
               ".png"),
        width = width, 
        height = height,
        res = 200)
    # generate plot
    fn_plot_4x3_multiplot(level_of_evidence = df_graphs_to_generate$data_time[g], 
                          v_models = v_models_set,    
                          v_model_OS_in_target = rep(df_graphs_to_generate$OS[g] == "with_os", length(v_models_set)),   
                          v_model_col = v_models_col_set, 
                          v_model_pch = v_models_pch_set,
                          v_model_bg = v_models_bg_set,
                          v_model_legend_name = v_models_names_set,
                          v_metric = c("Bias", "Cover", "EmpSE", "splittingModSeR_IE_w_share"),
                          v_metric_names = c("Bias", "Coverage", "Empirical SE", "Splitting SE ratio"), 
                          v_metric_dotted_line = c(0, 0.95, NA, 1),
                          ylim_for_row1 = c(-0.02, 0.2), # bias
                          ylim_for_row2 = c(0, 1), # coverage
                          ylim_for_row3 = c(0, 0.2), # empse
                          ylim_for_row4 = c(0, 3.5), # splitting SE ratio
                          main_for_col1 = "No outlier",  
                          main_for_col2 = "One moderate outlier",
                          main_for_col3 = "One extreme outlier",
                          simulation_set_number_col1 = 1, # no outlier, no noise 
                          simulation_set_number_col2 = 2, # moderate outlier, no noise
                          simulation_set_number_col3 = 3 # extreme outlier, no noise
    )
    
  } else {
    
    ## target indication is an outlier
    
    # create graph
    png(paste0(figures_location , # save location 
               paste0(
                 "target_outlier_",
                 df_graphs_to_generate$OS[g], "_",
                 df_graphs_to_generate$Model[g]
               ),
               ".png"),
        width = width, height = height,
        res = 200)
    # generate plot
    fn_plot_4x3_multiplot_time_points(v_models = v_models_set,    
                          v_model_OS_in_target = rep(df_graphs_to_generate$OS[g] == "with_os", length(v_models_set)),   
                          v_model_col = v_models_col_set, 
                          v_model_pch = v_models_pch_set,
                          v_model_bg = v_models_bg_set,
                          v_model_legend_name = v_models_names_set,
                          v_metric = c("Bias", "Cover", "EmpSE", "splittingModSeR_IE_w_share"),
                          v_metric_names = c("Bias", "Coverage", "Empirical SE", "Splitting SE ratio"), 
                          v_metric_dotted_line = c(0, 0.95, NA, 1),
                          ylim_for_row1 = c(-0.20, 0.02), # bias
                          ylim_for_row2 = c(0, 1), # coverage
                          ylim_for_row3 = c(0, 0.2), # empse
                          ylim_for_row4 = c(0, 3.5), # splitting SE ratio
                          main_for_col1 = "Small dataset",  
                          main_for_col2 = "Medium dataset",
                          main_for_col3 = "Large dataset",
                          simulation_set_number = ifelse(df_graphs_to_generate$OS[g] == "with_os", 7, 5)
    )
  }
  
  dev.off()
  cat('\r', paste0(round(g/nrow(df_graphs_to_generate)*100,0), "% complete"))
}



# Additional results #####

### Matched surrogate #### 

#### Bi matched late #####
# One extreme outlier vs target outlier with OS and no noise
fn_plot_3x2_multiplot_spread(level_of_evidence = "late", 
                             v_models = c("uni_ie_w_share",
                                          "uni_re_w_share",
                                          "bi_ce_matched_w_share",
                                          "bi_re_matched_w_share"),
                             v_model_OS_in_target = c(TRUE,     
                                                      TRUE,      
                                                      TRUE, TRUE),  
                             # boarder colour
                             v_model_col = c("black", 
                                             "black",
                                             "indianred",
                                             "springgreen3"),
                             v_model_pch = c(19, # ie uni common
                                             21, # re uni common  
                                             17, # bi common   
                                             17),# bi common    
                             # fill colour
                             v_model_bg = c("black", 
                                            "springgreen3",
                                            "indianred",
                                            "springgreen3"),
                             v_model_legend_name = c(expression(paste("IP", tau)),
                                                     paste0("RP\U1D70F"),                    
                                                     paste0("Bi-CP\U1D70F \n", "matched"),  
                                                     paste0("Bi-RP\U1D70F \n", "matched")), 
                             v_metric = c("Bias", "Cover", "splittingModSeR_IE_w_share"),
                             v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                             v_metric_dotted_line = c(0, 0.95, 1),
                             ylim_for_row1 = c(-0.15, 0.07),
                             ylim_for_row2 = c(0.7, 1),
                             ylim_for_row3 = c(0, 1.5),
                             main_for_col1 = "One extremily divergent indication",
                             main_for_col2 = "Moderately divergent target indication",
                             simulation_set_number_col1 = 3,
                             simulation_set_number_col2 = 7
)

#### Bi matched early #####
# One extreme outlier vs target outlier with OS and no noise
fn_plot_3x2_multiplot_spread(level_of_evidence = "early", 
                             v_models = c("uni_ie_w_share",
                                          "uni_re_w_share",
                                          "bi_ce_matched_w_share",
                                          "bi_re_matched_w_share"),
                             v_model_OS_in_target = c(TRUE,     
                                                      TRUE,      
                                                      TRUE, TRUE),  
                             # boarder colour
                             v_model_col = c("black", 
                                             "black",
                                             "indianred",
                                             "springgreen3"),
                             v_model_pch = c(19, # ie uni common
                                             21, # re uni common  
                                             17, # bi common   
                                             17),# bi common    
                             # fill colour
                             v_model_bg = c("black", 
                                            "springgreen3",
                                            "indianred",
                                            "springgreen3"),
                             v_model_legend_name = c(expression(paste(  "IP", tau)),
                                                     paste0("RP\U1D70F"),                    
                                                     paste0("Bi-CP\U1D70F \n", "matched"),  
                                                     paste0("Bi-RP\U1D70F \n", "matched")), 
                             v_metric = c("Bias", "Cover",  "splittingModSeR_IE_w_share"),
                             v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                             v_metric_dotted_line = c(0, 0.95, 1),
                             ylim_for_row1 = c(-0.15, 0.07),
                             ylim_for_row2 = c(0.7, 1),
                             ylim_for_row3 = c(0, 1.5),
                             main_for_col1 = "One extremily divergent indication",
                             main_for_col2 = "Moderately divergent target indication",
                             simulation_set_number_col1 = 3,
                             simulation_set_number_col2 = 7
)



### no OS surrogate #####

#### Bi unmatched late - no OS #####
# One extreme outlier vs target outlier with OS and no noise
fn_plot_3x2_multiplot_spread(level_of_evidence = "late", 
                             v_models = c(#"uni_ie_w_share",
                                          "uni_re_w_share",
                                          "bi_ce_unmatched_w_share",
                                          "bi_re_unmatched_w_share"),
                             v_model_OS_in_target = c(# FALSE,     
                                                      FALSE,      
                                                      FALSE, FALSE),  
                             # boarder colour
                             v_model_col = c(#"black", 
                                             "black",
                                             "indianred",
                                             "springgreen3"),
                             v_model_pch = c(#19, # ie uni common
                                             21, # re uni common  
                                             17, # bi common   
                                             17),# bi common    
                             # fill colour
                             v_model_bg = c(#"black", 
                                            "springgreen3",
                                            "indianred",
                                            "springgreen3"),
                             v_model_legend_name = c(#expression(paste("IP", tau)),
                                                     paste0("RP\U1D70F"),                    
                                                     paste0("Bi-CP\U1D70F \n", "unmatched"),  
                                                     paste0("Bi-RP\U1D70F \n", "unmatched")), 
                             v_metric = c("Bias", "Cover", "splittingModSeR_IE_w_share"),
                             v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                             v_metric_dotted_line = c(0, 0.95, 1),
                             ylim_for_row1 = c(-0.15, 0.07),
                             ylim_for_row2 = c(0.7, 1),
                             ylim_for_row3 = c(0, 1.5),
                             main_for_col1 = "One extremily divergent indication",
                             main_for_col2 = "Moderately divergent target indication",
                             simulation_set_number_col1 = 3,
                             simulation_set_number_col2 = 5
)

#### Bi unmatched early - no OS #####
# One extreme outlier vs target outlier with OS and no noise
fn_plot_3x2_multiplot_spread(level_of_evidence = "early", 
                             v_models = c(#"uni_ie_w_share",
                                          "uni_re_w_share",
                                          "bi_ce_unmatched_w_share",
                                          "bi_re_unmatched_w_share"),
                             v_model_OS_in_target = c(#FALSE,     
                                                      FALSE,      
                                                      FALSE, FALSE),  
                             # boarder colour
                             v_model_col = c(#"black", 
                                             "black",
                                             "indianred",
                                             "springgreen3"),
                             v_model_pch = c(#19, # ie uni common
                                             21, # re uni common  
                                             17, # bi common   
                                             17),# bi common    
                             # fill colour
                             v_model_bg = c(#"black", 
                                            "springgreen3",
                                            "indianred",
                                            "springgreen3"),
                             v_model_legend_name = c(#expression(paste(  "IP", tau)),
                                                     paste0("RP\U1D70F"),                    
                                                     paste0("Bi-CP\U1D70F \n", "unmatched"),  
                                                     paste0("Bi-RP\U1D70F \n", "unmatched")), 
                             v_metric = c("Bias", "Cover",  "splittingModSeR_IE_w_share"),
                             v_metric_names = c("Bias", "Coverage", "Splitting SE ratio"), 
                             v_metric_dotted_line = c(0, 0.95, 1),
                             ylim_for_row1 = c(-0.15, 0.07),
                             ylim_for_row2 = c(0.7, 1),
                             ylim_for_row3 = c(0, 1.5),
                             main_for_col1 = "One extremily divergent indication",
                             main_for_col2 = "Moderately divergent target indication",
                             simulation_set_number_col1 = 3,
                             simulation_set_number_col2 = 5
)




# MiM presentation #######





## Bi unmatched late (alt) #####
fn_plot_3x2_multiplot(level_of_evidence = "late", 
                      v_models = c("uni_ie_w_share",
                                   "bi_ce_unmatched_w_share"),
                      v_model_OS_in_target = c(TRUE,       
                                               TRUE),  
                      # boarder colour
                      v_model_col = c("black", 
                                      "indianred"),
                      v_model_pch = c(19, # ie uni common
                                      17),# bi common    
                      # fill colour
                      v_model_bg = c("black", 
                                     "indianred"),
                      v_model_legend_name = c(expression(paste("IP", tau)),
                                              paste0("Bi-CP\U1D70F \n", "unmatched")), 
                      v_metric = c("Bias", "Cover", "ModSE"),
                      v_metric_names = c("Bias", "Coverage", "SE"), 
                      v_metric_dotted_line = c(0, 0.95, NA),
                      ylim_for_row1 = c(-0.02, 0.04),
                      ylim_for_row2 = c(0.9, 1),
                      ylim_for_row3 = c(0, 0.2),
                      main_for_col1 = "One extreme outlier", 
                      main_for_col2 = "Target outlier",
                      simulation_set_number_col1 = 3,
                      simulation_set_number_col2 = 7
)

fn_plot_3x2_multiplot(level_of_evidence = "late", 
                      v_models = c("uni_ie_w_share",
                                   "bi_ce_unmatched_w_share"),
                      v_model_OS_in_target = c(TRUE,       
                                               TRUE),  
                      # boarder colour
                      v_model_col = c("black", 
                                      "indianred"),
                      v_model_pch = c(19, # ie uni common
                                      17),# bi common    
                      # fill colour
                      v_model_bg = c("black", 
                                     "indianred"),
                      v_model_legend_name = c(expression(paste("IP", tau)),
                                              paste0("Bi-CP\U1D70F \n", "unmatched")), 
                      v_metric = c("Bias", "Cover", "ModSE"),
                      v_metric_names = c("Bias", "Coverage", "SE"), 
                      v_metric_dotted_line = c(0, 0.95, NA),
                      ylim_for_row1 = c(-0.02, 0.04),
                      ylim_for_row2 = c(0.9, 1),
                      ylim_for_row3 = c(0, 0.2),
                      main_for_col1 = "No outlier", 
                      main_for_col2 = "No outlier",
                      simulation_set_number_col1 = 1,
                      simulation_set_number_col2 = 1
)


## Bi unmatched early (alt) #####
fn_plot_3x2_multiplot(level_of_evidence = "early", 
                      v_models = c("uni_ie_w_share",
                                   "bi_ce_unmatched_w_share"),
                      v_model_OS_in_target = c(TRUE,       
                                               TRUE),  
                      # boarder colour
                      v_model_col = c("black", 
                                      "indianred"),
                      v_model_pch = c(19, # ie uni common
                                      17),# bi common    
                      # fill colour
                      v_model_bg = c("black", 
                                     "indianred"),
                      v_model_legend_name = c(expression(paste("IP", tau)),
                                              paste0("Bi-CP\U1D70F \n", "unmatched")), 
                      v_metric = c("Bias", "Cover", "ModSE"),
                      v_metric_names = c("Bias", "Coverage", "SE"), 
                      v_metric_dotted_line = c(0, 0.95, NA),
                      ylim_for_row1 = c(-0.02, 0.04),
                      ylim_for_row2 = c(0.9, 1),
                      ylim_for_row3 = c(0, 0.2),
                      main_for_col1 = "One extreme outlier", 
                      main_for_col2 = "Target outlier",
                      simulation_set_number_col1 = 3,
                      simulation_set_number_col2 = 7
)


fn_plot_3x2_multiplot(level_of_evidence = "early", 
                      v_models = c("uni_ie_w_share",
                                   "bi_ce_unmatched_w_share"),
                      v_model_OS_in_target = c(TRUE,       
                                               TRUE),  
                      # boarder colour
                      v_model_col = c("black", 
                                      "indianred"),
                      v_model_pch = c(19, # ie uni common
                                      17),# bi common    
                      # fill colour
                      v_model_bg = c("black", 
                                     "indianred"),
                      v_model_legend_name = c(expression(paste("IP", tau)),
                                              paste0("Bi-CP\U1D70F \n", "unmatched")), 
                      v_metric = c("Bias", "Cover", "ModSE"),
                      v_metric_names = c("Bias", "Coverage", "SE"), 
                      v_metric_dotted_line = c(0, 0.95, NA),
                      ylim_for_row1 = c(-0.02, 0.04),
                      ylim_for_row2 = c(0.9, 1),
                      ylim_for_row3 = c(0, 0.2),
                      main_for_col1 = "No outlier", 
                      main_for_col2 = "No outlier",
                      simulation_set_number_col1 = 1,
                      simulation_set_number_col2 = 1
)

# ## Old graphs ######
# 
# ### Plots showing model average values over all simulations #####
# 
# # list of metrics which we want to plot
# v_metrics_to_plot_values <- c("Bias", "Cover", "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                        "splittingModSeR_IE", "PED_IE", "splittingModSeR_IE_w_share", "PED_IE_w_share", "MSE", "EmpSE", "ModSE", "MAE", "splittingModSeR_obs",
#                        "Prop_Rhat_1.1_option1", "Prop_Rhat_1.1_option2",
#                        "Num_target_posterior_exist", "Num_underlying_dataset_exist", "Num_Rhat_exist"
#                        )
# # how many decimal places for each metric (in the order above)
# v_decimal_places <- c(2, 2, 1,
#                       2, 2, 2, 2, 2, 2, 3, 2, 2,
#                       2, 2, 
#                       3, 3, 3)
# # check
# length(v_metrics_to_plot_values) == length(v_decimal_places)
# 
# # loop over the 6 sets of simulations
# for(sim_set in 1:6){
#   print(paste("Begin set", sim_set))
#   # loop over metrics and plot early,mid,late for all models, effect sizes etc
#   for(index in 1:length(v_metrics_to_plot_values)){
#     fn_CV_result_loop_plot(simulation_set_number = sim_set,
#                            metric = v_metrics_to_plot_values[index], 
#                            decimal_places = v_decimal_places[index], 
#                            CI_interval = FALSE,
#                            figures_location = paste0(david_local_path, 
#                                                      "Figures/Model_values/Set", sim_set, "/"),
#                            google_drive_path = google_drive_path)
#     cat('\r', paste0(round(index/length(v_metrics_to_plot_values)*100,0), "% complete"))
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# ### Plot individual datasets
# 
# user <-   "David"
# source("SimulationSetUp.R")
# results_location <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/Viking simulation results/Datasets/Data/"
# results_location <- "G:/Shared drives/MRC multi-indication/4 WP3/Multi Indication R/Simulation study/Desktop simulation results David/Datasets/Data/"
# 
# p <- "1.50"
# i <- 950
# fn_plot_full_dataset(df_data_set = read.csv(paste0(results_location,
#                                                    "df_data_set_", p, "_i", i, ".csv")),
#                      plot_title = paste0(p ,", iteration ", i))
# 
# 
# 
# ### Intrepret changes on log scale
# 
# x_axis <- seq(from = -0.1, to = 0.1, length.out = 100)
# y_axis <- exp(log(0.6) + x_axis)
# plot(x = x_axis, y = y_axis, type = "l", ylim = c(0.5, 1),
#      col = "red", main = "Intrepret changes in LHR",
#      xlab = "Change in LHR", ylab = "Hazard ratio")
# lines(x = x_axis, y=  exp(log(0.8) + x_axis), col = "blue")
# abline(v = 0)
# legend("topleft", legend=c("Small effect HR 0.8", "Large effect HR 0.6"), 
#        lty = c(1, 1),  col= c("blue", "red"))
# 
# ## Main paper OLD
# 
# ### univariate non-mixture multiplots
# # first late then early
# 
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_ie",
#                "uni_ie_w_share",
#                "uni_ce",
#                "uni_ce_w_share",
#                "uni_re",            
#                "uni_re_w_share"),    
#   v_model_OS_in_target = c(TRUE, TRUE,     # IE independent, common, with OS
#                            TRUE, TRUE,     # CE independent, common, with OS
#                            TRUE, TRUE),     # RE independent, common, with OS
#   v_model_col = c(
#     "black", "black",
#     "indianred","indianred",
#     "springgreen3","springgreen3"),
#   v_model_pch = c(1, 19,      # IE independent, common, with OS
#                   1, 19,      # CE independent, common, with OS
#                   1, 19),      # RE independent, common, with OS
#   v_model_legend_name = c(expression(paste("Univariate ", "IP", tau, "j")),
#                           expression(paste("Univariate ", "IP", tau)),
#                           expression(paste("Univariate ", "CP", tau, "j")),
#                           expression(paste("Univariate ", "CP", tau)),
#                           expression(paste("Univariate ", "RP", tau, "j")),
#                           expression(paste("Univariate ", "RP", tau))),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 2.5)
# )
# fn_plot_multiplot(
#   level_of_evidence = "early",
#   v_models = c("uni_ie",
#                "uni_ie_w_share",
#                "uni_ce",
#                "uni_ce_w_share",
#                "uni_re",            
#                "uni_re_w_share"),    
#   v_model_OS_in_target = c(TRUE, TRUE,     # IE independent, common, with OS
#                            TRUE, TRUE,     # CE independent, common, with OS
#                            TRUE, TRUE),     # RE independent, common, with OS
#   v_model_col = c(
#     "black", "black",
#     "indianred","indianred",
#     "springgreen3","springgreen3"),
#   v_model_pch = c(1, 19,      # IE independent, common, with OS
#                   1, 19,      # CE independent, common, with OS
#                   1, 19),      # RE independent, common, with OS
#   v_model_legend_name = c(expression(paste("Univariate ", "IP", tau, "j")),
#                           expression(paste("Univariate ", "IP", tau)),
#                           expression(paste("Univariate ", "CP", tau, "j")),
#                           expression(paste("Univariate ", "CP", tau)),
#                           expression(paste("Univariate ", "RP", tau, "j")),
#                           expression(paste("Univariate ", "RP", tau))),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 2.5)
# )
# 
# ### univariate mixture multiplots
# # first late then mid then early
# 
# fn_plot_multiplot_mixture(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(TRUE, 5),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("Univariate ", "RP", tau)),
#                           expression(paste("Univariate ","MRIP", tau)),
#                           expression(paste("Univariate ","CP", tau)),
#                           expression(paste("Univariate ","MCIP", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_mixture(
#   level_of_evidence = "mid",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(TRUE, 5),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("Univariate ", "RP", tau)),
#                           expression(paste("Univariate ","MRIP", tau)),
#                           expression(paste("Univariate ","CP", tau)),
#                           expression(paste("Univariate ","MCIP", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_mixture(
#   level_of_evidence = "early",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(TRUE, 5),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("Univariate ", "RP", tau)),
#                           expression(paste("Univariate ","MRIP", tau)),
#                           expression(paste("Univariate ","CP", tau)),
#                           expression(paste("Univariate ","MCIP", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# 
# 
# ### surrogate multiplot: unmatched
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_unmatched",        
#                "bi_ce_unmatched_w_share",
#                "bi_re_unmatched",        
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "unmatched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "unmatched", "j"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.02, 0.04),
#   ylim_for_Cover = c(0.9, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot(
#   level_of_evidence = "early",
#   v_models = c("uni_re_w_share",
#                "bi_ce_unmatched",        
#                "bi_ce_unmatched_w_share",
#                "bi_re_unmatched",        
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "unmatched"),                      #expression(paste("Bivariate ", "CP", tau, "j \n unmatched")), # "CE unmatched",   
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), # "CE unmatched \n common", 
#                           paste0("Bivariate RP\U1D70F", "j \n", "unmatched", "j"), # "RE unmatched",   
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), # "RE unmatched \n common"), 
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# ### surrogate multiplot: matched
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_matched",        
#                "bi_ce_matched_w_share",
#                "bi_re_matched",        
#                "bi_re_matched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common 
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "matched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "matched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "matched", "j"), 
#                           paste0("Bivariate RP\U1D70F \n", "matched")), 
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 2.5)
# )
# fn_plot_multiplot(
#   level_of_evidence = "early",
#   v_models = c("uni_re_w_share",
#                "bi_ce_matched",        
#                "bi_ce_matched_w_share",
#                "bi_re_matched",        
#                "bi_re_matched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "matched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "matched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "matched", "j"), 
#                           paste0("Bivariate RP\U1D70F \n", "matched")), 
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 2.5)
# )
# 
# ### Outlier indication
# 
# fn_plot_multiplot_outlier_indication(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_unmatched",        
#                "bi_ce_unmatched_w_share",
#                "bi_re_unmatched",        
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "unmatched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "unmatched", "j"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.07, 0.05),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# 
# ## Appendix
# 
# 
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_ce",
#                "uni_ce_w_share",
#                "uni_re",            
#                "uni_re_w_share"),    
#   v_model_OS_in_target = c(FALSE, FALSE,     # CE independent, common, with OS
#                            FALSE, FALSE),     # RE independent, common, with OS
#   v_model_col = c("indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(1, 19,      # CE independent, common, with OS
#                   1, 19),      # RE independent, common, with OS
#   v_model_legend_name = c(expression(paste("Univariate ", "CP", tau, "j")),
#                           expression(paste("Univariate ", "CP", tau)),
#                           expression(paste("Univariate ", "RP", tau, "j")),
#                           expression(paste("Univariate ", "RP", tau))),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.06),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 3)
# )
# 
# ### (A) univariate mixture multiplots - no OS in target
# # first late then early
# 
# fn_plot_multiplot_mixture(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(FALSE, 4),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("RE", tau)),
#                           expression(paste("MRIE", tau)),
#                           expression(paste("CE", tau)),
#                           expression(paste("MCIE", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_mixture(
#   level_of_evidence = "mid",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(FALSE, 4),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("RE", tau)),
#                           expression(paste("MRIE", tau)),
#                           expression(paste("CE", tau)),
#                           expression(paste("MCIE", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_mixture(
#   level_of_evidence = "early",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(FALSE, 4),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("RE", tau)),
#                           expression(paste("MRIE", tau)),
#                           expression(paste("CE", tau)),
#                           expression(paste("MCIE", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# 
# ## Outlier indication
# 
# ### surrogate multiplot: unmatched
# fn_plot_multiplot_outlier_indication(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_unmatched",        
#                "bi_ce_unmatched_w_share",
#                "bi_re_unmatched",        
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "unmatched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.12, 0.05),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_outlier_indication(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_unmatched",        
#                "bi_ce_unmatched_w_share",
#                "bi_re_unmatched",        
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(FALSE,     
#                            FALSE, FALSE,     
#                            FALSE, FALSE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "unmatched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.12, 0.05),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# 
# ### surrogate multiplot: matched
# fn_plot_multiplot_outlier_indication(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_matched",        
#                "bi_ce_matched_w_share",
#                "bi_re_matched",        
#                "bi_re_matched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "matched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "matched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "matched"), 
#                           paste0("Bivariate RP\U1D70F \n", "matched")), 
#   ylim_for_Bias = c(-0.12, 0.05),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_outlier_indication(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "bi_ce_matched",        
#                "bi_ce_matched_w_share",
#                "bi_re_matched",        
#                "bi_re_matched_w_share"),
#   v_model_OS_in_target = c(FALSE,     
#                            FALSE, FALSE,     
#                            FALSE, FALSE),     
#   v_model_col = c("springgreen3",
#                   "indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(19, # uni common     
#                   2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),
#                           paste0("Bivariate CP\U1D70F", "j \n", "matched"),                      
#                           paste0("Bivariate CP\U1D70F \n", "matched"), 
#                           paste0("Bivariate RP\U1D70F", "j \n", "matched"), 
#                           paste0("Bivariate RP\U1D70F \n", "matched")), 
#   ylim_for_Bias = c(-0.12, 0.05),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# 
# 
# ## Graphs for checks
# 
# ### PFS bias etc
# fn_plot_multiplot_PFS(
#   level_of_evidence = "late",
#   v_models = c("bi_ce_matched",        
#                "bi_ce_matched_w_share",
#                "bi_re_matched",        
#                "bi_re_matched_w_share"),
#   v_model_OS_in_target = c(TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c("PFS CE",  
#                           "PFS CE \n common",
#                           "PFS RE",  
#                           "PFS RE \n common"),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot_PFS(
#   level_of_evidence = "early",
#   v_models = c("bi_ce_matched",        
#                "bi_ce_matched_w_share",
#                "bi_re_matched",        
#                "bi_re_matched_w_share"),
#   v_model_OS_in_target = c(TRUE, TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("indianred","indianred",
#                   "springgreen3","springgreen3"),
#   v_model_pch = c(2,  # bi indep
#                   17, # bi common     
#                   2,  # bi indep
#                   17),# bi common      
#   v_model_legend_name = c("PFS CE",  
#                           "PFS CE \n common",
#                           "PFS RE",  
#                           "PFS RE \n common"),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# 
# ## Graphs for euhea presentation
# 
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_ie_w_share",
#                "uni_ce_w_share",
#                "uni_re_w_share"),    
#   v_model_OS_in_target = c(TRUE,     # CE independent, common, with OS
#                            TRUE, TRUE),     # RE independent, common, with OS
#   v_model_col = c(
#     "black", 
#     "indianred",
#     "springgreen3"),
#   v_model_pch = c(19,      # IE independent, common, with OS
#                   19,      # CE independent, common, with OS
#                   19),      # RE independent, common, with OS
#   v_model_legend_name = c(expression(paste("Univariate ", "IP", tau)),
#                           expression(paste("Univariate ", "CP", tau)),
#                           expression(paste("Univariate ", "RP", tau))),
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 1.1)
# )
# fn_plot_multiplot_mixture(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",
#                "uni_mrie_w_share",
#                "uni_ce_w_share",
#                "uni_mcie_w_share"),
#   v_model_OS_in_target = rep(TRUE, 5),     
#   v_model_col = c(
#     "springgreen3",
#     "springgreen3",
#     "indianred",
#     "indianred"),
#   v_model_pch = c(16, 15, 16, 15),
#   v_model_legend_name = c(expression(paste("Univariate ", "RP", tau)),
#                           expression(paste("Univariate ","MRIP", tau)),
#                           expression(paste("Univariate ","CP", tau)),
#                           expression(paste("Univariate ","MCIP", tau))
#   ),
#   ylim_for_Bias = c(-0.02, 0.09),
#   ylim_for_Cover = c(0.3, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",     
#                "bi_ce_unmatched_w_share",   
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred",
#                   "springgreen3"),
#   v_model_pch = c(19, # uni common 
#                   17, # bi common 
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),                 
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.02, 0.12),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 1.1)
# )
# fn_plot_multiplot_outlier_indication(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",   
#                "bi_ce_unmatched_w_share",
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(TRUE,     
#                            TRUE, TRUE),     
#   v_model_col = c("springgreen3",
#                   "indianred",
#                   "springgreen3"),
#   v_model_pch = c(19, # uni common   
#                   17, # bi common   
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),                     
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.12, 0.05),
#   ylim_for_Cover = c(0.8, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 5)
# )
# fn_plot_multiplot(
#   level_of_evidence = "late",
#   v_models = c("uni_re_w_share",     
#                "bi_ce_unmatched_w_share",   
#                "bi_re_unmatched_w_share"),
#   v_model_OS_in_target = c(FALSE,     
#                            FALSE, FALSE),     
#   v_model_col = c("springgreen3",
#                   "indianred",
#                   "springgreen3"),
#   v_model_pch = c(19, # uni common 
#                   17, # bi common 
#                   17),# bi common      
#   v_model_legend_name = c(paste0("Univariate RP\U1D70F"),                 
#                           paste0("Bivariate CP\U1D70F \n", "unmatched"), 
#                           paste0("Bivariate RP\U1D70F \n", "unmatched")), 
#   ylim_for_Bias = c(-0.02, 0.06),
#   ylim_for_Cover = c(0.9, 1),
#   ylim_for_EmpSE = c(0, 0.15),
#   ylim_for_splittingModSeR_IE_w_share = c(0, 3)
# )


## old code

# ## Plots showing winning model over all simulations
# # * does PED "winner" make sense?
# 
# # list of metrics which we want to identify "best" model for
# v_metrics_to_plot_winners <- c("Bias","splittingModSeR_IE", "PED_IE", "splittingModSeR_IE_w_share", "PED_IE_w_share", 
#                                "MSE", "EmpSE", "ModSE")
# 
# # how many decimal places for each metric (in the order above)
# v_lower_better <- c(TRUE, TRUE, FALSE, TRUE, FALSE, 
#                     TRUE, TRUE, TRUE)
# 
# ### Set 1, uni_non_mixture models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 1,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "uni_non_mixture",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set1/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# ### Set 1, all univariate models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 1,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "uni_all",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set1/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# ### Set 1, all models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 1,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "all",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set1/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# 
# 
# 
# 
# ### Set 2, uni_non_mixture models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 2,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "uni_non_mixture",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set2/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# ### Set 2, all univariate models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 2,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "uni_all",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set2/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# ### Set 2, all models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 2,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "all",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set2/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# 
# ### Set 3, uni_non_mixture models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 3,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "uni_non_mixture",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set3/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# ### Set 3, all univariate models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 3,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "uni_all",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set3/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }
# 
# ### Set 3, all models
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_winners)){
#   fn_CV_winner_loop_plot(simulation_set_number = 3,
#                          metric = v_metrics_to_plot_winners[index], 
#                          model_set = "all",
#                          lower_better = v_lower_better[index],
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_winner/Set3/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_winners)*100,0), "% complete"))
# }




# ### Set 1
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_values)){
#   fn_CV_result_loop_plot(simulation_set_number = 1,
#                          metric = v_metrics_to_plot_values[index], 
#                          decimal_places = v_decimal_places[index], 
#                          CI_interval = FALSE,
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_values/Set1/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_values)*100,0), "% complete"))
# }
# 
# ### Set 2
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_values)){
#   fn_CV_result_loop_plot(simulation_set_number = 2,
#                          metric = v_metrics_to_plot_values[index], 
#                          decimal_places = v_decimal_places[index], 
#                          CI_interval = FALSE,
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_values/Set2/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_values)*100,0), "% complete"))
# }
# 
# ### Set 3
# # loop over metrics and plot early,mid,late for all models, effect sizes etc
# for(index in 1:length(v_metrics_to_plot_values)){
#   fn_CV_result_loop_plot(simulation_set_number = 3,
#                          metric = v_metrics_to_plot_values[index], 
#                          decimal_places = v_decimal_places[index], 
#                          CI_interval = FALSE,
#                          figures_location = paste0(david_local_path, 
#                                                    "Figures/Model_values/Set3/"),
#                          google_drive_path = google_drive_path)
#   cat('\r', paste0(round(index/length(v_metrics_to_plot_values)*100,0), "% complete"))
# }


# ### MSE
# png(paste0(figures_location, "MSE Set 1 small effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "MSE Set 1 small effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "MSE Set 1 large effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "MSE Set 1 large effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### Bias_abs
# # use the absolute magnitude of the bias to detemine the winner
# # otherwise it will favour models with negative bias.
# png(paste0(figures_location, "Bias_abs Set 1 small effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias_abs",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "Bias_abs Set 1 small effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias_abs",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "Bias_abs Set 1 large effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias_abs",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "Bias_abs Set 1 large effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias_abs",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# 
# ### EmpSE
# png(paste0(figures_location, "EmpSE Set 1 small effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "EmpSE Set 1 small effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "EmpSE Set 1 large effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# png(paste0(figures_location, "EmpSE Set 1 large effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]))
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### PED
# png(paste0(figures_location, "PED Set 1 small effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "PED",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     lower_better = FALSE)
# }
# dev.off()
# png(paste0(figures_location, "PED Set 1 large effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "PED",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     lower_better = FALSE)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### splittingModSeR
# png(paste0(figures_location, "splittingModSeR Set 1 small effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "splittingModSeR",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     lower_better = TRUE)
# }
# dev.off()
# png(paste0(figures_location, "splittingModSeR Set 1 large effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_winner_plot(evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "splittingModSeR",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     lower_better = TRUE)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ## other metrics
# # [1] "Model"                          "RelBias"                       
# # [3] "RelBias_MCSE"                   "Bias"                          
# # [5] "Bias_MCSE"                      "Cover"                         
# # [7] "Cover_MCSE"                     "MSE"                           
# # [9] "MSE_MCSE"                       "EmpSE"                         
# # [11] "EmpSE_MCSE"                     "PED"                           
# # [13] "PED_MCSE"                       "RelPED"                        
# # [15] "RelPED_MCSE"                    "splittingModSeR"               
# # [17] "splittingModSeR_MCSE"           "splittingEmpSeR"               
# # [19] "splittingEmpSeR_MCSE"           "ModSE"                         
# # [21] "ModSE_MCSE"                     "Ratio_E_ModSE_sq_over_EmpSE_sq"
# # [23] "Prop_Rhat_1.2"                  "Prop_Rhat_1.1"                 
# # [25] "Prop_Rhat_1.05"                 "Prop_Rhat_1.01"                
# # [27] "mean_observed_OS_LHR"           "sd_observed_OS_LHR"            
# # [29] "sd_true_OS_LHR_restricted"      "sd_true_OS_LHR_unrestricted"   
# # [31] "CV_LHROS_observed"              "CV_LHROS_true_restricted"      
# # [33] "CV_LHRPFS_observed"             "CV_LHRPFS_true_restricted"  




# ## heatmap
# 
# data<- matrix(NA, nrow = 5, ncol = 5,
#               dimnames = list(c("0%bw", "7%bw", "15%bw", "30%bw", "50%bw"),
#                               c("0%w", "7%w", "15%w", "30%w", "50%w")))  
# 
# 
# # late
# # with OS
# # small effect
# # MSE as the metric
# # 0% bw, 0% w
# # data[5,5] <- model_value[which.min(l_results$`1.1_late_with OS.csv`$MSE)] 
# # # 50% bw, 50% w
# # data[1,1] <- model_value[which.min(l_results[[paste0("1.25", "_late_with OS.csv")]]$MSE)] 
# 
# 
# # matrix with parameter sets:
# # small effect
# # 
# m_parameter_index <- matrix(paste0(1, ".", 1:25), 
#                             nrow = 5, ncol = 5, byrow = FALSE,
#                             dimnames = list(c("0%bw", "7%bw", "15%bw", "30%bw", "50%bw"),
#                                             c("0%w", "7%w", "15%w", "30%w", "50%w")))  
# 
# # fill matrix with number coresponding to winning model
# for(n_row in 1:5){
#   for(n_col in 1:5){
#     try(data[n_row,n_col] <- model_value[which.min(l_results[[paste0(m_parameter_index[n_row, n_col], 
#                                                                      "_late_with OS.csv")]]$MSE)] )
#   }
# }
# 
# library(ggplot2)
# 
# # find winning model for each combination
# # and assign the model index score:
# # 0 => NA - missing data (white)
# # 1 => IE (lightgray)
# # 2 => RE
# # 3 => CE (black)
# model_value <- c(1, 3, 3, 2, 2) # in table IE, CE, MCIE, RE, MRIE
# data2$score <- NA
# for(s in 1:nrow(data2)){
#   try(data2$score[s] <- model_value[which.min(l_results[[paste0(data2$Parameter_set[s], 
#                                           "_late_with OS.csv")]]$MSE)] )
# }                                                   
# data2[is.na(data2)] <- 0 # NA is score 0
# # convert score to a factor
# data2$score_factor <- factor(data2$score, levels = c(0, 1, 2, 3))
# 
# cols <- c("0" = "white", "1" = "lightgray", "2" = "darkgray", "3" = "black")
# # Heatmap 
# ggplot(data2, aes(log_M_01_CV_w , log_M_01_CV_bw , fill= score_factor)) + 
#   theme(panel.background = element_blank()) +
#     geom_tile(color = "white",
#             lwd = 0.5,
#             linetype = 1) +
#   labs(x = "CV within indications", 
#        y = "CV between indications",
#        title = "MSE performance, late with OS, small effect") +
#   scale_colour_manual(labels = c('NA', 'IE', 'RE', 'CE'), 
#                     values = cols,
#                     name = "Best model")
# 
# 
# p <- ggplot(mtcars, aes(mpg, wt)) +
#   geom_point(aes(colour = factor(cyl)))
# p + scale_colour_manual(values = c("red", "blue", "green"))
# 
# # It's recommended to use a named vector
# cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
# p + scale_colour_manual(values = cols)
# 
# 
# 
# set.seed(10)
# 
# # Dummy data
# x <- LETTERS[1:20]
# y <- paste0("var", seq(1,20))
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, -1, 2)
# # Heatmap 
# ggplot(data, aes(X, Y, fill= factor(round(data$Z)))) + 
#   theme(panel.background = element_blank()) +
#   geom_tile(color = "white",
#             lwd = 0.5,
#             linetype = 1)+
#   scale_fill_manual(labels = c('red', 'green', 'yellow', 'blue'), values = c('red', 'green', 'yellow', 'blue'), name = 'Z')
# 
# 
# 
# x <- -10:10
# y <- -10:10
# z <- sqrt(outer(x ^ 2, y ^ 2, "+"))
# image(x, y, z)
# 
# image(x = c(0, 7, 15, 30, 50), 
#       y = c(0, 7, 15, 30, 50), 
#       z = data,
#       xlab = "CV w", ylab = "CV bw",
#       xlim = c(0, 50), ylim = c(0, 50))
# 
# 
# library(RColorBrewer)
# heatmap(data,Colv = NA, Rowv = NA, 
#         scale="none", xlab="something", ylab="", main="A title", 
#         labRow=row.names(data), labCol=colnames(data) , 
#         col= c("lightgray", "darkgrey", "black"))
# legend(x="bottomright", legend=c("min", "ave", "max"), 
#        fill=c("lightgray", "darkgrey", "black"))
# 
# 
# head(data,5)
# heatmap(data, Rowv = NA, Colv = NA, scale = "none", main = "Title")
# legend(x="bottomright", legend=c("min", "ave", "max"))
# 
# #### old 
# 
# m_CV_w <- matrix(c(rep(0.5, 5), rep(0.3, 5), rep(0.15, 5), rep(0.07, 5), rep(0, 5)),
#                  nrow = 5, ncol = 5,
#                  byrow = FALSE)
# m_CV_bw <- matrix(c(rep(0.5, 5), rep(0.3, 5), rep(0.15, 5), rep(0.07, 5), rep(0, 5)),
#                   nrow = 5, ncol = 5,
#                   byrow = TRUE)
# 
# # find parameter set where the conditions are met
# # small effect, 0% w and 0% bw
# Simulation_set1$Parameter_set[which(Simulation_set1$log_M_01_mu == -0.223 & 
#                                       Simulation_set1$log_M_01_CV_w == 0 & 
#                                       Simulation_set1$log_M_01_CV_bw == 0)]
# 
# # fill empty matrix where conditions are met
# for(n_row in 1:5){
#   for(n_col in 1:5){
#     m_parameter_index[n_row, n_col] <- Simulation_set1$Parameter_set[which(Simulation_set1$log_M_01_mu == -0.223 & 
#                                                                              Simulation_set1$log_M_01_CV_w == m_CV_w[n_row, n_col] & 
#                                                                              Simulation_set1$log_M_01_CV_bw == m_CV_bw[n_row, n_col])]    
#   }
# }
# 
# # small effect => 1.1 to 1.25
# # first half of file_list
# relevant_files1 <- file_list[1:(length(file_list)/2)]
# file_list
# 
# 
# 
# # https://stackoverflow.com/questions/57395558/how-to-show-legend-in-heatmap
# 
# 
# ### Best and worst models within each scenario
# # takes l_results as an input and outputs a dataframe with winning model
# 
# ## based on MEAN performance
# v_metrics <- c("Bias", "MSE")
# df_summary_mean <- data.frame(NULL)
# for(s in 1:length(l_results)){
#   
#   ## identify winners/losers for scenario s 
#   # e.g. "1.1_early_with OS", "1.1_early_no OS"
#   
#   df_summary_temp <- as.data.frame(matrix(NA, nrow = length(v_metrics)*2,
#                                           ncol = 4))
#   df_summary_temp[,1] <- names(l_results)[s]
#   names(df_summary_temp) <- c("scenario_name" ,"metric" ,"model","value")
#   
#   # calculate lowest and highest MEAN for each metric
#   for(m in 1:length(v_metrics)){
#     # assign name of metric
#     df_summary_temp$metric[m*2-1] <- paste0("lowest_", v_metrics[m])
#     df_summary_temp$metric[m*2] <- paste0("highest_", v_metrics[m])
#     # df_summary_temp$metric[length(v_metrics) + m*2-1] <- paste0("lowest_median_", v_metrics[m])
#     # df_summary_temp$metric[length(v_metrics) + m*2] <- paste0("highest_median_", v_metrics[m])
#     
#     # find column index for metrics
#     metric_col_index_mean <- names(l_results[[s]])== paste0(v_metrics[m])
#     #metric_col_index_median <- names(l_results[[s]])== paste0("median_",v_metrics[m])
#     
#     # best and worst models
#     model_with_lowest_mean <-     l_results[[s]]$Model[which.min(l_results[[s]][,metric_col_index_mean])]
#     model_with_highest_mean <-    l_results[[s]]$Model[which.max(l_results[[s]][,metric_col_index_mean])]
#     # model_with_lowest_median <-     l_results[[s]]$Models[which.min(l_results[[s]][,metric_col_index_median])]
#     # model_with_highest_median <-    l_results[[s]]$Models[which.max(l_results[[s]][,metric_col_index_median])]
#     # best and worst values
#     value_lowest_mean <- l_results[[s]][,metric_col_index_mean][which.min(l_results[[s]][,metric_col_index_mean])]
#     value_highest_mean <- l_results[[s]][,metric_col_index_mean][which.max(l_results[[s]][,metric_col_index_mean])]
#     # value_lowest_median <- l_results[[s]][,metric_col_index_median][which.min(l_results[[s]][,metric_col_index_median])]
#     # value_highest_median <- l_results[[s]][,metric_col_index_median][which.max(l_results[[s]][,metric_col_index_median])]
#     
#     # assign results 
#     df_summary_temp$model[m*2-1] <- toString(model_with_lowest_mean)
#     df_summary_temp$model[m*2] <- toString(model_with_highest_mean)
#     df_summary_temp$value[m*2-1] <- value_lowest_mean
#     df_summary_temp$value[m*2] <- value_highest_mean
#     
#     # # assign results MEDIAN
#     # df_summary_temp$model[length(v_metrics) + m*2-1] <- toString(model_with_lowest_mean)
#     # df_summary_temp$model[length(v_metrics) +m*2] <- toString(model_with_highest_mean)
#     # df_summary_temp$value[length(v_metrics) +m*2-1] <- value_lowest_mean
#     # df_summary_temp$value[length(v_metrics) +m*2] <- value_highest_mean
#     
#   }
#   
#   # # # DIC
#   # # lowest_DIC <- l_results[[s]]$Models[which.min(l_results[[s]]$DIC_mean)]
#   # # value_lowest_DIC <- l_results[[s]]$DIC_mean[which.min(l_results[[s]]$DIC_mean)]
#   # # highest_DIC <- l_results[[s]]$Models[which.max(l_results[[s]]$DIC_mean)]
#   # # value_highest_DIC <- l_results[[s]]$DIC_mean[which.max(l_results[[s]]$DIC_mean)]
#   # 
#   # # "95 interval"
#   # smallest_95_interval <- l_results[[s]]$Models[which.min(l_results[[s]]$width_95_interval_mean)]
#   # value_smallest_95_interval <- l_results[[s]]$width_95_interval_mean[which.min(l_results[[s]]$width_95_interval_mean)]
#   # largest_95_interval <- l_results[[s]]$Models[which.max(l_results[[s]]$width_95_interval_mean)]
#   # value_largest_95_interval <- l_results[[s]]$width_95_interval_mean[which.max(l_results[[s]]$width_95_interval_mean)]
#   # 
#   # # "partial_MAE"
#   # lowest_partial_MAE <- l_results[[s]]$Models[which.min(l_results[[s]]$partial_MAE_mean)]
#   # value_lowest_partial_MAE <- l_results[[s]]$partial_MAE_mean[which.min(l_results[[s]]$partial_MAE_mean)]
#   # highest_partial_MAE <- l_results[[s]]$Models[which.max(l_results[[s]]$partial_MAE_mean)]
#   # value_highest_partial_MAE <- l_results[[s]]$partial_MAE_mean[which.max(l_results[[s]]$partial_MAE_mean)]
#   # 
#   # # "MAE" (long run)
#   # lowest_MAE <- l_results[[s]]$Models[which.min(l_results[[s]]$MAE_mean)]
#   # value_lowest_MAE <- l_results[[s]]$MAE_mean[which.min(l_results[[s]]$MAE_mean)]
#   # highest_MAE <- l_results[[s]]$Models[which.max(l_results[[s]]$MAE_mean)]
#   # value_highest_MAE <- l_results[[s]]$MAE_mean[which.max(l_results[[s]]$MAE_mean)]
#   # 
#   # # "coverage to 95%" (partial)
#   # closest_partial_coverage_95 <- l_results[[s]]$Models[which.min(abs(l_results[[s]]$partial_truth_within_95_interval - 0.95))]
#   # value_closest_partial_coverage_95 <- l_results[[s]]$partial_truth_within_95_interval[which.min(abs(l_results[[s]]$partial_truth_within_95_interval - 0.95))]
#   # farthest_partial_coverage_95 <- l_results[[s]]$Models[which.max(abs(l_results[[s]]$partial_truth_within_95_interval - 0.95))]
#   # value_farthest_partial_coverage_95 <- l_results[[s]]$partial_truth_within_95_interval[which.max(abs(l_results[[s]]$partial_truth_within_95_interval - 0.95))]
#   # 
#   # # "coverage to 95%" (long run)
#   # closest_coverage_95 <- l_results[[s]]$Models[which.min(abs(l_results[[s]]$truth_within_95_interval - 0.95))]
#   # value_closest_coverage_95 <- l_results[[s]]$truth_within_95_interval[which.min(abs(l_results[[s]]$truth_within_95_interval - 0.95))]
#   # farthest_coverage_95 <- l_results[[s]]$Models[which.max(abs(l_results[[s]]$truth_within_95_interval - 0.95))]
#   # value_farthest_coverage_95 <- l_results[[s]]$truth_within_95_interval[which.max(abs(l_results[[s]]$truth_within_95_interval - 0.95))]
#   # 
#   # 
#   # # collect resutls in a data frame
#   # df_summary_temp <- data.frame(
#   #   scenario_name = rep(names(l_results)[s], 10),
#   #   metric = c(#"Lowest DIC","Highest DIC",
#   #              "Smallest 95 interval", "Largest 95 interval", 
#   #              "Lowest partial_MAE", "Highest partial_MAE",
#   #              "Lowest long_run_MAE", "Highest long_run_MAE",
#   #              "Closest coverage to 95% (partial)", "Farthest coverage from 95% (partial)",
#   #              "Closest coverage to 95% (long run)", "Farthest coverage from 95% (long run)"),
#   #   model = c(#lowest_DIC,       highest_DIC,       
#   #             smallest_95_interval,       largest_95_interval,       
#   #             lowest_partial_MAE,       highest_partial_MAE, 
#   #             lowest_MAE,       highest_MAE, 
#   #             closest_partial_coverage_95,       farthest_partial_coverage_95,
#   #             closest_coverage_95,       farthest_coverage_95),
#   #   value = c(#value_lowest_DIC ,value_highest_DIC, 
#   #             value_smallest_95_interval ,value_largest_95_interval ,
#   #             value_lowest_partial_MAE ,value_highest_partial_MAE, 
#   #             value_lowest_MAE ,value_highest_MAE, 
#   #             value_closest_partial_coverage_95 ,value_farthest_partial_coverage_95,
#   #             value_closest_coverage_95 ,value_farthest_coverage_95)
#   # )
#   
#   # append results
#   df_summary_mean <- rbind(df_summary_mean, df_summary_temp)
# }
# print(df_summary_mean, row.names = FALSE)
# 
# 
# 
# 
# ## create function which goes thorugh all elements in l_results and calculates
# # which model has the 
# 
# 
# 
# 
# 
# # # # load and print specific results
# # # all files
# # list.files(paste0(results_location, "Result_elements/"))
# # print(read.csv(file = paste0(results_location, "Result_elements/", "3.1_early_with OS.csv")), 
# #       row.names = FALSE)
# 
# 
# ## results
# fn_compare_performance <- function(metric){
#   df_means <- data.frame(
#     models = model_set,
#     early_with_OS = as.numeric(unlist(l_results[["2.2_early_with OS"]][metric])),
#     mid_with_OS = as.numeric(unlist(l_results[["2.2_mid_with OS"]][metric])),
#     late_with_OS = as.numeric(unlist(l_results[["2.2_late_with OS"]][metric])),
#     early_no_OS = as.numeric(unlist(l_results[["2.2_early_no OS"]][metric])),
#     mid_no_OS =  as.numeric(unlist(l_results[["2.2_mid_no OS"]][metric])),
#     late_no_OS = as.numeric(unlist(l_results[["2.2_late_no OS"]][metric]))
#   )
# 
#   # df_MCSE <- data.frame(
#   #   models = model_set,
#   #   early_with_OS = as.numeric(unlist(l_results[["2.2_early_with OS"]][paste0(metric, "_MCSE")])),
#   #   mid_with_OS = as.numeric(unlist(l_results[["2.2_mid_with OS"]][paste0(metric, "_MCSE")])),
#   #   late_with_OS = as.numeric(unlist(l_results[["2.2_late_with OS"]][paste0(metric, "_MCSE")])),
#   #   early_no_OS = as.numeric(unlist(l_results[["2.2_early_no OS"]][paste0(metric, "_MCSE")])),
#   #   mid_no_OS =  as.numeric(unlist(l_results[["2.2_mid_no OS"]][paste0(metric, "_MCSE")])),
#   #   late_no_OS = as.numeric(unlist(l_results[["2.2_late_no OS"]][paste0(metric, "_MCSE")]))
#   # )
#   #
#   # for(e in 1:length(evidence_set)){
#   #   # initialise plot
#   #   x_axis_bounds <- c(
#   #     # lower bound
#   #     min(df_means[,e+1] - 2*df_MCSE[,e+1] ),
#   #     # upper bound
#   #     max(df_means[,e+1] + 2*df_MCSE[,e+1] ))
#   #
#   #   # create plot title
#   #   plot_title <- paste0(evidence_set[e])
#   #
#   #   # initialise plot
#   #   plot(1, type="n",
#   #        xlab= metric, xlim=x_axis_bounds,
#   #        ylab="", ylim=c(0, 10), yaxt='n',
#   #        main = plot_title)
#   #
#   #   # define the y axis values for:
#   #   # each of the models in model_set
#   #   # number of gaps =
#   #   # number of models - 1 (gaps between the models) +
#   #   # 2 (gap at bottom and top)
#   #   number_of_gaps <- length(model_set) - 1 + 2
#   #   size_of_gaps <- 10/number_of_gaps
#   #   y_values <- cumsum(rep(size_of_gaps, 3)) # y axis co-ordinates for models
#   #
#   #   # add y axis labels for models
#   #   axis(side = 2, at=y_values, labels=model_set, las = 2)
#   #
#   #   # plot the metric point est and 95 interval
#   #   # for all models
#   #   for(m in 1:length(model_set)){
#   #     # point estimate for model m, evidence set e
#   #     # points(df_means[m, e+1], y_values[m])
#   #     # 95% interval for model m, evidence set e
#   #     lines(x = c(
#   #       df_means[m, e+1] - df_MCSE[m, e+1]*1.96,
#   #       df_means[m, e+1] + df_MCSE[m, e+1]*1.96),
#   #       y = rep(y_values[m],2), lwd = 2)
#   #   }
#   # }
#   df_means
# }
# 
# fn_compare_performance(metric = "EmpSE")
# 
# 
# 
# evidence_set
# 
# print(l_results[["2.2_late_with OS"]]$Bias, row.names = FALSE)

# ### Bias CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE Bias Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE Bias Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE Bias Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE Bias Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset


### old code

# * empSE for IE with no OS is ~zero because it is drawn from prior with a lot of samples!
# because we use the same prior draw 
# assign this a high value to stop it interfering with the model comparion
# for(s in 1:length(l_results)){
#   l_results[[s]]$EmpSE[1] <- 10
# }

# # to compare bias across models, compute the absolute bias
# for(s in 1:length(l_results)){
#   l_results[[s]]$Bias_abs <- abs(l_results[[s]]$Bias)
# }
# 
# # to compare relative bias across models, convert to %
# for(s in 1:length(l_results)){
#   l_results[[s]]$RelBias_prec <- l_results[[s]]$RelBias*100
# }


# ### EmpSE CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE EmpSE Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "CE EmpSE Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "CE EmpSE Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "CE EmpSE Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### EmpSE RE
# png(paste0(figures_location, "RE EmpSE Set 1 small effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "RE EmpSE Set 1 small effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "RE EmpSE Set 1 large effect no OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "RE EmpSE Set 1 large effect with OS.png"),
#     width = 1180, height = 220)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "EmpSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# 
# ### Bias CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE Bias Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE Bias Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE Bias Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE Bias Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### Bias RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE Bias Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3,
#                     CI_interval = FALSE)
# }
# dev.off()
# png(paste0(figures_location, "RE Bias Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3,
#                     CI_interval = FALSE)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### Bias IE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "IE Bias Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "IE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Bias",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3,
#                     CI_interval = FALSE)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# 
# ### Cover CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE Cover Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Cover Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Cover Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Cover Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### Cover RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE Cover Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Cover Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Cover Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Cover Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Cover",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# 
# 
# 
# ### MSE CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE MSE Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE MSE Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE MSE Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE MSE Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### MSE RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE MSE Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3,
#                     CI_interval = FALSE)
# }
# dev.off()
# png(paste0(figures_location, "RE MSE Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "MSE",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3,
#                     CI_interval = FALSE)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### PED CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE PED Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "PED",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE PED Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "PED",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### PED RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE PED Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "PED",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE PED Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "PED",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# 
# ### Prop_Rhat_1.01 CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE Prop_Rhat_1.01 Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Prop_Rhat_1.01 Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Prop_Rhat_1.01 Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Prop_Rhat_1.01 Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### Prop_Rhat_1.01 RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE Prop_Rhat_1.01 Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Prop_Rhat_1.01 Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Prop_Rhat_1.01 Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Prop_Rhat_1.01 Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.01",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### Prop_Rhat_1.2 CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE Prop_Rhat_1.2 Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Prop_Rhat_1.2 Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Prop_Rhat_1.2 Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "CE Prop_Rhat_1.2 Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### Prop_Rhat_1.2 RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE Prop_Rhat_1.2 Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Prop_Rhat_1.2 Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Prop_Rhat_1.2 Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# png(paste0(figures_location, "RE Prop_Rhat_1.2 Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Prop_Rhat_1.2",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 2)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### splittingModSeR CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE splittingModSeR Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "splittingModSeR",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "CE splittingModSeR Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "splittingModSeR",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### splittingModSeR RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE splittingModSeR Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "splittingModSeR",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# png(paste0(figures_location, "RE splittingModSeR Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "splittingModSeR",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 3)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### Ratio_E_ModSE_sq_over_EmpSE_sq CE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "CE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# png(paste0(figures_location, "CE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# png(paste0(figures_location, "CE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# png(paste0(figures_location, "CE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# 
# ### Ratio_E_ModSE_sq_over_EmpSE_sq RE
# width <- 1180
# height <- 220
# png(paste0(figures_location, "RE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# png(paste0(figures_location, "RE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 small effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 4:6){ #
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# png(paste0(figures_location, "RE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# png(paste0(figures_location, "RE Ratio_E_ModSE_sq_over_EmpSE_sq Set 1 large effect with OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 10:12){ # 
#   fn_CV_result_plot(model = "RE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "Ratio_E_ModSE_sq_over_EmpSE_sq",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 1)
# }
# dev.off()
# par(mfrow=c(1,1)) # reset
# 
# 
# ### Validation: CV_LHROS_true_restricted CE
# # note this does not depend on the model
# width <- 1180
# height <- 220
# png(paste0(figures_location, "True effects Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "CV_LHROS_true_restricted",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# dev.off()
# png(paste0(figures_location, "True effects Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "CV_LHROS_true_restricted",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 4)
# }
# par(mfrow=c(1,1)) # reset
# 
# ### Validation: CV_LHROS_observed CE
# # note this does not depend on the model
# width <- 1180
# height <- 220
# png(paste0(figures_location, "Observed effects Set 1 small effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 1:3){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "CV_LHROS_observed",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 6)
# }
# dev.off()
# png(paste0(figures_location, "Observed effects Set 1 large effect no OS.png"),
#     width = width, height = height)
# par(mfrow=c(1,3))
# for(c in 7:9){ # 
#   fn_CV_result_plot(model = "CE",
#                     evidence_set = as.character(plot_combinations$evidence_set[c]),
#                     metric = "CV_LHROS_observed",
#                     effect_size = as.character(plot_combinations$effect_size[c]),
#                     decimal_places = 6)
# }
# par(mfrow=c(1,1)) # reset

