

# w_share => should the model share within indication heterogeneity
applyIe_JAGS <- function(dataset, w_share){
  
  # remove if OS studies are missing - work with complete cases only
  complete_data_rows <- !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  # The specification of the data is the same in JAGS as in BUGS
  jagsData <- list(nStudies       = nrow(dataset),   
                   y              = dataset$OS_LHR_mean,
                   sigma          = dataset$OS_LHR_se,
                   indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                   nIndications   = length(unique(dataset$indication_index)))
  
  # initial values required for each chain, each with different starting values
  initialValues <- list(
    list(theta     = rep(0, jagsData$nStudies),
         mu.theta  = rep(0, jagsData$nIndications),
         tau.theta = rep(1, ifelse(w_share, 1, jagsData$nIndications)),
         .RNG.name = "base::Wichmann-Hill", # *
         .RNG.seed = 1), #*
    list(theta     = rep(-5, jagsData$nStudies),
         mu.theta  = rep(0, jagsData$nIndications),
         tau.theta = rep(0.001, ifelse(w_share, 1, jagsData$nIndications)),
         .RNG.name = "base::Wichmann-Hill",# *
         .RNG.seed = 2),# *
    list(theta     = rep(5, jagsData$nStudies),
         mu.theta  = rep(0, jagsData$nIndications),
         tau.theta = rep(2, ifelse(w_share, 1, jagsData$nIndications)),
         .RNG.name = "base::Wichmann-Hill",# *
         .RNG.seed = 3)# *
  )
  
  parameters <- c("theta", "mu.theta", "tau.theta")  # The parameter(s) to be monitored.
  
  # all the code below is new
  # number of iterations that JAGS will use to choose the sampler and to assure optimum mixing of the MCMC chain
  # Number of steps to "tune" the samplers. 
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/ie/ie",ifelse(w_share, "_w_share.txt", ".txt")), 
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  # Burn-in:
  update( jagsModel , n.iter=burnInSteps, progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps , progress.bar = "none")
  # summary(codaSamples)
  # resulting codaSamples object has these indices:
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  
  # # codaSamples object
  # # list with n.chains elements
  # codaSamples[[1]] # matrix of samples for all parameters in chain 1
  # codaSamples[[2]] # matrix of samples for all parameters in chain 1
  # codaSamples[[3]] # matrix of samples for all parameters in chain 1
  
  # return result
  output <- list(
    codaSamples_OS = codaSamples
  )
  output
}

# w_share => should the model share within indication heterogeneity
applyCe_JAGS <- function(dataset, w_share){
  
  # remove if OS studies are missing - work with complete cases only
  complete_data_rows <- !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- list(nStudies       = nrow(dataset),   
                   y              = dataset$OS_LHR_mean,
                   sigma          = dataset$OS_LHR_se,
                   indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                   nIndications   = length(unique(dataset$indication_index)))
  
  # initial values required for each chain, each with different starting values
  # note, if sharing within indication heterogeneity, only one tau.theta initial value required
  initialValues <- list(
    list(theta     = rep(0, jagsData$nStudies),
         tau.theta = rep(1, ifelse(w_share, 1, jagsData$nIndications)),
         mu        = 0,
         .RNG.name = "base::Wichmann-Hill", 
         .RNG.seed = 1),
    list(theta     = rep(-5, jagsData$nStudies),
         tau.theta = rep(0.001, ifelse(w_share, 1, jagsData$nIndications)),
         mu        = 5,
         .RNG.name = "base::Wichmann-Hill", 
         .RNG.seed = 2),
    list(theta     = rep(5, jagsData$nStudies),
         tau.theta = rep(2, ifelse(w_share, 1, jagsData$nIndications)),
         mu        = -5,
         .RNG.name = "base::Wichmann-Hill", 
         .RNG.seed = 3)
  )
  
  parameters <- c("theta", "tau.theta", "mu")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/ce/ce",ifelse(w_share, "_w_share.txt", ".txt")), 
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  # Burn-in:
  update( jagsModel , n.iter=burnInSteps , progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps, progress.bar = "none" )
  
  # return result
  output <- list(
    codaSamples_OS = codaSamples
  )
  output
  
}

applyMcie_JAGS <- function(dataset, w_share){
  
  # remove if OS studies are missing - work with complete cases only
  complete_data_rows <- !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- list(nStudies       = nrow(dataset),   
                   y              = dataset$OS_LHR_mean,
                   sigma          = dataset$OS_LHR_se,
                   indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                   nIndications   = length(unique(dataset$indication_index)))
  
  initialValues <- 
    with(jagsData, 
         list(
           list(theta     = rep(0, nStudies),
                tau.theta = rep(1, ifelse(w_share, 1, nIndications)),
                c         = rep(0, nIndications),
                mu.branch = t(matrix(c(NA, 0), 2, nIndications)),
                mu        = 0,
                p         = rep(0.5, nIndications),
                .RNG.name = "base::Wichmann-Hill", # *
                .RNG.seed = 1),
           list(theta     = rep(-5, nStudies),
                tau.theta = rep(0.001, ifelse(w_share, 1, nIndications)),
                c         = rep(0, nIndications),
                mu.branch = t(matrix(c(NA, 5), 2, nIndications)),
                mu        = 5,
                p         = rep(0.001, nIndications),
                .RNG.name = "base::Wichmann-Hill", # *
                .RNG.seed = 2),
           list(theta     = rep(5, nStudies),
                tau.theta = rep(2, ifelse(w_share, 1, nIndications)),
                c         = rep(1, nIndications),
                mu.branch = t(matrix(c(NA, -5), 2, nIndications)),
                mu        = -5,
                p         = rep(0.999, nIndications),
                .RNG.name = "base::Wichmann-Hill", # *
                .RNG.seed = 3)
         )
    )
  
  parameters <- c("theta", "mu.theta", "tau.theta", "mu", "mu.branch", "ifBranch",
                  "c", "p")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=10                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/mcie/mcie",ifelse(w_share, "_w_share.txt", ".txt")),
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  # Burn-in:
  update( jagsModel , n.iter=burnInSteps , progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps, progress.bar = "none" )
  
  # return result
  output <- list(
    codaSamples_OS = codaSamples
  )
  output
  
}

applyRe_JAGS <- function(dataset, w_share){
  
  # remove if OS studies are missing - work with complete cases only
  complete_data_rows <- !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- list(nStudies       = nrow(dataset),   
                   y              = dataset$OS_LHR_mean,
                   sigma          = dataset$OS_LHR_se,
                   indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                   nIndications   = length(unique(dataset$indication_index)))
  
  initialValues <- list(
    list(theta     = rep(0, jagsData$nStudies),
         tau.theta = rep(1, ifelse(w_share, 1, jagsData$nIndications)),
         mMu       = 0,
         mTau      = 1,
         .RNG.name = "base::Wichmann-Hill", # *
         .RNG.seed = 1),
    list(theta     = rep(-5, jagsData$nStudies),
         tau.theta = rep(0.001, ifelse(w_share, 1, jagsData$nIndications)),
         mMu       = -5,
         mTau      = 0.001,
         .RNG.name = "base::Wichmann-Hill", # *
         .RNG.seed = 2),
    list(theta     = rep(5, jagsData$nStudies),
         tau.theta = rep(2, ifelse(w_share, 1, jagsData$nIndications)),
         mMu       = 5,
         mTau      = 2,
         .RNG.name = "base::Wichmann-Hill", # *
         .RNG.seed = 3)
  )
  
  parameters <- c("theta", "mu.theta", "tau.theta", "mMu", "mTau", "mPred")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/re/re" ,ifelse(w_share, "_w_share.txt", ".txt")),
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  # Burn-in:
  update( jagsModel , n.iter=burnInSteps , progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps, progress.bar = "none" )
  # return result
  output <- list(
    codaSamples_OS = codaSamples
  )
  output
  
}


applyMrie_JAGS <- function(dataset, w_share){
  
  # remove if OS studies are missing - work with complete cases only
  complete_data_rows <- !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- list(nStudies       = nrow(dataset),   
                   y              = dataset$OS_LHR_mean,
                   sigma          = dataset$OS_LHR_se,
                   indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                   nIndications   = length(unique(dataset$indication_index)))
  
  initialValues <- 
    with(jagsData, 
         list(
           list(theta     = rep(0, nStudies),
                tau.theta = rep(1, ifelse(w_share, 1, nIndications)),
                c         = rep(0, nIndications),
                mu.branch = t(matrix(c(0, 0), 2, nIndications)),
                mMu       = 0,
                mTau      = 1,
                mPred     = 0,
                p         = rep(0.5, nIndications),
                .RNG.name = "base::Wichmann-Hill", # *
                .RNG.seed = 1),
           list(theta     = rep(-5, nStudies),
                tau.theta = rep(0.001, ifelse(w_share, 1, nIndications)),
                c         = rep(0, nIndications),
                mu.branch = t(matrix(c(5, 5), 2, nIndications)),
                mMu       = 5,
                mTau      = 0.001,
                mPred     = 5,
                p         = rep(0.001, nIndications),
                .RNG.name = "base::Wichmann-Hill", # *
                .RNG.seed = 2),
           list(theta     = rep(5, nStudies),
                tau.theta = rep(2, ifelse(w_share, 1, nIndications)),
                c         = rep(1, nIndications),
                mu.branch = t(matrix(c(-5), 2, nIndications)),
                mMu       = -5,
                mTau      = 2,
                mPred     = -5,
                p         = rep(0.999, nIndications),
                .RNG.name = "base::Wichmann-Hill", # *
                .RNG.seed = 3)
         )
    )
  
  parameters <- c("theta", "mu.theta", "tau.theta", "mMu", "mTau", "mPred",
                  "mu.branch", "ifBranch", "p", "c")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=10                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/mrie/mrie" ,ifelse(w_share, "_w_share.txt", ".txt")),
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  # Burn-in:
  update( jagsModel , n.iter=burnInSteps , progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps, progress.bar = "none" )
  
  # return result
  output <- list(
    codaSamples_OS = codaSamples
  )
  output
  
}




# ### Old code #####
# 
# applyCe_homo_JAGS <- function(dataset, model_name, i ){
#   
#   # remove if OS studies are missing - work with complete cases only
#   complete_data_rows <- !is.na(dataset$OS_LHR_mean)
#   dataset <- dataset[complete_data_rows,]
#   
#   jagsData <- list(nStudies       = nrow(dataset),   
#                    y              = dataset$OS_LHR_mean,
#                    sigma          = dataset$OS_LHR_se,
#                    indicationNum  = dataset$indication_index, 
#                    nIndications   = max(dataset$indication_index))
#   
#   # initial values required for each chain, each with different starting values
#   initialValues <- list(
#     list(theta     = rep(0, nrow(dataset)),
#          #tau.theta = rep(1, max(dataset$indication_index)),
#          mu        = 0,
#          .RNG.name = "base::Wichmann-Hill", # *
#          .RNG.seed = 1),
#     list(theta     = rep(-5, nrow(dataset)),
#          #tau.theta = rep(0.001, max(dataset$indication_index)),
#          mu        = 5,
#          .RNG.name = "base::Wichmann-Hill", # *
#          .RNG.seed = 2),
#     list(theta     = rep(5, nrow(dataset)),
#          #tau.theta = rep(2, max(dataset$indication_index)),
#          mu        = -5,
#          .RNG.name = "base::Wichmann-Hill", # *
#          .RNG.seed = 3)
#   )
#   
#   parameters <- c("theta", "mu" )# , "tau.theta")
#   
#   n.adapt = 5000              
#   burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
#   nChains = 3                   # Number of chains to run.
#   numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
#   thinSteps=1                   # Number of steps to "thin" (1=keep every step).
#   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
#   
#   # Create, initialize, and adapt the model:
#   jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/ce_homo/ce_homo.txt"), # where is BUGS model 
#                           data = jagsData, 
#                           inits = initialValues, 
#                           n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
#   # Burn-in:
#   update( jagsModel , n.iter=burnInSteps , progress.bar = "none" )
#   # The saved MCMC chain:
#   # cat( "Sampling final MCMC chain...\n" )
#   codaSamples = coda.samples( jagsModel , variable.names=parameters ,
#                               n.iter=nIter , thin=thinSteps, progress.bar = "none")
#   
#   # return result
#   output <- list(
#     codaSamples_OS = codaSamples
#   )
#   output
#   
# }
# 
# 
# applyRe_homo_JAGS <- function(dataset, model_name, i ){
#   
#   # remove if OS studies are missing - work with complete cases only
#   complete_data_rows <- !is.na(dataset$OS_LHR_mean)
#   dataset <- dataset[complete_data_rows,]
#   
#   jagsData <- list(nStudies       = nrow(dataset),   
#                    y              = dataset$OS_LHR_mean,
#                    sigma          = dataset$OS_LHR_se,
#                    indicationNum  = dataset$indication_index, 
#                    nIndications   = max(dataset$indication_index))
#   
#   initialValues <- list(
#     list(theta     = rep(0, nrow(dataset)),
#          #tau.theta = rep(1, max(dataset$indication_index)),
#          mMu       = 0,
#          mTau      = 1,
#          .RNG.name = "base::Wichmann-Hill", # *
#          .RNG.seed = 1),
#     list(theta     = rep(-5, nrow(dataset)),
#          #tau.theta = rep(0.001, max(dataset$indication_index)),
#          mMu       = -5,
#          mTau      = 0.001,
#          .RNG.name = "base::Wichmann-Hill", # *
#          .RNG.seed = 2),
#     list(theta     = rep(5, nrow(dataset)),
#          #tau.theta = rep(2, max(dataset$indication_index)),
#          mMu       = 5,
#          mTau      = 2,
#          .RNG.name = "base::Wichmann-Hill", # *
#          .RNG.seed = 3)
#   )
#   
#   parameters <- c("theta", "mu.theta", "mMu", "mTau", "mPred") # ,"tau.theta")
#   
#   n.adapt = 5000              
#   burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
#   nChains = 3                   # Number of chains to run.
#   numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
#   thinSteps=1                   # Number of steps to "thin" (1=keep every step).
#   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
#   
#   # Create, initialize, and adapt the model:
#   jagsModel <- jags.model(file = paste0(getwd(), "/JAGS BUGS models/univariate/re_homo/re_homo.txt"), # where is BUGS model 
#                           data = jagsData, 
#                           inits = initialValues, 
#                           n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
#   # Burn-in:
#   update( jagsModel , n.iter=burnInSteps , progress.bar = "none" )
#   # The saved MCMC chain:
#   # cat( "Sampling final MCMC chain...\n" )
#   codaSamples = coda.samples( jagsModel , variable.names=parameters ,
#                               n.iter=nIter , thin=thinSteps, progress.bar = "none" )
#   # return result
#   output <- list(
#     codaSamples_OS = codaSamples
#   )
#   output
#   
# }


