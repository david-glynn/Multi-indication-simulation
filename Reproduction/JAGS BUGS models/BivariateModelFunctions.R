# convert from BUGS to JAGS
# https://doingbayesiandataanalysis.blogspot.com/2012/09/from-bugs-with-brugs-to-jags-with-rjags.html

# dataset <- df_fit 
# model_name <- "1.1_bi_ie_early"

# Surrogate model with common (C) parameters (P)
apply_bi_ce <- function(dataset){
  
  # remove if OS studies are missing 
  # this will remove indication 8 when estimating surrogacy model as it is not used when there is OS or when there is no OS
  complete_data_rows <-  !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- with(dataset, list(nStudies       = nrow(dataset),   
                                 Y              = cbind(PFS_LHR_mean, OS_LHR_mean),
                                 #y1             = PFS_LHR_mean,
                                 sigma1         = PFS_LHR_se,
                                 #y2             = OS_LHR_mean,
                                 sigma2         = OS_LHR_se,
                                 indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                                 nIndications   = length(unique(dataset$indication_index))))
  
  initialValues <- with(jagsData,
                        list(
                          list(Mu       = matrix(0, nrow(dataset), 2),
                               rhoW     = 0.5,
                               mLambda0 = 0,
                               mLambda1 = 0,
                               mPsi     = 1,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1),
                          list(Mu       = matrix(5, nrow(dataset), 2),
                               rhoW     = 0.001,
                               mLambda0 = 5,
                               mLambda1 = 5,
                               mPsi     = 0.001,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1),
                          list(Mu       = matrix(-5, nrow(dataset), 2),
                               rhoW     = 0.998,
                               mLambda0 = -5,
                               mLambda1 = -5,
                               mPsi     = 2,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1)
                        )
  )
  
  parameters <- c("rhoW", "lambda0", "lambda1", "psi",
                  "mLambda0", "mLambda1", "mPsi")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  jagsModel <- jags.model(file = "JAGS BUGS models/bivariate/bivariatecp.txt",
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  
  update( jagsModel , n.iter=burnInSteps, progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps , progress.bar = "none")
  
  codaSamples
  
}

# # plot surrogacy in the largest indication and compare estimate in codaSamples
# # source("Plotting functions.R")
# fn_plot_indication_surrogacy(df_data_set = df_fit[df_fit$indication_index==2,], plot_title="a")
# m_codaSamples <- as.matrix(codaSamples)
# abline(a = mean(m_codaSamples[, "mLambda0"]), b = mean(m_codaSamples[, "mLambda1"]))
# summary(codaSamples)


# Surrogate model with random (R) parameters (P)
apply_bi_re <- function(dataset){
  
  # remove if OS studies are missing i.e. remove indication 8 when estimating surrogacy model as it is not used when there is OS or when there is no OS
  complete_data_rows <-  !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- with(dataset, list(nStudies       = nrow(dataset),   
                                 Y              = cbind(PFS_LHR_mean, OS_LHR_mean),
                                 #y1             = PFS_LHR_mean,
                                 sigma1         = PFS_LHR_se,
                                 #y2             = OS_LHR_mean,
                                 sigma2         = OS_LHR_se,
                                 indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                                 nIndications   = length(unique(dataset$indication_index))))

  initialValues <- with(jagsData, 
                        list(
                          list(Mu      = matrix(0, nrow(dataset), 2),
                               rhoW    = 0.5,
                               lambda0 = rep(0, nIndications),
                               lambda1 = rep(0, nIndications),
                               psi     = rep(1, nIndications),
                               beta0   = 0, # pooled mean for lambda0
                               beta1   = 0, # pooled mean for lambda1
                               xi0     = 1, # between trial SE for lambda0
                               xi1     = 1, # between trial SE for lambda1
                               h       = 0.5, # between indications variance parameter for psi (gamma distributed)
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1
                               ),
                          list(Mu      = matrix(5, nrow(dataset), 2),
                               rhoW    = 0.001,
                               lambda0 = rep(5, nIndications),
                               lambda1 = rep(5, nIndications),
                               psi     = rep(0.001, nIndications),
                               beta0   = 5,
                               beta1   = 5,
                               xi0     = 0.001,
                               xi1     = 0.001,
                               h       = 0.5,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1
                               ),
                          list(Mu      = matrix(-5, nrow(dataset), 2),
                               rhoW    = 0.998,
                               lambda0 = rep(-5, nIndications),
                               lambda1 = rep(-5, nIndications),
                               psi     = rep(2, nIndications),
                               beta0   = -5,
                               beta1   = -5,
                               xi0     = 2,
                               xi1     = 2,
                               h       = 0.5,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1
                               )
                        ))
  
  parameters <- c("rhoW", "lambda0", "lambda1", "psi", 
                  "beta0", "beta1", "prec0", "prec1", "xi0", "xi1", "h",
                  "lambda0Pred", "lambda1Pred", "psiPred")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=10                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel <- jags.model(file = "JAGS BUGS models/bivariate/bivariaterp.txt",
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  
  # Burn-in:
  update( jagsModel , n.iter=burnInSteps, progress.bar = "none" )
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps , progress.bar = "none")
  
  codaSamples
  
}


# Surrogate model with common (C) parameters (P)
apply_bi_one_indication <- function(dataset, thinSteps,
                                    jags_file){
  
  # remove if OS studies are missing 
  # this will remove indication 8 when estimating surrogacy model as it is not used when there is OS or when there is no OS
  complete_data_rows <-  !is.na(dataset$OS_LHR_mean)
  dataset <- dataset[complete_data_rows,]
  
  jagsData <- with(dataset, list(nStudies       = nrow(dataset),   
                                 Y              = cbind(PFS_LHR_mean, OS_LHR_mean),
                                 #y1             = PFS_LHR_mean,
                                 sigma1         = PFS_LHR_se,
                                 #y2             = OS_LHR_mean,
                                 sigma2         = OS_LHR_se))
                                 # indicationNum  = rep(1:length(unique(dataset$indication_index)), table(dataset$indication_index)), 
                                 # nIndications   = length(unique(dataset$indication_index))))
  
  initialValues <- with(jagsData,
                        list(
                          list(Mu       = matrix(0, nrow(dataset), 2),
                               rhoW     = 0.5,
                               mLambda0 = 0,
                               mLambda1 = 0,
                               mPsi     = 1,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1),
                          list(Mu       = matrix(5, nrow(dataset), 2),
                               rhoW     = 0.001,
                               mLambda0 = 5,
                               mLambda1 = 5,
                               mPsi     = 0.001,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1),
                          list(Mu       = matrix(-5, nrow(dataset), 2),
                               rhoW     = 0.998,
                               mLambda0 = -5,
                               mLambda1 = -5,
                               mPsi     = 2,
                               .RNG.name = "base::Wichmann-Hill", # *
                               .RNG.seed = 1)
                        )
  )
  
  parameters <- c("rhoW", 
                  "mLambda0", "mLambda1", "mPsi", "Mu")
  
  n.adapt = 5000              
  burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
  thinSteps=thinSteps                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  jagsModel <- jags.model(file = jags_file,
                          data = jagsData, 
                          inits = initialValues, 
                          n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
  
  update( jagsModel , n.iter=burnInSteps)
  # The saved MCMC chain:
  # cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters ,
                              n.iter=nIter , thin=thinSteps)
  
  codaSamples
  
}



### old code #####

# # Surrogate model with independent (I) parameters (P) - not possible in my case
# apply_bi_ie <- function(dataset, model_name, # which model_name is being run e.g. 1.1, 1.2 assign using df_scenarios_full$model_name[s]
#                         i # which iteration is being run
# ){
#   
#   # remove if PFS studies are missing ()
#   complete_data_rows <-  !is.na(dataset$PFS_LHR_mean)
#   dataset <- dataset[complete_data_rows,]
#   
#   jagsData <- with(dataset, list(nStudies       = sum(complete.cases(dataset)),   # allows for missing OS studies
#                                  y1             = PFS_LHR_mean,
#                                  sigma1         = PFS_LHR_se,
#                                  y2             = OS_LHR_mean,
#                                  sigma2         = OS_LHR_se,
#                                  nIndications   = length(unique(indication_index)),
#                                  indicationNum  = unique(indication_index)))
#   
#   initialValues <- with(jagsData,
#                         list(
#                           list(Mu      = matrix(0, nrow(dataset), 2),
#                                rhoW    = 0.5,
#                                lambda0 = rep(0, nIndications),
#                                lambda1 = rep(0, nIndications),
#                                psi     = rep(1, nIndications),
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1),
#                           list(Mu      = matrix(5, nrow(dataset), 2),
#                                rhoW    = 0.001,
#                                lambda0 = rep(5, nIndications),
#                                lambda1 = rep(5, nIndications),
#                                psi     = rep(0.001, nIndications),
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1),
#                           list(Mu      = matrix(-5, nrow(dataset), 2),
#                                rhoW    = 0.998,
#                                lambda0 = rep(-5, nIndications),
#                                lambda1 = rep(-5, nIndications),
#                                psi     = rep(2, nIndications),
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1)
#                         ))
#   
#   parameters <- c("rhoW", "lambda0", "lambda1", "psi")
#   
#   # all the code below is new
#   # number of iterations that JAGS will use to choose the sampler and to assure optimum mixing of the MCMC chain
#   # Number of steps to "tune" the samplers. 
#   n.adapt = 5000              
#   burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
#   nChains = 3                   # Number of chains to run.
#   numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
#   thinSteps=1                   # Number of steps to "thin" (1=keep every step).
#   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
#   
#   # Create, initialize, and adapt the model:
#   jagsModel <- jags.model(file = "JAGS BUGS models/bivariate/bivariateip.txt",
#                           data = jagsData, 
#                           inits = initialValues, 
#                           n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
#   
#   # Burn-in:
#   update( jagsModel , n.iter=burnInSteps, progress.bar = "none" )
#   # The saved MCMC chain:
#   # cat( "Sampling final MCMC chain...\n" )
#   codaSamples = coda.samples( jagsModel , variable.names=parameters ,
#                               n.iter=nIter , thin=thinSteps , progress.bar = "none")
#   
#   codaSamples
# }


# # Surrogate model with mixed (M) common (C) and independent (I) parameters (P)
# apply_bi_mcie <- function(dataset, model_name, i){
#   
#   # remove if PFS studies are missing ()
#   complete_data_rows <-  !is.na(dataset$PFS_LHR_mean)
#   dataset <- dataset[complete_data_rows,]
#   
#   jagsData <- with(dataset, list(nStudies       = sum(complete.cases(dataset)),   # allows for missing OS studies
#                                  y1             = PFS_LHR_mean,
#                                  sigma1         = PFS_LHR_se,
#                                  y2             = OS_LHR_mean,
#                                  sigma2         = OS_LHR_se,
#                                  nIndications   = length(unique(indication_index)),
#                                  indicationNum  = unique(indication_index)))
#   
#   initialValues <- with(jagsData, 
#                         list(
#                           list(Mu      = matrix(0, nrow(dataset), 2),
#                                rhoW    = 0.5,
#                                branch0 = t(matrix(c(NA, 5), 2, nIndications)),
#                                branch1 = t(matrix(c(NA, 5), 2, nIndications)),
#                                branch2 = t(matrix(c(NA, 5), 2, nIndications)),
#                                mPsi    = 1,
#                                mLambda0   = 0,
#                                mLambda1   = 0,
#                                p0      = rep(0.5, nIndications),
#                                p1      = rep(0.5, nIndications),
#                                p2      = rep(0.5, nIndications),
#                                c0      = rep(1, nIndications),
#                                c1      = rep(1, nIndications),
#                                c2      = rep(1, nIndications),
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1
#                           ),
#                           list(Mu      = matrix(5, nrow(dataset), 2),
#                                rhoW    = 0.001,
#                                branch0 = t(matrix(c(NA, 5), 2, nIndications)),
#                                branch1 = t(matrix(c(NA, 5), 2, nIndications)),
#                                branch2 = t(matrix(c(NA, 5), 2, nIndications)),
#                                mPsi    = 0.001,
#                                mLambda0   = 5,
#                                mLambda1   = 5,
#                                p0      = rep(0.5, nIndications),
#                                p1      = rep(0.5, nIndications),
#                                p2      = rep(0.5, nIndications),
#                                c0      = rep(1, nIndications),
#                                c1      = rep(1, nIndications),
#                                c2      = rep(1, nIndications),
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1
#                           ),
#                           list(Mu      = matrix(-5, nrow(dataset), 2),
#                                rhoW    = 0.998,
#                                branch0 = t(matrix(c(NA, 5), 2, nIndications)),
#                                branch1 = t(matrix(c(NA, 5), 2, nIndications)),
#                                branch2 = t(matrix(c(NA, 5), 2, nIndications)),
#                                mPsi    = 2,
#                                mLambda0   = -5,
#                                mLambda1   = -5,
#                                p0      = rep(1, nIndications),
#                                p1      = rep(1, nIndications),
#                                p2      = rep(1, nIndications),
#                                c0      = rep(0, nIndications),
#                                c1      = rep(0, nIndications),
#                                c2      = rep(0, nIndications),
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1
#                           )
#                         ))
#   
#   parameters <- c("rhoW", "lambda0", "lambda1", "psi", 
#                   "mLambda0", "mLambda1", "mPsi", 
#                   "p0", "p1", "p2", "c0", "c1", "c2")
#   
#   n.adapt = 5000              
#   burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
#   nChains = 3                   # Number of chains to run.
#   numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
#   thinSteps=1                   # Number of steps to "thin" (1=keep every step).
#   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
#   
#   # Create, initialize, and adapt the model:
#   jagsModel <- jags.model(file = "JAGS BUGS models/bivariate/bivariatemcip.txt",
#                           data = jagsData, 
#                           inits = initialValues, 
#                           n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
#   
#   # Burn-in:
#   update( jagsModel , n.iter=burnInSteps, progress.bar = "none" )
#   # The saved MCMC chain:
#   # cat( "Sampling final MCMC chain...\n" )
#   codaSamples = coda.samples( jagsModel , variable.names=parameters ,
#                               n.iter=nIter , thin=thinSteps , progress.bar = "none")
#   
#   codaSamples
#   
# }
# 
# 
# # Surrogate model with mixed (M) random (R) and independent (I) parameters (P)
# apply_bi_mrie <- function(dataset, model_name, i){
#   
#   # remove if PFS studies are missing ()
#   complete_data_rows <-  !is.na(dataset$PFS_LHR_mean)
#   dataset <- dataset[complete_data_rows,]
#   
#   jagsData <- with(dataset, list(nStudies       = sum(complete.cases(dataset)),   # allows for missing OS studies
#                                  y1             = PFS_LHR_mean,
#                                  sigma1         = PFS_LHR_se,
#                                  y2             = OS_LHR_mean,
#                                  sigma2         = OS_LHR_se,
#                                  nIndications   = length(unique(indication_index)),
#                                  indicationNum  = unique(indication_index)))
#   
#   initialValues <- with(jagsData, 
#                         list(
#                           list(Mu      = matrix(0, nrow(dataset), 2),
#                                rhoW    = 0.5,
#                                branch0 = matrix(5, nIndications, 2),
#                                branch1 = matrix(5, nIndications, 2),
#                                branch2 = matrix(5, nIndications, 2),
#                                beta0   = 0,
#                                beta1   = 0,
#                                xi0     = 1,
#                                xi1     = 1,
#                                p0      = rep(0.5, nIndications),
#                                p1      = rep(0.5, nIndications),
#                                p2      = rep(0.5, nIndications),
#                                c0      = rep(1, nIndications),
#                                c1      = rep(1, nIndications),
#                                c2      = rep(1, nIndications),
#                                h       = 0.5,
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1
#                           ),
#                           list(Mu      = matrix(5, nrow(dataset), 2),
#                                rhoW    = 0.001,
#                                branch0 = matrix(5, nIndications, 2),
#                                branch1 = matrix(5, nIndications, 2),
#                                branch2 = matrix(5, nIndications, 2),
#                                beta0   = 5,
#                                beta1   = 5,
#                                xi0     = 0.001,
#                                xi1     = 0.001,
#                                p0      = rep(0.5, nIndications),
#                                p1      = rep(0.5, nIndications),
#                                p2      = rep(0.5, nIndications),
#                                c0      = rep(1, nIndications),
#                                c1      = rep(1, nIndications),
#                                c2      = rep(1, nIndications),
#                                h       = 0.5,
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1
#                           ),
#                           list(Mu      = matrix(-5, nrow(dataset), 2),
#                                rhoW    = 0.998,
#                                branch0 = matrix(5, nIndications, 2),
#                                branch1 = matrix(5, nIndications, 2),
#                                branch2 = matrix(5, nIndications, 2),
#                                beta0   = -5,
#                                beta1   = -5,
#                                xi0     = 2,
#                                xi1     = 2,
#                                p0      = rep(1, nIndications),
#                                p1      = rep(1, nIndications),
#                                p2      = rep(1, nIndications),
#                                c0      = rep(0, nIndications),
#                                c1      = rep(0, nIndications),
#                                c2      = rep(0, nIndications),
#                                h       = 0.5,
#                                .RNG.name = "base::Wichmann-Hill", # *
#                                .RNG.seed = 1
#                           )
#                         ))
#   
#   parameters <- c("rhoW", "lambda0", "lambda1", "psi", 
#                   "beta0", "beta1", "xi0", "xi1", "h", 
#                   "p0", "p1", "p2", "c0", "c1", "c2")
#   
#   n.adapt = 5000              
#   burnInSteps = nBurnin         # Number of steps to "burn-in" the samplers.
#   nChains = 3                   # Number of chains to run.
#   numSavedSteps= (nIterations - nBurnin)   # Total number of steps to save (distributed across all chains).
#   thinSteps=1                   # Number of steps to "thin" (1=keep every step).
#   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
#   
#   # Create, initialize, and adapt the model:
#   jagsModel <- jags.model(file = "JAGS BUGS models/bivariate/bivariatemrip.txt",
#                           data = jagsData, 
#                           inits = initialValues, 
#                           n.chains = 3, n.adapt=n.adapt, quiet=TRUE)
#   
#   # Burn-in:
#   update( jagsModel , n.iter=burnInSteps, progress.bar = "none" )
#   # The saved MCMC chain:
#   # cat( "Sampling final MCMC chain...\n" )
#   codaSamples = coda.samples( jagsModel , variable.names=parameters ,
#                               n.iter=nIter , thin=thinSteps , progress.bar = "none")
#   
#   codaSamples
#   
# }