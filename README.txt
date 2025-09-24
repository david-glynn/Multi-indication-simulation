## 0) Introduction ##
- This code is organised to be run on a cluster computer in a series of stages, but it is possible to run on a desktop.

## 1) Choose scenarios to run ##
- Define scenarios required in "Define scenarios DESKTOP.csv" i.e. degree of within and between indication heterogeneity. 
- A key for indexing scenarios is provided at the end of this document (point 5)

## 2) Generate datasets and estimate models ##
- Open "SimulationLoop.R"
- Install and load the required packages.
- The user will be defined as "David" by default
- Choose the number of iterations to simulate (n_sim) 
- Run the loop, this will generate datasets and fit models for the chosen scenarios defined above (This will take about 6 minutes). 
- The datasets and models generated will be stored in subfolders in "Desktop simulation results David/Datasets" and "Desktop simulation results David/Model_posterior_summary"
(warning: the file names and folders can get long so if you get the error "cannot open file 'Desktop simulation results David/Model_posterior_summary/MCMC_Output_1026_uni_ie_early_i1.csv': No such file or directory" this may be because the path length is longer than 260)

## 3) Calculate average model performance against the true values ##
- Open "CalculateResults.R"
- Run the full script. This will calculate average model performance over the n_sim iterations for the models and scenarios defined. 
- Results will be saved in "Desktop simulation results David/Result_elements"
(note: results will be missing if the models do not converge, with low values of n_sim this can result in warnings)

## 4) View results ##
- Open the folder "Desktop simulation results David/Result_elements".
- The results for scenario indexed 1026 for early dataset with no overall survival data in the target indication will be in a file called "1026_early_no OS.csv". Files will be created for each index chosen, each time point (early, mid, late) and for scenarios with and wihtout overall survival in the target indication.
- These files will report average performance metrics for each of the models considered.
(note: results will be missing if the models do not converge, with low values of n_sim this can result in "NA" results for some models)

## 5) Key for indexing scenarios ##

Within indication hetero: add
0% => + 1
7% => + 6
15% => + 11
30% => +16
50% => + 21 
Between indication hetero: add
0% => + 0
7% => + 1
15% => + 2
30% => +3
50% => + 4 
Effect size: 
Small effect => + 0 (no longer applicable)
Large effect => + 25 (always need to add this)
Set
Set 1 => + 1000
Set 2 => + 2000
Set 3 => + 3000
Set 4 => + 4000

E.g. 30% within, 50% between, large effect in set 3. Index = 16 + 4 + 25 + 3000 = 3045.

