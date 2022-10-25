# Rabies-Mutation-Rates

Repo for code relating to project on calculating per-generation mutation rates in rabies

## Preparation

- [1_probability_density_matrix.R](code/1_probability_density_matrix.R) - generates the time/generations density matrix (NOTE: density matrix for empirical data is not available in this repo bc the file was too big to upload, you'll have to generate this on your own machine and it takes a while)
- [2a_prep_phylo_trees.R](code/2a_prep_phylo_trees.R) - takes a phylogenetic tree in the MEGA X table format and makes it usable
- [2b_sim_output_prep.R](code/2b_sim_output_prep.R) - as above, but for simulated transmission trees. Simulates accumulation of mutations down the chains using [mutations_function.R](code/mutations_function.R)


## Main Calculations

- [3_main_calculations.R](code/3_main_calculations.R) - calculates number of generations between sequences and per generation mutation rate. Uses [generations_calculation_function.R](code/generations_calculation_function.R)
- [sim_calcs.R](code/sim_calcs.R) - as above, but for simulated data.

## Analysis

- [4_graphs.R](code/4_graphs.R) - makes graphs
- [compare_sim_gens.R](code/compare_sim_gens.R) - compares the number of predicted generations between simulated cases with the actual number of generations. 


Some output files are too big for GitHub! (But the scripts needed to generate them are available)
