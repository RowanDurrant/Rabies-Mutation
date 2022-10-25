# Rabies-Mutation-Rates

Repo for code relating to the manuscipt on how and how much rabies mutates.

## Input
Data files needed to run these scripts. NB: all files needed may not be publicly available due to this project relying on people kindly letting me use their unpublished data!

## Code: How does rabies mutate?
- [simulate_perfect_clockrate.R](code/simulate_perfect_clockrate.R) takes simulation output and generates mutations onto cases using a clock rate model (mutations accumulate per unit time). Uses [mutations_function_time.R](code/mutations_function_time.R).
- [simulate_per_generation.R](code/simulate_per_generation.R) does the same, but with a per-generation mutation model. Uses [mutations_function.R](code/mutations_function.R).
- [compare_clockrate_gen_divergence.R](code/compare_clockrate_gen_divergence.R) makes divergence-time plots and compares R^2 values for both per-time and per-generation simulations.
- [4_nice_graphs.R](code/4_nice_graphs.R) - makes graphs

## Code: What is RABV's mutation rate?
(NOTE: clock rate method calculation was done in excel, so R script for this doesn't exist, and following scripts are for novel method & comparison only)
### Simulations to test accuracy
The scripts below are all packaged up and sourced in one overall script, [sim_analysis_main.R](code/sim_analysis_main.R):
- [1_probability_density_matrix.R](code/1_probability_density_matrix.R) - generates the time/generations density matrix
- [2b_sim_output_prep.R](code/2b_sim_output_prep.R) - Takes simulation output and simulates accumulation of mutations down the chains using [mutations_function.R](code/mutations_function.R)
- [prep_sim_trees.R](code/prep_sim_trees.R) - takes a phylogenetic tree in the MEGA X table format and makes it usable
- [sim_calcs.R](code/sim_calcs.R) - calculates number of generations between sequences and per generation mutation rate. Uses [generations_calculation_function.R](code/generations_calculation_function.R)
- [compare_sim_gens.R](code/compare_sim_gens.R) - compares the number of predicted generations between simulated cases with the actual number of generations. 
- [4_nice_graphs.R](code/4_nice_graphs.R) - makes graphs

### Applications to Pemba dataset
- [1_probability_density_matrix.R](code/1_probability_density_matrix.R) - generates the time/generations density matrix (NOTE: density matrix for empirical data is not available in this repo bc the file was too big to upload, you'll have to generate this on your own machine and it takes a while)
- [2a_prep_phylo_trees.R](code/2a_prep_phylo_trees.R) - takes a phylogenetic tree in the MEGA X table format and makes it usable
- [3_main_calculations.R](code/3_main_calculations.R) - calculates number of generations between sequences and per generation mutation rate. Uses [generations_calculation_function.R](code/generations_calculation_function.R)
- [4_nice_graphs.R](code/4_nice_graphs.R) - makes graphs

### Output files
Some output files are too big for GitHub! (But the scripts needed to generate them are available)

### Plots
Files of plots used in manuscript.

### Communication
Posters and presentations I've used to communicate this project.
