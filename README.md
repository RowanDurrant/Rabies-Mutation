# Rabies Mutation
Creator: Rowan Durrant - School of Biodiversity, One Health & Veterinary Medicine, Glasgow University

Contact me: r.durrant.1@research.ac.uk

Description: Repo for code and data used in the manuscipt "Determining the Relationship Between Time, Transmission and the Evolution of the Rabies Virus".


## Input
Data files needed to run these scripts.
- [EF_input](input/EF_input) - output from a branching process simulation that we use as a starting point to simulate mutations on top of.
- [Pemba_assigment.csv](input/Pemba_assignment.csv) - Lineages of each sequence in the Pemba dataset, assigned using [MADDOG](https://github.com/KathrynCampbell/MADDOG).
- [Pemba.csv](input/pemba.csv) - Pemba timescaled tree in MEGA X's table format.

## Code: How does rabies mutate?
- [simulate_perfect_clockrate.R](code/simulate_perfect_clockrate.R) takes simulation output and generates mutations onto cases using a clock rate model (mutations accumulate per unit time). Uses [mutations_function_time.R](code/mutations_function_time.R).
- [simulate_per_generation.R](code/simulate_per_generation.R) does the same, but with a per-generation mutation model. Uses [mutations_function.R](code/mutations_function.R).
- [compare_clockrate_gen_divergence.R](code/compare_clockrate_gen_divergence.R) makes divergence-time plots and compares R^2 values for both per-time and per-generation simulations.
- [4_nice_graphs.R](code/4_nice_graphs.R) - makes graphs

## Code: What is RABV's mutation rate?
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
- [5_snp_distrib_param_calcs.R](code/5_snp_distrib_param_calcs.R) - fits distribution curves to the novel method's output.
- [clock_rate_method_pemba_distribution.R](code/clock_rate_method_pemba_distribution.R) - as above, but for the clock rate method.
- [SNP_probs_1_gen.R](code/SNP_probs_1_gen.R) - calculates the probability of a number of SNPs occuring over one generation, given the mean SNPs/gen values from the novel and clock rate methods.

Steps 2a and 3 are run through an overall script, [0_overall_script.R](code/0_overall_script.R)

### Prefer Julia?
Some scripts are also available in [Julia](https://julialang.org/)! 
- [0_main_script.jl](0_main_script.jl)
- [3_main_calculations.jl](code/3_main_calculations.jl) - Using the Julia version runs about 4x quicker than the R version!
- [generations_calculation_function.jl](code/generations_calculation_function.jl)

## Output
Some output files are too big for GitHub! (But the scripts needed to generate them are available)
- [simulation](output/simulation) - folder with output files relating to all the simulated methods, including divergence-time plot related data and mutation rate calculation method accuracy. Raw simulation output files aren't uploaded here (too hefty/messy) but can be made available upon request.
- [pemba](output/pemba) - folder with output files relating to calculating the per-generation substitution rate from real RABV sequences. The full bootstrapped novel method output is too big to upload but can be made available on request.

## Plots
Files of plots used in the manuscript.

## Communication
Posters and presentations I've used to communicate this project.
