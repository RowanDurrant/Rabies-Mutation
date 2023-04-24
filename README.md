# Rabies Mutation
Creator: Rowan Durrant - School of Biodiversity, One Health & Veterinary Medicine, Glasgow University

Contact me: r.durrant.1@research.ac.uk

Description: Repo for code and data used in the manuscipt "Determining the Relationship Between Time, Transmission and the Evolution of the Rabies Virus".


## Input
Data files needed to run these scripts.
- [EF_input](input/EF_input) - output from a branching process simulation that we use as a starting point to simulate mutations on top of.
- [Pemba_assigment.csv](input/Pemba_assignment.csv) - Lineages of each sequence in the Pemba dataset, assigned using [MADDOG](https://github.com/KathrynCampbell/MADDOG).


## Code: How does rabies mutate?
- [simulate_perfect_clockrate.R](code/simulate_perfect_clockrate.R) takes simulation output and generates mutations onto cases using a clock rate model (mutations accumulate per unit time). Uses [mutations_function_time.R](code/mutations_function_time.R).
- [simulate_per_generation.R](code/simulate_per_generation.R) does the same, but with a per-generation mutation model. Uses [mutations_function.R](code/mutations_function.R).
- [compare_clockrate_gen_divergence.R](code/compare_clockrate_gen_divergence.R) makes divergence-time plots and compares R^2 values for both per-time and per-generation simulations.
- [4_nice_graphs.R](code/4_nice_graphs.R) - makes graphs

## Code: What is RABV's mutation rate?
### Simulations to test accuracy
REDO

### Applications to Pemba dataset
REDO


## Output
Some output files are too big for GitHub! (But the scripts needed to generate them are available)
- [simulation](output/simulation) - folder with output files relating to all the simulated methods, including divergence-time plot related data and mutation rate calculation method accuracy. Raw simulation output files aren't uploaded here (too hefty/messy) but can be made available upon request.
- [pemba](output/pemba) - folder with output files relating to calculating the per-generation substitution rate from real RABV sequences.

## Plots
Files of plots used in the manuscript.

## Communication
Posters and presentations I've used to communicate this project.
