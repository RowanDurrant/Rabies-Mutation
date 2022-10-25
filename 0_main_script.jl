using DataFrames
using CSV
using BenchmarkTools

include("code/julia/generations_calculation_function.jl")

densities = CSV.read("output/pemba/densities.csv", DataFrame)
densities = densities[:, 2:ncol(densities)]

NoReps = 100
genomeLength = 12000

@time include("code/julia/3_main_calculations.jl")