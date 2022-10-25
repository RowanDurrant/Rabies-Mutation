using DataFrames
using CSV

alnDist = CSV.read("output/pemba/snpdistancesall.csv", DataFrame)
treeYearsUnpaired = CSV.read("output/pemba/treeYearsUnpaired.csv", DataFrame)
tips = unique(treeYearsUnpaired[Bool[length(x)>5 for x in treeYearsUnpaired[!,:Desc]],:Desc])
 
using Phylo
tree = open(parsenewick, Phylo.path("C:/Users/User/Documents/Github/Rabies-Mutation-Rates/input/pemba tree newick.nwk"))

Tip1 = Vector{String}()
Tip2 = Vector{String}()
TimeDiff = Vector{Float64}()
Gens = Vector{Int64}()
snpsPerGen = Vector{Float64}()
snpDist = Vector{Float64}()

for j in tips
    for k in tips
        if (k > j)
            timeDiff = distance(tree, j, k) * 365

            for m in 1:NoReps
                push!(Tip1, j)
                push!(Tip2, k)
                append!(TimeDiff, timeDiff)
                gens = GenCalc(timeDiff, densities)
                append!(Gens, gens)
                snpdist = alnDist[Bool[x == j for x in alnDist[!,:1]], k] * genomeLength
                append!(snpDist, snpdist)
                append!(snpsPerGen, snpdist/gens)

            end

        end

    end
end

fullBootstraps = DataFrame(Tip1 = Tip1, Tip2 = Tip2, TimeDiff = TimeDiff, Gens = Gens, snpsPerGen = snpsPerGen, snpDist = snpDist)
