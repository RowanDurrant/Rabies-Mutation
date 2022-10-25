using Distributions
function GenCalc(time::Float64, densities::DataFrame)
    subDens = Vector(densities[ceil(Int, time),:])
    subDensStack = subDens/sum(subDens)
    noSteps = wsample(1:ncol(densities), subDensStack, 1)
    return(noSteps)
end