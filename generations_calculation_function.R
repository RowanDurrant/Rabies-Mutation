##generations calculation function

gencalc = function(time){
  roundedTime = round(time)
  if(roundedTime == 0){noSteps = 1}
  else{
    subDens = as.numeric(densities[roundedTime,])
    subDensStack = subDens/sum(subDens)
    noSteps = sample(1:ncol(densities), 1, prob = subDensStack)
  }
  
  return(noSteps)
  
}