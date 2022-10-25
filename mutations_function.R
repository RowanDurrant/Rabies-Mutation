viralSeq = function(seq, perBaseRate){
  mutRate = perBaseRate * (4/3)#rate of mutation per base or whatever
  splitSeq = strsplit(seq, split = "")[[1]]
  
  noMuts = rpois(1, length(splitSeq)*mutRate)
  if(noMuts > 0){
    replaceBases = sample(1:length(splitSeq), noMuts, replace = F)
    for(i in replaceBases){
      splitSeq[i] = sample(c("a", "t", "c", "g"), 1)
    }
  }
  
  mutatedSeq = paste(splitSeq, collapse = "")
  return(mutatedSeq)
}