#simple simulation of how many generations occur before at least one SNP arises

Generations = c()
for(i in 1:10000){
  noSNPs = 0
  gens = 0
  while(noSNPs == 0){
    gens = gens + 1
    noSNPs = noSNPs + rpois(1, 0.17)
  }
  Generations = append(Generations, gens)
}
hist(Generations)
length(Generations[Generations < 5])/length(Generations)
