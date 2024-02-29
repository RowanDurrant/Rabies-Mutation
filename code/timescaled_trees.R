library(ape)
library(ggplot2)
library(viridisLite)
require(stats)
library(treeio)
##usual params- 2x10^-4 subs/site/yr mutation rate, 11923 bases seq length

seeds = c(1, 2, 3)
branches = c(43, 12, 34)

percents = c(1,2.5,5,7.5,10,20,30,40,50,60,70,80,90)
reps = 1:5
proportion = c()
outbreak_size = c()
outbreak_id = c()
outbreak_length = c()
mean_branchlength = c()
sd_branchlength = c()

for(g in 1:length(seeds)){
  # epidata = read.csv(paste0("seed",seeds[g],"outbreak",branches[g],"/simFullCases_perTime_",seeds[g],"_2e-04_",branches[g],".csv"))
  # 
  # mytree = read.tree(paste0("seed",seeds[g],"outbreak",branches[g],"/trees/simFullCases_",seeds[g],"_2e-04_",branches[g],".tre"))
  # 
  # mean_branchlength = append(mean_branchlength, mean(mytree$edge.length))
  # sd_branchlength = append(sd_branchlength, sd(mytree$edge.length))
  # 
  # proportion = append(proportion, 1)
  # outbreak_size = append(outbreak_size, nrow(epidata))
  # outbreak_id = append(outbreak_id, paste0(seeds[g],"_",branches[g],"_100"))
  # outbreak_length = append(outbreak_length, max(epidata$transD))
  
  for(k in reps){
    
    for(j in percents){
      if(file.exists(paste0("seed",seeds[g],"outbreak",branches[g],"/trees/sequences_",k,"_",j,".fasta.treefile"))){
        
        mytree = read.beast(paste0("seed",seeds[g],"outbreak",branches[g],"/trees/sequences_",k,"_",j,".tre"))
        mean_branchlength = append(mean_branchlength, mean(mytree@phylo$edge.length))
        sd_branchlength = append(sd_branchlength, sd(mytree@phylo$edge.length))
        proportion = append(proportion, (length(mytree@phylo$tip.label)/nrow(epidata)))
        outbreak_size = append(outbreak_size, nrow(epidata))
        outbreak_id = append(outbreak_id, paste0(seeds[g],"_",branches[g],"_",k))
        outbreak_length = append(outbreak_length, max(epidata$transD))
        
        
      }
      
    }
    
    
  }
  
}

library(dplyr)
df = data.frame(cbind(proportion, mean_branchlength, sd_branchlength, outbreak_size, outbreak_length))
df = mutate_all(df, function(x) as.numeric(as.character(x)))

ggplot(data = df, aes(x = proportion, 
                           y = mean_branchlength,
                           colour = outbreak_size))+
  geom_point(alpha = 0.8) + 
  scale_colour_viridis_c()+
  ylab("Mean branch length") + xlab("% cases sequenced") +
  ggtitle("2x10^-4 subs/site/yr, full genome,\nfull outbreak, timetrees")+
  ylim(0,150) + xlim(0,1) +
  stat_function(fun = function(x) (26.15599)/(2*sqrt(x)), colour = "black")+
  theme(legend.position = "none")

