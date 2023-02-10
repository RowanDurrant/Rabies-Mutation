data = read.csv("output/simulation/simsampledtips_perGen_24_0.000166666666666667_124_621.7.csv")
library(stringr)
startSeq = paste(rep("a",12000), collapse = "")
data$divergence = NA
for(i in 1:nrow(data)){
  data$divergence[i] = (nchar(startSeq) - str_count(data$sequence[i], "a"))/nchar(startSeq)
  
}
data$rate = data$divergence/data$infD
par(mfrow=c(1,2))
plot(data$divergence ~ data$infD, 
     col = ifelse(data$rate > 8e-06 & data$infD > 750, "red", "darkgrey"),
     pch = 16,
     xlab = "Time (days)", ylab = "Divergence (subs./site)")
title("A")

ridgepoints = data[data$rate > 8e-06 & data$infD > 750,]
dataFull = read.csv("../Rabies-Mutation-Rates/output/simulation/simFullCases_perGen_24_0.000166666666666667_124.csv")
dataFull = dataFull[,2:ncol(dataFull)]
dataFull$from = as.character(dataFull$from)
dataFull$to = as.character(dataFull$to)

library(igraph)
g = graph.data.frame(dataFull, directed = F)
E(g)$length = dataFull$TimeDiff
E(g)$mode = "-"
plot(g,
     vertex.color = ifelse(names(V(g)) %in% ridgepoints$caseID , adjustcolor("red", alpha.f = 1),adjustcolor("darkgrey", alpha.f = .15)), 
     vertex.label.color = adjustcolor("black", .05),
     vertex.frame.color = ifelse(names(V(g)) %in% ridgepoints$caseID , adjustcolor("red", alpha.f = 1),adjustcolor("darkgrey", alpha.f = .15)),
     layout = layout_as_tree(g),
     vertex.label = NA,
     vertex.size = 2,
     margin = c(-.4,-.3,-.4,-.2))
title("B")
legend(x=-1, y=-0.7, c("In offshoot ridge","In main ridge"), pch=16,
       col=c("red", "darkgrey"), cex=.8, bty="n", ncol=1)
