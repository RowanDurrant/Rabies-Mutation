##STYLE GUIDE: ANYTHING NOVEL METHOD RED, ANYTHING CLOCK RATE BLUE
library(ggplot2)
library(patchwork)

#FIGURE 1
df = read.csv("output/simulation/clockrate-gen-model-comparison2.csv")

ggplot(data = df, aes(x = EquivalentPerGenRate, y = R_Squared, colour = Method)) +
  geom_point() +
  geom_smooth(se=F) +
  xlab("Equivalent Per-Generation Mutation Rate") + ylab("R Squared")+
  facet_wrap( ~PercentSampled) +
  theme_bw() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))+
  scale_color_manual(name='Mutation Model',
                     values=c('Clock Rate'='blue', 'Generation'='red'))
ggsave("time divergence transparent.png", bg = "transparent")


#FIGURE 3
novelMethodAccuracy = read.csv("output/simulation/novel_method_accuracy.csv")
clockRateMethodAccuracy = read.csv("output/simulation/clockrate_method_accuracy.csv")

p1 = ggplot(data = novelMethodAccuracy, aes(x = SNPRate, y = SNPsAccuracy, colour = "Novel Method")) +
  geom_point() + 
  geom_smooth(se = F, method="glm") +
  geom_point(data = clockRateMethodAccuracy, 
             aes(x = SNPRate, y = SNPsAccuracy, colour = "Clock Rate Method"))+
  geom_smooth(data = clockRateMethodAccuracy, 
              aes(x = SNPRate, y = SNPsAccuracy, colour = "Clock Rate Method"), 
              se = F, method="glm") +
  geom_hline(yintercept = 100, lty = 2) +
  theme_bw() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))+
  ylab("% Accuracy") + xlab("Input SNPs per Generation") +
  scale_color_manual(name='Prediction Method',
                     values=c('Clock Rate Method'='blue', 'Novel Method'='red'))

ggsave("plots/figure 1 transparent.png", bg = "transparent")


#FIGURE 4
p4 = ggplot(data = data.frame(x = c(0, 0.5)), aes(x)) +
  stat_function(fun = dlnorm, n = 101, args = list(meanlog =  -1.820404, sdlog = 0.3321436), aes(colour = "Clock Rate Method")) + 
  ylab("") +
  xlab("SNPs per Generation")+
  stat_function(fun=dgamma, args=list(shape = 10.01941, rate = 58.80515), aes(colour = "Novel Method")) +
  theme_bw() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))+
  scale_color_manual(name='Prediction Method',
                     values=c('Clock Rate Method'='blue', 'Novel Method'='red'))
ggsave("plots/figure 4 transparent.png", bg = "transparent")

#FIGURE 5
tipDists = read.csv("output/pemba/full_bootstraps_100.csv") #change if you changed no. reps
clusters = read.csv("input/pembaWGS.csv")
alnDist = read.csv("output/pemba/snpdistancesall.csv")
rownames(alnDist) <- alnDist[,1]
alnDist[,1] <- NULL
colnames(alnDist) = rownames(alnDist)
clusters = clusters[clusters$ID %in% rownames(alnDist),]
tipDists$TimeDiffYears = tipDists$TimeDiff / 365
lineages = unique(clusters$lineage)
lineageTipDists = head(tipDists, 0)


for(i in lineages){
  if(nrow(clusters[clusters$lineage == i,]) > 1){
    newLineage = tipDists[tipDists$Tip1 %in% clusters$ID[clusters$lineage == i] &
               tipDists$Tip2 %in% clusters$ID[clusters$lineage == i],]
    newLineage$Lineage = i
    lineageTipDists = rbind(lineageTipDists, newLineage)
  }
}

p5 = ggplot(data = lineageTipDists, aes(x = snpsPerGen)) +
  geom_density(alpha=.35, fill="#FF6666") +
  xlim(0,0.75)+
  facet_wrap(~ Lineage) +
  theme_bw() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))+
  xlab("SNPs per Generation")+
  geom_vline(xintercept = 0.17, lty = 2)
ggsave("plots/figure 5 transparent vline.png", bg = "transparent")

#FIGURE 6a

p6 = ggplot(data = lineageTipDists, aes(y = snpsPerGen, x = TimeDiffYears)) +
  geom_point(colour = "red", alpha = .01) +
  theme_bw() +
  ylab("SNPs per Generation") + xlab("Temporal Distance (Years)") +
  geom_hline(yintercept = mean(lineageTipDists$snpsPerGen), lty = 2)
#p6

#FIGURE 6b

simTipDists = read.csv("output/simulation/full_bootstraps_sim_281.66666666666667e-0510010002546.39977097979.csv")
simTipDists$TimeDiffYears = simTipDists$TimeDiff / 365


p7 = ggplot(data = simTipDists, aes(y = snpsPerGen, x = TimeDiffYears)) +
  geom_point(colour = "blue", alpha = .05) +
  theme_bw() +
  ylab("SNPs per Generation") + xlab("Temporal Distance (Years)") +
  geom_hline(yintercept = mean(simTipDists$snpsPerGen), lty = 2)
#p7

 p6 + p7
 