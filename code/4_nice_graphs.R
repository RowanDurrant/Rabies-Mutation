##STYLE GUIDE: ANYTHING NOVEL METHOD RED, ANYTHING CLOCK RATE BLUE
library(ggplot2)
library(patchwork)


#FIGURE 1
library(stringr)
startSeq = paste(rep("a",12000), collapse = "")
files = c("output/simulation/simsampledtips_perGen_24_0.000166666666666667_124_621.7.csv",
          "output/simulation/simsampledtips_perfectClockRate_24_6.33713561470215e-06_124_621.7.csv",
          "output/simulation/simsampledtips_perGen_24_1.66666666666667e-05_124_621.7.csv",
          "output/simulation/simsampledtips_perfectClockRate_24_6.33713561470216e-07_124_621.7.csv")

for(f in 1:length(files)){
        samples = read.csv(files[f])
        time = rep(NA, nrow(samples))
        divergence = rep(NA, nrow(samples))
        for(i in 1:nrow(samples)){
          time[i] = samples$infD[i]
          divergence[i] = (nchar(startSeq) - str_count(samples$sequence[i], "a"))/nchar(startSeq)
          
        }
        ml = lm(divergence ~ time)
        if(f == 1){
        plot(divergence ~ time, xlab = "Time (days)", ylab = "Divergence (nucleotide subs./site)", col = "red",main = "2 SNPs per Generation")
        abline(reg = ml, col = "red")
        legend(x = "topleft",          # Position
               legend = c("Generation", "Clock Rate"),  # Legend texts
               fill = c("red", "blue"),
               bty="n")         # Line colors
        }
        else if(f == 2){
          points(divergence ~ time, col = "blue",)
          abline(reg = ml, col = "blue")
        }
        else if(f == 3){
          plot(divergence ~ time, xlab = "Time (days)", ylab = "Divergence (nucleotide subs./site)", col = "red",
               main = "0.2 SNPs per Generation", ylim = c(0,0.0022))
          abline(reg = ml, col = "red")
          legend(x = "topleft",          # Position
                 legend = c("Generation", "Clock Rate"),  # Legend texts
                 fill = c("red", "blue"),
                 bty = "n")         # Line colors
        }
        else if(f == 4){
          points(divergence ~ time, col = "blue")
          abline(reg = ml, col = "blue")
        }
      }


#FIGURE 2
df = read.csv("output/simulation/clockrate-gen-model-comparison.csv")
df = df[is.na(df$Method) == F,]
ggplot(data = df, aes(x = EquivalentPerGenRate, y = R_Squared, fill = Method)) +
  geom_point(aes(colour = Method)) +
  geom_smooth(method = "gam", aes(colour = Method)) +
  xlab("Equivalent Per-Generation Mutation Rate") + ylab("R Squared")+
  facet_wrap( ~PercentSampled) +
  theme_bw() +
  scale_color_manual(name='Mutation Model',
                     values=c('Clock Rate'='blue', 'Generation'='red'))+
  scale_fill_manual(name='Mutation Model',
                     values=c('Clock Rate'='blue', 'Generation'='red'))
#ggsave("Figure 1.png", bg = "transparent")

library(ggplot2)
library(devtools)

df2 = df[df$PercentSampled == 0.2,]

ggplot(data = df2, aes(x = EquivalentPerGenRate, y = R_Squared, fill = Method)) +
  geom_point(aes(colour = Method)) +
scale_x_continuous(trans='log10')+
  geom_smooth(method = "gam", aes(colour = Method)) +
  xlab("Equivalent Per-Generation Mutation Rate (SNPs/Generation)") + ylab("R Squared")+
  theme_bw() +
  scale_color_manual(name='Mutation Model',
                     values=c('Clock Rate'='blue', 'Generation'='red'))+
  scale_fill_manual(name='Mutation Model',
                    values=c('Clock Rate'='blue', 'Generation'='red')) 

#ggsave("Figure 1 0.2 sampled only.png")

#FIGURE 3 
novelMethodAccuracy = read.csv("output/simulation/novel_method_accuracy.csv")
novelMethodAccuracy$Method = "Novel Method"
clockRateMethodAccuracy = read.csv("output/simulation/clockrate_method_accuracy.csv")
clockRateMethodAccuracy$Method = "Clock Rate Method"
Accuracy = rbind(clockRateMethodAccuracy[,c(2,3,5,6,7)], novelMethodAccuracy[,c(3,5,6,9,11)])

ggplot(data = Accuracy, aes(x = SNPRate, y = SNPsAccuracy, 
                                            fill = Method)) +
  geom_point(aes(colour=Method)) + 
  geom_smooth(se = T, method="glm", aes(colour = Method)) +
  geom_hline(yintercept = 100, lty = 2) +
  theme_bw() +
  ylab("% Accuracy") + xlab("Equivalent Per-Generation Mutation Rate (SNPs/Generation)") +
  scale_color_manual(name='Prediction Method',
                     values=c('Clock Rate Method'='blue', 'Novel Method'='red'))+
  scale_fill_manual(name='Prediction Method',
                    values=c('Clock Rate Method'='blue', 'Novel Method'='red'))

ggsave("plots/Figure 3 updated.png", bg = "transparent")


#FIGURE 4
tipDists = read.csv("output/pemba/full_bootstraps_100.csv")

bw = 0.005
n_obs = sum(!is.na(tipDists$snpsPerGen))

ggplot(data = data.frame(x = c(0, 0.5)), aes(x)) +
  geom_histogram(data= tipDists, aes(snpsPerGen), binwidth = bw, alpha=0.2, fill = "red")+
  stat_function(fun = function(x) 
    dlnorm(x, meanlog =  -1.820404, sdlog = 0.3321436)* bw * n_obs, aes(colour = "Clock Rate Method"), size = 1.1) + 
  ylab("") +
  xlab("SNPs per Generation")+
  stat_function(fun= function(x)
    dgamma(x, shape = 10.01941, rate = 58.80515)* bw * n_obs, aes(colour = "Novel Method"), size = 1.1) +
  theme_bw() + xlim(0,0.5)+
  scale_color_manual(name='Prediction Method',
                     values=c('Clock Rate Method'='blue', 'Novel Method'='red'))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ggsave("plots/figure 4 with hist.png")

#FIGURE 5
tipDists = read.csv("output/pemba/full_bootstraps_100.csv") #change if you changed no. reps
clusters = read.csv("input/Pemba_assignment.csv")
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
    print(paste(i, nrow(clusters[clusters$lineage == i,]), mean(newLineage$snpsPerGen)))
  }
  else{print(paste(i, nrow(clusters[clusters$lineage == i,])))}
}

ggplot(data = lineageTipDists, aes(x = snpsPerGen)) +
  geom_density(alpha=.35, fill="#FF6666") +
  xlim(0,0.75)+
  facet_wrap(~ Lineage) +
  theme_bw() +
  xlab("SNPs per Generation")+
  geom_vline(xintercept = 0.17, lty = 2)
ggsave("plots/figure 5 vline updated.png")

#FIGURE 6

simTipDists = read.csv("output/simulation/full_bootstraps_sim_24_1.41666666666667e-05_166_150_timescaled.csv")
simTipDists$TimeDiffYears = simTipDists$TimeDiff / 365

p6 = ggplot(data = simTipDists, aes(y = snpsPerGen, x = TimeDiffYears)) +
  geom_point(colour = "blue", alpha = .1) +
  theme_bw() +
  ylab("SNPs per Generation") + xlab("Temporal Distance (Years)") +
  geom_hline(yintercept = 0.17)
#ggsave("plots/figure 6 hline updated.png")
#p6
p7 = ggplot(data = lineageTipDists, aes(x = TimeDiffYears, y = snpsPerGen)) +
  geom_point(alpha=.35, colour = "red") +
 # xlim(0,0.75)+
  #geom_hline(aes(yintercept = mean(snpsPerGen)), lty = 2)+
  facet_wrap(~ Lineage) +
  theme_bw() +
  ylab("SNPs per Generation")+
  xlab("Temporal Distance (years)")

p6 / p7

