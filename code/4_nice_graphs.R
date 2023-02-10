##STYLE GUIDE: ANYTHING NOVEL METHOD RED, ANYTHING CLOCK RATE BLUE
library(ggplot2)
library(patchwork)
library(scales)

#FIGURE 1
library(stringr)
startSeq = paste(rep("a",12000), collapse = "")
files = c("output/simulation/simsampledtips_perGen_24_0.000166666666666667_124_621.7.csv",
          "output/simulation/simsampledtips_perfectClockRate_24_6.33713561470215e-06_124_621.7.csv",
          "output/simulation/simsampledtips_perGen_24_1.66666666666667e-05_124_621.7.csv",
          "output/simulation/simsampledtips_perfectClockRate_24_6.33713561470216e-07_124_621.7.csv")

par(mfrow=c(1,2), mar = c(5,4,3,0.5))

for(f in 1:length(files)){
  samples = read.csv(files[f])
  time = rep(NA, nrow(samples))
  divergence = rep(NA, nrow(samples))
  for(i in 1:nrow(samples)){
    time[i] = samples$infD[i]/365
    divergence[i] = (nchar(startSeq) - str_count(samples$sequence[i], "a"))/nchar(startSeq)
    
  }
  ml = lm(divergence ~ time)
  if(f == 1){
    plot(divergence ~ time, xlab = "Time (years)", 
         ylab = "Divergence (nucleotide subs./site)", 
         col = alpha("red", 0.3), pch = 16,
         ylim = c(0,0.022),
         xlim = c(0,7.2))
    abline(reg = ml, col = "red")
    title("A", adj = 0, line = 0.5)
    legend(x = "topleft",          # Position
           legend = c("Generation model", "Time model"),  # Legend texts
           fill = c("red", "blue"),
           bty="n")         # Line colors
  }
  else if(f == 2){
    points(divergence ~ time, col = alpha("blue",0.3), pch = 16)
    abline(reg = ml, col = "blue")
  }
  else if(f == 3){
    par(mar = c(5,3,3,1.5))
    plot(divergence ~ time, xlab = "Time (years)", 
         ylab = NA, 
         col = alpha("red",0.3), pch = 16, 
         ylim = c(0,0.0022),
         xlim = c(0,7.2))
    abline(reg = ml, col = "red")
    title("B", adj = 0, line = 0.5)
  }
  else if(f == 4){
    points(divergence ~ time, col = alpha("blue",0.3), pch=16)
    abline(reg = ml, col = "blue")
  }
}


#FIGURE 2
df = read.csv("output/simulation/clockrate-gen-model-comparison.csv")
df = df[is.na(df$Method) == F,]


library(ggplot2)

df2 = df[df$PercentSampled == 0.2,]
df2$Method[df2$Method == "Clock Rate"] = "Time"
library(betareg)
gy_logit_generation = betareg(R_Squared ~ EquivalentPerGenRate, data = df2, subset = Method == "Generation")
gy_logit_time = betareg(R_Squared ~ EquivalentPerGenRate, data = df2, subset = Method == "Time")

predict(gy_logit_time, newdata = df2, interval = 'prediction',
        type = "quantile", at = c(0.025, 0.975))

mpi1 <- cbind(df2[df2$Method == "Generation",], predict(gy_logit_generation, newdata = df2, interval = 'prediction',
                                                        type = "quantile", at = c(0.025, 0.975)))

mpi2 <- cbind(df2[df2$Method == "Time",], predict(gy_logit_time, newdata = df2, interval = 'prediction',
                                                        type = "quantile", at = c(0.025, 0.975)))

ggplot(data = df2, aes(x = EquivalentPerGenRate, y = R_Squared)) +
  geom_point(aes(fill = Method), shape = 21, alpha = 0.5) +
  geom_ribbon(data = mpi1, aes(ymin = q_0.025, ymax = q_0.975),
              fill = "red", alpha = 0.15) +
  
  geom_ribbon(data = mpi2, aes(ymin = q_0.025, ymax = q_0.975),
              fill = "blue", alpha = 0.15) +
  geom_line(aes(y = predict(gy_logit_time, df2),
                colour = "Time", linetype = "Time")) +
  geom_line(aes(y = predict(gy_logit_generation, df2), 
                colour = "Generation", linetype = "Generation")) +
  scale_x_continuous(breaks = c(0.05,0.1,0.2,0.5,1,2,3))+
  coord_trans(x='log10')+
  
  xlab("Equivalent Per-Generation Substitution Rate (SNPs/Generation)") + ylab("R Squared")+
  theme_bw() +
  scale_colour_manual("", values = c("red", "blue")) +
  scale_fill_manual(name='Mutation Model',
                    values=c('Time'='blue', 'Generation'='red')) +
  scale_linetype_manual("", values = c("solid", "dashed"))

ggsave("plots/Figure 2.png")

#FIGURE 3 
novelMethodAccuracy = read.csv("output/simulation/novel_method_accuracy.csv")
novelMethodAccuracy$Method = "Novel Method"
clockRateMethodAccuracy = read.csv("output/simulation/clockrate_method_accuracy.csv")
clockRateMethodAccuracy$Method = "Clock Rate Method"
Accuracy = rbind(clockRateMethodAccuracy[,c(2,3,5,6,7)], novelMethodAccuracy[,c(3,5,6,9,11)])
Accuracy$decimalAccuracy = Accuracy$SNPsAccuracy/100

library(betareg)
gy_logit_clock = betareg(decimalAccuracy ~ SNPRate, data = Accuracy, subset = Method == "Clock Rate Method")
gy_logit_novel = betareg(decimalAccuracy ~ SNPRate, data = Accuracy, subset = Method == "Novel Method")

predict(gy_logit_clock, newdata = Accuracy, interval = 'prediction',
        type = "quantile", at = c(0.025, 0.975))

mpi1 <- cbind(Accuracy[Accuracy$Method == "Clock Rate Method",], predict(gy_logit_clock, newdata = Accuracy, interval = 'prediction',
                                                                         type = "quantile", at = c(0.025, 0.975)))
mpi2 <- cbind(Accuracy[Accuracy$Method == "Novel Method",], predict(gy_logit_novel, newdata = Accuracy, interval = 'prediction',
                                                                    type = "quantile", at = c(0.025, 0.975)))


library(ggplot2)
ggplot(Accuracy, aes(x = SNPRate, y = decimalAccuracy)) +
  geom_point(aes(fill = Method), shape = 21, alpha = 0.5) +
  scale_fill_manual(name='Prediction Method',
                    values=c('Clock Rate Method'='blue', 'Novel Method'='red')) +
  geom_line(aes(y = predict(gy_logit_clock, Accuracy),
                colour = "Clock Rate Method", linetype = "Clock Rate Method")) +
  geom_line(aes(y = predict(gy_logit_novel, Accuracy), 
                colour = "Novel Method", linetype = "Novel Method")) +
  geom_ribbon(data = mpi1, aes(ymin = q_0.025, ymax = q_0.975),
              fill = "blue", alpha = 0.2) +
  
  geom_ribbon(data = mpi2, aes(ymin = q_0.025, ymax = q_0.975),
              fill = "red", alpha = 0.2) +
  scale_colour_manual("", values = c("blue", "red")) +
  scale_linetype_manual("", values = c("solid", "dashed")) +
  xlab("Equivalent Per-Generation Substitution Rate (SNPs/Generation)")+
  ylab("Accuracy") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme_bw()

ggsave("plots/Figure 3.png", bg = "transparent")


#FIGURE 4
tipDists = read.csv("output/pemba/full_bootstraps_100_2022.csv")

bw = 0.005
n_obs = sum(!is.na(tipDists$snpsPerGen))

ggplot(data = data.frame(x = c(0, 0.5)), aes(x)) +
  geom_histogram(data= tipDists, aes(snpsPerGen), binwidth = bw, alpha=0.2, fill = "red")+
  stat_function(fun = function(x) 
    dlnorm(x, meanlog =  -2.231138, sdlog = 0.9662017)* bw * n_obs, aes(colour = "Clock Rate Method"), size = 1.1) + 
  ylab("") +
  xlab("SNPs per Generation")+
  stat_function(fun= function(x)
    dgamma(x, shape = 10.03098, rate = 59.26635)* bw * n_obs, aes(colour = "Novel Method"), size = 1.1) +
  theme_bw() + xlim(0,0.5)+
  scale_color_manual(name='Prediction Method',
                     values=c('Clock Rate Method'='blue', 'Novel Method'='red'))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ggsave("plots/Figure 4.png")

#FIGURE 5
tipDists = read.csv("output/pemba/full_bootstraps_100_2022.csv") #change if you changed no. reps
clusters = read.csv("input/Pemba_assignment.csv")
alnDist = read.csv("output/pemba/snpdistancesall.csv")
rownames(alnDist) <- alnDist[,1]
alnDist[,1] <- NULL
colnames(alnDist) = rownames(alnDist)
clusters = clusters[clusters$ID %in% rownames(alnDist),]
tipDists$TimeDiffYears = tipDists$TimeDiff / 365
tipDists$n = NA
lineages = unique(clusters$lineage)
lineageTipDists = head(tipDists, 0)


for(i in lineages){
  if(nrow(clusters[clusters$lineage == i,]) > 1){
    newLineage = tipDists[tipDists$Tip1 %in% clusters$ID[clusters$lineage == i] &
                            tipDists$Tip2 %in% clusters$ID[clusters$lineage == i],]
    newLineage$Lineage = i
    newLineage$n = nrow(clusters[clusters$lineage == i,])
    lineageTipDists = rbind(lineageTipDists, newLineage)
    # R program to find the confidence interval
    
    # Calculate the mean and standard error
    model <- lm(snpsPerGen ~ 1, newLineage)
    
    # Find the confidence interval
    conf95 = confint(model, level=0.95)
    
    
    print(paste(i, nrow(clusters[clusters$lineage == i,]), 
                mean(newLineage$snpsPerGen), conf95[1], conf95[2]))
  }
  else{print(paste(i, nrow(clusters[clusters$lineage == i,])))}
}

ggplot(data = lineageTipDists, aes(x = snpsPerGen)) +
  geom_density(alpha=.35, fill="#FF6666") +
  xlim(0,0.75)+
  facet_wrap(~ Lineage) +
  geom_text(data = lineageTipDists,
            mapping = aes(x = 0.6,
                          y = 20,
                          label = paste0("n = ", as.character(n))))+
  theme_bw() +
  ylab("Density") +
  xlab("SNPs per Generation")+
  geom_vline(xintercept = 0.1690, lty = 2)
ggsave("plots/figure 5 vline updated 2022.png")

#FIGURE 6

simTipDists = read.csv("output/simulation/full_bootstraps_sim_24_1.41666666666667e-05_166_150_timescaled.csv")
simTipDists$TimeDiffYears = simTipDists$TimeDiff / 365

p6 = ggplot(data = simTipDists, aes(y = snpsPerGen, x = TimeDiffYears)) +
  geom_point(colour = "blue", alpha = .1) +
  theme_bw() +
  ylab("SNPs per Generation") + xlab("Temporal Distance (Years)") +
  geom_hline(yintercept = 0.17) +
  labs(title="A")
#ggsave("plots/figure 6 hline updated.png")
#p6

lineageNames = c("AF1b_A1",
                 "AF1b_A1.1.1",
                 "AF1b_A1.1.2",
                 "AF1b_B1",
                 "AF1b_B1.1",
                 "AF1b_B1.1.1",
                 "AF1b_B1.2",
                 "AF1b_B1.3",
                 "AF1b_C1")
names(lineageNames) = c("Cosmopolitan AF1b_A1",
                        "Cosmopolitan AF1b_A1.1.1",
                        "Cosmopolitan AF1b_A1.1.2",
                        "Cosmopolitan AF1b_B1",
                        "Cosmopolitan AF1b_B1.1",
                        "Cosmopolitan AF1b_B1.1.1",
                        "Cosmopolitan AF1b_B1.2",
                        "Cosmopolitan AF1b_B1.3",
                        "Cosmopolitan AF1b_C1")

p7 = ggplot(data = lineageTipDists, aes(x = TimeDiffYears, y = snpsPerGen)) +
  geom_point(alpha=.35, colour = "red") +
  # xlim(0,0.75)+
  #geom_hline(aes(yintercept = mean(snpsPerGen)), lty = 2)+
  facet_wrap(~ Lineage, 
             labeller = labeller(Lineage = lineageNames)) +
  theme_bw() +
  ylab("SNPs per Generation")+
  xlab("Temporal Distance (Years)")+
  labs(title = "B")

p6 + p7

