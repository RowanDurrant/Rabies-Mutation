##STYLE GUIDE: ANYTHING NOVEL METHOD RED, ANYTHING CLOCK RATE BLUE
library(ggplot2)
library(patchwork)
library(scales)

#FIGURE 2
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
  ml = lm(divergence ~ 0 + time)
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


#FIGURE 3
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

ggsave("plots/Figure 3.png")

#FIGURE 4 
#novelMethodAccuracy = read.csv("output/simulation/novel_method_accuracy.csv")
#novelMethodAccuracy$Method = "Novel Method"
Accuracy = read.csv("output/simulation/clockrate_method_accuracy.csv")
library(ggplot2)
ggplot(Accuracy, aes(x = trueSNPRate, y = accuracy)) +
  geom_point(colour = "palegreen4", shape = 16) +
  ylim(-1, 1)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  xlab("Equivalent Per-Generation Substitution Rate (SNPs/Generation)")+
  ylab("Accuracy (natural log of the ratio)") +
  theme_bw() + theme(legend.position = "none") +
  coord_trans(x = "log")

ggsave("plots/Figure 5.png")

#stats
clock02 = clockRateMethodAccuracy$SNPsAccuracy[clockRateMethodAccuracy$SNPRate == 0.2]
clock05 = clockRateMethodAccuracy$SNPsAccuracy[clockRateMethodAccuracy$SNPRate == 0.5]
clock1 = clockRateMethodAccuracy$SNPsAccuracy[clockRateMethodAccuracy$SNPRate == 1]
clock2 = clockRateMethodAccuracy$SNPsAccuracy[clockRateMethodAccuracy$SNPRate == 2]
clock5 = clockRateMethodAccuracy$SNPsAccuracy[clockRateMethodAccuracy$SNPRate == 5]
# 
# novel02 = novelMethodAccuracy$SNPsAccuracy[novelMethodAccuracy$SNPRate == 0.2]
# novel05 = novelMethodAccuracy$SNPsAccuracy[novelMethodAccuracy$SNPRate == 0.5]
# novel1 = novelMethodAccuracy$SNPsAccuracy[novelMethodAccuracy$SNPRate == 1]
# novel2 = novelMethodAccuracy$SNPsAccuracy[novelMethodAccuracy$SNPRate == 2]
# novel5 = novelMethodAccuracy$SNPsAccuracy[novelMethodAccuracy$SNPRate == 5]

# t.test(clock02,novel02)
# t.test(clock05,novel05)
# t.test(clock1,novel1)
# t.test(clock2,novel2)
# t.test(clock5,novel5)

#FIGURE 5
tipDists = read.csv("output/pemba/multiplied_posteriors.csv")

bw = 0.005
n_obs = 124299

p4 = ggplot(data = data.frame(x = c(0, 0.35)), aes(x)) +
  geom_histogram(data= tipDists, aes(x), binwidth = bw, alpha=0.3, fill = "palegreen3")+
  stat_function(fun = function(x)
    dgamma(x, shape = 51.69189, rate = 301.8095)* bw * n_obs, colour = "darkgreen", size = 1.1) +
  ylab("") +
  xlab("SNPs per Generation")+
#  stat_function(fun= function(x)
#    dgamma(x, shape = 10.03098, rate = 59.26635)* bw * n_obs, aes(colour = "Novel Method"), size = 1.1) +
  theme_bw() + xlim(0,0.5)+
#  scale_color_manual(name='Prediction Method',
 #                    values=c('Clock Rate Method'='blue', 'Novel Method'='red'))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")

library(ggplot2)
library(patchwork)
library(Rmisc)

#clock_0= dpois(0, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_0= dpois(0, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_1= dpois(1, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_1= dpois(1, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_2= dpois(2, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_2= dpois(2, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_3= dpois(3, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_3= dpois(3, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_4= dpois(4, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_4= dpois(4, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_5= dpois(5, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_5= dpois(5, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_6= dpois(6, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_6= dpois(6, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_7= dpois(7, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_7= dpois(7, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_8= dpois(8, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_8= dpois(8, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_9= dpois(9, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_9= dpois(9, rgamma(10000,shape = 51.69189, rate = 301.8095))
#clock_10= dpois(10, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_10= dpois(10, rgamma(10000,shape = 51.69189, rate = 301.8095))

# clock_means = c(mean(clock_0),mean(clock_1),mean(clock_2),mean(clock_3),mean(clock_4),
#                 mean(clock_5),mean(clock_6),mean(clock_7),mean(clock_8),mean(clock_9),
#                 mean(clock_10))
# clock_CI_upper = unname(c(CI(clock_0)[1],CI(clock_1)[1],CI(clock_2)[1],CI(clock_3)[1],CI(clock_4)[1],
#                           CI(clock_5)[1],CI(clock_6)[1],CI(clock_7)[1],CI(clock_8)[1],CI(clock_9)[1],
#                           CI(clock_10)[1]))
# clock_CI_lower = unname(c(CI(clock_0)[3],CI(clock_1)[3],CI(clock_2)[3],CI(clock_3)[3],CI(clock_4)[3],
#                           CI(clock_5)[3],CI(clock_6)[3],CI(clock_7)[3],CI(clock_8)[3],CI(clock_9)[3],
#                           CI(clock_10)[3]))

clock_means = c(mean(novel_0),mean(novel_1),mean(novel_2),mean(novel_3),mean(novel_4),
                mean(novel_5),mean(novel_6),mean(novel_7),mean(novel_8),mean(novel_9),
                mean(novel_10))
clock_CI_upper = unname(c(CI(novel_0)[1],CI(novel_1)[1],CI(novel_2)[1],CI(novel_3)[1],CI(novel_4)[1],
                          CI(novel_5)[1],CI(novel_6)[1],CI(novel_7)[1],CI(novel_8)[1],CI(novel_9)[1],
                          CI(novel_10)[1]))
clock_CI_lower = unname(c(CI(novel_0)[3],CI(novel_1)[3],CI(novel_2)[3],CI(novel_3)[3],CI(novel_4)[3],
                          CI(novel_5)[3],CI(novel_6)[3],CI(novel_7)[3],CI(novel_8)[3],CI(novel_9)[3],
                          CI(novel_10)[3]))

Mean = c(clock_means)
CI_upper = c(clock_CI_upper)
CI_lower = c(clock_CI_lower)

df = as.data.frame(cbind(Mean, CI_upper, CI_lower))
df$Method = rep("Clock Rate", 11)
df$gens = c(0:10)


p1 = ggplot(data=df, aes(x=gens, y=Mean)) + 
  geom_bar(position = "dodge", stat = 'identity', fill = "palegreen3") +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("1 Generation")+
  geom_errorbar( aes(x=gens, ymin=CI_lower, ymax=CI_upper), 
                 position = "dodge", stat = 'identity')+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  coord_cartesian(xlim = c(0, 10)) + ylim(0,1)
 # scale_fill_manual(name='Prediction Method',
#                    values=c('Clock Rate'='blue', 'Novel'='red'))
##FIVE GENS                 

#clock_0= dpois(0, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_0= dpois(0, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_1= dpois(1, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_1= dpois(1, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_2= dpois(2, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_2= dpois(2, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_3= dpois(3, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_3= dpois(3, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_4= dpois(4, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_4= dpois(4, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_5= dpois(5, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_5= dpois(5, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_6= dpois(6, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_6= dpois(6, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_7= dpois(7, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_7= dpois(7, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_8= dpois(8, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_8= dpois(8, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_9= dpois(9, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_9= dpois(9, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
#clock_10= dpois(10, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_10= dpois(10, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)

# clock_means = c(mean(clock_0),mean(clock_1),mean(clock_2),mean(clock_3),mean(clock_4),
#                 mean(clock_5),mean(clock_6),mean(clock_7),mean(clock_8),mean(clock_9),
#                 mean(clock_10))
# clock_CI_upper = unname(c(CI(clock_0)[1],CI(clock_1)[1],CI(clock_2)[1],CI(clock_3)[1],CI(clock_4)[1],
#                           CI(clock_5)[1],CI(clock_6)[1],CI(clock_7)[1],CI(clock_8)[1],CI(clock_9)[1],
#                           CI(clock_10)[1]))
# clock_CI_lower = unname(c(CI(clock_0)[3],CI(clock_1)[3],CI(clock_2)[3],CI(clock_3)[3],CI(clock_4)[3],
#                           CI(clock_5)[3],CI(clock_6)[3],CI(clock_7)[3],CI(clock_8)[3],CI(clock_9)[3],
#                           CI(clock_10)[3]))

clock_means = c(mean(novel_0),mean(novel_1),mean(novel_2),mean(novel_3),mean(novel_4),
                mean(novel_5),mean(novel_6),mean(novel_7),mean(novel_8),mean(novel_9),
                mean(novel_10))
clock_CI_upper = unname(c(CI(novel_0)[1],CI(novel_1)[1],CI(novel_2)[1],CI(novel_3)[1],CI(novel_4)[1],
                          CI(novel_5)[1],CI(novel_6)[1],CI(novel_7)[1],CI(novel_8)[1],CI(novel_9)[1],
                          CI(novel_10)[1]))
clock_CI_lower = unname(c(CI(novel_0)[3],CI(novel_1)[3],CI(novel_2)[3],CI(novel_3)[3],CI(novel_4)[3],
                          CI(novel_5)[3],CI(novel_6)[3],CI(novel_7)[3],CI(novel_8)[3],CI(novel_9)[3],
                          CI(novel_10)[3]))

Mean = c(clock_means)
CI_upper = c(clock_CI_upper)
CI_lower = c(clock_CI_lower)

df = as.data.frame(cbind(Mean, CI_upper, CI_lower))
df$Method = rep("Clock Rate", 11)
df$gens = c(0:10)


p2 = ggplot(data=df, aes(x=gens, y=Mean)) + 
  geom_bar(position = "dodge", stat = 'identity', fill = "palegreen3") +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("5 Generations")+
  ylab("Mean Probability of\nSNPs Occurring")+
  geom_errorbar( aes(x=gens, ymin=CI_lower, ymax=CI_upper), 
                 position = "dodge", stat = 'identity')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  coord_cartesian(xlim = c(0, 10)) + ylim(0,1)
 # scale_fill_manual(name='Prediction Method',
#                    values=c('Clock Rate'='blue', 'Novel'='red'))



##TEN GENS

#clock_0= dpois(0, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_0= dpois(0, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_1= dpois(1, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_1= dpois(1, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_2= dpois(2, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_2= dpois(2, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_3= dpois(3, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_3= dpois(3, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_4= dpois(4, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_4= dpois(4, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_5= dpois(5, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_5= dpois(5, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_6= dpois(6, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_6= dpois(6, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_7= dpois(7, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_7= dpois(7, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_8= dpois(8, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_8= dpois(8, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_9= dpois(9, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_9= dpois(9, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
#clock_10= dpois(10, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_10= dpois(10, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)

# clock_means = c(mean(clock_0),mean(clock_1),mean(clock_2),mean(clock_3),mean(clock_4),
#                 mean(clock_5),mean(clock_6),mean(clock_7),mean(clock_8),mean(clock_9),
#                 mean(clock_10))
# clock_CI_upper = unname(c(CI(clock_0)[1],CI(clock_1)[1],CI(clock_2)[1],CI(clock_3)[1],CI(clock_4)[1],
#                           CI(clock_5)[1],CI(clock_6)[1],CI(clock_7)[1],CI(clock_8)[1],CI(clock_9)[1],
#                           CI(clock_10)[1]))
# clock_CI_lower = unname(c(CI(clock_0)[3],CI(clock_1)[3],CI(clock_2)[3],CI(clock_3)[3],CI(clock_4)[3],
#                           CI(clock_5)[3],CI(clock_6)[3],CI(clock_7)[3],CI(clock_8)[3],CI(clock_9)[3],
#                           CI(clock_10)[3]))

clock_means = c(mean(novel_0),mean(novel_1),mean(novel_2),mean(novel_3),mean(novel_4),
                mean(novel_5),mean(novel_6),mean(novel_7),mean(novel_8),mean(novel_9),
                mean(novel_10))
clock_CI_upper = unname(c(CI(novel_0)[1],CI(novel_1)[1],CI(novel_2)[1],CI(novel_3)[1],CI(novel_4)[1],
                          CI(novel_5)[1],CI(novel_6)[1],CI(novel_7)[1],CI(novel_8)[1],CI(novel_9)[1],
                          CI(novel_10)[1]))
clock_CI_lower = unname(c(CI(novel_0)[3],CI(novel_1)[3],CI(novel_2)[3],CI(novel_3)[3],CI(novel_4)[3],
                          CI(novel_5)[3],CI(novel_6)[3],CI(novel_7)[3],CI(novel_8)[3],CI(novel_9)[3],
                          CI(novel_10)[3]))

Mean = c(clock_means)
CI_upper = c(clock_CI_upper)
CI_lower = c(clock_CI_lower)

df = as.data.frame(cbind(Mean, CI_upper, CI_lower))
df$Method = c(rep("Clock Rate", 11))
df$gens = c(0:10)


p3 = ggplot(data=df, aes(x=gens, y=Mean)) + 
  geom_bar(position = "dodge", stat = 'identity', fill = "palegreen3") +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("10 Generations")+
  geom_errorbar( aes(x=gens, ymin=CI_lower, ymax=CI_upper), 
                 position = "dodge", stat = 'identity')+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")+
  coord_cartesian(xlim = c(0, 10)) + ylim(0,1) +
  scale_x_continuous(breaks = c(0:10), labels = c("0":"10"))
 # scale_fill_manual(name='Prediction Method',
  #                  values=c('Clock Rate'='blue', 'Novel'='red'))
library(ggpubr)

p5 = (p1/p2/p3)

ggarrange(p4, p5, nrow = 1,
          common.legend = TRUE, legend="bottom")


ggsave("plots/Figure 6.png")

#FIGURE 6
# tipDists = read.csv("output/pemba/full_bootstraps_100_2022.csv") #change if you changed no. reps
# clusters = read.csv("input/Pemba_assignment.csv")
# alnDist = read.csv("output/pemba/snpdistancesall.csv")
# rownames(alnDist) <- alnDist[,1]
# alnDist[,1] <- NULL
# colnames(alnDist) = rownames(alnDist)
# clusters = clusters[clusters$ID %in% rownames(alnDist),]
# tipDists$TimeDiffYears = tipDists$TimeDiff / 365
# tipDists$n = NA
# lineages = unique(clusters$lineage)
# lineageTipDists = head(tipDists, 0)
# 
# 
# for(i in lineages){
#   if(nrow(clusters[clusters$lineage == i,]) > 1){
#     newLineage = tipDists[tipDists$Tip1 %in% clusters$ID[clusters$lineage == i] &
#                             tipDists$Tip2 %in% clusters$ID[clusters$lineage == i],]
#     newLineage$Lineage = i
#     newLineage$n = nrow(clusters[clusters$lineage == i,])
#     lineageTipDists = rbind(lineageTipDists, newLineage)
#     # R program to find the confidence interval
#     
#     # Calculate the mean and standard error
#     model <- lm(snpsPerGen ~ 1, newLineage)
#     
#     # Find the confidence interval
#     conf95 = confint(model, level=0.95)
#     
#     
#     print(paste(i, nrow(clusters[clusters$lineage == i,]), 
#                 mean(newLineage$snpsPerGen), conf95[1], conf95[2]))
#   }
#   else{print(paste(i, nrow(clusters[clusters$lineage == i,])))}
# }
# 
# ggplot(data = lineageTipDists, aes(x = snpsPerGen)) +
#   geom_density(alpha=.35, fill="#FF6666") +
#   xlim(0,0.75)+
#   facet_wrap(~ Lineage) +
#   geom_text(data = lineageTipDists,
#             mapping = aes(x = 0.6,
#                           y = 20,
#                           label = paste0("n = ", as.character(n))))+
#   theme_bw() +
#   ylab("Density") +
#   xlab("SNPs per Generation")+
#   geom_vline(xintercept = 0.1690, lty = 2)
# ggsave("plots/figure 6 vline updated 2022.png")
# 
# #FIGURE 7
# 
# simTipDists = read.csv("output/simulation/full_bootstraps_sim_24_1.41666666666667e-05_166_150_timescaled.csv")
# simTipDists$TimeDiffYears = simTipDists$TimeDiff / 365
# 
# p6 = ggplot(data = simTipDists, aes(y = snpsPerGen, x = TimeDiffYears)) +
#   geom_point(colour = "blue", alpha = .1) +
#   theme_bw() +
#   ylab("SNPs per Generation") + xlab("Temporal Distance (Years)") +
#   geom_hline(yintercept = 0.17) +
#   labs(title="A")
# #ggsave("plots/figure 6 hline updated.png")
# #p6
# 
# lineageNames = c("AF1b_A1",
#                  "AF1b_A1.1.1",
#                  "AF1b_A1.1.2",
#                  "AF1b_B1",
#                  "AF1b_B1.1",
#                  "AF1b_B1.1.1",
#                  "AF1b_B1.2",
#                  "AF1b_B1.3",
#                  "AF1b_C1")
# names(lineageNames) = c("Cosmopolitan AF1b_A1",
#                         "Cosmopolitan AF1b_A1.1.1",
#                         "Cosmopolitan AF1b_A1.1.2",
#                         "Cosmopolitan AF1b_B1",
#                         "Cosmopolitan AF1b_B1.1",
#                         "Cosmopolitan AF1b_B1.1.1",
#                         "Cosmopolitan AF1b_B1.2",
#                         "Cosmopolitan AF1b_B1.3",
#                         "Cosmopolitan AF1b_C1")
# 
# p7 = ggplot(data = lineageTipDists, aes(x = TimeDiffYears, y = snpsPerGen)) +
#   geom_point(alpha=.35, colour = "red") +
#   # xlim(0,0.75)+
#   #geom_hline(aes(yintercept = mean(snpsPerGen)), lty = 2)+
#   facet_wrap(~ Lineage, 
#              labeller = labeller(Lineage = lineageNames)) +
#   theme_bw() +
#   ylab("SNPs per Generation")+
#   xlab("Temporal Distance (Years)")+
#   labs(title = "B")
# 
# p6 + p7

