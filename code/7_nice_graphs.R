library(ggplot2)
library(patchwork)
library(scales)
library(tidyverse)
library(ggtree)
library(treeio)
library(ggmap)
library(grid)
library(ggplot2)
library(RColorBrewer)

#FIGURE 1---------------------------------------------------------------------------

df = read.table("input/pemba non timescaled.txt", header = T)
clusters = read.csv("input/Pemba_assignment.csv")
df = df[df$tip %in% clusters$ID,]

df$lineage = NA
df$location = NA

for(i in 1:nrow(df)){
  df$lineage[i] = clusters$lineage[clusters$ID == df$tip[i]]
  df$location[i] = strsplit(df$tip[i], '[!?._][[:space:]]*')[[1]][3]
}

#Root to tip divergence
p2 = ggplot(data = df, aes(x = date, y = distance))  +
  geom_point(aes(col = lineage)) +
  theme(legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8)) +
  stat_smooth(method="lm",fullrange=TRUE,se=F, col = "black")+
  scale_color_manual(values = c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                                "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                                "#920000","#924900","#db6d00","#24ff24","#ffff6d",
                                "#555555", "#999999"))+
  ylab("root-to-tip distance") + 
  theme_bw() 
p2 = p2+ annotate("text",
                  label = "x intercept = 1970.35",
                  x = 2010, y = 0.0165)

#Phylogenetic tree
tree = read.beast("input/pemba_tz_n153_timescaled.mcc.tre")
tipcolours = c()
for(i in 1:length(tree@phylo$tip.label)){
  tipcolours[i] = df$lineage[df$tip == tree@phylo$tip.label[i]]
  tree@phylo$tip.label[i] = strsplit(tree@phylo$tip.label[i], "_")[[1]][1]
}
d <- data.frame(node=1:(Nnode(tree)+length(tree@phylo$tip.label)), 
                lineage = c(tipcolours, rep("black", Nnode(tree))))

p = ggtree(tree, mrsd="2018-01-01") + 
  theme_tree2() + 
  xlim(1950, 2022)
  

p = p %<+% d  +  
  geom_tippoint(aes(color=lineage)) +
  scale_color_manual(values = c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                                "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                                "#920000","#924900","#db6d00","#24ff24","#ffff6d",
                                "#555555", "#999999")) +
  theme(legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8))
#Map
d2 <- data.frame(location = c("Dodoma", "Kibaha", "Kilombero",
                              "Morogoro", "Nachingwea", "Newala",
                              "Ngorongoro", "Pemba", "Same", 
                              "Serengeti", "Temeke", "Ulanga"),
                 lat = c(-6.16321839449851, -6.78292087610276, -8.094573329844343,
                         -6.829822582806052, -10.331969188962217,-10.729783497624434,
                         -3.242787751353734,-5.214532107434417,-4.059036129939253,
                         -1.9078154430190237, -6.939700978622705, -8.907696367406384),
                 long = c(35.75237861054599, 38.99129390980104, 36.67142740598683,
                          37.65825142647812, 38.715948470886694, 39.29205406542055,
                          35.4939020975142,  39.77440205076576, 37.75980296830634,
                          34.753567137337896, 39.37371777456853, 36.824963731565475))

d2$freq = NA
tab = table(df$location)
for(i in 1:nrow(d2)){
  d2$freq[i] = as.numeric(tab[d2$location[i]])
}


tanzania = get_stamenmap(bbox = c(left = 33, bottom = -11, right =
                                    43, top = -1.5), zoom = 6, maptype = 'toner-lite')

p_tanzania = ggmap(tanzania) + 
  geom_point(data = d2, aes(x = long, y = lat, size=freq), colour = "red" , alpha=1) +
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

p1 = p +
  inset(ggplotGrob(p_tanzania), xmin = 1950, xmax = 1995, ymin = 60, ymax = 150)

library(ggpubr)

tiff("plots/Figure1_hi_res.tiff", width = 2250, height = 1700, units = 'px', res = 300)
ggarrange(p1, p2, labels = c("A", "B"), common.legend = TRUE, legend = "bottom")
dev.off()

#ggsave("plots/Figure 1.png")

### FIG 1 PIE CHARTS ###
require(rworldmap)
require(rworldxtra)

namevector =sort(unique(df$lineage))
d2[,namevector] <- NA
for(j in 1:nrow(d2)){
  lineage_tab = table(df$lineage[df$location == d2$location[j]])
  for(k in namevector){
    d2[j,k] = lineage_tab[k]
  }
}

p_tanzania_2 = mapPies(d2, nameX="long", 
                     nameY="lat", 
                     nameZs=namevector,
                     xlim = c(35,40),
                     ylim = c(-11, -1),
                     oceanCol = "lightblue",
                     landCol = "white",
                     symbolSize = 10,
                     zColours = c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                                  "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                                  "#920000","#924900","#db6d00","#24ff24","#ffff6d",
                                  "#555555", "#999999"),
                     addCatLegend = F)

tiff("plots/Figure1_map_hi_res.tiff", width = 1000, height = 1200, units = 'px', res = 300)
mapPies(d2, nameX="long", 
        nameY="lat", 
        nameZs=namevector,
        xlim = c(35,38),
        ylim = c(-11, 0),
        oceanCol = "lightblue",
        landCol = "white",
        symbolSize = 10,
        zColours = c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                     "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                     "#920000","#924900","#db6d00","#24ff24","#ffff6d",
                     "#555555", "#999999"),
        addCatLegend = F)
dev.off()

#FIGURE 2-----------------------------------------------------------------------------------
library(stringr)
startSeq = paste(rep("a",12000), collapse = "")
files = c("output/simulation/simsampledtips_perGen_24_0.000166666666666667_124_621.7.csv",
          "output/simulation/simsampledtips_perfectClockRate_24_6.33713561470215e-06_124_621.7.csv",
          "output/simulation/simsampledtips_perGen_24_1.66666666666667e-05_124_621.7.csv",
          "output/simulation/simsampledtips_perfectClockRate_24_6.33713561470216e-07_124_621.7.csv")

data_list = vector(mode='list', length=4)

for(f in 1:length(files)){
  samples = read.csv(files[f])
  Time = rep(NA, nrow(samples))
  Divergence = rep(NA, nrow(samples))
  for(i in 1:nrow(samples)){
    Time[i] = samples$infD[i]/365
    Divergence[i] = (nchar(startSeq) - str_count(samples$sequence[i], "a"))/nchar(startSeq)
    
  }
  data_list[[f]] = data.frame(cbind(Divergence, Time))
}

p1 = ggplot(data = data_list[[1]], aes(x = Time, y = Divergence))+
  geom_point(colour = "red", shape = 16, alpha = 0.5,size = 2)+
  geom_point(data = data_list[[2]], aes(x = Time, y = Divergence), colour = "blue", 
             alpha = 0.5,shape = 16, size = 2)+ ylim(0,0.0225)+
  theme_bw() + xlab("Time (Years)") + ylab("Divergence (subs./site)")

p2 = ggplot(data = data_list[[3]], aes(x = Time, y = Divergence))+
  geom_point(colour = "red", shape = 16, alpha = 0.5,size = 2)+
  geom_point(data = data_list[[4]], aes(x = Time, y = Divergence), colour = "blue", 
             alpha = 0.5,shape = 16, size = 2)+ ylim(0,0.00225)+
  theme_bw() + xlab("Time (Years)") + ylab("Divergence (subs./site)")

df = read.csv("output/simulation/clockrate-gen-model-comparison.csv")
df = df[is.na(df$Method) == F,]


library(ggplot2)

df2 = df[df$PercentSampled == 0.05,]
df2$Method[df2$Method == "Clock Rate"] = "Time"
library(betareg)
gy_logit_generation = betareg(R_Squared ~ EquivalentPerGenRate, data = df2, subset = Method == "Generation")
gy_logit_time = betareg(R_Squared ~ EquivalentPerGenRate, data = df2, subset = Method == "Time")

mpi1 <- cbind(df2[df2$Method == "Generation",], predict(gy_logit_generation, newdata = df2, interval = 'prediction',
                                                        type = "quantile", at = c(0.025, 0.975)))

mpi2 <- cbind(df2[df2$Method == "Time",], predict(gy_logit_time, newdata = df2, interval = 'prediction',
                                                  type = "quantile", at = c(0.025, 0.975)))

p3 = ggplot(data = df2, aes(x = EquivalentPerGenRate, y = R_Squared)) +
  geom_point(aes(colour = Method), 
             alpha = 0.5,shape = 16, size = 2) +
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
  
  xlab("Equivalent Per-Generation Substitution Rate (SNPs/Generation)") + ylab(expression(R^2))+
  theme_bw() +
  scale_colour_manual("", values = c("red", "blue")) +
  scale_fill_manual(name='Mutation Model',
                    values=c('Generation'='red','Time'='blue')) +
  scale_linetype_manual("", values = c("solid", "dashed")) +
  guides(linetype = "none")

library(ggpubr)
png("plots/Figure2_hi_res.png", width = 2000, height = 1700, units = 'px', res = 300)
ggarrange(ggarrange(p1, p2, labels = c("A", "B")), p3, nrow = 2, 
          common.legend = TRUE, legend = "bottom", labels = c(NA, "C"))
dev.off()

#ggsave("plots/Figure 2.png")

#FIGURE 3-------------------------------------------------------------------
Accuracy = read.csv("output/simulation/clockrate_method_accuracy.csv")
library(ggplot2)
library(viridis)

percent.labs <- c("1% of cases sequenced", "5% of cases sequenced", "10% of cases sequenced",
                  "20% of cases sequenced")
names(percent.labs) <- c(0.01, 0.05, 0.1, 0.2)

png("plots/Figure3_hi_res.png", width = 2000, height = 1700, units = 'px', res = 300)
ggplot(Accuracy, aes(x = trueSNPRate, y = accuracy, colour = no_cases)) +
  geom_point(
    #colour = "palegreen4",
    alpha = 0.75,
    shape = 16) +
  #ylim(-1, max(Accuracy$accuracy)+0.1)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  xlab("Equivalent Per-Generation Substitution Rate (SNPs/Generation)")+
  ylab("Accuracy (natural log of the ratio)") +
  theme_bw() + 
  coord_trans(x = "log") + scale_x_continuous(breaks=c(0.05,0.1,0.25,0.5,1,2))+
  scale_color_viridis(name = "No. sequences",trans = "log", breaks = c(20,150,1000)) +
  facet_wrap(~percent_sampled,  labeller = labeller(percent_sampled = percent.labs))
dev.off()
#ggsave("plots/Figure 3.png")

#stats
mdl1 = lm(data = Accuracy, percent_sampled~accuracy)
summary(mdl1)
mdl2 = lm(data = Accuracy, no_cases~accuracy)
summary(mdl2)
mdl3 = lm(data = Accuracy, trueSNPRate~accuracy)
summary(mdl3)

sqrt(mean((Accuracy$accuracy[Accuracy$no_cases < 100])^2))
sqrt(mean((Accuracy$accuracy[Accuracy$no_cases >= 100 & Accuracy$no_cases < 200])^2))
sqrt(mean((Accuracy$accuracy[Accuracy$no_cases >= 200 & Accuracy$no_cases < 1000])^2))
sqrt(mean((Accuracy$accuracy[Accuracy$no_cases < 1000])^2))

#FIGURE 4-------------------------------------------------------------------------------
tipDists = read.csv("output/pemba/multiplied_posteriors.csv")

bw = 0.005
n_obs = 124299

p4 = ggplot(data = data.frame(x = c(0, 0.35)), aes(x)) +
  geom_histogram(data= tipDists, aes(x), binwidth = bw, alpha=0.3, fill = "palegreen3")+
  stat_function(fun = function(x)
    dgamma(x, shape = 51.69189, rate = 301.8095)* bw * n_obs, colour = "darkgreen", size = 1.1) +
  ylab("") +
  xlab("SNPs per Generation")+
  theme_bw() + xlim(0,0.35)

library(ggplot2)
library(patchwork)
library(Rmisc)

novel_0= dpois(0, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_1= dpois(1, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_2= dpois(2, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_3= dpois(3, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_4= dpois(4, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_5= dpois(5, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_6= dpois(6, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_7= dpois(7, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_8= dpois(8, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_9= dpois(9, rgamma(10000,shape = 51.69189, rate = 301.8095))
novel_10= dpois(10, rgamma(10000,shape = 51.69189, rate = 301.8095))

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

##FIVE GENS                 
novel_0= dpois(0, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_1= dpois(1, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_2= dpois(2, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_3= dpois(3, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_4= dpois(4, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_5= dpois(5, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_6= dpois(6, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_7= dpois(7, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_8= dpois(8, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_9= dpois(9, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)
novel_10= dpois(10, rgamma(10000,shape = 51.69189, rate = 301.8095)*5)


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


##TEN GENS

novel_0= dpois(0, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_1= dpois(1, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_2= dpois(2, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_3= dpois(3, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_4= dpois(4, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_5= dpois(5, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_6= dpois(6, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_7= dpois(7, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_8= dpois(8, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_9= dpois(9, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)
novel_10= dpois(10, rgamma(10000,shape = 51.69189, rate = 301.8095)*10)


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

library(ggpubr)

p5 = p1/p2/p3

png("plots/Figure4_hi_res.png", width = 2000, height = 1500, units = 'px', res = 300)
ggarrange(p4+ rremove("y.text"), p5, nrow = 1,
          common.legend = TRUE, legend="bottom", labels = c("A", "B"))
dev.off()

#ggsave("plots/Figure 4.png")

##SUPPLEMENTARY FIGURE 1---------------------------------------------------

df = read.csv("output/simulation/clockrate-gen-model-comparison.csv")
df = df[is.na(df$Method) == F,]
df$Method[df$Method == "Clock Rate"] = "Time"

p = ggplot(data = df, aes(x = EquivalentPerGenRate, y = R_Squared, group = Method)) +
  geom_point(aes(colour = Method), 
             alpha = 0.5,shape = 16, size = 2) +
  geom_smooth(se=F, aes(colour = Method))+
  scale_x_continuous(breaks = c(0.05,0.1,0.2,0.5,1,2,3))+
  coord_trans(x='log10')+
  xlab("Equivalent Per-Generation Substitution Rate (SNPs/Generation)") + ylab(expression(R^2))+
  theme_bw() +
  scale_colour_manual("", values = c("red", "blue")) +
  scale_fill_manual(name='Mutation Model',
                    values=c('Generation'='red','Time'='blue')) +
  scale_linetype_manual("", values = c("solid", "dashed")) +
  guides(linetype = "none")+
  facet_wrap(~PercentSampled)

##SUPPLEMENTARY FIGURE 2---------------------------------------------------------------
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
dataFull = read.csv("output/simulation/simFullCases_perGen_24_0.000166666666666667_124.csv")
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
ggsave("plots/Supp Fig 2.png")