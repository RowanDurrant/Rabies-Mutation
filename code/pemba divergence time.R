df = read.table("input/pemba non timescaled.txt", header = T)
clusters = read.csv("input/Pemba_assignment.csv")
df = df[df$tip %in% clusters$ID,]

plot(df$distance ~ df$date)
lm = lm(df$distance ~ df$date)
mysubtitle = paste("X intercept = ",(coef(lm)[1] * -1)/coef(lm)[2])
mtext(mysubtitle, side = 3, line = 0.25, cex = 0.65)
abline(lm)

df$lineage = NA

for(i in 1:nrow(df)){
  df$lineage[i] = clusters$lineage[clusters$ID == df$tip[i]]  
}

plot(df$distance ~ df$date, col = as.factor(df$lineage), pch =16)
lm = lm(df$distance ~ df$date)
abline(lm)

library(ggplot2)
library(RColorBrewer)

p2 = ggplot(data = df, aes(x = date, y = distance))  +
  geom_point(aes(col = lineage)) +
  stat_smooth(method="lm",fullrange=TRUE,se=F, col = "black")+
  scale_color_brewer("Lineage", palette="Paired")+
  ylab("root-to-tip distance") + 
  theme_bw() 
p2+ annotate("text",
             label = paste("x intercept =",prettyNum((coef(lm)[1] * -1)/coef(lm)[2])),
  x = 2005, y = 0.016)

ggsave("plots/Figure 4.png")
