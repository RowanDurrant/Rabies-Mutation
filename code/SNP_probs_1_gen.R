library(ggplot2)
library(patchwork)

#sample(1:1000, 1) #216
set.seed(216)

a = rpois(100000, 0.17)
b = table(a)/sum(table(a))
c = as.data.frame(b)

p1 = ggplot(data=c, aes(x=a, y=Freq)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("1 Generation")+
  coord_cartesian(xlim = c(1, 10)) + ylim(0,1)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))  

 
a = rpois(100000, 0.17*5)
b = table(a)/sum(table(a))
c = as.data.frame(b)

p2 = ggplot(data=c, aes(x=a, y=Freq)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("5 Generations")+ ylab("Probability of SNPs")+
  coord_cartesian(xlim = c(1, 10)) + ylim(0,1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

a = rpois(100000, 0.17*10)
b = table(a)/sum(table(a))
c = as.data.frame(b)

p3 = ggplot(data=c, aes(x=a, y=Freq)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("10 Generations") +
  coord_cartesian(xlim = c(1, 10)) + ylim(0,1) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

p1/p2/p3
