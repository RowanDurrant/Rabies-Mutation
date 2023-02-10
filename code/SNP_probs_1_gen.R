library(ggplot2)
library(patchwork)

#sample(1:1000, 1) #216
set.seed(216)

a = rpois(100000, 0.1690)
b = table(a)/sum(table(a))
c = as.data.frame(b)
c$Method = "Novel"
d = rpois(100000, 0.1075)
e = table(d)/sum(table(d))
f = as.data.frame(e)
f$Method = "Clock"
colnames(f) = c("a", "Freq", "Method")
g = rbind(c,f)
p1 = ggplot(data=g, aes(x=a, y=Freq, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ylab("1 Generation")+
  coord_cartesian(xlim = c(1, 10)) + ylim(0,1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")  +
  scale_fill_manual("", values = c("blue", "red"))

 
a = rpois(100000, 0.1690*5)
b = table(a)/sum(table(a))
c = as.data.frame(b)
c$Method = "Novel"
d = rpois(100000, 0.1075*5)
e = table(d)/sum(table(d))
f = as.data.frame(e)
f$Method = "Clock"
colnames(f) = c("a", "Freq", "Method")
g = rbind(c,f)
p2 = ggplot(data=g, aes(x=a, y=Freq, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ylab("5 Generations")+
  coord_cartesian(xlim = c(1, 10)) + ylim(0,1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual("", values = c("blue", "red"))

a = rpois(100000, 0.1690*10)
b = table(a)/sum(table(a))
c = as.data.frame(b)
c$Method = "Novel Method"
d = rpois(100000, 0.1075*10)
e = table(d)/sum(table(d))
f = as.data.frame(e)
f$Method = "Clock Rate Method"
colnames(f) = c("a", "Freq", "Method")
g = rbind(c,f)
p3 = ggplot(data=g, aes(x=a, y=Freq, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ylab("10 Generations") +
  coord_cartesian(xlim = c(1, 10)) + ylim(0,1)+
  theme(legend.position = "bottom")+
  scale_fill_manual("", values = c("blue", "red"))

p1/p2/p3
