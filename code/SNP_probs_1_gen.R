library(ggplot2)

#sample(1:1000, 1) #121
set.seed(121)
a = rpois(100000, 0.1690)
b = table(a)/sum(table(a))
c = as.data.frame(b)
c$Method = "Novel"

d = rpois(100000, 0.1113)
e = table(d)/sum(table(d))
f = as.data.frame(e)
f$Method = "Clock"
colnames(f) = c("a", "Freq", "Method")

g = rbind(c,f)

ggplot(data=g, aes(x=a, y=Freq, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_bw() +
  xlab("No. SNPs") + ylab("Probability of Occurence in 1 Generation")
 