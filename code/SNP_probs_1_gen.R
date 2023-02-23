library(ggplot2)
library(patchwork)
library(Rmisc)

clock_0= dpois(0, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_0= dpois(0, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_1= dpois(1, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_1= dpois(1, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_2= dpois(2, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_2= dpois(2, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_3= dpois(3, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_3= dpois(3, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_4= dpois(4, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_4= dpois(4, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_5= dpois(5, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_5= dpois(5, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_6= dpois(6, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_6= dpois(6, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_7= dpois(7, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_7= dpois(7, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_8= dpois(8, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_8= dpois(8, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_9= dpois(9, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_9= dpois(9, rgamma(10000,shape = 10.03098, rate = 59.26635))
clock_10= dpois(10, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017))
novel_10= dpois(10, rgamma(10000,shape = 10.03098, rate = 59.26635))

clock_means = c(mean(clock_0),mean(clock_1),mean(clock_2),mean(clock_3),mean(clock_4),
                mean(clock_5),mean(clock_6),mean(clock_7),mean(clock_8),mean(clock_9),
                mean(clock_10))
clock_CI_upper = unname(c(CI(clock_0)[1],CI(clock_1)[1],CI(clock_2)[1],CI(clock_3)[1],CI(clock_4)[1],
                          CI(clock_5)[1],CI(clock_6)[1],CI(clock_7)[1],CI(clock_8)[1],CI(clock_9)[1],
                          CI(clock_10)[1]))
clock_CI_lower = unname(c(CI(clock_0)[3],CI(clock_1)[3],CI(clock_2)[3],CI(clock_3)[3],CI(clock_4)[3],
                          CI(clock_5)[3],CI(clock_6)[3],CI(clock_7)[3],CI(clock_8)[3],CI(clock_9)[3],
                          CI(clock_10)[3]))

novel_means = c(mean(novel_0),mean(novel_1),mean(novel_2),mean(novel_3),mean(novel_4),
                mean(novel_5),mean(novel_6),mean(novel_7),mean(novel_8),mean(novel_9),
                mean(novel_10))
novel_CI_upper = unname(c(CI(novel_0)[1],CI(novel_1)[1],CI(novel_2)[1],CI(novel_3)[1],CI(novel_4)[1],
                          CI(novel_5)[1],CI(novel_6)[1],CI(novel_7)[1],CI(novel_8)[1],CI(novel_9)[1],
                          CI(novel_10)[1]))
novel_CI_lower = unname(c(CI(novel_0)[3],CI(novel_1)[3],CI(novel_2)[3],CI(novel_3)[3],CI(novel_4)[3],
                          CI(novel_5)[3],CI(novel_6)[3],CI(novel_7)[3],CI(novel_8)[3],CI(novel_9)[3],
                          CI(novel_10)[3]))

Mean = c(clock_means, novel_means)
CI_upper = c(clock_CI_upper, novel_CI_upper)
CI_lower = c(clock_CI_lower, novel_CI_lower)

df = as.data.frame(cbind(Mean, CI_upper, CI_lower))
df$Method = c(rep("Clock Rate", 11), rep("Novel", 11))
df$gens = c(0:10, 0:10)


p1 = ggplot(data=df, aes(x=gens, y=Mean, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
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

clock_0= dpois(0, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_0= dpois(0, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_1= dpois(1, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_1= dpois(1, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_2= dpois(2, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_2= dpois(2, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_3= dpois(3, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_3= dpois(3, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_4= dpois(4, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_4= dpois(4, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_5= dpois(5, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_5= dpois(5, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_6= dpois(6, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_6= dpois(6, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_7= dpois(7, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_7= dpois(7, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_8= dpois(8, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_8= dpois(8, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_9= dpois(9, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_9= dpois(9, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)
clock_10= dpois(10, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*5)
novel_10= dpois(10, rgamma(10000,shape = 10.03098, rate = 59.26635)*5)

clock_means = c(mean(clock_0),mean(clock_1),mean(clock_2),mean(clock_3),mean(clock_4),
                mean(clock_5),mean(clock_6),mean(clock_7),mean(clock_8),mean(clock_9),
                mean(clock_10))
clock_CI_upper = unname(c(CI(clock_0)[1],CI(clock_1)[1],CI(clock_2)[1],CI(clock_3)[1],CI(clock_4)[1],
                          CI(clock_5)[1],CI(clock_6)[1],CI(clock_7)[1],CI(clock_8)[1],CI(clock_9)[1],
                          CI(clock_10)[1]))
clock_CI_lower = unname(c(CI(clock_0)[3],CI(clock_1)[3],CI(clock_2)[3],CI(clock_3)[3],CI(clock_4)[3],
                          CI(clock_5)[3],CI(clock_6)[3],CI(clock_7)[3],CI(clock_8)[3],CI(clock_9)[3],
                          CI(clock_10)[3]))

novel_means = c(mean(novel_0),mean(novel_1),mean(novel_2),mean(novel_3),mean(novel_4),
                mean(novel_5),mean(novel_6),mean(novel_7),mean(novel_8),mean(novel_9),
                mean(novel_10))
novel_CI_upper = unname(c(CI(novel_0)[1],CI(novel_1)[1],CI(novel_2)[1],CI(novel_3)[1],CI(novel_4)[1],
                          CI(novel_5)[1],CI(novel_6)[1],CI(novel_7)[1],CI(novel_8)[1],CI(novel_9)[1],
                          CI(novel_10)[1]))
novel_CI_lower = unname(c(CI(novel_0)[3],CI(novel_1)[3],CI(novel_2)[3],CI(novel_3)[3],CI(novel_4)[3],
                          CI(novel_5)[3],CI(novel_6)[3],CI(novel_7)[3],CI(novel_8)[3],CI(novel_9)[3],
                          CI(novel_10)[3]))

Mean = c(clock_means, novel_means)
CI_upper = c(clock_CI_upper, novel_CI_upper)
CI_lower = c(clock_CI_lower, novel_CI_lower)

df = as.data.frame(cbind(Mean, CI_upper, CI_lower))
df$Method = c(rep("Clock Rate", 11), rep("Novel", 11))
df$gens = c(0:10, 0:10)


p2 = ggplot(data=df, aes(x=gens, y=Mean, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("5 Generations")+
  ylab("Mean Probability of\nSNPs Occurring")+
  geom_errorbar( aes(x=gens, ymin=CI_lower, ymax=CI_upper), 
                 position = "dodge", stat = 'identity')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(0, 10)) + ylim(0,1)



##TEN GENS


clock_0= dpois(0, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_0= dpois(0, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_1= dpois(1, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_1= dpois(1, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_2= dpois(2, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_2= dpois(2, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_3= dpois(3, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_3= dpois(3, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_4= dpois(4, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_4= dpois(4, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_5= dpois(5, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_5= dpois(5, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_6= dpois(6, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_6= dpois(6, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_7= dpois(7, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_7= dpois(7, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_8= dpois(8, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_8= dpois(8, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_9= dpois(9, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_9= dpois(9, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)
clock_10= dpois(10, rlnorm(10000,meanlog =  -2.231138, sdlog = 0.9662017)*10)
novel_10= dpois(10, rgamma(10000,shape = 10.03098, rate = 59.26635)*10)

clock_means = c(mean(clock_0),mean(clock_1),mean(clock_2),mean(clock_3),mean(clock_4),
                mean(clock_5),mean(clock_6),mean(clock_7),mean(clock_8),mean(clock_9),
                mean(clock_10))
clock_CI_upper = unname(c(CI(clock_0)[1],CI(clock_1)[1],CI(clock_2)[1],CI(clock_3)[1],CI(clock_4)[1],
                          CI(clock_5)[1],CI(clock_6)[1],CI(clock_7)[1],CI(clock_8)[1],CI(clock_9)[1],
                          CI(clock_10)[1]))
clock_CI_lower = unname(c(CI(clock_0)[3],CI(clock_1)[3],CI(clock_2)[3],CI(clock_3)[3],CI(clock_4)[3],
                          CI(clock_5)[3],CI(clock_6)[3],CI(clock_7)[3],CI(clock_8)[3],CI(clock_9)[3],
                          CI(clock_10)[3]))

novel_means = c(mean(novel_0),mean(novel_1),mean(novel_2),mean(novel_3),mean(novel_4),
                mean(novel_5),mean(novel_6),mean(novel_7),mean(novel_8),mean(novel_9),
                mean(novel_10))
novel_CI_upper = unname(c(CI(novel_0)[1],CI(novel_1)[1],CI(novel_2)[1],CI(novel_3)[1],CI(novel_4)[1],
                          CI(novel_5)[1],CI(novel_6)[1],CI(novel_7)[1],CI(novel_8)[1],CI(novel_9)[1],
                          CI(novel_10)[1]))
novel_CI_lower = unname(c(CI(novel_0)[3],CI(novel_1)[3],CI(novel_2)[3],CI(novel_3)[3],CI(novel_4)[3],
                          CI(novel_5)[3],CI(novel_6)[3],CI(novel_7)[3],CI(novel_8)[3],CI(novel_9)[3],
                          CI(novel_10)[3]))

Mean = c(clock_means, novel_means)
CI_upper = c(clock_CI_upper, novel_CI_upper)
CI_lower = c(clock_CI_lower, novel_CI_lower)

df = as.data.frame(cbind(Mean, CI_upper, CI_lower))
df$Method = c(rep("Clock Rate", 11), rep("Novel", 11))
df$gens = c(0:10, 0:10)


p3 = ggplot(data=df, aes(x=gens, y=Mean, fill = Method)) + 
  geom_bar(position = "dodge", stat = 'identity') +
  theme_classic() +
  xlab("No. SNPs") + ggtitle("10 Generations")+
  geom_errorbar( aes(x=gens, ymin=CI_lower, ymax=CI_upper), 
                 position = "dodge", stat = 'identity')+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  coord_cartesian(xlim = c(0, 10)) + ylim(0,1) +
  scale_x_continuous(breaks = c(0:10), labels = c("0":"10"))


p1/p2/p3
