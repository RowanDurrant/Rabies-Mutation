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
  geom_point(data = simTipDists, aes(y = snpsPerGen, x = TimeDiffYears),
             colour = "lightgrey", alpha = 1)+
  geom_point(alpha=.35, colour = "red") +
  xlim(0,15)+
  #geom_hline(aes(yintercept = mean(snpsPerGen)), lty = 2)+
  facet_wrap(~ Lineage, 
             labeller = labeller(Lineage = lineageNames)) +
  theme_bw() +
  ylab("SNPs per Generation")+
  xlab("Temporal Distance (Years)")+
  labs(title = "B")

p6 + p7