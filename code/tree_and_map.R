library(tidyverse)
library(ggtree)
library(treeio)
library(ggmap)
library(grid)


tree = read.beast("../Pemba/data/genetic/pemba_tz_n153_timescaled.mcc.tre")

tipcolours = c()
for(i in 1:length(tree@phylo$tip.label)){
  tipcolours[i] = strsplit(tree@phylo$tip.label[i], '[!?._][[:space:]]*')[[1]][3]
  tree@phylo$tip.label[i] = strsplit(tree@phylo$tip.label[i], "_")[[1]][1]
}
d <- data.frame(node=1:(Nnode(tree)+length(tree@phylo$tip.label)), 
                location = c(tipcolours, rep("black", Nnode(tree))))

d2 <- data.frame(location = c("Dodoma", "Kibaha", "Kilombero",
                              "Morogoro", "Nachingwea", "Newala",
                              "Ngorongoro", "Pemba", "Same", 
                              "Serengeti", "Tameke", "Ulanga"),
                 lat = c(-6.16321839449851, -6.78292087610276, -8.094573329844343,
                         -6.829822582806052, -10.331969188962217,-10.729783497624434,
                         -3.242787751353734,-5.214532107434417,-4.059036129939253,
                         -1.9078154430190237, -6.939700978622705, -8.907696367406384),
                 long = c(35.75237861054599, 38.99129390980104, 36.67142740598683,
                          37.65825142647812, 38.715948470886694, 39.29205406542055,
                          35.4939020975142,  39.77440205076576, 37.75980296830634,
                          34.753567137337896, 39.37371777456853, 36.824963731565475))


p = ggtree(tree, mrsd="2018-01-01") + 
  theme_tree2() + 
  #geom_tiplab(align=TRUE, linesize=.5, size = 3) + 
  xlim(1950, 2022)

p = p %<+% d  +  
  geom_tippoint(aes(color=location), size=3, alpha=1) +
  scale_color_brewer("location", palette="Paired")
p

tanzania = get_stamenmap(bbox = c(left = 33, bottom = -11, right =
                         43, top = -1.5), zoom = 6, maptype = 'toner-lite')


p_tanzania = ggmap(tanzania) + 
  geom_point(data = d2, aes(x = long, y = lat, colour = location), size=4, alpha=1) +
  scale_color_brewer("location", palette="Paired")+
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

p +
  inset(ggplotGrob(p_tanzania), xmin = 1950, xmax = 1995, ymin = 60, ymax = 150)
