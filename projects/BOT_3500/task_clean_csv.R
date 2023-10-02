library(dplyr)

df <- read.csv("./projects/BOT_3500/funtothefun.csv") %>% 
  filter(trait_name == c("cellobiohydrolase6_count", "substrate", "trophic_mode_fg"))