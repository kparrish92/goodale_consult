library(tidyverse)
library(here)
mv_long = read.csv(here("data", "tidy", "mv_long.csv"))
dz_long = read.csv(here("data", "tidy", "dz_long.csv")) 

long_all = rbind(mv_long, dz_long)

long_all$tones = gsub("L*L%"  , "L* L%" , long_all$tones , fixed = TRUE)

