# Author: Mike Ackerman
# Purpose: Query Snake River PIT tag MRR and INT sites from PTAGIS
# 
# Created: June 6, 2023
# Last Modified:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
# library(here)
# library(sf)
# library(janitor)

# load PITcleanr
# remotes::install_github("mackerman44/PITcleanr@main", build_vignettes = T, force = T)
library(PITcleanr)
browseVignettes("PITcleanr")
