# -----------------------
# Author: Mike Ackerman
# Purpose: Compile some information on IPTDS operational times.
# 
# Created: April 22, 2024
#   Last Modified: May 6, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(janitor)
library(here)
library(fisheR)

# query interrogation site metadata
iptds_meta = queryInterrogationMeta() %>%
  clean_names()
  
# snake river interrogation site metadata
sr_iptds_meta = iptds_meta %>%
  filter(str_starts(rkm, "522"),
         site_type %in% c("Instream Remote Detection System",
                          "Adult Fishway"),
         # removes the four Snake River dams
         !operations_organization_code == "PSMFC") %>%
  select(site_code,
         name,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date,
         last_file_opened_on,
         operation_period,
         operations_organization_code,
         rkm,
         site_type,
         latitude,
         longitude) ; rm(iptds_meta)

# summarize iptds operational dates
iptds_ops = sr_iptds_meta %>%
  # get the latest date for each site
  group_by(site_code) %>%
  mutate(last_date = max(last_date, last_file_opened_on, na.rm = T)) %>%
  ungroup() %>%
  select(site_code,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date) %>%
  mutate(first_date = as.Date(first_date),
         last_date = as.Date(last_date)) %>%
  # BED doesn't have a first_date, grabbed from PTAGIS
  mutate(first_date = if_else(site_code == "BED", as.Date("2024-02-15"), first_date)) 

# sequence of years that each site was in operation
site_yrs = iptds_ops %>%
  group_by(site_code) %>%
  summarise(year = list(seq(min(year(first_date)), max(year(last_date)), by = 1))) %>%
  unnest(year)

# summarize the years and days that each iptds was installed per year
iptds_ops %<>%
  left_join(site_yrs, by = "site_code") %>%
  group_by(site_code, year) %>%
  summarise(
    days = sum(pmax(0, pmin(last_date, as.Date(paste0(year, "-12-31"))) - pmax(first_date, as.Date(paste0(year, "-01-01"))) + 1))
  ) %>%
  spread(year, days, fill = 0) %>%
  left_join(iptds_ops, by = "site_code") %>%
  select(site_code,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date,
         everything()) ; rm(site_yrs)

# snake river interrogation site configuration
sr_iptds_config = queryInterrogationConfig() %>%
  clean_names() %>%
  filter(site_code %in% sr_iptds_meta$site_code)

# which iptds sites are in biologic?
source(here("keys/biologic_login.txt"))
biologic_login(email, password)
biologic_sites = get_biologic_sites() # biologic sites that i have permission for

biologic_sites = sr_iptds_meta %>%
  filter(site_code %in% biologic_sites)

# write out biologic sites to a .csv
write_csv(biologic_sites,
          file = paste0(here("output/biologic_sites_"), Sys.Date(), ".csv"))

### END SCRIPT

