# -----------------------
# Author: Mike Ackerman
# Purpose: Query virtual test tag data for all Snake River sites for
#   a given year.
# 
# Created: April 29, 2024
#   Last Modified: 
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(janitor)
library(here)

# query interrogation site metadata
iptds_meta = queryInterrogationMeta() %>%
  clean_names()

# snake river interrogation site meta
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
yrs = iptds_ops %>%
  group_by(site_code) %>%
  summarise(year = list(seq(min(year(first_date)), max(year(last_date)), by = 1))) %>%
  unnest(year)

# summarize the years and days that each iptds was installed per year
iptds_ops %<>%
  left_join(yrs, by = "site_code") %>%
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
         everything()) ; rm(yrs)

#---------------------
# Query virtual test tags via PTAGIS API requests

# year of interest
yr = 2023

# create list of sites that were operational for the given year
sites = yrs %>%
  filter(year == yr) %>%
  select(site_code) %>%
  pull()

# set MAs api key
api_key = "35AA5C57-B2BA-4BF0-A862-E19386625F71"

# query virtual test tag data for all sites in a given year
for(s in sites) {
  # use queryTestTagSite() from PITcleanr to send virtual test tag API request to PTAGIS
  vtt_df = queryTestTagSite(site_code = s,
                            year = yr,
                            api_key = api_key)
  
  # if vtt_df exists continue; otherwise, move onto next loop
  if( exists("vtt_df") ) {
    saveRDS(vtt_df, paste0(here("output/virtual_test_tags"), "/", s, "_", yr, ".rds"))
    print(paste0("Virtual test tag data saved for site ", s, ", year ", yr, "."))
  } else {
    print(paste0("No virtual test tag data for site ", s, ", year ", yr, "."))
  }
} # end sites loop

### END SCRIPT