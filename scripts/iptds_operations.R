# -----------------------
# Author: Mike Ackerman
# Purpose: Compile some information on IPTDS operational times
# 
# Created: April 22, 2024
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
         longitude)
rm(iptds_meta)

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

# snake river interrogation site configuration
sr_iptds_config = queryInterrogationConfig() %>%
  clean_names() %>%
  filter(site_code %in% sr_iptds_meta$site_code)

#---------------------
# Testing PTAGIS API requests

# virtual test tags; an example
api_key = '1AA5CF55-C98E-4001-96E4-1E96CEE1E806' # replace with my own
test_tag = queryTestTagSite(site_code = "ZEN",
                            year = 2023,
                            api_key = api_key)

# plot test tag info for a single site and year
test_tag %>%
  ggplot(aes(x = time_stamp, y = 1)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  #facet_grid(antenna_id~transceiver_id)
  facet_wrap(~antenna_id, ncol = 1)

# consider code to query all sites for a year, either from PTAGIS or BioLogic

# interrogation file information based upon site code and year
site_code = "ZEN"
yr = "2023"
url = paste0("https://api.ptagis.org/files/interrogation/sites/", site_code, "/year/", yr)
web_req = httr::GET(url) %>%
  httr::content(., "parsed")

# list of interrogation files for a site and year
int_file_meta = web_req$model %>%
  bind_rows()

# get one interrogation file name
int_file = int_file_meta$fileName[1]
url = paste0("https://api.ptagis.org/files/interrogation/", int_file)
web_req = httr::GET(url) %>%
  httr::content(., "parsed")

# a single interrogation file
int_file = web_req$interrogationFieldData %>%
  bind_rows()

#---------------------
# Playing around w/ BioLogic API requests

#library(fisheR)

# log into Biomark's BioLogic database to retrive an API token
source(here("R/biologic_login.R")) # taken from fisheR
email = "mikea@nezperce.org"
password = "#npt1855"
biologic_login(email, password)

# pass API token to BioLogic to retrieve vector of sites that user has permissions
source(here("R/get_biologic_sites.R")) # taken from fisheR
biologic_sites = get_biologic_sites()

site = c("KRS", "SFG", "ZEN", "ESS")
begin_dt = "2024-01-01"
end_dt = Sys.Date()

# pass API token to BioLogic to retrieve site environmental data
source(here("R/get_biologic_data.R")) # taken from fisheR

# retrieve environmental data for a single site
site_env_df = get_biologic_data(site = site[1],
                                endpoint = "enviro",
                                begin_dt = begin_dt,
                                end_dt = end_dt) %>%
  select(reader.site.slug,
         parameter.slug,
         parameter.units,
         read_at,
         value)

# which metrics are available?
site_env_df %>%
  tabyl(parameter.slug)

# retrieve and save environmental data for multiple sites
for(st in site) {
  df = get_biologic_data(site = st,
                         endpoint = "enviro",
                         begin_dt = begin_dt,
                         end_dt = end_dt) %>%
    select(reader.site.slug,
           parameter.slug,
           parameter.units,
           read_at,
           value)
  
  saveRDS(df, paste0(here("data/raw_data/enviro"), "/", st, "_", begin_dt, "_", end_dt, ".rds"))
}

# this option currently not working due to a url parsing error
# map(site,
#     .f = function(x){
#       df = get_biologic_data(site = site,
#                              endpoint = "enviro",
#                              begin_dt = begin_dt,
#                              end_dt = end_dt) %>%
#         select(reader.site.slug,
#                parameter.slug,
#                parameter.units,
#                read_at,
#                value)
#       
#       saveRDS(df, paste0(here("data/raw_data/enviro"), "/", st, "_", begin_dt, "_", end_dt, ".rds"))
#     })

endpoint = c('tags', 'reader', 'enviro', 'antenna')
# retrieve tag data (possibility to return test tags?)
tag_df = get_biologic_data(site = site[1],
                           endpoint = "tags",
                           begin_dt = begin_dt,
                           end_dt = end_dt)

# retrieve reader data
reader_df = get_biologic_data(site = site[1],
                              endpoint = "reader",
                              begin_dt = begin_dt,
                              end_dt = end_dt)

site_env_df = get_biologic_data(site = site[1],
                                endpoint = "enviro",
                                begin_dt = begin_dt,
                                end_dt = end_dt) %>%
  select(reader.site.slug,
         parameter.slug,
         parameter.units,
         read_at,
         value)
for(st in site) {
  df = get_biologic_data(site = st,
                         endpoint = "enviro",
                         begin_dt = begin_dt,
                         end_dt = end_dt) %>%
    select(reader.site.slug,
           parameter.slug,
           parameter.units,
           read_at,
           value)

### END SCRIPT

