setwd("/Users/mkonczal/Documents/GitHub/State-and-Local-Employment-Data/")

library(janitor)
library(tidyverse)
library(ggtext)

############### NOW TO DO STATE AND LOCAL DATA #################################################################
# This code reads in and analyzes state and local employment (SAE) data from the BLS. This is based on the CES survey.
# Created: 2-15-22
# Last Edited: 2-23-22
# Mike Konczal
# Let's see if both changes happen.

# Currently this is designed to investigate where the missing state and local workers are in the covid-19 recovery.
# We want to check if the job losses are disproportionately:
# - located in states versus local employment,
# - across states,
# - and in education versus non-education.

# Initial dive (2-15-22) finds that the jobs missing are broad based across all. Next steps are to present that
# data better in one clear table, and to compare and contrast with the Great Recession.

############### READ IN AND CLEAN UP STATE AND LOCAL EMPLOYMENT DATA ########################################

# Read in files
state_industry_codes <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.industry")
state_data_type_codes <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.data_type")
supersector_codes <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.supersector")
state_codes <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.state")
area_codes <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.area")

sae_series_code <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.series")
sae_series_code <- sae_series_code %>%
  clean_names()
sae_series_code$series_id <- str_trim(sae_series_code$series_id)

sae <- read_delim(file = "https://download.bls.gov/pub/time.series/sm/sm.data.0.Current")
sae <- sae %>%
  clean_names()
sae$value <- as.numeric(sae$value)
sae$series_id <- str_trim(sae$series_id)

sae <- inner_join(sae, sae_series_code, by = c("series_id"))
sae <- inner_join(sae, state_codes, by = c("state_code"))
sae <- inner_join(sae, area_codes, by = c("area_code"))
sae <- inner_join(sae, supersector_codes, by = c("supersector_code"))
sae <- inner_join(sae, state_industry_codes, by = c("industry_code"))
sae <- inner_join(sae, state_data_type_codes, by = c("data_type_code"))

# Remove some columns we won't use, but may in the future.
sae <- select(sae, -c("footnote_codes.x", "footnote_codes.y", "benchmark_year", "begin_year", "begin_period", "end_year", "end_period"))

save(sae, file = "data/sae.RData")
