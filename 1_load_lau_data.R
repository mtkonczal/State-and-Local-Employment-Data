###############################################################
# Code to read in JOLTS data from CPS website and begin analysis.
# This file reads in and store all the JOLTS monthly jobs data.
#
# Recent fix: added email to calls to work with new BLS protocols.
#
# Mike Konczal
# Last updated 5/1/2023

library(janitor)
library(tidyverse)
library(ggtext)
library(lubridate)
library(data.table)
library(httr)

#### State and Local Unemployment ####

lau <- GET("https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()
lau$value <- as.numeric(lau$value)
lau$date <- paste(substr(lau$period, 2,3), "01", lau$year, sep="/")
lau$date <- as.Date(lau$date, "%m/%d/%Y")

lau_series <- GET("https://download.bls.gov/pub/time.series/la/la.series", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()
lau_series$series_id <- str_trim(lau_series$series_id)


la_area_type <- GET("https://download.bls.gov/pub/time.series/la/la.area_type", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()

la_state_region <-  GET("https://download.bls.gov/pub/time.series/la/la.state_region_division", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()

lau <- lau %>%
  inner_join(lau_series, by="series_id") %>%
  inner_join(la_area_type, by="area_type_code") %>%
  inner_join(la_state_region, by="srd_code")


rm(lau_series, la_area_type)