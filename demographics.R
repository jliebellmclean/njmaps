# Author: Julia Liebell-McLean
# Date: December 2 - 
# Title: Demographic maps
# Notes: Use census data to graph demographic trends in NJ over time

library(devtools)
library(maps)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(tidycensus)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(sf)

# Set the working directory
setwd("C:\\Users\\julia\\OneDrive\\Documents\\GitHub\\njmaps")

# Read in the county shapefile data which is available from the Census (or the class drive)
county_all <- st_read("data/cb_2024_us_county_500k.shp")

# Filter to just New Jersey
county <- county_all |>
  filter(STATEFP == 34)

# Load 2020 population for NJ counties, without geometry
census <- get_decennial(
  geography = "county",
  variables = "P1_001N",   # total population
  state = "NJ",
  year = 2020,
  geometry = FALSE
)

#Look at the data
census


# need the codes for race, income, education
# pull for three census counts
# creates five graphics with three years per graph
