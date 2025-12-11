# Author: Julia Liebell-McLean
# Date: November 24 - December 11
# Title: Partisan Lean Graphs
# Notes: Use presidential, gubernatorial, and senate election data to show 
#         trend shifts between 2000 and the current day. Graph in terms of
#         partisan lean at the county level.
# Outputs: Four maps: general county map; presidential elections; gubernatorial
#           elections; senate elections

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

# Create a fairly simple map of NJ counties
map_colors <- c("turquoise3", "turquoise3", "pink3", 
                "forestgreen", "orange2", "gold",
                "forestgreen", "orange2", "orange2",
                "turquoise3", "gold", "orange2",
                "forestgreen", "orange2", "orange2",
                "gold", "pink3", "forestgreen",
                "pink3", "pink3", "gold")

p <- ggplot(county) + 
  geom_sf(aes(fill = NAME), color = "white") +
  geom_sf_text(
    aes(label = NAME),   # label with county name
    size = 2.5,
    color = "black",
    face = "bold"
  ) +
  labs(title = "New Jersey Counties") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),                # remove axis text
    axis.ticks = element_blank(),               # remove axis ticks
    axis.title = element_blank(),               # remove gridlines
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
    ) +       
  scale_fill_manual(values = map_colors)

p

# Save the NJ maps
ggsave("figures/county-color.png", plot = p, width = 4, dpi = 300)


# PRESIDENTIAL ELECTIONS

# Load data about presidential election results from Carlos. This is publicly available 
# at https://dataverse.harvard.edu/file.xhtml?fileId=5028532&version=1.1. A copy 
# is saved to the data file.
load("data/dataverse_shareable_presidential_county_returns_1868_2020.Rdata")

# Specify which years we are interested in
years <- c(2000, 2004, 2008, 2012, 2016, 2020)

# Select the interesting categories
pres <- pres_elections_release |>
  select(election_year, fips, sfips, office, election_type,
         democratic_raw_votes, republican_raw_votes,
         pres_raw_county_vote_totals_two_party)

# Select for the right years
pres <- pres |>
  filter(election_year %in% years)

# Double check that the office and election_type categories are meaningless
# unique(pres$office)
# unique(pres$election_type)
# Yep, they're all "G" and "PRES"

# Drop some columns and filter to NJ only
pres <- pres |>
  filter(sfips == 34) |>
  select(-office, -election_type)

# Double check that the total two party vote number is correct
# pres1 <- pres |>
# mutate(total = democratic_raw_votes + republican_raw_votes,
# check = pres_raw_county_vote_totals_two_party - total)
# unique(pres1$check)

# Calculate the percent of Democrat and Republican votes and partisan lean
pres2 <- pres |>
  mutate(pct_dem = democratic_raw_votes/pres_raw_county_vote_totals_two_party,
         pct_rep = republican_raw_votes/pres_raw_county_vote_totals_two_party,
         d_points = pct_dem - pct_rep)|>
  # Drop everything except the lean, county ID, and the election years
  select(election_year, fips, d_points) |>
  # Rename the fips code for merging
  rename(GEOID = fips)

# Isolate the geometry and fips code
geo <- county |>
  select(GEOID, geometry)

# Add on the geometry to the election results
pres2 <- pres2 |>
  left_join(geo, by = "GEOID")

# I got the idea for using facet_wrap from Copilot and then adjusted the code.

# Graph the partisan lean in a given year, by county, for NJ
p <- ggplot(pres2, aes(fill = d_points, geometry = geometry)) +
  geom_sf(color = "grey2", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#CB454A", mid = "white", high = "#2E74C0",
    midpoint = 0,
    name = "Partisan Lean"
  ) +
  facet_wrap(~ election_year, ncol = 3) +
  labs(title = "Partisan Lean in Presidential Elections, 2000–2020") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )
p

# Save the figure
ggsave("figures/pres.png", plot = p, dpi = 300)


# GUBERNATORIAL ELECTIONS

load("data/dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata")

# Double check that the office and election_type categories are meaningless
# unique(pres$office)
# unique(pres$election_type)
# Yep, they're all "G" and "GOV"

# Drop some columns and filter to NJ only
gov <- gov_elections_release |>
  filter(sfips == 34) |>
  select(-office, -election_type)

# Specify which years we are interested in
years <- c(2005, 2007, 2009, 2013, 2017)

# Select the interesting categories
gov <- gov |>
  select(election_year, fips, sfips,
         democratic_raw_votes, republican_raw_votes,
         gov_raw_county_vote_totals_two_party)

# Select for the right years
gov <- gov |>
  filter(election_year %in% years)

# Calculate the percent of Democrat and Republican votes and partisan lean
gov2 <- gov |>
  mutate(pct_dem = democratic_raw_votes/gov_raw_county_vote_totals_two_party,
         pct_rep = republican_raw_votes/gov_raw_county_vote_totals_two_party,
         d_points = pct_dem - pct_rep)|>
  # Drop everything except the lean, county ID, and the election years
  select(election_year, fips, d_points) |>
  # Rename the fips code for merging
  rename(GEOID = fips)

# Isolate the geometry and fips code
geo <- county |>
  select(GEOID, geometry)

# Add on the geometry to the election results
gov2 <- gov2 %>%
  left_join(geo, by = "GEOID")

# Add in information from 2021 and 2025
# I pulled information from the NJ state website:
# https://www.nj.gov/state/elections/election-night-results.shtml
# For 2025, I was redirected to the County Clerk websites for all counties
# I pulled that information
# I then corroborated the numbers against the AP's reported results. They match

gov_updated <- read.csv("data/updated_gov.csv")

# Rename the name category
gov_updated <- gov_updated |>
  rename(NAME = county)

# Isolate the county names and fips codes
county_code <- county |>
  select(GEOID, NAME)

# Merge the GEOIDs onto the new election results
gov_updated <- gov_updated %>%
  left_join(county_code, by = "NAME")

# Specify all the column names in the cleaned gov2 dataset
colnames(gov2)

# Make sure that the column names will match in the updated gov
gov_updated <- gov_updated |>
  # Rename election year
  rename(election_year = year) |>
  mutate(
    # Create a totals column
    total = d_votes + r_votes,
    # Calculate Democratic and Republican vote percentages
    pct_dem = d_votes/total,
    pct_rep = r_votes/total,
    # Calculate lean
    d_points = pct_dem - pct_rep)

# Drop everything except the lean, county ID, and the election years
gov_updated <- gov_updated |>
  select(election_year, GEOID, d_points, geometry)

# Bind the newer gubernatorial data onto the older data
gov3 <- rbind(gov2, gov_updated)
  
# Graph the partisan lean in a given year, by county, for NJ
p <- ggplot(gov3, aes(fill = d_points, geometry = geometry)) +
  geom_sf(color = "grey2", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#CB454A", mid = "white", high = "#2E74C0",
    midpoint = 0,
    name = "Partisan Lean"
  ) +
  facet_wrap(~ election_year, ncol = 3) +
  labs(title = "Partisan Lean in Gubernatorial Elections, 2005–2025") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )
p

# Save the figure
ggsave("figures/gov.png", plot = p, dpi = 300)

# Now, make some of the same maps for the Senate races

# Load senate data
load("data/dataverse_shareable_us_senate_county_returns_1908_2020.Rdata")

# Filter down to just NJ results
senate <- senate_elections_release |>
  filter(sfips == 34)

# Check what years are included
# unique(senate$election_year)
# Okay, that goes up until 2020. Does not capture Andy Kim's election in 2024

# Specify which years we are interested in
years <- c(2000, 2002, 2006, 2008, 2012, 2013, 2014, 2018, 2020)

# Select the interesting categories
senate <- senate |>
  select(election_year, fips, sfips, office,
         democratic_raw_votes, republican_raw_votes,
         senate_raw_county_vote_totals_two_party)

# Select for the right years
senate <- senate |>
  filter(election_year %in% years)

# Double check that there are only senate elections left
# unique(senate$office)

# Drop some columns and filter to NJ only
senate <- senate |>
  filter(sfips == 34) |>
  select(-office)

# Double check that the total two party vote number is correct
# sen1 <- senate |>
# mutate(total = democratic_raw_votes + republican_raw_votes,
# check = senate_raw_county_vote_totals_two_party - total)
# unique(sen1$check)

# Calculate the percent of Democrat and Republican votes and partisan lean
senate1 <- senate |>
  mutate(pct_dem = democratic_raw_votes/senate_raw_county_vote_totals_two_party,
         pct_rep = republican_raw_votes/senate_raw_county_vote_totals_two_party,
         d_points = pct_dem - pct_rep)|>
  # Drop everything except the lean, county ID, and the election years
  select(election_year, fips, d_points) |>
  # Rename the fips code for merging
  rename(GEOID = fips)

# Isolate the geometry and fips code
geo <- county |>
  select(GEOID, geometry)

# Add on the geometry to the election results
senate2 <- senate1 |>
  left_join(geo, by = "GEOID")

# Now graph:
# Graph the partisan lean in a given senate election year, by county, for NJ
p <- ggplot(senate2, aes(fill = d_points, geometry = geometry)) +
  geom_sf(color = "grey2", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#CB454A", mid = "white", high = "#2E74C0",
    midpoint = 0,
    name = "Partisan Lean"
  ) +
  facet_wrap(~ election_year, ncol = 3) +
  labs(title = "Partisan Lean in Senate Elections, 2000–2020") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )
p

# Note, NJ hasn't elected a Republican senator since 1972 (two were appointed to 
# finish terms but did not seek/win re-election). 

# Save the figure
ggsave("figures/senate.png", plot = p, dpi = 300)
