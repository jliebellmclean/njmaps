# Author: Julia Liebell-McLean
# Date: December 2 - 11
# Title: 2025 Gubernatorial Recreation
# Notes: Use self-compiled gubernatorial election data to recreate maps
#         made by the AP.
# Outputs: Two maps: general county map with vote share by dot size;
#           shift from 2021 gubernatorial election

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

# Obtain information from 2021 and 2025
# I pulled information from the NJ state website:
# https://www.nj.gov/state/elections/election-night-results.shtml
# For 2025, I was redirected to the County Clerk websites for all counties
# I pulled that information
# I then corroborated the numbers against the AP's reported results. They match

gov_updated <- read.csv("data/updated_gov.csv")

# Rename the name category
gov_updated <- gov_updated |>
  rename(NAME = county)

# Calculate the percent of Democrat and Republican votes and partisan lean
gov <- gov_updated |>
  mutate(totals_two_party = d_votes + r_votes,
         pct_dem = d_votes/totals_two_party,
         pct_rep = r_votes/totals_two_party,
         d_points = pct_dem - pct_rep)

# Read in the county shapefile data which is available from the Census (or the class drive)
county_all <- st_read("data/cb_2024_us_county_500k.shp")

# Filter to just New Jersey
geo <- county_all |>
  filter(STATEFP == 34) |>
  # Isolate the geometry, name and fips code
  select(GEOID, geometry, NAME)

# Add on the geometry to the election results
gov2 <- gov %>%
  left_join(geo, by = "NAME")





# Create the data set for ballot totals
map1 <- gov2 |>
  filter(year == 2025) |>
  select(totals_two_party, d_points, GEOID, geometry)

# Begin to graph the 2025 ballot count map from the AP

# Compute centroids of each county
map1 <- st_as_sf(map1)
county_centers <- st_centroid(map1)


p <- ggplot() +
  # County outlines
  geom_sf(data = map1, fill = NA, color = "grey2", linewidth = 0.05) +
  # Add a dot at the county centers
  geom_sf(data = county_centers, aes(color = d_points, size = totals_two_party), shape = 20) +
  scale_color_gradient2(
    low = "#D43D5A", mid = "white", high = "#2E75C9",
    midpoint = 0,
    name = "Partisan Lean"
  ) + 
  scale_size_continuous(
    range = c(5, 17),   # adjust min/max dot size
    name = "Ballots Counted"
  ) +
  labs(title = "Ballots Counted") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )

p

# The AP map also has two major cities

# Pull city coordinates
cities <- data.frame(
  city = c("Newark", "Trenton"),
  lon  = c(-74.1724, -74.7670),
  lat  = c(40.7357, 40.2171)
)

# Convert to an sf object
cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326)

# Make sure the projections match
cities_sf <- st_transform(cities_sf, st_crs(map1))

cities_coords <- st_coordinates(cities_sf)
cities_sf$X <- cities_coords[,1]
cities_sf$Y <- cities_coords[,2]

# add the cities to the map
p1 <- p +
  geom_point(data = cities_sf, aes(x = X, y = Y, shape = city), color = "grey2", size = 2) +
  geom_text(data = cities_sf,
            aes(x = X, y = Y, label = city),
            nudge_y = -0.07,
            fontface = "bold",
            size = 3) +
  scale_shape_manual(values = c("Newark" = 15, "Trenton" = 8)) +
  guides(shape = "none")
p1

# Save the figure
ggsave("figures/2025ballotcount.png", plot = p1, dpi = 300)


# Now move onto the margin map
# Begin to isolate the data we need from the gov2 dataframe
map2 <- gov2 |>
  select(year, pct_dem, pct_rep, GEOID, geometry)

# Pivot wider to have the counties as observations and vote percentages
# disaggregated by year
map2 <- map2 |>
  pivot_wider(
    names_from = year,
    values_from = c(pct_dem, pct_rep)
  )

# Calculate changes in pct rep and dem and then calculate vote margin
map2 <- map2 |>
  mutate(
    dem_change = pct_dem_2025 - pct_dem_2021,
    rep_change = pct_rep_2025 - pct_rep_2021,
    # Calculate shift towards democrats -- positive will indicate dem shift
    shift = round(100*(dem_change - rep_change),1)
  )

# Cull it to just the things we want to graph
map2 <- map2 |>
  select(geometry, shift)

# Build the plot: convert the dataframe to an sf object
# Calculate the county centers
map2 <- st_as_sf(map2)
county_centers <- st_centroid(map2)

# Define a scaling factor
scaling_factor <- 50

# Create a data frame of arrow segments
arrows_df <- county_centers |>
  mutate(
    X = st_coordinates(geometry)[,1],
    Y = st_coordinates(geometry)[,2],
    shift_height = shift / scaling_factor,  # scale to fit map
    Yend = Y + shift_height,
    arrow_color = ifelse(shift >= 0, "#2E75C9", "#CC5E73")
  )

# Plot with arrows
p <- ggplot() +
  # County outlines
  geom_sf(data = map2, fill = NA, color = "grey2", linewidth = 0.05) +
  # Add the arrows
  geom_segment(data = arrows_df,
               aes(x = X, y = Y, xend = X, yend = Yend, color = arrow_color),
               arrow = arrow(length = unit(0.15, "cm")),
               linewidth = 1.2) +
  scale_color_identity() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"))
p

# Remove the gridlines and add back on the cities
p1 <- p +
  geom_point(data = cities_sf, aes(x = X, y = Y, shape = city), color = "grey2", size = 2) +
  geom_text(data = cities_sf,
            aes(x = X, y = Y, label = city),
            nudge_y = -0.07,
            fontface = "bold",
            size = 3) +
  scale_shape_manual(values = c("Newark" = 15, "Trenton" = 8)) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  labs(title = "Margin Shift")
  
p1

# Save
ggsave("figures/2025marginshift.png", plot = p1, dpi = 300)