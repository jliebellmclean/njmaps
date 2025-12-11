# Author: Julia Liebell-McLean
# Date: December 2 - 9
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




# 2020 Decennial Census Variables for PL 
decennial_2020_vars <- load_variables(
  year = 2020, 
  "pl", 
  cache = TRUE)# 2010 Decennial Census Variables

# Load the 2010 variables for PL
decennial_2010 <- load_variables(
  year = 2010,
  "pl",
  cache = TRUE)




# Load 2020 population for NJ counties, without geometry
census20 <- get_decennial(
  geography = "county",
  variables = "P1_001N",   # total population
  state = "NJ",
  year = 2020,
  geometry = FALSE
)

# Load 2010 population for NJ counties, without geometry
census10 <- get_decennial(
  geography = "county",
  variables = "P001001",
  state = "NJ",
  year = 2010,
  geometry = FALSE
)

census00 <- get_decennial(
  geography = "county",
  variables = "P001001",   # total population
  state = "NJ",
  year = 2000,
  geometry = FALSE
)





# Rename the population variable so it's more clear; drop the name since we have GEOID
# Do this for all three years of data
census00a <- census00 |>
  rename(population = value) |>
  mutate(year = 2000) |>
  select(population, year, GEOID)

census10a <- census10 |>
  rename(population = value) |>
  mutate(year = 2010) |>
  select(population, year, GEOID)

census20a <- census20 |>
  rename(population = value) |>
  mutate(year = 2020) |>
  select(population, year, GEOID)

# Bind all of the population data
pop_full <- rbind(census00a, census10a)

pop_full <- rbind(pop_full, census20a)

# Add population onto the county information, including area
full <- county |>
  left_join(pop_full, by = "GEOID")

# Now clean up that data, calculate population density (by square mile not square meter)
full1 <- full |>
  select(GEOID, NAME, year, ALAND, AWATER, population, geometry) |>
  mutate(area = ALAND + AWATER,
         density = population / ALAND * 2.59e+6)

# Identify the most populous and least populous counties in 2020
county_order <- full1 |>
  arrange(desc(population))
most_pop <- county_order[1,2]
least_pop <- county_order[21,2]

# Identify the most dense and least dense (population) in 2020
county_order <- full1 |>
  arrange(desc(density))
most_dense <- county_order[1,2]
least_dense <- county_order[21,2]


# Draw a map of the population
# Specify the max population
vmax <- max(full1$population, na.rm = TRUE)
vmin <- min(full1$population, na.rm = TRUE)

#Build plot for population with scale transform, breaks, and legend simplification
p  <- ggplot(full1, aes(fill = population)); p
p1 <- p + geom_sf(color = "white", linewidth = 0.05) + coord_sf(datum = NA); p1
p2 <- p1 +
  scale_fill_gradient(
    limits = c(vmin, vmax), 
    oob    = squish,  
    labels = label_number(scale_cut = cut_short_scale()),
    low    = "lightblue",
    high   = "darkblue"
  ) +
  facet_wrap( ~ year, ncol = 3) +
  labs(title = "Population by County, 2000-2020", fill = NULL); p2
p3 <- p2 +
  theme_map() +
  guides(fill = guide_colorbar(barheight = unit(60, "pt"))) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.background = element_blank(),                # remove gray bar
    strip.text       = element_text(color = "black",
                                    size = 12),   # plain black text
    plot.title       = element_text(size = 16, face = "bold")
    )
p3

ggsave("figures/population.png", plot = p3, width = 8.5, height = 6, units = "in", dpi = 300)


# Now do the same for population density
# Specify the max population density
vmin <- min(full1$density)
vmax <- round(max(full1$density, na.rm = TRUE))

# First, bin the population density, make sure to include the highest value
breaks <- c(seq(0, vmax, by = 300), vmax + 1)

# Generate labels like "0–299", "300–599", etc.
labels <- paste0(
  head(breaks, -1), "-", tail(breaks, -1) - 1
)

# Apply cut() with labels, then drop unused levels
full1 <- full1 %>%
  mutate(
    density_bin = cut(
      density,
      breaks = breaks,
      include.lowest = TRUE,
      right = TRUE,
      labels = labels
    ) %>% droplevels()
  )

# Check the number of bins
nlevels(full1$density_bin)

# Check the number of bins in use
# These should now be the same after applying the droplevels() command above
length(unique(full1$density_bin))

# Define a 19 color scale
my_colors <- c("#FFF9FA", "#FFEBEF", "#FCD9DD",
               "#F9C7CB", "#F6B5B9", "#F09195", 
               "#ED7F83", "#E75B5F", "#E4494D",
               "#E1373B", "#DE2529", "#DB1317", 
               "#D80005", "#C40005", "#B00005", 
               "#9C0005", "#880005", "#4B0000",
               "black")


#Build plot for population with scale transform, breaks, and legend simplification
p  <- ggplot(full1, aes(fill = density_bin)); p
p1 <- p + geom_sf(color = "black", linewidth = 0.05) + coord_sf(datum = NA); p1
p2 <- p1 +
  scale_fill_manual(values = my_colors) +
  facet_wrap( ~ year, ncol = 3) +
  labs(title = "Population Density by County, 2000-2020", fill = NULL); p2
p3 <- p2 +
  theme_map() +
  guides(fill = guide_legend(ncol = 1)) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.background = element_blank(),                # remove gray bar
    strip.text       = element_text(color = "black",
                                    size = 12),   # plain black text
    plot.title       = element_text(size = 16, face = "bold"),
    legend.position  = "right",
    legend.justification = "center",
    legend.box.margin = margin(0, 20, 0, 0)           # add space from plot
  )
p3

# Save the figure
ggsave("figures/density.png", plot = p3, width = 8.5, height = 6, units = "in", dpi = 300)