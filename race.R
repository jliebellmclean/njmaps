# Author: Julia Liebell-McLean
# Date: December 2 - 9
# Title: Race and Ethnicity maps
# Notes: Use census data to graph racial and ethnic trends in NJ over time

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
  cache = TRUE)

# Load the 2000 variables
decennial_2000 <- load_variables(
  year = 2000,
  cache = TRUE)


# Start with the 2000 census
# Pull information about racial identity, including Hispanic/Latino, table P004
race_data <- get_decennial(
  geography = "county",
  variables = c("P004001", "P004002", "P004003", "P004005",
                "P004006", "P004007", "P004008", "P004009", "P004010",
                "P004011"),
  state = "NJ",
  year = 2000,
  geometry = FALSE)

# Pivot wider so each racial/ethnic identity has its own column
race_data_w <- race_data |>
  pivot_wider(
    names_from = variable,
    values_from = value)

# Rename the columns so that they are easy to understand
# Add a variable to signify year
race_data_w <- race_data_w |>
  mutate(year = 2000) |>
  rename("total" = "P004001",
         "hisp.latino" = "P004002", 
         "not.hisp" = "P004003", 
         "white" = "P004005",
         "black" = "P004006", 
         "am.indian" = "P004007", 
         "asian" = "P004008", 
         "hawaii.pac.isl" = "P004009", 
         "other" = "P004010",
         "two.more" = "P004011")

# Let's just do a quick check to make sure things didn't get lost as I renamed 
# everything
# race_data_check <- race_data_w |>
  # mutate(sum = hisp.latino + white + black + am.indian + asian + hawaii.pac.isl + other + two.more,
         # check = total - sum)
# Seems okay


# Okay, do the same thing for 2010
# Pull information about racial identity, including Hispanic/Latino, table P005
race_data10 <- get_decennial(
  geography = "county",
  variables = c("P005001", "P005010", "P005002", "P005003",
                "P005004", "P005005", "P005006", "P005007", "P005008",
                "P005009"),
  state = "NJ",
  year = 2010,
  geometry = FALSE)

# Pivot wider so each racial/ethnic identity has its own column
race_data10_w <- race_data10 |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# Rename the columns so that they are easy to understand
# Add a variable to signify year
race_data10_w <- race_data10_w |>
  mutate(year = 2010) |>
  rename("total" = "P005001",
         "hisp.latino" = "P005010", 
         "not.hisp" = "P005002", 
         "white" = "P005003",
         "black" = "P005004", 
         "am.indian" = "P005005", 
         "asian" = "P005006", 
         "hawaii.pac.isl" = "P005007", 
         "other" = "P005008",
         "two.more" = "P005009")

# Check:
# race_data_check <- race_data10_w |>
  # mutate(sum = hisp.latino + white + black + am.indian + asian + hawaii.pac.isl + other + two.more,
  # check = total - sum)
# All good




# Onto the 2020 census
# Pull information about racial identity, including Hispanic/Latino, table P005
race_data20 <- get_decennial(
  geography = "county",
  variables = c("P2_001N", "P2_002N", "P2_003N", "P2_005N",
                "P2_006N", "P2_007N", "P2_008N", "P2_009N", "P2_010N",
                "P2_011N"),
  state = "NJ",
  year = 2020,
  geometry = FALSE
)

# Pivot wider so each racial/ethnic identity has its own column
race_data20_w <- race_data20 |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# Rename the columns so that they are easy to understand
# Add a variable to signify year
race_data20_w <- race_data20_w |>
  mutate(year = 2020) |>
  rename("total" = "P2_001N",
         "hisp.latino" = "P2_002N", 
         "not.hisp" = "P2_003N", 
         "white" = "P2_005N",
         "black" = "P2_006N", 
         "am.indian" = "P2_007N", 
         "asian" = "P2_008N", 
         "hawaii.pac.isl" = "P2_009N", 
         "other" = "P2_010N",
         "two.more" = "P2_011N")

# Check:
# race_data_check <- race_data20_w |>
# mutate(sum = hisp.latino + white + black + am.indian + asian + hawaii.pac.isl + other + two.more,
# check = total - sum)
# All good


# Now, we can bind the three data frames together to get racial data across three census years
race_full <- rbind(race_data_w, race_data10_w)
race_full <- rbind(race_full, race_data20_w)

# Now do some more manipulation to get at things like percentages
race_full1 <- race_full |>
  # Create a variable that is just non-white
  mutate(nonwhite = black + am.indian + asian + hawaii.pac.isl + other + two.more) |>
  # Calculate percentages
  mutate(pct_nonwhite = 100 * nonwhite / total,
         pct_white = 100 * white / total,
         pct_hisp = 100 * hisp.latino / total, 
         pct_black = 100 * black / total, 
         pct_am.indian = 100 * am.indian / total, 
         pct_asian = 100 * asian / total, 
         pct_hawaii.pac.isl = 100 * hawaii.pac.isl / total, 
         pct_other = 100 * other / total,
         pct_two.more = 100 * two.more / total)

# Based on this, I sorted the various percentages and picked the racial/ethnic
# categories that looked most interesting to me

# Trim the dataset down to a more manageable set of variables to graph
race_full2 <- race_full1 |>
  select(GEOID, NAME, year, 
         pct_asian, pct_nonwhite, pct_white, pct_hisp, pct_black)

# Add on geometry from the county file
geo <- county |>
  select(GEOID, geometry)
race_full2 <- race_full2 |>
  left_join(geo, by = "GEOID") |>
  st_as_sf() # Make sure it remains an sf object so geometry can be recognized

# Round the percentages for the census data variables
race_full2 <- race_full2 |>
  mutate(across(c(pct_asian, pct_nonwhite, pct_white, pct_hisp, pct_black),
                round,
                digits = 1))


# Rename the columns so that they can be used to title figures
race_full2 <- race_full2 |>
  rename("Percent_White" = "pct_white",
         "Percent_Nonwhite" = "pct_nonwhite",
         "Percent_Black" = "pct_black",
         "Percent_Hispanic_Latino" = "pct_hisp",
         "Percent_Asian" = "pct_asian")

# Specify a group for the function to loop through
groups <- c("Percent_White", "Percent_Nonwhite", "Percent_Black",
            "Percent_Hispanic_Latino", "Percent_Asian")

# Use a function to loop through the five demographic groups and graph their 
# percentages on a scale from 0-100% by county
for (gp in groups) {
  p  <- ggplot(race_full2, aes_string(fill = gp));
  p1 <- p + geom_sf(color = "white", linewidth = 0.05) + coord_sf(datum = NA);
  p2 <- p1 +
    scale_fill_gradient(
      limits = c(0, 100), 
      oob    = squish,  
      labels = label_number(scale_cut = cut_short_scale()),
      low    = "beige",
      high   = "maroon4"
    ) +
    facet_wrap( ~ year, ncol = 3) +
    labs(title = paste0(gp, ", 2000-2020"), fill = NULL); p2
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
  ggsave(filename = paste0("figures/", gp, "-100scale.png"),
         plot = p3,
         width = 8.5, height = 6, units = "in", dpi = 300)
}


# Now, create a loop for a figure that does not use a 0-100 scale
for (gp in groups) {
  p  <- ggplot(race_full2, aes_string(fill = gp)); p
  p1 <- p + geom_sf(color = "white", linewidth = 0.05) + coord_sf(datum = NA); p1
  p2 <- p1 +
    scale_fill_gradient(
      oob    = squish,  
      labels = label_number(scale_cut = cut_short_scale()),
      low    = "beige",
      high   = "maroon4"
    ) +
    facet_wrap( ~ year, ncol = 3) +
    labs(title = paste0(gp, ", 2000-2020"), fill = NULL); p2
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
    ))
  ggsave(filename = paste0("figures/", gp, "range-scale.png"),
         plot = p3,
         width = 8.5, height = 6, units = "in", dpi = 300)
}


# Calculate what is the dominant racial/ethnic group for each county in a given year
# Note: I used Copilot to develop this formula
race_full3 <- race_full2 %>%
  rowwise() %>%
  mutate(
    # store the vector of group names
    groups = list(c("White", "Black", "Asian", "Hispanic/Latino")),
    # store the vector of percentages
    values = list(c_across(c(Percent_White, Percent_Black, Percent_Asian, Percent_Hispanic_Latino))),
    # find index of max
    idx = which.max(values),
    # combine group name and percentage into one string
    dominant_group = paste0(groups[idx], " (", values[idx], "%)")) %>%
  mutate(
    label = dominant_group
  ) %>%
  separate(dominant_group, into = c("Category", "Percent"), sep = "\\(") %>%
  mutate(
    Percent = gsub("\\)", "", Percent),   # remove closing parenthesis
    Percent = gsub("%", "", Percent),     # remove percent sign
    Percent = as.numeric(Percent)         # convert to numeric
  )


# Now, graph the most common racial/ethnic group by county in a given year
# Specify max and min, as a decimal
vmin <- min(race_full3$Percent) / 100
vmax <- max(race_full3$Percent) / 100

# Build the graph
p <- ggplot(race_full3) +
  geom_sf(aes(fill = Category, alpha = Percent), color = "white", linewidth = 0.05) +
  coord_sf(datum = NA) +
  scale_fill_brewer(palette = "Set1", name = "Group") + # Color the counties by dominant group using discrete color scale
  scale_alpha(range = c(vmin, vmax), name = "Dominant %") + # Shade the counties based on the percentage
  facet_wrap( ~ year, ncol = 3) + # Include all three years available in a single graph
  labs(
    title = "Dominant Racial/Ethnic Group, 2000-2020", fill = NULL,
    caption = "Note: Counties are colored by dominant racial/ethnic group. Shading intensity reflects the percentage of that group."
  )
p1 <- p +
  geom_sf_text(
    aes(label = paste0(round(Percent, 0), "%")),   # label with rounded percent
    size = 2, color = "black"
  ) +
    theme_map() +
  guides(
      alpha = "none", # Remove the legend for % shading - it's not very helpful
      fill = guide_legend(nrow = NULL)) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.background = element_blank(),
    strip.text       = element_text(color = "black",
                                    size = 12),   # plain black text
    plot.title       = element_text(size = 16, face = "bold"),
    legend.position  = "right",
    legend.justification = "center",
    legend.box.margin = margin(0, 20, 0, 0) 
  )
p1

# Save the figure
ggsave(filename = "figures/shadedrace.png",
       plot = p1,
       width = 8.5, height = 5.5, units = "in", dpi = 300)