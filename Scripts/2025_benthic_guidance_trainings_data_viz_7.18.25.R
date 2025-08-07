## 2025 FHAB Benthic Guidance Trainings - Data Visualization
## Elena Suglia
## Created 5.30.25

# Libraries ----
library(tidyverse)
library(dplyr)
library(ggrepel)
library(maps)
library(cowplot)

# Load data ----
registrants = read.csv("data/2025-05_benthic_guidance_training_registrant_data_7.18.25.csv")
participants = read.csv("data/2025-05_benthic_guidance_training_participant_data_7.18.25.csv")

participants_w_orgs = participants %>%
  left_join(registrants, by = "first_last_name")

# write.csv(participants_w_orgs, "data/2025-05_benthic_guidance_training_participants_w_orgs.csv")

# Create dataframe with orgs & county
#orgs = as.data.frame(unique(participants_w_orgs$org))
#write.csv(orgs, "data/org_loc.csv")
#populate manually

# read in dataframe with organization location info
org_loc_latlong = read.csv("data/org_loc_latlong.csv")

### Making a map showing participants by county & organization ###
# want to color counties in a gradient so that darker shades represent more participants for that county
# number of participants per organization
n_org = participants_w_orgs %>%
  group_by(org, org_category) %>%
  summarize(n_org = n())

n_org_cat = participants_w_orgs %>%
  group_by(org_category) %>%
  summarize(n_org_cat = n())

# TO DO: one instance of USFS is labeled as state govt; should be federal govt

n_cty = org_loc_latlong %>%
  group_by(county) %>%
  summarize(n_cty = n()) %>%
  #rename counties so they match ca_counties df (all lowercase)
  mutate(county = tolower(county))

# make a df with number of participants by county
all_dat = org_loc_latlong %>%
  select(org, county, lat, long) %>%
  left_join(n_cty) %>%
  left_join(n_org) %>%
  left_join(n_org_cat)

# load county data
usa_counties = map_data("county") 

# filter for CA counties
ca_counties = usa_counties %>%
  filter(region == "california") %>%
  # join n_cty dataframe
  left_join(n_cty, join_by(subregion == county)) %>%
  # replace NAs with 0s in n column
  mutate(n_cty = ifelse(is.na(n_cty), 0, n_cty))

# load map data on US States
#usa_states = map_data('state')

#library(RColorBrewer)

teal_gradient = c("white", "#d1eeea","#a8dbd9", "#85c4c9", "#68abb8", "#4f90a6", "#3b738f", "#2a5674")
library(ggrepel)

## TO DO: Add state column, fill out county column completely

#### Create the map with counties ####
ggplot(data = ca_counties, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = n_cty), color = "black",  linewidth = 0.2) + # County boundaries # #
  scale_fill_gradientn(colors = teal_gradient) +
  geom_point(data = all_dat %>%
               filter(state == "CA"),
               # remove counties outside CA for now
               aes(x = long, y = lat, group = org, size = n_org, shape = org_category), color = "orange") + #shape = 1, 
  geom_label_repel(data = all_dat %>%
                filter(county != "Sacramento") %>%
               # remove counties outside CA for now
               drop_na(county), aes(x = long, y = lat, group = org, label = org), nudge_x = 0.2, box.padding = 1.3, max.overlaps = Inf) + # make sure all points are labeled
  coord_fixed(1.3) +  # Adjust aspect ratio if needed
  theme_void() +
  labs(title = "Participants by Organization & County")

#### Only Sacramento county to use as a pop-out ####
ggplot(data = ca_counties %>% filter(subregion == "sacramento"), aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#2a5674", color = "black",  linewidth = 0.2) + # County boundaries # #
  scale_fill_gradientn(colors = teal_gradient) +
  geom_point(data = all_dat %>%
               filter(county == "Sacramento"),
               # remove counties outside CA for now
               aes(x = long, y = lat, group = org, size = n_org), shape = 7, color = "orange") +
  geom_label_repel(data = all_dat %>%
                     filter(county == "Sacramento") %>%
                     # remove counties outside CA for now
                     drop_na(county),
                   aes(x = long, y = lat, group = org, label = org), nudge_x = 0.2, box.padding = 1.3, max.overlaps = Inf) +
  coord_fixed(1.3) +  # Adjust aspect ratio if needed
  theme_void() +
  labs(title = "Sacramento County")

# If we wanted to replicate the above with regional WB boundaries, would have to import a shp file online using sf package and then map

library(stringr)
#### Create a bar graph depicting non-WB state agency representation ####
all_dat %>%
  filter(org_category == "State_Government") %>%
  filter(!str_detect(org, "Water Board")) %>%
  filter (state == "CA") %>%
  ggplot(aes(org, n_org)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

#### Create a bar graph showing numbers of participants from outside CA ####
all_dat %>%
  filter(state != "CA") %>%
  ggplot(aes(org, n_org)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

# Outdated code for plotting bar charts ----
# Add column with counts for each org
d_counts = d %>%
  group_by(org, org_category, attended) %>%
  mutate(count = n())

# Plot by org, registrants
d_counts %>%
  ggplot(aes(org, count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

# Plot by org, participants
d_counts %>%
  filter(attended == "Yes") %>%
  ggplot(aes(org, count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

# make a for loop to plot participant numbers by organization category
# State Government shown here:
d_counts %>%
  filter(attended == "Yes") %>%
  filter(org_category == "State_Government") %>%
  ggplot(aes(org, count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

d_counts %>%
  filter(attended == "Yes") %>%
  filter(org_category == "Tribe") %>%
  ggplot(aes(org, count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

# Add column with counts for each category
d_categories = d %>%
  group_by(org_category, attended) %>%
  summarize(count = n())

# Order the categories for plotting
d_categories$org_category = factor(d_categories$org_category, levels = c("State_Government", "Local_Government", "Tribe",  "Industry", "NGO", "Federal_Government", "Academia"))

# Plot by category, registrants
d_categories %>%
  ggplot(aes(org_category, count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

# Plot by category, participants
d_categories %>%
  filter(attended == "Yes") %>%
  ggplot(aes(org_category, count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

# Generate a list of organizations
orgs = unique(d_counts$org)
orgs
