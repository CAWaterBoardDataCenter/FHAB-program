## 2025 FHAB Benthic Guidance Trainings - Data Visualization
## Elena Suglia
## Created 5.30.25

# Libraries ----
library(tidyverse)
library(dplyr)

# Load data ----
participants = read.csv("2025-05_benthic_guidance_training_participant_data.csv")
# cleaned up registrant list manually in excel
d = read.csv("2025-05_benthic_guidance_training_registrant_data.csv") %>%
  left_join(participants) %>%
  mutate(attended = ifelse(is.na(guest) == TRUE, "No", "Yes"))

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
