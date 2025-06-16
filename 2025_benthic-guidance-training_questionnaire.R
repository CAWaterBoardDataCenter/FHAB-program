## 2025 FHAB Benthic Guidance Trainings - Data Visualization for post-training questionnaire results
## Hannah Merges
## Created 6.16.25

########## Libraries ##########
library(tidyverse)
library(dplyr)
library(readr)
library(wordcloud2) 

########## Load Data ##########
questionnaire <- read_csv("Training-for-CCHAB-Benthic-Toxic-Algal-Mats-Guidance-Responses.csv")

########## Tidy Data ##########

questionnaire_codes <- questionnaire %>% 
  mutate(
    q1_code = case_when(
      q1 == "Neutral" ~ "N",
      q1 == "Somewhat comfortable" ~ "SC", 
      q1 == "Somewhat uncomfortable" ~ "SU", 
      q1 == "Very uncomfortable" ~ "VU",
      q1 == "Very comfortable" ~ "VC"), 
    q2_code = case_when(
      q2 == "Yes, this training was more helpful and I understand the material better" ~ "Yes", 
      q2 == "Neutral, I feel the same as I think I would have by reading through the SOP" ~ "Neutral")) 

###
questionnaire_countsQ1 <- questionnaire %>% 
  count(q1) %>% 
  mutate(grp = factor(q1, levels = c("Very comfortable","Somewhat comfortable","Neutral","Somewhat uncomfortable", "Very uncomfortable"))) # put the responses in an order I want for plotting

###
questionnaire_countsQ2 <- questionnaire_codes %>% 
  count(q2_code)

###
questionnaire_countsQ4 <- questionnaire %>% 
  count(q4)

### 
questionnaire_countsQ5 <- questionnaire %>%  
  select(q5) %>% 
  drop_na() %>%
  count(q5)


########## Plot ##########

##### Q1 

q1_results <- questionnaire_countsQ1 %>%  
  ggplot(aes(x=grp, 
             y=n)) +
  geom_col(fill="darkolivegreen3") + 
  theme_classic() + 
  labs(x= "How comfortable do you feel with the material (SOP) after taking this training?", 
       y= "Counts")
  

q1_results


##### Q2
q2_results <- questionnaire_countsQ2 %>%  
  ggplot(aes(x=q2_code, 
             y=n)) +
  geom_col(fill="darkolivegreen4") + 
  theme_classic() + 
  labs(x= "Do you feel like you are more comfortable having taken this training than by just reading the SOP?", 
       y= "Counts")


q2_results


##### Q3 







##### Q4 

q4_results <- questionnaire_countsQ4 %>%  
  ggplot(aes(x=q4, 
             y=n)) +
  geom_col(fill="darkolivegreen") + 
  theme_classic() + 
  labs(x= "Would you like follow-up from staff?", 
       y= "Counts")


q4_results




##### Q5 
# think it would be cool to try a word cloud maybe? 

# Basic plot

q5_results <- questionnaire_countsQ5 %>% 
  wordcloud2(size=1.6)

q5_results



