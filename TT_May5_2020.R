# Tidy Tuesday May 5, 2020

library(tidyverse)
library(lubridate)
library(stringr)
#library(tidyTuesdayR)




# Get the Data

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')



vill_v3 <- villagers %>%
  select(gender, species) %>%
  mutate(species = str_to_title(species)) %>%
  group_by(species, gender) %>%
  summarise(total = n()) %>%
  arrange(species) %>%
  pivot_wider(names_from = gender, values_from = total, values_fill = list(total = 0)) %>%
  mutate(PctFemale = female / (female + male), PctMale = male / (female + male)) %>%
  mutate(PctGreater = case_when(
                                PctFemale > PctMale ~ PctFemale,
                                PctMale >= PctFemale ~ PctMale
                                ))
                                

ggplot(data = vill_v3, aes(x = reorder(species, -PctMale), y = PctGreater, fill = PctMale)) +
  geom_col() +
  labs(x = "Species", y = "Gender Fraction", title = "Animal Crossing: New Horizons\nGendering of Animal Characters") +
  scale_fill_gradient(low = "#FDC0C7", high = "#86C8E9", name = "Gender\nFraction", breaks = c(0, 0.5, 1), labels = c("0.0" = "Female", "0.5" = "Even", "1.0" = "Male")) +
  scale_y_continuous(breaks = c(0.5, 1), labels = c("0.5" = "Even", "1.0" = "Single\nGender")) +
  theme(
    axis.title.x = element_text(face="bold", colour="#990000", size=14),
    axis.text.x  = element_text(angle=90, hjust = 1, vjust = .25, size=12),
    axis.title.y = element_text(face="bold", colour="#990000", size=14),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14))



