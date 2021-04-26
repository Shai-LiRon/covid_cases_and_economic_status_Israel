library(tidyverse)
library(dplyr)
library(janitor)
library(primer.data)
library(readxl)
library(lubridate)
library(ggthemes)
library(patchwork)
library(ggplot2)
library(hrbrthemes)
library(ggdist)
library(gt)


# Part 1 Economic model:

# First I must load the relevant data:

city_econ1 <- read_excel(path = "final_data/raw_data/econ_status_city.xlsx")
perc_vax_city1 <- read_excel(path = "final_data/raw_data/perc_city.xlsx")
clean_perc_vax_city1 <- perc_vax_city %>% 
  mutate_at(vars(-c("city")), as.numeric) 

# I must clean the data and mutate as numeric so I can use it.

vax_econ_clean1 <- city_econ1 %>% 
  clean_names() %>% 
  group_by(city) %>% 
  full_join(clean_perc_vax_city1, by = "city") %>% 
  mutate("rank2017" = as.numeric(cluster_2017_4))

# Joining the two data sets by city is my next step. Challenging when they don't
# all have the same city names format. 

econ_vax_rate_plot <- vax_econ_clean1 %>% 
  select(city, rank2017, city_rank, perc_second_dose, active_per_10000) %>% 
  ggplot(aes(x = rank2017, 
             y = perc_second_dose)) +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1), breaks = 0:10) +
  labs(title = "Relationship Between City's Socioeconomic Rank and Vaccination Percentage",
       subtitle = "Cities with a higher socio-economic status have a higher vaccination percentages",
       x = "City Socio-Economic Rank (2017)",
       y = "Percentage Vaccinated (Second Dose)",
       caption = "Source: Israel Health Ministry") +
  geom_smooth(method = "lm", 
              formula = y ~ x) +
  scale_color_gradient(low="blue", high="red") + 
  theme_classic()

# _____________________________________________________________________________
