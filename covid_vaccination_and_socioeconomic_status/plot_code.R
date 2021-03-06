library(tidyverse)
library(janitor)
library(primer.data)
library(readxl)
library(lubridate)
library(ggthemes)
library(patchwork)
library(ggplot2)
library(hrbrthemes)
library(ggdist)


# Part 1 Economic model:

# First I must load the relevant data:

city_econ1 <- read_excel(path = "final_data/raw_data/econ_status_city.xlsx")
perc_vax_city1 <- read_excel(path = "final_data/raw_data/perc_city.xlsx")
clean_perc_vax_city1 <- perc_vax_city1 %>% 
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

# this plots the relationship between economic rank and vaccination percentages.

relationship_plot <- perc_vax_city1 %>% 
  mutate_at(vars(-c("city")), as.numeric) %>% 
  arrange(perc_first_dose) %>% 
  select(city, perc_second_dose, active_per_10000) %>% 
  ggplot(aes(x = perc_second_dose, 
             y = active_per_10000)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ x) +
  labs(title = "Active Cases per 10,000 People and Vaccination Percentages",
       subtitle = "A negative relationship exists between active cases and vaccination percentage",
       x = "Percent Vaccinated Second Dose in Given City",
       y = "Number of Active Cases per 10,000 People in Given City",
       caption = "Source: Israel Health Ministry") 

# _____________________________________________________________________________

total_case_vax <- read_csv("final_data/total_case_vax.csv") 

# this plots the daily vaccination, of first and second dose. 

vaccine_plot <- total_case_vax %>% 
  filter(date > as.Date("2020-12-01")) %>% 
  pivot_longer(cols = c(first_dose, second_dose), 
               names_to = "type", values_to = "value") %>% 
  ggplot(aes(x = date,
             y = value,
             color = type)) + 
  geom_line(size=0.8) +
  scale_y_continuous(labels = scales::number_format(accuracy = 100)) +
  labs(title = "Number of Israelis Vaccinated: First and Second Dose",
       subtitle = "Number of daily vaccinations increased less rapidly March",
       x = "Date",
       y = "Number of Vaccinatated",
       caption = "Source: Israel Ministry of Health") +
  geom_label(x=as.Date("2020-12-19"), y = -100, 
             label = "First Vaccines", color = "black", size = 2) +
  geom_label(x=as.Date("2021-03-19"), y = 4526000, 
             label = "50% of Population Vaccinated", color = "black", size = 2) +
  scale_color_discrete(name = "Vaccine Dose", labels = c("First Dose", "Second Dose")) 

# _____________________________________________________________________________

# this plots the daily number of new cases over time.

new_cases <- total_case_vax %>% 
  ggplot(aes(x = date,
             y = new_cases)) + 
  geom_line(size=0.8, color=rgb(0.1, 0.6, 0.9, 1)) +
  labs(title = "Daily Number of New Covid Cases in Israel",
       subtitle = "Number of covid cases decreased when 50% of population was vaccinated",
       x = "Date",
       y = "Daily Number of New Covid Cases",
       caption = "Source: Israel Ministry of Health") +
  geom_label(x=as.Date("2020-12-19"), y = 5000, 
             label = "Vaccination Starts", color = "black", size = 2) +
  geom_label(x=as.Date("2021-03-19"), y = -100, 
             label = "50% Vaccinated", color = "black", size = 2) +
  geom_label(x=as.Date("2021-02-09"), y = 6000, 
             label = "25% Vaccinated", color = "black", size = 2)

