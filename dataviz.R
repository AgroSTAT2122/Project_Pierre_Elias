# package loading
library(readr)
library(tidyverse)
library(visNetwork)
# library(lubridate)
# library(leaflet)

# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv") %>% 
  as_tibble()

# add date field
train <- train %>% 
  arrange(Year, Month) %>% 
  mutate(MonthYr = paste(Year, Month, sep = "-"))

# add GPS coord. field
stations <- read.table("data/liste-gares.csv", sep = ";", dec = ".", encoding = "utf-8")


# Nombre de mois sur la période (jan. 2015 - jun. 2020)
train %>% 
  select(Year, Month) %>% 
  mutate(MonthYr = paste(Year, Month)) %>% 
  select(MonthYr) %>% 
  unique() %>% 
  nrow()

# Nombre de liaisons sur la période
train %>%
  mutate(Liaison = paste(`Departure station`, `Arrival station`, sep = " - ")) %>% 
  group_by(Liaison) %>% 
  summarise(Nb = n()) %>% 
  filter(Nb < 66)
 


