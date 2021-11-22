# package loading
library(readr)
library(tidyverse)
library(visNetwork)
# library(lubridate)
# library(leaflet)

# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv") %>% 
  as_tibble()

solo2 <- train %>% select(`Departure station`) %>% unique() %>% 
  arrange(`Departure station`)

write.csv(solo2$`Departure station`,"solo2.csv",row.names = FALSE)

solo <- train %>%  
  group_by(`Departure station`,`Arrival station`) %>%  summarise(NbMois = n()) %>% 
  filter(NbMois < 66) %>% select(`Departure station`) %>% 
  unique()
