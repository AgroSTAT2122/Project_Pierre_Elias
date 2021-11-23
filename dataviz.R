# package loading
library(readr)
library(tidyverse)
library(ggnet)C
library(sf)
library(rnaturalearth)
library(visNetwork)
library(lubridate)
library(leaflet)
library(plotly)

# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv") %>% 
  as_tibble()
coord <- read_delim("data/Coordonnees.csv", delim = ";", escape_double = FALSE,
                    trim_ws = TRUE) %>% as_tibble()

# add % of late trains (on arrival)
train <- train %>% 
  mutate(`% trains late` = `Number of trains late on arrival`/
           (`Number of expected circulations`-`Number of cancelled trains`))


# carte des gares
europa <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name %in% c("France", "Belgium", "Luxembourg", "Germany", "Spain", "Portugal", "Switzerland", "Italy"))

# france <- ne_countries(scale = "medium", returnclass = "sf") %>%
#   filter(name == "France")

france_metro <- europa %>% 
  st_crop(xmin = -9.86, xmax = 10.38, ymin = 39, ymax = 51.56)

france_metro %>% 
  st_transform(crs = 4326) %>%
  ggplot() + geom_sf() +
  geom_point(data = coord, aes(x = Longitude, y = Latitude), 
             shape = 20) +
  # coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
  theme_minimal()

# coord %>% filter(`Departure station` %in% train$`Departure station`) %>% 
#   nrow()

## add date and liaison fields
train <- train %>% 
  mutate(Liaison = paste(`Departure station`, `Arrival station`, sep = " - "),
         Month = as.factor(Month),
         Year = as.factor(Year))

# # même carte avec Leaflet
# tile_thunderforest_pionner <- "https://tile.thunderforest.com/pioneer/{z}/{x}/{y}.png?apikey=5e3ed9d6d59c4266bac89b7b809b4aaf"
# icon_station <- icons(iconUrl = "data/train.png", iconWidth = 10)
# 
# leaflet(data = coord) %>% 
#   setView(lng = 3, lat = 46, zoom = 5) %>% 
#   addTiles(urlTemplate = tile_thunderforest_pionner) %>% 
#   addMarkers(icon = icon_station,lng = ~Longitude, lat = ~Latitude)


# Nombre de mois sur la période (jan. 2015 - jun. 2020)
train %>% 
  select(Year, Month) %>% 
  mutate(MonthYr = paste(Month, Year)) %>% 
  select(MonthYr) %>% 
  unique() %>% nrow()

# Nombre de gares (LGV)
train %>% 
  select(`Departure station`) %>% 
  unique() %>% nrow()

# Nombre de liaisons par mois
train %>% 
  group_by(Year, Month) %>% 
  summarise(Nb = n())

# Liaisons < 66 sur la période (n'apparaissent pas tous les mois)
liaisons_occasional <- train %>%
  mutate(Liaison = paste(`Departure station`, `Arrival station`, sep = " - ")) %>% 
  group_by(Liaison) %>% 
  summarise(Nb_mois = n()) %>% 
  filter(Nb_mois < 66) %>% 
  select(Liaison) %>% pull()


month.name.fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
                   "Août", "Septembre", "Octobre", "Novembre", "Décembre")
month.shortname.fr <- c("Jan.", "Fév.", "Mars", "Avr.", "Mai", "Juin", "Jui.",
                        "Août", "Sep.", "Oct.", "Nov.", "Déc.")


# Évolution mensuelle des retards
train %>% 
  # conservations des liaisons fréquentes (au moins une par mois)
  filter(!Liaison %in% liaisons_occasional) %>% 
  group_by(Month) %>% 
  summarise(Retards = mean(`% trains late`, na.rm = TRUE)) %>%
  # diagramme en barres
  ggplot() + aes(x = Month, y = Retards*100) +
  geom_bar(stat = "identity", width = 0.5, fill = "#0088CE", alpha = 1) +
  scale_x_discrete(labels = month.name.fr) +
  ylab(element_blank()) + xlab(element_blank()) +
  labs(title = "Trains en retards à la gare d'arrivée (en %)",
       caption = "Période : janvier 2015 à juin 2020") +
  theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        axis.text.x = element_text(angle = 40, vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())


# Répartition mensuelle des retards
train %>% 
  # conservations des liaisons fréquentes (au moins une par mois)
  filter(!Liaison %in% liaisons_occasional) %>% 
  group_by(Month, Liaison) %>% 
  summarise(Delay = mean(`Average delay of late arriving trains (min)`, na.rm = TRUE)) %>% 
  # boxplot par mois
  ggplot() + aes(x = Month, y = Delay) +
  geom_boxplot(aes(group = Month), outlier.shape = 19, color = "#0088CE") +
  scale_x_discrete(labels = month.name.fr) +
  ylab("Délai (en minutes)") + xlab(element_blank()) +
  labs(title = "Répartition du délai moyen des trains en retard à la gare d'arrivée",
       caption = "Période : janvier 2015 à juin 2020") +
  theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        axis.text.x = element_text(angle = 40, vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = 'grey', linetype = 'dotted'),
        axis.ticks.x = element_blank())


# Évolution mensuelle des annulations
train %>% 
  # conservations des liaisons fréquentes (au moins une par mois)
  filter(!Liaison %in% liaisons_occasional) %>% 
  group_by(Month) %>% 
  summarise(Cancel = mean(`Number of cancelled trains`, na.rm = TRUE)) %>%
  # diagramme en barres
  ggplot() + aes(x = Month, y = Cancel) +
  geom_bar(stat = "identity", width = 0.5, fill = "#0088CE", alpha = 1) +
  scale_x_discrete(labels = month.name.fr) +
  ylab(element_blank()) + xlab(element_blank()) +
  labs(title = "Nombre moyen de trains annulés",
       caption = "Période : janvier 2015 à juin 2020") +
  theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        axis.text.x = element_text(angle = 40, vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())



