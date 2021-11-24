library(maps)
library(geosphere)
library(mapproj)
library(sp)
library(sna)
library(ggplot2)
library(GGally)

# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv")
coord <- read_delim("data/Coordonnees.csv", delim = ";", escape_double = FALSE)

train <- train %>% 
  mutate(`% trains late` = `Number of trains late on arrival`/
           (`Number of expected circulations`-`Number of cancelled trains`))

# liaison map
# edges: liaisons
e_train <- train %>% select(`Departure station`, `Arrival station`,
                            `Number of expected circulations`) %>% 
  rename(Dep = `Departure station`, Arr = `Arrival station`) %>% 
  group_by(Dep, Arr) %>% 
  summarise(Nb = sum(`Number of expected circulations`, na.rm = TRUE))

# nodes: stations
y_train <- as.data.frame(coord) %>% filter(`Departure station` %in% e_train$Dep |
                              `Departure station` %in% e_train$Arr) %>% 
  rename(Station = `Departure station`)

test <- map_data("world") %>% 
  filter(region %in% c("France", "Belgium", "Luxembourg", "Germany", "Spain", "Portugal", "Switzerland", "Italy"))

e_train <- e_train %>% 
  filter(str_detect(Dep,"PARIS")|str_detect(Arr,"PARIS"))

# convert to network
sens <- network(e_train, directed = TRUE,loops = TRUE)

rownames(y_train) <- y_train$Station

# add geographic coordinates
sens %v% "lat" <- y_train[network.vertex.names(sens),"Latitude"]
sens %v% "lon" <- y_train[network.vertex.names(sens),"Longitude"]

# drop isolated airports
delete.vertices(sens, which(degree(sens) < 2))

# compute degree centrality
sens %v% "degree" <- degree(sens, gmode = "graph")

# add random groups
sens %v% "mygroup" <- y_train[network.vertex.names(sens),"Direction"]


# carte des gares
europa <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name %in% c("France", "Belgium", "Luxembourg", "Germany", "Spain", "Portugal", "Switzerland", "Italy"))

# france <- ne_countries(scale = "medium", returnclass = "sf") %>%
#   filter(name == "France")

france_metro <- europa %>% 
  st_crop(xmin = -9.86, xmax = 10.38, ymin = 39, ymax = 51.56)

ggp <- france_metro %>% 
  st_transform(crs = 4326) %>%
  ggplot() + geom_sf() +
  # geom_point(data = coord, aes(x = Longitude, y = Latitude), shape = 20) +
  # geom_point(data = g, aes(lon, lat)) +
  # geom_edges(data = g, aes(lon, lat, xend = xend, yend = yend)) +
  # geom_nodelabel(data = g, aes(lon, lat, label = vertex.names))
  # coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
  theme_minimal() ; ggp

# create a map of the USA
europa <- ggplot(test, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +  
  coord_map(xlim = c(-5, 10.5),ylim = c(40, 51)); europa

# delete.vertices(sens, which(sens %v% "lon" < min(ggp$data$long)))
# delete.vertices(sens, which(sens %v% "lon" > max(usa$data$long)))
# delete.vertices(sens, which(sens %v% "lat" < min(usa$data$lat)))
# delete.vertices(sens, which(sens %v% "lat" > max(usa$data$lat)))

# overlay network data to map
ggnetworkmap(europa, net = sens, size = 10, great.circles = TRUE,
             segment.color = "steelblue", arrow.size = 0,
             node.group = mygroup, alpha = 1,
             ring.group = degree, weight = degree)

