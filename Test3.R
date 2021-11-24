library(maps)
library(geosphere)
library(mapproj)
library(sp)
library(sna)
library(ggplot2)
library(GGally)
library(readr)
library(tidyverse)

 
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

write.table(e_train,"e_train.csv", sep = ";",row.names = FALSE)

# convert to network
sens <- network(e_train, directed = TRUE,loops = TRUE)

rownames(y_train) <- y_train$Station

# add geographic coordinates
sens %v% "lat" <- y_train[network.vertex.names(sens),"Latitude"]
sens %v% "lon" <- y_train[network.vertex.names(sens),"Longitude"]

# compute degree centrality
sens %v% "degree" <- degree(sens, gmode = "graph")

# add random groups
sens %v% "mygroup" <- y_train[network.vertex.names(sens),"Direction"]

# create a map of the USA
europa <- ggplot(test, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +  
  coord_map(xlim = c(-5, 10.5),ylim = c(40, 51))

# overlay network data to map



europa %>% ggnetworkmap(net = sens, size = 10, great.circles = TRUE,
                        segment.color = "steelblue", arrow.size = 0,
                        node.group = mygroup, alpha = 0.5, segment.size = 1,
                        ring.group = degree, weight = degree, legend)

ggnetworkmap(europa, net = sens, size = 10, great.circles = TRUE,
             segment.color = "steelblue", arrow.size = 0,
             node.group = mygroup, alpha = 0.5, segment.size = 1,
             ring.group = Nb, weight = Nb, arrow.size = 1)
