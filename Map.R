# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv")
coord <- read_delim("data/Coordonnees.csv", delim = ";", escape_double = FALSE)

train <- train %>% 
  mutate(`% trains late` = `Number of trains late on arrival`/
           (`Number of expected circulations`-`Number of cancelled trains`))

# liaison map
# edges: liaisons
e_train <- train %>% select(`Departure station`, `Arrival station`,
                            `Number of expected circulations`,`Number of cancelled trains`,`Number of trains late on arrival`) %>% 
  rename(Dep = `Departure station`, Arr = `Arrival station`) %>% 
  group_by(Dep, Arr) %>% 
  summarise(Nb = sum(`Number of expected circulations`-`Number of cancelled trains`, na.rm = TRUE), Nb_Delay = sum(`Number of trains late on arrival`,na.rm=TRUE))

e_train <- e_train %>% 
  filter(str_detect(Dep,"PARIS")|str_detect(Arr,"PARIS"))

e_trajet <- read_delim("data/e_trajet.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

e_train <- as.data.frame(c(e_train, e_trajet[,4:5]))

e_test <- e_train %>% 
  group_by(Trajet.A,Trajet.B) %>% summarise(Tot = sum(Nb), Delay = sum(Nb_Delay))

for (i in 1:nrow(e_test)){
  for (j in 1:nrow(y_train)){
    if (e_test$Trajet.B[i] == y_train$Station[j]){
      e_test$Direction[i] <- y_train$Direction[j] 
    }
  }
}

Tot <- e_test %>% group_by(Direction) %>% 
  summarise(Total = sum(Tot), Delay = sum(Delay))

Netdf <- cbind(rep("PARIS",4),Cardinaux$Direction)
Netdf <- as.data.frame(Netdf)

colnames(Netdf) <- c("PARIS", "DIRECTION")

Fleches <- read_delim("data/Fleches.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

Cardinaux <- cbind(Tot,Fleches)

Cardinaux$Percent <- (Cardinaux$Delay/Cardinaux$Total)
Paris <- c("PARIS",sum(Cardinaux$Total),sum(Cardinaux$Delay),"PARIS",	2.3522219, 	48.856614, (sum(Cardinaux$Delay)/sum(Cardinaux$Total)))
Paris <- t(as.data.frame(Paris))
colnames(Paris) <- colnames(Cardinaux)

Jdd <- rbind(Cardinaux, Paris)
rownames(Jdd) <- Jdd$Direction 
Jdd$Total <- as.numeric(Jdd$Total)
Jdd$Delay <- as.numeric(Jdd$Delay)
Jdd$Longitude <- as.numeric(Jdd$Longitude)
Jdd$Latitude <- as.numeric(Jdd$Latitude)
Jdd$Percent <- as.numeric(Jdd$Percent)

#############

NORD_EST <- network(Net[1,],directed = TRUE,loops = TRUE)
SUD_EST <- network(Net[3,])
NORD_OUEST <- network(Net[2,],directed = TRUE,loops = TRUE)
SUD_OUEST <- network(Net[4,],directed = TRUE,loops = TRUE)


SUD_EST %v% "lat" <- Jdd[network.vertex.names(SUD_EST),"Latitude"]
SUD_EST %v% "lon" <- Jdd[network.vertex.names(SUD_EST),"Longitude"]
SUD_EST %v% "Delay" <- Jdd[network.vertex.names(SUD_EST),"Percent"]
# add random groups
SUD_EST %v% "mygroup" <- Jdd[network.vertex.names(SUD_EST),"Direction"]
SUD_EST %v% "degree" <- degree(SUD_EST, gmode = "graph")


NORD_OUEST %v% "lat" <- Jdd[network.vertex.names(NORD_OUEST),"Latitude"]
NORD_OUEST %v% "lon" <- Jdd[network.vertex.names(NORD_OUEST),"Longitude"]
NORD_OUEST %v% "Delay" <- Jdd[network.vertex.names(NORD_OUEST),"Percent"]
NORD_OUEST %v% "mygroup" <- Jdd[network.vertex.names(sens),"Direction"]

SUD_OUEST %v% "lat" <- Jdd[network.vertex.names(SUD_OUEST),"Latitude"]
SUD_OUEST %v% "lon" <- Jdd[network.vertex.names(SUD_OUEST),"Longitude"]
SUD_OUEST %v% "Delay" <- Jdd[network.vertex.names(SUD_OUEST),"Percent"]
SUD_OUEST %v% "mygroup" <- Jdd[network.vertex.names(sens),"Direction"]

NORD_EST %v% "lat" <- Jdd[network.vertex.names(NORD_EST),"Latitude"]
NORD_EST %v% "lon" <- Jdd[network.vertex.names(NORD_EST),"Longitude"]
NORD_EST %v% "Delay" <- Jdd[network.vertex.names(NORD_EST),"Percent"]
NORD_EST %v% "mygroup" <- Jdd[network.vertex.names(sens),"Direction"]

# create a map of the Europe
europa <- ggplot(test, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +  
  coord_map(xlim = c(-5, 10.5),ylim = c(40, 51))

europa %>% ggnetworkmap(net = SUD_EST, size = c(1,10), great.circles = TRUE,
                        segment.color = "steelblue", arrow.size = 0,
                        node.group = mygroup, alpha = 0.5, segment.size = 1) %>% 
  ggnetworkmap(net = NORD_OUEST, size = 10, great.circles = TRUE,
               segment.color = "red", arrow.size = 0,
               node.group = mygroup, alpha = 0.5, segment.size = 1) %>% 
  ggnetworkmap(net = NORD_EST, size = 10, great.circles = TRUE,
               segment.color = "red", arrow.size = 0,
               node.group = mygroup, alpha = 0.5, segment.size = 1) %>% 
  ggnetworkmap(net = SUD_OUEST, size = 10, great.circles = TRUE,
               segment.color = "yellow", arrow.size = 0,
               node.group = mygroup, alpha = 0.5, segment.size = 1)

#######

Net <- network(Net,directed = TRUE,loops = TRUE)


Net %v% "lat" <- Jdd[network.vertex.names(Net),"Latitude"]
Net %v% "lon" <- Jdd[network.vertex.names(Net),"Longitude"]
Net %v% "Delay" <- Jdd[network.vertex.names(Net),"Percent"]
Net %v% "mygroup" <- Jdd[network.vertex.names(Net),"Direction"]
Net %v% "degree" <- degree(Net, gmode = "graph")

# create a map of the Europe
europa <- ggplot(test, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +  
  coord_map(xlim = c(-5, 10.5),ylim = c(40, 51))

europa %>% ggnetworkmap(net = Net, size = 5, great.circles = TRUE,
                        segment.color = "blue", node.group = mygroup,
                        arrow.size = 0,
                        alpha = 0.5, segment.size = 2,label.nodes = TRUE)

