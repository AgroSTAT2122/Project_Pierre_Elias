library(viridis)


# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv")
coord <- read_delim("data/Coordonnees.csv", delim = ";", escape_double = FALSE)

train <- train %>% 
  mutate(`% trains late` = `Number of trains late on arrival`/
           (`Number of expected circulations`-`Number of cancelled trains`))

# liaison map
# edges: liaisons

e_train <- train %>% select(`Departure station`, `Arrival station`,
                            `Number of expected circulations`,`Number of cancelled trains`,`Number of trains late on arrival`,`Number of late trains at departure`) %>% 
  rename(Dep = `Departure station`, Arr = `Arrival station`) %>% 
  group_by(Dep, Arr) %>% 
  summarise(Nb = sum(`Number of expected circulations`-`Number of cancelled trains`, na.rm = TRUE), Nb_Delay = sum(`Number of trains late on arrival`,na.rm=TRUE), Nb_Delay_dep = sum(`Number of late trains at departure`), na.rm = TRUE)

e_train <- e_train %>% 
  filter(str_detect(Dep,"PARIS")|str_detect(Arr,"PARIS"))

e_trajet <- read_delim("data/e_trajet.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

e_train <- as.data.frame(c(e_train, e_trajet[,4:5]))

e_test <- e_train %>% 
  group_by(Trajet.A,Trajet.B) %>% summarise(Tot = sum(Nb), Delay = sum(Nb_Delay), Delay_dep = sum(Nb_Delay_dep))

for (i in 1:nrow(e_test)){
  for (j in 1:nrow(y_train)){
    if (e_test$Trajet.B[i] == y_train$Station[j]){
      e_test$Direction[i] <- y_train$Direction[j] 
    }
  }
}

Tot <- e_test %>% group_by(Direction) %>% 
  summarise(Total = sum(Tot), Delay = sum(Delay),Delay_dep = sum(Delay_dep))


Fleches <- read_delim("data/Fleches.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

Cardinaux <- cbind(Tot,Fleches)

Cardinaux$Percent <- (Cardinaux$Delay/Cardinaux$Total)*100
Cardinaux$Percent_dep <- (Cardinaux$Delay_dep/Cardinaux$Total)*100
Paris <- c("PARIS",sum(Cardinaux$Total),sum(Cardinaux$Delay),sum(Cardinaux$Delay_dep),"PARIS",	2.3522219, 	48.856614, (sum(Cardinaux$Delay)/sum(Cardinaux$Total))*100, (sum(Cardinaux$Delay_dep)/sum(Cardinaux$Total))*100)
Paris <- t(as.data.frame(Paris))
colnames(Paris) <- colnames(Cardinaux)

Jdd <- rbind(Cardinaux, Paris)
rownames(Jdd) <- Jdd$Direction 
Jdd$Total <- as.numeric(Jdd$Total)
Jdd$Delay <- as.numeric(Jdd$Delay)
Jdd$Delay_dep <- as.numeric(Jdd$Delay_dep)
Jdd$Longitude <- as.numeric(Jdd$Longitude)
Jdd$Latitude <- as.numeric(Jdd$Latitude)
Jdd$Percent <- as.numeric(Jdd$Percent)
Jdd$Percent_dep <- as.numeric(Jdd$Percent_dep)

# create a map of the Europe
europa <- ggplot() +
  geom_polygon(aes(x = test$long, y = test$lat, group = test$group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +  
  coord_map(xlim = c(-5, 8),ylim = c(42, 51))

europa %>% ggnetworkmap(net = Net, size = 5, great.circles = TRUE,
                        node.group = mygroup,
                        arrow.size = 0, 
                        alpha = 0.5, segment.size = 2,label.nodes = TRUE)

arrow()

point <- geom_point(aes(x = Jdd$Longitude, y = Jdd$Latitude, color = Jdd$Direction), inherit.aes = FALSE, size = 8)
color_point <- c("#440154FF","#31688EFF","grey","#35B779FF","#FDE725FF")

segment <- geom_segment(aes(x = Jdd$Longitude[5], xend = Jdd$Longitude[-5], y = Jdd$Latitude[5], yend = Jdd$Latitude[-5]), size = 2,inherit.aes = FALSE, arrow = arrow()) 


europa +
  geom_segment(aes(x = Jdd$Longitude[5], xend = Jdd$Longitude[-5], y = Jdd$Latitude[5], yend = Jdd$Latitude[-5], color = Jdd$Percent[-5]), size = 2,inherit.aes = FALSE, arrow = arrow()) +
  labs(color = "Trains en retard à l'arrivée (en %)") +
  scale_color_gradient("Trains en retard à l'arrivée (en %)", high = "red", low = "yellow") 



europa + geom_point(aes(x = Jdd$Longitude, y = Jdd$Latitude, color = Jdd$Direction), inherit.aes = FALSE, size = c(8,8,8,8,5)) +
  scale_color_manual(values = color_point, guide = "none") +
  geom_segment(aes(x = Jdd$Longitude[5], xend = Jdd$Longitude[-5], y = Jdd$Latitude[5], yend = Jdd$Latitude[-5], color = Jdd$Percent), size = 2,inherit.aes = FALSE, arrow = arrow()) +
  theme(panel.background = element_blank(), #On enlève le fond
        axis.title = element_blank(), #On enlève le titre des axes
        axis.ticks = element_blank(), #On enlève les graduations
        axis.text = element_blank()) #On enlève le texte des axes
  

europa + geom_point(aes(x = Jdd$Longitude, y = Jdd$Latitude, fill = Jdd$Direction, size = Jdd$Total), inherit.aes = FALSE, shape = 21) + scale_size(range = c(7,14)) +
  scale_fill_manual(values = color_point, guide = "none") + labs(size = "Nombre Total de Trajets effectués") +
  geom_segment(aes(x = Jdd$Longitude[5], xend = Jdd$Longitude[-5], y = Jdd$Latitude[5], yend = Jdd$Latitude[-5], color = Jdd$Percent[-5]), size = 2,inherit.aes = FALSE, arrow = arrow()) +
  labs(color = "Trains en retard à l'arrivée (en %)") +
  scale_color_gradient("Trains en retard à l'arrivée (en %)", high = "darkblue", low = "cyan")  +
  theme(panel.background = element_blank(), #On enlève le fond
        axis.title = element_blank(), #On enlève le titre des axes
        axis.ticks = element_blank(), #On enlève les graduations
        axis.text = element_blank()) + #On enlève le texte des axes 
  geom_text(aes(x = Jdd$Longitude, y = Jdd$Latitude) ,label = rownames(Jdd), nudge_x = 0, nudge_y = c(0.65,0.65,-0.55,-0.45,0.65))



