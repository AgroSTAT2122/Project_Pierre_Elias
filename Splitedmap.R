dpt_shape <-  st_read(dsn = 'depts')

dpt_shape %>%
  select(CODE_DEPT, NOM_DEPT, NOM_REGION) %>%
  group_by(NOM_REGION) %>%
  summarise(geometry= st_union(geometry)) %>% st_as_sf() ->
  region

region_dir <- read_delim("region.csv", delim = ";", 
                                   escape_double = FALSE, trim_ws = TRUE)
colnames(region_dir) <- c("NOM_REGION","Localisation")

region_uni <- cbind(region,region_dir)
region_uni <- region_uni[,-2]

region_uni <- region_uni[-9,] #Retrait de la corse

region_uni %>% 
  group_by(Localisation) %>% 
  summarise(geometry = st_union(geometry)) %>% st_as_sf() -> 
  Delimitation

try <- Delimitation

try$geometry <- (try$geometry / 1000000)

carte <- ggplot() + geom_sf(aes(fill = try$Localisation, geometry = try$geometry)) + scale_fill_manual(values = color_point, guide = "none")

testtt <- Jdd

testtt$Latitude <- c(6.8,6.8,6.4,6.4,6.85)
testtt$Longitude <- c(0.9,0.5,0.9,0.5,0.65)


carte + 
  geom_segment(aes(x = testtt$Longitude[5], xend = testtt$Longitude[-5], y = testtt$Latitude[5], yend = testtt$Latitude[-5], size = testtt$Total[-5], color = testtt$Percent_dep[-5]), lineend = "round", inherit.aes = FALSE, arrow = arrow(angle = 50)) +
  scale_size(range = c(1,5), guide = "none")+
  labs(color = "Trains en retard au départ (en %)") +
  scale_color_gradient("Trains en retard au départ (en %)", high = "darkblue", low = "cyan")  +
  theme(panel.background = element_blank(), #On enlève le fond
        axis.title = element_blank(), #On enlève le titre des axes
        axis.ticks = element_blank(), #On enlève les graduations
        axis.text = element_blank()) +#On enlève le texte des axes 
  geom_text(aes(x = testtt$Longitude, y = testtt$Latitude) ,label = rownames(testtt), nudge_x = 0, nudge_y = c(-0.05,-0.05,-0.03,-0.03,0.02), fontface = 2, color = c("white","white","black","black","black"))



carte + 
  geom_segment(aes(x = testtt$Longitude[5], xend = testtt$Longitude[-5], y = testtt$Latitude[5], yend = testtt$Latitude[-5], size = testtt$Total[-5], color = testtt$Percent[-5]), lineend = "round", inherit.aes = FALSE, arrow = arrow(angle = 50)) +
  scale_size(range = c(1,5), guide = "none")+
  labs(color = "Trains en retard à l'arrivée (en %)") +
  scale_color_gradient("Trains en retard à l'arrivée (en %)", high = "darkblue", low = "cyan")  +
  theme(panel.background = element_blank(), #On enlève le fond
        axis.title = element_blank(), #On enlève le titre des axes
        axis.ticks = element_blank(), #On enlève les graduations
        axis.text = element_blank()) +#On enlève le texte des axes 
  geom_text(aes(x = testtt$Longitude, y = testtt$Latitude) ,label = rownames(testtt), nudge_x = 0, nudge_y = c(-0.05,-0.05,-0.03,-0.03,0.02), fontface = 2, color = c("white","white","black","black","black"))

            