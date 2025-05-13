# Zones climatiques
df_grille <- fread("data/ZC_grille_01_elargie.csv")
dt_dpt_for_plot <- ggplot2::map_data("france") %>% filter(!stringr::str_detect(region, "Corse"))

ggplot(df_grille) + 
  geom_raster(aes(x=Lon, y=Lat, fill = factor(ZC.adapte)), alpha = 0.3, show.legend = FALSE) + 
  theme_minimal() + 
  scale_alpha(range = c(0.2, 0.5), na.value = 0) + 
  geom_polygon(aes(long, lat, group = group),fill = NA, colour = "grey70", data=dt_dpt_for_plot, alpha = 0.5) +
  geom_text(aes(x=Lon, y=Lat, label = ZC.adapte), df_grille[,.(Lon= mean(Lon), Lat= mean(Lat)), by = .(ZC.adapte)]) +
  coord_quickmap()

# Visualisation de la vitesse du vent (Zone 1)

ggplot(df_meteo_zone) + 
  geom_tile(aes(x = longitude, y = latitude, fill = ff100_1)) + 
  theme_minimal() +
  scale_fill_viridis_c()


# Visualisation de l'évolution de la puissance installée et de la production éolienne réelle

ggplot(df_fr) + geom_line(aes(x=date_cible, y=pi, color="PI")) + 
  geom_line(aes(x=date_cible, y=eolien, color="prod")) 


# Visualisation des résidus (Ecarts entre les prédictions du facteur de charge et les vraies valeurs)

ggplot(df_prev) + 
  geom_point(aes(x = prev, y = FC - prev)) + 
  labs(title = "Résidus de la prédiction", x = "Prédictions", y = "Résidus") +
  theme_minimal()

# Histogramme des résidus
ggplot(df_prev) + 
  geom_histogram(aes(x = FC - prev), bins = 30, fill = "blue", alpha = 0.7) + 
  labs(title = "Histogramme des résidus", x = "Résidus", y = "Fréquence") +
  theme_minimal()
