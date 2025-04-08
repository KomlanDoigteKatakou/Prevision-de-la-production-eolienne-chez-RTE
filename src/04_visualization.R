# Visualisation des données de vent
ggplot(df_meteo_zone) + 
  geom_tile(aes(x = longitude, y = latitude, fill = ff100_1)) + 
  theme_minimal() +
  scale_fill_viridis_c()

# Visualisation des prédictions de production
ggplot(df_fr) + geom_line(aes(x=date_cible, y=pi, color="PI")) + 
  geom_line(aes(x=date_cible, y=eolien, color="prod")) 

# Visualisation des résidus
ggplot(df_prev) + 
  geom_point(aes(x = prev, y = FC - prev)) + 
  labs(title = "Résidus de la prédiction", x = "Prédictions", y = "Résidus") +
  theme_minimal()

# Histogramme des résidus
ggplot(df_prev) + 
  geom_histogram(aes(x = FC - prev), bins = 30, fill = "blue", alpha = 0.7) + 
  labs(title = "Histogramme des résidus", x = "Résidus", y = "Fréquence") +
  theme_minimal()
