# Agrégation des données météorologiques et de production éolienne
df_prod <- df_prod0[,.(eolien = mean(eolien)), by = .(date_cible = ceiling_date(date_cible, unit = "hour"))]

# Traitement de la grille géographique
df_grille <- fread("data/ZC_grille_01_elargie.csv")
dt_dpt_for_plot <- ggplot2::map_data("france") %>% filter(!stringr::str_detect(region, "Corse"))

ggplot(df_grille) + 
  geom_raster(aes(x=Lon, y=Lat, fill = factor(ZC.adapte)), alpha = 0.3, show.legend = FALSE) + 
  theme_minimal() + 
  scale_alpha(range = c(0.2, 0.5), na.value = 0) + 
  geom_polygon(aes(long, lat, group = group),fill = NA, colour = "grey70", data=dt_dpt_for_plot, alpha = 0.5) +
  geom_text(aes(x=Lon, y=Lat, label = ZC.adapte), df_grille[,.(Lon= mean(Lon), Lat= mean(Lat)), by = .(ZC.adapte)]) +
  coord_quickmap()

# Fusion des données météorologiques et de production éolienne pour le calcul de la puissance installée
df_fr <- merge(df_meteo_zone[,.(ff100_fr = mean(ff100)), by = .(date_cible)], 
               df_prod, by = "date_cible") %>% 
  mutate(time_since_beginning = as.numeric(difftime(date_cible, ymd_hms("2017-01-01 00:00:00"))))
