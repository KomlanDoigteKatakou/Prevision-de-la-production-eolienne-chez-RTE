library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(data.table)
library(ggplot2)
library(mgcv)

rmse <- function(obs, prev, na_rm = TRUE){
  return(sqrt(mean((prev - obs)^2, na.rm = na_rm)))
}

#---AVEC 17 ZONES

# 1. Agrégation des fichiers, calcul de ff50 juste pour la nouvelle carte
df_grille_17zones <- map(list.files("data/wind_era5", "nc", full.names = TRUE), function(ff){
  logger::log_info(ff)
  fid <- ncdf4::nc_open(ff)
  file_dump <- fid$dim
  
  origin_date <- strsplit(split = " since ", x = file_dump$time$units)[[1]][2]
  
  indexes_names <- list(
    longitude = as.character(round(file_dump$longitude$vals, 2)),
    latitude = as.character(round(file_dump$latitude$vals, 2)),
    time =  as.character(file_dump$time$vals)
  )
  
  array_meteo0 <- sqrt(ncdf4::ncvar_get(nc = fid, varid = "u100")^2 + ncdf4::ncvar_get(nc = fid, varid = "v100")^2)
  dimnames(array_meteo0) <- indexes_names
  df_meteo0 <- as.data.frame.table(array_meteo0, stringsAsFactors = FALSE, responseName = 'ff100') %>% setDT()
  df_meteo0[, date_cible := ymd_hms(origin_date) + dhours(as.numeric(time))]
  df_meteo0$time <- NULL
  
  ncdf4::nc_close(fid)
  
  # Calcul de ff50
  df_meteo0[, .(ff50 = mean(ff100*(0.5^0.2), na.rm = TRUE)), by = .(longitude, latitude)]
}) %>% rbindlist(use.names = TRUE)

# 2. Classification en 17 zones
df_grille_17zones[, longitude := as.numeric(longitude)]
df_grille_17zones[, latitude := as.numeric(latitude)]

df_grille_17zones[, ZCnew := fcase(
  ff50 <= 4.5, 1,
  ff50 > 4.5 & ff50 <= 4.7, 2,
  ff50 > 4.7 & ff50 <= 4.9, 3,
  ff50 > 4.9 & ff50 <= 5.2, 4,
  ff50 > 5.2 & ff50 <= 5.4, 5,
  ff50 > 5.4 & ff50 <= 5.6, 6,
  ff50 > 5.6 & ff50 <= 5.8, 7,
  ff50 > 5.8 & ff50 <= 6.0, 8,
  ff50 > 6.0 & ff50 <= 6.2, 9,
  ff50 > 6.2 & ff50 <= 6.4, 10,
  ff50 > 6.4 & ff50 <= 6.6, 11,
  ff50 > 6.6 & ff50 <= 6.8, 12,
  ff50 > 6.8 & ff50 <= 7.0, 13,
  ff50 > 7.0 & ff50 <= 7.2, 14,
  ff50 > 7.2 & ff50 <= 7.4, 15,
  ff50 > 7.4 & ff50 <= 7.5, 16,
  ff50 > 7.5, 17
)]

# Nouvelle carte

# Télécharger la France sous forme de sf
france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name == "France")

# Convertir df_grille_20zones en sf
sf_17zones <- st_as_sf(df_grille_17zones, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326) %>%
  st_transform(crs = st_crs(france))

# Retenir que les points dans les limites de la France
sf_17zones <- st_intersection(sf_17zones, france)

# Palette de 17 couleurs distinctes
custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
                    "#c49c94", "#f7b6d2")

ggplot() +
  # Fond de carte France
  geom_sf(data = france, fill = "gray", color = "black") +
  
  # Points colorés (uniquement dans les limites de la France)
  geom_sf(data = sf_17zones, aes(fill = as.factor(ZCnew)), 
          color = "white", size = 2, shape = 21) +
  
  # Palette de couleurs
  scale_fill_manual(values = custom_palette, name = "Zone de vent") +
  
  # Paramètres esthétiques
  labs(
    title = "Découpage en 17 zones selon la vitesse à 50 m",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(41, 51), expand = FALSE) +  # Limiter à la France
  theme_minimal()

# Nouvel histogramme
categories <- unique(df_grille_17zones$ZCnew)

# Création de l'histogramme avec la palette
ggplot(df_grille_17zones, aes(x = as.factor(ZCnew), fill = as.factor(ZCnew))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette, name = "Zone de Classement (ZCnew)") +
  labs(
    title = "Nombre d'observations selon ZCnew",
    x = "ZCnew (Zone de Classement)",
    y = "Nombre d'observations"
  ) +
  theme_minimal()


# Reprise de l'agrégation des fichiers pour la modélisation

# Traitement des fichiers NetCDF
df_meteo_zone <- map(list.files("~/RSTUDIO/transfer_9332328_files_2d5ddfd9/data/wind_era5", "nc", full.names = TRUE), function(ff){
  logger::log_info(ff)
  
  # Ouverture du fichier NetCDF
  fid <- ncdf4::nc_open(ff)
  file_dump <- fid$dim
  
  # Extraction de la date d'origine
  origin_date <- strsplit(split = " since ", x = file_dump$time$units)[[1]][2]
  
  # Définition des index
  indexes_names <- list(
    longitude = as.character(round(file_dump$longitude$vals, 2)),
    latitude = as.character(round(file_dump$latitude$vals, 2)),
    time = as.character(file_dump$time$vals)
  )
  
  # Extraction des données
  array_meteo0 <- sqrt(ncdf4::ncvar_get(nc = fid, varid = "u100")^2 + ncdf4::ncvar_get(nc = fid, varid = "v100")^2)
  dimnames(array_meteo0) <- indexes_names
  
  # Conversion en data.table
  df_meteo0 <- as.data.frame.table(array_meteo0, stringsAsFactors = FALSE, responseName = 'ff100') %>% setDT()
  df_meteo0[, date_cible := ymd_hms(origin_date) + dhours(as.numeric(time))]
  df_meteo0$time <- NULL
  
  # Fermeture du fichier NetCDF
  ncdf4::nc_close(fid)
  
  # Calcul de ff50 à partir de ff100
  df_meteo0[, ff50 := as.numeric(ff100 * (0.5^0.2))]
  
  # Application du rezonage (ZCnew) en fonction de ff50
  df_meteo0[, ZCnew := fcase(
    ff50 <= 4.5, 1,
    ff50 > 4.5 & ff50 <= 4.7, 2,
    ff50 > 4.7 & ff50 <= 4.9, 3,
    ff50 > 4.9 & ff50 <= 5.2, 4,
    ff50 > 5.2 & ff50 <= 5.4, 5,
    ff50 > 5.4 & ff50 <= 5.6, 6,
    ff50 > 5.6 & ff50 <= 5.8, 7,
    ff50 > 5.8 & ff50 <= 6, 8,
    ff50 > 6 & ff50 <= 6.2, 9,
    ff50 > 6.2 & ff50 <= 6.4, 10,
    ff50 > 6.4 & ff50 <= 6.6, 11,
    ff50 > 6.6 & ff50 <= 6.8, 12,
    ff50> 6.8 & ff50 <= 7, 13,
    ff50> 7 & ff50 <= 7.2, 14,
    ff50> 7.2 & ff50 <= 7.4, 15,
    ff50> 7.4 & ff50 <= 7.5, 16,
    ff50> 7.5, 17
  )]
  
  # Agrégation zonale
  df_meteo_zone_ff <- df_meteo0[
    , .(ff50 = mean(ff50, na.rm = TRUE)), 
    by = .(date_cible, ZCnew)
  ]
  
  df_meteo_zone_ff
}) %>% rbindlist(use.names = TRUE)

# Affichage des premières lignes
print(head(df_meteo_zone))

# Visualisation de ff50 sur Janv 2020 selon la nouvelle classif ZCnew
df_meteo_zone[(year(date_cible)==2020) & (month(date_cible) == 1)] %>% 
  ggplot() + 
  geom_line(aes(x=date_cible, y=ff50, color = factor(ZCnew)))+
  theme_light()

# Sauvegarde sous format wide
saveRDS(df_meteo_zone %>% 
          pivot_wider(id_cols = date_cible, 
                      names_from = ZCnew, 
                      values_from = ff50, 
                      names_prefix = "ff50_"), 
        "data/input_model/df_meteo_zone_wide.RDS")

# Modélisation (avec et sans renorm) -----------------------------------------------------

df_fr <- merge(
  df_meteo_zone[, .(ff50_fr = mean(ff50, na.rm = TRUE)), by = .(date_cible)],
  df_prod,
  by = "date_cible"
)

df_fr[, time_since_beginning := as.numeric(difftime(date_cible, ymd_hms("2017-01-01 00:00:00")))]

mod_gam_noFC_linklog <- bam(
  eolien ~ s(ff50_fr, k = 10) + s(time_since_beginning, k = 7),
  data = df_fr,
  family = gaussian(link = "log"),
  discrete = TRUE
)

# PREVISION de eolien SANS RENORM
df_fr[, eolien_pred := exp(predict(mod_gam_noFC_linklog, newdata = df_fr))]

# Calcul du RMSE
rmse(df_fr$eolien, df_fr$eolien_pred)

# Tracé temporel des valeurs vraies et ajustées
ggplot(df_fr, aes(x = date_cible)) +
  geom_line(aes(y = eolien, color = "Valeurs vraies"), size = 1) +  # Ligne des valeurs vraies
  geom_line(aes(y = eolien_pred, color = "Valeurs prédites"), size = 1, linetype = "dashed") +  # Ligne des valeurs prédites
  scale_color_manual(
    values = c("Valeurs vraies" = "blue", "Valeurs prédites" = "red"),
    name = "Légende"
  ) +
  labs(
    title = "Évolution temporelle des valeurs vraies et prédites",
    x = "Date",
    y = "Production éolienne",
    color = "Ligne"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# SUITE DE LA MODELISATION (AVEC RENORM)

df_fr[,pi := exp(predict(mod_gam_noFC_linklog, newdata = df_fr %>% mutate(ff50_fr = 12*(0.5^0.2))))]


# Sauvegarde
saveRDS(df_fr[,.(date_cible, pi, eolien_pred)], "~/RSTUDIO/transfer_9332328_files_2d5ddfd9/data/input_model/df_pi.RDS")

# import data -------------------------------------------------------------

df_input <- merge(readRDS("data/input_model/df_pi.RDS"), readRDS("data/input_model/df_prod.RDS"), by = "date_cible") %>% 
  mutate(FC=eolien/pi) %>% 
  merge(readRDS("data/input_model/df_meteo_zone_wide.RDS"), by = "date_cible")

# 3. Vérification
head(df_input)

# GAM
modgam_17z_formula <- "FC ~ s(ff50_1) + s(ff50_2) + s(ff50_3) + s(ff50_4) + s(ff50_5)+s(ff50_6)+s(ff50_7)+s(ff50_8)+s(ff50_9)+s(ff50_10)+s(ff50_11)+s(ff50_12)+s(ff50_13)+s(ff50_14)+s(ff50_15)+s(ff50_16)+s(ff50_17)"
modgam_17z <- bam(as.formula(modgam_17z_formula), data=df_input, discrete = TRUE)

plot(modgam_17z, pages = 1)
summary(modgam_17z)

# GAM avec validation croisée par blocs annuels
df_prev <- map(unique(year(df_input$date_cible)), function(yy_test){
  
  df_input[,train_test := fifelse(year(date_cible) == yy_test, "test", "train")]
  if(yy_test != 2022){
    df_input[year(date_cible) == 2022, train_test := "out"]
  }
  
  modgam_17z <- bam(as.formula(modgam_17z_formula), data=df_input[train_test == "train"], discrete = TRUE)
  df_input[train_test == "test"] %>% mutate(prev = predict(modgam_17z, newdata = .))
  
}) %>% 
  rbindlist()

df_prev[,.(rmse = rmse(FC, prev))]

#---------------- AVEC 20 ZONES ---------------

# 1. Agrégation des fichiers et calcul de ff50 juste pour la carte
df_grille_20zones <- map(list.files("data/wind_era5", "nc", full.names = TRUE), function(ff){
  logger::log_info(ff)
  fid <- ncdf4::nc_open(ff)
  file_dump <- fid$dim
  
  origin_date <- strsplit(split = " since ", x = file_dump$time$units)[[1]][2]
  
  indexes_names <- list(
    longitude = as.character(round(file_dump$longitude$vals, 2)),
    latitude = as.character(round(file_dump$latitude$vals, 2)),
    time =  as.character(file_dump$time$vals)
  )
  
  array_meteo0 <- sqrt(ncdf4::ncvar_get(nc = fid, varid = "u100")^2 + ncdf4::ncvar_get(nc = fid, varid = "v100")^2)
  dimnames(array_meteo0) <- indexes_names
  df_meteo0 <- as.data.frame.table(array_meteo0, stringsAsFactors = FALSE, responseName = 'ff100') %>% setDT()
  df_meteo0[, date_cible := ymd_hms(origin_date) + dhours(as.numeric(time))]
  df_meteo0$time <- NULL
  
  ncdf4::nc_close(fid)
  
  # Calcul de ff50
  df_meteo0[, .(ff50 = mean(ff100*(0.5^0.2), na.rm = TRUE)), by = .(longitude, latitude)]
}) %>% rbindlist(use.names = TRUE)

# 2. Classification en 20 zones
df_grille_20zones[, longitude := as.numeric(longitude)]
df_grille_20zones[, latitude := as.numeric(latitude)]

df_grille_20zones[, ZCnew := fcase(
  ff50 <= 4.5, 1,
  ff50 > 4.5 & ff50 <= 4.7, 2,
  ff50 > 4.7 & ff50 <= 4.9, 3,
  ff50 > 4.9 & ff50 <= 5.2, 4,
  ff50 > 5.2 & ff50 <= 5.4, 5,
  ff50 > 5.4 & ff50 <= 5.6, 6,
  ff50 > 5.6 & ff50 <= 5.8, 7,
  ff50 > 5.8 & ff50 <= 6.0, 8,
  ff50 > 6.0 & ff50 <= 6.2, 9,
  ff50 > 6.2 & ff50 <= 6.4, 10,
  ff50 > 6.4 & ff50 <= 6.6, 11,
  ff50 > 6.6 & ff50 <= 6.8, 12,
  ff50 > 6.8 & ff50 <= 7.0, 13,
  ff50 > 7.0 & ff50 <= 7.2, 14,
  ff50 > 7.2 & ff50 <= 7.4, 15,
  ff50 > 7.4 & ff50 <= 7.5, 16,
  ff50> 7.5 & ff50 <= 7.7, 17,
  ff50> 7.7 & ff50 <= 7.9, 18,
  ff50> 7.9 & ff50 <= 8.1, 19,
  ff50> 8.1 , 20
)]

# Nouvelle carte

# Télécharger la France sous forme de sf
france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name == "France")

# Convertir df_grille_20zones en sf
sf_20zones <- st_as_sf(df_grille_20zones, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326) %>%
  st_transform(crs = st_crs(france))

# Retenir que les points dans les limites de la France
sf_20zones <- st_intersection(sf_20zones, france)

# Palette de 20 couleurs distinctes
custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
                    "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5")

ggplot() +
  # Fond de carte France
  geom_sf(data = france, fill = "gray", color = "black") +
  
  # Points colorés (uniquement dans les limites de la France)
  geom_sf(data = sf_20zones, aes(fill = as.factor(ZCnew)), 
          color = "white", size = 2, shape = 21) +
  
  # Palette de couleurs
  scale_fill_manual(values = custom_palette, name = "Zone de vent") +
  
  # Paramètres esthétiques
  labs(
    title = "Découpage en 20 zones selon la vitesse à 50 m",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(41, 51), expand = FALSE) +  # Limiter à la France
  theme_minimal()

# Nouvel histogramme
categories <- unique(df_grille_20zones$ZCnew)

# Création de l'histogramme avec la palette
ggplot(df_grille_20zones, aes(x = as.factor(ZCnew), fill = as.factor(ZCnew))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette, name = "Zone de Classement (ZCnew)") +
  labs(
    title = "Nombre d'observations selon ZCnew",
    x = "ZCnew (Zone de Classement)",
    y = "Nombre d'observations"
  ) +
  theme_minimal()


# Reprise de l'agrégation pour la modélisation
# Traitement des fichiers NetCDF
df_meteo_zone <- map(list.files("~/RSTUDIO/transfer_9332328_files_2d5ddfd9/data/wind_era5", "nc", full.names = TRUE), function(ff){
  logger::log_info(ff)
  
  # Ouverture du fichier NetCDF
  fid <- ncdf4::nc_open(ff)
  file_dump <- fid$dim
  
  # Extraction de la date d'origine
  origin_date <- strsplit(split = " since ", x = file_dump$time$units)[[1]][2]
  
  # Définition des index
  indexes_names <- list(
    longitude = as.character(round(file_dump$longitude$vals, 2)),
    latitude = as.character(round(file_dump$latitude$vals, 2)),
    time = as.character(file_dump$time$vals)
  )
  
  # Extraction des données
  array_meteo0 <- sqrt(ncdf4::ncvar_get(nc = fid, varid = "u100")^2 + ncdf4::ncvar_get(nc = fid, varid = "v100")^2)
  dimnames(array_meteo0) <- indexes_names
  
  # Conversion en data.table
  df_meteo0 <- as.data.frame.table(array_meteo0, stringsAsFactors = FALSE, responseName = 'ff100') %>% setDT()
  df_meteo0[, date_cible := ymd_hms(origin_date) + dhours(as.numeric(time))]
  df_meteo0$time <- NULL
  
  # Fermeture du fichier NetCDF
  ncdf4::nc_close(fid)
  
  # Calcul de ff50 à partir de ff100
  df_meteo0[, ff50 := as.numeric(ff100 * (0.5^0.2))]
  
  # Application du rezonage (ZCnew) en fonction de ff50
  df_meteo0[, ZCnew := fcase(
    ff50 <= 4.5, 1,
    ff50 > 4.5 & ff50 <= 4.7, 2,
    ff50 > 4.7 & ff50 <= 4.9, 3,
    ff50 > 4.9 & ff50 <= 5.2, 4,
    ff50 > 5.2 & ff50 <= 5.4, 5,
    ff50 > 5.4 & ff50 <= 5.6, 6,
    ff50 > 5.6 & ff50 <= 5.8, 7,
    ff50 > 5.8 & ff50 <= 6, 8,
    ff50 > 6 & ff50 <= 6.2, 9,
    ff50 > 6.2 & ff50 <= 6.4, 10,
    ff50 > 6.4 & ff50 <= 6.6, 11,
    ff50 > 6.6 & ff50 <= 6.8, 12,
    ff50> 6.8 & ff50 <= 7, 13,
    ff50> 7 & ff50 <= 7.2, 14,
    ff50> 7.2 & ff50 <= 7.4, 15,
    ff50> 7.4 & ff50 <= 7.5, 16,
    ff50> 7.5 & ff50 <= 7.7, 17,
    ff50> 7.7 & ff50 <= 7.9, 18,
    ff50> 7.9 & ff50 <= 8.1, 19,
    ff50> 8.1 , 20
  )]
  
  # Agrégation zonale
  df_meteo_zone_ff <- df_meteo0[
    , .(ff50 = mean(ff50, na.rm = TRUE)), 
    by = .(date_cible, ZCnew)
  ]
  
  df_meteo_zone_ff
}) %>% rbindlist(use.names = TRUE)

# Affichage des premières lignes
print(head(df_meteo_zone))

# Visualisation de ff50 sur Janv 2020 selon la nouvelle classif ZCnew
df_meteo_zone[(year(date_cible)==2020) & (month(date_cible) == 1)] %>% 
  ggplot() + 
  geom_line(aes(x=date_cible, y=ff50, color = factor(ZCnew)))+
  theme_light()

# Sauvegarde sous format wide
saveRDS(df_meteo_zone %>% 
          pivot_wider(id_cols = date_cible, 
                      names_from = ZCnew, 
                      values_from = ff50, 
                      names_prefix = "ff50_"), 
        "data/input_model/df_meteo_zone_wide.RDS")

# Modélisation (avec et sans renorm) -----------------------------------------------------

df_fr <- merge(
  df_meteo_zone[, .(ff50_fr = mean(ff50, na.rm = TRUE)), by = .(date_cible)],
  df_prod,
  by = "date_cible"
)

df_fr[, time_since_beginning := as.numeric(difftime(date_cible, ymd_hms("2017-01-01 00:00:00")))]

mod_gam_noFC_linklog <- bam(
  eolien ~ s(ff50_fr, k = 10) + s(time_since_beginning, k = 7),
  data = df_fr,
  family = gaussian(link = "log"),
  discrete = TRUE
)

# PREVISION de eolien SANS RENORM
df_fr[, eolien_pred := exp(predict(mod_gam_noFC_linklog, newdata = df_fr))]

# Calcul du RMSE
rmse(df_fr$eolien, df_fr$eolien_pred)

# Tracé temporel des valeurs vraies et ajustées
ggplot(df_fr, aes(x = date_cible)) +
  geom_line(aes(y = eolien, color = "Valeurs vraies"), size = 1) +  # Ligne des valeurs vraies
  geom_line(aes(y = eolien_pred, color = "Valeurs prédites"), size = 1, linetype = "dashed") +  # Ligne des valeurs prédites
  scale_color_manual(
    values = c("Valeurs vraies" = "blue", "Valeurs prédites" = "red"),
    name = "Légende"
  ) +
  labs(
    title = "Évolution temporelle des valeurs vraies et prédites (20 zones)",
    x = "Date",
    y = "Production éolienne",
    color = "Ligne"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# SUITE DE LA MODELISATION (AVEC RENORM)

df_fr[,pi := exp(predict(mod_gam_noFC_linklog, newdata = df_fr %>% mutate(ff50_fr = 12*(0.5^0.2))))]


# Sauvegarde
saveRDS(df_fr[,.(date_cible, pi, eolien_pred)], "~/RSTUDIO/transfer_9332328_files_2d5ddfd9/data/input_model/df_pi.RDS")

# import data -------------------------------------------------------------

df_input <- merge(readRDS("data/input_model/df_pi.RDS"), readRDS("data/input_model/df_prod.RDS"), by = "date_cible") %>% 
  mutate(FC=eolien/pi) %>% 
  merge(readRDS("data/input_model/df_meteo_zone_wide.RDS"), by = "date_cible")

# 3. Vérification
head(df_input)

# GAM
modgam_20z_formula <- "FC ~ s(ff50_1) + s(ff50_2) + s(ff50_3) + s(ff50_4) + s(ff50_5)+s(ff50_6)+s(ff50_7)+s(ff50_8)+s(ff50_9)+s(ff50_10)+s(ff50_11)+s(ff50_12)+s(ff50_13)+s(ff50_14)+s(ff50_15)+s(ff50_16)+s(ff50_17)+s(ff50_18)+s(ff50_19)+s(ff50_20)"
modgam_20z <- bam(as.formula(modgam_20z_formula), data=df_input, discrete = TRUE)

plot(modgam_20z, pages = 1)
summary(modgam_20z)

# GAM avec validation croisée par blocs annuels
df_prev <- map(unique(year(df_input$date_cible)), function(yy_test){
  
  df_input[,train_test := fifelse(year(date_cible) == yy_test, "test", "train")]
  if(yy_test != 2022){
    df_input[year(date_cible) == 2022, train_test := "out"]
  }
  
  modgam_20z <- bam(as.formula(modgam_20z_formula), data=df_input[train_test == "train"], discrete = TRUE)
  df_input[train_test == "test"] %>% mutate(prev = predict(modgam_20z, newdata = .))
  
}) %>% 
  rbindlist()

df_prev[,.(rmse = rmse(FC, prev))]
