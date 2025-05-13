# Agrégation des données météorologiques et de production éolienne

df_prod <- df_prod0[,.(eolien = mean(eolien)), by = .(date_cible = ceiling_date(date_cible, unit = "hour"))]


# Fusion des données météorologiques et de production éolienne pour la modélisation

df_fr <- merge(df_meteo_zone[,.(ff100_fr = mean(ff100)), by = .(date_cible)], 
               df_prod, by = "date_cible") %>% 
  mutate(time_since_beginning = as.numeric(difftime(date_cible, ymd_hms("2017-01-01 00:00:00"))))
