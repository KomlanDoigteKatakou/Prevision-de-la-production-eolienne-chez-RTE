# Sauvegarde de la production éolienne agrégée
saveRDS(df_prod, "data/output_model/df_prod_output.RDS")

# Sauvegarde des données météorologiques traitées
saveRDS(df_meteo_zone_wide, "data/output_model/df_meteo_output.RDS")

# Sauvegarde des prédictions de puissance installée
saveRDS(df_fr[,.(date_cible, pi)], "data/output_model/df_pi_output.RDS")

# Sauvegarde des prédictions finales
saveRDS(df_prev, "data/output_model/df_prev_output.RDS")
