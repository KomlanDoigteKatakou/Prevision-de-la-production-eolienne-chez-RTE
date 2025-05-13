# Ajustement d'un modèle GAM avec lien logarithmique pour prédire la puissance installée

mod_gam_noFC_linklog <- bam(eolien ~ s(ff100_fr) + s(time_since_beginning, k=7), data = df_fr, family = gaussian(link = "log"), discrete = TRUE)

# Estimation de la puissance installée

df_fr[,pi := exp(predict(mod_gam_noFC_linklog, newdata = df_fr %>% mutate(ff100_fr = 12)))]

# Sauvegarde des prédictions au format RDS

saveRDS(df_fr[,.(date_cible, pi)], "data/input_model/df_pi.RDS")


# Définition de la métrique d'évaluation

rmse <- function(obs, prev, na_rm = TRUE){
  return(sqrt(mean((prev - obs)^2, na.rm = na_rm)))
}

# Chargement et fusion des fichiers stockés précédemment, Calcul du FC

df_input <- merge(readRDS("data/input_model/df_pi.RDS"), readRDS("data/input_model/df_prod.RDS"), by = "date_cible") %>% 
  mutate(FC=eolien/pi) %>% 
  merge(readRDS("data/input_model/df_meteo_zone_wide.RDS"), by = "date_cible")


# Modélisation GAM pour prédire le facteur de charge à partir de la vitesse du vent sur les 10 zones

modgam_10z_formula <- "FC ~s(ff100_1)+s(ff100_2)+s(ff100_3)+s(ff100_4)+s(ff100_5)+s(ff100_6)+s(ff100_7)+s(ff100_8)+s(ff100_9)+s(ff100_10)"
modgam_10z <- bam(as.formula(modgam_10z_formula), data=df_input, discrete = TRUE)


# Validation croisée par blocs annuels avec GAM

df_prev <- map(unique(year(df_input$date_cible)), function(yy_test){
  df_input[,train_test := fifelse(year(date_cible) == yy_test, "test", "train")]
  if(yy_test != 2022){
    df_input[year(date_cible) == 2022, train_test := "out"]
  }
  
  modgam_10z <- bam(as.formula(modgam_10z_formula), data=df_input[train_test == "train"], discrete = TRUE)
  df_input[train_test == "test"] %>% mutate(prev = predict(modgam_10z, newdata = .))
}) %>% 
  rbindlist()


# Calcul de l'erreur de prédiction

df_prev[,.(rmse = rmse(FC, prev))]
