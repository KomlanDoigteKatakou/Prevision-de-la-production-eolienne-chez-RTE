# Ajustement d'un modèle GAM avec lien logarithmique pour
# prédire la puissance installée

mod_gam_noFC_linklog <- bam(eolien ~ s(ff100_fr) + s(time_since_beginning, k=7), data = df_fr, family = gaussian(link = "log"), discrete = TRUE)
df_fr[,pi := exp(predict(mod_gam_noFC_linklog, newdata = df_fr %>% mutate(ff100_fr = 12)))]
saveRDS(df_fr[,.(date_cible, pi)], "data/input_model/df_pi.RDS")
modgam_10z_formula <- "FC ~s(ff100_1)+s(ff100_2)+s(ff100_3)+s(ff100_4)+s(ff100_5)+s(ff100_6)+s(ff100_7)+s(ff100_8)+s(ff100_9)+s(ff100_10)"
modgam_10z <- bam(as.formula(modgam_10z_formula), data=df_input, discrete = TRUE)
plot(modgam_10z, pages = 1)


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

df_prev[,.(rmse = rmse(FC, prev))]
