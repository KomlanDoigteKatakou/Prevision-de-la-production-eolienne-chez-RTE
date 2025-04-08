# Importation des données brutes et nettoyage
df_prod0 <- map(list.files("data/eco2mix/", "xls$", full.names = TRUE), function(ff) {
  log_info(ff)
  tmp <- read.table(ff, fill = T, header = T, quote="", sep  ="\t", row.names = NULL)
  names(tmp) = c(names(tmp[,-1]),"dumb")
  clean_names(tmp) %>% 
    select(perimetre, nature, date, heures, eolien) %>% 
    drop_na()
}) %>% 
  rbindlist(use.names=TRUE)

df_prod0[,date_cible := with_tz(force_tz(ymd_hm(paste(date, heures)), "Europe/Paris"), "UTC")]
df_prod <- df_prod0[,.(eolien = mean(eolien)), by = .(date_cible = ceiling_date(date_cible, unit = "hour"))]
saveRDS(df_prod, "data/input_model/df_prod.RDS")


# Importation des données de vent
df_meteo_zone <- map(list.files("data/wind_era5", "nc", full.names = TRUE), function(ff){
  logger::log_info(ff)
  fid <- ncdf4::nc_open(ff)
  file_dump <- fid$dim
  origin_date <- strsplit(split = " since ", x = file_dump$time$units)[[1]][2]
  indexes_names <- list(longitude = as.character(round(file_dump$longitude$vals, 2)),
                        latitude = as.character(round(file_dump$latitude$vals, 2)),
                        time = as.character(file_dump$time$vals))
  
  array_meteo0 <- sqrt(ncdf4::ncvar_get(nc = fid, varid = "u100")^2 + ncdf4::ncvar_get(nc = fid, varid = "v100")^2)
  dimnames(array_meteo0) <- indexes_names
  df_meteo0 <- as.data.frame.table(array_meteo0, stringsAsFactors = FALSE, responseName = 'ff100') %>% setDT()
  df_meteo0[,date_cible := ymd_hms(origin_date) + dhours(as.numeric(time))]
  df_meteo_zone_ff <- merge(df_meteo0 %>% mutate(longitude = round(as.numeric(longitude), 2), latitude = round(as.numeric(latitude), 2)),
                            df_grille %>% mutate(longitude=round(Lon, 2), latitude = round(Lat, 2)),
                            by = c("longitude", "latitude"))[ ,.(ff100 = mean(ff100)), by = .(date_cible, ZC.adapte)]
  
  df_meteo_zone_ff
}) %>% 
  rbindlist(use.names=TRUE)
saveRDS(df_meteo_zone %>% pivot_wider(date_cible, names_from = ZC.adapte, values_from = ff100, names_prefix = "ff100_"), "data/input_model/df_meteo_zone_wide.RDS")
