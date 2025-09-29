# Extraction of Snowcover days - Function ---------------------------------
  # this function will output a dataframe with values representing number of days of snowcover for each plot containing species, calculates a mean number of days for each plot and includes original logger_ID ang geom points
  
  snowcover_days_fun <- function(species_data, raster_list){
    coord <- vect(species_data$geom)
    values <- terra::extract(raster_list, coord) 
    snowcover_days <- values %>%  
      mutate(mean_days = rowMeans(values[,-1]))
    result <- bind_cols(species_data %>% 
                          select(geom, logger_ID),
                        snowcover_days[,-1]
    )
  }
  
  # Present January - July  -------------------------------------------------
  # Gnaphalium.supinum --------------------------------------------------------------
  g_supinum_pres_1_7 <- snowcover_days_fun(g_supinum_data, snowbed_pres_1_7)
  
  summary_g_supinum_1_7 <- g_supinum_pres_1_7 %>%
    summarise(
      species = "g_supinum",
      q25 = quantile(mean_days, 0.25, na.rm = TRUE),
      mean = mean(mean_days, na.rm = TRUE),
      sd = sd(mean_days, na.rm = TRUE),
      median = median(mean_days, na.rm = TRUE),
      q75= quantile(mean_days, 0.75, na.rm = TRUE),
      q95 = quantile(mean_days, 0.95, na.rm = TRUE)
    )
  # Luzula.alpino.pilosa -------------------------------------------------------------
  l_alpinospinosa_pres_1_7 <- snowcover_days_fun(l_alpinospinosa_data, snowbed_pres_1_7)
  
  summary_l_alpinospinosa_pres_1_7 <- l_alpinospinosa_pres_1_7 %>%
    summarise(
      species = "l_alpinospinosa",
      q25 = quantile(mean_days, 0.25, na.rm = TRUE),
      mean = mean(mean_days, na.rm = TRUE),
      sd = sd(mean_days, na.rm = TRUE), 
      median = median(mean_days, na.rm = TRUE),
      q75= quantile(mean_days, 0.75, na.rm = TRUE),
      q95 = quantile(mean_days, 0.95, na.rm = TRUE)
    )
  
  # Salix.herbacea -------------------------------------------------------------
  s_herbacea_pres_1_7 <- snowcover_days_fun(s_herbacea_data, snowbed_pres_1_7)
  
  summary_s_herbacea_pres_1_7 <- s_herbacea_pres_1_7 %>%
    summarise(
      species = "s_herbacea",
      q25 = quantile(mean_days, 0.25, na.rm = TRUE),
      mean = mean(mean_days, na.rm = TRUE),
      sd = sd(mean_days, na.rm = TRUE),
      median = median(mean_days, na.rm = TRUE),
      q75= quantile(mean_days, 0.75, na.rm = TRUE),
      q95 = quantile(mean_days, 0.95, na.rm = TRUE)
    )
  
  # Sibbaldia.procumbens -------------------------------------------------------------
  s_procumbens_1_7 <- snowcover_days_fun(s_procumbens_data, snowbed_pres_1_7)
  
  summary_s_procumbens_1_7 <- s_procumbens_1_7 %>%
    summarise(
      species = "s_procumbens",
      q25 = quantile(mean_days, 0.25, na.rm = TRUE),
      mean = mean(mean_days, na.rm = TRUE),
      sd = sd(mean_days, na.rm = TRUE),
      median = median(mean_days, na.rm = TRUE),
      q75= quantile(mean_days, 0.75, na.rm = TRUE),
      q95 = quantile(mean_days, 0.95, na.rm = TRUE)
    )
  
  
  # Total summary of Snowcover days -----------------------------------------
  summary_snowcover_1_7 <- summary_g_supinum_1_7 %>% 
    rbind(summary_l_alpinospinosa_pres_1_7, summary_s_herbacea_pres_1_7, summary_s_procumbens_1_7)
  
  summary_snowcover_1_7
  
  
  # binary maps trials  --------------------------------------------------------
  
  trials <- snowbed_pres_1_7
  trials_fut <- snowbed_fut_1_7
  
  g_supinum_min <- (summary_snowcover_1_7$mean[summary_snowcover_1_7$species == "g_supinum"]) - 
    5
  #(summary_snowcover_1_7$sd[summary_snowcover_1_7$species == "g_supinum"])
  
  g_supinum_max <- (summary_snowcover_1_7$mean[summary_snowcover_1_7$species == "g_supinum"]) + 
    5
  #(summary_snowcover_1_7$sd[summary_snowcover_1_7$species == "g_supinum"])
  
  
  trials[["snowcover1_7_2024"]] <- ifel(
    trials[["snowcover1_7_2024"]] > g_supinum_min & trials[["snowcover1_7_2024"]] < g_supinum_max,
    1,
    0)
  
  trials_fut[["snowcover1_7_2100"]] <- ifel(
    trials_fut[["snowcover1_7_2100"]] > g_supinum_min & trials_fut[["snowcover1_7_2100"]] < g_supinum_max,
    1,
    0)
  
  
  plot(snowbed_pres_1_7$snowcover1_7_2024)
  plot(trials$snowcover1_7_2024)
  points(geo_data$geom, col = "purple", pch = 1, cex = 1) # all geom points from plots sampled
  points(g_supinum_data$geom, col="red", pch = 1, cex = 1) # points from supium only  
  
  plot(trials_fut$snowcover1_7_2100)
  
  #' need to know when this point data is from - seems closer to 2024 than 2025 - there's a lot of areas that could be suitable but they do not show presence 
  #' with SD we have really large numbers - maybe have less variation with changes in number of days ?? 
  
  #' should i create a predictor/model that explains the presence of the species depending on the number of days of snow coverage? Obviously snow cover days are not enough to just predict the presence in the plots 
  