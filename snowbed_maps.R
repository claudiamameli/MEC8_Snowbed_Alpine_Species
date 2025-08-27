# Library -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(sp)
library(colorRamps)
library(RColorBrewer)
library(paletteer)
library(terra)
library(sf)
library(rworldmap)
library(gpkg)


# A. SPECIES DATA ---------------------------------------------------------
# Specied Data Import & Cleanup ------------------------------------------------------------
df_species_tot <- read.csv("~/Desktop/GitHub/MEC8_Snowbed_Alpine_Species/Data/species_wide.csv")
str(df_species_tot)

# Gnaphalium supinum, Luzula alpino-pilosa, Salix herbacea, Sibbaldia procumbens
colnames(df_species_tot)[grepl("Gnaphalium", colnames(df_species_tot))]
colnames(df_species_tot)[grepl("Luzula", colnames(df_species_tot))]
colnames(df_species_tot)[grepl("Salix", colnames(df_species_tot))]
colnames(df_species_tot)[grepl("Sibbaldia", colnames(df_species_tot))]

df_species <- df_species_tot %>% 
  select("logger_ID", "Gnaphalium.supinum", "Luzula.alpino.pilosa", "Salix.herbacea", "Sibbaldia.procumbens")



# # Uncomment if need to save the file again
# write.csv(df_species, "~/Desktop/GitHub/MEC8_Snowbed_Alpine_Species/Data/df_species.csv")

# Geo Data  Import & Cleanup ----------------------------------------------------------------
st_layers("~/Desktop/GitHub/MEC8_Snowbed_Alpine_Species/Data/site_data.gpkg")
geo_data <- read_sf("~/Desktop/GitHub/MEC8_Snowbed_Alpine_Species/Data/site_data.gpkg")

plot(geo_data$geom)


all_data <- df_species %>% 
  left_join(geo_data$geom, by = "logger_ID")


g_supinum_data <- df_species %>%
  select("logger_ID", "Gnaphalium.supinum") %>% 
  filter(Gnaphalium.supinum != 0) %>% 
  left_join(geo_data, by = "logger_ID")

l_alpinospinosa_data <- df_species %>%
  select("logger_ID", "Luzula.alpino.pilosa") %>% 
  filter(Luzula.alpino.pilosa != 0) %>% 
  left_join(geo_data, by = "logger_ID")

s_herbacea_data <- df_species %>%
  select("logger_ID", "Salix.herbacea") %>% 
  filter(Salix.herbacea != 0) %>% 
  left_join(geo_data, by = "logger_ID")

s_procumbens_data <- df_species %>%
  select("logger_ID", "Sibbaldia.procumbens") %>% 
  filter(Sibbaldia.procumbens != 0) %>% 
  left_join(geo_data, by = "logger_ID")



# B. GEO DATA -------------------------------------------------------------
# Import & Clean data  --------------------------------------------------------------

snowbeds_tif <- list.files("~/Desktop/GitHub/MEC8_Snowbed_Alpine_Species/Data/snow_cover_maps" ,pattern = "tif$",full.names=T )

snowbed_list <-  rast(snowbeds_tif)
plot(snowbed_list)
names(snowbed_list)

years <- stringr::str_extract(names(snowbed_list), "\\d{4}") %>% as.numeric()
months <- stringr::str_extract(names(snowbed_list), "\\d_\\d_")

snowbed_pres_1_7 <- snowbed_list[[years >= 2015 & years <= 2025 & months == "1_7_"]]
snowbed_pres_1_5 <- snowbed_list[[years >= 2015 & years <= 2025 & months == "1_5_"]]
snowbed_fut_1_7  <- snowbed_list[[years >= 2090 & years <= 2100 & months == "1_7_"]]
snowbed_fut_1_5  <- snowbed_list[[years >= 2090 & years <= 2100 & months == "1_5_"]]

plot(snowbed_pres_1_7)
plot(snowbed_fut_1_7) 
plot(snowbed_pres_1_5)
plot(snowbed_fut_1_5) 

plot(snowbed_pres_1_7$snowcover1_7_2023)

points(g_supinum_data$geom, col="red", pch = 16, cex=1)
points(l_alpinospinosa_data$geom, col="purple", pch = 16, cex=1)
points(s_herbacea_data$geom, col="blue", pch = 16, cex=1)
points(s_procumbens_data$geom, col="green", pch = 16, cex=1)


# Extraction of Snowcover days - Function ---------------------------------
# this funciton will output a dataframe with values representing number of days of snowcover for each plot containing species, calculates a mean number of days for each plot and includes original logger_ID ang geom points

dd <- data.frame(geo_data, terra::extract(snowbed_pres_1_7, geo_data, ID = F))

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
