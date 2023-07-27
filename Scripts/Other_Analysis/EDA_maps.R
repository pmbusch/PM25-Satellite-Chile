library(dplyr)
library(chilemapas)
library(leaflet)
library(sf)
library(tmap)
library(ggplot2)
library(tidyverse)

# datasets
# mortality_data.csv
# pm25exposure_commune.csv
# landTemp_commune.csv

setwd("/Users/paulorocha/Documents/Notebooks/PM25-Satelite-Chile")

# mortality data
df.mortality <- read.delim("Data/mortality_data.csv", sep = ",")

# create a subset with only year 2019 using dpplr
df.mortality.2019 <- df.mortality %>% filter(year == 2019)

# create a dataset with the sum of deaths by commune divided by total population
df.mortality.2019.sum <- df.mortality.2019 %>%
  group_by(codigo_comuna) %>%
  summarise(
    death_count = sum(death_count),
    total_pop = mean(pop75, na.rm = T)
  ) %>%
  mutate(death_rate_1000 = death_count / total_pop * 1000) %>%
  ungroup()

# removing 05201, 12202 communes
new.mapa_comunas <- chilemapas::mapa_comunas %>% filter(!codigo_comuna %in% c("12202", "05201"))

new.mapa_comunas$codigo_comuna <- as.integer(new.mapa_comunas$codigo_comuna)

# merge mortality with new.mapa_comunas with codigo_comuna as key keeping all on the left
df.map.mortality <- merge(df.mortality.2019.sum, new.mapa_comunas, by = "codigo_comuna", all.x = T)

df.map.mortality.na <- df.map.mortality %>% mutate(death_rate_1000 = ifelse(total_pop < 50, NA, death_rate_1000))

df.map.mortality.na %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = death_rate_1000), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(
    palette = "Oranges", trans = "reverse", # YlOrBr
    na.value = "white"
  ) +
  guides(fill = guide_legend(title = "75+ mortality per 1,000"))

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/death_rate_75_2019_na.png", width = 10, height = 10, dpi = 300)

# df.pm25[df.pm25$NOM_REGION == "REGIÓN METROPOLITANA DE SANTIAGO",]

# mortality data santiago

df.map.mortality.santiago <- df.map.mortality %>% filter(codigo_region == 13)

df.map.mortality.santiago %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = death_rate_1000), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(
    palette = "Oranges", trans = "reverse", # YlOrBr
    na.value = "white"
  ) +
  guides(fill = guide_legend(title = "75+ mortality per 1,000"))

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/death_rate_75_santiago_2019.pdf", width = 10, height = 10, dpi = 300)

# pm25exposure_commune.csv
# pm25 exposure data
df.pm25 <- read.delim("Data/pm25exposure_commune.csv", sep = ";")

# create a subset with only year 2019 using dpplr
df.pm25.2019 <- df.pm25 %>% filter(year == 2019)

df.pm25.2019.mean <- df.pm25.2019 %>%
  group_by(codigo_comuna) %>%
  summarise(pm25_exposure = mean(pm25_Exposure, na.rm = T),
            pop = mean(total_pop, na.rm = T)) %>%
  ungroup()

df.map.pm25 <- merge(df.pm25.2019.mean, new.mapa_comunas, by = "codigo_comuna", all.x = T)

# plot pm25 exposure by commune (average of 2019)
df.map.pm25 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pm25_exposure), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(palette = "Reds", trans = "reverse") +
  guides(fill = guide_legend(title = expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "] exposure"))))



# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/pm25_exposure_2019.png", width = 10, height = 10, dpi = 300)

# pm25 exposure data santiago
df.map.pm25.santiago <- df.map.pm25 %>% filter(codigo_region == 13)

df.map.pm25.santiago %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pm25_exposure), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(palette = "Reds", trans = "reverse") +
  guides(fill = guide_legend(title = expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "] exposure"))))



ggsave("Scripts/Other_Analysis/Figures-Paulo/pm25_exposure_santiago_2019.pdf", width = 10, height = 10, dpi = 300)

# age 75+
df.map.age <- merge(df.map.pm25, df.map.mortality, by = "codigo_comuna", all.x = T)

df.map.age$pop_75 <- df.map.age$total_pop/df.map.age$pop*100

df.map.age %>%
  ggplot() +
  geom_sf(aes(geometry = geometry.x, fill = pop_75), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(
    palette = "Greens", trans = "reverse", # YlOrBr
    na.value = "white"
  ) +
  guides(fill = guide_legend(title = "75+ population %"))

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/pop_75_percentage.png", width = 10, height = 10, dpi = 300)

# age 75+ santiago
df.map.age.santiago <- df.map.age %>% filter(codigo_region.x == 13)

df.map.age.santiago %>%
  ggplot() +
  geom_sf(aes(geometry = geometry.x, fill = pop_75), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(
    palette = "Greens", trans = "reverse", # YlOrBr
    na.value = "white"
  ) +
  guides(fill = guide_legend(title = "75+ population %"))

ggsave("Scripts/Other_Analysis/Figures-Paulo/pop_75_percentage_santiago.pdf", width = 10, height = 10, dpi = 300)


# landTemp_commune.csv
# temperature data
df.temp <- read.delim("Data/landTemp_commune.csv", sep = ";")

# create a subset with only year 2019 using dpplr
df.temp.2019 <- df.temp %>% filter(year == 2019)

df.temp.2019.mean <- df.temp.2019 %>%
  group_by(codigo_comuna) %>%
  summarise(landTemp = mean(landTemp, na.rm = T)) %>%
  ungroup()

df.map.temp <- merge(df.temp.2019.mean, new.mapa_comunas, by = "codigo_comuna", all.x = T)

# plot land temperature by commune (average of 2019)
df.map.temp %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = landTemp), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(palette = "Blues",
                       na.value = "white") +
  guides(fill = guide_legend(title = "Mean Temperature [°C]"))

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/landTemp_2019.png", width = 10, height = 10, dpi = 300)

# temperature data santiago
df.map.temp.santiago <- df.map.temp %>% filter(codigo_region == 13)

df.map.temp.santiago %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = landTemp), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(palette = "Blues",
                       na.value = "white") +
  guides(fill = guide_legend(title = "Mean Temperature [°C]"))

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/landTemp_santiago_2019.pdf", width = 10, height = 10, dpi = 300)

# monitor location
# open file monitor_coordinates.csv
df.monitor <- read.delim("Data/monitor_coordinates.csv", sep = ";")

# get the coordinates of the unique monitors based on site
df.monitor_site <- df.monitor %>%
  group_by(site) %>%
  slice(1) %>%
  ungroup()

# plot monitor coordinates on the map add legend for site
df.monitor_site %>%
  ggplot() +
  geom_sf(data = df.map.pm25, aes(geometry = geometry, fill = pm25_exposure), lwd = 0.05) +
  scale_fill_distiller(palette = "Reds", trans = "reverse") +
  guides(fill = guide_legend(title = expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "] exposure"))))+
  geom_point(aes(x = longitude, y = latitude, color="Monitor site"), alpha = 1, size = 1) +
  scale_color_manual("",values = c("Monitor site" = "black")) +
  theme_void() 

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/pm25_exposure_2019_monitor_site.png", width = 10, height = 10, dpi = 300)

# monitor location santiago
df.monitor_site.santiago <- df.monitor_site %>% filter(region == "M")

df.monitor_site.santiago %>%
  ggplot() +
  geom_sf(data = df.map.pm25.santiago, aes(geometry = geometry, fill = pm25_exposure), lwd = 0.05) +
  scale_fill_distiller(palette = "Reds", trans = "reverse") +
  guides(fill = guide_legend(title = expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "] exposure"))))+
  geom_point(aes(x = longitude, y = latitude, color="Monitor site"), alpha = 1, size = 3) +
  scale_color_manual("",values = c("Monitor site" = "black")) +
  theme_void() 

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/pm25_exposure_2019_monitor_site_santiago.pdf", width = 10, height = 10, dpi = 300)

# map for 16 regions 
mapa_regiones <- generar_regiones(mapa = new.mapa_comunas)

my.palette <- colorRampPalette(RColorBrewer::brewer.pal(9,name = 'Paired'))(length(unique(mapa_regiones$codigo_region)))
set.seed(44) 

mapa_regiones %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill=codigo_region), lwd = 0.05) +
  scale_fill_manual(values=sample(my.palette)) +
  # scale_fill_brewer(my.palette) +
  theme_void()

# save plot
ggsave("Scripts/Other_Analysis/Figures-Paulo/chile_region_map.png", width = 10, height = 10, dpi = 300)


# Region map with pm25 exposure
df.pm25.2019.region.mean <- df.pm25.2019 %>%
  group_by(REGION) %>%
  summarise(pm25_exposure = mean(pm25_Exposure, na.rm = T)) %>%
  ungroup()

mapa_regiones$codigo_region <- as.integer(mapa_regiones$codigo_region)

df.map.pm25.region <- merge(df.pm25.2019.region.mean, mapa_regiones, 
                            by.x = "REGION", by.y = "codigo_region", all.x = T)
df.map.pm25.region %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pm25_exposure), lwd = 0.05) +
  # geom_sf_text(aes(geometry = geometry, label = codigo_region),size=2)+
  theme_void() +
  scale_fill_distiller(palette = "Reds", trans = "reverse") +
  guides(fill = guide_legend(title = expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "] exposure"))))


ggsave("Scripts/Other_Analysis/Figures-Paulo/chile_region_pm25_map.png", width = 10, height = 10, dpi = 300)