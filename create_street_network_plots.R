
library(tidyverse)
library(sf)
library(lwgeom)

drive <- c("trunk", "primary", "secondary", "residential", "tertiary", "primary_link", "secondary_link", "tertiary_link", "trunk_link")

roads <- read_sf('data/gis_osm_roads_free_1.shp')
city <- read_sf("data/gis_osm_places_a_free_1.shp") %>% filter(name == 'Астрахань')

city_roads <- st_intersection(city, roads) %>% 
  mutate(type = st_geometry_type(geometry)) %>% 
  filter(type == 'LINESTRING', fclass.1 %in% drive)

saveRDS(city_roads, 'city_roads.rds')

# 1. Compute bearings and lengths ----

# Helper function

create_st_point <- function(point) {
  
  x <- point[[1]][2]
  y <- point[[1]][1]
  result <- sf::st_point(c(x, y))
  
  return(result)
}

# Main function

get_street_stats <- function(street) {
  
  start <- lwgeom::st_startpoint(street)
  start_point <- create_st_point(start)
  
  end <- lwgeom::st_endpoint(street)
  end_point <- create_st_point(end)
  
  f <- sf::st_sfc(start_point, end_point, crs = 4326) # f stands for feature
  rad <- lwgeom::st_geod_azimuth(f) # rad stands for radian
  deg <- rad * 180 / pi # def stands for degrees
  
  street_utm <- street %>% sf::st_transform(crs = 32639)
  l <- sf::st_length(street_utm) # l stands for length
  
  result <- data.frame(osm_id = street$osm_id.1, deg = as.numeric(deg), length = as.numeric(l))
  
  print(street$osm_id.1)
  return(result)
}

# Collect street stats

df_stats <- map_df(.x = 1:nrow(city_roads), .f = ~get_street_stats(city_roads[.x,]))
saveRDS(df_stats, 'df_stats.rds')

# 2. Visualize data ----

maps_theme <-   theme(
  axis.ticks = element_blank(), 
  axis.text = element_blank(), 
  panel.background=element_blank(),
  panel.border=element_blank(),
  panel.grid.major=element_line(colour = "transparent"),
  panel.grid.minor=element_blank(),
  plot.background=element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

city_roads <- readRDS('city_roads.rds')
df_stats <- readRDS('df_stats.rds')

df_stats <- df_stats %>% 
  na.omit() %>% 
  mutate(
    deg = round(deg, 0),
    deg = ifelse(deg < 180, deg + 180, deg - 180)
  )

create_gg_df <- function(df) {
  
  df_90 <- df %>% 
    filter(deg <= 90)
  
  df_90_180 <- df %>% 
    filter(deg > 90, deg <= 180)
  
  df_180_270 <- df %>% 
    filter(deg > 180, deg <= 270)
  
  df_270_360 <- df %>% 
    filter(deg > 270)
  
  df1 <- rbind(df_90, df_180_270 %>% mutate(deg = deg - 180)) %>% 
    mutate(deg_bin = cut(deg, breaks = seq(0,90,10), labels = FALSE, include.lowest = T)) %>% 
    group_by(deg_bin) %>% 
    summarise(n = n()/2)
  
  df2 <- rbind(df_90_180, df_270_360 %>% mutate(deg = deg - 180)) %>% 
    mutate(
      deg_bin = cut(deg, breaks = seq(90,180,10), labels = FALSE, include.lowest = T),
      deg_bin = deg_bin + 9
    ) %>% 
    group_by(deg_bin) %>% 
    summarise(n = n()/2)
  
  df3 <- df1 %>%
    ungroup() %>% 
    mutate(deg_bin = deg_bin + 18)
  
  df4 <- df2 %>%
    ungroup() %>% 
    mutate(deg_bin = deg_bin + 18)
  
  df_gg <- df1 %>% 
    rbind(df2) %>% 
    rbind(df3) %>% 
    rbind(df4)
  
  return(df_gg)
}

df_gg <- create_gg_df(df_stats)

# Street network map

gg1 <- ggplot() + 
  geom_sf(data = city_roads, col = 'grey60') +
  maps_theme

gg1

ggsave(filename = 'street_network.png', plot = gg1, device = 'png', path = 'plots', scale = 2, dpi = 800)

# Orientation histogram

gg2 <- ggplot(df_gg, aes(x = deg_bin, y = n)) +
  geom_col(fill = '#003366', width = 1, color = 'grey10', size = 0.5, position = position_nudge(x = -0.5), alpha = 0.7) +
  coord_polar(start = 270 * pi / 180, direction = -1) +
  labs(x = '', y = '') +
  scale_x_continuous(breaks = c(0,9,18,27)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(colour = 'grey70', size = 0.8),
    panel.grid.minor.x = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  )

gg2

ggsave(filename = 'orientation_histogram.png', plot = gg2, device = 'png', path = 'plots', dpi = 300)

