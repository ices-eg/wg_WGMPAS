### WGMPAs - who are we

#load libraries
library(tidyverse)
library(rnaturalearth)
library(sf)

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

##load in the who are we data (copied from online)
wgmpas_df <- read.csv("data/who_are_we.csv")

wgmpas_countries <- wgmpas_df%>%filter(Country != "International",
                                       Country != "Australia")%>%pull(Country)%>%unique()

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")%>%st_transform(latlong)

# Check and standardize country names to ensure matching
# (e.g., adjusting names manually or using pattern matching as needed)
world <- world %>%
  mutate(country_match = case_when(
    name_long == "United States of America" ~ "United States",
    name_long == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    TRUE ~ name_long
  ))

# Mark countries in wgmpas_countries as TRUE in a new column 'highlight'
world <- world %>%
  mutate(highlight = country_match %in% wgmpas_countries)

## ICES Strata

ices_strat <- read_sf("data/shapefile/")%>%st_transform(latlong)

global_mpas <- read_sf("data/WDPA_WDOECM_Nov2024_Public_all_shp/")

plot_lims <- st_bbox(world) 
plot_lims[1:4] <- c(-100,30,40,75)

# Plot the map
p1 <- ggplot() +
  geom_sf(data = world,aes(fill = highlight), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "gray90"), name = "Highlight") +
  coord_sf(xlim = plot_lims[c(1,3)], ylim = plot_lims[c(2,4)])+
  theme_minimal() +
  labs(title = "Working Group on Marine Protected Areas and Other Spatial Conservation Measures",
       subtitle = "WGMPAs") +
  theme(legend.position = "none")

#WDPA data files

focal_names <- NULL
wdpa_pointfiles <- c("data/WDPA_WDOECM_Nov2024_Public_all_shp/mpa_dump_1/WDPA_WDOECM_Nov2024_Public_all_shp_0/WDPA_WDOECM_Nov2024_Public_all_shp-points.shp",
                     "data/WDPA_WDOECM_Nov2024_Public_all_shp/mpa_dump_2/WDPA_WDOECM_Nov2024_Public_all_shp-points.shp",
                     "data/WDPA_WDOECM_Nov2024_Public_all_shp/mpa_dump_3/WDPA_WDOECM_Nov2024_Public_all_shp_2/WDPA_WDOECM_Nov2024_Public_all_shp-points.shp")

mpa_ids <- NULL
for(i in wdpa_pointfiles){
  
  mpa_ids <- rbind(mpa_ids,
                 read_sf(i)%>%
                 filter(MARINE !=0)%>%
                 st_transform(latlong)%>%
                 st_intersection(.,plot_lims%>%st_as_sfc())%>%
                 mutate(file_group = which(i == wdpa_pointfiles))%>%
                 data.frame()%>%
                 dplyr::select(WDPAID,NAME,file_group))
}

wdpa_shapefiles <- gsub("-points.shp","-polygons.shp",wdpa_pointfiles)

mpa_polys <- NULL

for(i in wdpa_shapefiles){
  
   mpa_polys <- rbind(mpa_polys,
                      #read_sf(i)%>%
                      tt%>%
                      filter(WDPAID %in% mpa_ids)%>%
                      st_transform(latlong)%>%
                      dplyr::select(WDPAID,NAME,geometry))
  
  }



mpa_polys_1 <- read_sf(wdpa_shapefiles[1])%>%st_transform(latlong)
mpa_polys_2 <- read_sf(wdpa_shapefiles[2])%>%st_transform(latlong)
mpa_polys_3 <- read_sf(wdpa_shapefiles[3])%>%st_transform(latlong)
