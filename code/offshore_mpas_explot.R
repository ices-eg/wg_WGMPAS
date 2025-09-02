##Map for the Offshore MPA example figure 2024 WGMPAs report

#load libraries
library(tidyverse)
library(rnaturalearth)
library(sf)
library(future)
library(furrr)
library(patchwork)
library(ggspatial)

#load processing scripts
source("code/processing_functions.R")

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm_focal <- st_crs(32631)# WGS84 / UTM zone 31N (EPSG:32631)

# Load world map data - also norway
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden","Norway","United Kingdom"
)

# Get European countries and filter for EU members
europe <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(latlong) %>%
  filter(name %in% eu_countries) %>%
  filter(
    !grepl("overseas", name, ignore.case = TRUE),
    !grepl("territory", name, ignore.case = TRUE)
  )

france_metro <- europe %>%
              filter(name == "France") %>%
              st_cast("POLYGON") %>%  # Split multipolygons into polygons
              mutate(area = st_area(.)) %>%
              slice_max(area, n = 1)%>%
              select(names(europe))

# Replace the original France entry with metropolitan France
europe <- europe %>%
  filter(name != "France") %>%
  rbind(france_metro)

eu_bbox <- st_bbox(c(
  xmin = -10,  # Western limit (Atlantic)
  ymin = 35,   # Southern limit (Mediterranean)
  xmax = 32,   # Eastern limit
  ymax = 70    # Northern limit (Scandinavia)
), crs = st_crs(latlong))

# Crop to continental Europe
europe <- europe %>%
  st_crop(eu_bbox)

europe_poly <- ne_countries(scale = "large", returnclass = "sf")%>%
  st_transform(latlong)%>%
  filter(continent == "Europe")%>%
  st_crop(eu_bbox)

europe_single <- europe%>%
                 st_union()%>%
                 st_as_sf()

europe_bbox <- st_bbox(c(xmin = -10, ymin = 35, xmax = 32, ymax = 70), crs = st_crs(latlong))
europe_poly <- st_as_sfc(europe_bbox)

world <- ne_countries(scale = "medium", returnclass = "sf")%>%st_transform(latlong)

#set up the plot
focal_lims <- europe_bbox
focal_lims[1:4] <- c(1.5,52.8,6,54.4)


#WDPA data files (these are curated to only MARINE %in% c(1,2)) in wdpa_process.R

eu_iso_codes <- c( #this will pre-filter shapes we want for the analysis 
  "AUT", # Austria
  "BEL", # Belgium
  "BGR", # Bulgaria
  "HRV", # Croatia
  "CYP", # Cyprus
  "CZE", # Czech Republic
  "DNK", # Denmark
  "EST", # Estonia
  "FIN", # Finland
  "FRA", # France
  "DEU", # Germany
  "GRC", # Greece
  "HUN", # Hungary
  "IRL", # Ireland
  "ITA", # Italy
  "LVA", # Latvia
  "LTU", # Lithuania
  "LUX", # Luxembourg
  "MLT", # Malta
  "NLD", # Netherlands
  "POL", # Poland
  "PRT", # Portugal
  "ROU", # Romania
  "SVK", # Slovakia
  "SVN", # Slovenia
  "ESP", # Spain
  "SWE", # Sweden
  "GBR", # United Kingdom - keeping for analysis
  "NOR"  #Norway
)

wgmpas_wdpa_mpa <- st_read("data/shapefile/wgmpas_wdpa_curated.shp", quiet = TRUE)%>%
  st_transform(latlong)%>%
  mutate(type="MPA")%>%
  filter(PARENT_ISO %in% eu_iso_codes)

# #curate 'offshore' MPAs
# 
# # Usage
# # First, set up parallel processing
# plan(multisession, workers = availableCores() - 1)
# 
# # Run the process
# offshore_results <- process_offshore_mpas_efficient(
#   mpas = wgmpas_wdpa_mpa,
#   europe = europe
# )
# 
# # Clean up parallel processing
# plan(sequential)
# 
# write_sf(offshore_results,dsn="data/shapefile/wgmpas_wdpa_offshore.shp")
# save(offshore_results,file="data/shapefile/offshore_results.RData")

wgmpas_wdpa_offshore <- read_sf("data/shapefile/wgmpas_wdpa_offshore.shp")

plot_bounds <- st_union(st_as_sfc(eu_bbox),wgmpas_wdpa_offshore%>%st_bbox()%>%st_as_sfc())%>%st_bbox()

p1 <- ggplot()+
  geom_sf(data=world)+
  geom_sf(data=wgmpas_wdpa_offshore,fill="cornflowerblue")+
  geom_sf(data=focal_lims%>%st_as_sfc(),fill=NA,linewidth=1.1)+
  coord_sf(xlim=plot_bounds[c(1,3)],ylim=plot_bounds[c(2,4)],expand=0)+
  theme_bw()

#zoom in of the study area

focal_sites <- wgmpas_wdpa_offshore%>%
  filter(WDPAID %in% c(555691638,555557108,555557049,555590816,555770434))

focal_buffer <- focal_sites%>%
                st_transform(utm_focal)%>%
                st_buffer(10*1000)%>%
                st_transform(latlong)

buffer_outline <- focal_buffer %>%
  st_union() %>%  # Combine all buffer polygons
  st_difference(st_union(focal_sites)) # Remove the original sites

csquares <- create_csquares_grid(focal_lims)

csquares_sites <- csquares[focal_sites, , op = st_within] %>%
  mutate(zone = "site")

csquares_buffer <- csquares[buffer_outline, , op = st_within] %>%
  mutate(zone = "buffer")

csquares_all <- rbind(csquares_sites, csquares_buffer)

p2 <- ggplot() +
  geom_sf(data = europe)+
  #geom_sf(data = wgmpas_wdpa_offshore,fill="cornflowerblue",alpha=0.2)+
  geom_sf(data = csquares_all, aes(fill = zone), alpha = 0.6) +
  geom_sf(data = focal_sites, fill = "cornflowerblue", color = "black",linewidth=1.1, alpha=0.5) +
  geom_sf(data = buffer_outline, fill = "orange", alpha=0.5) +
  theme_bw()+
  coord_sf(xlim=focal_lims[c(1,3)],ylim=focal_lims[c(2,4)])+
  theme(legend.position = "none",
        axis.text = element_blank())+
  annotation_scale(location="br")

combo_plot <- p1 + p2 + plot_layout(nrow=2)

ggsave("output/offshore_mpa_ex.png",height=8,width=5,units="in",dpi=300)

