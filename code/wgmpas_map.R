### WGMPAs - who are we

#load libraries
library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggnewscale)

#projections ------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

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
  mutate(highlight = country_match %in% wgmpas_countries,
         highlight = ifelse(country_match == "United States",FALSE,highlight)) #the US doesn't really participate. 


#set up the plot
plot_lims <- st_bbox(world) 
plot_lims[1:4] <- c(-100,30,40,75)

plot_lims2 <- plot_lims
plot_lims2[2] <- 34

# Plot the map
p1 <- ggplot() +
  geom_sf(data = world,aes(fill = highlight), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "gray90"), name = "Highlight") 
  coord_sf(xlim = plot_lims[c(1,3)], ylim = plot_lims[c(2,4)])+
  theme_minimal() +
  labs(title = "Working Group on Marine Protected Areas and Other Spatial Conservation Measures",
       subtitle = "WGMPAs") +
  theme(legend.position = "none")

#WDPA data files (these are curated to only MARINE %in% c(1,2)) in wdpa_process.R

wgmpas_wdpa_mpa <- st_read("data/shapefile/wgmpas_wdpa_curated.shp", quiet = TRUE)%>%
               st_transform(latlong)%>%
               mutate(type="MPA")

wgmpas_wdpa_oecm <- st_read("data/shapefile/wgmpas_wdpa_curated_oecm.shp", quiet = TRUE)%>%
                    st_transform(latlong)%>%
                    mutate(type="OECM")

wgmpas_wdpa <- rbind(wgmpas_wdpa_mpa%>%dplyr::select("geometry","type"), wgmpas_wdpa_oecm%>%dplyr::select("geometry","type"))


p1 <- ggplot() +
  geom_sf(data = world,aes(fill = highlight), color = "black", size = 0.1,show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "darkslateblue", "FALSE" = "gray90"), name = "Highlight") +
  new_scale_fill()+
  geom_sf(data=wgmpas_wdpa,aes(fill=type)) +
  scale_fill_manual(values = c("MPA" = "cornflowerblue","OECM" = "#69b3a2"))+
  coord_sf(xlim = plot_lims2[c(1,3)], ylim = plot_lims2[c(2,4)])+
  theme_bw() +
  labs(title = "Working Group on Marine Protected Areas and Other Spatial Conservation Measures",
       subtitle = "WGMPAs")+
  theme(legend.position = "inside",
        legend.position.inside = c(0.08,0.15),
        legend.background = element_blank(),
        legend.title = element_blank())

ggsave("output/wgmpas_plot.png",p1,height=6,width=8,units="in",dpi=300)
