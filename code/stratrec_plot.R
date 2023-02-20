#load libaries ----
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)

#Projections ------------
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
stratrec_proj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs" #projection assocaited with ICES Stratrec shapefiles

#load shapeiles
ices_statrec <- read_sf("data/shapefile/ICES_SubStatrec_20150113_3857.shp")
ices_statrec_eco <- read_sf("data/shapefile/ICES_StatRec_mapto_Ecoregions.shp")

ices_basemap <- ne_countries(scale = "medium",
                       type = 'map_units',
                       returnclass = "sf")%>%
              st_transform(st_crs(ices_statrec_eco))%>%
              st_intersection(.,st_bbox(ices_statrec_eco)%>%st_as_sfc())


p1 <- ggplot()+
  geom_sf(data=ices_statrec_eco,aes(fill=Ecoregion))+
  geom_sf(data=ices_basemap)+
  theme_bw()+
  coord_sf(expand=0)
  
ggsave("output/ices_stratrec_ecoregion.png",p1,height=6,width=10,units="in",dpi=300)

