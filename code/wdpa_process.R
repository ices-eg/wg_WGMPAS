## Code to trim out the terrestial shapes from the WDPA data

#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)

#shapefile paths
shapefiles <- c("data/WDPA_Aug2025_Public_shp/WDPA_Aug2025_Public_shp_0/WDPA_Aug2025_Public_shp-polygons.shp",
                "data/WDPA_Aug2025_Public_shp/WDPA_Aug2025_Public_shp_1/WDPA_Aug2025_Public_shp-polygons.shp",
                "data/WDPA_Aug2025_Public_shp/WDPA_Aug2025_Public_shp_2/WDPA_Aug2025_Public_shp-polygons.shp")

for(i in shapefiles){
  
gpkgfile <- sub("\\.shp$", ".gpkg", i)

# 1. Convert to GPKG 
st_write(st_read(i, quiet=TRUE), gpkgfile, 
         layer = tools::file_path_sans_ext(basename(i))) #this takes a while

# 2. Get layer name
layers <- st_layers(gpkgfile)
layername <- layers$name[1] # or assign if >1 layers

# 3. Now filter on read so that the terrestrial areas aren't loading
marine <- st_read(gpkgfile, 
                  query = sprintf("SELECT * FROM \"%s\" WHERE MARINE IN (1,2)", layername), 
                  quiet = TRUE)%>%st_make_valid()

#now write the trimmed polygon layer
newpath <- gsub("_shp-polygons","_shp-polygons_marine",i)

st_write(marine,dsn=newpath,append = TRUE)

#remove loop specific layers
rm(marine,layers,layername,newpath,gpkgfile)

gc() #purge stuff in the memory

}

#now process to match the focal extent of a Northwest Atlantic map - note this will have to match that of plots using these outputs later

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(latlong)

plot_lims <- st_bbox(world)
plot_lims[1:4] <- c(-100, 30, 40, 75)  # [xmin, ymin, xmax, ymax]

# Convert bbox to sf polygon for intersection (still in latlong)
plot_box <- st_as_sfc(plot_lims)


#identify each of the shapefiles processed in the above script WDPA - marine shapes only
wdpa_files <- list.files("data/WDPA_Aug2025_Public_shp/", pattern = "_shp-polygons_marine.shp", 
                         recursive = TRUE, full.names = TRUE)


sf_use_s2(FALSE) #makes the overlap analysis faster, though slightly less accurate. but at this scale/applicaiton it doesn't matter
selected_features_list <- list() # to store results

# 3. For each shapefile: read, reproject plot extent, intersect
for (shp in wdpa_files) {
  
  message(paste0("Working on ",i))
  shp_data <- st_read(shp, quiet = TRUE)
  
  # Get this shapefile's proj, transform box to match
  shp_crs <- st_crs(shp_data)
  plot_box_proj <- st_transform(plot_box, crs = shp_crs)
  
  # Identify overlapping features (bounding-box fast test)
  overlaps <- st_intersects(shp_data, plot_box_proj, sparse = FALSE)[, 1]
  # st_intersects gives TRUE also for partial overlaps!
  
  filtered <- shp_data[overlaps, ]
  selected_features_list[[shp]] <- filtered
  cat(basename(shp), ": selected", nrow(filtered), "features overlapping plot_lims\n")
}

# If you want to combine all selected features into one sf object:
all_selected <- do.call(rbind, selected_features_list)

st_write(all_selected,dsn="data/shapefile/wgmpas_wdpa_curated.shp")

#OECMS 

#shapefile paths
oecmfiles <- list.files("data/WDOECM_Aug2025_Public_shp/", pattern = "shp-polygons.shp", 
                         recursive = TRUE, full.names = TRUE)

for(i in oecmfiles){
  
  gpkgfile <- sub("\\.shp$", ".gpkg", i)
  
  # 1. Convert to GPKG 
  st_write(st_read(i, quiet=TRUE), gpkgfile, 
           layer = tools::file_path_sans_ext(basename(i))) #this takes a while
  
  # 2. Get layer name
  layers <- st_layers(gpkgfile)
  layername <- layers$name[1] # or assign if >1 layers
  
  # 3. Now filter on read so that the terrestrial areas aren't loading
  marine <- st_read(gpkgfile, 
                    query = sprintf("SELECT * FROM \"%s\" WHERE MARINE IN (1,2)", layername), 
                    quiet = TRUE)%>%st_make_valid()
  
  #now write the trimmed polygon layer
  newpath <- gsub("_shp-polygons","_shp-polygons_marine",i)
  
  st_write(marine,dsn=newpath,append = TRUE)
  
  #remove loop specific layers
  rm(marine,layers,layername,newpath,gpkgfile)
  
  gc() #purge stuff in the memory
  
}


#filter to the map extent 
oecm_marine_files <- list.files("data/WDOECM_Aug2025_Public_shp/", pattern = "_shp-polygons_marine.shp", 
                         recursive = TRUE, full.names = TRUE)

sf_use_s2(FALSE) #makes the overlap analysis faster, though slightly less accurate. but at this scale/applicaiton it doesn't matter

selected_features_list_oecm <- list() # to store results

# 3. For each shapefile: read, reproject plot extent, intersect
for (shp in oecm_marine_files) {
  
  message(paste0("Working on ",i))
  shp_data <- st_read(shp, quiet = TRUE)
  
  # Get this shapefile's proj, transform box to match
  shp_crs <- st_crs(shp_data)
  plot_box_proj <- st_transform(plot_box, crs = shp_crs)
  
  # Identify overlapping features (bounding-box fast test)
  overlaps <- st_intersects(shp_data, plot_box_proj, sparse = FALSE)[, 1]
  # st_intersects gives TRUE also for partial overlaps!
  
  filtered <- shp_data[overlaps, ]
  selected_features_list_oecm[[shp]] <- filtered
  cat(basename(shp), ": selected", nrow(filtered), "features overlapping plot_lims\n")
}

# If you want to combine all selected features into one sf object:
all_selected_oecm <- do.call(rbind, selected_features_list_oecm)

st_write(all_selected_oecm,dsn="data/shapefile/wgmpas_wdpa_curated_oecm.shp")

