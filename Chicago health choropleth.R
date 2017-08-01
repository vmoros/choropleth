# CA throughout means "Community Area"
library(maptools)
library(ggplot2)
library(ggmap)

health_choro <- function(column_name, zoom_level,CA_map = get_CA_map(),
        base_map = ggmap(get_googlemap(center = "Chicago", zoom = zoom_level))){
  # make copies of maps to avoid changing the originals
  CA_map_copy <- CA_map
  output_map <- base_map
  
  # add colored polygons corresponding to cancer data over the map
  output_map <- output_map + geom_polygon(aes_string(x="long", y="lat",
                  group="group",fill = column_name), data=CA_map_copy, alpha=.9)
  
  # add borders to the community areas
  output_map <- output_map + geom_path(aes(x=long, y=lat, group=group),
                                             data = CA_map, color = 'black')

  # save plot to png file
  png(filename = paste0("plots/", column_name, " zoom ", toString(zoom_level),
                        ".png"), width = 960, height = 960, type = "cairo")
  plot(output_map)
  dev.off()
  print(paste0("Map of ", column_name, " made with zoom level of ",
               toString(zoom_level)))
}

make_all_choropleths <- function(zoom_level){
  # remove all old plots
  clear_plots_folder()
  
  # get health data and the names of columns in it
  chi_health <- read.csv("Health by CA.csv", fileEncoding = "UTF-8-BOM")
  columns <- names(chi_health)[-(1:2)]
  
  # read community area shape file, turn it into a data frame, and merge the
  # health data into it
  CA_map <- readShapePoly(fn = "CA_map/CA_map")
  CA_map <- fortify(CA_map, region = 'area_numbe')
  CA_map <- merge(CA_map, chi_health, by.x = "id", by.y = names(chi_health)[1])
  CA_map <- CA_map[order(CA_map$order),]
  
  # get base map from google
  base_map <- ggmap(get_googlemap(center = "Chicago", zoom = zoom_level))
  lapply(columns, function (x) health_choro(x, zoom_level, CA_map = CA_map,
                                            base_map = base_map))
}

get_CA_map <- function(){
  # get health data
  chi_health <- read.csv("Health by CA.csv", fileEncoding = "UTF-8-BOM")
  
  # read community area shape file, turn it into a data frame, and merge the
  # health data into it
  CA_map <- readShapePoly(fn = "CA_map/CA_map")
  CA_map <- fortify(CA_map, region = 'area_numbe')
  CA_map <- merge(CA_map, chi_health, by.x = "id", by.y = names(chi_health)[1])
  CA_map <- CA_map[order(CA_map$order),]
  return(CA_map)
}

clear_plots_folder <- function(){
  file.remove(list.files("plots", full.names = T))
}

# to do: 
# only pull base map once
# make parameter to specify if map should be shown or saved
# make ability to make map on demand
# change colors
# change direction of coloring
# put legend over water in map
# research get_googlemap function vs get_map
# think of way to overlay two maps to compare them
# put on github