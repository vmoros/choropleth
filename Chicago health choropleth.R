# CA throughout means "Community Area"
library(maptools)
library(ggplot2)
library(ggmap)

health_choro <- function(column_name, zoom_level,CA_map = get_CA_map(),
        base_map = ggmap(get_googlemap(center = "Chicago", zoom = zoom_level)),
        save = T, show = F, low_color, high_color){
  # make copies of maps to avoid changing the originals
  CA_map_copy <- CA_map
  output_map <- base_map
  
  # add colored polygons corresponding to cancer data over the map
  output_map <- output_map + geom_polygon(aes_string(x="long", y="lat",
                  group="group",fill = column_name), data=CA_map_copy, alpha=.9)
  
  # add borders to the community areas
  output_map <- output_map + geom_path(aes(x=long, y=lat, group=group),
                                             data = CA_map, color = 'black')
  
  # change colors of map if colors are provided
  if (hasArg(low_color) && hasArg(high_color)){
    output_map <- output_map + scale_fill_gradient(low = low_color,
                                                   high = high_color)
  }
  
  
  if (save){
  # save plot to png file
  png(filename = paste0("plots/", column_name, " zoom ", toString(zoom_level),
                        ".png"), width = 960, height = 960, type = "cairo")
  plot(output_map)
  dev.off()
  print(paste0("I saved a map of ", column_name, "with a zoom level of ",
               toString(zoom_level)))
  }
  
  if (show){
    output_map
    print(paste0("Plotting map of ", toString(column_name)))
  }
}

make_all_choropleths <- function(zoom_level){
  # remove all old plots
  clear_plots_folder()
  
  # get the merged map and health data and get the names of the columns
  # excluding columns 1 through 8 because those are map data and CA names
  CA_map <- get_CA_map()
  columns <- names(CA_map)[-(1:8)]
  
  # get base map from google
  base_map <- ggmap(get_googlemap(center = "Chicago", zoom = zoom_level))
  
  # make choropleth of each column
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
  if (!dir.exists("plots")){
    dir.create("plots")
  } else {
  file.remove(list.files("plots", full.names = T))
  }
}

# to do: 
# put legend over water in map
# think of way to overlay two maps to compare them


# done:
# only pull base map once
# make ability to make map on demand
# put on github
# research get_googlemap function vs get_map
# change colors
# make parameters to specify if map should be shown or saved
# change direction of coloring