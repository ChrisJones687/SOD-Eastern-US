
## prepare initial infection data from safegraph data
library(sp)
library(rgdal)
library(rgeos)
library(folderfun)


folderfun::setff("In", "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/SafegraphData")
folderfun::setff("Out", "H:/Shared drives/Data/Vector/USA")
dirs = list.dirs("H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/SafegraphData", full.names = TRUE)
np_folders <- dirs[2:34]
wm_folders <- dirs[37:69]

nursery_points <- read.csv(ffIn("/444220-424930-482111-CORE_POI-PATTERNS-2017_01-2019-10-23/core_poi-patterns.csv"))
nursery_points <- nursery_points[,c(1,3,6:11,17:20,23:26)]

wm_points <- read.csv(ffIn("/Lowes-Walmart-TheHomeDepot-CORE_POI-PATTERNS-2017_01-2019-10-23/core_poi-patterns.csv"))
wm_points <- wm_points[,c(1,3,6:11,17:20,23:26)]

date_range <- c("Visits_2017_01", "Visitors_2017_01", "mean_distance_from_home_2017_01",
                "Visits_2017_02", "Visitors_2017_02", "mean_distance_from_home_2017_02", 
                "Visits_2017_03", "Visitors_2017_03", "mean_distance_from_home_2017_03", 
                "Visits_2017_04", "Visitors_2017_04", "mean_distance_from_home_2017_04",
                "Visits_2017_05", "Visitors_2017_05", "mean_distance_from_home_2017_05", 
                "Visits_2017_06", "Visitors_2017_06", "mean_distance_from_home_2017_06", 
                "Visits_2017_07", "Visitors_2017_07", "mean_distance_from_home_2017_07", 
                "Visits_2017_08", "Visitors_2017_08", "mean_distance_from_home_2017_08",
                "Visits_2017_09", "Visitors_2017_09", "mean_distance_from_home_2017_09", 
                "Visits_2017_10", "Visitors_2017_10", "mean_distance_from_home_2017_10", 
                "Visits_2017_11", "Visitors_2017_11", "mean_distance_from_home_2017_11", 
                "Visits_2017_12", "Visitors_2017_12", "mean_distance_from_home_2017_12",
                "Visits_2018_01", "Visitors_2018_01", "mean_distance_from_home_2018_01", 
                "Visits_2018_02", "Visitors_2018_02", "mean_distance_from_home_2018_02", 
                "Visits_2018_03", "Visitors_2018_03", "mean_distance_from_home_2018_03", 
                "Visits_2018_04", "Visitors_2018_04", "mean_distance_from_home_2018_04",
                "Visits_2018_05", "Visitors_2018_05", "mean_distance_from_home_2018_05", 
                "Visits_2018_06", "Visitors_2018_06", "mean_distance_from_home_2018_06", 
                "Visits_2018_07", "Visitors_2018_07", "mean_distance_from_home_2018_07", 
                "Visits_2018_08", "Visitors_2018_08", "mean_distance_from_home_2018_08",
                "Visits_2018_09", "Visitors_2018_09", "mean_distance_from_home_2018_09", 
                "Visits_2018_10", "Visitors_2018_10", "mean_distance_from_home_2018_10", 
                "Visits_2018_11", "Visitors_2018_11", "mean_distance_from_home_2018_11", 
                "Visits_2018_12", "Visitors_2018_12", "mean_distance_from_home_2018_12",
                "Visits_2019_01", "Visitors_2019_01", "mean_distance_from_home_2019_01", 
                "Visits_2019_02", "Visitors_2019_02", "mean_distance_from_home_2019_02", 
                "Visits_2019_03", "Visitors_2019_03", "mean_distance_from_home_2019_03", 
                "Visits_2019_04", "Visitors_2019_04", "mean_distance_from_home_2019_04",
                "Visits_2019_05", "Visitors_2019_05", "mean_distance_from_home_2019_05", 
                "Visits_2019_06", "Visitors_2019_06", "mean_distance_from_home_2019_06", 
                "Visits_2019_07", "Visitors_2019_07", "mean_distance_from_home_2019_07", 
                "Visits_2019_08", "Visitors_2019_08", "mean_distance_from_home_2019_08",
                "Visits_2019_09", "Visitors_2019_09", "mean_distance_from_home_2019_09")

col_index <- length(nursery_points) + 1
col_index2 <- col_index + 2

for (i in 1:length(np_folders)) {
  # np_files = list.files(path = np_folders[i], pattern="core_poi-patterns.csv", full.names = TRUE)
  wm_files = list.files(path = wm_folders[i], pattern="core_poi-patterns.csv", full.names = TRUE)
  # np <- read.csv(np_files[1])
  wm <- read.csv(wm_files[1])
  # nursery_points[, col_index:col_index2] <- np[, c(25:26,31)]
  wm_points[, col_index:col_index2] <- wm[, c(25:26,31)]
  col_index <- col_index + 3
  col_index2 <- col_index + 2
  print(i)
}

all_points <- rbind(nursery_points, wm_points)
naming <- names(all_points)
naming <- c(naming[1:16], date_range)
names(all_points) <- naming

all_locations <- all_points[, c(2,4,6:12, 17:length(all_points))]
rails <- all_locations[all_locations$sub_category == 'Line-Haul Railroads', ]
nuseries <- all_locations[all_locations$sub_category != 'Line-Haul Railroads', ]

nurseries <- SpatialPointsDataFrame(nuseries[,c(4,3)], nuseries, proj4string = crs("+proj=longlat +datum=WGS84 +no_defs"))
railways <- SpatialPointsDataFrame(rails[,c(4,3)], rails, proj4string = crs("+proj=longlat +datum=WGS84 +no_defs"))

plot(host)
plot(railways)

nurseries$count <- 1
# nurseries2 <- coordinates(nurseries)
# nurseries2 <- SpatialPointsDataFrame(coords = nurseries2, data = nurseries@data, proj4string = crs(nurseries))
# nurseries2 <- gCentroid(nurseries, byid = TRUE)
nurseries_rast  <- rasterize(nurseries, weather_average, fun = 'count')
nurseries_c <- nurseries_rast$count
writeOGR(obj=nurseries, dsn="H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA", layer="nurseries_w_distance", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=railways, dsn="H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA", layer="rail_stations", driver="ESRI Shapefile") # this is in geographical projection

## write as geopackage
writeOGR(obj=nurseries, dsn=ffOut("nurseries.gpkg"), layer="nurseries", driver="GPKG") # this is in geographical projection
writeOGR(obj=railways, dsn=ffOut("rail_stations.gpkg"), layer="rail_stations", driver="GPKG") # this is in geographical projection
