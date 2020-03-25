library(raster)
library(readr)
library(rgdal)
files = list.files(path = "H:/Shared drives/APHIS  Projects/PoPS/FIA_raster_data/", pattern=".img", full.names = TRUE)
dlist <- lapply(files, raster)
summary_table <- read.csv("H:/Shared drives/APHIS  Projects/PoPS/FIA_raster_data/summary_table_all_mod.csv", header = TRUE)
species_codes <- summary_table[1:324, 1:4]
code_order <- parse_number(files)
red_oak_codes <- c(812,833)
magnolia_codes <- c(650,651,652,653,654,655,658)
maple_codes <- c(315)
red_oaks_list <- c()
red_oaks <- c()
ro <- 1
magnolias <- c()
magnolia_list <- c()
mag <- 1
maples <- c()
maple_list <- c()
map <- 1

for (i in 1:length(dlist)){
  if (code_order[i] %in% red_oak_codes) {
    # roaks <- dlist[[i]]
    # roaks[roaks <= minValue(roaks)] <- NA
    red_oaks[ro] <- dlist[i]
    red_oaks_list[ro] <- code_order[i]
    ro <- ro + 1
  } else if (code_order[i] %in% magnolia_codes) {
    magnolias[mag] <- dlist[i]
    magnolia_list[mag] <- code_order[i]
    mag <- mag + 1
  }else if (code_order[i] %in% maple_codes) {
    maples[map] <- dlist[i]
    maple_list[map] <- code_order[i]
    map <- map + 1
  }
}

red_oaks_all <- sum(stack(red_oaks))
magnolias_all <- sum(stack(magnolias))
maples_all <- sum(stack(maples))
all_biomass <- sum(stack(dlist))

red_oaks_all[red_oaks_all <= minValue(red_oaks_all)] <- NA
magnolias_all[magnolias_all <= minValue(magnolias_all)] <- NA
maples_all[maples_all <= minValue(maples_all)] <- NA
plot(red_oaks_all)
plot(magnolias_all)
plot(maples_all)

writeRaster(red_oaks_all, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/red_oaks.tif')
writeRaster(magnolias_all, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/magnolias.tif')
writeRaster(maples_all, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/maples.tif')

oaks <- raster('H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/red_oaks.tif')
magnolias <- raster('H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/magnolias.tif')
maples <- raster('H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/maples.tif')
oaks[is.na(oaks)] <- 0
magnolias[is.na(magnolias)] <- 0
maples[is.na(maples)] <- 0

rankings <- data.frame(species = c('oaks', 'maples', 'magnolias'), 
                       map_h_mag_l = c(0, 1.0, 0.1), map_m_mag_l = c(0, 0.5, 0.1), map_l_mag_l = c(0, 0.1, 0.1), 
                       map_h_mag_m = c(0, 1.0, 0.5), map_m_mag_m = c(0, 0.5, 0.5), map_l_mag_m = c(0, 0.1, 0.5),
                       map_h_mag_h = c(0, 1.0, 1.0), map_m_mag_h = c(0, 0.5, 1.0), map_l_mag_h = c(0, 0.1, 1.0))

all_hosts <- oaks + magnolias + maples
all_max <- maxValue(all_hosts)
all_hosts_mod <- ceiling(all_hosts/all_max * 100)
plot(all_hosts_mod)

host_maps <- stack(all_hosts_mod)

for (i in 2:length(rankings)) {
  host <- ceiling(((oaks * rankings[rankings$species == "oaks", i]) + (maples * rankings[rankings$species == "maples", i]) + (magnolias * rankings[rankings$species == "magnolias", i]))/all_max * 100)
  host_maps <- stack(host_maps, host)
}

## prepare temp and precip data
temp_coeff_14 <- stack("H:/My Drive/Folder/temp_coeff_eusa_14.tif")
temp_coeff_15 <- stack("H:/My Drive/Folder/temp_coeff_eusa_15.tif")
temp_coeff_16 <- stack("H:/My Drive/Folder/temp_coeff_eusa_16.tif")
temp_coeff_17 <- stack("H:/My Drive/Folder/temp_coeff_eusa_17.tif")
temp_coeff_18 <- stack("H:/My Drive/Folder/temp_coeff_eusa_18.tif")
prcp_coeff_14 <- stack("H:/My Drive/Folder/prcp_coeff_eusa_14.tif")
prcp_coeff_15 <- stack("H:/My Drive/Folder/prcp_coeff_eusa_15.tif")
prcp_coeff_16 <- stack("H:/My Drive/Folder/prcp_coeff_eusa_16.tif")
prcp_coeff_17 <- stack("H:/My Drive/Folder/prcp_coeff_eusa_17.tif")
prcp_coeff_18 <- stack("H:/My Drive/Folder/prcp_coeff_eusa_18.tif")

temp_coeff <- stack(temp_coeff_14, temp_coeff_15, temp_coeff_16, temp_coeff_17, temp_coeff_18)
prcp_coeff <- stack(prcp_coeff_14, prcp_coeff_15, prcp_coeff_16, prcp_coeff_17, prcp_coeff_18)

weather_coeff <- temp_coeff[[1]] * prcp_coeff[[1]]
weather_coeff <- stack(weather_coeff)

for (i in 2:nlayers(temp_coeff)) {
  weather <- temp_coeff[[i]] * prcp_coeff[[i]]
  weather_coeff <- stack(weather_coeff, weather)
  print(i)
}

writeRaster(prcp_coeff, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/prcp_coeff.tif')
writeRaster(temp_coeff, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/temp_coeff.tif')
writeRaster(weather_coeff, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/weather_coef.tif', overwrite = TRUE)

hosts <- projectRaster(host_maps, weather_coeff)
writeRaster(hosts, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/hosts.tif')
all_plants <- weather_coeff[[1]]
all_plants[weather_coeff[[1]] >= 0] <- 100
writeRaster(all_plants, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/all_plants.tif')


weather_average <- calc(weather_coefficient_stack, fun = mean, na.rm = TRUE)
eusa <- readOGR("H:/Shared drives/APHIS  Projects/SOD eastern US/Eusa_states.shp")
eusa <- spTransform(eusa, crs(weather_average))

png(paste("H:/Shared drives/APHIS  Projects/SOD eastern US/Figures/Average_Weather_Coefficient_Total.png", sep = ""), width=600, height=500, res=120) # start export
  temp_plot <- plot(weather_average, main = paste("Average Weather Coefficient Year"))
  temp_plot <- plot(eusa, add = TRUE)
  print(temp_plot) 
dev.off() # finish export

rcl <- c(0, 0.18, 1,
         0.18, 0.22, 2,
         0.22, 0.45, 3,
         0.45, 1, 4)
rclmat <- matrix(rcl, ncol=3, byrow=TRUE)
weather_risk <- reclassify(weather_average, rclmat)
weather_risk[hosts[[8]] < 100] <- NA
plot(weather_risk, col = c("transparent", "green", "yellow", "red"))
plot(eusa, add = TRUE)

## prepare initial infection data from safegraph data
library(sp)
library(rgdal)
library(rgeos)
dirs = list.dirs("C:/Users/cmjone25/Desktop/SafegraphData", full.names = TRUE)
np_folders <- dirs[2:34]
wm_folders <- dirs[37:69]

nursery_points <- read.csv("C:/Users/cmjone25/Desktop/SafegraphData/444220-424930-482111-CORE_POI-PATTERNS-2017_01-2019-10-23/core_poi-patterns.csv")
nursery_points <- nursery_points[,c(1,3,6:11,17:20,23:26)]

wm_points <- read.csv("C:/Users/cmjone25/Desktop/SafegraphData/Lowes-Walmart-TheHomeDepot-CORE_POI-PATTERNS-2017_01-2019-10-23/core_poi-patterns.csv")
wm_points <- wm_points[,c(1,3,6:11,17:20,23:26)]

date_range <- c("Visits_2017_01", "Visitors_2017_01", "Visits_2017_02", "Visitors_2017_02", 
                "Visits_2017_03", "Visitors_2017_03", "Visits_2017_04", "Visitors_2017_04",
                "Visits_2017_05", "Visitors_2017_05", "Visits_2017_06", "Visitors_2017_06", 
                "Visits_2017_07", "Visitors_2017_07", "Visits_2017_08", "Visitors_2017_08",
                "Visits_2017_09", "Visitors_2017_09", "Visits_2017_10", "Visitors_2017_10", 
                "Visits_2017_11", "Visitors_2017_11", "Visits_2017_12", "Visitors_2017_12",
                "Visits_2018_01", "Visitors_2018_01", "Visits_2018_02", "Visitors_2018_02", 
                "Visits_2018_03", "Visitors_2018_03", "Visits_2018_04", "Visitors_2018_04",
                "Visits_2018_05", "Visitors_2018_05", "Visits_2018_06", "Visitors_2018_06", 
                "Visits_2018_07", "Visitors_2018_07", "Visits_2018_08", "Visitors_2018_08",
                "Visits_2018_09", "Visitors_2018_09", "Visits_2018_10", "Visitors_2018_10", 
                "Visits_2018_11", "Visitors_2018_11", "Visits_2018_12", "Visitors_2018_12",
                "Visits_2019_01", "Visitors_2019_01", "Visits_2019_02", "Visitors_2019_02", 
                "Visits_2019_03", "Visitors_2019_03", "Visits_2019_04", "Visitors_2019_04",
                "Visits_2019_05", "Visitors_2019_05", "Visits_2019_06", "Visitors_2019_06", 
                "Visits_2019_07", "Visitors_2019_07", "Visits_2019_08", "Visitors_2019_08",
                "Visits_2019_09", "Visitors_2019_09")

col_index <- length(nursery_points) + 1
col_index2 <- col_index + 1
for (i in 1:length(np_folders)) {
  np_files = list.files(path = np_folders[i], pattern="core_poi-patterns.csv", full.names = TRUE)
  wm_files = list.files(path = wm_folders[i], pattern="core_poi-patterns.csv", full.names = TRUE)
  np <- read.csv(np_files[1])
  wm <- read.csv(wm_files[1])
  nursery_points[, col_index:col_index2] <- np[, 25:26]
  wm_points[, col_index:col_index2] <- wm[, 25:26]
  col_index <- col_index + 2
  col_index2 <- col_index + 1
}

all_points <- rbind(nursery_points, wm_points)
naming <- names(all_points)
naming <- c(naming[1:16], date_range)
names(all_points) <- naming

all_nursuries <- all_points[, c(2,4,6:12, 17:length(all_points))]
rails <- all_nursuries[all_nursuries$sub_category == 'Line-Haul Railroads', ]
a_nuseries <- all_nursuries[all_nursuries$sub_category != 'Line-Haul Railroads', ]

nurseries <- SpatialPointsDataFrame(a_nuseries[,c(4,3)], a_nuseries, proj4string = crs(host))
railways <- SpatialPointsDataFrame(rails[,c(4,3)], rails, proj4string = crs(host))

plot(host)
plot(railways, add= TRUE)

nurseries$count <- 1
# nurseries2 <- coordinates(nurseries)
# nurseries2 <- SpatialPointsDataFrame(coords = nurseries2, data = nurseries@data, proj4string = crs(nurseries))
# nurseries2 <- gCentroid(nurseries, byid = TRUE)
nurseries_rast  <- rasterize(nurseries, weather_average, fun = 'count')
nurseries_c <- nurseries_rast$count
writeOGR(obj=nurseries, dsn="H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA", layer="nurseries", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=railways, dsn="H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA", layer="railways", driver="ESRI Shapefile") # this is in geographical projection

writeRaster(nurseries_c, 'H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/nurseries.tif')


## download iNaturalist Data for species of interest
library(spocc)
library(rinat)
species_of_interest <- "Rhododendrons and Azaleas"
extent <- c(24.396308,-124.848974, 49.384358, -66.885444) # bounding box for lower 48 states
inat_rhododendrons <- get_inat_obs(taxon_name = species_of_interest, maxresults = 40000, geo = TRUE, bounds = extent, quality = 'research')
