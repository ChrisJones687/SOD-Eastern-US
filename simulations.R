library(raster)
library(sp)
library(lubridate)
library(PoPS)
library(rgdal)

# state_boundaries <- readOGR(dsn ="H:/Shared drives/APHIS  Projects/shared resources/data/usa_boundaries/us_lower_48_states.shp")
# state_boundaries <- spTransform(state_boundaries, CRSobj = crs(z))


infected_file <- "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/nurseries.tif"
host_file <- "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/hosts.tif"
total_plants_file <- "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/all_plants.tif"
temperature_file <- ""
temperature_coefficient_file <- "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/sudden_oak_death/Eastern USA/weather_coeff.tif"
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- FALSE
precip <- FALSE
season_month_start <- 1
season_month_end <- 12
time_step <- "week"
start_date <- '2019-01-01'
end_date <- '2022-12-31'
lethal_temperature <- -35
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 0.6
treatments_file <- ""
treatment_dates <- c('2019-12-24')
treatment_method <- "ratio"
management <- FALSE
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- c(29, 2)
anthropogenic_distance_scale <- 0.0
natural_dir <- "NONE"
natural_kappa <- 0
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0
pesticide_duration <- c(0)
pesticide_efficacy <- 1.0
random_seed = NULL
output_frequency = "year"
movements_file <- ""
use_movements <- FALSE
num_iterations <- 2
number_of_cores <- 10

# data <- PoPS::pops(infected_file, host_file, total_plants_file, 
#                    temp, temperature_coefficient_file, 
#                    precip, precipitation_coefficient_file, 
#                    time_step, reproductive_rate,
#                    season_month_start, season_month_end, 
#                    start_date, end_date, 
#                    use_lethal_temperature, temperature_file,
#                    lethal_temperature, lethal_temperature_month,
#                    mortality_on, mortality_rate, mortality_time_lag, 
#                    management, treatment_dates, treatments_file,
#                    treatment_method,
#                    percent_natural_dispersal,
#                    natural_kernel_type, anthropogenic_kernel_type,
#                    natural_distance_scale, anthropogenic_distance_scale,
#                    natural_dir, natural_kappa, 
#                    anthropogenic_dir, anthropogenic_kappa,
#                    pesticide_duration, pesticide_efficacy,
#                    random_seed, output_frequency,
#                    movements_file, use_movements)

data <- PoPS::pops_multirun(infected_file, host_file, total_plants_file, 
                            temp, temperature_coefficient_file, 
                            precip, precipitation_coefficient_file, 
                            time_step, reproductive_rate,
                            season_month_start, season_month_end, 
                            start_date, end_date, 
                            use_lethal_temperature, temperature_file,
                            lethal_temperature, lethal_temperature_month,
                            mortality_on, mortality_rate, mortality_time_lag, 
                            management, treatment_dates, treatments_file,
                            treatment_method,
                            percent_natural_dispersal,
                            natural_kernel_type, anthropogenic_kernel_type,
                            natural_distance_scale, anthropogenic_distance_scale,
                            natural_dir, natural_kappa, 
                            anthropogenic_dir, anthropogenic_kappa,
                            num_iterations, number_of_cores,
                            pesticide_duration, pesticide_efficacy,
                            random_seed = NULL, output_frequency,
                            movements_file, use_movements)
