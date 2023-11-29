###############################################################################
# Code to summarize rivers and populations within each region
#
# Request from Eileen to Marc and Steph Nov 22, 2023
###############################################################################
library(sf)
library(dplyr)

###############################################################################
# Q1. Area of regions
###############################################################################

# regions_spat <- st_read("data/ignore/pse-regions/se_boundary_regions_simple.shp")

# Use unsimplified boundaries
regions_spat <- st_read("../../../../../5_DATA/Mapping/study areas/shapefiles/PSE_regions/se_boundary_regions.shp")
sf_use_s2(FALSE)

area.out <- data.frame(
	region = regions_spat$Region,
	area_m2 =	st_area(regions_spat)
)

write.csv(area.out, file = "output/regionsQ1.csv")

###############################################################################
# Q2. Area of regions
###############################################################################

# 1. How much habitat is available for spawning and rearing salmon? What percentage of the region is salmon habitat?
# square km of spawning/rearing ZOIs
# km of rivers and streams

spawn_sp <- st_read("../../../../../5_DATA/Mapping/PSE/se_spwng_zoi_wspecies.shp")

spawn_region <- spawn_sp %>% 
	group_by(regionname) %>% 
	summarize()

sf_use_s2(FALSE)

plot(st_geometry(spawn_sp[which(spawn_sp$regionname == "Skeena"),]))
plot(st_geometry(spawn_region), border = 2, add = TRUE)

zoi.area.out <- data.frame(
	region = spawn_region$regionname,
	area_m2 =	st_area(spawn_region)
)

write.csv(zoi.area.out, file = "output/regionsQ2.csv")

###############################################################################
# Q3. Which rivers are major producers of salmon?
###############################################################################

spawner_surveys <- read.csv("data/spawner_surveys.csv", na.strings = -989898) %>%
	subset(!is.na(spawner_surveys$stream_observed_count))

# Create variable with more general species designations (not even/odd, lake/river)
spawner_surveys$species_pooled <- spawner_surveys$species_name
spawner_surveys$species_pooled[spawner_surveys$species_name %in% c("Pink (odd)", "Pink (even)")] <- "Pink"
spawner_surveys$species_pooled[spawner_surveys$species_name %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"

# **Each streamid is unique to each CU-locations combination, which separates species qualified***
# Create a variable for each unique pooled species + location combination -> pooled_pop
spawner_surveys$pooled_pop <- paste(spawner_surveys$latitude, spawner_surveys$longitude, spawner_surveys$species_pooled)

# There are some locations that are duplicated among regions...need to assign these based on lat/lon rather than CU
uniqueObs <- paste(spawner_surveys$latitude, spawner_surveys$longitude, spawner_surveys$year, spawner_surveys$species_name, spawner_surveys$stream_observed_count)

duplicates <- which(tapply(uniqueObs, uniqueObs, length) > 1)
length(duplicates) #91

# Calculate average abundance for these pooled_pops
avgS <- data.frame(
	region = tapply(spawner_surveys$region, spawner_surveys$pooled_pop, unique),
	stream_name_pse = tapply(spawner_surveys$stream_name_pse, spawner_surveys$pooled_pop, unique),
	species = tapply(spawner_surveys$species_pooled, spawner_surveys$pooled_pop, unique),
	avg_spawners = tapply(spawner_surveys$stream_observed_count, spawner_surveys$pooled_pop, mean, na.rm = TRUE),
		n_years = tapply(spawner_surveys$stream_observed_count, spawner_surveys$pooled_pop, function(x){sum(!is.na(x))})
)

rownames(avgS) <- NULL
dim(avgS) # 6140

# Remove streams with <10 years of data
avgS <- avgS[which(avgS$n_years >= 10), ] # 3700

# Create region-stream_name_pse variable (stream names are duplicated in some regions)
avgS$region_stream <- paste(avgS$region, avgS$stream_name_pse, sep = "-")

# Sum across species for each region-stream_name
spSum <- cbind(
	region = tapply(avgS$region, avgS$region_stream, unique),
	stream_name_pse = tapply(avgS$stream_name_pse, avgS$region_stream, unique),
	sppSum = tapply(avgS$avg_spawners, avgS$region_stream, sum, na.rm = TRUE)
)

write.csv(spSum, "output/regionsQ3.csv")

###############################################################################
# Q4. Number of CUs
###############################################################################

cu <- read.csv("data/conservationunits_decoder.csv")
cu$species_pooled <- cu$species_name
cu$species_pooled[cu$species_name %in% c("Pink (odd)", "Pink (even)")] <- "Pink"
cu$species_pooled[cu$species_name %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"

n_CUs <- data.frame(
	region = regions,
	chinook = rep(NA, length(regions)),
	chum = rep(NA, length(regions)),
	coho = rep(NA, length(regions)),
	pink = rep(NA, length(regions)),
	sockeye = rep(NA, length(regions)),
	steelhead = rep(NA, length(regions))
)

for(r in 1:length(regions)){
	for(s in 1:6){
		n_CUs[r, s + 1] <- length(unique(cu$pooledcuid[cu$region == regions[r] & cu$species_pooled == c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")[s]]))
	}}

write.csv(n_CUs, file= "output/regionsQ4.csv")

###############################################################################
# Q4. Number of stream survey "populations"
###############################################################################

ss <- read.csv("data/spawner_surveys.csv")

# Create variable with more general species designations (not even/odd, lake/river)
ss$species_pooled <- ss$species_name
ss$species_pooled[ss$species_name %in% c("Pink (odd)", "Pink (even)")] <- "Pink"
ss$species_pooled[ss$species_name %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"

regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

n_pops <- data.frame(
	region = regions,
	chinook = rep(NA, length(regions)),
	chum = rep(NA, length(regions)),
	coho = rep(NA, length(regions)),
	pink = rep(NA, length(regions)),
	sockeye = rep(NA, length(regions)),
	steelhead = rep(NA, length(regions))
)

for(r in 1:length(regions)){
	for(s in 1:6){
		n_pops[r, s + 1] <- length(unique(ss$streamid[ss$region == regions[r] & ss$species_pooled == c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")[s]]))
	}}

write.csv(n_pops, file= "output/regionsQ4_pops.csv")
