###############################################################################
# Create shapefile that splits EVIMI and WVI
# April 16, 2025
###############################################################################
library(sf)
library(dplyr)

Dropbox_directory <- "/Users/stephaniepeacock/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/"

pse_regions <- st_read(paste0(Dropbox_directory, "data-gis/se_boundary_regions/se_boundary_regions.shp")) %>% 
	st_transform(crs = 4269)
											 
plot(pse_regions)

cu_boundaries <- st_read(paste0(Dropbox_directory, "data-gis/pse_conservation_units/pse_conservation_units.gdb")) %>% 
	st_transform(crs = 4269)

vimi_cus <- cu_boundaries %>%
	filter(region == "Vancouver Island & Mainland Inlets", species == "River sockeye")

plot(vimi_cus)	
unique(vimi_cus$CU_NAME)

wvi <- st_union(vimi_cus %>% filter(CU_NAME %in% c("West Vancouver Island", "NW Vancouver Island")))
evimi <- st_union(vimi_cus %>% filter(CU_NAME %in% c("East Vancouver Island & Georgia Strait", "Southern Fjords")))

plot(st_geometry(wvi))
plot(st_geometry(evimi), add = TRUE, border = 2)

plot(st_geometry(pse_regions), xlim = c(-125, -118), ylim = c(48, 54), col = rainbow(n = 10)[c(1,5,8,7,2,9,3,6,4)])
plot(st_geometry(wvi), add =TRUE, border = 2)
plot(st_geometry(evimi), add = TRUE, border = 4)

pse_regions2 <- bind_rows(pse_regions %>%
	filter(regionname != "Vancouver Island & Mainland Inlets"),
	data.frame(regionid = 55, regionname = "West Vancouver Island", geometry = wvi))

pse_regions3 <- bind_rows(
	pse_regions2,
	data.frame(regionid = 56, regionname =  "East Vancouver Island & Mainland Inlets", geometry = evimi))				

plot(pse_regions3)

saveRDS(pse_regions3, file = "data/ignore/pse-regions-2025/pse-regions-2025.rds")
