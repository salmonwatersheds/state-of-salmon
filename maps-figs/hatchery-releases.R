###############################################################################
# Map of hatchery releases

library(sf)
library(dplyr)

###############################################################################
# Map base layers
###############################################################################
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")
XDrive <- get_XDrive()

# Set root for spatial datasets
dat_root <- paste0(XDrive, "1_PROJECTS/1_Active/Climate Change/Data & Analysis/ccva/freshwater/data/spatial/")

#------------------------------------------------------------------------------
# Lakes, rivers, and shorelines
#------------------------------------------------------------------------------

lakes <- readRDS("maps-figs/waterbodies_lowRes_Can.rds")
rivers <- readRDS("maps-figs/watercourse_lowRes_Can.rds")
shoreline <- st_read(dsn = paste0(dat_root, "layers/GSHHS_i_L1.shp"))

# BC Yuk border
BCYuk <- readRDS("/Users/stephaniepeacock/Documents/Code/mapping/layers/BCYuk_lowRes.rds")

###############################################################################
# Hatchery data
###############################################################################

hatch <- st_read("data/ignore/se_hatcheryfacilities/se_hatcheryfacilities.shp")
head(hatch)

plot(st_geometry(hatch), col = as.factor(hatch$program))
plot(st_geometry(shoreline), add = TRUE)
plot(st_geometry(rivers0), add = TRUE)

# Hatchery releases
rel <- st_read("data/ignore/se_hatcheryreleasesites/se_hatcheryreleasesites.shp") # Don't use spatial release sites; hard to match

head(rel)

rel_dat <- read.csv("data/ignore/dataset384_hatchery_releases.csv")

fac <- readxl::read_xlsx(paste0(XDrive, "1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/hatchery-releases/output/archive/SWP_hatchery_data_2026-06-02.xlsx"), sheet = "DataEntry_facilities")

rel_dat <- readxl::read_xlsx(paste0(XDrive, "1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/hatchery-releases/output/archive/SWP_hatchery_data_2026-06-02.xlsx"), sheet = "DataEntry_releases") %>%
	rename("year" = "release_date") %>%
	mutate(unique_site = paste(release_site_latitude, release_site_longitude)) %>%
	mutate(locationid = as.numeric(as.factor(unique_site))) %>%
	mutate(channel = grepl("Chan", release_stage))

# Create spatial points for releases
rel_pts <- rel_dat %>% 
	mutate(recent_releases = max(year > 2015)) %>%
	dplyr::select(release_site_name, locationid, release_site_latitude, release_site_longitude, recent_releases) %>%
	distinct(locationid, .keep_all = TRUE) %>%
	rename("latitude" = "release_site_latitude",
				 "longitude" = "release_site_longitude"
				 ) %>%
	
	st_as_sf(
	coords = c("longitude", "latitude"),  # column names, X (lon) first, then Y (lat)
	crs = 4326                            # EPSG 4326 = WGS84, standard for lat/lon
)


	
head(rel_dat)

unique(rel_dat$release_type_pse)

# Are location_name_pse unqiue?
length(unique(rel_dat$location_name_pse))
length(unique(paste(rel_dat$location_name_pse, rel_dat$locationid))) #no...


rel_summ <- rel_dat %>% 
	filter(year > 2015) %>%
	group_by(paste(locationid, channel)) %>%
	summarise(avg_release = mean(total_release),
						release_site_name = unique(release_site_name)[1],
						release_stage = paste(unique(release_stage), collapse = ", "), 
						channel = unique(channel),
						species_name = paste(unique(species_name), collapse = ", "),
						locationid = unique(locationid)) %>%
	arrange(1/avg_release)
write.csv(rel_summ, "maps/release_summary_2016-2025.csv", row.names = FALSE)

# Points to emphasize
locationid_emph <- c(
	1246, #Fulton channel	
	1164, # Nadina River
)

ind <- which(rel_dat$locationid == 1246 & rel_dat$year > 2015) # Fulton channel
ind <- which(rel_dat$locationid == 1164 & rel_dat$year >= 2015) # Fulton channel
ind <- which(rel_dat$locationid == 50 & rel_dat$year > 2015) # Fulton channel

if(length(unique(rel_dat$species_name[ind])) > 1){
	print(tapply(rel_dat$total_release[ind], rel_dat$species_name[ind], mean))
} else {
	print(paste(unique(rel_dat$species_name[ind]), mean(rel_dat$total_release[ind])))
}


hist(log(rel_summ$avg_release))

rel_summ$locationid %in% rel$locationid


rel <- rel %>% left_join(rel_summ)
# Location ids for ?1M releases and not channe;
chan_loc <- rel_summ$locationid[which(rel_summ$channel == TRUE & rel_summ$avg_release >= 1e6)]
rel_loc <- rel_summ$locationid[which(rel_summ$channel == FALSE & rel_summ$avg_release >= 1e6)]

# Labels
rel_coords <- st_coordinates(rel_pts[which(rel_pts$locationid %in% rel_loc), ])

svg("hatchery_releases_wide.svg", width = 10, height = 6)
png("maps-figs/hatchery_releases_wide.png", width = 10, height = 6, units = "in", res = 300)
par(bg = "#EBF8F9")
plot(st_geometry(rel_pts), pch = 1, col = NA, bg = "#EBF8F9", xlim = c(-138, -110), ylim = c(45, 61))
plot(st_geometry(shoreline), col = "#CED0CD", border = "#86888F", add = TRUE)
plot(st_geometry(BCYuk), col = NA, border = "#86888F", add = TRUE)
plot(st_geometry(lakes), col = "#EBF8F9", border = "#86888F60", add =TRUE)
plot(st_geometry(rivers),  col = "#86888F60", add =TRUE)

plot(st_geometry(rel_pts), pch = 21, bg = "#97c8c8", col = "#00000060", add = TRUE)
plot(st_geometry(rel_pts %>% filter(locationid %in% chan_loc)), pch = 21, bg = "#9D9692", col = "#000000", add = TRUE)
plot(st_geometry(rel_pts %>% filter(locationid %in% rel_loc)), pch = 21, bg = "#0EA1A0", col = "#000000", add = TRUE, cex = 1.2)
# 
# # Add labels
# text(
# 	x      = rel_coords[, "X"],
# 	y      = rel_coords[, "Y"],
# 	labels = 1:length(rel_loc),#rel_pts$release_site_name[which(rel_summ$channel == FALSE & rel_summ$avg_release >= 1e6)],
# 	cex    = 0.8,    # text size
# 	pos    = c(4       # position: 1=below, 2=left, 3=above, 4=right
# )


# legend("topright", pch = 21, pt.bg= c("#97c8c8", "#9D9692", "#0EA1A0"), col = c("#00000060", "#000000", "#000000"), pt.cex = c(1, 1, 1.2), legend = c("Release sites", "Spawning channels >1M", "Hatchery releases >1M"), bg = "white")

dev.off()