##############################################################################
# This code compiles the region-specific data for (1) spawner abundance and 
# (2) run size (where available) from EITHER region-specific datasets 
# (preferable) or expansions from spawner surveys.
# 
# Author: Steph Peacock
# Date: Oct 25, 2023
###############################################################################

library(dplyr)
library(abind)

source("code/functions.R")

regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Obtain list of files so as to ignore regions that do not yet have output data
output.files <- list.files(path = "output/")

# Generation length by species and region (for smoothing)
genLength <- read.csv("data/gen_length_regions.csv") 

###############################################################################
###############################################################################
# Go through each region and add data
###############################################################################
###############################################################################

pdf(file = "output/ignore/figures/spawners_and_runsize_ALL.pdf",
		width = 6, height = 4, pointsize = 10)

###############################################################################
# Yukon
###############################################################################

#------------------------------------------------------------------------------
# Yukon: Chinook
#------------------------------------------------------------------------------
ytck <- read.csv('data/yukon_chinook_appendixB11.csv')

# Check if years are continuous
unique(diff(ytck$Year)) # Yes, all one year apart

# Reformat data for SPS
ytck_sps <- data.frame(
	region = rep("Yukon", dim(ytck)[1]),
	species = rep("Chinook", dim(ytck)[1]),
	year = ytck$Year,
	spawners = ytck$Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytck$Canadian.origin.total.run.size.estimate,
	smoothedRunsize = NA
) 

# Smoothing
ytck_sps$smoothedSpawners <- genSmooth(
	abund = ytck_sps$spawners,
	years = ytck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chinook"]
	)

ytck_sps$smoothedRunsize <- genSmooth(
	abund = ytck_sps$runsize,
	years = ytck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chinook"]
)

plot_abund(ytck_sps)

# First region/species, so initiate sps dataframe
sps_data <- ytck_sps

#------------------------------------------------------------------------------
# Yukon: Chum
#------------------------------------------------------------------------------
ytcm_spawners <- read.csv('data/yukon_chum_appendixB16.csv')
ytcm_runsize <- read.csv('data/yukon_chum_appendixB20.csv')

# Check if years are continuous
unique(diff(ytcm_spawners$Date)) # Yes, all one year apart
unique(diff(ytcm_runsize$Year)) # Yes, all one year apart
# Note run size available for fewer years

# Reformat data for SPS
ytcm_sps <- data.frame(
	region = rep("Yukon", dim(ytcm)[1]),
	species = rep("Chum", dim(ytcm)[1]),
	year = ytcm_spawners$Date,
	spawners = ytcm_spawners$Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytcm_runsize$Total.estimated.Canadian.origin.run.size[match(ytcm_spawners$Date, ytcm_runsize$Year)],
	smoothedRunsize = NA
) 

# Smoothing
ytcm_sps$smoothedSpawners <- genSmooth(
	abund = ytcm_sps$spawners,
	years = ytcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chum"]
)

ytcm_sps$smoothedRunsize <- genSmooth(
	abund = ytcm_sps$runsize,
	years = ytcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chum"]
)

plot_abund(ytcm_sps)

# Add to SPS data
sps_data <- rbind(sps_data, ytcm_sps)

###############################################################################
# Transboundary
###############################################################################

#------------------------------------------------------------------------------
# Transboundary: Chinook
#------------------------------------------------------------------------------
tbrck <- read.csv('data/tbr_chinook.csv')

# Remove 1975 -> missing Alsek data
tbrck <- tbrck[!is.na(tbrck$Alsek_Escapement), ]

# Check if years are continuous
unique(diff(tbrck$Year)) # Yes, all one year apart

# Reformat data for SPS
tbrck_sps <- data.frame(
	region = rep("Transboundary", dim(tbrck)[1]),
	species = rep("Chinook", dim(tbrck)[1]),
	year = tbrck$Year,
	spawners = apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")], 1, sum), 
	smoothedSpawners = NA,
	runsize = apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")]/(1 - tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")]), 1, sum),
	smoothedRunsize = NA
) 

# Smoothing
tbrck_sps$smoothedSpawners <- genSmooth(
	abund = tbrck_sps$spawners,
	years = tbrck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Chinook"]
)

tbrck_sps$smoothedRunsize <- genSmooth(
	abund = tbrck_sps$runsize,
	years = tbrck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Chinook"]
)

plot_abund(tbrck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrck_sps)

#------------------------------------------------------------------------------
# Transboundary: Sockeye
#------------------------------------------------------------------------------

# TBR Sockeye data are the sum of the Stikine (Appendix B21) and Taku (Appendix D15) 

tbrse_stik <- read.csv('data/tbr_sockeye_appendixB21.csv')
tbrse_taku <- read.csv('data/tbr_sockeye_appendixD15.csv')

# What is the earliest common year?
min(tbrse_stik$Year) # 1979
min(tbrse_taku$Year) # 1984

max(tbrse_stik$Year) # 2020
max(tbrse_taku$Year) # 2020

# Remove Stikine data prior to 1984
tbrse_stik <- subset(tbrse_stik, tbrse_stik$Year >= 1984)

# Reformat data for SPS
tbrse_sps <- data.frame(
	region = rep("Transboundary", dim(tbrse_stik)[1]),
	species = rep("Sockeye", dim(tbrse_stik)[1]),
	year = tbrse_stik$Year,
	spawners = tbrse_stik$Escapement.broodstock + tbrse_taku$Escapement, 
	smoothedSpawners = NA,
	runsize = tbrse_stik$Terminal.Run + tbrse_taku$Terminal.Run,
	smoothedRunsize = NA
) 

# Smoothing
tbrse_sps$smoothedSpawners <- genSmooth(
	abund = tbrse_sps$spawners,
	years = tbrse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Sockeye"]
)

tbrse_sps$smoothedRunsize <- genSmooth(
	abund = tbrse_sps$runsize,
	years = tbrse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Sockeye"]
)

plot_abund(tbrse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrse_sps)

#------------------------------------------------------------------------------
# Transboundary: Coho
#------------------------------------------------------------------------------
tbrco <- read.csv('data/tbr_coho_appendixD20.csv')

# Check if years are continuous
unique(diff(tbrco$Year)) # Yes, all one year apart

# Reformat data for SPS
tbrco_sps <- data.frame(
	region = rep("Transboundary", dim(tbrco)[1]),
	species = rep("Coho", dim(tbrco)[1]),
	year = tbrco$Year,
	spawners = tbrco$Escape., 
	smoothedSpawners = NA,
	runsize = tbrco$Total.Run,
	smoothedRunsize = NA
) 

# Smoothing
tbrco_sps$smoothedSpawners <- genSmooth(
	abund = tbrco_sps$spawners,
	years = tbrco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Coho"]
)

tbrco_sps$smoothedRunsize <- genSmooth(
	abund = tbrco_sps$runsize,
	years = tbrco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Coho"]
)

plot_abund(tbrco_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrco_sps)

#------------------------------------------------------------------------------
# Transboundary: Pink, chum, and steelhead
#------------------------------------------------------------------------------

# Monitoring of pink and chum salmon in the Transboundary region is limited, and the 
# best available information is the Canyon Island fish wheel. 

# tbrpkcm <- read.csv("data/spawner_surveys.csv", na.strings = c(-989898)) %>%
# 	subset(!is.na(stream_observed_count)) %>% # Remove stream with no data
# 	subset(stream_name_pse == "CANYON ISLAND") %>% # Subset Canyon Island data
# 	subset(species_name %in% c("Pink (even)", "Pink (odd)", "Chum")) # Use only data from 1950 to present
# 
# tbrpkcm$species_pooled <- tbrpkcm$species_name
# tbrpkcm$species_pooled[tbrpkcm$species_name %in% c("Pink (even)", "Pink (odd)")] <- "Pink"

# --- 
# Pink
# --- 

tbrpk <- read.csv("data/TTC_ManualExtract_Taku_Pink.csv") %>%
	filter(!is.na(Value))

# Check if years are continuous
unique(diff(tbrpk$Year)) # Yes, all one year apart

# Reformat data for SPS
tbrpk_sps <- data.frame(
	region = rep("Transboundary", nrow(tbrpk)), #rep("Transboundary", length(which(tbrpkcm$species_pooled == "Pink"))),
	species = rep("Pink", nrow(tbrpk)), #length(which(tbrpkcm$species_pooled == "Pink"))),
	year = tbrpk$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"]),
	spawners = tbrpk$Value, # tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Pink"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 
# Smoothing
tbrpk_sps$smoothedSpawners <- genSmooth(
	abund = tbrpk_sps$spawners,
	years = tbrpk_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Pink"]
)

plot_abund(tbrpk_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrpk_sps)

# --- 
# Chum
# --- 
tbrcm <- read.csv("data/TTC_ManualExtract_Taku_Chum.csv") %>%
	filter(!is.na(Value))

# Reformat data for SPS
tbrcm_sps <- data.frame(
	region = rep("Transboundary", nrow(tbrcm)), #length(which(tbrpkcm$species_pooled == "Chum"))),
	species = rep("Chum", nrow(tbrcm)), #length(which(tbrpkcm$species_pooled == "Chum"))),
	year = tbrcm$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"]),
	spawners = tbrcm$Value, #tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Chum"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 
# Smoothing
tbrcm_sps$smoothedSpawners <- genSmooth(
	abund = tbrcm_sps$spawners,
	years = tbrcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Chum"]
)

plot_abund(tbrcm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrcm_sps)

# --- 
# Steelhead
# --- 

tbrsh <- read.csv("data/TTC_ManualExtract_Taku_Steelhead.csv")

# Check if years are continuous
unique(diff(tbrsh$Year)) # Yes, all one year apart

# Start timeseries at first non-NA
tbrsh <- tbrsh[which(!is.na(tbrsh$Value))[1]:nrow(tbrsh), ]

# Reformat data for SPS
tbrsh_sps <- data.frame(
	region = rep("Transboundary", nrow(tbrsh)), #rep("Transboundary", length(which(tbrpkcm$species_pooled == "Pink"))),
	species = rep("Steelhead", nrow(tbrsh)), #length(which(tbrpkcm$species_pooled == "Pink"))),
	year = tbrsh$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"]),
	spawners = tbrsh$Value, # tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Pink"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 
# Smoothing
tbrsh_sps$smoothedSpawners <- genSmooth(
	abund = tbrsh_sps$spawners,
	years = tbrsh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Steelhead"]
)

plot_abund(tbrsh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrsh_sps)
###############################################################################
# Haida Gwaii
###############################################################################

# PSC Northern Boundary data for comparison
hg_table30 <- read.csv("data/TCNB-23-01_Table30_Area1escapement.csv")
names(hg_table30) <- c("year", "Sockeye", "Coho", "Pink", "Chum", "Chinook") # Change names to be consistent with species

#------------------------------------------------------------------------------
# Haida Gwaii: Chinook
#------------------------------------------------------------------------------
# No recent data, but at least show what we have for Yakoun
# These are consistent with the TCNB escapement data for Area 1 (Table 30)

hgck <- read.csv("data/spawner_surveys.csv", na.strings = c(-989898)) %>%
	subset(species_name == "Chinook" & region == "Haida Gwaii") %>%
	subset(stream_name_pse == "YAKOUN RIVER")

# There are some missing years of data; want to impute in between when smoothing
diff(hgck$year)
hgck_years <- min(hgck$year):max(hgck$year)

# Reformat data for SPS
hgck_sps <- data.frame(
	region = rep("Haida Gwaii", length(hgck_years)),
	species = rep("Chinook", length(hgck_years)),
	year = hgck_years,
	spawners = hgck$stream_observed_count[match(hgck_years, hgck$year)], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
hgck_sps$smoothedSpawners <- genSmooth(
	abund = hgck_sps$spawners,
	years = hgck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Haida Gwaii" & genLength$species == "Chinook"]
)

plot_abund(hgck_sps)
# lines(hg_table30$year, hg_table30$Chinook*10^-3, lty = 2, col = 2)
# legend("topleft", lty = 2, col = 2, "NBTC Table 30 (Area 1) escapement")

# Add to master sps dataframe
sps_data <- rbind(sps_data, hgck_sps)


#------------------------------------------------------------------------------
# Haida Gwaii: Chum, Coho, Pink, Sockeye
#------------------------------------------------------------------------------
# Expansion from spawner surveys only; no steelhead data

# Use expanded spawner abundance -> NO, not super accurate
sp <- readRDS("output/Haida Gwaii-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 2:5){
	hg.s <- sp[2, species[s], ]
	# hg.s <- hg.s[!is.na(hg.s)]

	# Reformat data for SPS
	hg.s_sps <- data.frame(
		region = rep("Haida Gwaii", length(hg.s)),
		species = rep(species[s], length(hg.s)),
		year = as.numeric(names(hg.s)),
		spawners = as.numeric(hg.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	hg.s_sps$smoothedSpawners <- genSmooth(
		abund = hg.s_sps$spawners,
		years = hg.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Haida Gwaii" & genLength$species == species[s]]
	)
	
	plot_abund(hg.s_sps)
	# lines(hg_table30$year, hg_table30[, species[s]]*10^-3, col = 2)
	# legend("topleft", lty = 1, col = 2, "NBTC Table 30 (Area 1) escapement", bty = "n")
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, hg.s_sps)
	
}	# end species

###############################################################################
# Nass
###############################################################################

#------------------------------------------------------------------------------
# Nass: Sockeye
#------------------------------------------------------------------------------
# # From Fig. 1 of PSC (2023) Assessment and management frameworks of the Pacific Salmon Treaty and their robustness to environmental change
# nassse <- read.csv('data/nass_sockeye.csv')
# names(nassse) <- c("Year", "Total.Run", "TE") # Rename to be consistent with NCCDBV2 output for easy switching

# # Compare to escapement from the TCNB (23)-01
# nassse2 <- read.csv("data/TCNB-23-01_Table31_Area3escapement.csv")
# plot(nassse$year, nassse$escapement,"o")
# lines(nassse2$YEAR, nassse2$SOCKEYE, col = 2)

# Compare to NCC run reconstruction: https://github.com/LGLLimited/nccdbv2/tree/master/run/2022-nass-update/data/KarlEnglish-2023-05-29
nassse <- read.csv("data/nassskeena_sockeye_lgl.csv") %>%
subset(Region == "Nass")
# par(mfrow = c(2, 1), mar = c(3,4,2,1))
# plot(nassse$year, nassse$escapement,"o", xlim = c(1980, 2023), bty = "l", ylab = "Escapement")
# lines(nassse3$Year[nassse3$Region == "Nass"], nassse3$TE[nassse3$Region == "Nass"], "o", col = 2, pch = 19, cex = 0.6)
# legend("topright", pch = c(1, 19), col = c(1,2), pt.cex = c(1, 0.8), legend = c("SCSC figure", "NCCDB_v2 GitHub"), lty = 1, bty = "n")
# 
# plot(nassse$year, nassse$runsize,"o", xlim = c(1980, 2023), bty = "l", ylab = "Run Size")
# lines(nassse3$Year[nassse3$Region == "Nass"], nassse3$Total.Run[nassse3$Region == "Nass"], "o", col = 2, pch = 19, cex = 0.6)
# mtext(side = 3, line = -1, outer= TRUE, 'Nass sockeye')

# Check if years are continuous
# unique(diff(nassse$Year)) # Yes, all one year apart

# Reformat data for SPS
nassse_sps <- data.frame(
	region = rep("Nass", dim(nassse)[1]),
	species = rep("Sockeye", dim(nassse)[1]),
	year = nassse$Year,
	spawners = nassse$TE, 
	smoothedSpawners = NA,
	runsize = nassse$Total.Run,
	smoothedRunsize = NA
) 

# Smoothing
nassse_sps$smoothedSpawners <- genSmooth(
	abund = nassse_sps$spawners,
	years = nassse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Sockeye"]
)

nassse_sps$smoothedRunsize <- genSmooth(
	abund = nassse_sps$runsize,
	years = nassse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Sockeye"]
)

plot_abund(nassse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nassse_sps)

#------------------------------------------------------------------------------
# Nass: Steelhead
#------------------------------------------------------------------------------
# Use CU-level Nass summer abundance which is a better region-scale indicator
nasssh <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% 
	subset(region == "Nass" & species_name == "Steelhead" & cu_name_pse == "Nass Summer" & !is.na(estimated_count))

# Reformat data for SPS
nasssh_sps <- data.frame(
	region = rep("Nass", dim(nasssh)[1]),
	species = rep("Steelhead", dim(nasssh)[1]),
	year = nasssh$year,
	spawners = as.numeric(nasssh$estimated_count), 
	smoothedSpawners = NA,
	runsize = as.numeric(nasssh$total_run),
	smoothedRunsize = NA
) 

# Smoothing
nasssh_sps$smoothedSpawners <- genSmooth(
	abund = nasssh_sps$spawners,
	years = nasssh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Steelhead"]
)

nasssh_sps$smoothedRunsize <- genSmooth(
	abund = nasssh_sps$runsize,
	years = nasssh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Steelhead"]
)

plot_abund(nasssh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nasssh_sps)

#------------------------------------------------------------------------------
# Nass: Chinook, Pink, Chum, Coho
#------------------------------------------------------------------------------

# Use expanded spawner abundance
sp <- readRDS("output/Nass-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in c(1:4)){
	nass.s <- sp[2, species[s], ]
	
	# Reformat data for SPS
	nass.s_sps <- data.frame(
		region = rep("Nass", length(nass.s)),
		species = rep(species[s], length(nass.s)),
		year = as.numeric(names(nass.s)),
		spawners = as.numeric(nass.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	nass.s_sps$smoothedSpawners <- genSmooth(
		abund = nass.s_sps$spawners,
		years = nass.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == species[s]]
	)
	
	plot_abund(nass.s_sps)

	# Add to master sps dataframe
	sps_data <- rbind(sps_data, nass.s_sps)
	
}	

###############################################################################
# Skeena
###############################################################################

#------------------------------------------------------------------------------
# Skeena: Sockeye
#------------------------------------------------------------------------------
# From Fig. 1 of PSC (2023) Assessment and management frameworks of the Pacific Salmon Treaty and their robustness to environmental change
# skse <- read.csv('data/skeena_sockeye.csv')

# Compare to NCC run reconstruction: https://github.com/LGLLimited/nccdbv2/tree/master/run/2022-nass-update/data/KarlEnglish-2023-05-29
skse <- read.csv("data/nassskeena_sockeye_lgl.csv") %>%
	subset(Region == "Skeena")
# par(mfrow = c(2, 1), mar = c(3,4,2,1))
# plot(skse$year, skse$escapement,"o", xlim = c(1960, 2023), bty = "l", ylab = "Escapement", ylim = c(0, max(nassse3$TE[nassse3$Region == "Skeena"])))
# lines(nassse3$Year[nassse3$Region == "Skeena"], nassse3$TE[nassse3$Region == "Skeena"], "o", col = 2, pch = 19, cex = 0.6)
# legend("topleft", pch = c(1, 19), col = c(1,2), pt.cex = c(1, 0.8), legend = c("SCSC figure", "NCCDB_v2 GitHub"), lty = 1, bty = "n")
# 
# plot(skse$year, skse$runsize,"o", xlim = c(1960, 2023), bty = "l", ylab = "Run Size")
# lines(nassse3$Year[nassse3$Region == "Skeena"], nassse3$Total.Run[nassse3$Region == "Skeena"], "o", col = 2, pch = 19, cex = 0.6)
# mtext(side = 3, line = -1, outer= TRUE, 'Skeena sockeye')

# Check if years are continuous
unique(diff(skse$Year)) # Yes, all one year apart

# Reformat data for SPS
skse_sps <- data.frame(
	region = rep("Skeena", dim(skse)[1]),
	species = rep("Sockeye", dim(skse)[1]),
	year = skse$Year,
	spawners = skse$TE, 
	smoothedSpawners = NA,
	runsize = skse$Total.Run,
	smoothedRunsize = NA
) 

# Smoothing
skse_sps$smoothedSpawners <- genSmooth(
	abund = skse_sps$spawners,
	years = skse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Sockeye"]
)

skse_sps$smoothedRunsize <- genSmooth(
	abund = skse_sps$runsize,
	years = skse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Sockeye"]
)

plot_abund(skse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, skse_sps)

#------------------------------------------------------------------------------
# Skeena: Steelhead
#------------------------------------------------------------------------------

# Use Skeena steelhead run size from BC Updates
sksh <- read.csv("data/Steelhead_total_runsize.csv")

# Reformat data for SPS
sksh_sps <- data.frame(
	region = rep("Skeena", dim(sksh)[1]),
	species = rep("Steelhead", dim(sksh)[1]),
	year = sksh$Year,
	spawners = sksh$prov_runsize_raw, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
sksh_sps$smoothedSpawners <- genSmooth(
	abund = sksh_sps$spawners,
	years = sksh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Steelhead"]
)

plot_abund(sksh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, sksh_sps)

#------------------------------------------------------------------------------
# Skeena: Chum
#------------------------------------------------------------------------------

sk_table32 <- read.csv("data/TCNB-23-01_Table32_Area4escapement.csv")

# Reformat data for SPS
skcm_sps <- data.frame(
	region = rep("Skeena", dim(sk_table32)[1]),
	species = rep("Chum", dim(sk_table32)[1]),
	year = sk_table32$YEAR,
	spawners = sk_table32$CHUM, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
skcm_sps$smoothedSpawners <- genSmooth(
	abund = skcm_sps$spawners,
	years = skcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Chum"]
)

plot_abund(skcm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, skcm_sps)

#------------------------------------------------------------------------------
# Skeena: Chinook, Pink, Coho
#------------------------------------------------------------------------------

# Use expanded spawner abundance -> NO, not super accurate
sp <- readRDS("output/Skeena-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in c(1, 3, 4)){
	skeena.s <- sp[2, species[s], ]
	skeena.s <- skeena.s[!is.na(skeena.s)]
	
	# Reformat data for SPS
	skeena.s_sps <- data.frame(
		region = rep("Skeena", length(skeena.s)),
		species = rep(species[s], length(skeena.s)),
		year = as.numeric(names(skeena.s)),
		spawners = as.numeric(skeena.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	skeena.s_sps$smoothedSpawners <- genSmooth(
		abund = skeena.s_sps$spawners,
		years = skeena.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == species[s]]
	)
	
	plot_abund(skeena.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, skeena.s_sps)
	
}	# end species

###############################################################################
# Central Coast
###############################################################################

# Expansion from spawner surveys only; no steelhead data
sp <- readRDS("output/Central Coast-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 1:5){
	cc.s <- sp[2, species[s], ]
	cc.s <- cc.s[!is.na(cc.s)]
	
	# Reformat data for SPS
	cc.s_sps <- data.frame(
		region = rep("Central Coast", length(cc.s)),
		species = rep(species[s], length(cc.s)),
		year = as.numeric(names(cc.s)),
		spawners = as.numeric(cc.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	cc.s_sps$smoothedSpawners <- genSmooth(
		abund = cc.s_sps$spawners,
		years = cc.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Central Coast" & genLength$species == species[s]]
	)
	
	plot_abund(cc.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, cc.s_sps)
	
}	# end species

###############################################################################
# Vancouver Island & Mainland Inlets
###############################################################################

# Expansion from spawner surveys only; no steelhead data
sp <- readRDS("output/Vancouver Island & Mainland Inlets-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 1:5){
	vimi.s <- sp[2, species[s], ]
	# Get rid of NAs (esp in 2022)
	vimi.s <- vimi.s[1:max(which(!is.na(vimi.s)))]
	
	
	# Reformat data for SPS
	vimi.s_sps <- data.frame(
		region = rep("Vancouver Island & Mainland Inlets", length(vimi.s)),
		species = rep(species[s], length(vimi.s)),
		year = as.numeric(names(vimi.s)),
		spawners = as.numeric(vimi.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	vimi.s_sps$smoothedSpawners <- genSmooth(
		abund = vimi.s_sps$spawners,
		years = vimi.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Vancouver Island & Mainland Inlets" & genLength$species == species[s]]
	)
	
	plot_abund(vimi.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, vimi.s_sps)
	
}	# end species

###############################################################################
# Fraser
###############################################################################

#------------------------------------------------------------------------------
# Chinook
#------------------------------------------------------------------------------
# (1) Use expanded spawner surveys from 1984 onward
sp <- readRDS("output/Fraser-spawners.rds")
yrs <- c(1984:max(as.numeric(dimnames(sp)[[3]])))

# (2) Use Atlas et al. (2023) Fraser populations for run size
frck_run0 <- read.csv("data/Atlas2023/CK_TotalRun_FINAL.csv") %>% 
	subset(group == "salish") %>% # select Salish group
	subset(grepl("skagit", population) == FALSE & grepl("cowichan", population) == FALSE) # Remove VIMI populations

frck_run <- tapply(frck_run0$tot_run, frck_run0$year, sum)

# Put in SPS format
frck_sps <- data.frame(
	region = rep("Fraser", length(yrs)),
	species = rep("Chinook", length(yrs)),
	year = yrs,
	spawners = sp[2, "Chinook", match(yrs, as.numeric(dimnames(sp)[[3]]))], 
	smoothedSpawners = NA,
	runsize = frck_run[match(yrs, as.numeric(names(frck_run)))],
	smoothedRunsize = NA
) 
	
	# Smoothing
frck_sps$smoothedSpawners <- genSmooth(
		abund = frck_sps$spawners,
		years = frck_sps$year,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
	)

frck_sps$smoothedRunsize <- genSmooth(
	abund = frck_sps$runsize,
	years = frck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
)

plot_abund(frck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frck_sps)

#------------------------------------------------------------------------------
# Chum
#------------------------------------------------------------------------------
# (1) Use expanded spawner surveys from 1984 onward
yrs <- as.numeric(dimnames(sp)[[3]])
yrs <- 1953:max(yrs[!is.na(sp[2, "Chum", ])])

# Put in SPS format
frcm_sps <- data.frame(
	region = rep("Fraser", length(yrs)),
	species = rep("Chum", length(yrs)),
	year = yrs,
	spawners = sp[2, "Chum", match(yrs, as.numeric(dimnames(sp)[[3]]))], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
frcm_sps$smoothedSpawners <- genSmooth(
	abund = frcm_sps$spawners,
	years = frcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chum"]
)

plot_abund(frcm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frcm_sps)


#------------------------------------------------------------------------------
# Coho
#------------------------------------------------------------------------------

# Interior Fraser Coho data shared by Marissa.Glavas@dfo-mpo.gc.ca on data request
frco <- read.csv("data/E. Hertz - IFC Data.csv")
yrs <- sort(unique(frco$ReturnYear))

# Put in SPS format
frco_sps <- data.frame(
	region = rep("Fraser", length(yrs)),
	species = rep("Coho", length(yrs)),
	year = yrs,
	spawners = tapply(frco$Total.Return, frco$ReturnYear, sum), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = tapply(frco$Total.Prefishery.Abundance, frco$ReturnYear, sum),
	smoothedRunsize = NA
) 

# Smoothing
frco_sps$smoothedSpawners <- genSmooth(
	abund = frco_sps$spawners,
	years = frco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Coho"]
)

frco_sps$smoothedRunsize <- genSmooth(
	abund = frco_sps$runsize,
	years = frco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Coho"]
)

plot_abund(frco_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frco_sps)

#------------------------------------------------------------------------------
# Pink
#------------------------------------------------------------------------------

# From PSC (updated to include)
frpk <- read.csv("data/pink_run_size_2023-12-08.csv")

# Put in SPS format
frpk_sps <- data.frame(
	region = rep("Fraser", length(frpk$Year)),
	species = rep("Pink", length(frpk$Year)),
	year = frpk$Year,
	spawners = frpk$Escapement, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frpk$Run.Size,
	smoothedRunsize = NA
) 

# Smoothing - don't do this for pink salmon in the Fraser - no even year data!
frpk_sps$smoothedSpawners <- frpk_sps$spawners
frpk_sps$smoothedRunsize <- frpk_sps$runsize

plot_abund(frpk_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frpk_sps)

#------------------------------------------------------------------------------
# Sockeye
#------------------------------------------------------------------------------

# From PSC
frse <- read.csv("data/Total Fraser_run_size_2023-09-20.csv")

# Put in SPS format
frse_sps <- data.frame(
	region = rep("Fraser", length(frse$Year)),
	species = rep("Sockeye", length(frse$Year)),
	year = frse$Year,
	spawners = frse$Spawning.Escapement, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frse$Run.Size,
	smoothedRunsize = NA
) 

# Fill in run size with in-season estimates if available
if(sum(is.na(frse_sps$runsize)) > 0){
	if(!is.na(frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])])){
		frse_sps$runsize[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])] <- frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])]
	}
}

# Smoothing
frse_sps$smoothedSpawners <- genSmooth(
	abund = frse_sps$spawners,
	years = frse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]
)

frse_sps$smoothedRunsize <- genSmooth(
	abund = frse_sps$runsize,
	years = frse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]
)

plot_abund(frse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frse_sps)

#------------------------------------------------------------------------------
# Steelhead
#------------------------------------------------------------------------------

# Use sum of two main Fraser steelhead CUs
frsh <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% 
	subset(region == "Fraser" & species_name == "Steelhead") %>%
	subset(cu_name_pse %in% c("Thompson Summer", "Mid Fraser Summer")) %>%
	subset(!is.na(estimated_count))

frsh_sum <- tapply(as.numeric(frsh$estimated_count), frsh$year, sum)

# Put in SPS format
frsh_sps <- data.frame(
	region = rep("Fraser", length(frsh_sum)),
	species = rep("Steelhead", length(frsh_sum)),
	year = as.numeric(names(frsh_sum)),
	spawners = frsh_sum, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
frsh_sps$smoothedSpawners <- genSmooth(
	abund = frsh_sps$spawners,
	years = frsh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Steelhead"]
)


plot_abund(frsh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frsh_sps)

###############################################################################
# Columbia
###############################################################################

#------------------------------------------------------------------------------
# Chinook
#------------------------------------------------------------------------------
colck <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% 
	subset(region == "Columbia" & species_name == "Chinook") %>%
	subset(!is.na(estimated_count))

# Add in more recent Chinook data that's not yet in database
# Sent directly by Chuck Parken: Pacific Salmon Commission Okanagan Work Group
# Okanagan Chinook: Summary of Findings and COnsiderations for FUture Actions
# June 28, 2023

colck <- rbind(colck, data.frame(
	region = rep("Columbia", 4),
	species_name = rep("Chinook", 4),
	cuid = rep(301, 4),
	cu_name_pse = rep("Okanagan", 4),
	year = c(2019:2022),
	estimated_count = c(15, 79, 73, 23),
	observed_count = c(15, 79, 73, 23),
	total_run = rep(NA, 4),
	uploadid = rep(NA, 4)
))

# Put in SPS format
colck_sps <- data.frame(
	region = rep("Columbia", length(colck$year)),
	species = rep("Chinook", length(colck$year)),
	year = colck$year,
	spawners = as.numeric(colck$estimated_count), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
colck_sps$smoothedSpawners <- genSmooth(
	abund = colck_sps$spawners,
	years = colck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Chinook"]
)


plot_abund(colck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, colck_sps)

#------------------------------------------------------------------------------
# Sockeye
#------------------------------------------------------------------------------
colse <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% 
	subset(region == "Columbia" &  species_name == "Lake sockeye") %>%
	subset(!is.na(estimated_count))

# Put in SPS format
colse_sps <- data.frame(
	region = rep("Columbia", length(colse$year)),
	species = rep("Sockeye", length(colse$year)),
	year = colse$year,
	spawners = as.numeric(colse$estimated_count), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
colse_sps$smoothedSpawners <- genSmooth(
	abund = colse_sps$spawners,
	years = colse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Sockeye"]
)


plot_abund(colse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, colse_sps)

#------------------------------------------------------------------------------
# Columbia: Steelhead
#------------------------------------------------------------------------------
colsh <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% 
	subset(region == "Columbia" &  species_name == "Steelhead") %>%
	subset(!is.na(estimated_count))

# Put in SPS format
colsh_sps <- data.frame(
	region = rep("Columbia", length(colsh$year)),
	species = rep("Steelhead", length(colsh$year)),
	year = colsh$year,
	spawners = as.numeric(colsh$estimated_count), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
colsh_sps$smoothedSpawners <- genSmooth(
	abund = colsh_sps$spawners,
	years = colsh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Steelhead"]
)


plot_abund(colsh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, colsh_sps)


dev.off()
###############################################################################
###############################################################################
# Write to .csv
###############################################################################
###############################################################################

write.csv(sps_data, "output/sps-data.csv", row.names = FALSE)
