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
ytcm <- read.csv('data/yukon_chum_appendixB16.csv')

# Check if years are continuous
unique(diff(ytcm$Date)) # Yes, all one year apart

# Reformat data for SPS
ytcm_sps <- data.frame(
	region = rep("Yukon", dim(ytcm)[1]),
	species = rep("Chum", dim(ytcm)[1]),
	year = ytcm$Date,
	spawners = ytcm$Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytcm$U.S..Canada.mainstem.border.passage.estimate,
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
# Transboundary: Pink and chum
#------------------------------------------------------------------------------

# Monitoring of pink and chum salmon in the Transboundary region is limited, and the 
# best available information is the Canyon Island fish wheel. 

tbrpkcm <- read.csv("data/spawner_surveys.csv", na.strings = c(-989898)) %>%
	subset(!is.na(stream_observed_count)) %>% # Remove stream with no data
	subset(stream_name_pse == "CANYON ISLAND") %>% # Subset Canyon Island data
	subset(species_name %in% c("Pink (even)", "Pink (odd)", "Chum")) # Use only data from 1950 to present

tbrpkcm$species_pooled <- tbrpkcm$species_name
tbrpkcm$species_pooled[tbrpkcm$species_name %in% c("Pink (even)", "Pink (odd)")] <- "Pink"

# --- 
# Pink
# --- 
# Reformat data for SPS
tbrpk_sps <- data.frame(
	region = rep("Transboundary", length(which(tbrpkcm$species_pooled == "Pink"))),
	species = rep("Pink", length(which(tbrpkcm$species_pooled == "Pink"))),
	year = sort(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"]),
	spawners = tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Pink"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"])], 
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
# Reformat data for SPS
tbrcm_sps <- data.frame(
	region = rep("Transboundary", length(which(tbrpkcm$species_pooled == "Chum"))),
	species = rep("Chum", length(which(tbrpkcm$species_pooled == "Chum"))),
	year = sort(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"]),
	spawners = tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Chum"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"])], 
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
diff(hgck_sps$year)
hgck_years <- min(hgck_sps$year):max(hgck_sps$year)

# Reformat data for SPS
hgck_sps <- data.frame(
	region = rep("Haida Gwaii", length(hgck_years)),
	species = rep("Chinook", length(hgck_years)),
	year = hgck_years,
	spawners = hgck$stream_observed_count[match(hgck_years, hgck_sps$year)], 
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
lines(hg_table30$year, hg_table30$Chinook*10^-3, lty = 2, col = 2)
legend("topleft", lty = 2, col = 2, "NBTC Table 30 (Area 1) escapement")

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
	lines(hg_table30$year, hg_table30[, species[s]]*10^-3, col = 2)
	legend("topleft", lty = 1, col = 2, "NBTC Table 30 (Area 1) escapement", bty = "n")
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, hg.s_sps)
	
}	# end species

###############################################################################
# Nass
###############################################################################

#------------------------------------------------------------------------------
# Nass: Sockeye
#------------------------------------------------------------------------------
# From Fig. 1 of PSC (2023) Assessment and management frameworks of the Pacific Salmon Treaty and their robustness to environmental change
nassse <- read.csv('data/nass_sockeye.csv')

# Check if years are continuous
unique(diff(nassse$year)) # Yes, all one year apart

# Reformat data for SPS
nassse_sps <- data.frame(
	region = rep("Nass", dim(nassse)[1]),
	species = rep("Sockeye", dim(nassse)[1]),
	year = nassse$year,
	spawners = nassse$escapement, 
	smoothedSpawners = NA,
	runsize = nassse$runsize,
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
skse <- read.csv('data/skeena_sockeye.csv')

# Check if years are continuous
unique(diff(skse$year)) # Yes, all one year apart

# Reformat data for SPS
skse_sps <- data.frame(
	region = rep("Skeena", dim(skse)[1]),
	species = rep("Sockeye", dim(skse)[1]),
	year = skse$year,
	spawners = skse$escapement, 
	smoothedSpawners = NA,
	runsize = skse$runsize,
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

for(s in 1:6){
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

# Look at data from Atlas et al. 2023
frck_spawn <- read.csv("data/Atlas2023/CK_esc_FINAL.csv")
frck_run <- read.csv("data/Atlas2023/CK_TotalRun_FINAL.csv") %>% 
	subset(group == "salish") %>% # select Salish group
	subset(grepl("skagit", population) == FALSE & grepl("cowichan", population) == FALSE) # Remove VIMI populations

