##############################################################################
# This code compiles the region-specific output from regional-expansions.R
# into a single csv file that is used in Rmd doc and figures
# 
# Author: Steph Peacock
# Date: Sept 5, 2023
###############################################################################

# Clear workspace (I rarely do this in a script, but need to avoid have variables
# spawners and numStreams already in environment)
rm(list=ls())
library(dplyr)
library(abind)

regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

# Obtain list of files so as to ignore regions that do not yet have output data
z <- list.files(path = "output/")

# Generation length by species and region (for smoothing)
genLength <- read.csv("data/gen_length_regions.csv") 

yrs <- 1950:2022

for(r in 1:length(regions)){
	
	#-----------------------------------------------------------------------------
	# Columbia: Use CU-level abundance
	#-----------------------------------------------------------------------------
	if(regions[r] == "Columbia"){
		cu_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Columbia" & !is.na(estimated_count))
		cu_abund$species_name[which(cu_abund$species_name == "Lake sockeye")] <- "Sockeye"
		
		# Add in more recent Chinook data that's not yet in database
		# Sent directly by Chuck Parken: Pacific Salmon Commision Okanagan Work Group
		# Okanagan Chinook: Summary of Findings and COnsiderations for FUture Actions
		# June 28, 2023
		
		Chinook_newdata <- data.frame(
			region = rep("Columbia", 4),
			species_name = rep("Chinook", 4),
			cuid = rep(301, 4),
			cu_name_pse = rep("Okanagan", 4),
			year = c(2019:2022),
			estimated_count = c(15, 79, 73, 23),
			observed_count = c(15, 79, 73, 23),
			total_run = rep(NA, 4)
		)
		
		cu_abund <- rbind(cu_abund, Chinook_newdata)
		
		spawners_r <- data.frame(
			region = rep(regions[r], dim(cu_abund)[1]),
			species = cu_abund$species_name,
			year = cu_abund$year,
			spawners = as.numeric(cu_abund$estimated_count),
			smoothedSpawners = rep(NA, dim(cu_abund)[1]),
			expansion_factor1 = NA,
			expansion_factor2 = NA
		)
		
		g_interim <- subset(genLength, region == regions[r])
		g <- g_interim$gen_length; names(g) <- g_interim$species
		rm(g_interim)
		
		species <- sort(unique(spawners_r$species))
		n.species <- length(species)
	  
		# Calculate geometric average
		for(s in 1:n.species){
			gs <- as.numeric(g[species[s]])
			yrs <- sort(unique(spawners_r$year[spawners_r$species == species[s]]))
			n.yrs <- length(yrs)
			
			for(k in 1:n.yrs){ # For each year
				smoothYrs <- c(max(yrs[1], yrs[k] - gs + 1):yrs[k]) # define previous years over which to smooth
				# Unweighted geometric mean
				S <- spawners_r$spawners[which(spawners_r$species == species[s] & spawners_r$year %in% smoothYrs)] + 0.01
				# Add 0.01 to spawners so that geometric mean is not zero for multiple years if there is an observation of zero?
				Y <- sum(!is.na(S))
				if(Y > 0){ # If there are no data, leave as NA
					spawners_r$smoothedSpawners[which(spawners_r$species == species[s] & spawners_r$year == yrs[k])] <- prod(S, na.rm = TRUE) ^ (1/Y)
				}
			} # end k years
		} # end s
		
		# Merge back to global dataframe
		if(("spawners" %in% ls()) == FALSE){
			spawners <- spawners_r
		} else {
			spawners <- rbind(spawners, spawners_r)
		}
		
	} # end Columbia
	
	#-----------------------------------------------------------------------------
	# All other regions
	#-----------------------------------------------------------------------------
	
	sp_file <- paste0(regions[r], "-spawners.rds")
	# If there are spawner data expanded to regional scale
	if(sp_file %in% z){
		
		sp <- readRDS(paste0("output/", sp_file))
		ex <- readRDS(paste0("output/", regions[r], "-expansion-factors.rds"))
		
		n.yrs <- dim(sp)[3]
		yrs <- as.numeric(dimnames(sp)[[3]])
		#----
		# Make region/species-specific adjustments
		#----
		
		if(regions[r] == "Fraser"){
			
			# Use PSC Mission counts for Fraser pink salmon	
			pk_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Fraser" & species_name == "Pink (odd)")
			sp[2, "Pink", ] <- NA
			sp[2, "Pink", match(pk_abund$year, yrs)] <- as.numeric(pk_abund$estimated_count)
			ex[[4]] <- NA
			
			# Use sum of two main Fraser steelhead CUs
			st_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Fraser" & species_name == "Steelhead" & cu_name_pse %in% c("Thompson Summer", "Mid Fraser Summer") & !is.na(estimated_count))
			st_sum <- tapply(as.numeric(st_abund$estimated_count), st_abund$year, sum) 
			
			sp[2, "Steelhead", ] <- NA
			sp[2, "Steelhead", match(as.numeric(names(st_sum)), yrs)] <- st_sum
			
			# Truncate Fraser Chinook to 1995+
			sp[2, "Chinook", which(yrs %in% c(1950:1994))] <- NA
		}
		
		if(regions[r] == "Nass"){
			# Use CU-level Nass summer abundance which is a better region-scale indicator
			st_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Nass" & species_name == "Steelhead" & cu_name_pse == "Nass Summer")
			
		# Has 2022 data, so 73 years
			yrs <- sort(unique(st_abund$year))
			n.yrs <- length(yrs)
			st <- rbind(
				as.numeric(st_abund$observed_count), # pre-expanded
				as.numeric(st_abund$estimated_count)
			)
			
			sp.new <- abind(sp, matrix(NA, nrow = 2, ncol = 6), along = 3)
			sp.new[, "Steelhead", ] <- st 
			dimnames(sp.new)[[3]] <- yrs
			sp <- sp.new
			
		}
		
		if(regions[r] == "Skeena"){
			# Use Skeena steelhead run size
			st_abund <- read.csv("data/Steelhead_total_runsize.csv")
			
			sp[1, 6, ] <- NA # Remove observed (not-expanded) estimate
			sp[2, 6, ] <- st_abund$prov_runsize_raw[match(yrs, st_abund$Year)]
		}
		
		# Calculate smoothed spawner abundance
		# Smooth abundance using the geometric running mean over the generation length
		g_interim <- subset(genLength, region == regions[r])
		g <- g_interim$gen_length; names(g) <- g_interim$species
		rm(g_interim)
		
		n.species <- dim(sp)[2]
		species <- dimnames(sp)[[2]]
		n.yrs <- dim(sp)[3]
		yrs <- as.numeric(dimnames(sp)[[3]])
		
		# Calculate geometric average
		smoothedSpawners <- array(NA, dim = c(n.species, n.yrs))
		for(s in 1:n.species){
			gs <- as.numeric(g[species[s]])
			if(sum(!is.na(sp[2, s, ])) > 0){
				max.yrs <- max(which(!is.na(sp[2, s, ])))
			for(k in 1:max.yrs){ # For each year
				smoothYrs <- c(max(yrs[1], yrs[k] - gs + 1):yrs[k]) # define previous years over which to smooth
				# Unweighted geometric mean
				S <- sp[2, s, yrs %in% smoothYrs]
				Y <- sum(!is.na(S))
				if(Y > 0){ # If there are no data, leave as NA
					smoothedSpawners[s, k] <- prod(S, na.rm = TRUE) ^ (1/Y)
				}
			} # end k years
			} # end if statement
		} # end s
		
		
		# Set up dataframe for the regional data
		spawners_r <- data.frame(
			region = rep(regions[r], n.species*n.yrs),
			species = rep(dimnames(sp)[[2]], each = n.yrs),
			year = rep(yrs, n.species),
			spawners = as.numeric(t(sp[2, , ])),
			smoothedSpawners = as.numeric(t(smoothedSpawners)),
			expansion_factor1 = NA,
			expansion_factor2 = NA
		)
		
		# Integrate expansion factors
		# Note: No expansion factors for steelhead (species #6)
		for(s in 1:5){
			if(length(ex[[s]]) == 2){
				spawners_r$expansion_factor1[which(spawners_r$species == species[s])][match(as.numeric(names(ex[[s]]$exp1)), spawners_r$year[which(spawners_r$species == species[s])])] <- ex[[s]]$exp1
				spawners_r$expansion_factor2[which(spawners_r$species == species[s])][match(as.numeric(names(ex[[s]]$exp1)), spawners_r$year[which(spawners_r$species == species[s])])] <- ex[[s]]$exp2
			}
		}
		
		# Merge back to global data frame
		if(("spawners" %in% ls()) == FALSE){
			spawners <- spawners_r
		} else {
			spawners <- rbind(spawners, spawners_r)
		}
	} # end if there is a file
	
	#----------------------------------------------
	# Stream numbers
	#----------------------------------------------
	if(regions[r] == "Columbia"){
		numStreams_r <- data.frame(
			region = rep("Columbia", 6),
			species = c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead"),
			indicator = c(1, 0, 0, 0, 1, 0),
			nonindicator = c(0, 0, 0, 0, 0, 0)
		)
		
		# Merge back to global dataframe
		if(("numStreams" %in% ls()) == FALSE){
			numStreams <- numStreams_r
		} else {
			numStreams <- rbind(numStreams, numStreams_r)
		}
	}
	
	n_file <- paste0(regions[r], "-numStreams.csv")
	# If there are spawner data expanded to regional scale
	if(n_file %in% z){
		numStreams_r <- read.csv(paste0("output/", n_file))
		numStreams_r <- merge(regions[r], numStreams_r)
		colnames(numStreams_r)[1] <- "region"
		
		# Not all streams seem to be in spawner surveys (some with no data), so add manually
		if(regions[r] == "Fraser"){
			numStreams_r[which(numStreams_r$species == "Steelhead"), ] <- c("Fraser", "Steelhead", 6, 4)
		}
		
		if(regions[r] == "Skeena"){
			# For some reason in the database there are 4 indicator and zero non-indicator
			# But in the PSE there is 3 indicator and 1 non-indicator
			numStreams_r[which(numStreams_r$species == "Steelhead"), ] <- c("Skeena", "Steelhead", 3, 1)
		}
		
		if(regions[r] == "Vancouver Island & Mainland Inlets"){
			numStreams_r <- rbind(numStreams_r, c("Vancouver Island & Mainland Inlets", "Steelhead", 8, 0))
		}
		
		
		# Merge back to global dataframe
		if(("numStreams" %in% ls()) == FALSE){
			numStreams <- numStreams_r
		} else {
			numStreams <- rbind(numStreams, numStreams_r)
		}
	} # end if there is a file
	
	
} # end r

write.csv(spawners, "output/spawners_all-regions.csv", row.names = FALSE)
write.csv(numStreams, "output/numStreams_all-regions.csv", row.names = FALSE)
