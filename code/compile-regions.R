
regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye")

# Obtain list of files so as to ignore regions that do not yet have output data
z <- list.files(path = "output/")

# Generation length by species and region (for smoothing)
genLength <- read.csv("data/gen_length_regions.csv") 


for(r in 1:length(regions)){
	
	sp_file <- paste0(regions[r], "-spawners.rds")
	if(sp_file %in% z){
		sp <- readRDS(paste0("output/", sp_file))
		ex <- readRDS(paste0("output/", regions[r], "-expansion-factors.rds"))
		
		
		#----
		# Make region/species-specific adjustments
		#----
		
		# Nass: Use NuSEDS until pink issue is sorted
		# https://salmonwatersheds.slack.com/archives/C01D2S4PRC2/p1688745942045519 
		if(regions[r] == "Nass"){
			sp <- readRDS(paste0("output/", regions[r], "-spawners_nuseds.rds"))
			ex <- readRDS(paste0("output/", regions[r], "-expansion-factors_nuseds.rds"))
		}
		
		# Central coast: Use NuSEDS until pink issue is sorted
		if(regions[r] == "Nass"){
			sp <- readRDS(paste0("output/", regions[r], "-spawners_nuseds.rds"))
			ex <- readRDS(paste0("output/", regions[r], "-expansion-factors_nuseds.rds"))
		}
				
		# Use PSC Mission counts for fraser pink salmon	
		if(regions[r] == "Fraser"){
			cu_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Fraser" & species_name == "Pink (odd)")
			cu_abund$estimated_count[cu_abund$year == 2021] <- 7827444.568
			sp[2, 4, ] <- cu_abund$estimated_count[match(cu_abund$year, as.numeric(dimnames(sp)[[3]]))]
			ex[[4]] <- NA
		}
		
		# Calculate smoothed spawner abundance
		# Smooth abundance using the geometric running mean over the generation length
		g_interim <- subset(genLength, region == regions[r])
		g <- g_interim$gen_length; names(g) <- g_interim$species
		rm(g_interim)
		
		n.species <- dim(sp)[2]
		n.yrs <- dim(sp)[3]
		yrs <- as.numeric(dimnames(sp)[[3]])
		
		# Calculate geometric average
		smoothedSpawners <- array(NA, dim = c(n.species, n.yrs))
		for(s in 1:n.species){
			gs <- as.numeric(g[species[s]])
			for(k in 1:n.yrs){ # For each year
				smoothYrs <- c(max(yrs[1], yrs[k] - gs + 1):yrs[k]) # define previous years over which to smooth
				# Unweighted geometric mean
				S <- sp[2, s, yrs %in% smoothYrs]
				Y <- sum(!is.na(S))
				if(Y > 0){ # If there are no data, leave as NA
					smoothedSpawners[s, k] <- prod(S, na.rm = TRUE) ^ (1/Y)
				}
			}
		} # end s
		
		
		spawners_r <- data.frame(
			region = rep(regions[r], n.species*n.yrs),
			species = rep(dimnames(sp)[[2]], each = n.yrs),
			year = rep(yrs, n.species),
			spawners = as.numeric(t(sp[2, , ])),
			smoothedSpawners = as.numeric(t(smoothedSpawners)),
			expansion_factor1 = NA,
			expansion_factor2 = NA
		)
		
		for(s in 1:5){
			if(length(ex[[s]]) == 2){
				spawners_r$expansion_factor1[which(spawners_r$species == species[s])] <- ex[[s]]$exp1
				spawners_r$expansion_factor2[which(spawners_r$species == species[s])] <- ex[[s]]$exp2
			}
		}
		
		# Merge back to global dataframe
		if(("spawners" %in% ls()) == FALSE){
			spawners <- spawners_r
		} else {
			spawners <- rbind(spawners, spawners_r)
		}
	} # end if there is a file
	
} # end r

write.csv(spawners, "output/spawners_all-regions.csv", row.names = FALSE)
