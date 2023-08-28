
regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye")

# Obtain list of files so as to ignore regions that do not yet have output data
z <- list.files(path = "output/")

for(r in 1:length(regions)){
	
	sp_file <- paste0(regions[r], "-spawners.rds")
	if(sp_file %in% z){
		sp <- readRDS(paste0("output/", sp_file))
		ex <- readRDS(paste0("output/", regions[r], "-expansion-factors.rds"))
		
		spawners_r <- data.frame(
			region = rep(regions[r], dim(sp)[2]*dim(sp)[3]),
			species = rep(dimnames(sp)[[2]], each = dim(sp)[3]),
			year = rep(as.numeric(dimnames(sp)[[3]]), dim(sp)[2]),
			spawners = as.numeric(t(sp[2, , ])),
			expansion_factor1 = NA,
			expansion_factor2 = NA
		)
		
		for(s in 1:5){
			if(length(ex[[s]]) > 0){
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
