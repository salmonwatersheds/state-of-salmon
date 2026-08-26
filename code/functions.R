###############################################################################
# Functions to summarize spawners through time for SPS reporting
# called on in docs/methods-results.Rmd 
# Author: Steph Peacock
# Date: July 10, 2023

###############################################################################
library(dplyr)

###############################################################################
# Basic runsize plot
###############################################################################
plot_abund <- function(sps_data_subset, cols = c("#000000", grey(0.6))){
	
	par(mar = c(3,4,2,1), mfrow = c(1,1))
	
	plot(sps_data_subset$year, sps_data_subset$spawners*10^-3, "n", xlab = "", ylab = "Abundance (thousands)", las = 1, ylim = c(0, max(c(sps_data_subset$runsize, sps_data_subset$spawners)*10^-3, na.rm = TRUE)), bty = "l")
	abline(v = seq(1950, 2025, 5), col = grey(0.8), lwd = 0.8)
	abline(v = seq(1950, 2025, 1), col = grey(0.8), lwd = 0.8, lty = 3)
	
	# Add spawners
	abline(h = exp(mean(log(sps_data_subset$spawners), na.rm = TRUE))*10^-3, col = cols[2], lty = 2)
	
	lines(sps_data_subset$year, sps_data_subset$spawners*10^-3, col = cols[2], lwd = 0.5, xpd = NA)
	points(sps_data_subset$year, sps_data_subset$spawners*10^-3, col = cols[2], pch = 21, bg = "white", lwd = 0.5)
	lines(sps_data_subset$year, sps_data_subset$smoothedSpawners*10^-3, col = cols[2], lwd = 2, xpd = NA)

	# Add run size
	if(sum(!is.na(sps_data_subset$runsize)) > 0){
		abline(h = exp(mean(log(sps_data_subset$runsize), na.rm = TRUE))*10^-3, col = cols[1], lty = 2)
	lines(sps_data_subset$year, sps_data_subset$runsize*10^-3, col = cols[1], xpd = NA, lwd = 0.5)
	points(sps_data_subset$year, sps_data_subset$runsize*10^-3, col = cols[1], pch = 21, bg = "white", lwd = 0.5)
	lines(sps_data_subset$year, sps_data_subset$smoothedRunsize*10^-3, col = cols[1], lwd = 2, xpd = NA)
	legend("topleft", lwd = 2, col = cols, c("Total", "Spawner"), bty = "n")
	
	} else {
		legend("topleft", lwd = 2, col = cols[2], c("Spawner"), bty = "n")
	}
	
	mtext(side = 3, line = 1, paste(unique(sps_data_subset$region), unique(sps_data_subset$species)))

	}

###############################################################################
# Return time series of smoothed abundance 
###############################################################################

genSmooth <- function(
		abund, # Estiamte of abundance (run size or spawner abundance) in order of increasing year
		years, # Ordered vector of years that correspond to abund
		genLength # generation length for geometric smoothing
		){
	

		g <- as.numeric(genLength)
		yrs <- sort(unique(years))
		n.yrs <- length(yrs)
		
		# Run checks
		if(length(yrs[1]:max(yrs)) != n.yrs){
			stop("Vector of years must be continuous.")
		}
		
		if(length(abund) != n.yrs){
			stop("Length of abundance does not match length of unique years.")
		}
		
		# Set up vector to store smoothed abundance
		smoothedAbund <- rep(NA, n.yrs)
		
		for(k in 1:n.yrs){ # For each year
			
			smoothYrs <- c(max(yrs[1], yrs[k] - g + 1):yrs[k]) # define previous years over which to smooth
			
			# Unweighted geometric mean
			S <- abund[which(yrs %in% smoothYrs)]
			# Add 0.01 to spawners so that geometric mean is not zero for multiple years if there is an observation of zero?
			
			
			N <- sum(!is.na(S)) # number of years with data
			if(N > 0){ # If there are no data, leave as NA
				smoothedAbund[k] <- exp(mean(log(S + 0.01), na.rm = TRUE))
			}
		} # end k years
		
		# If tail end years are NA, don't smooth over them
		index_lastData <- max(which(!is.na(abund)))
		if(index_lastData < length(smoothedAbund)){
			smoothedAbund[(index_lastData + 1):length(smoothedAbund)] <- NA
		}
		
		return(smoothedAbund)
}

###############################################################################
# Percent decline in spawners over the most recent generation
###############################################################################

calcPercDecline <- function(
		selected_region, # Select region for which to create multi-species plot
		spawners, # Data frame with (at minimum) fields for year, region, species, and abundance
		genLength # data frame of generation length for all regions and species
){
	
	# Subset spawner data for selected_region
	ss <- spawners[which(spawners$region == selected_region), c("species", "year", "smoothedSpawners")]
	
	# Define species and year variables for use
	yrs <- sort(unique(ss$year))
	species_vec <- sort(unique(ss$species[!is.na(ss$smoothedSpawners)]))
	n.species <- length(species)
	
	# Loop through species to calculate percent decline in the most recent generation
	max.yrs <- tapply(ss$year[!is.na(ss$smoothedSpawners)], ss$species[!is.na(ss$smoothedSpawners)], max)
	percDecline <- rep(NA, n.species); names(percDecline) <- species
	for(s in 1:n.species){
		# Extract relevant generation length
		g <- genLength$gen_length[which(genLength$region == selected_region & genLength$species == species_vec[s])]
		
		# Compute percent decline
		percDecline[s] <- (ss$smoothedSpawners[which(ss$species == species_vec[s] & ss$year == max.yrs[s])] - ss$smoothedSpawners[which(ss$species == species_vec[s] & ss$year == max.yrs[s] - g)]) / ss$smoothedSpawners[which(ss$species == species_vec[s] & ss$year == max.yrs[s] - g)]
	}
	
	return(percDecline)
}

###############################################################################
# Plot regional index of spawner abundance for all species through time
###############################################################################

plot.regional_abund <- function(
	selected_region, # Select region for which to create multi-species plot
	abund = "spawners", # Select "spawners" or "runsize"
	sps_metrics, 
	sps_dat
){
	
	# Subset selected_region
	dat <- sps_dat[which(sps_dat$region == selected_region), c(
		"species",
		"year",
		paste0("smoothed", toTitleCase(abund))
		)]
	
	dat <- dat[which(!is.na(dat[, 3])), ]
	
	if(dim(dat)[1] > 0){
		species_withData <- sort(unique(dat$species))
		n.species <- length(species_withData)
	
	# Calculate abundance as a percentage of the long-term average
	dat$relativeAbund <- NA
	for(s in 1:n.species){
		H <- exp(mean(log(dat[dat$species == species_withData[s], 3]), na.rm = TRUE))
		dat$relativeAbund[dat$species == species_withData[s]] <- (dat[dat$species == species_withData[s], 3] - H)/H*100
	}

	# Set margins
	par(mar = c(4,5,2,11))
	
	# Initiate blank plot
	plot(range(dat$year), 
			 c(min(dat[dat$year > 1970, 4]), min(300, quantile(dat[dat$year > 1970, 4], 0.99, na.rm = TRUE))),
			 "n", las = 1, ylab = "", xlab = "", bty = "l",
			 main = paste(selected_region, c("Spawners", "Total Return")[as.numeric(abund == "runsize") + 1]),
			 yaxt = "n")
	axis(side = 2, at = pretty(c(min(dat[dat$year > 1970, 4]), quantile(dat[dat$year > 1970, 4], 0.99, na.rm = TRUE))), labels = paste0(pretty(c(min(dat[dat$year > 1970, 4]), quantile(dat[dat$year > 1970, 4], 0.99, na.rm = TRUE))), "%"), las = 1)
	
	abline(v = seq(1950, 2025, 10), col = grey(0.8), lwd = 0.5)
	abline(v = seq(1950, 2025, 2), col = grey(0.8), lty = 3, lwd = 0.5)
	
	# Store plotting window extent for polygons and text positioning
	u <- par("usr")
	
	# Add y-axis label
	mtext(side = 2, line = 3, c("Spawner abundance", "Total return")[as.numeric(abund == "runsize") + 1])
	
	# Line for y < 1 (less than long-term average)
	segments(x0 = u[1], x1 = u[2], y0 = 0, y1 = 0, col = grey(0.8), lwd = 3, xpd = NA)
	text(u[2] + 0.1 * (u[2]-u[1]), 0, "Historical average", xpd = NA, col = grey(0.6), cex = 0.8)
	# segments(x0 = u[1], x1 = u[2] + 0.05 * (u[2]-u[1]), y0 = u[3], y1 = u[3], col = 1, xpd = NA)
	# arrows(x0 = rep(u[2] + 0.1 * (u[2]-u[1]), 2), 
	# 			 x1 = rep(u[2] + 0.1 * (u[2]-u[1]), 2), 
	# 			 y0 = 0, 
	# 			 y1 = 0 + c(-1, 1)*(u[4] - u[3])/10, 
	# 			 length = 0.08, lwd = 1.5, xpd = NA, col = grey(0.6))
	# 
	# text(rep(u[2] + 0.2 * (u[2]-u[1]), 2), 1 + c(-1, 1)*(u[4] - u[3])/10, pos = c(1,3), c("Below\nhistorical\naverage", "Above\nhistorical\naverage"), xpd = NA, col = grey(0.6), cex = 0.8)

	# Add lines for each species
	for(s in 1:n.species){
		lines(dat$year[dat$species == species_withData[s]],
					dat$relativeAbund[dat$species == species_withData[s]], 
					col = species_cols_light[species_withData[s]], lwd = 1.5, xpd = NA)
	}
	
	max.yrs <- tapply(dat$year[!is.na(dat$relativeAbund)], dat$species[!is.na(dat$relativeAbund)], max)
	for(s in 1:n.species){
		g <- sps_metrics$generation_length[sps_metrics$region == selected_region & sps_metrics$species == species_withData[s] & sps_metrics$type == c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]]
		
		points(max.yrs[species_withData[s]], dat$relativeAbund[which(dat$species == species_withData[s] & dat$year == max.yrs[species_withData[s]])], col = species_cols_dark[species_withData[s]], pch = 21, bg = species_cols_light[species_withData[s]], cex = 1.5, xpd = NA)
		
		# points(max.yrs[species_withData[s]] - g, dat$relativeAbund[which(dat$species == species_withData[s] & dat$year == (max.yrs[species_withData[s]] - g))], col = sp_cols[species_withData[s]], pch = 21, bg = "white", cex = 1.5, lwd = 2)
		
	}
	
	# Calculate percent decline from historical average
	status <- sps_metrics$current_status[sps_metrics$region == selected_region & sps_metrics$species %in% species_withData & sps_metrics$type == c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]]
	
	# Add plus if positive and blank if NA
	percDecline <- paste0(round(status*100),"%")
	percDecline[which(percDecline > 0)] <- paste0("+", percDecline[which(status > 0)])
	percDecline <- paste0(" (", percDecline, ")")
	percDecline[is.na(status)] <- ""
	
	# Calculate y-axis positioning for labels (don't want them too squished)
	y <- tapply(dat$relativeAbund[!is.na(dat$relativeAbund)], dat$species[!is.na(dat$relativeAbund)], tail, 1)
	
	# Order labels on right from highest to lowest most recent abundance
	o <- order(y)
	# y[which(percDecline < 0)] <- seq(0, 0.9, length.out = length(which(percDecline < 0)))
	# y[which(percDecline > 0)] <- seq(1.1, max(1.28, max(ss$relativeSpawners[ss$year == max(yrs)][which(percDecline > 0)])), length.out = length(which(percDecline > 0)))
	# 
	# y[is.na(percDecline)] <- NA
	# If some of the labels are too squished, adjust
	while(sum(diff(y[o]) < (u[4] - u[3])/15, na.rm = TRUE) > 0){
		ind0 <- which(diff(y[o]) < (u[4] - u[3])/15)
		y[o][ind0] <- y[o][ind0] - 0.001
		y[o][ind0 + 1] <- y[o][ind0 + 1] + 0.001
			}
	
	# Add labels with species and % change from previous generation
	text(u[2], y, paste0(species_withData, percDecline), col = species_cols_dark[species_withData], xpd = NA, adj = 0, font = 2)
	
	} else { # If no data
		par(mar = c(4,5,2,11))
		
		# Initiate blank plot
		plot(c(1950,2022), c(0, 2), "n", las = 1, ylab = "", xlab = "", main = paste(selected_region, c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]), bty = "l")
	text(mean(c(1950,2022)), 1, font = 2, "Data Deficient", cex = 2, col = grey(0.6))
			
		# Add y-axis label
		mtext(side = 2, line = 3, c("Spawner abundance", "Total return")[as.numeric(abund == "runsize") + 1])
		
	}

}



###############################################################################
# Calculate table for "By the Numbers"
###############################################################################

# Include each species in the table, even if there are no data.

byTheNumbers <- function(
		selected_region, # Select region for which to create multi-species plot
		sps_metrics # data frame of metrics
		){
	
	# Subset metrics for selected_region
	dat <- sps_metrics[which(sps_metrics$region == selected_region), ]
	
	species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")
	n.species <- length(species)
	
	#-------------------------------------
	# Metric 1: Percent decline from historical average
	#-------------------------------------
	# Add plus if positive
	metric1 <- paste0(round(dat$current_status*100, 1),"%")
	metric1[which(dat$current_status > 0)] <- paste0("+", metric1[which(dat$current_status > 0)])
	metric1[is.na(dat$current_status)] <- ""
	
	metric1_cat <- rep(NA, dim(dat)[1])
	metric1_cat[which(dat$current_status < 0)] <- "arrow-down"
	metric1_cat[which(dat$current_status > 0)] <- "arrow-up"
	metric1_cat[which(dat$current_status == 0)] <- "arrows-left-right"
	
	#-------------------------------------
	# Metric 2: Trend over entire time series (annual)
	#-------------------------------------
	
	metric2 <- paste0(round(dat$long_trend*100, 1),"%")
	metric2[which(dat$long_trend > 0)] <- paste0("+", metric2[which(dat$long_trend > 0)])
	metric2[is.na(dat$long_trend)] <- ""
	
	metric2_color <- dat$long_trend_cat

	#-------------------------------------
	# Metric 3: Trend over 3 gens (annual)
	#-------------------------------------
	
	metric3 <- paste0(round(dat$short_trend*100, 1),"%")
	metric3[which(dat$short_trend > 0)] <- paste0("+", metric3[which(dat$short_trend > 0)])
	metric3[is.na(dat$short_trend)] <- ""
	
	#-------------------------------------
	# Compile table of summary
	#-------------------------------------
	btn <- data.frame(
		Species = dat$species,
		Variable = case_when(
			dat$type == "Spawners" ~ "Spawners",
			dat$type == "Run Size" ~ "Total return"),
		
		metric1 = metric1_cat,
		metric1_perc = metric1,
		
		metric2 = dat$long_trend_cat,
		metric2_perc = metric2,
		
		metric3 = dat$short_trend_cat,
		metric3_perc = metric3,
		
		current_abund = prettierNum(round(dat$current_abundance)), 
		lastgen_abund = prettierNum(round(dat$previous_gen_abundance)),
		avg_abund = prettierNum(round(dat$average_abundance)),
		
		gen_length = ifelse(is.na(dat$gen_length), "", dat$gen_length),
		
		nyears = ifelse(is.na(dat$nyears), 0, dat$nyears),
		rangeyears = ifelse(is.na(dat$rangeyears), "", dat$rangeyears)
	)

	# Keep NAs for icon colour and type to avoid warning (" " not a colour)
	btn$metric2[which(btn$metric2 == "")] <- NA
	btn$metric3[which(btn$metric3 == "")] <- NA
	
	return(btn)
}


###############################################################################
# Function to add commas if >= 10,000
###############################################################################

prettierNum <- function(
		x # Some number or vector of numbers to be converted to pretty display
		){
	x1 <- character(length(x))
	for(i in 1:length(x)){
		# If number is na, display as blank
		if(is.na(x[i])){
			x1[i] <- " "
		
		} else {
			# If number is greater than 10,000 add a comma
			if(x[i] >= 10000){
				x1[i] <- prettyNum(x[i], big.mark=",", preserve.width="none")
			} else {
				x1[i] <- as.character(x[i])
			}
		}}
	return(x1)
}

###############################################################################
# Render reactable table for display in R Markdown docs
###############################################################################

btn_table <- function(
		selected_region, # Select region for which to create multi-species plot
		sps_metrics
){
	
	# Compile dataframe of numners
	btn <- byTheNumbers(selected_region = selected_region,
											sps_metrics = sps_metrics)
	
	
	# Render table
	btn %>%
		reactable(
		., 
		columns = list(
			Species = colDef(
				style = function(value){
					if(value == "Chinook"){
						background <- "#87AC9F"
					} else if(value == "Chum"){
						background <- "#BDA3A5"
					}else if(value == "Coho"){
						background <- "#D0B669"
					}else if(value == "Pink"){
						background <- "#EDBAAF"
					}else if(value == "Sockeye"){
						background <- "#C5CC8C"
					} else if(value == "Steelhead"){
						background <- "#6F99AD"
					}
					list(background = background)
				},
				maxWidth = 100
			),
			
			metric1 = colDef(show = FALSE),
			metric1_perc = colDef(
				name = "Current status",
				style = function(value, index){
					if(is.na(btn$metric1[index])){
						color <- "#FFFFFF"
						fontWeight <- "normal"
					} else if(btn$metric1[index] == "arrow-down"){
						color <- "#C06363"
						fontWeight <- "bold"
					} else if(btn$metric1[index] == "arrow-up"){
						color <- "#83B687"
						fontWeight <- "bold"
					} else if(btn$metric1[index] == "arrows-left-right"){
						color <- "#A7A9AC"
						fontWeight <- "normal"
					}
					list(color = color, fontWeight = fontWeight)
				}
			),
			
			metric2 = colDef(show = FALSE),
			metric2_perc = colDef(
				name = "Long-term trend",
				style = function(value, index){
					if(is.na(btn$metric2[index])){
						color <- "#FFFFFF"
						fontWeight <- "normal"
					} else if(btn$metric2[index] == "arrow-down"){
						color <- "#C06363"
						fontWeight <- "bold"
					} else if(btn$metric2[index] == "arrow-up"){
						color <- "#83B687"
						fontWeight <- "bold"
					} else if(btn$metric2[index] == "arrows-left-right"){
						color <- "#A7A9AC"
						fontWeight <- "normal"
					}
					list(color = color, fontWeight = fontWeight)
				}
			),
			
			metric3 = colDef(show = FALSE),
			metric3_perc = colDef(
				name = "Short-term trend",
				style = function(value, index){
					if(is.na(btn$metric3[index])){
						color <- "#FFFFFF"
						fontWeight <- "normal"
					} else if(btn$metric3[index] == "arrow-down"){
						color <- "#C06363"
						fontWeight <- "bold"
					} else if(btn$metric3[index] == "arrow-up"){
						color <- "#83B687"
						fontWeight <- "bold"
					} else if(btn$metric3[index] == "arrows-left-right"){
						color <- "#A7A9AC"
						fontWeight <- "normal"
					}
					list(color = color, fontWeight = fontWeight)
				}
			),
			
			current_abund = colDef(
				name = "Current",
				maxWidth = 100),
			lastgen_abund = colDef(
				name = "Previous generation",
				maxWidth = 100),
			avg_abund = colDef(
				name = "Historical average",
				maxWidth = 100),
			# num_indicator = colDef(
			# 	name = "Indicator",
			# 	maxWidth = 80),
			# num_nonindicator = colDef(
			# 	name = "Non-indicator",
			# 	maxWidth = 80),
			gen_length = colDef(
				name = "Generation length",
				maxWidth = 80),
			nyears = colDef(
				name = "Years of data",
				maxWidth = 80),
			rangeyears = colDef(
				name = "Span of data",
				maxWidth = 80)
		),
		
		columnGroups = list(
			colGroup(name = "Metrics", columns = c("metric1_perc", "metric2_perc", "metric3_perc")),
			
			colGroup(name = "Index of abundance", columns = c("current_abund", "lastgen_abund", "avg_abund"))#,
			# colGroup(name = "Number of monitored streams", columns = c("num_indicator", "num_nonindicator"))
		),
		#   meta = list(
		# 		speciesColors = sp_cols,
		# 		species_vec = species),
		bordered = TRUE,
		highlight = TRUE,
		striped = TRUE,
		resizable = TRUE,
		fullWidth = FALSE,
		wrap = TRUE,
		style = list(fontSize = "1.25rem"),
		defaultPageSize = 12
	)
	
}


###############################################################################
# Render reactable table for all regions
###############################################################################

btn_table.all <- function(
		new_metrics,
		old_metrics# data frame with region, species, metric
){
	
	# Arrange so in order region > species
	new_metrics <- new_metrics %>%
		arrange(factor(region, levels = regions), species)
	old_metrics <- old_metrics %>%
		arrange(factor(region, levels = regions), species)
	
	species_vec <- unique(new_metrics$species)
	region_tbl <- data.frame(region = unique(new_metrics$region))
	tab_long <- data.frame(
		Region = rep(unique(new_metrics$region), 2)
	)

	
	for(s in 1:length(species_vec)){

		new_s <- left_join(region_tbl, new_metrics[which(new_metrics$species == species_vec[s]),c("region", "current_status", "current_abundance_year")])
		old_s <- left_join(region_tbl, old_metrics[which(old_metrics$species == species_vec[s]),c("region", "current_status", "current_abundance_year")])
		
		tab_long <- cbind(
			tab_long,
			# Year
			c(new_s[,"current_abundance_year"],
				old_s[,"current_abundance_year"]),
			# Current status
			c(new_s[,"current_status"],
				old_s[,"current_status"])
			# # Different year?
			# rep(c(new_metrics$current_abundance_year[which(new_metrics$species == species_vec[s])]>old_metrics$current_abundance_year[which(old_metrics$species == species_vec[s])]), 2)
		)
	}
	names(tab_long) <- c("Region", rep(species_vec, each = 2))
	
	names(tab_long)[seq(2, dim(tab_long)[2], 2)] <- paste0(names(tab_long)[seq(2, dim(tab_long)[2], 2)], "_Year")
	# names(tab_long)[seq(4, dim(tab_long)[2], 3)] <- paste0(names(tab_long)[seq(4, dim(tab_long)[2], 3)], "_Updated")
	
	tab_long <- tab_long %>%
		arrange(factor(Region, levels = regions))

	# Add whether year is the same
	
	
	# Set species that are not present
	tab_long[tab_long$Region == "Yukon", which(names(tab_long) %in% c("Pink", "Sockeye", "Steelhead"))] <- -989898
	tab_long[tab_long$Region == "Columbia", which(names(tab_long) %in% c("Pink", "Chum", "Coho"))] <- -989898
	
	tab_long$Region[tab_long$Region == "East Vancouver Island & Mainland Inlets"] <- "EVIMI"
	tab_long$Region[tab_long$Region == "West Vancouver Island"] <- "WVI"
	
	# function which returns background colour based on cell value (using colour map)
	# also takes column name as an input, which allows to get max and min
	stylefunc <- function(value, index, name) {
		if(is.na(value)){
			color <- NA
			background <- "#D8D8D8"
		} else if(value == -989898){
			color <- "#000000"
			background <- "#000000"
		} else if(value == -1000){
			color <- "#C06263"
			background <- "#C06263"
		} else if(value > 0){
			color <- "#83B686"
			background <- "#83B68630"
		} else if(value < 0){
			color <- "#C06263"
			background <- "#C0626330"
		}
		
		list(color = color, background = background, fontWeight = "bold", `border-right` = "thin solid")
	}
	
	
	wyear <- 50
	wstate <- 60
	 
	# replicate list to required length
	coldefs <- list(
		Region = colDef(
		style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by region
        if (!firstSorted || firstSorted.id === 'Region') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Region'] === prevRow['Region']) {
            return { visibility: 'hidden' }
          }
        }
      }")),
		Chinook_Year = colDef(
			name = "Year",
			maxWidth = wyear#,
			# style = function(index) {
			# 	if (!is.na(tab_long[index, "Chinook_Updated"]) & tab_long[index, "Chinook_Updated"] == TRUE) {
			# 		list(fontWeight = "bold")
			# 	}
			# }
		),
		# Chinook_Updated = colDef(
		# 	show = FALSE
		# ),
		Chinook = colDef(
			name = "State",
			style = stylefunc, 
			format = colFormat(digits = 0, percent = TRUE),
			maxWidth = wstate),
		Chum_Year = colDef(
			name = "Year",
			maxWidth = wyear
		),
		Chum = colDef(
			name = "State",
			style = stylefunc, 
			format = colFormat(digits = 0, percent = TRUE),
			maxWidth = wstate),
		Coho_Year = colDef(
			name = "Year",
			maxWidth = wyear
		),
		Coho = colDef(
			name = "State",
			style = stylefunc, 
			format = colFormat(digits = 0, percent = TRUE),
			maxWidth = wstate),
		Pink_Year = colDef(
			name = "Year",
			maxWidth = wyear
		),
		Pink = colDef(
			name = "State",
			style = stylefunc, 
			format = colFormat(digits = 0, percent = TRUE),
			maxWidth = wstate),
		Sockeye_Year = colDef(
			name = "Year",
			maxWidth = wyear
		),
		Sockeye = colDef(
			name = "State",
			style = stylefunc, 
			format = colFormat(digits = 0, percent = TRUE),
			maxWidth = wstate),
		Steelhead_Year = colDef(
			name = "Year",
			maxWidth = wyear
		),
		Steelhead = colDef(
			name = "State",
			style = stylefunc, 
			format = colFormat(digits = 0, percent = TRUE),
			maxWidth = wstate))
	
	# name elements of list according to cols
	# names(coldefs) <- species
	
	# Render table
	 reactable(
			tab_long, 
			rowStyle = function(index) {
				if (index %in% seq(3, 20, 2)) {
					list(`border-top` = "thin solid")
				}
			},
			columns = coldefs,
			columnGroups = list(
				colGroup(name = "Chinook", columns = c("Chinook", "Chinook_Year")),
				colGroup(name = "Chum", columns = c("Chum", "Chum_Year")),
				colGroup(name = "Coho", columns = c("Coho", "Coho_Year")),
				colGroup(name = "Pink", columns = c("Pink", "Pink_Year")),
				colGroup(name = "Sockeye", columns = c("Sockeye", "Sockeye_Year")),
				colGroup(name = "Steelhead", columns = c("Steelhead", "Steelhead_Year"))
			),
			bordered = TRUE,
			highlight = TRUE,
			striped = TRUE,
			resizable = TRUE, 
			wrap = TRUE,
			searchable = TRUE,
			style = list(fontSize = "1.25rem"),
			showSortIcon = TRUE,
			height = 800,
			pagination = FALSE
		)
	
}

###############################################################################
# Change time series to percent anomaly
###############################################################################

percAnomaly <- function(y, average){
	return((y - average)/average * 100)
}

###############################################################################
# Plot expansion factors 
###############################################################################

plot_expansions <- function(
		speciesname = "Chum",
		regionname = "Haida Gwaii", # For display
		region_spawners, # rds saved output 
		expansion_factors, # rds saved output 
		added_series = NULL, # added time series with columns year and value if desired
		exp_ylim = NULL # To explicitly control y axis if needed
		){

	col.exp <- viridisLite::viridis(n = 3, alpha = 0.8)
	
	s <- match(speciesname, c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead"))
	yrs <- as.numeric(dimnames(region_spawners)[[3]])
	
	par(mfrow = c(2,1), mar = c(3,4,2,1), oma = c(0,0,2,0))
	
	if(sum(!is.na(region_spawners[2, s,])) > 0){
		
		plot(yrs, region_spawners[2, s,]*10^-3, "o", lwd = 1.2, pch = 19, cex = 0.8, bty = "l", las = 1, 
				 xlab = "", ylab = "Spawners (thousands)", ylim = c(0, max(region_spawners[2, s,]*10^-3, na.rm = TRUE)), 
				 xpd = NA, yaxs = "i")
		abline(v = 2024, lty = 3, lwd = 1.2)
		text(2024, par('usr')[4], pos = 3, 2024, cex = 0.8, xpd = NA)
		abline(v = seq(1940, 2025, 5), lty = 3, col = grey(0.8), lwd = 0.8)
		abline(h = pretty(region_spawners[2, s, ]*10^-3), lty = 3, col = grey(0.8), lwd = 0.8)
		points(yrs, region_spawners[1, s, ]*10^-3,  col = col.exp[1], xpd = NA, pch = 21, bg = "white", cex = 0.8)
		mtext(side = 3, outer = TRUE, paste0(regionname, " ", speciesname))
		mtext(side = 3, adj = 0, line = 0.5, "(a)")
		
		if(is.null(added_series) == FALSE){
			points(added_series$year, added_series$value*10^-3, pch = 1, cex = 1, lwd = 1.2, col = col.exp[2], xpd = NA)
			legend("topleft", pch = c(19, 21, 1), pt.cex = c(0.8, 0.8, 1), pt.bg = c(NA, "white", NA), col = c(1,col.exp[1],col.exp[2]), c("Expanded", "Observed (indicator)", "Alternate"), bg = "white", lwd = c(1.2, NA, NA))
		} else {
			legend("topleft", pch = c(19, 21), col = c(1,col.exp[1]), c("Expanded", "Observed (indicator)"), bg = "white", pt.bg = "white", lwd = c(1.2, NA))
		}
		
		if(is.null(exp_ylim)) exp_ylim <- c(1, max(expansion_factors[[s]]$exp1[is.finite(expansion_factors[[s]]$exp1)]) + 1)
		
		plot(yrs, expansion_factors[[s]]$exp1, las = 1, ylim = exp_ylim, ylab = "Expansion Factor 1", xlab = "", bty = "l", pch = 19, cex = 0.8)
		abline(v = seq(1940, 2025, 5), lty = 3, col = grey(0.8), lwd = 0.8)
		abline(h = pretty(expansion_factors[[s]]$exp1), lty = 3, col = grey(0.8), lwd = 0.8)
		abline(h = 1, col = col.exp[3], lwd = 2)
		mtext(side = 3, adj = 0, line = 0.5, "(b)")
		
		# if(length(expansion_factors[[s]]$exp2) == 1){
		# 	exp2.s <- rep(expansion_factors[[s]]$exp2, length(yrs))
		# } else {
		# 	exp2.s <- expansion_factors[[s]]$exp2
		# }
		# 
		# plot(yrs, exp2.s,  las = 1, ylab = "Expansion Factor 2", xlab = "", bty = "l")
		# abline(v = seq(1940, 2025, 5), lty = 3, col = grey(0.6))
		# abline(h = pretty(expansion_factors[[s]]$exp1), lty = 3, col = grey(0.6))
		# mtext(side = 3, adj = 0, line = 0.5, "(c)")
	}
	
}

