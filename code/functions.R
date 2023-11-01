###############################################################################
# Functions to summarize spawners through time for SPS reporting
# called on in docs/methods-results.Rmd 
# Author: Steph Peacock
# Date: July 10, 2023

###############################################################################

###############################################################################
# Basic runsize plot
###############################################################################
plot_abund <- function(sps_data_subset, cols = c("#887A52", "#416191")){
	
	plot(sps_data_subset$year, sps_data_subset$spawners*10^-3, "n", xlab = "", ylab = "Abundance (thousands)", las = 1, ylim = c(0, max(c(sps_data_subset$runsize, sps_data_subset$spawners)*10^-3, na.rm = TRUE)), bty = "l")
	abline(v = seq(1950, 2025, 5), col = grey(0.8), lwd = 0.8)
	abline(v = seq(1950, 2025, 1), col = grey(0.8), lwd = 0.8, lty = 3)
	
	# Add spawners
	abline(h = mean(sps_data_subset$smoothedSpawners*10^-3, na.rm = TRUE), col = cols[2], lty = 2)
	lines(sps_data_subset$year, sps_data_subset$spawners*10^-3, col = cols[2], lwd = 0.5, xpd = NA)
	lines(sps_data_subset$year, sps_data_subset$smoothedSpawners*10^-3, col = cols[2], lwd = 2, xpd = NA)

	# Add run size
	if(sum(!is.na(sps_data_subset$runsize)) > 0){
		abline(h = mean(sps_data_subset$smoothedRunsize*10^-3, na.rm = TRUE), col = cols[1], lty = 2)
	lines(sps_data_subset$year, sps_data_subset$runsize*10^-3, col = cols[1], xpd = NA, lwd = 0.5)
	lines(sps_data_subset$year, sps_data_subset$smoothedRunsize*10^-3, col = cols[1], lwd = 2, xpd = NA)
	legend("topright", lwd = 2, col = cols, c("Run size", "Spawners"), bty = "n")
	
	} else {
		legend("topright", lwd = 2, col = cols[2], c("Spawners"), bty = "n")
	}
	
	mtext(side = 3, line = 2, paste(unique(sps_data_subset$region), unique(sps_data_subset$species)))

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
			S <- abund[which(yrs %in% smoothYrs)] + 0.01
			# Add 0.01 to spawners so that geometric mean is not zero for multiple years if there is an observation of zero?
			
			
			N <- sum(!is.na(S)) # number of years with data
			if(N > 0){ # If there are no data, leave as NA
				smoothedAbund[k] <- prod(S, na.rm = TRUE) ^ (1/N)
			}
		} # end k years
	
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
	species <- sort(unique(ss$species[!is.na(ss$smoothedSpawners)]))
	n.species <- length(species)
	
	# Loop through species to calculate percent decline in the most recent generation
	max.yrs <- tapply(ss$year[!is.na(ss$smoothedSpawners)], ss$species[!is.na(ss$smoothedSpawners)], max)
	percDecline <- rep(NA, n.species); names(percDecline) <- species
	for(s in 1:n.species){
		# Extract relevant generation length
		g <- genLength$gen_length[which(genLength$region == selected_region & genLength$species == species[s])]
		
		# Compute percent decline
		percDecline[s] <- (ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max.yrs[s])] - ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max.yrs[s] - g)]) / ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max.yrs[s] - g)]
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
	sps_dat,	# data frame from compile-regional-data.R
	sp_cols # vector of colours for species lines
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
	
	# Calculate abundanxce relative to long-term average
	dat$relativeAbund <- NA
	for(s in 1:n.species){
		dat$relativeAbund[dat$species == species_withData[s]] <- dat[dat$species == species_withData[s], 3]/mean(dat[dat$species == species_withData[s], 3], na.rm = TRUE)
	}

	# Set margins
	par(mar = c(4,5,2,11))
	
	# Initiate blank plot
	plot(range(dat$year), c(min(dat[dat$year > 1970, 4]), quantile(dat[dat$year > 1970, 4], 0.99, na.rm = TRUE)), "n", las = 1, ylab = "", xlab = "", main = paste(selected_region, c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]), bty = "l")
	abline(v = seq(1950, 2025, 10), col = grey(0.8), lwd = 0.5)
	abline(v = seq(1950, 2025, 2), col = grey(0.8), lty = 3, lwd = 0.5)
	
	# Store plotting window extent for polygons and text positioning
	u <- par("usr")
	
	# Add y-axis label
	mtext(side = 2, line = 3, paste0("Index of ", abund))
	
	# Line for y < 1 (less than long-term average)
	segments(x0 = u[1], x1 = u[2] + 0.2 * (u[2]-u[1]), y0 = 1, y1 = 1, col = grey(0.6), lty = 2, xpd = NA)
	# segments(x0 = u[1], x1 = u[2] + 0.05 * (u[2]-u[1]), y0 = u[3], y1 = u[3], col = 1, xpd = NA)
	arrows(x0 = rep(u[2] + 0.1 * (u[2]-u[1]), 2), 
				 x1 = rep(u[2] + 0.1 * (u[2]-u[1]), 2), 
				 y0 = 1, 
				 y1 = 1 + c(-1, 1)*(u[4] - u[3])/10, 
				 length = 0.08, lwd = 1.5, xpd = NA, col = grey(0.6))
	
	text(rep(u[2] + 0.2 * (u[2]-u[1]), 2), 1 + c(-1, 1)*(u[4] - u[3])/10, pos = c(1,3), c("Below\nhistorical\naverage", "Above\nhistorical\naverage"), xpd = NA, col = grey(0.6), cex = 0.8)

	# Add lines for each species
	for(s in 1:n.species){
		lines(dat$year[dat$species == species_withData[s]],
					dat$relativeAbund[dat$species == species_withData[s]], 
					col = sp_cols[species_withData[s]], lwd = 2, xpd = NA)
	}
	
	max.yrs <- tapply(dat$year[!is.na(dat$relativeAbund)], dat$species[!is.na(dat$relativeAbund)], max)
	for(s in 1:n.species){
		g <- sps_metrics$generation_length[sps_metrics$region == selected_region & sps_metrics$species == species_withData[s] & sps_metrics$type == c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]]
		
		points(max.yrs[species_withData[s]], dat$relativeAbund[which(dat$species == species_withData[s] & dat$year == max.yrs[species_withData[s]])], col = sp_cols[species_withData[s]], pch = 19, cex = 1.5, xpd = NA)
		
		# points(max.yrs[species_withData[s]] - g, dat$relativeAbund[which(dat$species == species_withData[s] & dat$year == (max.yrs[species_withData[s]] - g))], col = sp_cols[species_withData[s]], pch = 21, bg = "white", cex = 1.5, lwd = 2)
		
	}
	
	# Calculate percent decline from historical average
	status <- sps_metrics$status[sps_metrics$region == selected_region & sps_metrics$species %in% species_withData & sps_metrics$type == c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]]
	
	# Add plus if positive
	percDecline <- paste0(round(status*100),"%")
	
	percDecline[which(percDecline > 0)] <- paste0("+", percDecline[which(status > 0)])
	
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
	text(u[2], y, paste0(species_withData, " (", percDecline, ")"), col = sp_cols[species_withData], xpd = NA, adj = 0, font = 2)
	
	} else { # If no data
		par(mar = c(4,5,2,11))
		
		# Initiate blank plot
		plot(c(1950,2022), c(0, 2), "n", las = 1, ylab = "", xlab = "", main = paste(selected_region, c("Spawners", "Run Size")[as.numeric(abund == "runsize") + 1]), bty = "l")
	text(mean(c(1950,2022)), 1, font = 2, "Data Deficient", cex = 2, col = grey(0.6))
			
		# Add y-axis label
		mtext(side = 2, line = 3, paste0("Index of ", abund))
	}

}

###############################################################################
# Plot regional expansion factors for all species through time
###############################################################################

plot.regional_expansion <- function(
		selected_region, # Select region for which to create multi-species plot
		spawners, # Data frame with (at minimum) fields for year, region, species, and abundance
		sp_cols # vector of colours for species lines
){
	
	# Subset selected species
	ss <- spawners[which(spawners$region == selected_region & spawners$species != "Steelhead"), c("species", "year", "expansion_factor1", "expansion_factor2")]
	yrs <- sort(unique(ss$year))
	species <- sort(unique(ss$species))
	n.species <- length(species)
	
	# Set margins
	par(mar = c(4,5,2,10))
	
	# Initiate blank plot
	plot(range(yrs), c(1, 5), "n", las = 1, ylab = "Expansion Factors", xlab = "", main = selected_region, bty = "l", xlim = c(1970, 2021))
	
	
	# Add species lines
	for(s in 1:length(species)){
		# Expansion factor 1
		lines(ss$year[ss$species == species[s]], ss$expansion_factor1[ss$species == species[s]], col = sp_cols[species[s]], lwd = 2)
		
		# Expansion factor 2
		lines(ss$year[ss$species == species[s]], ss$expansion_factor2[ss$species == species[s]], col = sp_cols[species[s]], lty = 2)
	}
	
	# Add legend
	legend("topleft", fill = sp_cols, legend = species, border = NA)
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
	metric1 <- paste0(round(dat$status*100, 1),"%")
	metric1[which(dat$status > 0)] <- paste0("+", metric1[which(dat$status > 0)])
	metric1[is.na(dat$status)] <- ""
	
	metric1_cat <- rep(NA, dim(dat)[1])
	metric1_cat[which(dat$status < 0)] <- "arrow-down"
	metric1_cat[which(dat$status > 0)] <- "arrow-up"
	metric1_cat[which(dat$status == 0)] <- "arrows-left-right"
	
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
		Index = dat$type,
		
		metric1 = metric1_cat,
		metric1_perc = metric1,
		
		metric2 = dat$long_trend_cat,
		metric2_perc = metric2,
		
		metric3 = dat$short_trend_cat,
		metric3_perc = metric3,
		
		current_abund = prettierNum(round(dat$current)), 
		lastgen_abund = prettierNum(round(dat$prevGen)),
		avg_abund = prettierNum(round(dat$hist)),
		
		num_indicator = ifelse(is.na(dat$n_indicator)|dat$n_indicator == 0, "", dat$n_indicator),
		
		num_nonindicator = ifelse(is.na(dat$n_nonindicator)|dat$n_nonindicator == 0, "", dat$n_nonindicator),
		
		gen_length = ifelse(is.na(dat$generation_length), "", dat$generation_length)
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
	
	# Define species levels
	# species <- sort(unique(btn$Species))
	
	# # function which returns background colour based on cell value (using colour map)
	# # also takes column name as an input, which allows to get max and min
	# stylefunc <- function(value, index, name) {
	# 	if(is.na(value)){
	# 		background <- "#FFFFFF"
	# 	} else if(value > 0){
	# 		background <- "#83B68680"
	# 	} else if(value < 0){
	# 		background <- "#C0626380"
	# 	}
	# 	list(background = background, fontWeight = "bold")
	# }
	# 
	# # list giving column formatting (using style function) for single column
	# coldefs <- list(
	# 	reactable::colDef(style = stylefunc)
	# )
	# 
	# # replicate list to required length
	# coldefs <- rep(coldefs,length(regions))
	# 
	# # name elements of list according to cols
	# names(coldefs) <- regions
	# 
	
	# Render table
	btn %>%
		reactable(
		., 
		columns = list(
			Species = colDef(
				style = function(value){
					if(value == "Chinook"){
						background <- "#33228850"
					} else if(value == "Chum"){
						background <- "#44AA9950"
					}else if(value == "Coho"){
						background <- "#88CCEE50"
					}else if(value == "Pink"){
						background <- "#CC667750"
					}else if(value == "Sockeye"){
						background <- "#88225550"
					} else if(value == "Steelhead"){
						background <- "#DDCC7750"
					}
					list(background = background)
				},
				maxWidth = 100
			),
			
			metric1 = colDef(show = FALSE),
			metric1_perc = colDef(
				name = "Current Status",
				style = function(value, index){
					if(is.na(btn$metric1[index])){
						color <- "#FFFFFF"
						fontWeight <- "normal"
					} else if(btn$metric1[index] == "arrow-down"){
						color <- "#C06263"
						fontWeight <- "bold"
					} else if(btn$metric1[index] == "arrow-up"){
						color <- "#83B686"
						fontWeight <- "bold"
					} else if(btn$metric1[index] == "arrows-left-right"){
						color <- "#808080"
						fontWeight <- "normal"
					}
					list(color = color, fontWeight = fontWeight)
				}
			),
			
			metric2 = colDef(show = FALSE),
			metric2_perc = colDef(
				name = "LT Trend",
				style = function(value, index){
					if(is.na(btn$metric2[index])){
						color <- "#FFFFFF"
						fontWeight <- "normal"
					} else if(btn$metric2[index] == "arrow-down"){
						color <- "#C06263"
						fontWeight <- "bold"
					} else if(btn$metric2[index] == "arrow-up"){
						color <- "#83B686"
						fontWeight <- "bold"
					} else if(btn$metric2[index] == "arrows-left-right"){
						color <- "#808080"
						fontWeight <- "normal"
					}
					list(color = color, fontWeight = fontWeight)
				}
			),
			
			metric3 = colDef(show = FALSE),
			metric3_perc = colDef(
				name = "3-Gen Trend",
				style = function(value, index){
					if(is.na(btn$metric3[index])){
						color <- "#FFFFFF"
						fontWeight <- "normal"
					} else if(btn$metric3[index] == "arrow-down"){
						color <- "#C06263"
						fontWeight <- "bold"
					} else if(btn$metric3[index] == "arrow-up"){
						color <- "#83B686"
						fontWeight <- "bold"
					} else if(btn$metric3[index] == "arrows-left-right"){
						color <- "#808080"
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
			num_indicator = colDef(
				name = "Indicator",
				maxWidth = 80),
			num_nonindicator = colDef(
				name = "Non-indicator",
				maxWidth = 80),
			gen_length = colDef(
				name = "Generation length",
				maxWidth = 80)
		),
		
		columnGroups = list(
			colGroup(name = "Metrics of change", columns = c("metric1_perc", "metric2_perc", "metric3_perc")),
			
			colGroup(name = "Index of abundance", columns = c("current_abund", "lastgen_abund", "avg_abund")),
			colGroup(name = "Number of monitored streams", columns = c("num_indicator", "num_nonindicator"))
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
		sps_metrics_tab = sps_metrics[which(sps_metrics$type == "Spawners"), c("region", "species", "status")] # data frame with region, species, metric
){
	
	regions <- unique(sps_metrics_tab$region)
	tab_long <- data.frame(
		species = unique(sps_metrics_tab$species)
	)
	
	for(i in 1:length(regions)){
		tab_long <- cbind(tab_long, sps_metrics_tab[which(sps_metrics_tab$region == regions[i]), 3])
	}
	names(tab_long) <- c("Species", regions)
	
	tab_long[which(tab_long$Species %in% c("Pink", "Sockeye", "Steelhead")), "Yukon"] <- -989898
	
	tab_long[which(tab_long$Species %in% c("Pink", "Chum", "Coho")), "Columbia"] <- -989898
	
	# tab_long_col <- ifelse(tab_long[, c(2:(dim(tab_long)[2]))] < 0, "#C06263", "#83B686")
	# 
	# tab <- cbind(tab_long, tab_long_col)
	# names(tab)[11:19] <- paste0("col_", names(tab)[11:19])
	
	# function which returns background colour based on cell value (using colour map)
	# also takes column name as an input, which allows to get max and min
	stylefunc <- function(value, index, name) {
		if(is.na(value)){
			color <- NA
			background <- "#FFFFFF"
		} else if(value == -989898){
			color <- "#000000"
			background <- "#000000"
		} else if(value > 0){
			color <- "#83B686"
			background <- "#83B68630"
		} else if(value < 0){
			color <- "#C06263"
			background <- "#C0626330"
		}
		list(color = color, background = background, fontWeight = "bold")
	}
	
	# list giving column formatting (using style function) for single column
	coldefs <- list(
		reactable::colDef(
			style = stylefunc, 
			format = colFormat(digits = 1, percent = TRUE),
			maxWidth = 77)
	)
	
	# replicate list to required length
	coldefs <- rep(coldefs,length(regions))
	
	# name elements of list according to cols
	names(coldefs) <- regions
	
	# Render table
	tab_long %>%
		reactable(
			., 
			columns = coldefs,
			columnGroups = list(
				colGroup(name = "Region", columns = c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia"))
				),
			bordered = TRUE,
			highlight = TRUE,
			striped = TRUE,
			resizable = TRUE,
			fullWidth = FALSE,
			wrap = TRUE,
			style = list(fontSize = "1.25rem"),
			defaultPageSize = 6
		)
	
}
