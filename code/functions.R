###############################################################################
# Functions to summarize spawners through time for SPS reporting
# called on in docs/methods-results.Rmd 
# Author: Steph Peacock
# Date: July 10, 2023

###############################################################################

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
	species <- sort(unique(ss$species))
	n.species <- length(species)
	
	# Loop through species to calculate percent decline in the most recent generation
	percDecline <- rep(NA, n.species); names(percDecline) <- species
	for(s in 1:n.species){
		# Extract relevant generation length
		g <- genLength$gen_length[which(genLength$region == selected_region & genLength$species == species[s])]
		
		# Compute percent decline
		percDecline[s] <- (ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max(yrs))] - ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max(yrs) - g)]) / ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max(yrs) - g)]
	}
	
	return(percDecline)
}

###############################################################################
# Plot regional index of spawner abundance for all species through time
###############################################################################

plot.regional_spawners <- function(
		selected_region, # Select region for which to create multi-species plot
		spawners, # Data frame with (at minimum) fields for year, region, species, and abundance
		baseline = "average",  # Baseline for calculating relative spawners; can be a year or "average"
		genLength, # data frame of generation length for all regions and species
		sp_cols # vector of colours for species lines
){
	
	# Subset selected_region
	ss <- spawners[which(spawners$region == selected_region), c("species", "year", "smoothedSpawners")]
	yrs <- sort(unique(ss$year))
	species <- sort(unique(ss$species))
	n.species <- length(species)
	
	# Calculate spawners relative to baseline
	ss$relativeSpawners <- NA
	for(s in 1:length(species)){
		if(is.numeric(baseline)){ # If baseline is a year
			ss.baseline <- ss$smoothedSpawners[which(ss$species == species[s] & ss$year == baseline)]
		} else if (baseline == "average"){
			ss.baseline <- mean(ss$smoothedSpawners[which(ss$species == species[s])], na.rm = TRUE)
		}
		
		if(!is.na(ss.baseline)){
			ss$relativeSpawners[ss$species == species[s]] <- ss$smoothedSpawners[ss$species == species[s]]/ss.baseline
		} else {
			warning(paste(selected_region, species[s], ": no data for baseline", baseline))
		}
	}
	
	# Change y-axis label depending on what baseline is being used
	if(is.numeric(baseline)){ # If baseline is a year
		ylab_baseline <- paste0("Spawners relative to ", baseline)
	} else if (baseline == "average"){
		ylab_baseline <- "Index of spawner abundance"
	}
	
	# Set margins
	par(mar = c(4,5,2,11))
	
	# Initiate blank plot
	plot(range(yrs), c(0, quantile(ss$relativeSpawners[ss$year > 1970], 0.99, na.rm = TRUE)), "n", las = 1, ylab = "", xlab = "", main = selected_region, bty = "l", xlim = c(1970, 2021))
	
	# Store plotting window extent for polygons and text positioning
	u <- par("usr")
	
	# Add y-axis label
	mtext(side = 2, line = 3, ylab_baseline)
	
	# Add polygon/line for y < 1 (less than long-term average)
	polygon(x = c(u[c(1, 1)], 2050, 2050), y = c(u[3], 1, 1, u[3]), border = NA, col = grey(0.8), xpd = NA)
	segments(x0 = u[1], x1 = 2050, y0 = 1, y1 = 1, col = grey(0.6), xpd = NA)
	segments(x0 = u[1], x1 = 2050, y0 = u[3], y1 = u[3], col = 1, xpd = NA)
	arrows(x0 = rep(2043, 2), x1 = rep(2043, 2), y0 = 1, y1 = 1 + c(-1, 1)*(u[4] - u[3])/10, length = 0.08, lwd = 1.5, xpd = NA, col = c("white", grey(0.6)))
	text(rep(2043, 2), 1 + c(-1, 1)*(u[4] - u[3])/10, pos = c(1,3), c("Below\nhistorical\naverage", "Above\nhistorical\naverage"), xpd = NA, col = c("white", grey(0.6)), cex = 0.8)

	# Add lines for each species
	for(s in 1:length(species)){
		lines(ss$year[ss$species == species[s]], ss$relativeSpawners[ss$species == species[s]], col = sp_cols[species[s]], lwd = 2)
	}
	
	# # Calculate percent decline over most recent generation
	# percDecline <- calcPercDecline(selected_region = selected_region,
	# 															 spawners = spawners,
	# 															 genLength = genLength)
	
	for(s in 1:n.species){
		g <- genLength$gen_length[which(genLength$region == selected_region & genLength$species == species[s])]
		points(max(yrs), ss$relativeSpawners[which(ss$species == species[s] & ss$year == max(yrs))], col = sp_cols[s], pch = 19, cex = 1.5)
		points(max(yrs) - g, ss$relativeSpawners[which(ss$species == species[s] & ss$year == max(yrs) - g)], col = sp_cols[s], pch = 21, bg = "white", cex = 1.5, lwd = 2)
		
	}
	
	# # Add plus if positive
	# percDecline2 <- round(percDecline*100)
	# percDecline2[which(percDecline2 > 0)] <- paste0("+", percDecline2[which(percDecline2 > 0)])
	
	# Calculate percent decline from historical average
	LTmean <- tapply(ss$smoothedSpawners, ss$species, mean, na.rm = TRUE)
	percDecline <- (ss$smoothedSpawners[ss$year == max(yrs)] - LTmean)/LTmean
	
	# Add plus if positive
	percDecline2 <- paste0(round(percDecline*100),"%")
	percDecline2[which(percDecline > 0)] <- paste0("+", percDecline2[which(percDecline > 0)])
	
	# Order labels on right from highest to lowest most recent abundance
	o <- order(ss$relativeSpawners[ss$year == max(yrs)])
	
	# Calculate y-axis positioning for labels (don't want them too squished)
	y <- ss$relativeSpawners[ss$year == max(yrs)]
													 
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
	text(max(yrs) + 3, y, paste0(species, " (", percDecline2, ")"), col = sp_cols, xpd = NA, adj = 0, font = 2)
	

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
	ss <- spawners[which(spawners$region == selected_region), c("species", "year", "expansion_factor1", "expansion_factor2")]
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

byTheNumbers <- function(
		selected_region, # Select region for which to create multi-species plot
		spawners, # Data frame with (at minimum) fields for year, region, species, and abundance
		genLength # data frame of generation length for all regions and species
		){
	
	# Subset spawners for selected_region
	ss <- spawners[which(spawners$region == selected_region), c("species", "year", "smoothedSpawners")]
	yrs <- sort(unique(ss$year))
	species <- sort(unique(ss$species))
	n.species <- length(species)
	
	# Calculate percent decline in the most recent generation
	percDecline <- calcPercDecline(selected_region = selected_region,
																 spawners = spawners,
																 genLength = genLength)
	
	# Add plus if positive
	percDecline2 <- paste0(round(percDecline*100, 1),"%")
	percDecline2[which(percDecline2 > 0)] <- paste0("+", percDecline2[which(percDecline2 > 0)])
	percDecline2[is.na(percDecline)] <- ""
	
	# Calculate percent decline from historical average
	LTmean <- tapply(ss$smoothedSpawners, ss$species, mean, na.rm = TRUE)
	percDeclineH <- (ss$smoothedSpawners[ss$year == max(yrs)] - LTmean)/LTmean
	
	# Add plus if positive
	percDeclineH2 <- paste0(round(percDeclineH*100, 1),"%")
	percDeclineH2[which(percDeclineH2 > 0)] <- paste0("+", percDeclineH2[which(percDeclineH2 > 0)])
	percDeclineH2[is.na(percDeclineH)] <- ""
	
	# Calculate last-gen spawners
	lastGen <- rep(NA, n.species)
	for(s in 1:n.species){
		g <- genLength$gen_length[which(genLength$region == selected_region & genLength$species == species[s])]
		lastGen[s] <- round(ss$smoothedSpawners[which(ss$species == species[s] & ss$year == max(yrs) - g)])
	}
	
	# Compile table of summary
	btn <- data.frame(
		species = species,
		LT_trend = ifelse(percDeclineH < 0, "arrow-down", "arrow-up"),
		LT_trend_perc = percDeclineH2,
		LT_trend_color = ifelse(percDeclineH < 0, "#C06263", "#83B686"),
		population_trend = ifelse(percDecline < 0, "arrow-down", "arrow-up"),
		population_trend_perc = percDecline2,
		population_trend_color = ifelse(percDecline < 0, "#C06263", "#83B686"),
		current_spawners = prettierNum(round(ss$smoothedSpawners[ss$year == max(yrs)])),
		avg_spawners = prettierNum(round(tapply(ss$smoothedSpawners, ss$species, mean, na.rm = TRUE))),
		lastgen_spawners = prettierNum(lastGen)
		# num_indicator = NA,
		# num_nonindicator = NA
	)
	rownames(btn) <- NULL
	
	# Change NAs to blanks for table rendering
	btn[which(is.na(btn), arr.ind = TRUE)] <- " "
	
	# Except keep NAs for icon colour and type to avoid warning (" " not a colour)
	btn$population_trend_color[btn$population_trend_color == " "] <- NA
	btn$LT_trend_color[btn$LT_trend_color == " "] <- NA
	
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
		spawners, # Data frame with (at minimum) fields for year, region, species, and abundance
		genLength # data frame of generation length for all regions and species
){
	
	# Compile dataframe of numners
	btn <- byTheNumbers(selected_region = selected_region,
											spawners = spawners,
											genLength = genLength)
	
	# Define species levels
	species <- btn$species
	
	# Render table
	btn %>%
		reactable(
		., 
		columns = list(
			species = colDef(
				name = "Species",
				style = function(value){
					if(value == "Chinook"){
						background <- "#44015450"
					} else if(value == "Chum"){
						background <- "#3B528B50"
					}else if(value == "Coho"){
						background <- "#21908C50"
					}else if(value == "Pink"){
						background <- "#5DC86350"
					}else if(value == "Sockeye"){
						background <- "#FDE72550"
					}
					list(background = background)
				},
				maxWidth = 100
			),
			population_trend = colDef(
				cell = icon_sets(., icon_ref = "population_trend", icon_position = "over", icon_size = 28, icon_color_ref = "population_trend_color"),
				name = "Direction",
				align = "center",
				maxWidth = 80),
			population_trend_perc = colDef(
				name = "Percent change",
				maxWidth = 80),
			population_trend_color = colDef(
				show = FALSE
			),
			LT_trend = colDef(
				cell = icon_sets(., icon_ref = "LT_trend", icon_position = "over", icon_size = 28, icon_color_ref = "LT_trend_color"),
				name = "Direction",
				align = "center",
				maxWidth = 80),
			LT_trend_perc = colDef(
				name = "Percent change",
				maxWidth = 80),
			LT_trend_color = colDef(
				show = FALSE
			),
			current_spawners = colDef(
				name = "Current",
				maxWidth = 100),
			lastgen_spawners = colDef(
				name = "Previous generation",
				maxWidth = 100),
			avg_spawners = colDef(
				name = "Historical average",
				maxWidth = 100)
		),
		columnGroups = list(
			colGroup(name = "Index of spawner abundance", columns = c("current_spawners", "lastgen_spawners", "avg_spawners")),
			colGroup(name = "Change from previous generation", columns = c("population_trend", "population_trend_perc")),
			colGroup(name = "Change from historical average", columns = c("LT_trend", "LT_trend_perc"))
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
		style = list(fontSize = "1.25rem")
	)
	
}
