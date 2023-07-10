###############################################################################
# Plotting spawners abundance for each region and species, calculating
# percent decline over most recent generation
# 
# July 6, 2023
# Steph Peacock (speacock@psf.ca)
###############################################################################

# Source PSF and SWP colour palettes
source("code/colours.R")

sp_cols <- SWP_cols[c('salmon', 'clay', 'soil2', 'soil1', 'tidal', 'greengrey')]

###############################################################################
# Load regional spawner abundance
###############################################################################

# Arrange regions from north to south
regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

# Select region
r <- "Central Coast"

# source("code/regional-expansions.R")

spawners <- readRDS(paste0("output/", r, "-spawners.rds"))

###############################################################################
# Smoothing
###############################################################################

# Smooth abundance using the geometric running mean over the generation length

genLength <- c(6, 4, 4, 2, 5); names(genLength) <- species
# Calculate geometric average
smoothedSpawners <- array(NA, dim = c(length(species), length(y)))
for(s in 1:length(species)){
	# g <- genLength[s]#*3
	g <- round(20 / genLength[s]) * genLength[s]
	for(k in 1:length(y)){ # For each year
		smoothYrs <- c(max(y[1], y[k] - g + 1):y[k]) # define previous years over which to smooth
		# Unweighted geometric mean
		smoothedSpawners[s, k] <- prod(spawners[2, s, y %in% smoothYrs]) ^ (1/sum(y %in% smoothYrs))
	}
}

for(s in 1:length(species)){
	par(mar = c(4,4,2,10))
	plot(y, spawners[1, s, ] * 10^-6, "l", col = grey(0.8), lwd = 2, las = 1, ylim = range(spawners[, s, ]* 10^-6), ylab = "Spawners (millions)", main = paste("Skeena", species[s]))
	lines(y, spawners[2, s, ]* 10^-6, col = col_sp[3])
	
	
	lines(y, smoothedSpawners[s, ] * 10^-6, col = col_sp[2], lwd = 2)
	
	u <- par('usr')
	# legend(u[2] + 0.01*(u[2]-u[1]), u[4], col = c(grey(0.8), col_sp[3]), lwd = c(2, 1), c("Observed", "Estimated"), xpd = NA, bty = "n")
	legend(u[2] + 0.01*(u[2]-u[1]), u[4], col = c(grey(0.8), col_sp[c(3,2)]), lwd = c(2, 1,2), c("Observed", "Estimated", "Smoothed"), xpd = NA, bty = "n")
	
}
