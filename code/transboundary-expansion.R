###############################################################################
# Transboundary expansions of spawner abundance
# Drawing on TTC survey data rather than NuSEDS spawner surveys
# August 15, 2023
# Steph Peacock (speacock@psf.ca)
###############################################################################

###############################################################################
# Import data
###############################################################################

# Data on spawner abundance in Transboundary watersheds has been extracted
# from Transboundary Technical Committee (TTC) reports by Gottfried Pestal
# (SOLV Consulting) and merged in the file TTC_MERGED.csv.
# See https://github.com/SOLV-Code/Transboundary_Data_Report/tree/main/DATA_PROCESSING/DATA/2_ExtractedData

tbr_spawners <- read.csv("https://raw.githubusercontent.com/SOLV-Code/Transboundary_Data_Report/main/DATA_PROCESSING/DATA/2_ExtractedData/TTC_MERGED.csv?token=GHSAT0AAAAAACGKYG3WEGUKV6324KYNAA24ZG3YCDA")

unique(tbr_spawners$SPECIES)
unique(tbr_spawners$Series[tbr_spawners$SPECIES == "Chinook" & tbr_spawners$Stock == "Alsek River"])

# Extract sources for each species and watershed
tbr_spawners$pop_id <- paste(tbr_spawners$SPECIES, tbr_spawners$Stock, tbr_spawners$Series)
tbr_spawners$pop_id <- as.numeric(factor(tbr_spawners$pop_id))

tbr_source <- data.frame(
	species = rep("Chinook")
)

# Example of Alsek Chinook series
ind <- which(tbr_spawners$SPECIES == "Chinook" & tbr_spawners$Stock == "Alsek River")
n_stocks <- length(unique(tbr_spawners$Series[ind]))
st_cols <- viridis(n = n_stocks)

quartz(width = 8, height = 6, pointsize = 12)
par(mar = c(4,4,8,1))

plot(tbr_spawners$Year[ind], tbr_spawners$Value[ind]*10^-3, "n", xlab = "", ylab = "Spawners (thousands)", las = 1, bty = "l")
mtext(side = 3, line = 6.5, paste(unique(tbr_spawners$Stock[ind]), unique(tbr_spawners$SPECIES[ind])), font = 2, cex = 1.5)
for(i in 1:n_stocks){
	points(tbr_spawners$Year[ind][which(tbr_spawners$Series[ind] == unique(tbr_spawners$Series[ind])[i])], tbr_spawners$Value[ind][which(tbr_spawners$Series[ind] == unique(tbr_spawners$Series[ind])[i])]*10^-3, "o", col = st_cols[i], pch = 19, cex = 0.6)	
}
u <- par('usr')
legend(u[1], 1.4*u[4], ncol = 2, pch = 19, col = st_cols, legend = unique(tbr_spawners$Series[ind]), xpd = NA, bty = "n")

