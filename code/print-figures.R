###############################################################################
# Code to produce figures for print report drawing on output daat
# Figures are output to output/print-figures
#
# Steph Peacock, June 13, 2025
###############################################################################

library(dplyr)
library(png)


fishy_cols <- c("#9c2323", "#b85657", "#ffcaca", "#98df8a", "#54a35c", "#299330")
fishy_bgcols <- c(bg = "#F1F8F9", dd = "#EFEFEF", crit = "#EBC8CD")

#' Create a linear scale for colours negative values in red and green positive
#' @param {number} value - The value to use as input for the scale of current abundance as a percent (-100 to 150)
#' @return {string} The corresponding hex color for the given value.

fishy_cols_func <- function(x){
	col_out <- NA
	if(is.na(x)){
		col_out <- "#A8A9AB"
	} else if (x == -999999){
		col_out <- "#600918"
	} else if(x < 0){
		col_out <- colorRampPalette(fishy_cols[c(3:1)])(n = 100)[round(abs(x))]
	}	else if (x >=0 & x <= 150){
		col_out <- colorRampPalette(fishy_cols[c(4:6)])(n = 150)[round(abs(x))]
	} else if(x > 150){
		col_out <- fishy_cols[6]
	}
	return(col_out)
}

###############################################################################
# Load data
###############################################################################

# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data.csv")
trends_plotting <- read.csv("output/sps-trends_plotting.csv")
sps_summary <- read.csv("output/sps-summary.csv")

# What is the range of current status outcomes for fishy dot plot?
range(sps_summary$current_status, na.rm = TRUE)

species <- unique(sps_summary$species)
regions <- unique(sps_summary$region)
region_abbr <- c("YT", "TB", "HG", "NA", "SK", "CC", "EV", "WV", "FR", "CO")

###############################################################################
# Fishy dot plot
###############################################################################

# Load fish icons
fishes <- list(
	readPNG("data/fish-icons/red100.png"),
	readPNG("data/fish-icons/red50.png"),
	readPNG("data/fish-icons/red0.png"),
	readPNG("data/fish-icons/green0.png"),
	readPNG("data/fish-icons/green50.png"),
	readPNG("data/fish-icons/green100.png"),
	readPNG("data/fish-icons/green150.png"),
	readPNG("data/fish-icons/crit.png"),
	readPNG("data/fish-icons/grey.png")
)


y_range <- c(-100, 150)
fishwidth <- 0.45
fishheight <- 2.2

pdf(file = "output/print-figures/fishy-dot-plot_Spawners.pdf", width = 6.5, height = 8, pointsize = 10)
quartz(width = 7, height = 8.5, pointsize = 10)

par(mar = rep(0, 4), mfrow = c(1,1))
plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0,6), ylim = c(-200, 200), xaxs = "i", yaxs = "i")
polygon(x = c(0, 0, 6, 6), y = c(-100, 150, 150, -100), col = fishy_bgcols[1], border = NA)
polygon(x = c(0, 0, 6, 6), y = c(-100, -150, -150, -100), col = fishy_bgcols[3], border = NA)
polygon(x = c(0, 0, 6, 6), y = c(-200, -150, -150, -200), col = fishy_bgcols[2], border = NA)

abline(v = seq(0, 6, 1), lwd = 5, col = "white")
abline(h = c(-100, -150, -200), lwd = 6, col = "white")
abline(h = 0, lty = 2, col = grey(0.6))


for(s in 1:6){
	species.s <- species[s]
	sps_summary.s <- sps_summary %>% 
		filter(species == species.s & type == "Spawners") %>%
		arrange(1/current_status)
	
	for(i in 1:dim(sps_summary.s)[1]){
		if(is.na(sps_summary.s$current_status[i])){
			# grey fish
		} else if(sps_summary.s$current_status[i] == -999999){
			#critical fish
		} else {
			
			x_loc <- s - 0.45 + 0.1 * sps_summary.s$status_offset_x[i] # tail
			y_loc <- sps_summary.s$current_status[i] + sps_summary.s$status_offset_y[i]
			z <- findInterval(sps_summary.s$current_status[i], c(-100, -50, -10, 0, 50, 100, 150))
			
			rasterImage(fishes[[z]], 
									xleft = x_loc, 
									ybottom = y_loc - fishheight/0.5, 
									xright = x_loc - fishwidth,
									ytop = y_loc + fishheight/0.5)
			
			text(s - 0.3, y_loc + sps_summary.s$region_label_offset_y[i], paste0(sps_summary.s$current_status[i], "%"), font = 2, cex = 0.7)
			text(s - 0.1, y_loc + sps_summary.s$region_label_offset_y[i], region_abbr[match(sps_summary.s$region[i], regions)], col = grey(0.6), font = 2, cex = 0.7)
			
		}
		} # end i
	} # end s
dev.off()