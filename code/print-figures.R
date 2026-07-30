###############################################################################
# Code to produce figures for print report drawing on output data
# Figures are output to output/print-figures
#
# Steph Peacock, June 13, 2025
###############################################################################

library(dplyr)
library(png)
library(recolorize)
library(raster)
library(magick)
library(extrafont)

file_type <- "pdf" # Choose one of "pdf" or "png"

# Import fonts
#font_import(paths = "data/print-report/fonts")
#loadfonts(device = "pdf")
#names(pdfFonts())[grepl("Sofia", names(pdfFonts()), ignore.case = TRUE)]

# # Trying to add .otf (Sofia Pro Condensed)
#library(showtext)
#font_add("Sofia Pro Condensed", regular = "data/print-report/fonts/SofiaProRegCond.otf")

#fonttable() %>% dplyr::filter(grepl("Sofia", FamilyName, ignore.case = TRUE)) %>% distinct(FamilyName)

# Help R system find ghostscript (needs to be installed if not done previously)
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs10.07.1/bin/gswin64c.exe")


# From James: Green: 96b584, Red: ad605f
fishy_cols <- c("#9c2323", "#b85657", "#ffcaca", "#98df8a", "#54a35c", "#299330")
crit_col <- c("#600918")
fishy_bgcols <- c(bg = "#f1f8fa", dd = "#f5f6f6", crit = "#f2dcd7", high = "#e7eee3")

# Create a linear scale for colours negative values in red and green positive
fishy_cols_func <- function(x){
	col_out <- rep(NA, length(x))
	for(i in 1:length(x)){
		if(is.na(x[i])){
			col_out[i] <- "#A8A9AB"
		} else if (x[i] == -999999){
			col_out[i] <- "#600918"
		} else if(x[i] < 0){
			col_out[i] <- colorRampPalette(c('#ffcaca', '#b85657', '#9c2323'))(n = 100)[round(abs(x[i]))]
		}	else if (x[i] >=0 & x[i] <= 150){
			col_out[i] <- colorRampPalette(c('#98df8a', '#54a35c', '#288330'))(n = 150)[round(abs(x[i]))]
		} else if(x[i] > 150){
			col_out[i] <- "#1B4E1F"
		}
	}
	return(col_out)
}

fishy_cols_func <- function(x){
	col_out <- rep(NA, length(x))
	
	for(i in 1:length(x)){
		if(is.na(x[i])){
			col_out[i] <- "#a6afb0"
		} else if (x[i] == -999999){
			col_out[i] <- "#9d5857"
		} else if(x[i] < 0){
			col_out[i] <- colorRampPalette(c("#d1aaa2",  '#ad605f'))(n = 100)[round(abs(x[i]))]
		}	else if (x[i] >=0 & x[i] <= 150){
			col_out[i] <- colorRampPalette(c('#c9d7bd', '#96b584'))(n = 150)[round(abs(x[i]))]
		} else if(x[i] > 150){
			col_out[i] <- "#89a67a"
		}
	}
	return(col_out)
}

# Function to control zero line
zero_line <- function(){
	abline(h = 0, lty = 2, col = grey(0.7), lwd = 0.5)
}
###############################################################################
# Load data
###############################################################################

# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data.csv")
trends_plotting <- read.csv("output/sps-trends_plotting.csv")
sps_summary <- read.csv("output/sps-summary.csv")

# Adjust Chinook labels
sps_summary$region_label_offset_y[sps_summary$species == "Chinook" & sps_summary$region %in% c("Skeena", "Yukon") & sps_summary$type == "Spawners"] <- c(-3, -1)

# What is the range of current status outcomes for fishy dot plot?
range(sps_summary$current_status, na.rm = TRUE)

species <- unique(sps_summary$species)
regions <- unique(sps_summary$region)
region_abbr <- c("YU", "TB", "HG", "NA", "SK", "CC", "EI", "WI", "FR", "CO")
type_name <- c("spawner", "total")

###############################################################################
# 1) Main fishy dot plot
###############################################################################

# Load fish icon
fish <- image_read("data/print-report/fish-icons/Fish.png")
fish_outline <- image_read("data/print-report/fish-icons/Fish_Outline_thin.png")

y_range <- c(-100, 150)
vert_space <- 0.06
green_range <- y_range[2] + 250/6 * c(vert_space, vert_space + 0.5)
crit_range <- y_range[1] - 250/6 * c(vert_space + 0.4, vert_space)
dd_range <- crit_range[1] - 250/6 * c(vert_space, vert_space + 1.2)

# Locations for data deficient fish (starting at top)
dd_y <- dd_range[1] - 5 - c(0:7)*5
green_y <- green_range[1] + 5 + c(0:2)*5

mar_width <- 0 # Inch for each axis
plot_height <- (max(green_range) - min(dd_range))* 6/250 + 1
plot_width <- 6 + 5*0.1 + mar_width

text_pad <- 0.15
text_cex <- 0.7

fishwidth <- 0.5
fishheight <- 0.5 * image_info(fish)$height/image_info(fish)$width

for(k in 1:2){

	# quartz(width = 2246/350, height = 2676/350, pointsize = 10, family = "Sofia Pro Bold")
	if(file_type == "png"){
		 png(file = paste0("output/print-figures/png/main_", type_name[k], ".png"), width = 2246, height = 2676, res = 350, pointsize = 10, family = "Sofia Pro Bold")
	} else if(file_type == "pdf"){
		pdf(file = paste0("output/print-figures/pdf/main_", type_name[k], ".pdf"), width = 2246/350, height = 2676/350, pointsize = 10, family = "Sofia Pro Bold")
	}
	
	
	par(mai = c(0, mar_width, mar_width, 0), mfrow = c(1,1), family = "Sofia Pro Bold")
	plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(min(dd_range), max(green_range)), xaxs = "i", yaxs = "i")
	
	polygon(x = c(0, 0, 6, 6), y = c(green_range, rev(green_range)), col = fishy_bgcols['high'], border = NA)
	polygon(x = c(0, 0, 6, 6), y = c(y_range, rev(y_range)), col = fishy_bgcols['bg'], border = NA)
	polygon(x = c(0, 0, 6, 6), y = c(crit_range, rev(crit_range)), col = fishy_bgcols['crit'], border = NA)
	polygon(x = c(0, 0, 6, 6), y = c(dd_range, rev(dd_range)), col = fishy_bgcols['dd'], border = NA)
	
	abline(v = seq(0, 6, 1), lwd = 5, col = "white")
	zero_line()
	
	for(s in 1:6){
		species.s <- species[s]
		type.k <- c("Spawners", "Total return")[k]
		sps_summary.s <- sps_summary %>% 
			filter(species == species.s & type == type.k) %>%
			arrange(1/current_status)
		
		# Data deficient
		na_ind <- which(is.na(sps_summary.s$current_status))
		if(length(na_ind) > 0){
			x_dd <- rep(c(s - 0.25, s - 0.45), ceiling(length(na_ind)))[1:length(na_ind)]
			
			for(i in 1:length(na_ind)){
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(NA)),
									xleft = x_dd[i] - xinch(fishwidth), 
									ybottom = dd_y[i] - yinch(fishheight)/2, 
									xright = x_dd[i],
									ytop = dd_y[i] + yinch(fishheight)/2)
			
			rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['dd']),
									xleft = x_dd[i] - xinch(fishwidth), 
									ybottom = dd_y[i] - yinch(fishheight)/2, 
									xright = x_dd[i],
									ytop = dd_y[i] + yinch(fishheight)/2)
			}
			
			text(s - text_pad, dd_y[1:length(na_ind)], region_abbr[match(sps_summary.s$region[na_ind], regions)], col = grey(0.6), font = 2, cex = text_cex)
			
		}
		
		# Critically low
		crit_ind <- which(sps_summary.s$current_status == -999999)
		if(length(crit_ind) > 0){
			x_crit <- s - 0.35
			y_crit <- mean(crit_range)
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(-999999)), 
									xleft = x_crit - xinch(fishwidth), 
									ybottom = y_crit - yinch(fishheight)/2, 
									xright = x_crit,
									ytop = y_crit + yinch(fishheight)/2)
			
			text(s - text_pad, y_crit, region_abbr[match(sps_summary.s$region[crit_ind], regions)], col = crit_col, font = 2, cex = text_cex)
			
		}
		
		# High
		high_ind <- which(sps_summary.s$current_status > 150)
		if(length(high_ind) > 0){
			
			high_ind <- high_ind[order(sps_summary.s$current_status[high_ind], decreasing = FALSE)]
			if(length(high_ind) == 1){
				x_high <- s - 0.45
				y_high <- mean(green_range)
				
			} else {
				x_high <- s - 0.45 #rep(c(s - 0.25, s - 0.55), ceiling(length(high_ind)))[1:length(high_ind)]
				y_high <- green_y
			}
			
			for(i in length(high_ind):1){
				
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(sps_summary.s$current_status[high_ind[i]])),
									xleft = x_high - xinch(fishwidth), 
									ybottom = y_high[i] - yinch(fishheight)/2, 
									xright = x_high,
									ytop = y_high[i] + yinch(fishheight)/2)
				
				rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['high']),
										xleft = x_high - xinch(fishwidth), 
										ybottom = y_high[i] - yinch(fishheight)/2, 
										xright = x_high,
										ytop = y_high[i] + yinch(fishheight)/2)
			}
			
			text(s - 0.33, y_high, paste0("+", sps_summary.s$current_status[high_ind], "%"), font = 2, cex = text_cex)
			text(s - 0.13, y_high, region_abbr[match(sps_summary.s$region[high_ind], regions)], col = grey(0.6), font = 2, cex = text_cex)
			
		}
		
		# Other with actual fish for current_status!
		cs_ind <- which(sps_summary.s$current_status >=-100 & sps_summary.s$current_status <= 150)
		cs_ind <- cs_ind[order(sps_summary.s$current_status[cs_ind] + sps_summary.s$region_label_offset_y[cs_ind], decreasing = TRUE)]
		x_loc <- s - 0.45 #0.45 + 0.1 * sps_summary.s$status_offset_x[cs_ind] # tail
		y_loc <- sps_summary.s$current_status[cs_ind] + sps_summary.s$region_label_offset_y[cs_ind]
		
		for(i in 1:length(cs_ind)){
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(sps_summary.s$current_status[cs_ind[i]])),
									xleft = x_loc - xinch(fishwidth), 
									ybottom = y_loc[i] - yinch(fishheight)/2, 
									xright = x_loc,
									ytop = y_loc[i] + yinch(fishheight)/2)
			
			rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['bg']),
									xleft = x_loc - xinch(fishwidth), 
									ybottom = y_loc[i] - yinch(fishheight)/2, 
									xright = x_loc,
									ytop = y_loc[i] + yinch(fishheight)/2)
		}
		
		text(s - 0.32, y_loc + sps_summary.s$region_label_offset_y[cs_ind], paste0(ifelse(sps_summary.s$current_status[cs_ind]>0, "+", ""), sps_summary.s$current_status[cs_ind], "%"), font = 2, cex = text_cex)
		text(s - 0.13, y_loc + sps_summary.s$region_label_offset_y[cs_ind], region_abbr[match(sps_summary.s$region[cs_ind], regions)], col = grey(0.6), font = 2, cex = text_cex)
		
	} # end s
	# quartz.save(paste0("output/print-figures/main_", type_name[k], ".pdf"), type = "pdf")
	dev.off()
	if(file_type == "pdf"){
		embed_fonts(paste0("output/print-figures/pdf/main_", type_name[k], ".pdf"), outfile = paste0("output/print-figures/pdf/main_", type_name[k], ".pdf"))
	}
} # end k

###############################################################################
# 2) Species fishy dot plot
###############################################################################

for(s in 1:6){
	species.s <- species[s]
	
	if(file_type == "png"){
		
	png(file = paste0("output/print-figures/png/species_", species[s], ".png"), width = 875, height = 2625, res = 350, pointsize = 10, family = "Sofia Pro Bold")
	
	} else if(file_type == "pdf"){
		pdf(file = paste0("output/print-figures/pdf/species_", species[s], ".pdf"), width = 875/350, height = 2625/350, pointsize = 10, family = "Sofia Pro Bold")
	}

	par(mai = rep(0, 4), mfrow = c(1,1), family = "Sofia Pro Bold")
	plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.04,1.96), ylim = c(min(dd_range), max(green_range)), xaxs = "i", yaxs = "i")
	polygon(x = c(0, 0, 2, 2), y = c(green_range, rev(green_range)), col = fishy_bgcols['high'], border = NA)
	polygon(x = c(0, 0, 2, 2), y = c(y_range, rev(y_range)), col = fishy_bgcols['bg'], border = NA)
	polygon(x = c(0, 0, 2, 2), y = c(crit_range, rev(crit_range)), col = fishy_bgcols['crit'], border = NA)
	polygon(x = c(0, 0, 2, 2), y = c(dd_range, rev(dd_range)), col = fishy_bgcols['dd'], border = NA)
	
	abline(v = seq(0, 2, 1), lwd = 10, col = "white")
	zero_line()
	
	for(k in 1:2){
		type.k <- c("Spawners", "Total return")[k]
		
		sps_summary.s <- sps_summary %>% 
			filter(species == species.s & type == type.k) %>%
			arrange(1/current_status)
		
		# Data deficient
		na_ind <- which(is.na(sps_summary.s$current_status))
		if(length(na_ind) > 0){
			x_dd <- rep(c(k - 0.25, k - 0.55), ceiling(length(na_ind)))[1:length(na_ind)]
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(NA)),
									xleft = x_dd - xinch(fishwidth), 
									ybottom = dd_y[1:length(na_ind)] - yinch(fishheight)/2, 
									xright = x_dd,
									ytop = dd_y[1:length(na_ind)] + yinch(fishheight)/2)
			
			rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['dd']),
									xleft = x_dd - xinch(fishwidth), 
									ybottom = dd_y[1:length(na_ind)] - yinch(fishheight)/2, 
									xright = x_dd,
									ytop = dd_y[1:length(na_ind)] + yinch(fishheight)/2)
			
			
			text(k - text_pad, dd_y[1:length(na_ind)], region_abbr[match(sps_summary.s$region[na_ind], regions)], col = grey(0.6), font = 2, cex = text_cex)
			
		}
		
		# Critically low
		crit_ind <- which(sps_summary.s$current_status == -999999)
		if(length(crit_ind) > 0){
			x_crit <- k - 0.4
			y_crit <- mean(crit_range)
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(-999999)), 
									xleft = x_crit - xinch(fishwidth), 
									ybottom = y_crit - yinch(fishheight)/2, 
									xright = x_crit,
									ytop = y_crit + yinch(fishheight)/2)
			
			text(k - text_pad, y_crit, region_abbr[match(sps_summary.s$region[crit_ind], regions)], col = crit_col, font = 2, cex = text_cex)
			
		}
		
		# High
		high_ind <- which(sps_summary.s$current_status > 150)
		if(length(high_ind) > 0){
			
			high_ind <- high_ind[order(sps_summary.s$current_status[high_ind], decreasing = FALSE)]
			if(length(high_ind) == 1){
				x_high <- k - 0.5
				y_high <- mean(green_range)
				
			} else {
				x_high <- k - 0.5 
				y_high <- green_y
			}
			
			for(i in length(high_ind):1){
				
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(sps_summary.s$current_status[high_ind[i]])),
										xleft = x_high - xinch(fishwidth), 
										ybottom = y_high[i] - yinch(fishheight)/2, 
										xright = x_high,
										ytop = y_high[i] + yinch(fishheight)/2)
				
				rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['high']),
										xleft = x_high - xinch(fishwidth), 
										ybottom = y_high[i] - yinch(fishheight)/2, 
										xright = x_high,
										ytop = y_high[i] + yinch(fishheight)/2)
				
			}
			
			text(k - 0.35, y_high, paste0("+", sps_summary.s$current_status[high_ind], "%"), font = 2, cex = text_cex)
			text(k - 0.13, y_high, region_abbr[match(sps_summary.s$region[high_ind], regions)], col = grey(0.6), font = 2, cex = text_cex)
			
		}
		
		# Other with actual fish for current_status!
		cs_ind <- which(sps_summary.s$current_status >=-100 & sps_summary.s$current_status <= 150)
		cs_ind <- cs_ind[order(sps_summary.s$current_status[cs_ind] + sps_summary.s$region_label_offset_y[cs_ind], decreasing = TRUE)]
		x_loc <- k - 0.5 
		y_loc <- sps_summary.s$current_status[cs_ind] + sps_summary.s$region_label_offset_y[cs_ind]
		
		for(i in 1:length(cs_ind)){
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(sps_summary.s$current_status[cs_ind[i]])),
									xleft = x_loc - xinch(fishwidth), 
									ybottom = y_loc[i] - yinch(fishheight)/2, 
									xright = x_loc,
									ytop = y_loc[i] + yinch(fishheight)/2)
			
			rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['bg']),
									xleft = x_loc - xinch(fishwidth), 
									ybottom = y_loc[i] - yinch(fishheight)/2, 
									xright = x_loc,
									ytop = y_loc[i] + yinch(fishheight)/2)
			
		}
		
		text(k - 0.32, y_loc + sps_summary.s$region_label_offset_y[cs_ind], paste0(ifelse(sps_summary.s$current_status[cs_ind]>0, "+", ""), sps_summary.s$current_status[cs_ind], "%"), font = 2, cex = text_cex)
		text(k - 0.13, y_loc + sps_summary.s$region_label_offset_y[cs_ind], region_abbr[match(sps_summary.s$region[cs_ind], regions)], col = grey(0.6), font = 2, cex = text_cex)
		
	} # end k
		dev.off()
		
		if(file_type == "pdf"){
			embed_fonts(paste0("output/print-figures/pdf/species_", species[s], ".pdf"), outfile = paste0("output/print-figures/pdf/species_", species[s], ".pdf"))
		}
		
	} # end s

###############################################################################
# 3) Region fishy dot plot
###############################################################################

y_range <- c(-100, 150)
vert_space <- 0.06
green_range <- y_range[2] + 250/6 * c(vert_space, vert_space + 0.5)
crit_range <- y_range[1] - 250/6 * c(vert_space + 0.4, vert_space)
dd_range <- crit_range[1] - 250/6 * c(vert_space, vert_space + 0.4)

# Locations for data deficient fish (starting at top)
dd_y <- mean(dd_range)
green_y <- green_range[1] + 5 + c(0:2)*5


for(r in 1:length(regions)){
	for(k in 1:2){
		region.r <- regions[r]
		type.k <- c("Spawners", "Total return")[k]
		
		sps_summary.r <- sps_summary %>% 
			filter(region == region.r & type == type.k) %>%
			arrange(1/current_status)
		
		
	if(file_type == "png"){
		png(file = paste0("output/print-figures/png/region_", region_abbr[r], "_", type_name[k], ".png"), width = 1925, height = 2538, res = 350, pointsize = 10, family = "Sofia Pro Bold")
	} else if(file_type == "pdf"){
		pdf(file = paste0("output/print-figures/pdf/region_", region_abbr[r], "_", type_name[k], ".pdf"), width = 1925/350, height = 2538/350, pointsize = 10, family = "Sofia Pro Bold")
	}
	# quartz(width = 5, height = 5 * 2538/1925, pointsize = 10, family = "Sofia Pro Bold")
	
	par(mai = c(0, mar_width, mar_width, 0), mfrow = c(1,1), family = "Sofia Pro Bold")
	
	#-----------------------------------------------------------------------------
	# Top panel
	#-----------------------------------------------------------------------------

	par(mar = rep(0,4), fig = c(0, 1, 0.8, 1), cex.lab = 1, family = "Sofia Pro Bold")
	plot(1,1,"n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, 1), xlim = c(0.02,5.98), xlab = "", ylab = "", bty = "n")
	polygon(x = c(0,0,6,6), y= c(0,1,1,0), col = fishy_bgcols['bg'], border = NA)
	
	# Current status
	for(s in 1:6){
		if(species[s] %in% sps_summary.r$species){
			polygon(x = c(s-1, s-1, s, s), y = 1 - c(0, 1/3, 1/3, 0), col = fishy_cols_func(sps_summary.r$current_status[sps_summary.r$species == species[s]]), border = NA)
					
			if(is.na(sps_summary.r$current_status[sps_summary.r$species == species[s]])){
				text(s - 0.5, 5/6, "?", cex = 2)
				} else if(sps_summary.r$current_status[sps_summary.r$species == species[s]] == -999999){
					text(s - 0.5, 5/6, "!", cex = 2)
				} else {
					text(s - 0.5, 5/6, paste0(ifelse(sps_summary.r$current_status[sps_summary.r$species == species[s]] > 0, "+", ""), sps_summary.r$current_status[sps_summary.r$species == species[s]], "%"), cex = 1.5)
				}
			} 
	} # end s
	abline(v = seq(0, 6, 1), lwd = 5, col = "white")
	
	# Trends
	for(s in 1:6){
		if(species[s] %in% sps_summary.r$species){
			species.s <- species[s]
			summ.rsk <- sps_summary.r %>% filter(species == species.s)

	if(summ.rsk$short_trend_cat == "arrow-down"){
		arrows(x0 = s - 0.5, y0 = 0.56, x1 = s - 0.5, y1 = 0.44, length = 0.12, lwd = 3, angle = 45)
	} else if(summ.rsk$short_trend_cat == "arrow-up"){
		arrows(x0 = s - 0.5, y0 = 0.44, x1 = s - 0.5, y1 = 0.56, length = 0.12, lwd = 3, angle = 45)
	} else if(summ.rsk$short_trend_cat =="arrows-left-right"){
		segments(x0 = s - 0.5 - 0.08, y0 = 0.5, x1 = s - 0.5 + 0.08, y1 = 0.5, lwd = 3)
	} else if(summ.rsk$short_trend_cat == ""){
		text(s - 0.5, 0.5, "?", cex = 2)
	}
	
	if(summ.rsk$long_trend_cat == "arrow-down"){
		arrows(x0 = s - 0.5, y0 = 1/6 + 0.06, x1 = s - 0.5, y1 = 1/6 - 0.06, length = 0.12, lwd = 3, angle = 45)
	} else if(summ.rsk$long_trend_cat == "arrow-up"){
		arrows(x0 = s - 0.5, y0 = 1/6 - 0.06, x1 = s - 0.5, y1 = 1/6 + 0.06, length = 0.12, lwd = 3, angle = 45)
	} else if(summ.rsk$long_trend_cat == "arrows-left-right"){
		segments(x0 = s - 0.5 - 0.08, y0 = 1/6, x1 = s - 0.5 + 0.08, y1 = 1/6, lwd = 3)
	} else if(summ.rsk$long_trend_cat == ""){
		text(s - 0.5, 1/6, "?", cex = 2)
	}
		}}
	
	# Species not present
	if(region.r == "Yukon"){
		polygon(x = c(3.02, 5.98, 5.98, 3.02), y = c(0, 0, 1, 1), border = NA, col = "#FBFCFD")
		text(4.5, 0.5, "Species Not Present", cex = 1.2)
	}
	
	if(region.r == "Columbia"){
		polygon(x = c(1.02, 3.98, 3.98, 1.02), y = c(0, 0, 1, 1), border = NA, col = "#FBFCFD")
		text(2.5, 0.5,  "Species Not Present", cex = 1.2)
	}
	
	# abline(h = c(0, 1/3, 2/3, 1), col = grey(0.8), lwd = 0.8)
	
	#-----------------------------------------------------------------------------
	# Fishy dot plot
	#-----------------------------------------------------------------------------
	par(mar = c(0, mar_width, mar_width, 0), family = "Sofia Pro Bold", new = TRUE, fig = c(0, 1, 0, 0.8))
	
	plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(min(dd_range), max(green_range)), xaxs = "i", yaxs = "i")
	polygon(x = c(0, 0, 6, 6), y = c(green_range, rev(green_range)), col = fishy_bgcols['high'], border = NA)
	polygon(x = c(0, 0, 6, 6), y = c(y_range, rev(y_range)), col = fishy_bgcols['bg'], border = NA)
	polygon(x = c(0, 0, 6, 6), y = c(crit_range, rev(crit_range)), col = fishy_bgcols['crit'], border = NA)
	polygon(x = c(0, 0, 6, 6), y = c(dd_range, rev(dd_range)), col = fishy_bgcols['dd'], border = NA)
	
	abline(v = seq(0, 6, 1), lwd = 5, col = "white")
	
	# Species not present
	if(region.r == "Yukon"){
		polygon(x = c(3.02, 5.98, 5.98, 3.02), y = rep(c(min(dd_range), max(green_range)), each = 2), border = NA, col = "#FFFFFF98")
		text(4.5, 50, "Species Not Present", cex = 1.2)
	}
	
	if(region.r == "Columbia"){
		polygon(x = c(1.02, 3.98, 3.98, 1.02), y = rep(c(min(dd_range), max(green_range)), each = 2), border = NA, col = "#FFFFFF98")
		text(2.5, 50,  "Species Not Present", cex = 1.2)
	}
	
 zero_line()
	
		# Data deficient
		na_ind <- which(is.na(sps_summary.r$current_status))
		if(length(na_ind) > 0){
			s <- match(sps_summary.r$species[na_ind], species)
			x_dd <- s - 0.25
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(NA)),
									xleft = x_dd - xinch(fishwidth), 
									ybottom = rep(dd_y, length(na_ind)) - yinch(fishheight)/2, 
									xright = x_dd,
									ytop = rep(dd_y, length(na_ind))  + yinch(fishheight)/2)
			
		}
		
		# Critically low
		crit_ind <- which(sps_summary.r$current_status == -999999)
		if(length(crit_ind) > 0){
			s <- match(sps_summary.r$species[crit_ind], species)
			x_crit <- s - 0.25
			y_crit <- mean(crit_range)
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(-999999)), 
									xleft = x_crit - xinch(fishwidth), 
									ybottom = y_crit - yinch(fishheight)/2, 
									xright = x_crit,
									ytop = y_crit + yinch(fishheight)/2)
	
			
		}
		
		# High
		high_ind <- which(sps_summary.r$current_status > 150)
		if(length(high_ind) > 0){
			
			high_ind <- high_ind[order(sps_summary.s$current_status[high_ind], decreasing = FALSE)]
			s <- match(sps_summary.r$species[high_ind], species)
			
			x_high <- s - 0.35
			y_high <- mean(green_range)
			
			for(i in length(high_ind):1){
				
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(sps_summary.r$current_status[high_ind[i]])),
										xleft = x_high[i] - xinch(fishwidth), 
										ybottom = y_high - yinch(fishheight)/2, 
										xright = x_high[i],
										ytop = y_high + yinch(fishheight)/2)
			}
			
			text(s - 0.2, y_high, paste0("+", sps_summary.r$current_status[high_ind], "%"), font = 2, cex = text_cex)
			
			
		}
		
		# Other with actual fish for current_status!
		cs_ind <- which(sps_summary.r$current_status >= -100 & sps_summary.r$current_status <= 150)
		if(length(cs_ind) > 0){
			cs_ind <- cs_ind[order(sps_summary.r$current_status[cs_ind] + sps_summary.r$region_label_offset_y[cs_ind], decreasing = TRUE)]
			s <- match(sps_summary.r$species[cs_ind], species)
			x_loc <- s - 0.35 
			y_loc <- sps_summary.r$current_status[cs_ind]
			
			for(i in 1:length(cs_ind)){
				
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(sps_summary.r$current_status[cs_ind[i]])),
										xleft = x_loc[i] - xinch(fishwidth), 
										ybottom = y_loc[i] - yinch(fishheight)/2, 
										xright = x_loc[i],
										ytop = y_loc[i] + yinch(fishheight)/2)
			}
			
			text(s - 0.2, y_loc, paste0(ifelse(sps_summary.r$current_status[cs_ind]>0, "+", ""), sps_summary.r$current_status[cs_ind], "%"), font = 2, cex = text_cex)
		}
	
		dev.off()
		
		# Embed fonts if pdf
		if(file_type == "pdf"){
			embed_fonts(paste0("output/print-figures/pdf/region_", region_abbr[r], "_", type_name[k], ".pdf"), outfile = paste0("output/print-figures/pdf/region_", region_abbr[r], "_", type_name[k], ".pdf"))
		}
		
	} # end k
} # end r

###############################################################################
# 4) Region-species trends
###############################################################################

# Read in species colour palette
source("code/colours.R")

sps_trends <- read.csv("output/sps-trends_plotting.csv")

fishwidth <- 0.8
fishheight <- fishwidth * image_info(fish)$height/image_info(fish)$width


for(r in 1:length(regions)){
	for(s in 1:length(species)){
		for(k in 1:2){
			
			
			# subset data
			species.s <- species[s]
			region.r <- regions[r]
			if(k == 1){
				trends.rsk <- sps_trends %>% dplyr::filter(region == region.r, species == species.s) %>%
					dplyr::select(year, spawners, spawners_short_trend, spawners_long_trend)
				summ.rsk <- sps_summary %>% filter(region == region.r, species == species.s & type == "Spawners")
			} else if(k == 2){
				trends.rsk <- sps_trends %>% dplyr::filter(region == region.r & species == species.s) %>%
					dplyr::select(year, total_return, total_return_short_trend, total_return_long_trend)
				summ.rsk <- sps_summary %>% dplyr::filter(region == region.r & species == species.s & type == "Total return")
			}
			
			
			if(dim(summ.rsk)[1] > 0){
				
				if(file_type == "png"){
					png(file = paste0("output/print-figures/png/trend_", region_abbr[r], "_", species[s], "_", type_name[k], ".png"), width = 1633, height = 1167, res = 350, pointsize = 10, family = "Sofia Pro Bold")
				} else if(file_type == "pdf"){
					pdf(file = paste0("output/print-figures/pdf/trend_", region_abbr[r], "_", species[s], "_", type_name[k], ".pdf"), width = 1633/350, height = 1167/350, pointsize = 10, family = "Sofia Pro Bold")
				}
				# quartz(width = 1633/350, height = 1167/350, pointsize = 10, family = "Sofia Pro")
				
				par(mfrow = c(1,1), family = "Sofia Pro Bold", bg = "#FFFFFF")
				
				# Top panel
				par(mar = rep(0,4), fig = c(0, 1, 0.75, 1), cex.lab = 1, family = "Sofia Pro Bold")
				plot(1,1,"n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, 1), xlim = c(0, 1), xlab = "", ylab = "", bty = "n")
				polygon(x = c(0,0,1,1), y= c(0,1,1,0), col = fishy_bgcols['bg'], border = NA)
				
				# Current state
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(summ.rsk$current_status)),
										xleft = 0.22 - xinch(fishwidth), 
										ybottom = 0.6 - yinch(fishheight)/2, 
										xright = 0.22,
										ytop = 0.6 + yinch(fishheight)/2)
				if(is.na(summ.rsk$current_status)){
					text(0.12, 0.2, "?", cex = 1.2)
				} else if(summ.rsk$current_status == -999999){
					text(0.12, 0.2, "!", cex = 1.2)
				} else {
					text(0.12, 0.2, paste0(ifelse(summ.rsk$current_status > 0, "+", ""), summ.rsk$current_status, "%"), cex = 1.2)
				}
				
				# Trends
				trends_anchor <- 0.35
				segments(x0 = trends_anchor, y0 = 0.2, x1 = trends_anchor, y1 = 0.8, col = grey(0.8))
				if(summ.rsk$short_trend_cat == "arrow-down"){
					arrows(x0 = trends_anchor - 0.05, y0 = 0.6, x1 = trends_anchor - 0.05, y1 = 0.4, length = 0.1, lwd = 3, angle = 45)
				} else if(summ.rsk$short_trend_cat == "arrow-up"){
					arrows(x0 = trends_anchor - 0.05, y0 = 0.4, x1 = trends_anchor - 0.05, y1 = 0.6, length = 0.1, lwd = 3, angle = 45)
				} else if(summ.rsk$short_trend_cat =="arrows-left-right"){
					segments(x0 = trends_anchor - 0.09, y0 = 0.5, x1 = trends_anchor - 0.03, y1 = 0.5, lwd = 3)
				} else if(summ.rsk$short_trend_cat == ""){
					text(trends_anchor - 0.05, 0.5, "?", cex = 1.5)
				}
				
				if(summ.rsk$long_trend_cat == "arrow-down"){
					arrows(x0 = trends_anchor + 0.05, y0 = 0.6, x1 = trends_anchor + 0.05, y1 = 0.4, length = 0.1, lwd = 3, angle = 45)
				} else if(summ.rsk$long_trend_cat == "arrow-up"){
					arrows(x0 = trends_anchor + 0.05, y0 = 0.4, x1 = trends_anchor + 0.05, y1 = 0.6, length = 0.1, lwd = 3, angle = 45)
				} else if(summ.rsk$long_trend_cat == "arrows-left-right"){
					segments(x0 = trends_anchor + 0.03, y0 = 0.5, x1 = trends_anchor + 0.09, y1 = 0.5, lwd = 3)
				} else if(summ.rsk$long_trend_cat == ""){
					text(trends_anchor + 0.05, 0.5, "?", cex = 1.5)
				}
				
				
				# Numbers
				if(is.na(summ.rsk$current_abundance)){
					text(0.62, 0.5, "?", cex = 1.5)
				} else {
					text(0.62, 0.6, prettyNum(summ.rsk$current_abundance, big.mark = ","), cex = 1.2)
				}
				
				if(is.na(summ.rsk$average_abundance)){
					text(0.96, 0.5, "?", cex = 1.5, adj = 1)
				} else {
					text(0.96, 0.6, prettyNum(summ.rsk$average_abundance, big.mark = ","), adj = 1, cex = 1.2)
				}
				
				par(family = "Sofia Pro Regular")
				
				if(!is.na(summ.rsk$current_abundance)){
					text(0.62, 0.35, paste0("(", summ.rsk$current_abundance_year - summ.rsk$gen_length + 1, "-", summ.rsk$current_abundance_year, ")"), font = 1)
				}
				
				if(!is.na(summ.rsk$average_abundance)){
					text(0.96, 0.35, paste0("(", summ.rsk$rangeyears, ")"), font = 1, adj = 1)
				}
				
				# Trends
				
				
				par(mar = c(2, 4, 0.5, 0.5), family = "Sofia Pro Bold", new = TRUE, fig = c(0, 1, 0, 0.74), mgp = c(3, 0.75, 0))
				if(dim(trends.rsk)[1] == 0 | sum(is.na(trends.rsk[,2])) == length(trends.rsk[,2])){
					plot(c(1980, 2025), c(-100, 150), "n", yaxt = "n", bty = "l", ylab = "", xaxt = "n")
					mtext(side = 2, line = 3, c("Spawner Abundance", "Total Abundance")[k])
					
					zero_line()
					y_lab <- pretty(c(-100, 150))
					text(mean(c(1980, 2025)), 75, "Data Deficient")
				
					} else {
						
						# Set custom y-axis range for some regions/speices
						if(region_abbr[r] == "WI" & species[s] == "Pink"){
							y_range <- c(-100, 3000)
						} else if(region_abbr[r] == "FR" & species[s] == "Sockeye"){
							y_range <- c(-100, 300)
						} else {
							y_range <- range(trends.rsk[, 2], na.rm = TRUE)
						}
						
					plot(trends.rsk$year, trends.rsk[, 2], "n", yaxt = "n", bty = "l", ylab = "", xaxt = "n", xlim = c(min(trends.rsk$year), 2025), ylim = y_range)
					mtext(side = 2, line = 3, c("Spawner Abundance", "Total Abundance")[k])
					
					zero_line()
					lines(trends.rsk$year, trends.rsk[, 2], col = species_cols_light[species[s]], lwd = 1.5)
					lines(trends.rsk$year, trends.rsk[, 3], col = species_cols_dark[species[s]], lwd = 3)
					lines(trends.rsk$year, trends.rsk[, 4], col = colorRampPalette(c(species_cols_light[species[s]], species_cols_dark[species[s]]))(n = 3)[2], lwd = 2)
					
					y_lab <- pretty(y_range)
				}
				
				par(family = "Sofia Pro Regular", cex.axis = 0.9)
				axis(side = 2, at = y_lab, labels = paste0(y_lab, "%"), las = 1)
				axis(side = 1)
				axis(side = 1, at = seq(1890, 2025, 1), tck = -0.01, labels = FALSE)
				
				# Add custom star for points off the chart for some regions/speices
				if(region_abbr[r] == "WI" & species[s] == "Pink"){
					text(c(1972, 1975), 3200, "*", col = species_cols_light[species[s]], xpd = NA, cex = 1.2)
				}
				if(region_abbr[r] == "FR" & species[s] == "Sockeye"){
					text(c(1894), 330, "*", col = species_cols_light[species[s]], xpd = NA, cex = 1.2)
				}
				
				
				dev.off()
				
				# Embed fonts if pdf
				if(file_type == "pdf"){
					embed_fonts(paste0("output/print-figures/pdf/trend_", region_abbr[r], "_", species[s], "_", type_name[k], ".pdf"), outfile = paste0("output/print-figures/pdf/trend_", region_abbr[r], "_", species[s], "_", type_name[k], ".pdf"))
				}
				
			} # end if 
		} # end k
	} # end s
} # end r

###############################################################################
# 5) Highlights report
###############################################################################
k <- 1
fishwidth <- 1
fishheight <- fishwidth * image_info(fish)$height/image_info(fish)$width

# quartz(width = 7, height = 7, pointsize = 10, family = "Sofia Pro Bold")
# png(file = paste0("output/ignore/", type_name[k], "_highlight_fig_2025-08-25.png"), width = 7, height = 7, units = "in", res = 350, pointsize = 10, family = "Sofia Pro Semi Bold", bg = NA)

pdf(file = paste0("output/ignore/", type_name[k], "_highlight_fig_", Sys.Date(), ".pdf"), width = 7, height = 7, pointsize = 10, family = "Sofia Pro Semi Bold", bg = NA)



par(mar = rep(0,4), family = "Sofia Pro Semi Bold")
plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(0.02, 9.98), xaxs = "i", yaxs = "i")
for(r in 1:10){
	for(s in 1:6){
		region.r <- rev(regions)[r]
		species.s <- species[s]
		type.k <- c("Spawners", "Total return")[k]
		summ.rsk <- sps_summary %>% filter(region == region.r & species == species.s & type == type.k)
		
		
		if(dim(summ.rsk)[1] > 0){
				if(is.na(summ.rsk$current_status)){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['dd'], border = NA)
			} else if(summ.rsk$current_status == -999999){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['crit'], border = NA)
			} else {
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['bg'], border = NA)
			}

			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(summ.rsk$current_status)),
									xleft = s - 0.08 - xinch(fishwidth),
									ybottom = r - 0.5 - yinch(fishheight)/2,
									xright = s - 0.08,
									ytop = r - 0.5 + yinch(fishheight)/2)

			if(is.na(summ.rsk$current_status)){
				text(s - 0.5, r - 0.5, "?", cex = 1.3)
			} else if(summ.rsk$current_status == -999999){
				text(s - 0.5, r - 0.5, "!", cex = 1.3)
			} else if (summ.rsk$current_status > 150 | summ.rsk$current_status < -80){
				text(s - 0.5, r - 0.5, paste0(ifelse(summ.rsk$current_status > 0, "+", ""), summ.rsk$current_status, "%"), cex = 1.3)
			} else {
				 text(s - 0.5, r - 0.5, paste0(ifelse(summ.rsk$current_status > 0, "+", ""), summ.rsk$current_status, "%"), cex = 1.3)
			}
		} # end if species are present
	
		} # end s
} # end r

# abline(v = seq(0, 6, 1), lwd = 5, col = "white")
# abline(h = seq(0, 10, 1), lwd = 5, col = "white")
dev.off()

if(file_type == "pdf"){
	embed_fonts(paste0("output/ignore/", type_name[k], "_highlight_fig_", Sys.Date(), ".pdf"), outfile = paste0("output/ignore/", type_name[k], "_highlight_fig_", Sys.Date(), ".pdf"))
}