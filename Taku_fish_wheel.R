###############################################################################
# Exploration of Taku River (Canyon Island) fish wheel data as an index of
# abundance for Taku pink, chum, and steelhead.
# Steph Peacock
# Sept 1, 2026
###############################################################################

library(readxl)
library(dplyr)
# 

appD24 <- read_xlsx("data/2025 PSF Pacific Salmon Explorer Data.xlsx", sheet = "TAK Appen D.24", range = "A8:P50")

# Calculate annual operating days
z <- strsplit(appD24$Operation, split = "-")
operationDates <- data.frame(
	Year = appD24$Year,
	startDate = NA,
	endDate = NA
)
ny <- length(appD24$Year)

for(i in 1:ny){
	operationDates$startDate[i] <- as.Date(paste0(z[[i]][1], "/", operationDates$Year[i]), format = "%m/%d/%Y")
	operationDates$endDate[i] <- as.Date(paste0(z[[i]][2], "/", operationDates$Year[i]), format = "%m/%d/%Y")
}

operationDates$nDays <- operationDates$endDate - operationDates$startDate + 1
operationDates$startDate <- as.Date(operationDates$startDate, origin = "1970-01-01")
operationDates$endDate <- as.Date(operationDates$endDate, origin = "1970-01-01")

operationDates$startMonth <- as.numeric(strftime(operationDates$startDate, format = "%m"))
operationDates$startDay <- as.numeric(strftime(operationDates$startDate, format = "%d"))
operationDates$endMonth <- as.numeric(strftime(operationDates$endDate, format = "%m"))
operationDates$endDay <- as.numeric(strftime(operationDates$endDate, format = "%d"))

range(operationDates$startMonth)
range(operationDates$endMonth)

par(mfrow = c(2,1), mar = c(3,4,1,4), oma = c(0,0,2,0), family = "Arial")
plot(range(operationDates$Year), as.Date(c("2000-04-01", "2000-10-31")), "n", xlab= "", ylab = "Operating window", las = 1)
abline(h = as.Date(paste(rep(2000, 8), c(4:11), rep(1, 8), sep = "-")), lty = 2)
for(i in 1:ny){
	segments(x0 = operationDates$Year[i],
					 x1 = operationDates$Year[i],
					 y0 = as.Date(paste(2000, operationDates$startMonth[i], operationDates$startDay[i], sep = "-")),
					 y1 = as.Date(paste(2000, operationDates$endMonth[i], operationDates$endDay[i], sep = "-")),
					 lwd = 3, lend = 1)
}

plot(operationDates$Year, operationDates$nDays, "o", pch = 21, bg = 'white', ylab = "Number of operating days", xlab = "", las = 1)		
par(new = TRUE)
plot(appD24$Year, appD24$hours, "l", col = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 2)
axis(side = 4, col = 2, las = 1)
mtext(side = 3, outer= TRUE, "Canyon Island Fish Wheel Operation")
mtext(side = 4, "Operating hours per day", col = 2, line = 2.5)

avgDays <- mean(operationDates$nDays * appD24$hours/24)

# Merge datasets
dat <- appD24 %>% left_join(operationDates) %>%
	dplyr::select(Year, Operation, startDate, endDate, nDays, hours, Pink_12, Chum, Steelhead_3) %>%
	rename("pink_raw" = "Pink_12",
				 "chum_raw" = "Chum",
				 "steelhead_raw" = "Steelhead_3") %>%
	# Calculate CPUE, where effort is fish wheel days
	mutate(pink_cpue = pink_raw/(nDays * hours/24),
				 chum_cpue = chum_raw/(nDays * hours/24),
				 steelhead_cpue = steelhead_raw/(nDays * hours/24)) %>%
	# calculate adjusted (standardized) index based on average number of days
	mutate(pink_adj = pink_cpue * avgDays,
				 chum_adj = chum_cpue * avgDays,
				 steelhead_adj = steelhead_cpue * avgDays)
				 )

head(dat)
par(mfrow = c(3,1))

plot(dat$Year, dat$chum_raw, "o", pch = 21, bg = "white", ylim = range(dat$chum_raw, dat$chum_adj), ylab = "Chum abundance")
abline(h = exp(mean(log(dat$chum_raw))), lty = 2)
points(dat$Year, dat$chum_adj, "o", col = 2, pch = 21, bg = "white")
abline(h = exp(mean(log(dat$chum_adj))), lty = 2, col = 2)

legend("topright", col = c(1,2), lwd = 1, pch = 21, pt.bg = "white", c("raw", "CPUE adjusted"))

plot(dat$Year, dat$pink_raw, "o", pch = 21, bg = "white", ylim = range(dat$pink_raw, dat$pink_adj), ylab = "Pink abundance")
abline(h = exp(mean(log(dat$pink_raw))), lty = 2)
points(dat$Year, dat$pink_adj, "o", col = 2, pch = 21, bg = "white")
abline(h = exp(mean(log(dat$pink_adj))), lty = 2, col = 2)

plot(dat$Year, dat$steelhead_raw, "o", pch = 21, bg = "white", ylim = range(c(dat$steelhead_raw, dat$steelhead_adj), na.rm = TRUE), ylab = "Steelhead abundance")
abline(h = exp(mean(log(dat$steelhead_raw), na.rm = TRUE)), lty = 2)
points(dat$Year, dat$steelhead_adj, "o", col = 2, pch = 21, bg = "white")
abline(h = exp(mean(log(dat$steelhead_adj), na.rm = TRUE)), lty = 2, col = 2)
