## Test indicator stream assumptions


library(dplyr)
library(ggplot2)
# Here, we read in spawner survey data in the PSE, and revise the indicator/
# non-indicator designations that are used in expansions where needed
# Also add 2024 data from escapement bulletin for WVI
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# Read in spawner survey data from PSE (source: NuSEDS)

spawner_surveys.all <- read.csv("data/dataset2_spawner-surveys_2026-03-16.csv") %>%
	dplyr::filter(year >= 1950, !is.na(stream_observed_count)) %>% # Use only data from 1950 to present
	dplyr::select(region, species_name, species_qualified, streamid, stream_name_pse, GFE_ID, indicator, 
								latitude, longitude, year, stream_observed_count, source_id)

# Read in LGL indicator streams list
lgl <- read.csv("data/ignore/OUTPUT_NCCStreams_2017.csv") %>% 
	dplyr::select(POP_ID, Indicator, SPP, GFE_ID, SYS_NM)#, CU_findex, CU_name, CU_index)
species_qualified <- sort(unique(lgl$SPP))


# NuSEDS indicator designation is by unique streamid
spawner_surveys.all %>% group_by(species_name, region, streamid) %>% 
	summarize(ind = n_distinct(indicator)) %>% filter(ind != 1) # should be empty

# LGL designation is by unique POP_ID
lgl %>% group_by(SPP, POP_ID) %>% summarize(ind = n_distinct(Indicator)) %>%
	filter(ind != 1) # should be empty


#####################################################
## 1. How frequently were NuSEDS and LGL indicator 
# streams monitored in each time period?
#####################################################

# Get spawner survey dataset with both designations in one dataframe
lgl_cleaned <- filter(lgl, POP_ID != 51771, POP_ID != 51772)
spwn <- spawner_surveys.all %>% 
	left_join(lgl_cleaned, by=c("GFE_ID", "species_qualified" = "SPP")) %>% # remove bella coola chum
	rename(ind_nuseds = indicator, ind_lgl = Indicator)

min(spwn$year)
max(spwn$year)

periods <- list("1955-1964"=c(1955:1964),
								"1965-1974"=c(1965:1974),
								"1975-1984"=c(1975:1984),
								"1985-1994"=c(1985:1994),
								"1995-2004"=c(1995:2004),
								"2005-2014"=c(2005:2014),
								"2015-2024"=c(2015:2024))

mean_yrs_nuseds <- bind_rows(lapply(periods, function(x){
	lst <- spwn %>% filter(year %in% x) %>%
		group_by(ind_nuseds, streamid) %>%
		summarize(n_popyrs = n()) %>% 
		ungroup(streamid) %>%
		summarize(mean_popyrs = mean(n_popyrs, na.rm=T),
							sd_popyrs = sd(n_popyrs, na.rm=T),
							n = n_distinct(streamid)) %>%
		filter(ind_nuseds %in% c("N", "Y"))
}), .id="time_period")

mean_yrs_lgl <- bind_rows(lapply(periods, function(x){
	lst <- spwn %>% filter(year %in% x) %>%
		group_by(ind_lgl, POP_ID) %>%
		summarize(n_popyrs = n()) %>% 
		ungroup(POP_ID) %>%
		summarize(mean_popyrs = mean(n_popyrs, na.rm=T),
							sd_popyrs = sd(n_popyrs, na.rm=T),
							n = n_distinct(POP_ID)) %>%
		filter(ind_lgl %in% c("N", "Y"))
}), .id="time_period")



ind_compare_lgl_nuseds <- ggplot() + 
	geom_point(data=mean_yrs_nuseds, aes(x=time_period, y=mean_popyrs, group=ind_nuseds, shape=ind_nuseds, col="NuSEDS")) +
	geom_line(data=mean_yrs_nuseds, aes(x=time_period, y=mean_popyrs, group=ind_nuseds, col="NuSEDS")) + 
	geom_point(data=mean_yrs_lgl, aes(x=time_period, y=mean_popyrs, group=ind_lgl, shape=ind_lgl, col="LGL")) +
	geom_line(data=mean_yrs_lgl, aes(x=time_period, y=mean_popyrs, group=ind_lgl, col="LGL")) +
	labs(x="Decade", y="Mean years monitored (in 10)", shape="Indicator?", col="Source") +
	scale_colour_manual(values=c("NuSEDS" = "blue", "LGL" = "darkgreen"))

ind_compare_lgl_nuseds

#############################################################
# 2. How many streams have been monitored per year over time?
#############################################################


## NuSEDS Indicator
spwn %>% group_by(region, year, species_name, ind_nuseds) %>% summarize(n_streams = n_distinct(streamid)) %>%
	filter(ind_nuseds %in% c("Y", "N")) %>% 
	ggplot() + geom_point(aes(x=year, y=n_streams, col=species_name)) + 
	geom_line(aes(x=year, y=n_streams, col=species_name)) + 
	facet_grid(rows=vars(region), cols=vars(ind_nuseds))

# Monitoring has gone down overall, but mainly for non-indicator streams.
# Central coast, Nass, and SKeena have seen recent declines in monitoring of indicator streams.

spwn %>% group_by(region, year, species_name, ind_nuseds) %>% summarize(n_streams = n_distinct(streamid)) %>%
	ungroup() %>% mutate(pr_streams = n_streams/max(n_streams), .by=c(region, species_name, ind_nuseds)) %>%
	filter(ind_nuseds %in% c("Y", "N")) %>% 
	ggplot() + geom_point(aes(x=year, y=pr_streams, col=species_name)) + 
	geom_line(aes(x=year, y=pr_streams, col=species_name)) + 
	facet_grid(rows=vars(region), cols=vars(ind_nuseds))

# Same as above, but as proportion of maximum streams monitored instead of # streams

## LGL Indicator
spwn %>% group_by(region, year, species_name, ind_lgl) %>% summarize(n_streams = n_distinct(POP_ID)) %>%
	filter(ind_lgl %in% c("Y", "N")) %>% 
	ggplot() + geom_point(aes(x=year, y=n_streams, col=species_name)) + 
	geom_line(aes(x=year, y=n_streams, col=species_name)) + 
	facet_grid(rows=vars(region), cols=vars(ind_lgl))

# Story roughly the same for LGL indicator streams
# Central coast, Nass, and SKeena have seen recent declines in monitoring of indicator streams.

spwn %>% group_by(region, year, species_name, ind_lgl) %>% summarize(n_streams = n_distinct(POP_ID)) %>%
	ungroup() %>% mutate(pr_streams = n_streams/max(n_streams), .by=c(region, species_name, ind_lgl)) %>%
	filter(ind_lgl %in% c("Y", "N")) %>% 
	ggplot() + geom_point(aes(x=year, y=pr_streams, col=species_name)) + 
	geom_line(aes(x=year, y=pr_streams, col=species_name)) + 
	facet_grid(rows=vars(region), cols=vars(ind_lgl))

# View as proportion

########################################################
# 3. What NuSEDS indicator designations are replaced by 
# LGL ones?
#######################################################

ind_changed <- read.csv("data/indicator_changed_2026-04-10.csv")[,-1]

ind_changed <- ind_changed %>% mutate(added_removed = if_else(indicator=="N" & Indicator=="Y", "added", "removed"),
											 which_ind = if_else(Indicator=="Y", "LGL", "NuSEDS"))

# How many indicator streams are added and how many are removed?
ind_changed %>% group_by(region, added_removed) %>% summarize(n = n_distinct(streamid))


# Monitoring by decade among added vs removed group

spwn %>% filter(streamid %in% ind_changed$streamid) %>%
	mutate(added_removed = if_else(ind_nuseds=="N" & ind_lgl=="Y", "added", "removed")) %>%
	mutate(decade = case_when(year %in% periods[[1]] ~ 1,
														year %in% periods[[2]] ~ 2,
														year %in% periods[[3]] ~ 3,
														year %in% periods[[4]] ~ 4,
														year %in% periods[[5]] ~ 5,
														year %in% periods[[6]] ~ 6,
														year %in% periods[[7]] ~ 7)) %>%
	group_by(region, species_name, added_removed, decade, streamid) %>%
	summarize(n_yrs = n()) %>% ungroup(streamid) %>%
	summarize(mean_nyrs = mean(n_yrs)) %>% 
	ggplot(aes(x=decade, y=mean_nyrs, col=species_name)) + geom_point() + geom_line() + 
	facet_grid(rows=vars(region), cols=vars(added_removed)) +
	labs(x="Decade", y="Years monitored (in 10)")

# In general, the nuseds indicator designations that are "replaced" by LGL indicators are jusified, 
# but they are getting out of date (LGL assessment in 2017)


#######################################################
# 4. How does a method of designating based on overall
# monitoring frequency compare?
#######################################################
# Consider trying a rolling 10-year count instead of defining decades

yrs_decade <- spwn %>% mutate(decade = case_when(year %in% periods[[1]] ~ names(periods)[1],
																	 year %in% periods[[2]] ~ names(periods)[2],
																	 year %in% periods[[3]] ~ names(periods)[3],
																	 year %in% periods[[4]] ~ names(periods)[4],
																	 year %in% periods[[5]] ~ names(periods)[5],
																	 year %in% periods[[6]] ~ names(periods)[6],
																	 year %in% periods[[7]] ~ names(periods)[7])) %>%
	group_by(region, species_name, decade, streamid) %>%
	mutate(n_yrs = n()) 

# Assign new indicator streams based on monitoring in each decade
# Pinks have to be dealt with separately
all_ind <- yrs_decade %>% filter(!is.na(decade)) %>%
	group_by(region, species_name, streamid) %>%
	summarize(lowest_decade = min(n_yrs)) %>%
	filter(lowest_decade >= 5) # at least 5 of every 10 years is monitored
pink_ind <- yrs_decade %>% filter(species_name=="Pink") %>%
	filter(!is.na(decade)) %>%
	group_by(region, species_name, streamid) %>%
	summarize(lowest_decade = min(n_yrs)) %>%
	filter(lowest_decade >= 5) # at least 3 of every 10 years is monitored for Pinks
new_indicators <- rbind(pink_ind, all_ind)

# Add new indicators to spwn dataframe
spwn <- spwn %>% mutate(ind_new = if_else(streamid %in% new_indicators$streamid, "Y", "N"))

# How many indicator streams does this create compared to the NuSEDS designation?
n_ind_new <- new_indicators %>% group_by(region, species_name) %>% summarize(n=n_distinct(streamid))
n_ind_nuseds <- spwn %>% group_by(region, species_name) %>% filter(ind_nuseds == "Y") %>% summarize(n=n_distinct(streamid))

n_ind_crosswalk <- expand.grid(region = unique(spwn$region), species_name = unique(spwn$species_name)) %>% 
	left_join(n_ind_new, by=c("region", "species_name")) %>% 
	left_join(n_ind_nuseds, by=c("region", "species_name"), suffix = c("_new", "_nuseds"))

# For which region-species combinations are there no indicators for under new method?
n_ind_crosswalk %>% filter(!is.na(n_nuseds) & is.na(n_new))


# Many species-region combinations have roughly similar amounts of indicator streams, but with 'new' method
# some have very few or none, which is a problem.


# Make the same plot made in part 1, using the same summarizing method
mean_yrs_new <- bind_rows(lapply(periods, function(x){
	lst <- spwn %>% filter(year %in% x) %>%
		group_by(ind_new, streamid) %>%
		summarize(n_popyrs = n()) %>% 
		ungroup(streamid) %>%
		summarize(mean_popyrs = mean(n_popyrs, na.rm=T),
							sd_popyrs = sd(n_popyrs, na.rm=T),
							n = n_distinct(streamid)) %>%
		filter(ind_new %in% c("N", "Y"))
}), .id="time_period")


ind_compare_lgl_nuseds +
	geom_point(data=mean_yrs_new, aes(x=time_period, y=mean_popyrs, group=ind_new, shape=ind_new, col = "New")) + 
	geom_line(data=mean_yrs_new, aes(x=time_period, y=mean_popyrs, group=ind_new, col = "New")) + 
	scale_colour_manual(values=c("NuSEDS" = "blue", "LGL" = "darkgreen", "New" = "darkred"))

# This indicator performs better for regions/species where it works.



#######################################################
# Future questions:
# --> How does a method of designating based on WEIGHTED
# monitoring frequency compare? (Recent > old)
# --> Does past monitoring historically predict 
# future monitoring?
#######################################################






########################################################
## Check out current expansion factors
########################################################
library(here)
regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "West Vancouver Island", "East Vancouver Island & Mainland Inlets", "Fraser", "Columbia")
species_vec <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead") 
exp_lst <- vector("list", length(regions))
names(exp_lst) <- regions

for(r in 1:length(regions)){
	if(any(grepl(regions[r], list.files(here("output/expanded-spawners"))))){
		
		regionname <- regions[r]
		region_spawners <- readRDS(here("output/expanded-spawners", paste0(regions[r], "-spawners.rds")))
		expansion_factors <- readRDS(file=here("output/expanded-spawners", paste0(regions[r], "-expansion-factors.rds")))
		
		# Get species from region_spawners
		sp <- dimnames(region_spawners)[[2]]
		names(expansion_factors) <- sp
		
	exp_summary <- lapply(expansion_factors, function(x){
			x[["exp1"]] <- sort(x[["exp1"]], decreasing=T)[1:10]
			x[["exp2"]] <- mean(x[["exp2"]], na.rm=T)
			return(x)
		})
		 
	exp_lst[[r]] <- exp_summary
	}
}


