# Fraser Chinook plot -- for Eric

library(dplyr)
library(ggplot2)
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# Read in spawner survey data from PSE
spawner_surveys.all <- read.csv("data/dataset2_spawner-surveys_2026-03-16.csv") %>%
	dplyr::filter(year >= 1950, !is.na(stream_observed_count)) %>% # Use only data from 1950 to present
	dplyr::select(region, species_name, species_qualified, streamid, stream_name_pse, GFE_ID, indicator, latitude, longitude, year, stream_observed_count, source_id)

spr_early <- c("CHILCOTIN", "COTTONWOOD", "BOWRON", "FRASER",
							 "BIRKENHEAD", "BONAPARTE", "DEADMAN", "COLDWATER",
								"NICOLA", "SPIUS", "FINN", "LOUIS", "SALMON")
sum_mid <- c("CLEARWATER", "ADAMS", "EAGLE", "SHUSWAP", "CHILKO",
						 "PITT", "NECHAKO", "LITTLE", "TASEKO", 
						  "NAHATLATCH", "PORTAGE", "SETON", "BARRIERE")
fall_late <- c("HARRISON")
all_streams <- c(spr_early, sum_mid, fall_late)

# Filter spawner surveys for streams in all_streams list
frck_all <- spawner_surveys.all %>% filter(species_name == "Chinook", region == "Fraser") %>%
	mutate(name_short = stringr::str_split_i(stream_name_pse, " ", i=1)) # Match only first words

frck <- frck_all %>% 
	filter(name_short %in% all_streams) %>% # filter
	mutate(group = ifelse(name_short %in% spr_early, "Spring", 
												ifelse(name_short %in% sum_mid, "Summer", "Fall"))) %>% # run timing
	mutate(group = factor(group, levels=c("Spring", "Summer", "Fall")))
	
# Check that all streams in list are in filtered dataframe	
all_streams %in% frck$name_short
# Check that groups assigned properly
unique(frck[,c("group", "name_short")])

# Sum escapements and plot
frck %>% 
	dplyr::summarize(esc = sum(stream_observed_count/1000), .by=c(year, group)) %>%
	filter_out(group=="Fall" & year < 1983) %>% # filter out fall run pre 1983
	ggplot(aes(x=year, y=esc)) + 
	geom_vline(aes(xintercept=1993), col="navyblue", lty=2) + # line for report year
	geom_point() + geom_line() + 
	facet_wrap(vars(group), nrow=3, scales="free_y", strip.position="top") +
	labs(x="Year", y="Escapement (thousands)") + 
	theme_minimal() 

