# Directory where the local github repo population-indicators is located
# For me, this is /Users/stephaniepeacock/Documents/PSF/PopulationAnalysis/population-indicators
base_dir <- getwd()

# Now move up in the file structure until you find the parent directory that contains Dropbox 
# ** Should work on iOS or Windows - not sure about Linux **
Dropbox_dir <- base_dir
while(length(grep("Salmon Watersheds Dropbox", list.files(path = Dropbox_dir))) == 0){
	setwd("..")
	Dropbox_dir <- getwd()
}

# Now set that working directory
Dropbox_dir <- paste(getwd(), "Salmon Watersheds Dropbox", sep = "/")

# WIthin Dropbox, the X Drive is in a named folder. But this can be found through exclusion of other common folders
name_folder <- list.files(path = Dropbox_dir)[which((list.files(path = Dropbox_dir) %in% 
																										 	c("Icon\r", "Team Folder", "Team Paper Docs", "desktop.ini")) == FALSE)]

if(length(name_folder) == 1){ 
	Dropbox_dir <- paste(Dropbox_dir, name_folder, sep = "/")
} else{
	stop("More than one possible Dropbox folder")
}

Dropbox_dir <- paste(Dropbox_dir, "X Drive/1_PROJECTS/1_Active/State of Salmon/2_Data & Analysis/state-of-salmon", sep = "/")

setwd(base_dir)
