#================================================================================================
#		  Article: Cost and Productivity Effects of Demographic Changes on 
#				Local Water Service. 
#							
# published in: Utilities Policy, 2022, 79(101435).
# authors: Astrid Cullmann, Caroline Stiel	
# affiliations: Technische Universitaet Berlin, DIW Berlin	
#				
#================================================================================================
#
#
#  						MASTER FILE
#
#
# 					calls the different programs
#
#================================================================================================


#================================================================================================
# 1) Set up R-environment
#================================================================================================


# clean memory 
#--------------
rm(list = ls())



# Use fixed notation instead of exponential notation
#---------------------------------------------------
options(scipen = 999)



#================================================================================================
# 2) Define working environment
#================================================================================================


# Variable 'RDC' defines the working environment
#-----------------------------------------------
# 0: own computer
# 1: remote access to original data set at research data centre (RDC)
 
RDC <- 1  

# Working environment 0: own computer
# -----------------------------------
if (RDC == 0)  {
	# Path1: directory where working data is saved
	Path1   <- "" 
	# Path2: directory where original data is stored
	Path2   <- "" 
	# path3: directory where all the programs are saved
	Path3   <- ""
	# Path 4: directory where output and logfiles are saved
	Path4   <- ""
	.libPaths("")


# Working environment 1: remote access to original data set at RDC
# ----------------------------------------------------------------
if (RDC == 1)  {
	Path1   <- "" 
	Path2   <- "" 
	Path3   <- "" 
	Path4	<- ""
	.libPaths("")

	
#================================================================================================
# 3) Run programs
#================================================================================================

# start the log file
#---------------------
sink(paste(Path4, "/Water_services_demographic_changes.log", sep = ""), append = FALSE, type = c("output", "message"), split = TRUE)


# Step 1: source functions
#-------------------------
source(paste(Path3, "/01_CullmannStiel_2022_functions.R", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 2: sample construction
#----------------------------
source(paste(Path3, "/02_CullmannStiel_2022_utilities_sample_construction.R", sep = ""), echo = TRUE, max.deparse.length = 99999)
source(paste(Path3, "/03_CullmannStiel_2022_merge_municipality_data.R", sep = ""), echo = TRUE, max.deparse.length = 99999)
source(paste(Path3, "/04_CullmannStiel_2022_merge_water_statistics.R", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 3: TFP estimation
#-------------------------
source(paste(Path3, "/05_CullmannStiel_2022_TFP_estimation.R", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 4: Summary statistics
#---------------------------
source(paste(Path3, "/06_CullmannStiel_2022_descriptives.R", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 5: Costs analysis
#----------------------
# see STATA files.


# close the log file
#-------------------
sink()
  

#================================================================================================
# date ()
#===============================End of file======================================================