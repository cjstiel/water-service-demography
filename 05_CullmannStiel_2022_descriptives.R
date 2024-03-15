#================================================================================================
#		  Article: Cost and Productivity Effects of Demographic Changes on 
#				Local Water Service. 
#							
# published in: Utilities Policy, 2022, 79(101435).
# authors: Astrid Cullmann, Caroline Stiel	
# affiliations: Technische Universitaet Berlin, DIW Berlin	
#				
#================================================================================================
#													                 ´
#	
#						SUMMARY STATISTICS
#
#	CONTENT: A descriptive analysis of the estimation sample
#
#	OUTLINE: PART 1: Demographics
#					1.1 Population density
#					1.2 Age structure
#					1.3 Share household supply in end-consumer supply
#			 PART 2: Inputs and output
# 			 PART 3: Topography
# 			 PART 4: Vertical structure
#			 PART 5: Water supply sources
#					5.1 Raw water composition
#			 		5.2 Water catchment areas
#			 		
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 28-May-2018 (v30)
# ----------------------------------------------------------------------------------------------					 
#
# input: data_wa_acf_v17_cs.dta
# output: -
#
#================================================================================================

#================================================================================================
#				0) Preparation                                              
#================================================================================================


date()


#================================================================================================
#  0.1 Packages	     	      
#================================================================================================

# load packages
# -------------
library(reshape)
library(foreign)
library(data.table)
library(gdata)
library(plm)
library(car)
library(ggplot2)
library(readstata13)




#================================================================================================
# 0.2 Load data
#================================================================================================


# load data set
# -------------
data_wg <- read.dta(file.path(Path1,"data_wa_acf_v17_cs.dta"))

class(data_wg)
dim(data_wg)



#================================================================================================
# 							START
#================================================================================================

# recode data as panel data: i = utility's id, t = year
# -----------------------------------------------------
data_p <- pdata.frame(data_wg, index=c("id","Jahr"),row.names=FALSE)
pdim(data_p)


#================================================================================================
# 1) Demographics
#================================================================================================

#================================================================================================
# 1.1 Population density
#================================================================================================

#---------------------------------------------------------------
# Table 1: Summary statistics demographics
#---------------------------------------------------------------

# levels
# ------
dstat(data_wg$popdens_corr)

# growth
# ------
dstat(data_wg$pd_t1_corr,d=4)

# correlation
# ----------
cor(data_wg$popdens_corr,data_wg$pd_t1_corr,use="complete.obs",method="pearson")



#================================================================================================
# 1.2 Age structure
#================================================================================================

#---------------------------------------------------------------
# Table 1: Summary statistics demographics
#---------------------------------------------------------------

# under 18 (levels)
# ---------------------
dstat(data_wg$s_young,d=2)

# under 18 (growth)
# -----------------
dstat(data_wg$g_young,d=4)

# correlations
# ------------
cor(data_wg$s_young,data_wg$g_young,use="complete.obs",method="pearson")



# above 60 (levels)
# -----------------
dstat(data_wg$s_old,d=2)

# above 60 (growth)
# -----------------
dstat(data_wg$g_old,d=4)

# correlations
# ------------
cor(data_wg$s_old,data_wg$g_old,use="complete.obs",method="pearson")



#================================================================================================
# 1.3 Share household supply in end-consumer supply
#================================================================================================


#---------------------------------------------------------------
# Table 1: Summary statistics demographics
#---------------------------------------------------------------


# How many utilities supply households and how important is household supply in end-consumer supply?
# ---------------------------------------------------------------------------
dstat(as.data.frame(data_p$shareHH_mean[data_p$shareHH_mean>0]),d=2)


#================================================================================================
# 2) Inputs and output
#================================================================================================

#-----------------------------------------------
# Table 3: Summary statistics: inputs and output
#-----------------------------------------------

# labour costs
# ------------
dstat(data_wg$bruttolohn/10^6,d=2)


# average wage per hour
# ---------------------
dstat(data_wg$wage,d=2)


# capital stock
# -------------
dstat(data_wg$K_adj/10^6,d=2)


# expenditure for external services
# ---------------------------------
dstat(data_wg$fremdeDL/10^6,d=2)


# expenditure for intermediate goods
# ----------------------------------
dstat(data_wg$intermediates/10^6,d=2)
dstat(data_p$intermediates/10^6,d=2)


# total water supplied
# ---------------------
data_wg$UK_Code8701[data_wg$UK_Code8701==0] <- NA
dstat(data_wg$UK_Code8701/10^3,d=2)


#================================================================================================
# 3) Topography
#================================================================================================

#--------------------------------------------------------
# Table 4: Summary statistics: Topography in supply areas
#--------------------------------------------------------


# share residential area and infrastructure area
# ----------------------------------------------
dstat(data_wg$shareSV_mean,d=2)

# share forest
# -------------
dstat(data_wg$shareWald_mean,d=2)

# share surface water
# -------------------
dstat(data_wg$shareWass_mean,d=2)

# share agriculture
# -----------------
dstat(data_wg$shareLW_mean,d=2)

# altitude
# --------
dstat(data_wg$hoehe_m_mean)



#================================================================================================
# 4) Vertical structure
#================================================================================================

#-----------------------------------------------------------
# Table 5: Summary statistics: utilities' vertical structure
#-----------------------------------------------------------


# share water from external sources
# ---------------------------------
dstat(as.data.frame(data_p$shareFB_mean[data_p$shareFB_mean>0]),d=2)


# share wholesale in total output
# -------------------------------
dstat(data_wg$shareWV[data_wg$shareWV!=0],d=2)




#================================================================================================
# 5) Water supply sources
#================================================================================================

#================================================================================================
# 5.1 Raw water composition
#================================================================================================

#-------------------------------------------------------------
# Table 6: Summary statistics: utilities' water supply sources
#-------------------------------------------------------------

# share groundwater in raw water
# ------------------------------
dstat(as.data.frame(data_p$shareGW_mean[data_p$shareGW_mean>0]),d=2)

# share spring water in raw water
# -------------------------------.
dstat(as.data.frame(data_p$shareQW_mean[data_p$shareQW_mean>0]),d=2)



#================================================================================================
# 5.2 Water catchment areas
#================================================================================================

#-------------------------------------------------------------
# Table 6: Summary statistics: utilities' water supply sources
#-------------------------------------------------------------

# The WEG-1 digit (wvu_EF3U1) provides information on the water catchment area in which the 
# utility is located. Path-2-digit and WEG-3-digit are geographical refinements.

# 1: Danube
# 2: Rhine
# 3: Ems
# 4: Weser
# 5: Elbe
# 6: Oder
# 9: Coast

# Summary statistics river basin locations
# ----------------------------------------
table(data_wg$WEG1_Donau,useNA="ifany")
table(data_wg$WEG1_Rhein,useNA="ifany")
table(data_wg$WEG1_Ems,useNA="ifany")
table(data_wg$WEG1_Weser,useNA="ifany")
table(data_wg$WEG1_ElbOdCo,useNA="ifany")



#=================================================================================================
date()
#========================================== End of file ==========================================