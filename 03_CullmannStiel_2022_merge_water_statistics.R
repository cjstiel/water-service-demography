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
#				MERGE WATER SUPPLY STATISTICS
#
#	CONTENT: Merges the data set on water supply to the utilities panel
#
#	OUTLINE: PART 1: Merge water supply statistics to utilities panel
#			 PART 2: Aggregate to firm level
# 			 PART 3: Generate variables for estimation
#					3.1 Water production
# 					3.2 Water sources own production
#					3.3 External procurement
# 					3.4 Customer structure
# 			 PART 4: Whole-period averages in water covariates
#			 		
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 20-Mar-2018 (v09)
# ----------------------------------------------------------------------------------------------					 
#
# input: data_water_gemeinden_v05_cs.dta, data_water_7w_wvu_2001-2013.dta
# output: data_wa_gem_7w_v02_cs.dta
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
library(gdata)
library(plyr)
library(stringr)
library(minqa)
library(numDeriv)
library(readstata13)



#================================================================================================
# 0.2 Load data
#================================================================================================


# load data set
# -------------
data_wg <- read.dta(file.path(Path1,"/data_water_gemeinden_v05_cs.dta"))
dim(data_wg)
data_7w <- read.dta13(file.path(Path1,"/data_water_7w_wvu_2001-2013.dta"))
dim(data_7w)



#================================================================================================
# 							START
#================================================================================================

#================================================================================================
# 1) Merge water supply statistics to utilities panel
#================================================================================================

# generate origin dummy in utilities panel
# ----------------------------------------
data_wg$wg <- 1

# rename id variable
# ------------------
data_7w$id <- data_7w$EF1

# merge water supply statistics with utilities panel
# -------------------------------------------------
data_wwg <- merge(data_wg,data_7w,by=c("id","Jahr"),all.x=TRUE)

# number of matches
# -----------------
addmargins(table(data_wwg$Jahr[data_wwg$wvu==1],useNA="ifany"))


#================================================================================================
# 2) Aggregation to firm level
#================================================================================================

# If a utility supplies 2 municipalities, there are 2 rows per year for this utility, one for each
# municipality. As we want to know how much water the utility has provided in total, we have to 
# add up all the municipalities it supplies each year. This corresponds to the structure of the 
# plant-firm level in the energy panel, with the municipalities behaving like plants.


# mean-aggregation
# ----------------
data_mean <- aggregate(cbind("id"=data_wwg$id
                             ,"EF20"=data_wwg$EF20
                             ,"EF6"=data_wwg$EF6
                             ,"eigentuemer_new"=data_wwg$eigentuemer_new
                             ,"wz"=data_wwg$wz
                             ,"bl"=data_wwg$bl
                             ,"Kreis"=as.numeric(data_wwg$Kreis)
                             ,"AGS"=as.numeric(data_wwg$AGS)
                             ,"ags_u_new"=data_wwg$ags_u_new
                             ,"EF7"=data_wwg$EF7
                             ,"Siedlung"=data_wwg$Siedlung
                             ,"Rechtsform_Zahl"=data_wwg$Rechtsform_Zahl
                             ,"Eintrittsjahr"=as.numeric(data_wwg$Eintrittsjahr)
                             ,"Austrittsjahr"=as.numeric(data_wwg$Austrittsjahr)
                             ,"baseyear"=as.numeric(data_wwg$baseyear)
                             ,"lastyear"=as.numeric(data_wwg$lastyear)
                             ,"Dauer"=as.numeric(data_wwg$Dauer)
                             ,"Lueckenjahre"=as.numeric(data_wwg$Lueckenjahre)
                             ,"wg"=data_wwg$wg
                             ,"wvu"=data_wwg$wvu
                             ,"UK_Code1501"=data_wwg$UK_Code1501
                             ,"EF24_0180"=data_wwg$EF24_0180
                             ,"UK_Code1601"=data_wwg$UK_Code1601
                             ,"UK_Code2001"=data_wwg$UK_Code2001
                             ,"UK_Code2501"=data_wwg$UK_Code2501
                             ,"EF24_0401"=data_wwg$EF24_0401
                             ,"UK_Code3701"=data_wwg$UK_Code3701
                             ,"EF24_0421"=data_wwg$EF24_0421
                             ,"EF24_0422"=data_wwg$EF24_0422
                             ,"EF24_0424"=data_wwg$EF24_0424
                             ,"UK_Code4501"=data_wwg$UK_Code4501
                             ,"UK_Code4901"=data_wwg$UK_Code4901
                             ,"UK_Code5001"=data_wwg$UK_Code5001
                             ,"EF24_0426"=data_wwg$EF24_0426
                             ,"UK_Code5201"=data_wwg$UK_Code5201
                             ,"EF24_0427"=data_wwg$EF24_0427
                             ,"UK_Code5301"=data_wwg$UK_Code5301
                             ,"UK_Code5401"=data_wwg$UK_Code5401
                             ,"UK_Code5501"=data_wwg$UK_Code5501
                             ,"UK_Code5901"=data_wwg$UK_Code5901
                             ,"UK_Code6101"=data_wwg$UK_Code6101
                             ,"UK_Code6201"=data_wwg$UK_Code6201
                             ,"UK_Code6301"=data_wwg$UK_Code6301
                             ,"UK_Code6401"=data_wwg$UK_Code6401
                             ,"UK_Code6501"=data_wwg$UK_Code6501
                             ,"UK_Code6601"=data_wwg$UK_Code6601
                             ,"UK_Code6901"=data_wwg$UK_Code6901
                             ,"UK_Code7401"=data_wwg$UK_Code7401
                             ,"UK_Code8501"=data_wwg$UK_Code8501
                             ,"UK_Code8601"=data_wwg$UK_Code8601
                             ,"UK_Code8701"=data_wwg$UK_Code8701
                             ,"UK_Code8801"=data_wwg$UK_Code8801
                             ,"UK_Code8901"=data_wwg$UK_Code8901
                             ,"UK_Code9001"=data_wwg$UK_Code9001
                             ,"UK_Code9101"=data_wwg$UK_Code9101
                             ,"EF24_9901"=data_wwg$EF24_9901
                             ,"EF24_9902"=data_wwg$EF24_9902
                             ,"UI_Code4001"=data_wwg$UI_Code4001
                             ,"UI_Code8101"=data_wwg$UI_Code8101
                             ,"EF24_9905"=data_wwg$EF24_9905
                             ,"EF24_9906"=data_wwg$EF24_9906
                             ,"EF24_9907"=data_wwg$EF24_9907
                             ,"EF24_9911"=data_wwg$EF24_9911
                             ,"PI_Invest"=data_wwg$PI_invest
                             ,"investment"=data_wwg$investment
                             ,"K_adj"=data_wwg$K_adj
                             ,"beschaeftigte"=data_wwg$beschaeftigte
                             ,"bruttolohn"=data_wwg$bruttolohn
                             ,"bruttolohn1"=data_wwg$bruttolohn1
							 ,"vorleistung"=data_wwg$vorleistung
							 ,"vorleistung1"=data_wwg$vorleistung1
                             ,"lohn_deflation_index"=data_wwg$lohn_deflation_index
                             ,"wage"=data_wwg$wage
                             ,"wage1"=data_wwg$wage1
                             ,"fremdeDL"=data_wwg$fremdeDL
                             ,"fremdeDL1"=data_wwg$fremdeDL1
                             ,"fdl_deflation_index"=data_wwg$fdl_deflation_index
                             ,"intermediates"=data_wwg$intermediates
                             ,"intermediates1"=data_wwg$intermediates1
                             ,"int_deflation_index"=data_wwg$int_deflation_index
                             ,"steuern"=data_wwg$steuern
                             ,"wage_share"=data_wwg$wage_share
                             ,"service_share"=data_wwg$service_share
                             ,"int_share"=data_wwg$int_share
                             ,"Bevoelk_insg"=data_wwg$Bevoelk_insg
                             ,"Bevoelk_U18"=data_wwg$Bevoelk_U18
                             ,"Bevoelk_18_60"=data_wwg$Bevoelk_18_60
                             ,"Bevoelk_60plus"=data_wwg$Bevoelk_60plus
                             ,"Flaeche_qkm"=data_wwg$Flaeche_qkm
                             ,"Einnahmen"=data_wwg$Einnahmen
                             ,"Ausgaben"=data_wwg$Ausgaben
                             ,"Schulden"=data_wwg$Schulden
                             ,"popdens"=data_wwg$popdens
                             ,"pd_t1"=data_wwg$pd_t1
                             ,"pd_kum_l"=data_wwg$pd_kum_l
                             ,"popdens_corr"=data_wwg$popdens_corr
                             ,"pd_t1_corr"=data_wwg$pd_t1_corr
                             ,"pd_kum_l_corr"=data_wwg$pd_kum_l_corr
                             ,"s_young"=data_wwg$s_young
                             ,"s_old"=data_wwg$s_old
                             ,"g_young"=data_wwg$g_young
                             ,"g_old"=data_wwg$g_old
                             ,"wat_Grundentgelt"=data_wwg$wat_Grundentgelt_EUR
                             ,"wat_Arbeitsentgelt_m3"=data_wwg$wat_Arbeitsentgelt_EUR_m3
                             ,"LF_ha"=data_wwg$LF_ha
                             ,"LW_ha"=data_wwg$LW_ha
                             ,"Siedl_ha"=data_wwg$Siedl_ha
                             ,"Acker_ha"=data_wwg$Acker_ha
                             ,"Wald_ha"=data_wwg$Wald_ha
                             ,"Wass_ha"=data_wwg$Wass_ha
                             ,"hoehe_m"=data_wwg$hoehe_m
                             ,"LF_ha_mean"=data_wwg$LF_ha_mean
                             ,"LW_ha_mean"=data_wwg$LW_ha_mean
                             ,"Siedl_ha_mean"=data_wwg$Siedl_ha_mean
                             ,"Acker_ha_mean"=data_wwg$Acker_ha_mean
                             ,"Wald_ha_mean"=data_wwg$Wald_ha_mean
                             ,"Wass_ha_mean"=data_wwg$Wass_ha_mean
                             ,"hoehe_m_mean"=data_wwg$hoehe_m_mean
                             ,"LFW_ha_mean"=data_wwg$LFW_ha_mean
                             ,"shareSV"=data_wwg$shareSV
                             ,"shareWald"=data_wwg$shareWald
                             ,"shareWass"=data_wwg$shareWass
                             ,"shareA"=data_wwg$shareA
                             ,"shareLW"=data_wwg$shareLW
                             ,"wvu_EF2_alt"=as.numeric(data_wwg$wvu_EF2_alt)
                             ,"wvu_EF2"=as.numeric(data_wwg$wvu_EF2)
                             ,"wvu_EF2U1"=as.numeric(data_wwg$wvu_EF2U1)
                             ,"wvu_EF2U2"=as.numeric(data_wwg$wvu_EF2U2)
                             ,"wvu_EF2U3"=as.numeric(data_wwg$wvu_EF2U3)
                             ,"wvu_EF2U4"=as.numeric(data_wwg$wvu_EF2U4)
                             ,"wvu_EF2U5"=as.numeric(data_wwg$wvu_EF2U5)
                             ,"wvu_EF3U4_alt"=as.numeric(data_wwg$wvu_EF3U4_alt)
                             ,"wvu_EF3"=as.numeric(data_wwg$wvu_EF3)
                             ,"wvu_EF3U1"=as.numeric(data_wwg$wvu_EF3U1)
                             ,"wvu_EF3U2"=as.numeric(data_wwg$wvu_EF3U2)
                             ,"wvu_EF3U3"=as.numeric(data_wwg$wvu_EF3U3)
                             ,"wvu_EF4U1"=as.numeric(data_wwg$wvu_EF4U1)
                             ,"wvu_EF4U2"=as.numeric(data_wwg$wvu_EF4U2)
                             ,"wvu_EF4U3"=as.numeric(data_wwg$wvu_EF4U3)
                             ,"wvu_EF5"=data_wwg$wvu_EF5
                             ,"wvu_EF6"=data_wwg$wvu_EF6
                             ,"wvu_EF7"=data_wwg$wvu_EF7
                             ,"wvu_EF8"=data_wwg$wvu_EF8
                             ,"wvu_EF9"=data_wwg$wvu_EF9
                             ,"wvu_EF10"=data_wwg$wvu_EF10
                             ,"wvu_EF11"=data_wwg$wvu_EF11
                             ,"wvu_EF12"=data_wwg$wvu_EF12
                             ,"wvu_EF13"=data_wwg$wvu_EF13
                             ,"wvu_EF14"=data_wwg$wvu_EF14
                             ,"wvu_EF15"=data_wwg$wvu_EF15
                             ,"wvu_EF16"=data_wwg$wvu_EF16
                             ,"wvu_EF17"=data_wwg$wvu_EF17
                             ,"wvu_EF18"=data_wwg$wvu_EF18
                             ,"wvu_EF19"=data_wwg$wvu_EF19
                             ,"wvu_EF20"=data_wwg$wvu_EF20
                             ,"wvu_EF21"=data_wwg$wvu_EF21
                             ,"wvu_EF22"=data_wwg$wvu_EF22
                             ,"wvu_EF23"=data_wwg$wvu_EF23
                             ,"wvu_EF24"=data_wwg$wvu_EF24
                             ,"wvu_EF25"=data_wwg$wvu_EF25
                             ,"wvu_EF26"=data_wwg$wvu_EF26
                             ,"wvu_EF27"=data_wwg$wvu_EF27
                             ,"wvu_EF28"=data_wwg$wvu_EF28
                             ,"wvu_EF29"=data_wwg$wvu_EF29
                             ,"wvu_EF30"=data_wwg$wvu_EF30
                             ,"wvu_EF31"=data_wwg$wvu_EF31
                             ,"wvu_EF32"=data_wwg$wvu_EF32
                             ,"wvu_EF33"=data_wwg$wvu_EF33
                             ,"wvu_EF34"=data_wwg$wvu_EF34
                             ,"wvu_EF35U2_alt"=data_wwg$wvu_EF35U2_alt)
                       ,by=list("unr"=data_wwg$unr,"Jahr"=data_wwg$Jahr),mean, na.rm=TRUE)

# sum aggregation
# ---------------
data_sum <- aggregate(cbind("B_MBE_EF11_mean"=data_wwg$B_MBE_EF11_mean
                            ,"B_MBE_EF13_mean"=data_wwg$B_MBE_EF13_mean
                            ,"B_MBE_EF15_mean"=data_wwg$B_MBE_EF15_mean
                            ,"B_MBE_EF17_mean"=data_wwg$B_MBE_EF17_mean)
                      ,by=list("unr"=data_wwg$unr,"Jahr"=data_wwg$Jahr),sum,na.rm=TRUE)


# max aggregation
# ---------------
data_max <- aggregate(cbind("wa"=data_wwg$wa
                            ,"aw"=data_wwg$aw
                            ,"sa"=data_wwg$sa
                            ,"sn"=data_wwg$sn
                            ,"se"=data_wwg$se
                            ,"ga"=data_wwg$ga
                            ,"wm"=data_wwg$wm
                            ,"afs"=data_wwg$afs)
                      ,by=list("unr"=data_wwg$unr,"Jahr"=data_wwg$Jahr),max,na.rm=TRUE)


# merge the three data sets
# -------------------------
data_U0 <- merge(data_mean,data_sum,by=c("unr","Jahr"))
data_U1 <- merge(data_U0,data_max,by=c("unr","Jahr"))
addmargins(table(data_U1$Jahr,useNA="ifany"))


# number of matches
addmargins(table(data_U1$Jahr[data_U1$wvu==1],useNA="ifany"))
# Note: The sum should correspond to the number of observations in data_wg, see dim() 
# at the beginning of the file.

# check: number of utilities in energy panel (with population data)
addmargins(table(data_U1$Jahr[data_U1$wg==1],useNA="ifany"))

# recode dummies to 0/1
data_U1$wvu[is.nan(data_U1$wvu)==TRUE] <- 0

# recode dummies to 0/1
data_U1$wg[is.nan(data_U1$wg)==TRUE] <- 0



#================================================================================================
# 3) Generate variables for estimation
#================================================================================================

#=================================================================================================
# 3.1 Water production                                          
#=================================================================================================

# bulk water supply
# -----------------
# how many utilities are pure bulk water supply companies?
data_U1$BWS <- ifelse(is.na(data_U1$UK_Code8701)==FALSE & data_U1$UK_Code8701>0 
                    & data_U1$UK_Code8601==0 | is.na(data_U1$UK_Code8601)==TRUE,1,0)
addmargins(table(as.factor(data_U1$BWS),data_U1$Jahr,useNA="ifany"))

# share bulk water supply in total supply
data_U1$shareWV <- ifelse(is.na(data_U1$UK_Code8501)==FALSE & is.na(data_U1$UK_Code8701)==FALSE 
                        & data_U1$UK_Code8701>0,data_U1$UK_Code8501/data_U1$UK_Code8701,0)

# how many utilities supply bulk water?
addmargins(table(data_U1$Jahr[data_U1$shareWV==0 & data_U1$aw==0],useNA="ifany"))

# relevance of bulk water supply among them
dstat(data_U1$shareWV[data_U1$shareWV!=0],d=2)


#================================================================================================
# 3.2 water sources own production
#================================================================================================

# share groundwater
# ------------------
data_U1$shareGW <- ifelse(is.na(data_U1$wvu_EF11)==FALSE & data_U1$wvu_EF11>0
                         ,data_U1$wvu_EF5/data_U1$wvu_EF11,NA)


# share spring water
# ------------------
data_U1$shareQW <- ifelse(is.na(data_U1$wvu_EF11)==FALSE & data_U1$wvu_EF11>0
                         ,data_U1$wvu_EF6/data_U1$wvu_EF11,NA)

# share bank filtrate
# -------------------
data_U1$shareUF <- ifelse(is.na(data_U1$wvu_EF11)==FALSE & data_U1$wvu_EF11>0
                         ,data_U1$wvu_EF7/data_U1$wvu_EF11,NA)


# share enriched groundwater
# --------------------------
data_U1$shareGWa <- ifelse(is.na(data_U1$wvu_EF11)==FALSE & data_U1$wvu_EF11>0
                          ,data_U1$wvu_EF8/data_U1$wvu_EF11,NA)


# share lake and reservoir water
# --------------------------------
data_U1$shareST <- ifelse(is.na(data_U1$wvu_EF11)==FALSE & data_U1$wvu_EF11>0
                         ,data_U1$wvu_EF9/data_U1$wvu_EF11,NA)

# share river water
# ------------------
data_U1$shareFW <- ifelse(is.na(data_U1$wvu_EF11)==FALSE & data_U1$wvu_EF11>0
                         ,data_U1$wvu_EF10/data_U1$wvu_EF11,NA)



#================================================================================================
# 3.3 external procurement
#================================================================================================

# share external procurement in total supply
# ------------------------------------------
data_U1$shareFB <- ifelse(is.na(data_U1$wvu_EF17)==FALSE & data_U1$wvu_EF17>0
                         ,data_U1$wvu_EF16/data_U1$wvu_EF17,NA)


# share procurement from other utilities in total external procurement
# --------------------------------------------------------------------
data_U1$shareFBW <- ifelse(is.na(data_U1$wvu_EF16)==FALSE & data_U1$wvu_EF16>0
                          ,data_U1$wvu_EF12/data_U1$wvu_EF16,NA)


# share procurement from industry and other suppliers in total external procurement
# ---------------------------------------------------------------------------------
data_U1$shareFBI <- ifelse(is.na(data_U1$wvu_EF16)==FALSE & data_U1$wvu_EF16>0
                          ,data_U1$wvu_EF13/data_U1$wvu_EF16,NA)


# share origin other federal states in total external procurement
# ---------------------------------------------------------------
data_U1$shareFBB <- ifelse(is.na(data_U1$wvu_EF16)==FALSE & data_U1$wvu_EF16>0
                          ,data_U1$wvu_EF14/data_U1$wvu_EF16,NA)


# share origin abroad in total external procurement 
# -------------------------------------------------
data_U1$shareFBA <- ifelse(is.na(data_U1$wvu_EF16)==FALSE & data_U1$wvu_EF16>0
                          ,data_U1$wvu_EF15/data_U1$wvu_EF16,NA)



#================================================================================================
# 3.4 customer structure
#================================================================================================

# share supply to households and small businesses in total supply to end-consumers
# --------------------------------------------------------------------------------
data_U1$shareHH <- ifelse(is.na(data_U1$wvu_EF25)==FALSE & data_U1$wvu_EF25>0
                         ,data_U1$wvu_EF26/data_U1$wvu_EF25,NA)


# share redistribution in total water supply
# ------------------------------------------
data_U1$shareWV2 <- ifelse(is.na(data_U1$wvu_EF33)==FALSE & data_U1$wvu_EF33>0
                          ,data_U1$wvu_EF31/data_U1$wvu_EF33,NA)




#================================================================================================
# 4) Whole-period averages in water covariates
#================================================================================================

# aggregate
# ---------
mean_wvu <- aggregate(cbind("shareGW_mean"=data_U1$shareGW
                            ,"shareQW_mean"=data_U1$shareQW
                            ,"shareUF_mean"=data_U1$shareUF
                            ,"shareGWa_mean"=data_U1$shareGWa
                            ,"shareST_mean"=data_U1$shareST
                            ,"shareFW_mean"=data_U1$shareFW
                            ,"shareFB_mean"=data_U1$shareFB
                            ,"shareFBW_mean"=data_U1$shareFBW
                            ,"shareFBI_mean"=data_U1$shareFBI
                            ,"shareFBA_mean"=data_U1$shareFBA
                            ,"shareHH_mean"=data_U1$shareHH
                            ,"shareWV2_mean"=data_U1$shareWV2
                            ,"WEG1_mean"=data_U1$wvu_EF3U1
                            ,"WEG2_mean"=data_U1$wvu_EF3U2
                            ,"WEG3_mean"=data_U1$wvu_EF3U3
                            ,"FGE1_mean"=data_U1$wvu_EF4U1
                            ,"FGE2_mean"=data_U1$wvu_EF4U2)
                      ,by=list("id"=data_U1$id),mean,na.rm=TRUE)

# summary statistics (number of NA's)
# -----------------------------------
dstat(mean_wvu[,c(2:17)])

# How many fraction numbers in WEG and FGE variables (river basins)?
# ------------------------------------------------------------------
length(which(mean_wvu$WEG1_mean!=round(mean_wvu$WEG1_mean,0))==TRUE)
length(which(mean_wvu$WEG2_mean!=round(mean_wvu$WEG2_mean,0))==TRUE)
length(which(mean_wvu$WEG3_mean!=round(mean_wvu$WEG3_mean,0))==TRUE)
length(which(mean_wvu$FGE1_mean!=round(mean_wvu$FGE1_mean,0))==TRUE)
length(which(mean_wvu$FGE2_mean!=round(mean_wvu$FGE2_mean,0))==TRUE)

# replace fraction numbers by 'NA'
# -------------------------------
mean_wvu$WEG1_mean[mean_wvu$WEG1_mean!=round(mean_wvu$WEG1_mean,0)] <- NA
mean_wvu$WEG2_mean[mean_wvu$WEG2_mean!=round(mean_wvu$WEG2_mean,0)] <- NA
mean_wvu$WEG3_mean[mean_wvu$WEG3_mean!=round(mean_wvu$WEG3_mean,0)] <- NA
mean_wvu$FGE1_mean[mean_wvu$FGE1_mean!=round(mean_wvu$FGE1_mean,0)] <- NA
mean_wvu$FGE2_mean[mean_wvu$FGE2_mean!=round(mean_wvu$FGE2_mean,0)] <- NA

data_U2 <- merge(data_U1,mean_wvu,by="id",all.x=TRUE)

# replace NAs by zeroes in water production and supply variables
# --------------------------------------------------------------
data_U2$shareQW_mean[is.na(data_U2$shareQW_mean)==TRUE] <- 0
data_U2$shareGW_mean[is.na(data_U2$shareGW_mean)==TRUE] <- 0
data_U2$shareFB_mean[is.na(data_U2$shareFB_mean)==TRUE] <- 0
data_U2$shareHH_mean[is.na(data_U2$shareHH_mean)==TRUE] <- 0




#================================================================================================
# 5) Save data set
#================================================================================================


# Set empty cells to NA since STATA cannot cope with empty strings
data_U2[data_U2=="."] <- NA
data_U2[data_U2==""] <- NA


# save data set
write.dta(data_U2,paste(Path1,"data_wa_gem_7w_v02_cs.dta",sep=""),version=10)

#=================================================================================================
date()
#========================================== End of file ==========================================