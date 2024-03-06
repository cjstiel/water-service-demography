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
#						SAMPLE CONSTRUCTION
#
#
#	CONTENT: Prepares the utilities data set.
#
#	OUTLINE: PART 1: Convert and recode data
# 					1.1 Energy statistics (KSE etc.)
#					1.2 JAB survey
# 					1.3 URS register
#			 PART 2: Generate control variables for economic activities
#					2.1 NACE codes
# 					2.2 Impute the fuel type for gas-fired power and heat plants in 2012
#					2.3 Industry fixed effects
# 			 PART 3: Aggregate plant-level data to firm level
#					3.1 Aggregation
#					3.2 Cleaning
# 			 PART 4: Definition of a 'public firm'
#					4.1 Number of matches with JAB survey
#					4.2 Interpolate public ownership
#					4.3 Interpolate distinction between full/mixed ownership
#					4.4 Define subset of public firms for the analysis
# 					4.5 Analyse public ownership in water sector
#			 PART 5: Define subset of water utilities
#			 PART 6: Balanced panel?
# 			 PART 7: Generate input variables for the production function estimation
# 					7.1 Capital stock
#					7.2 Labour
#					7.3 Hourly wages
# 					7.4 External services
# 					7.5 Material / intermediates
#					7.6 Vorleistungen (external services + intermediates)
#					7.7 Revenues
#			 PART 8: Cost shares in revenues
# 			 PART 9: Drop outliers
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 15-Nov-2017 (v19)
# ----------------------------------------------------------------------------------------------					 
#
# input: <original data set name>.dta 
# output: data_water_structural_v09_cs.dta
#	        
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
library(boot)
library(car)
library(ggplot2)
library(gdata)
library(plyr)
library(stringr)
library(minqa)
library(numDeriv)
library(readstata13)




#================================================================================================
# 0.2 Define functions to be used in the analysis              	        
#================================================================================================


#================================================================================================
# Function 'dstat'                                                                                   
# ---------------                                                                                 
# Calculates descriptive statistics: Provides q1, q5 q25 q50, q75, q95, q99 quantiles, 
# ratio q75/q25, ratio 95/q5, mean, variance, standard deviation, sum, number of zeroes, 
# number of '.', NA's, max, second-hightes value.
#                                                                                                 
# Inputs: X - data frame with the variables for which descriptives statistics should be calculated
# Inputs: d - number of digits to which R shall round (default=0)                      
#================================================================================================

dstat <- function(X,d){
  X <- as.matrix(X)
  mat <- matrix(NA, ncol=17, nrow=ncol(X))
  colnames(mat) <- c("q1","q5","q25","med","mean","q75","q95","q99","var", "sd", "sum","nonNAs"
                     , "zeroes", "number '.'","NAs","max","secondhighest")
  rownames(mat) <- colnames(X)
  mat[,1] <- round(apply(X, 2, quantile, probs=0.01, na.rm=T),digits=d)
  mat[,2] <- round(apply(X, 2, quantile, probs=0.05, na.rm=T),digits=d)
  mat[,3] <- round(apply(X, 2, quantile, probs=0.25, na.rm=T),digits=d)
  mat[,4] <- round(apply(X, 2, median, na.rm=T),digits=d)
  mat[,5] <- round(apply(X, 2, mean, na.rm=T),digits=d)
  mat[,6] <- round(apply(X, 2, quantile, probs=0.75, na.rm=T),digits=d)
  mat[,7] <- round(apply(X, 2, quantile, probs=0.95, na.rm=T),digits=d)
  mat[,8] <- round(apply(X, 2, quantile, probs=0.99, na.rm=T),digits=d)
  mat[,9] <- round(apply(X, 2, var,na.rm=T),digits=d)
  mat[,10] <- round(apply(X, 2, sd, na.rm=T), digits=d)
  mat[,11] <- round(apply(X, 2, sum, na.rm=T), digits=d)
  mat[,12] <- round(apply(X, 2, nobs),digits=d)
  for (i in 1:ncol(X)) {mat[i,13] <- length(which(X[,i]==0))[1]}
  for (i in 1:ncol(X)) {mat[i,14] <- length(which(X[,i]=="."))[1]}
  for (i in 1:ncol(X)) {mat[i,15] <- length(which(is.na(X[,i])))[1]}
  mat[,16] <- round(apply(X, 2, max, na.rm=T), digits=d)
  for (i in 1:ncol(X)) {mat[i,17] <- round(sort(X[,i],decreasing=T)[2],digits=d)}
  return(mat)
}




#================================================================================================
# 0.3 Load data
#================================================================================================

# load data set
# -------------
data <- read.dta13(file.path(Path2,"/<original data set name>.dta"))  

class(data)
dim(data)


#================================================================================================
# 							START
#================================================================================================

#================================================================================================
# 1) Convert and recode data
#================================================================================================

#================================================================================================
# 1.1 Energy statistics (KSE etc.)
#================================================================================================

# Convert some variables to numeric format
# ----------------------------------------
if(class(data$wz_b) != "numeric") data$wz_b <- as.numeric(data$wz_b)
if(class(data$wz_u) != "numeric") data$wz_u <- as.numeric(data$wz_u)  
if(class(data$bnr) != "numeric") data$bnr <- as.numeric(data$bnr)
if(class(data$unr) != "numeric") data$unr <- as.numeric(data$unr)

data$ags_u_new <- ifelse(data$ags_u=="." | data$ags_u=="",NA,as.numeric(data$ags_u))
if(class(data$ags_u_new) != "numeric") data$ags_u_new <- as.numeric(data$ags_u_new)



# Extract county info (Kreisebene) from location identifier (Allgemeiner Gemeindeschluessel: AGS)
# -----------------------------------------------------------------------------------------------
summary(as.factor(nchar(data$ags_u_new)))

# AGs with 8 digits: first 5 digits
# AGS with 7 digits: first 4 digits
# AGS with 5 digits: all 5 digits
# AGS with 4 digits: all 4 digits
# AGS with 2 digits: fill in with '000'
# AGS with 1 digit: fill in with '0000'
.
data$Kreis <- ifelse(nchar(data$ags_u_new)==8,substr(data$ags_u_new,1,5)
                            ,ifelse(nchar(data$ags_u_new)==7,substr(data$ags_u_new,1,4)
                                   ,ifelse(nchar(data$ags_u_new)==2 | nchar(data$ags_u_new)==1
                                          ,paste(data$ags_u_new,"000",sep="")
                                          ,data$ags_u_new)))


# Drop observations with missing values in the index variables 'unr' and 'Jahr'
# ----------------------------------------------------------------------------
dim(data)
data$unr[data$unr=="."] <- NA
data$unr[data$unr==Inf] <- NA
data$unr[data$unr==-Inf] <- NA
data <- data[!is.na(data$unr),]
dim(data)

data$Jahr[data$Jahr=="."] <- NA
data$Jahr[data$Jahr==Inf] <- NA
data$Jahr[data$Jahr==-Inf] <- NA
data <- data[!is.na(data$Jahr),]
dim(data)

length(which(is.na(data$bnr)==TRUE))
length(which(is.na(data$bnr)==TRUE))


# Recode survey participation variables to binary (0/1) variable
# --------------------------------------------------------------
if(class(data$TM064_b) == "factor") data$TM064_b <- as.character(data$TM064_b)
data$TM064_b[data$TM064_b=="ja"] <- 1
data$TM064_b[data$TM064_b=="nein" | is.na(data$TM064_b)==TRUE | data$TM064_b==""] <- 0
if(class(data$TM064_b) == "character") data$TM064_b <- as.numeric(data$TM064_b)
if(class(data$TM065_b) == "factor") data$TM065_b <- as.character(data$TM065_b)
data$TM065_b[data$TM065_b=="ja"] <- 1
data$TM065_b[data$TM065_b=="nein" | is.na(data$TM065_b)==TRUE | data$TM065_b==""] <- 0
if(class(data$TM065_b) == "character") data$TM065_b <- as.numeric(data$TM065_b)
if(class(data$TM066K_b) == "factor") data$TM066K_b <- as.character(data$TM066K_b)
data$TM066K_b[data$TM066K_b=="ja"] <- 1
data$TM066K_b[data$TM066K_b=="nein" | is.na(data$TM066K_b)==TRUE | data$TM066K_b==""] <- 0
if(class(data$TM066K_b) == "character") data$TM066K_b <- as.numeric(data$TM066K_b)
if(class(data$TM070_u) == "factor") data$TM070_u <- as.character(data$TM070_u)
data$TM070_u[data$TM070_u=="ja"] <- 1
data$TM070_u[data$TM070_u=="nein" | is.na(data$TM070_u)==TRUE | data$TM070_u==""] <- 0
if(class(data$TM070_u) == "character") data$TM070_u <- as.numeric(data$TM070_u)
if(class(data$TM083_u) == "factor") data$TM083_u <- as.character(data$TM083_u)
data$TM083_u[data$TM083_u=="ja"] <- 1
data$TM083_u[data$TM083_u=="nein" | is.na(data$TM083_u)==TRUE | data$TM083_u==""] <- 0
if(class(data$TM083_u) == "character") data$TM083_u <- as.numeric(data$TM083_u)
if(class(data$TM066N_u) == "factor") data$TM066N_u <- as.character(data$TM066N_u)
data$TM066N_u[data$TM066N_u=="ja"] <- 1
data$TM066N_u[data$TM066N_u=="nein" | is.na(data$TM066N_u)==TRUE | data$TM066N_u==""] <- 0
if(class(data$TM066N_u) == "character") data$TM066N_u <- as.numeric(data$TM066N_u)


# Recode sector activities: NA -> 0
# ---------------------------------
data$UI_Code11_1[is.na(data$UI_Code11_1)==TRUE] <- 0
data$UI_Code11_2[is.na(data$UI_Code11_2)==TRUE] <- 0
data$UI_Code11_3[is.na(data$UI_Code11_3)==TRUE] <- 0
data$UI_Code11_4[is.na(data$UI_Code11_4)==TRUE] <- 0
data$UI_Code11_5[is.na(data$UI_Code11_5)==TRUE] <- 0
data$UI_Code11_6[is.na(data$UI_Code11_6)==TRUE] <- 0
data$UI_Code11_7[is.na(data$UI_Code11_7)==TRUE] <- 0
data$BI_Code11_1[is.na(data$BI_Code11_1)==TRUE] <- 0
data$BI_Code11_2[is.na(data$BI_Code11_2)==TRUE] <- 0
data$BI_Code11_3[is.na(data$BI_Code11_3)==TRUE] <- 0
data$BI_Code11_4[is.na(data$BI_Code11_4)==TRUE] <- 0
data$BI_Code11_5[is.na(data$BI_Code11_5)==TRUE] <- 0
data$BI_Code11_6[is.na(data$BI_Code11_6)==TRUE] <- 0
data$BI_Code11_7[is.na(data$BI_Code11_7)==TRUE] <- 0


# Recode federal states to numbers
# --------------------------------
# firm level
levels(as.factor(data$bl_u))
data$bl <- as.numeric(factor(data$bl_u))
levels(as.factor(data$bl))

# plant level
levels(as.factor(data$bl_b))
data$bl_b_new <- as.numeric(factor(data$bl_b))
levels(as.factor(data$bl_b_new))


# Replace missing information for federal states at firm level with info from plant level
# ----------------------------------------------------------------------------------------
length(which(is.na(data$bl)==TRUE)==TRUE)
data$bl[is.na(data$bl)==TRUE] <- data$bl_b_new[is.na(data$bl)==TRUE]
length(which(is.na(data$bl)==TRUE)==TRUE)
levels(as.factor(data$bl))


# Replace missing information for NACE code at firm level with info from plant level
# ----------------------------------------------------------------------------------
length(which(is.na(data$wz_u)==TRUE)==TRUE)
data$wz_u[is.na(data$wz_u)==TRUE] <- data$wz_b[is.na(data$wz_u)==TRUE]
length(which(is.na(data$wz_u)==TRUE)==TRUE)


# Recode legal form information from KSE survey as number
# -------------------------------------------------------
levels(as.factor(data$Rechtsform))
data$Rechtsform_Zahl[data$Rechtsform=="keine Angaben"] <- 0
data$Rechtsform_Zahl[data$Rechtsform=="Einzelfirma"] <- 1
data$Rechtsform_Zahl[data$Rechtsform=="ohg"] <- 2
data$Rechtsform_Zahl[data$Rechtsform=="kg"] <- 3
data$Rechtsform_Zahl[data$Rechtsform=="GmbH & Co KG"] <- 4
data$Rechtsform_Zahl[data$Rechtsform=="GmbH"] <- 5
data$Rechtsform_Zahl[data$Rechtsform=="AG bzw. KGaA"] <- 6
data$Rechtsform_Zahl[data$Rechtsform=="Genossenschaft"] <- 7
data$Rechtsform_Zahl[data$Rechtsform=="Eigenbetrieb"] <- 8
data$Rechtsform_Zahl[data$Rechtsform=="Verband"] <- 9
data$Rechtsform_Zahl[data$Rechtsform=="sonstiges"] <- 10
levels(as.factor(data$Rechtsform_Zahl))


# Generate variable with fuel type information (power/heat plants)
# ----------------------------------------------------------------
# tabulate original variable from the survey
levels(as.factor(data$Hauptenergietraeger))

# hard coal: 1
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 1 & data$Hauptenergietraeger<= 5] <- 1 
# lignite: 2
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 11 & data$Hauptenergietraeger<= 17] <- 2 
# oil: 3
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 21 & data$Hauptenergietraeger<= 27] <- 3 
# natural gas: 4
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & (data$Hauptenergietraeger>= 31 & data$Hauptenergietraeger<= 35)] <- 4
# water: 5
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 41 & data$Hauptenergietraeger<= 44] <- 5                
# wind: 6
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 45] <- 6 
# solar: 7
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 46] <- 7 
# geothermal energy: 8
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 47] <- 8 
# biomass: 9
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 51 & data$Hauptenergietraeger<= 52] <- 9    
# biogas: 10
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 53 & data$Hauptenergietraeger<= 56] <- 10                
# other RES: 11
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 57] <- 11

# waste: 12
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 61 & data$Hauptenergietraeger<= 63] <- 12 

# nuclear power: 13
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 71] <- 13

# miscellaneous: 14
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & (data$Hauptenergietraeger>= 72 & data$Hauptenergietraeger<= 81)] <- 14

levels(as.factor(data$HETGruppen))

summary(as.factor(data$Hauptenergietraeger))



#================================================================================================
# 1.2 JAB survey (Jahresabschluesse oeffentlicher Fonds, Einrichtungen und Unternehmen)
#================================================================================================


# Check the format of the identifier 'jab'
# ----------------------------------------
# Note: The variable 'jab' identifies all entities that took part in the JAB survey.
dstat(data$jab)
length(which(data$jab==Inf))
length(which(data$jab==-Inf))
data$jab[data$jab=="."] <- 0
addmargins(table(data$jab,data$Jahr,useNA="ifany"))

class(data$jab)
if(class(data$jab) != "numeric") data$jab <- as.numeric(data$jab)
class(data$jab)


# Convert location identifier (AGS) to numeric format
# ---------------------------------------------------
data$EF7 <- ifelse(data$EF7=="." | data$EF7=="",NA,data$EF7)
data$EF7_n <- as.numeric(data$EF7)

summary(data$EF7)
dstat(data$EF7_n)


# Recode ownership type
# ---------------------
# Note: Needs to be recoded for the years 2012-2014 to obtain consistent results through the whole
# sample period since as the survey setup slightly changed after 2010.
class(data$EF10)
data$EF10_neu <- ifelse((data$Jahr==2012 | data$Jahr==2013 | data$Jahr==2014) & data$EF10==1,1
                        ,ifelse((data$Jahr==2012 | data$Jahr==2013 | data$Jahr==2014) & data$EF10==2,6
                                ,ifelse(data$EF10==11 | data$EF10==13,2
                                        ,ifelse(data$EF10==12 | data$EF10==14,7
                                                ,ifelse(data$EF10==21 | data$EF10==23 | data$EF10==31 
                                                        | data$EF10==33 | data$EF10==41 | data$EF10==43
                                                        | data$EF10==61 | data$EF10==63 | data$EF10==71
                                                        | data$EF10==73 | data$EF10==81 | data$EF10==83 
                                                        | data$EF10==93 | data$EF10==51 | data$EF10==53,3
                                                        ,ifelse(data$EF10==22 | data$EF10==24 | data$EF10==32
                                                                | data$EF10==34 | data$EF10==42 | data$EF10==44
                                                                | data$EF10==62 | data$EF10==64 | data$EF10==72
                                                                | data$EF10==74 | data$EF10==82 | data$EF10==84
                                                                | data$EF10==94 | data$EF10==52 | data$EF10==54,8
                                                                ,ifelse(data$EF10==147 | data$EF10==103,4
                                                                        ,ifelse(data$EF10==148 | data$EF10==104,9
                                                                                ,ifelse(data$EF10==125 | data$EF10==135,5
                                                                                        ,ifelse(data$EF10==126 | data$EF10==136,0,data$EF10))))))))))
levels(as.factor(data$EF10_neu))
class(data$EF10_neu)


# Generate variable with ownership information (fully public vs. mixed ownership)
# --------------------------------------------------------------------------------
# Note: Based on variable EF10 from the JAB survey.

# fully public based on variable EF10, categories 1-5
data$eigentuemer[is.na(data$EF10_neu)==FALSE & data$EF10_neu>0 & data$EF10_neu<=5] <- 2

# fully public: A single owner who is not in mixed ownership
data$eigentuemer[is.na(data$EF10_neu)==TRUE & data$EF20==1 & data$EF21U1_1!=9] <- 2

# fully public: Entity has the legal form 'Eigenbetrieb'
data$eigentuemer[is.na(data$EF10_neu)==TRUE & data$Rechtsform=="Eigenbetrieb"] <- 2

# mixed ownership (public majority) based on variable EF10, categories 0, 6-9
data$eigentuemer[is.na(data$EF10_neu)==FALSE & (data$EF10_neu==0 | data$EF10_neu>5)] <- 1

addmargins(table(data$eigentuemer,data$Jahr,useNA="ifany"))
class(data$eigentuemer)
if(class(data$eigentuemer) != "numeric") data$eigentuemer <- as.numeric(data$eigentuemer)
addmargins(table(data$eigentuemer,data$Jahr,useNA="ifany"))



#================================================================================================
# 1.3 URS (Unternehmensregister)
#================================================================================================


# Convert public firm identifier to numeric format
# ------------------------------------------------
table(data$urs_public,data$Jahr,useNA="ifany")
if(class(data$urs_public) != "numeric") data$urs_public <- as.numeric(data$urs_public)
table(data$urs_public,data$Jahr,useNA="ifany")



#================================================================================================
# 2) Generate control variables for economic activities (industries)                                     
#================================================================================================

#================================================================================================
# 2.1 NACE code
#================================================================================================

# Generate a uniform 4-digit NACE code
# ------------------------------------
dstat(data$wz_u)
data$wz <- as.numeric(substr(data$wz_u,1,4))
dstat(data$wz)



#================================================================================================
# 2.2 Impute the fuel type for gas-fired power and heat plants in 2012
#================================================================================================

# Reason: In 2012, there is a unusual decline in gas-fired power plants vis-à-vis 2011 and 2013
# together with an increase in missing values.
table(data$HETGruppen,data$Jahr,useNA="ifany")

# Select the relevant variables
# -----------------------------
data_gkw <- subset(data, select=c(bnr,Jahr,HETGruppen))

# Identify all power plants that are gas-fired in 2011 and 2013
# -------------------------------------------------------------
data_gkw$dgkw[data_gkw$Jahr==2011 & is.na(data_gkw$HETGruppen)==FALSE & data_gkw$HETGruppen==4] <- 1
data_gkw$dgkw[data_gkw$Jahr==2013 & is.na(data_gkw$HETGruppen)==FALSE & data_gkw$HETGruppen==4] <- 1
data1 <- aggregate(cbind("dgkw"=dgkw) ~ bnr, data_gkw,sum,na.rm=TRUE)

# Normalize to 1 and merge information to original data set
# ---------------------------------------------------------
data1$dgkw <- ifelse(data1$dgkw==2,1,NA)
data <- merge(data,data1,by=c("bnr"),all.x=TRUE)

# Replace fuel type for all plants with missing fuel type in 2012 which are gas-fired in 2011 and 2013
# ----------------------------------------------------------------------------------------------------
data$HETGruppen <- ifelse(is.na(data$dgkw)==FALSE & data$Jahr==2012,4,data$HETGruppen)
table(data$HETGruppen,data$Jahr,useNA="ifany")



#================================================================================================
# 2.3 Industry fixed effects
#================================================================================================

# Generate industry fixed effects. The industry fixed effects are not mutually exclusive and
# allow to charactereize the product space of multi-product firms.

# electricity retail
# ------------------
# criterion: participation in survey no. 83
data$sa <- ifelse(data$TM083_u==1,1,0)

# electricity distribution
# ------------------------
# criterion: participation in surveys no. 70 or no. 66N
data$sn <- ifelse((data$TM070_u>0 | data$TM066N_u>0),1,0)

# electricity generation
# ----------------------
# criterion: participation in survey no. 66K
data$se <- ifelse(data$TM066K_b==1,1,0)

# heat supply
# -----------
# criterion: participation in survey no. 64_b or positive number of employees
# in the field of heat supply reported in survey no. 65.
data$wm <- ifelse(data$TM064_b==1 | (is.na(data$B_MBE_EF15_mean)==FALSE 
                                     & data$B_MBE_EF15_mean>0),1,0)
# gas supply
# ----------
# criterion: reported gas activity in KSE survey (firm/plant-level) or positive number of employees
# in the field of gas supply reported in survey no. 65.
data$ga <- ifelse(data$UI_Code11_3==1 | data$BI_Code11_3==1 
                  | (is.na(data$B_MBE_EF13_mean)==FALSE 
                     & data$B_MBE_EF13_mean>0),1,0)
# water supply
# ------------
# criterion: reported water activity in KSE survey (firm/plant-level) or positive amount of water 
# sold reported in KSE survey or positive number of employees in the field of water supply reported 
# in survey no. 65 or NACE code of 3600 or 4100.
data$wa <- ifelse(data$UI_Code11_4==1 | data$BI_Code11_4==1
                  | (is.na(data$UK_Code8701)==FALSE & data$UK_Code8701>0) 
                  | (is.na(data$B_MBE_EF17_mean)==FALSE 
                     & data$B_MBE_EF17_mean>0) 
                  | is.na(data$wz)==FALSE & (data$wz==3600 | data$wz==4100),1,0)

# sewerage
# --------
# criterion: reported sewerage activity in KSE survey (firm/plant-level) or NACE code of 3700, 
# 9000, or 9001.
data$aw <- ifelse(data$UI_Code11_5==1 | data$BI_Code11_5==1 | is.na(data$wz)==FALSE 
                  & (data$wz==3700 | data$wz==9000 | data$wz==9001),1,0)

# waste management
# ----------------
data$af <- ifelse(data$UI_Code11_6==1 | data$BI_Code11_6==1 | is.na(data$wz)==FALSE 
                  & (grepl(data$wz,pattern="38")==TRUE
                     | grepl(data$wz,pattern="39")==TRUE
                     | data$wz==9002),1,0)


# Have all plants been sorted into at least one industry?
# -------------------------------------------------------
data_rest <- subset(data, sa==0 & sn==0 & se==0 & wa==0 & aw==0 & af==0)
nrow(data_rest)


# Label the remaining plants together with the waste management plants as 'miscellaneous'
# ---------------------------------------------------------------------------------------
data$afs <- ifelse(data$sa==0 & data$sn==0 & data$se==0 & data$wa==0 & data$aw==0,1,0)


# Check: There should be no more plants without industry affiliation
# ------------------------------------------------------------------
data_rest2 <- subset(data, sa==0 & sn==0 & se==0 & wa==0 & aw==0 & afs==0)
nrow(data_rest2)



#================================================================================================
# 3) Aggregate plant-level data to firm level
#================================================================================================

#================================================================================================
# 3.1 Aggregation
#================================================================================================


# Step 1: Compute mean of all variables observed at the firm level (=remain unchanged)
# ------------------------------------------------------------------------------------
data_mean <- aggregate(cbind("jab"=data$jab
                             ,"urs_public"=data$urs_public
                             ,"EF20"=data$EF20
                             ,"EF6"=data$EF6
                             ,"eigentuemer"=data$eigentuemer
                             ,"wz"=data$wz
                             ,"bl"=data$bl
                             ,"Kreis"=as.numeric(data$Kreis)
                             ,"ags_u_new"=data$ags_u_new
                             ,"EF7"=data$EF7_n
                             ,"Siedlung"=data$Siedlung
                             ,"Rechtsform_Zahl"=data$Rechtsform_Zahl
                             ,"UK_Code1501"=data$UK_Code1501
                             ,"EF24_0180"=data$EF24_0180
                             ,"UK_Code1601"=data$UK_Code1601
                             ,"UK_Code2001"=data$UK_Code2001
                             ,"UK_Code2501"=data$UK_Code2501
                             ,"EF24_0401"=data$EF24_0401
                             ,"UK_Code3701"=data$UK_Code3701
                             ,"EF24_0421"=data$EF24_0421
                             ,"EF24_0422"=data$EF24_0422
                             ,"EF24_0424"=data$EF24_0424
                             ,"UK_Code4501"=data$UK_Code4501
                             ,"UK_Code4901"=data$UK_Code4901
                             ,"UK_Code5001"=data$UK_Code5001
                             ,"EF24_0426"=data$EF24_0426
                             ,"UK_Code5201"=data$UK_Code5201
                             ,"EF24_0427"=data$EF24_0427
                             ,"UK_Code5301"=data$UK_Code5301
							,"UK_Code5401"=data$UK_Code5401
							,"UK_Code5501"=data$UK_Code5501
							,"UK_Code5901"=data$UK_Code5901
                             ,"UK_Code6101"=data$UK_Code6101
							,"UK_Code6201"=data$UK_Code6201
							,"UK_Code6301"=data$UK_Code6301
							,"UK_Code6401"=data$UK_Code6401
							,"UK_Code6501"=data$UK_Code6501
							,"UK_Code6601"=data$UK_Code6601
                             ,"UK_Code6901"=data$UK_Code6901
                             ,"UK_Code7401"=data$UK_Code7401
                             ,"UK_Code8501"=data$UK_Code8501
                             ,"UK_Code8601"=data$UK_Code8601
                             ,"UK_Code8701"=data$UK_Code8701
                             ,"UK_Code8801"=data$UK_Code8801
                             ,"UK_Code8901"=data$UK_Code8901
                             ,"UK_Code9001"=data$UK_Code9001
                             ,"UK_Code9101"=data$UK_Code9101
                             ,"EF24_9901"=data$EF24_9901
                             ,"EF24_9902"=data$EF24_9902
                             ,"UI_Code4001"=data$UI_Code4001
                             ,"UI_Code8101"=data$UI_Code8101
                             ,"EF24_9905"=data$EF24_9905
                             ,"EF24_9906"=data$EF24_9906
                             ,"EF24_9907"=data$EF24_9907
                             ,"EF24_9911"=data$EF24_9911)
                       ,by=list("unr"=data$unr,"Jahr"=data$Jahr),mean, na.rm=TRUE)


# Step 2: Aggregate plant-level variables to firm level (sum)
# -----------------------------------------------------------
# Notes: Use sum function for quantity data (e.g., input and output volumes)
data_sum <- aggregate(cbind("B_MBE_EF11_mean"=data$B_MBE_EF11_mean
                            ,"B_MBE_EF13_mean"=data$B_MBE_EF13_mean
                            ,"B_MBE_EF15_mean"=data$B_MBE_EF15_mean
                            ,"B_MBE_EF17_mean"=data$B_MBE_EF17_mean)
                      ,by=list("unr"=data$unr,"Jahr"=data$Jahr),sum,na.rm=TRUE)



# Step 3: Aggregate plant-level variables to firm level (max)
# -----------------------------------------------------------
# Notes: Use max function for binary varables (fixed effects).
data_max <- aggregate(cbind("wa"=data$wa
                            ,"aw"=data$aw
                            ,"sa"=data$sa
                            ,"sn"=data$sn
                            ,"se"=data$se
                            ,"ga"=data$ga
                            ,"wm"=data$wm
                            ,"afs"=data$afs)
                      ,by=list("unr"=data$unr,"Jahr"=data$Jahr),max,na.rm=TRUE)


# Merge the three data sets
# -------------------------
data_U0 <- merge(data_mean,data_sum,by=c("unr","Jahr"))
data_U1 <- merge(data_U0,data_max,by=c("unr","Jahr"))
addmargins(table(data_U1$Jahr,useNA="ifany"))


# Rename index variable 'unr' -> 'id'
# -----------------------------------
data_U1$id <- data_U1$unr
data_all <- data_U1




#================================================================================================
# 3.2 Cleaning
#================================================================================================


# Settlement structure
# --------------------
# create a new category '5 = mixed settlement structure' for non-integer settlement structures 
# after aggregation
length(which(data_all$Siedlung!=round(data_all$Siedlung,0))==TRUE)
data_all$Siedlung[data_all$Siedlung!=round(data_all$Siedlung,0)] <- 5

# NACE codes
# ----------
# Create a new category '9999 = mixed NACE codes' for non-integer NACE codes after aggregation
length(which(data_all$wz!=round(data_all$wz,0))==TRUE)
data_all$wz[data_all$wz!=round(data_all$wz,0)] <- 9999

# Federal states
# --------------
# Create a new category '17 = mixed location firm' for non-integer federal states  after 
# aggregation 
length(which(data_all$bl!=round(data_all$bl,0))==TRUE)
data_all$bl[data_all$bl!=round(data_all$bl,0)] <- 17

# Municipality
# ------------
# Create a new category '99999999 = mixed location firm' for non-integer municipality codes 
# after aggregation
length(which(data_all$ags_u_new!=round(data_all$ags_u_new,0))==TRUE)
data_all$ags_u_new[data_all$ags_u_new!=round(data_all$ags_u_new,0)] <- 99999999

# Mixed ownership
# ----------------
# If a firm has mixed ownership values < 2, it must have at least one plant in mixed ownership
# so that we will classify the whole firm as being in 'mixed ownership' 
data_all$eigentuemer_new[is.na(data_all$eigentuemer)==FALSE & data_all$eigentuemer<2] <- 1
data_all$eigentuemer_new[is.na(data_all$eigentuemer)==FALSE & data_all$eigentuemer==2] <- 2
data_all$eigentuemer_new[is.na(data_all$eigentuemer)==TRUE] <- NA
summary(as.factor(data_all$eigentuemer_new))

# Fuel type
# ---------
# Create a new category '15 = mixed fuels' for non-integer fuel types after aggregation
length(which(data_all$HETGruppen!=round(data_all$HETGruppen,0))==TRUE)
data_all$HETGruppen[data_all$HETGruppen!=round(data_all$HETGruppen,0)] <- 15


# Generate for each utility their entry and exit year into the data set
# ---------------------------------------------------------------------
entry <- as.data.frame(aggregate(cbind("Eintrittsjahr"=data_all$Jahr)
                                 ,by=list("id"=data_all$id),min,na.rm=TRUE))
exit <- as.data.frame(aggregate(cbind("Austrittsjahr"=data_all$Jahr)
                                ,by=list("id"=data_all$id),max,na.rm=TRUE))
entryexit <- merge(entry,exit,by=c("id"),all=FALSE)
data_all <- merge(data_all,entryexit,by=c("id"),all=TRUE)






#================================================================================================
# 4) Definition of a 'public firm'		            	                                   
#================================================================================================

# time stamp
# ----------
date()

#================================================================================================
# 4.1 Number of matches with JAB survey 
#================================================================================================

# The survey 'Jahresabschluesse oeffentlicher Fonds, Einrichtungen und Unternehmen' collects 
# information on all firms where public entities hold at least 50 percent of the shares/votes.

# How many firms have matches with the JAB survey?
# ------------------------------------------------
addmargins(table(data_all$Jahr[data_all$jab==1]))

# For how many firms do we have at least one plant with a match in the JAB survey?
# --------------------------------------------------------------------------------
addmargins(table(data_all$Jahr[data_all$jab>0 & data_all$jab!=1]))



#================================================================================================
# 4.2 Interpolate public ownership
#================================================================================================

#================================================================================================
# 4.2.1 Continuity of participation in JAB survey             
#================================================================================================

# Idea:
# -----
# If a firm participated in the JAB survey in the previous and following year, it is very likely
# that it is also a public firm in the current year despite a missing match.


# Algorithm:
# ----------
# 1) Build a subset with all observations of a federal state.
# 2) Firms whose ownership status is to be replaced shall fulfill 3 conditions:
#    a) There is no match with the JAB survey in the current year.
#    b) There is a match with the JAB survey in the previous year.
#    c) There is a match with the JAB survey in the following year.
# 3) Firms that fulfill all three conditions have the auxiliary binary variable set to 1 (vs. NA).
# 4) Merge the binary variable to the original data set. Note: The relevant firms will have the
#    binary variable set to 1 for all years where they are observed.
# 5) Set public = 1 for all firm whose binary variable != NA in the 'missing' year. For all other 
#    firms and years, keep information from original JAB variable. 
#    Notes: Use binary variable != NA since binary == 1 leads to wrong results.


# Niedersachsen 2005
# ------------------
data_ns <- subset(data_all, bl==3, select=c(id,Jahr,bl,jab))
data_ns$id1[data_ns$Jahr==2005 & data_ns$jab==0] <- 1
data_ns$id1[data_ns$Jahr==2004 & data_ns$jab==1] <- 1
data1 <- aggregate(cbind("id_ns"=id1) ~ id, data_ns,sum)
data1$id_ns <- ifelse(data1$id_ns==2,1,NA)
data_all_new <- merge(data_all,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_ns)==FALSE & data_all_new$Jahr==2005,1,data_all_new$jab)


# Hessen 2003
# -----------
data_he <- subset(data_all, bl==6, select=c(id,Jahr,bl,jab))
data_he$id1[data_he$Jahr==2003 & data_he$jab==0] <- 1
data_he$id1[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he1"=id1) ~ id, data_he,sum)
data1$id_he1 <- ifelse(data1$id_he1==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he1)==FALSE & data_all_new$Jahr==2003,1
                            ,data_all_new$public)

# Hessen 2004
# -----------
data_he$id2[data_he$Jahr==2004 & data_he$jab==0] <- 1
data_he$id2[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he2"=id2) ~ id, data_he,sum)
data1$id_he2 <- ifelse(data1$id_he2==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he2)==FALSE & data_all_new$Jahr==2004,1
                            ,data_all_new$public)


# Hessen 2006
# -----------
data_he$id3[data_he$Jahr==2006 & data_he$jab==0] <- 1
data_he$id3[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he3"=id3) ~ id, data_he,sum)
data1$id_he3 <- ifelse(data1$id_he3==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he3)==FALSE & data_all_new$Jahr==2006,1
                            ,data_all_new$public)


# Hessen 2007
# -----------
data_he$id4[data_he$Jahr==2007 & data_he$jab==0] <- 1
data_he$id4[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he4"=id4) ~ id, data_he,sum)
data1$id_he4 <- ifelse(data1$id_he4==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he4)==FALSE & data_all_new$Jahr==2007,1
                            ,data_all_new$public)


# Thueringen 2006
# ---------------
data_th <- subset(data_all,bl==16, select=c(id,Jahr,bl,jab))
data_th$id1[data_th$Jahr==2006 & data_th$jab==0] <- 1
data_th$id1[data_th$Jahr==2005 & data_th$jab==1] <- 1
data_th$id1[data_th$Jahr==2007 & data_th$jab==1] <- 1
data1 <- aggregate(cbind("id_th"=id1) ~ id, data_th,sum)
data1$id_th <- ifelse(data1$id_th==3,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_th)==FALSE & data_all_new$Jahr==2006,1
                            ,data_all_new$public)


# Schleswig-Holstein
# ------------------
data_sh <- subset(data_all, bl==1, select=c(id,Jahr,bl,jab))
data_sh$id1[data_sh$Jahr==2003 & data_sh$jab==0] <- 1
data_sh$id1[data_sh$Jahr==2004 & data_sh$jab==1] <- 1
data1 <- aggregate(cbind("id_sh"=id1) ~ id, data_sh,sum)
data1$id_sh <- ifelse(data1$id_sh==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_sh)==FALSE & data_all_new$Jahr==2003,1
                            ,data_all_new$public)


#================================================================================================
# 4.2.2 Legal form 'Eigenbetrieb'            
#================================================================================================

# Only fully publicly-owned utilities can choose the legal form 'Eigenbetrieb'. Hence all firms 
# that report there legal status to be 'Eigenbetrieb' must be (fully) public.

# Declare all utilities with the legal form 'Eigenbetrieb' to be public
# ---------------------------------------------------------------------
data_all_new$public <- ifelse(is.na(data_all_new$Rechtsform_Zahl)==FALSE 
                            & data_all_new$Rechtsform_Zahl==8,1,data_all_new$public)


#================================================================================================
# 4.2.3 'urs public' variable
#================================================================================================

# For the years 2013 and 2014, additionally use information from the company register (URS) on
# public ownership 

# Set all utilities with urs_public>0 to public
# ---------------------------------------------
data_all_new$public <- ifelse(is.na(data_all_new$urs_public)==FALSE & data_all_new$urs_public>0,1
                              ,data_all_new$public)


#================================================================================================
# 4.2.4 Check: Result of interpolation          
#================================================================================================

# Check result of interpolation ('jab' (old variable ) -> 'public' (new variable))
# ---------------------------------------------------------------------------------
summary(as.factor(data_all_new$jab))
summary(as.factor(data_all_new$public))

# Table public utilities by federal state
# ---------------------------------------
addmargins(table(data_all_new$bl[data_all_new$public>0],data_all_new$Jahr[data_all_new$public>0]
                 ,useNA="ifany"))

# Table private utilities by federal state
# -----------------------------------------
addmargins(table(data_all_new$bl[data_all_new$public==0],data_all_new$Jahr[data_all_new$public==0]
                 ,useNA="ifany"))


# time stamp
# ----------
date()




#================================================================================================
# 4.3 Interpolate distinction between full/mixed ownership
#================================================================================================


# Compute the average of the categorical variable characterising full/mixed ownership. Use the
# min function, i.e. assume if a firm is in mixed ownership during the years where its status is
# observed it will also be in mixed ownership during the years where the information is missing.

# Apply min function
# ------------------
data3 <- aggregate(cbind("eigner_neu"=data_all_new$eigentuemer_new),by=list("id"=data_all_new$id)
                   ,min,na.rm=TRUE)
data3$eigner_neu[data3$eigner_neu==Inf] <- NA

# Merge the time-invariant information on full/mixed ownership due to the original data set
# -----------------------------------------------------------------------------------------
data_all_new <- merge(data_all_new,data3,by=c("id"),all.x=TRUE)

# Generate a new variable 'eigentuemer2'
# --------------------------------------
# Replaces the information on full/mixed ownership in the years w/o status info with the 
# time-invariant variable
data_all_new$eigentuemer2 <- ifelse(data_all_new$public==1 & is.na(data_all_new$eigentuemer_new)==TRUE
                                    ,data_all_new$eigner_neu,data_all_new$eigentuemer_new)

# Compare frequencies before and after interpolation
# --------------------------------------------------
addmargins(table(data_all_new$eigentuemer_new,data_all_new$Jahr,useNA="ifany"))
addmargins(table(data_all_new$eigentuemer2,data_all_new$Jahr,useNA="ifany"))


#================================================================================================
# 4.4 Define subset of public firms for the analysis
#================================================================================================

# Define subset without private firms
# -----------------------------------
data_public <- subset(data_all_new, public==1)



#================================================================================================
# 4.5 Analyse public ownership in water sector
#================================================================================================


# How many of the pure water utilities are publicly owned?
# ------------------------------------------------------
data_public_wa <- subset(data_public, wa==1 & sa==0 & sn==0 & se==0 & wm==0 & ga==0 & aw==0
                         & afs==0)
addmargins(table(data_public_wa$Jahr))


# How many of the pure sewerage utilities are publicly owned?
# ----------------------------------------------------------
data_public_aw <- subset(data_public, wa==0 & sa==0 & sn==0 & se==0 & wm==0 & ga==0 & aw==1
                         & afs==0)
addmargins(table(data_public_aw$Jahr))


# How many mixed utilities (water and sewerage) are publicly owned?
# -----------------------------------------------------------------
data_public_awa <- subset(data_public, wa==1 & sa==0 & sn==0 & se==0 & wm==0 & ga==0 & aw==1
                          & afs==0)
addmargins(table(data_public_awa$Jahr))


# For comparison: all public water/sewerage utilities incl. those supplying electricity etc.
# ------------------------------------------------------------------------------------------
data_public_w_gesamt <- subset(data_public, wa==1)
addmargins(table(data_public_w_gesamt$Jahr))

data_public_aw_gesamt <- subset(data_public, aw==1)
addmargins(table(data_public_aw_gesamt$Jahr))


# For comparison: all (public & private) water/utilities incl. those supplying electricity etc.
# ---------------------------------------------------------------------------------------------
data_w_gesamt <- subset(data_all_new, wa==1)
addmargins(table(data_w_gesamt$Jahr))




#================================================================================================
# 5) Define subset of water utilities
#================================================================================================

# Focus on water and sewerage utilities in the analysis. Notably, keep
# (a) pure water utilities
# (b) pure sewerage utilities
# (c) pure water and sewerage utilities

data2 <- subset(data_all_new, (wa==1 & sa==0 & sn==0 & se==0 & wm==0 & ga==0 & aw==0 & afs==0)
                | (wa==0 & sa==0 & sn==0 & se==0 & wm==0 & ga==0 & aw==1 & afs==0)
                | (wa==1 & sa==0 & sn==0 & se==0 & wm==0 & ga==0 & aw==1 & afs==0))

nrow(data2)





#================================================================================================
# 6) Balanced Panel?
#================================================================================================


# entry and exit into sample 'data_public'
# -----------------------------------------
# Eintrittsjahr = first-time observation in full panel 'data_all'
# Baseyear = first-time observation in public panel 'data_public'.

entry1 <- as.data.frame(aggregate(cbind("baseyear"=data2$Jahr)
                                        ,by=list("id"=data2$id),min,na.rm=TRUE))
exit1 <- as.data.frame(aggregate(cbind("lastyear"=data2$Jahr)
                                       ,by=list("id"=data2$id),max,na.rm=TRUE))
entryexit1 <- merge(entry1,exit1,by=c("id"),all=FALSE)
data2 <- merge(data2,entryexit1,by="id",all=TRUE)

cbind("Entry"=addmargins(table(data2$baseyear,useNA="ifany"))
      ,"Exit"=addmargins(table(data2$lastyear,useNA="ifany")))



# Number of years observed
# ------------------------
# Count the number of years for each utility. Subsequently, table frequencies.
Dauer <- aggregate(cbind("Dauer"=data2$Jahr),by=list("id"=data2$id),length)
table(as.factor(Dauer$Dauer),dnn="Anzahl Jahre")
data2 <- merge(data2,Dauer,by=c("id"),all.x=TRUE)



# How many utilitiea are not continuosly observed?
# ---------------------------------------------------
# Notes: This means that either the utility is not in the main sample or it is not a
# pure water/sewerage utility in some years.
data2$Lueckenjahre <- (data2$lastyear-data2$baseyear+1-data2$Dauer)
summary(as.factor(data2$Lueckenjahre))

# Drop all utilities which are not continuously observed
# ------------------------------------------------------
data2 <- subset(data2,Lueckenjahre==0)
addmargins(table(data2$Jahr))





#================================================================================================
# 7) Generate input variables for the production function estimation
#================================================================================================

date()

# Notes: Most of the variables are available both in the KSE survey (Kostenstrukturerhebung bei 
# Unternehmen der Energie- und Wasserversorgung, sowie bei Unternehmen zur Abwasser- und Abfall-
# entsorgung und Beseitigung von Umweltverschmutzungen) and the JAB survey (Jahresabschluesse 
# oeffentlicher Fonds, Einrichtungen und Unternehmen). 

# The former contains balance sheet data information for both private and public firms, while
# the latter contains balance sheet data for public firms only.

# Most of the time, we use the information from the KSE survey. If data is missing, we use the
# information from the JAB survey. The only exception is data on the capital stock, this 
# information is only available in the JAB survey. 

#================================================================================================
# 7.1 Capital stock
#================================================================================================

# Use the perpetual inventory method (PIM) to compute the capital stock based on the initial 
# capital stock and current investments minus depreciation.
#================================================================================================
# 7.1.1 Initial capital stock
#================================================================================================

# Definition of initial capital stock (K_0)
# -----------------------------------------
# K_0 = K_start[Baseyear]
# K_start = EF24_9901-EF24_9906 ('assets at the beginning of the year' - 'depreciation at the 
# beginning of year')

# Calculate K_start
# -----------------
data2$K_start <- data2$EF24_9901 - data2$EF24_9906
data2$K_start[data2$K_start==0 | is.nan(data2$K_start)==TRUE] <- NA


# For how many firm-year observations is the information missing?
# ---------------------------------------------------------------
dstat(data2$K_start/10^6,d=2)


# Build the subset of firm-year observations for which values have to be imputed
# ------------------------------------------------------------------------------
data8 <- subset(data2, is.na(data2$K_start)==TRUE)
data9 <-sapply(unique(data8$id),function(x)which(data2[,"id"]==x))
data9 <-data2[unlist(data9),]


# Calculate the growth rate of assets in the remaining years
# ----------------------------------------------------------
# K_growth_t = (K_t+1_start - K_t_start)/K_t_start
# Idea: Checks for each utility whether in the current and next year information on tangible
# assets are available. If so, the growth rate is calculated, otherwise left NA.

for (i in levels(as.factor(data9$id)))
{
  for (j in seq(2003,max(unique(data9$Jahr))-1))
  {ifelse(data9$id==i & data9$Jahr==j & is.na(data9$K_start[data9$id==i & data9$Jahr==j])==FALSE
          & is.na(data9$K_start[data9$id==i & data9$Jahr==j+1])==FALSE
          ,data9$capgrowth[data9$id==i & data9$Jahr==j] <-  (data9$K_start[data9$id==i & data9$Jahr==j+1]-data9$K_start[data9$id==i & data9$Jahr==j])/data9$K_start[data9$id==i & data9$Jahr==j]
          ,NA)
  }}
dstat(data9$capgrowth,d=2)


# Next, calculate average growth rate g over all years (per utility)
# ------------------------------------------------------------------
capgrowth_av <- aggregate(cbind("capgrowth_av"=data9$capgrowth),by=list("id"=data9$id),mean
                          ,na.rm=TRUE)
data9 <- merge(data9,capgrowth_av,by="id")
dstat(data9$capgrowth_av,d=2)

date()

# Impute missing information on K_start with linear interpolation
# ---------------------------------------------------------------
# K_t+k = (1+g)^k*K_t
# Idea: Checks for each firm-year observation whether information on K_start is available.
# If not, but if K_start is available in one of the following years, impute current K_start using
# the average growth rate as stated in the formula.

data9$cap_new3 <- NA
date()
for (i in levels(as.factor(data9$id))){
  for (j in levels(as.factor(data9$Jahr[data9$id==i]))){
    for (z in levels(as.factor(data9$Jahr[data9$id==i]))){
      ifelse(data9$id==i & data9$Jahr==j & is.na(data9$K_start[data9$id==i & data9$Jahr==j])==TRUE
             & is.na(data9$K_start[data9$id==i & data9$Jahr==z])==FALSE
             & is.na(data9$cap_new3[data9$id==i & data9$Jahr==j])==TRUE
             ,data9$cap_new3[data9$id==i & data9$Jahr==j] <-  data9$K_start[data9$id==i & data9$Jahr==z]/(1+data9$capgrowth_av[data9$id==i & data9$Jahr==j])^(as.numeric(z)-as.numeric(j))
             ,data9$cap_new3[data9$id==i & data9$Jahr==j])
    }}}
date()


# Merge imputed values to original data set
# -----------------------------------------
data2 <- merge(data2,subset(data9,select=c(id,Jahr,cap_new3)),by=c("id","Jahr"),all.x=TRUE)

# Replace missing values - if possible - by imputed values
# --------------------------------------------------------
data2$K_start_new <- ifelse(is.na(data2$K_start)==FALSE,data2$K_start,data2$cap_new3)
dstat(data2$K_start_new/10^6,d=2)

# Correct for implausible values
# ------------------------------
data2$K_start_new[data2$K_start_new>max(data2$K_start,na.rm=TRUE)] <- NA
data2$K_start_new[data2$K_start_new<min(data2$K_start,na.rm=TRUE)] <- NA

# Deflate values with PPI for investment goods
# --------------------------------------------
data2$K_start_new_defl <- data2$K_start_new/data2$PI_invest
dstat(data2$K_start_new_defl/10^6,2)


# Compute initial capital stock
# ----------------------------
for (i in as.factor(data2$id))
{data2$K0[data2$id==i] <- data2$K_start_new_defl[data2$id==i & data2$Jahr==data2$baseyear]}
dstat(data2$K0/10^6,2)


#================================================================================================
# 7.1.2 Depreciation
#================================================================================================


# Depreciation rate per year
# --------------------------
# d=EF24_9907/EF24_9901 (depreciation in current year/assets at beginning of the year)
data2$depreciation <- data2$EF24_9907/data2$EF24_9901
data2$depreciation[data2$depreciation==Inf] <- NA
data2$depreciation[is.nan(data2$depreciation)==TRUE] <- NA
data2$depreciation[data2$depreciation==0] <- NA
data2$depreciation[data2$depreciation>1] <- NA

dstat(data2$depreciation,3)


# Compute average over all years (2003-2014) for each utility
# -----------------------------------------------------------
for (i in as.factor(data2$id))
{data2$av_depreciation[data2$id==i] <- mean(data2$depreciation[data2$id==i]
                                            ,na.rm=TRUE)}
dstat(data2$av_depreciation,3)



#================================================================================================
# 7.1.3 Investments
#================================================================================================



# Yearly investment (deflated with investment index)
# --------------------------------------------------
# I_t = EF24_9905-EF24_9901 (assets at the end of the year - assets at the beginning of the year)
# Use information on investments into tangible assets from the investment survey if the 
# information is missing in the JAB survey.
data2$investment <- data2$EF24_9905-data2$EF24_9901
data2$investment <- ifelse((data2$investment==0 | is.na(data2$investment)==TRUE)
                           & is.na(data2$UI_Code4001)==FALSE
                           ,data2$UI_Code4001
                           ,data2$investment)
dstat(data2$investment/10^6,2)


# Deflate with PPI for investment goods
# -------------------------------------
data2$invest_defl <- data2$investment/data2$PI_invest
dstat(data2$invest_defl/10^6,2)



#================================================================================================
# 7.1.4 Estimate capital stock with PIM
#================================================================================================


# -----------------------------------------------------------------------------------------
# Formula:
# K_t+1 = (1-d)*K_t + I_t+1/PI_t+1
# ------------------------------------------------------------------------------------------

date()
for (i in levels(as.factor(data2$id))){
  ifelse(data2$id==i & data2$Jahr==data2$baseyear,
         data2$K_adj[data2$id==i & data2$Jahr==data2$baseyear] <- ((1-data2$av_depreciation[data2$id==i & data2$Jahr==data2$baseyear])
                                                                   *data2$K0[data2$id==i & data2$Jahr==data2$baseyear]
                                                                   + data2$invest_defl[data2$id==i & data2$Jahr==data2$baseyear])
         ,NA)
  for (k in seq(1,max(unique(data2$Jahr)-2003))){
    ifelse(data2$id==i & data2$Jahr==data2$baseyear+k,
           data2$K_adj[data2$id==i & data2$Jahr==data2$baseyear+k] <- ((1-data2$av_depreciation[data2$id==i & data2$Jahr==data2$baseyear+k])
                                                                       *data2$K_adj[data2$id==i & data2$Jahr==data2$baseyear+k-1]
                                                                       + data2$invest_defl[data2$id==i & data2$Jahr==data2$baseyear+k])
           ,data2$K_adj)
  }}
date()


dstat(data2$K_adj/10^6,d=2)


#================================================================================================
# 7.2 Labour (Employees, FTE)
#================================================================================================

# Number of employees in KSE survey
# ---------------------------------
dstat(data2$UK_Code1501)

# Number of employees in JAB survey
# ---------------------------------
dstat(data2$EF24_0180)

# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data2$UK_Code1501,data2$EF24_0180,use="complete.obs")
data2$cor_labour <- ifelse(data2$EF24_0180!=0,data2$UK_Code1501/data2$EF24_0180,NA)
dstat(data2$cor_labour,d=2)

# Use employees from KSE data - if not available, use employees from JAB survey
# ----------------------------------------------------------------------------- 
data2$beschaeftigte <- ifelse(is.na(data2$UK_Code1501)==FALSE 
                                    & data2$UK_Code1501>0,data2$UK_Code1501
                                    ,data2$EF24_0180)
dstat(data2$beschaeftigte)



#================================================================================================
# 7.3 Hourly wages
#================================================================================================


# Compute labour costs in KSE survey 
# ----------------------------------
# labour costs = wage bill + mandatory social contributions + other social contributions
data2$bruttolohn_KSE <- (data2$UK_Code5001 + data2$UK_Code5201 
                               + ifelse(is.na(data2$UK_Code5301)==FALSE
                                        ,data2$UK_Code5301,0))
dstat(data2$bruttolohn_KSE/10^6,d=2)

# Compute labour costs in JAB survey 
# ----------------------------------
data2$bruttolohn_JAB <- data2$EF24_0426 + data2$EF24_0427
dstat(data2$bruttolohn_JAB/10^6,d=2)

# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data2$bruttolohn_KSE,data2$bruttolohn_JAB,use="complete.obs")
data2$cor_lohn <- ifelse(data2$bruttolohn_JAB!=0,data2$bruttolohn_KSE/data2$bruttolohn_JAB,NA)
dstat(data2$cor_lohn,d=2)


# Use labour costs from KSE data - if not available, use labour costs from JAB survey
# ----------------------------------------------------------------------------------- 
data2$bruttolohn1 <- ifelse(is.na(data2$bruttolohn_KSE)==FALSE 
                                  & data2$bruttolohn_KSE>0
                                  ,data2$bruttolohn_KSE
                                    ,data2$bruttolohn_JAB)


# Deflate with German labour costs index
# --------------------------------------
data2$bruttolohn <- data2$bruttolohn1/data2$lohn_deflation_index
dstat(data2$bruttolohn/10^6,d=2)


# Calculate average hourly wage using the number of hours worked from the KSE survey
# ---------------------------------------------------------------------------------
data2$wage1 <- ifelse((data2$bruttolohn>0&data2$UK_Code1601>0)
                           ,data2$bruttolohn/data2$UK_Code1601,NA)
dstat(data2$wage1)


# If the firm-level hourly wage is not available, use industry average
# --------------------------------------------------------------------
data2$wage <- ifelse(is.na(data2$wage1)==TRUE,mean(data2$wage1,na.rm=TRUE),
                                data2$wage1)
dstat(data2$wage)



#================================================================================================
# 7.4 External services
#================================================================================================


# Costs for external services in KSE survey
# ------------------------------------------
dstat(data2$UK_Code5501/10^6,d=2)


# Costs for external services in JAB survey
# ------------------------------------------
dstat(data2$EF24_0422/10^6,d=2)


# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data2$UK_Code5501,data2$EF24_0422,use="complete.obs")
data2$cor_fremdeDL <- ifelse(data2$EF24_0422!=0,data2$UK_Code5501/data2$EF24_0422,NA)
dstat(data2$cor_fremdeDL,d=2)


# Use external services from KSE data - if not available, use external services from JAB survey
# ---------------------------------------------------------------------------------------------
data2$fremdeDL1 <- ifelse(is.na(data2$UK_Code5501)==FALSE 
                                    & data2$UK_Code5501>0,data2$UK_Code5501
                                    ,data2$EF24_0422)



# Deflate with PPI for NACE M (professional services)
# ----------------------------------------------------
data2 <- data2[order(data2$id,data2$Jahr),]
data2$fremdeDL <- data2$fremdeDL1/data2$fdl_deflation_index
dstat(data2$fremdeDL/10^6,d=2)



#================================================================================================
# 7.5 Material / Intermediates                        
#================================================================================================


# Material expenditures KSE
# --------------------------
dstat(data2$UK_Code3701/10^6,d=2)


# Material expenditures JAB
# --------------------------
dstat(data2$EF24_0421/10^6,d=2)


# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data2$UK_Code3701,data2$EF24_0421,use="complete.obs")
data2$cor_mat <- ifelse(data2$EF24_0421!=0,data2$UK_Code3701/data2$EF24_0421,NA)
dstat(data2$cor_mat,d=2)

# The JAB variable is much larger than the KSE variable. Add 'commodities' and 'procured energy
# and water' to 'material expenditure' in the survey. Obtains 'intermediates'.

data2$intermediates_KSE <- (data2$UK_Code3701
                                  +ifelse(is.na(data2$UK_Code4501)==FALSE
                                          ,data2$UK_Code4501,0)
                                  +ifelse(is.na(data2$UK_Code4901)==FALSE
                                          ,data2$UK_Code4901,0))


# Compare again the correlation between KSE variable and JAB variable
# -------------------------------------------------------------------
cor(data2$intermediates_KSE,data2$EF24_0421,use="complete.obs")
data2$cor_int <- ifelse(data2$EF24_0421!=0,data2$intermediates_KSE/data2$EF24_0421,NA)
dstat(data2$cor_int,d=2)


# Use intermediates from KSE data - if not available, use intermediates from JAB survey
# -------------------------------------------------------------------------------------
data2$intermediates1 <- ifelse(is.na(data2$intermediates_KSE)==FALSE 
                                     & data2$intermediates_KSE>0
                                     ,data2$intermediates_KSE,data2$EF24_0421)


# Deflate with German PPI for intermediate goods
# ----------------------------------------------
data2$intermediates <- data2$intermediates1/data2$int_deflation_index
dstat(data2$intermediates/10^6,d=2)



#================================================================================================
# 7.6 Vorleistungen (external services + intermediates)
#================================================================================================

# sum Vorleistungen in KSE
# ------------------------
# external services (UK_Code5501) + commodities (UK_Code4901) + materiel (UK_Code3701) 
# + procured energy and water (UK_Code4501)
data2$vorleistung_KSE <- (ifelse(is.na(data2$UK_Code5501)==FALSE
                                 ,data2$UK_Code5501,0)
                          +ifelse(is.na(data2$UK_Code4901)==FALSE
                                  ,data2$UK_Code4901,0)
                          +ifelse(is.na(data2$UK_Code3701)==FALSE
                                  ,data2$UK_Code3701,0)
                          +ifelse(is.na(data2$UK_Code4501)==FALSE
                                  ,data2$UK_Code4501,0))


# sum Vorleistungen in JAB
# ------------------------
# sum = external services (EF24_0422) + material (EF24_0421)
data2$vorleistung_JAB <- (ifelse(is.na(data2$EF24_0424)==FALSE,data2$EF24_0424,0))


# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data2$vorleistung_KSE,data2$vorleistung_JAB,use="complete.obs")
data2$cor_vor <- ifelse(data2$vorleistung_JAB!=0,data2$vorleistung_KSE/data2$vorleistung_JAB,NA)
dstat(data2$cor_vor,d=2)


# Use total intermediates from KSE data - if not available, use total intermediates from JAB survey
# ------------------------------------------------------------------------------------------------
data2$vorleistung1 <- ifelse(is.na(data2$vorleistung_KSE)==FALSE
                               & data2$vorleistung_KSE>0
                               ,data2$vorleistung_KSE,data2$EF24_0424)


# Deflate with German PPI for intermediate goods
# ----------------------------------------------
data2$vorleistung <- data2$vorleistung1/data2$int_deflation_index
dstat(data2$vorleistung/10^6,d=2)



#================================================================================================
# 7.7 Revenues
#================================================================================================


# revenues in KSE
# ---------------
dstat(data2$UK_Code2501/10^6,d=2)

# revenues in JAB
# ----------------
dstat(data2$EF24_0401/10^6,d=2)

# correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data2$UK_Code2501,data2$EF24_0401,use="complete.obs")
data2$cor_umsatz <- ifelse(data2$EF24_0401!=0,data2$UK_Code2501/data2$EF24_0401,NA)
dstat(data2$cor_umsatz,d=2)

# Use revenues from KSE survey - if not available, use revenues from JAB survey
# ------------------------------------------------------------------------------
data2$revenues <- ifelse(is.na(data2$UK_Code2501)==FALSE & data2$UK_Code2501>0
                                ,data2$UK_Code2501,data2$EF24_0401)
dstat(data2$revenues/10^6,d=2)




#================================================================================================
# 8) Costs share in revenues
#================================================================================================

# labour costs share
# -------------------
data2$wage_share <- data2$bruttolohn/data2$revenues
dstat(data2$wage_share,d=2)

# intermediates cost share
# ------------------------
data2$int_share <-  data2$intermediates/data2$revenues
dstat(data2$int_share,d=2)





#================================================================================================
# 9) Drop outliers
#================================================================================================


# Criteria KSE vs. JAB
# ---------------------
# Drop all utilities whose values in KSE survey differ too much from values in JAB survey as it
# is not clear which values are the right ones. 
# Exception: employees and intermediates since definitions differ between both surveys. Furthermore,
# keep cor_*=0/NA to avoid dropping observations with missing in KSE while JAB is filled and vice versa.

# wage
data2$out_lohn <- ifelse(is.na(data2$cor_lohn)==TRUE
                               | data2$cor_lohn==0
                               | (data2$cor_lohn>0.5 & data2$cor_lohn<2),0,1)
summary(as.factor(data2$out_lohn))

# intermediates
data2$out_int <- ifelse(is.na(data2$cor_int)==TRUE
                              | data2$cor_int==0
                              | (data2$cor_int>0.3 & data2$cor_int<3),0,1)
summary(as.factor(data2$out_int))

# revenues
data2$out_umsatz <- ifelse(is.na(data2$cor_umsatz)==TRUE
                                 | data2$cor_umsatz==0
                                 | (data2$cor_umsatz>0.5 & data2$cor_umsatz<2),0,1)
summary(as.factor(data2$out_umsatz))

# clean
data2_clear1 <- subset(data2, out_lohn==0 & out_umsatz==0 & out_int==0)
addmargins(table(data2_clear1$Jahr))



# Implausible input shares
# -------------------------
data2_clear2 <- subset(data2_clear1, (is.na(wage_share)==TRUE |wage_share <100)
                            & (is.na(int_share)==TRUE | int_share<100))
addmargins(table(data2_clear2$Jahr))




# Q99.9 qantile: Drop the right 0.001 tail of the input and output distributions
# ------------------------------------------------------------------------------

out_q999 <- data2_clear2$id[as.numeric(data2_clear2$fremdeDL)> quantile(data2_clear2$fremdeDL,0.999,na.rm=TRUE)
                | as.numeric(data2_clear2$K_adj)>quantile(data2_clear2$K_adj,0.999,na.rm=TRUE)
                | as.numeric(data2_clear2$bruttolohn)>quantile(data2_clear2$bruttolohn,0.999,na.rm=TRUE)
                | as.numeric(data2_clear2$intermediates)>quantile(data2_clear2$intermediates,0.999,na.rm=TRUE)
                | as.numeric(data2_clear2$UK_Code8701)>quantile(data2_clear2$UK_Code8701,0.999,na.rm=TRUE)
                | as.numeric(data2_clear2$revenues)>quantile(data2_clear2$revenues,0.999,na.rm=TRUE)
                ]

outlier_q999 <- data2_clear2[(data2_clear2$id %in% c(out_q999)),]
addmargins(table(outlier_q999$Jahr,dnn="Anzahl Outlier basierend auf q99.9"))
data2_clear2 <- data2_clear2[!(data2_clear2$id %in% c(out_q999)),]
addmargins(table(data2_clear2$Jahr,useNA="ifany"))



# Q0.001 qantile: Drop the left 0.001 tail of the input and output distributions
# ------------------------------------------------------------------------------

out_q0001 <- data2_clear2$id[as.numeric(data2_clear2$fremdeDL)< quantile(data2_clear2$fremdeDL,0.001,na.rm=TRUE)
                | as.numeric(data2_clear2$K_adj)<quantile(data2_clear2$K_adj,0.001,na.rm=TRUE)
                | as.numeric(data2_clear2$bruttolohn)<quantile(data2_clear2$bruttolohn,0.001,na.rm=TRUE)
                | as.numeric(data2_clear2$intermediates)<quantile(data2_clear2$intermediates,0.001,na.rm=TRUE)
                | as.numeric(data2_clear2$UK_Code8701)<quantile(data2_clear2$UK_Code8701,0.001,na.rm=TRUE)
                | as.numeric(data2_clear2$revenues)<quantile(data2_clear2$revenues,0.001,na.rm=TRUE)
                ]

outlier_q0001 <- data2_clear2[(data2_clear2$id %in% c(out_q0001)),]
addmargins(table(outlier_q0001$Jahr,dnn="Anzahl Outlier basierend auf q0.001"))
data2_clear3 <- data2_clear2[!(data2_clear2$id %in% c(out_q0001)),]
addmargins(table(data2_clear3$Jahr,useNA="ifany"))


#================================================================================================
# 10) Clean and save data set                                                             
#================================================================================================

# final data set
# --------------
data_final <- data2_clear3


# save data set
# -------------
write.dta(data_final,paste(Path1,"data_water_structural_v09_cs.dta",sep=""),version=10)


#================================================================================================
date()
#===========================End of file==========================================================