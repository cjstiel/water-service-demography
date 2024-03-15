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
#						PREPARATION POPULATION DATA
#
#
#	CONTENT: Merges the population data to the utilities panel
#
#	OUTLINE: PART 1: Analyze key variable (municipality code) in both data sets
# 					1.1 Analyse municipality code in utilities data set
#					1.2 Analyze municipality code in population data set from Destatis
#			 PART 2: Prepare population data for merge
# 			 PART 3: Prepare utilities data for merge
#					3.1 Generate artificial year of observation '2002'
#					3.2 Harmonise municipality code 'ags_u_new'
# 					3.3 Harmonise municipality code variable 'EF7'
# 			 PART 4: Merge utilities panel and population data via municipality key and year
#					4.1 Merge via ags_u_n3
#					4.2 Merge via EF7
#					4.3 Merge both data sets
#					4.4 Create common municipality code for further analyses
#					4.5 Analyse 'leftovers'
#			 PART 5: Generate population variables for estimation
#					5.1 Population density (levels): popdens
#					5.2 Cumulative change in population density between first and last year of observation: pd_kum_l
# 					5.3 Annual changes in population density: pd_t1
#					5.4 Age structure
#					5.5 Topography
#			 PART 6: Correct for Census 2011 effect
#					6.1 Interpolate population density growth 'pd_t1' in 2011
# 					6.2 Interpolate population density 'popdens'
# 					6.3 Correct total population density growth 'pd_kum_l'
# 			 PART 7: Merge population data to subset of utilities data set
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 22-Feb-2018 (v15)
# ----------------------------------------------------------------------------------------------					 
#
# input: <original data set name>.dta,  data_water_structural_v09_cs.dta, 
# 			Gemeindedatensatz_FDZ_2000-2015_final.dta
# output: data_water_gemeinden_v05_cs.dta
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

# original data set from RDC
# --------------------------
orgdata <- read.dta13(file.path(Path2,"/<original data set name>.dta")) 


# utilities panel
# ---------------
data0 <- read.dta(file.path(Path1,"/data_water_structural_v09_cs.dta"))


# population data
# ---------------
gemeindedaten <- read.dta(file.path(Path1,"/Gemeindedatensatz_FDZ_2000-2015_final.dta"))


class(data)
dim(data)


#================================================================================================
# 							START
#================================================================================================


#================================================================================================
# 1) Analyze key variable (municipality code) in both data sets
#================================================================================================

#================================================================================================
# 1.1 Analyse municipality code in utilities data set
#================================================================================================

# Use original data set with all utilities (incl. energy utilities) to minimize holes in 
# population time series calculated in part 5.

dim(orgdata)

# Notes: We have two variables for the municipality code in the data set: 'ags' (from energy panel
# data set) and 'EF7' (from public firms data set).


# recode relevant variables as numeric
# ------------------------------------
if(class(orgdata$bnr) != "numeric") orgdata$bnr <- as.numeric(orgdata$bnr)
if(class(orgdata$unr) != "numeric") orgdata$unr <- as.numeric(orgdata$unr)
orgdata$ags_u_new <- ifelse(orgdata$ags_u=="." | orgdata$ags_u=="",NA,as.numeric(orgdata$ags_u))
orgdata$EF7 <- ifelse(orgdata$EF7=="." | orgdata$EF7=="",NA,orgdata$EF7)
orgdata$EF7_n <- as.numeric(orgdata$EF7)


# count digits of numeric municipality code
# -----------------------------------------
 summary(as.factor(nchar(orgdata$ags_u_new[orgdata$Jahr>=2003])))


# aggregate municipality code at firm level
# ----------------------------------------
orgdata_U <- aggregate(cbind("ags_u_new"=orgdata$ags_u_new,"EF7"=orgdata$EF7_n,"bl_u"=orgdata$bl_u)
                        ,by=list("unr"=orgdata$unr,"Jahr"=orgdata$Jahr),mean,na.rm=TRUE)
dim(orgdata_U)


# how many firms have ambiguous municipality codes (=more than one code)?
# -----------------------------------------------------------------------
length(which(orgdata_U$ags_u_new!=round(orgdata_U$ags_u_new,0))==FALSE)
length(which(orgdata_U$EF7!=round(orgdata_U$EF7,0))==FALSE)


# drop all utilities with ambiguous municipality code (=not an integer)
# ---------------------------------------------------------------------
orgdata_U <- subset(orgdata_U
                    , ags_u_new==round(orgdata_U$ags_u_new,0)| EF7==round(orgdata_U$EF7,0) )
dim(orgdata_U)


# count digits again
# ------------------
summary(as.factor(nchar(orgdata_U$ags_u_new[orgdata_U$ags_u_new==round(orgdata_U$ags_u_new,0)])))



#================================================================================================
# 1.2 Analyze municipality code in population data set from Destatis
#================================================================================================

# How many missings are there in the municipality code variable? 
# ---------------------------------------------------------------
# use variable 'AGS_n' since variable 'AGS' is filled only for observations from debt statistics
class(gemeindedaten$AGS_n)

# How many digits has the municipality code?
# -------------------------------------------
summary(as.factor(nchar(gemeindedaten$AGS_n)))

# Are there any duplicates?
# -------------------------
nrow(unique.data.frame(cbind(gemeindedaten$AGS_n,gemeindedaten$Jahr)))==nrow(gemeindedaten)




#================================================================================================
# 2) Prepare population data for merge (harmonise municipality code)
#================================================================================================

# Recode municipality code to 8-digit variable.

# 1-digit: corresponds to federal states 1-9 or whole Germany (0)
# ---------------------------------------------------------------
# fill with zeroes.

# 2- digits: Regierungsbezirke (administrative district) within federal states 1-9 or federal states 10-16
# --------------------------------------------------------------------------------------------------------
# fill with zeroes.

# 3-dgirts: Regierungsbezirke (administrative district) within federal states 10-16
# -------------------------------------------------------------------------------
# fill with zeroes.

# 4-digits: Kreise (counties) within federal states 1-9
# -----------------------------------------------------
# fill with zeroes.

# 5-digits: Kreise (counties) within federal states 10-16
# -------------------------------------------------------
# fill with zeroes.

# 6-digits: Verbandsgemeinde in federal state 7
# --------------------------------------------
# Insert '50' (code for 'Verbandsgemeinde') after digit 4 from Verbandsschluessel.

# 7-digits: municipalities in federal states 1-9
# ----------------------------------------------
# remain as such.

# 8-digits: municipalities in federal states 10-16
# ------------------------------------------------
# remain as such.

# digits: municipalities in Rheinland-Pfalz
# ----------------------------------------
# Contains 'Verbandsziffern' at digit position 5 and 6. Delete.


# 10-digitis: municipalities in Niedersachsen
# --------------------------------------
# Contains 'Verbandsziffern' at digit positions 5 to 7. Delete.


# Fill the codes for 'Kreise' with zeroes to obtain 7/8-digit code.
gemeindedaten$AGS_n2 <- ifelse(nchar(gemeindedaten$AGS_n)==4 | nchar(gemeindedaten$AGS_n)==5
                               ,paste(gemeindedaten$AGS_n,"000",sep="")
                               # Address Regierungsbezirke in BL 1-9.
                               ,ifelse(nchar(gemeindedaten$AGS_n)==2 & gemeindedaten$AGS_n>17
                                       ,paste(gemeindedaten$AGS_n,"00000",sep="")
                                       # Address ferdeal states 10-16
                                       ,ifelse(nchar(gemeindedaten$AGS_n)==2 & gemeindedaten$AGS_n>=10 & gemeindedaten$AGS_n<=16
                                               ,paste(gemeindedaten$AGS_n,"000000",sep="")
                                               # Address Verbandsgemeinden in BL 7.
                                               ,ifelse(nchar(gemeindedaten$AGS_n)==6
                                                       ,as.numeric(paste(substr(gemeindedaten$AGS_n,1,4),"50",substr(gemeindedaten$AGS_n,5,6),sep=""))
                                                       # Address Regierungsbezirke in BL 10-16.
                                                       ,ifelse(nchar(gemeindedaten$AGS_n)==3
                                                               ,paste(gemeindedaten$AGS_n,"00000",sep="")
                                                               # Address BL 1-9
                                                               ,ifelse(nchar(gemeindedaten$AGS_n)==1
                                                                       ,paste(gemeindedaten$AGS_n,"000000",sep="")
                                                                       # Delete Verbandsziffern from municipality codes in Rheinland-Pfalz.
                                                                       ,ifelse(nchar(gemeindedaten$AGS_n)==9
                                                                               ,as.numeric(paste(substr(gemeindedaten$AGS_n,1,4),substr(gemeindedaten$AGS_n,7,9),sep=""))
                                                                               # Delete Verbandsziffern from municipality codes in Niedersachsen
                                                                               ,ifelse(nchar(gemeindedaten$AGS_n)==10
                                                                                       ,as.numeric(paste(substr(gemeindedaten$AGS_n,1,4),substr(gemeindedaten$AGS_n,8,10),sep=""))
                                                                                       ,gemeindedaten$AGS_n))))))))

summary(as.factor(nchar(gemeindedaten$AGS_n2)))

# Is the variable AGS_n2 unambiguous?
# -----------------------------------
nrow(gemeindedaten)-nrow(unique.data.frame(cbind(gemeindedaten$AGS_n2,gemeindedaten$Jahr)))
# No. There are 428 duplicates.

# It is often the case that the population data is in one row and and the water tariffs
# in another (duplicated) row. Deleting the duplicates would inevitably result in the loss 
# of one of the information. 
# Instead, aggregate the information within the duplicate observations so that population
# and water rates from one year are combined with NAs removed.
date()
gem <- aggregate(cbind("Bevoelk_insg"=gemeindedaten$Bevoelk_insg
                       ,"Bevoelk_U18"=gemeindedaten$Bevoelk_U18
                       ,"Bevoelk_18_60"=gemeindedaten$Bevoelk_18_60
                       ,"Bevoelk_60plus"=gemeindedaten$Bevoelk_60plus
                       ,"Flaeche_qkm"=gemeindedaten$Flaeche_qkm
                       ,"Einnahmen"=gemeindedaten$Einnahmen
                       ,"Ausgaben"=gemeindedaten$Ausgaben
                       ,"Schulden"=gemeindedaten$Schulden
                       ,"wat_Arbeitsentgelt_EUR_m3"=gemeindedaten$wat_Arbeitsentgelt_EUR_m3
                       ,"wat_Grundentgelt_EUR"=gemeindedaten$wat_Grundentgelt_EUR
                       ,"LF_ha"=gemeindedaten$LF_ha
                       ,"Acker_ha"=gemeindedaten$Acker_ha
                       ,"Siedl_ha"=gemeindedaten$Siedl_ha
                       ,"LW_ha"=gemeindedaten$LW_ha
                       ,"Wald_ha"=gemeindedaten$Wald_ha
                       ,"Wass_ha"=gemeindedaten$Wass_ha
                       ,"hoehe_m"=gemeindedaten$hoehe_m)
                 ,by=list("AGS_n2"=gemeindedaten$AGS_n2,"Jahr"=gemeindedaten$Jahr),mean,na.rm=TRUE)
date()
# Check whether aggregation (only) deleted the duplicated rows.
nrow(gemeindedaten)-nrow(gem)


# Generate a dummy identifier to faciliate identification of obs from this data set.
# ----------------------------------------------------------------------------------
gem$gd <- 1




#================================================================================================
# 3) Prepare original utilities data for merge (harmonise municipality code)
#================================================================================================

#================================================================================================
# 3.1 Generate artificial year of observation '2002' in utilities panel data set
#================================================================================================

# Note: Generate year of observation '2002' to enable calculation of annual population growth
# for utilities observed in 2003.


# Generate year '2002' for all firm IDs
# -------------------------------------
# Damit pd_t1 auch für das Jahr 2003 berechnet werden kann.
iddat = expand.grid(unr=unique(orgdata_U$unr[orgdata_U$Jahr==2003]),Jahr=2002)
iddat <- iddat[order(iddat$unr, iddat$Jahr),]
orgdata_U <- merge(orgdata_U, iddat, all.x=TRUE, all.y=TRUE, by=c("unr", "Jahr"))


# Copy info on municipality key etc. from year 2003.
# --------------------------------------------------
date()
for (i in levels(as.factor(orgdata_U$unr)))
{
  orgdata_U$ags_u_new[orgdata_U$unr==i & orgdata_U$Jahr==2002] <- orgdata_U$ags_u_new[orgdata_U$unr==i & orgdata_U$Jahr==2003]
  orgdata_U$EF7[orgdata_U$unr==i & orgdata_U$Jahr==2002] <- orgdata_U$EF7[orgdata_U$unr==i & orgdata_U$Jahr==2003]
  orgdata_U$bl_u[orgdata_U$unr==i & orgdata_U$Jahr==2002] <- orgdata_U$bl_u[orgdata_U$unr==i & orgdata_U$Jahr==2003]
}
date()
table(orgdata_U$Jahr,useNA="ifany")



#================================================================================================
# 3.2 Harmonise municipality code 'ags_u_new'
#================================================================================================


# Fill the 4 and 5-digit codes with zeroes to obtain 7/8 digit code.
orgdata_U$ags_u_n2 <- ifelse(nchar(orgdata_U$ags_u_new)==4 | nchar(orgdata_U$ags_u_new)==5
                                ,paste(orgdata_U$ags_u_new,"000",sep="")
                                # Address Regierungsbezirke in BL 1-9.
                                ,ifelse(nchar(orgdata_U$ags_u_new)==2
                                        ,paste(orgdata_U$ags_u_new,"00000",sep="")
                                        # Address Verbandsgemeinden in BL 7.
                                        ,ifelse(nchar(orgdata_U$ags_u_new)==6
                                                ,as.numeric(paste(substr(orgdata_U$ags_u_new,1,4),"50",substr(orgdata_U$ags_u_new,5,6),sep=""))
                                                # Address Regierungsbezirke in BL 10-16.
                                                ,ifelse(nchar(orgdata_U$ags_u_new)==3
                                                        ,paste(orgdata_U$ags_u_new,"00000",sep="")
                                                        # Address BL 1-9
                                                        ,ifelse(nchar(orgdata_U$ags_u_new)==1
                                                                ,paste(orgdata_U$ags_u_new,"000000",sep="")
                                                                ,orgdata_U$ags_u_new)))))

# Note: Due to fractional numbers there can be more than 8 digits. Subsetting does not work here
# because ags_u_n2 is a character and is rounded when converted to as.numeric.
table(as.factor(nchar(orgdata_U$ags_u_n2)),orgdata_U$Jahr,useNA="ifany")


# Recode territorial changes (Gebietsstandänderungen) by hand
# -----------------------------------------------------------
orgdata_U$ags_u_n3 <- orgdata_U$ags_u_n2
# Liebenwalde 2003
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==12065192] <- 12065193
# Torgau 2008-2015
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==14389320 & orgdata_U$Jahr>2007] <- 14730310
# Erxleben bei Haldensleben 2003-2006
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==15083205 & orgdata_U$Jahr<2007] <- 15362034
# Blankenburg im Harz 2003-2006
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==15085055 & orgdata_U$Jahr<2007] <- 15369005
# Freital in Sachsen 2003-2006
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==14628110 & orgdata_U$Jahr<2007] <- 14290130
# Greifswald 2003-2010
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==13075039 & orgdata_U$Jahr<2011] <- 13001000
# Aachen 2008-2015
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==5313000 & orgdata_U$Jahr>=2008] <- 5334000
# Görlitz
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==14263000] <- 14626110
# Radeberg ab 2008
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==14292445 & orgdata_U$Jahr>=2008] <- 14625480
# Leuna ab 2008
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==15261033 & orgdata_U$Jahr>=2008] <- 15088205
# Burg auf Fehmarn
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==1055008] <- 1055046
# Burgwedel
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==3253004] <- 3241004
# Burgdorf
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==3253003] <- 3241003
# Hannover
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==3253001] <- 3241001
# Wyhratal --> Borna
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==14379740] <- 14379100
# Gemeindenr. 08325046 probably typo for Oberndorf am Neckar
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==8325046] <- 8325045
# Berlin-West becomes Berlin
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==11100000] <- 11000000
# Berlin-Ost becomes Berlin
orgdata_U$ags_u_n3[orgdata_U$ags_u_n3==11200000] <- 11000000




#================================================================================================
# 3.3 Harmonise municipality code variable 'EF7'
#================================================================================================

# Variable EF7 is coded as regional key from 2012 on (11,12-digits)
# -----------------------------------------------------------------
orgdata_U$EF7[is.nan(orgdata_U$EF7)==TRUE] <- NA
addmargins(table(as.factor(nchar(orgdata_U$EF7)),orgdata_U$Jahr,useNA="ifany"))

# Recode to standard 7/8 digit version
# ------------------------------------
orgdata_U$EF7_2 <- ifelse(nchar(orgdata_U$EF7)==11
                           # Delete Verbandsschlüssel in BL 1-9
                           ,paste(substr(orgdata_U$EF7,1,4),substr(orgdata_U$EF7,9,11),sep="")
                           # Delete Verbandsschlüssel in BL 10-16
                           ,ifelse(nchar(orgdata_U$EF7)==12
                                   ,paste(substr(orgdata_U$EF7,1,5),substr(orgdata_U$EF7,10,12),sep="")
                                   ,orgdata_U$EF7))

# Check new coding of EF7
# -----------------------
addmargins(table(as.factor(nchar(orgdata_U$EF7_2)),orgdata_U$Jahr,useNA="ifany"))




#================================================================================================
# 4) Merge utilities panel and population data via municipality key and year
#================================================================================================

#================================================================================================
# 4.1 Merge via ags_u_n3
#================================================================================================


# how many matches?
# -----------------
data_g <- merge(orgdata_U,gem,by.x=c("ags_u_n3","Jahr"),by.y=c("AGS_n2","Jahr"),all.x=TRUE)
addmargins(table(data_g$Jahr[is.na(data_g$gd)==FALSE],useNA="ifany"))
dim(data_g)

# in how many cases did match fail? --> Try via EF7
# -------------------------------------------------
addmargins(table(data_g$Jahr[is.na(data_g$gd)==TRUE],useNA="ifany"))

# generate indicator for merge via 'ags_u_n3'
# ------------------------------------------
data_g$gd2[is.na(data_g$gd)==FALSE] <- 1 



#================================================================================================
# 4.2 Merge via EF7
#================================================================================================


# Merge all remaining observations via EF7
# ----------------------------------------
data_g2 <- merge(subset(data_g,is.na(data_g$gd)==TRUE, select=-c(Bevoelk_insg,Bevoelk_U18
                                                                 ,Bevoelk_18_60,Bevoelk_60plus
                                                                 ,Flaeche_qkm
                                                                 ,Einnahmen,Ausgaben,Schulden
                                                                 ,wat_Arbeitsentgelt_EUR_m3
                                                                 ,wat_Grundentgelt_EUR,LF_ha
                                                                 ,Acker_ha,Siedl_ha,LW_ha
                                                                 ,Wald_ha,Wass_ha,hoehe_m,gd))
                 ,gem,by.x=c("EF7_2","Jahr"),by.y=c("AGS_n2","Jahr"),all=FALSE)

# how many matches?
# -----------------
addmargins(table(data_g2$Jahr,useNA="ifany"))

# generate indicator for merge via 'EF7_2'
# ----------------------------------------
data_g2$gd3 <- 1 



#================================================================================================
# 4.3 Merge both data sets
#================================================================================================

# keep only population data in second data set (EF7)
# --------------------------------------------------
data_g2_success <- subset(data_g2,select=c(unr,Jahr,Bevoelk_insg,Bevoelk_U18,Bevoelk_18_60
                                           ,Bevoelk_60plus,Flaeche_qkm,gd,Einnahmen,Ausgaben
                                           ,Schulden,wat_Arbeitsentgelt_EUR_m3
                                           ,wat_Grundentgelt_EUR,LF_ha,Acker_ha,Siedl_ha,LW_ha
                                           ,Wald_ha,Wass_ha,hoehe_m,gd3))

dim(data_g2_success)

# merge second data set (EF7) to first data set (ags_u_n3)
# --------------------------------------------------------
data_g3 <- merge(data_g,data_g2_success,by=c("unr","Jahr"),all.x=TRUE)
dim(data_g3)


# replace all obs in first data set (ags_u_n3) where match with population data failed with info from second data set (EF7)
# -----------------------------------------------------------------------------
data_g3$Bevoelk_insg.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Bevoelk_insg.y[is.na(data_g3$gd3)==FALSE]
data_g3$Bevoelk_U18.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Bevoelk_U18.y[is.na(data_g3$gd3)==FALSE]
data_g3$Bevoelk_18_60.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Bevoelk_18_60.y[is.na(data_g3$gd3)==FALSE]
data_g3$Bevoelk_60plus.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Bevoelk_60plus.y[is.na(data_g3$gd3)==FALSE]
data_g3$Flaeche_qkm.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Flaeche_qkm.y[is.na(data_g3$gd3)==FALSE]
data_g3$Einnahmen.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Einnahmen.y[is.na(data_g3$gd3)==FALSE]
data_g3$Ausgaben.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Ausgaben.y[is.na(data_g3$gd3)==FALSE]
data_g3$Schulden.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Schulden.y[is.na(data_g3$gd3)==FALSE]
data_g3$wat_Arbeitsentgelt_EUR_m3.x[is.na(data_g3$gd3)==FALSE] <- data_g3$wat_Arbeitsentgelt_EUR_m3.y[is.na(data_g3$gd3)==FALSE]
data_g3$wat_Grundentgelt_EUR.x[is.na(data_g3$gd3)==FALSE] <- data_g3$wat_Grundentgelt_EUR.y[is.na(data_g3$gd3)==FALSE]
data_g3$LW_ha.x[is.na(data_g3$gd3)==FALSE] <- data_g3$LW_ha.y[is.na(data_g3$gd3)==FALSE]
data_g3$LF_ha.x[is.na(data_g3$gd3)==FALSE] <- data_g3$LF_ha.y[is.na(data_g3$gd3)==FALSE]
data_g3$Acker_ha.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Acker_ha.y[is.na(data_g3$gd3)==FALSE]
data_g3$Siedl_ha.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Siedl_ha.y[is.na(data_g3$gd3)==FALSE]
data_g3$Wald_ha.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Wald_ha.y[is.na(data_g3$gd3)==FALSE]
data_g3$Wass_ha.x[is.na(data_g3$gd3)==FALSE] <- data_g3$Wass_ha.y[is.na(data_g3$gd3)==FALSE]
data_g3$hoehe_m.x[is.na(data_g3$gd3)==FALSE] <- data_g3$hoehe_m.y[is.na(data_g3$gd3)==FALSE]
data_g3$EF7_n.x[is.na(data_g3$gd3)==FALSE] <- data_g3$EF7_2.y[is.na(data_g3$gd3)==FALSE]
data_g3$gd.x[is.na(data_g3$gd3)==FALSE] <- data_g3$gd.y[is.na(data_g3$gd3)==FALSE]

# delete all variables .y
# ------------------------
# Note: use prefix "\\" to avoid interpretation as regular expression
data_g3 <- subset(data_g3, select=-c(grep("\\.y",colnames(data_g3))))
dim(data_g3)

# delete .x ending in variable names
# ----------------------------------
colnames(data_g3) <- sub(".x","",colnames(data_g3),fixed=TRUE)
dim(data_g3)





#================================================================================================
# 4.4 Create common municipality code for further analyses
#================================================================================================

# fill new municipality code 'AGS' with 'ags_u_n3' for matches via 'ags_u_n3' and with 'EF7_2'
# for matches via 'EF7_2'
# ---------------------------------------------
data_g3$AGS <- data_g3$ags_u_n3
data_g3$AGS[is.na(data_g3$gd3)==FALSE] <- data_g3$EF7_2[is.na(data_g3$gd3)==FALSE]

# drop obsolete municipality code variables
# -----------------------------------------
data_g3 <- data_g3[,!colnames(data_g3) %in% c("ags_u_n2","ags_u_n3","ags_u_new","EF7_2","EF7")]
dim(data_g3)




#================================================================================================
# 4.5 Analyse 'leftovers'
#================================================================================================


# Which utilities (municipality codes) could not be matched, i.e. no population data available?
# ----------------------------------------------------------------------------------------------
leftovers <- subset(data_g3, is.na(data_g3$gd)==TRUE)
uebersicht <- na.omit(cbind("Jahr"=leftovers$Jahr,"Gemeindeschluessel"=leftovers$AGS
                            ,"bl"=leftovers$bl_u))
# number of obs
nrow(uebersicht)




#================================================================================================
# 5) Generate population variables for estimation
#================================================================================================

# Notes: By tracking population density via unr (utilities panel) instead of AGS 
# (municipal data), changes in population density can be followed over time for a specific utility
# even if the local municipality code is modified (e.g., following territorial reforms).


#================================================================================================
# 5.1 Population density (levels): popdens
#================================================================================================

# calculate population density = inhabitants / area
# -------------------------------------------------
data_g3$popdens <- ifelse(is.na(data_g3$Bevoelk_insg)==FALSE & is.na(data_g3$Flaeche_qkm)==FALSE,
                          data_g3$Bevoelk_insg/data_g3$Flaeche_qkm,NA)
dstat(data_g3$popdens)


#================================================================================================
# 5.2 Cumulative change in population density between first and last year of observation: pd_kum_l
#================================================================================================

# formula: (t_end - t_0)/t_0

# base year: when is the utility first observed? 
# ---------------------------------------------
tab.base <- aggregate(cbind("Basisjahr"=data_g3$Jahr) ~ unr,data_g3,min)
data_g3 <- merge(data_g3,tab.base)
dim(data_g3)

# last year: when is the utility observed for the last time in the data set?
# ---------------------------------------------------------------------------
tab.last <- aggregate(cbind("Austrittsjahr.ew"=data_g3$Jahr) ~ unr,data_g3,max)
data_g3 <- merge(data_g3,tab.last)
dim(data_g3)



# for each utility, keep population density (popdens) in final year
# -------------------------------------------------------------------
data_aux1 <- subset(data_g3,Austrittsjahr.ew==Jahr)
data_aux1 <- subset(data_aux1,select=c(unr,Jahr,popdens))
data_aux1 <- rename(data_aux1,c(popdens="popdens_end",Jahr="Austrittsjahr.ew"))

# for each utility, keep population density (popdens) in first year
# ----------------------------------------------------------------
data_aux2 <- subset(data_g3,Basisjahr==Jahr)
data_aux2 <- subset(data_aux2,select=c(unr,Jahr,popdens))
data_aux2 <- rename(data_aux2,c(popdens="popdens_start",Jahr="Basisjahr"))

# merge both data sets
# --------------------
data_aux3 <- merge(data_aux1,data_aux2,by=c("unr"),all=FALSE)

# calculate population density growth between first and last year
# ---------------------------------------------------------------
data_aux3$pd_kum_l <- (data_aux3$popdens_end/data_aux3$popdens_start)-1
data_aux3 <- subset(data_aux3, select=c(unr,pd_kum_l))

# merge information to main data set
# ----------------------------------
data_g4 <- merge(data_g3,data_aux3,by=c("unr"),all.x=TRUE)

# recode Inf
data_g4$pd_kum_l[data_g4$pd_kum_l==Inf] <- NA

# summary statistics
# ------------------
dstat(data_g4$pd_kum_l*100,d=1)

# clean
# -----
rm(list=ls(pattern="data_aux"))




#================================================================================================
# 5.3 Annual changes in population density. pd_t1
#================================================================================================

# formula: (t - t-1)/ t-1

# calculate annual population density growth within each municipality
# -------------------------------------------------------------------
date()
data_g5$pd_t1 <- NA
for (i in levels(as.factor(data_g5$unr))){
  for (j in levels(as.factor(data_g5$Jahr))){
    if (as.factor(as.numeric(j)-1) %in% (data_g5$Jahr[data_g5$unr==i]))
      data_g5$pd_t1[data_g5$unr==i & data_g5$Jahr==j] <- (data_g5$popdens[data_g5$unr==i & data_g5$Jahr==j]
                                                                               /data_g5$popdens[data_g5$unr==i &data_g5$Jahr==as.numeric(j)-1])-1
  }
  #i2 <- i2 + 1
  #cat("\r",round(i2*100/length(levels(as.factor(data_g5$unr))),2)," % done ")
  #flush.console()
}
date()


# recode Inf
# ----------
data_g5$pd_t1[data_g5$pd_t1==Inf] <- NA

# summary statistics
# ------------------
dstat(data_g5$pd_t1*100,d=1)




#================================================================================================
# 5.4 Age structure
#================================================================================================

# share persons younger than 18 in total population
# --------------------------------------------------
data_g6$s_young <- data_g6$Bevoelk_U18/data_g6$Bevoelk_insg
data_g6$s_young[data_g6$s_young==Inf] <- NA

# share persons above 60 years in total population
# -------------------------------------------------
data_g6$s_old <- data_g6$Bevoelk_60plus/data_g6$Bevoelk_insg
data_g6$s_old[data_g6$s_old==Inf] <- NA


# annual growth in share young persons
# ------------------------------------
date()
data_g6$g_young <- NA
for (i in levels(as.factor(data_g6$unr))){
  for (j in levels(as.factor(data_g6$Jahr))){
    if (as.factor(as.numeric(j)-1) %in% (data_g6$Jahr[data_g6$unr==i]))
      data_g6$g_young[data_g6$unr==i & data_g6$Jahr==j] <- (data_g6$s_young[data_g6$unr==i & data_g6$Jahr==j]
                                                          -data_g6$s_young[data_g6$unr==i &data_g6$Jahr==as.numeric(j)-1])
 }
}
date()

# annual growth in share old persons
# ----------------------------------
data_g6$g_old <- NA
for (i in levels(as.factor(data_g6$unr))){
  for (j in levels(as.factor(data_g6$Jahr))){
    if (as.factor(as.numeric(j)-1) %in% (data_g6$Jahr[data_g6$unr==i]))
    data_g6$g_old[data_g6$unr==i & data_g6$Jahr==j] <- (data_g6$s_old[data_g6$unr==i & data_g6$Jahr==j]
                                                        -data_g6$s_old[data_g6$unr==i &data_g6$Jahr==as.numeric(j)-1])
  }
}
date()




#================================================================================================
# 5.5 Topography
#================================================================================================


# calculate mean over all years as topographic data will be mostly time-invariant and this allows us to
# to use information from all years
# ---------------------------------
mean_gem <- aggregate(cbind("LF_ha_mean"=data_g6$LF_ha
                            ,"LW_ha_mean"=data_g6$LW_ha
                            ,"Siedl_ha_mean"=data_g6$Siedl_ha
                            ,"Acker_ha_mean"=data_g6$Acker_ha
                            ,"Wald_ha_mean"=data_g6$Wald_ha
                            ,"Wass_ha_mean"=data_g6$Wass_ha
                            ,"hoehe_m_mean"=data_g6$hoehe_m)
                      ,by=list("unr"=data_g6$unr),mean,na.rm=TRUE)
# summary statistics
dstat(mean_gem[,c(2:8)])

# merge new mean-variables to main data set
# -----------------------------------------
data_g6 <- merge(data_g6,mean_gem,by="unr",all.x=TRUE)



# harmonise variable on area for agriculture
# ------------------------------------------
data_g6$LFW_ha_mean <- ifelse(is.na(data_g6$LF_ha_mean)==FALSE,data_g6$LF_ha_mean,data_g6$LW_ha_mean)
dstat(data_g6$LFW_ha_mean)



# generate topogrqphic variables (share in total municipal area)
# --------------------------------------------------------------

# share residential area (incl. infrastructure) in total municipal area
data_g6$shareSV <- ifelse(is.na(data_g6$Flaeche_qkm)==FALSE,data_g6$Siedl_ha_mean/(data_g6$Flaeche_qkm*100),NA)
dstat(data_g6$shareSV,d=2)

# share forest
data_g6$shareWald <- ifelse(is.na(data_g6$Flaeche_qkm)==FALSE,data_g6$Wald_ha_mean/(data_g6$Flaeche_qkm*100),NA)
dstat(data_g6$shareWald,d=2)

# share surface water
data_g6$shareWass <- ifelse(is.na(data_g6$Flaeche_qkm)==FALSE,data_g6$Wass_ha_mean/(data_g6$Flaeche_qkm*100),NA)
dstat(data_g6$shareWass,d=2)

# share cropland
data_g6$shareA <- ifelse(is.na(data_g6$Flaeche_qkm)==FALSE,data_g6$Acker_ha_mean/(data_g6$Flaeche_qkm*100),NA)
dstat(data_g6$shareA,d=2)

# share agriculture
data_g6$shareLW <- ifelse(is.na(data_g6$Flaeche_qkm)==FALSE,data_g6$LFW_ha_mean/(data_g6$Flaeche_qkm*100),NA)
dstat(data_g6$shareLW,d=2)




#================================================================================================
# 6) Correct for Census 2011 effect
#================================================================================================

# The Census in Germany in 2011 caused a structural break in population figures, implying a sudden
# increase in population numbers between 2010 and 2011.

# Assumption: Population numbers grew between 2010 and 2011 in the same rhythm as in the years
# directly before/after the structural break.

#================================================================================================
# 6.1 Interpolate population density growth 'pd_t1' in 2011
#================================================================================================

# Therefore, replace growth rates (pd_t1) in 2010/2011 by a linear interpolation (average) of the 
# rates 2009/2010 and 2011/2012. 

# First step: Generate the last year observed before 2011 (usually 2010) and the first year observed
# after 2011 (usually 2012).

# last year observed before 2011
# ------------------------------
data_aux1 <- subset(data_g6, Jahr<2011)
pre <- aggregate(cbind("Pre.Jahr"=data_aux1$Jahr),by=list("unr"=data_aux1$unr),max,na.rm=TRUE)
data_g7 <- merge(data_g6,pre, by=c("unr"),all.x=TRUE)

# first year observed after 2011
# ------------------------------
data_aux2 <- subset(data_g6, Jahr>2011)
past <- aggregate(cbind("Past.Jahr"=data_aux2$Jahr),by=list("unr"=data_aux2$unr),min,na.rm=TRUE)
data_g8 <- merge(data_g7,past, by=c("unr"),all.x=TRUE)


# add to each observation the growth rate from the relevant Pre.year
# ------------------------------------------------------------------
data_aux4 <- subset(data_g8,select=c(pd_t1,unr,Jahr))
data_aux4 <- rename(data_aux4,c(pd_t1="pd_t1_pre"))
data_aux5 <- merge(data_g8,data_aux4,by.x=c("unr","Pre.Jahr"),by.y=c("unr","Jahr"),all.x=TRUE)

# add to each observation the growth rate from the relevant Past.year
# ------------------------------------------------------------------
data_aux6 <- subset(data_g8,select=c(pd_t1,unr,Jahr))
data_aux6 <- rename(data_aux6,c(pd_t1="pd_t1_past"))
data_aux7 <- merge(data_aux5,data_aux6,by.x=c("unr","Past.Jahr"),by.y=c("unr","Jahr"),all.x=TRUE)

# for each utility, define pd_t1 in 2011 as the average of both growth rates (Pre/Past)
# ------------------------------------------------------------------------------------
data_aux7$pd_t1_2011 <- (data_aux7$pd_t1_pre + data_aux7$pd_t1_past)/2

# merge the interpolated growth rate for 2011 to the main data set
# -----------------------------------------------------------------
data_aux8 <- subset(data_aux7,select=c(unr,Jahr,pd_t1_2011))
data_g9 <- merge(data_g8,data_aux8,by=c("unr","Jahr"),all.x=TRUE)

# replace the growth rate in 2011 by the interpolated value
# ---------------------------------------------------------
data_g9$pd_t1_corr <- ifelse(data_g9$Jahr==2011,data_g9$pd_t1_2011,data_g9$pd_t1)
dstat(data_g9$pd_t1_corr*100,d=1)



#================================================================================================
# 6.2 Interpolate population density 'popdens'
#================================================================================================

# Correcting the population density 'popdens' requires a recursive imputation of the population 
# figures prior to 2011 and only works for utilities that are continuously observed.
# Population density is then updated backwards from the Census year based on the annual percentage
#  change 'pd_t1' calculated with the 'old' values.

# generate new variable: popdens_corr = popdens for years >= 2011 and = NA for years < 2011
# ----------------------------------------------------------------------------
data_g9$popdens_corr <- ifelse(data_g9$Jahr>=2011,data_g9$popdens,NA)


# recursively fill popdens_corr since 2011 using the annual growth rate pd_t1
# ---------------------------------------------------------------------------
# Method: solve pd_t1 formula for popdens[t-1]: popdens[t-1]=popdens[t]/(pd[t]+1)

date()
for (i in seq(1,9)){
	# calculate new population density
	data_g9$popdenscorr_Vorjahr <- data_g9$popdens_corr/(data_g9$pd_t1_corr+1)
	# generate variable 'previous year' 
	data_g9$Vorjahr <- data_g9$Jahr-1
	# select relevant variables
	data_aux9 <- subset(data_g9,select=c(unr,Vorjahr,popdenscorr_Vorjahr))
	data_g9 <- subset(data_g9,select=-c(Vorjahr,popdenscorr_Vorjahr))
	# merge new population density via variable 'previous year' to main data set
	data_g9 <- merge(data_g9,data_aux9,by.x=c("unr","Jahr"),by.y=c("unr","Vorjahr"),all.x=TRUE)
	# replace NAs in popdens_corr with newly calculated value
	data_g9$popdens_corr[is.na(data_g9$popdens_corr)==TRUE] <- data_g9$popdenscorr_Vorjahr[is.na(data_g9$popdens_corr)==TRUE]
	data_g9 <- subset(data_g9,select=-c(popdenscorr_Vorjahr))
	# move foward and repeat procedure for years 2009, 2008, ... , 2002.
	i <- i+1
	}
date()

# summary statistics
# -------------------
dstat(data_g9$popdens_corr)

# success rate: In how many cases prior to 2011 were we able to update the population density (otherwise 'NA')?
# ------------------------------------------------------------------------------------------------------------
table(data_g9$Jahr[is.na(data_g9$popdens_corr)==FALSE])

# for comparison: number of observation with original population density variable 'popdens'
# -----------------------------------------------------------------------------------------
dstat(data_g9$popdens)
table(data_g9$Jahr[is.na(data_g9$popdens)==FALSE])

# Note: Utilities which are not continuously observed between 2003 and 2011 have 'holes' in popdens_corr,.
# Thus, the number of obs(popdens_corr) < number of obs(popdens).




#================================================================================================
# 6.3 Correct total population density growth 'pd_kum_l' (t_end-t_0)/t_0
#================================================================================================

# store for each utility 'popdens_corr' from the last year in which the utility is observed (t_end)
# -------------------------------------------------------------------------------------------------
data_aux10 <- subset(data_g9,Austrittsjahr.ew==Jahr)
data_aux10 <- subset(data_aux10,select=c(unr,Jahr,popdens_corr))
data_aux10 <- rename(data_aux10,c(popdens_corr="popdens_corr_end",Jahr="Austrittsjahr.ew"))

# store for each utility the 'popdens_corr' from the first year in which the utility is observed (t_0)
# ----------------------------------------------------------------------------------------------------
data_aux11 <- subset(data_g9,Basisjahr==Jahr)
data_aux11 <- subset(data_aux11,select=c(unr,Jahr,popdens_corr))
data_aux11 <- rename(data_aux11,c(popdens_corr="popdens_corr_start",Jahr="Basisjahr"))

# merge both temp data sets
# -------------------------
data_aux12 <- merge(data_aux10,data_aux11,by=c("unr"),all=FALSE)

# calculate the new total change in population density between t_end and t_0 based on popdens_corr
# -----------------------------------------------------------------------------------------------
data_aux12$pd_kum_l_corr <- (data_aux12$popdens_corr_end/data_aux12$popdens_corr_start)-1
data_aux12 <- subset(data_aux12, select=c(unr,pd_kum_l_corr))

# merge new total population density growth to main data set
# -----------------------------------------------------------
data_g10 <- merge(data_g,data_aux12,by=c("unr"),all.x=TRUE)

# summary statistics
# ------------------
dstat(data_g10$pd_kum_l_corr*100,d=1)

# success rate: In how many cases were we able to update total population density growth (otherwise 'NA')?
# ---------------------------------------------------------------------------------------------------------
table(data_g10$Jahr[is.na(data_g10$pd_kum_l_corr)==FALSE])

# for comparison: number of observation with original population density variable 'popdens'
# -----------------------------------------------------------------------------------------
dstat(data_g10$pd_kum_l*100,d=1)
table(data_g10$Jahr[is.na(data_g10$pd_kum_l)==FALSE])

# Note: Total population density growth could not be updated for all utilities which are not
# continuously observed between 2003 and 2011.
# Thus, the number of obs(pd_kum_l_corr) < number of obs(pd_kum_l).



#================================================================================================
# 7) Merge population and topography data to (subset of) utilities panel data set
#================================================================================================

# Select the relevant variables
# ------------------------------
data_g11 <- subset(data_g10, select=c(unr,Jahr,AGS,Bevoelk_insg, Flaeche_qkm, popdens, pd_t1
                                      , pd_kum_l, popdens_corr,pd_t1_corr
                                      ,pd_kum_l_corr,s_young,s_old,g_young
                                      ,g_old,wat_Arbeitsentgelt_EUR_m3,wat_Grundentgelt_EUR,LF_ha
                                      ,Acker_ha,Siedl_ha,LW_ha,Wald_ha,Wass_ha,hoehe_m
                                      ,LF_ha_mean,Acker_ha_mean,Siedl_ha_mean,LW_ha_mean
                                      ,Wald_ha_mean,Wass_ha_mean,hoehe_m_mean,LFW_ha_mean,shareSV
                                      ,shareWald,shareWass,shareA,shareLW))


# merge selected variables with utilities panel
# ---------------------------------------------
data_wg <- merge(data0,data_g11,by=c("unr","Jahr"),all.x=TRUE)


# summary statistics
# -----------------
dstat(data_wg$pd_t1*100,d=1)
dstat(data_wg$pd_t1_corr*100,d=1)
dstat(data_wg$pd_kum_l*100,d=1)
dstat(data_wg$pd_kum_l_corr*100,d=1)


# save data set
# -------------
write.dta(data_wg,paste(Path1,"data_water_gemeinden_v05_cs.dta",sep=""),version=10)


#=================================================================================================
date()
#========================================== End of file ==========================================