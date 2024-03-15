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
#				PRODUCTIVITY ESTIMATION
#
#	CONTENT: Estimates production function of water utilities.
#
#	OUTLINE: PART 1: Data Preparation
# 					1.1 Inputs and outputs
# 					1.2 Topography
# 					1.3 Population structure
#			 PART 2: Sample definition
# 			 PART 3: Plain OLS model
#			 PART 4: Structural production function estimation
# 					4.1 First-stage estimation (OLS)
#					4.2 Second stage estimation: preparing the lags
# 					4.3 Second-stage estimation: GMM optimisation
# 					4.4 Bootstrapping the SE
# 			 PART 5: Results
# 					5.1 Output elasticities
# 					5.2 Productivity levels (TFP)
# 					5.3 Productivity growth (law of motion)
# 					5.4 Robustness check: dynamic panel
#
#				
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 27-Mar-2018 (v108)
# ----------------------------------------------------------------------------------------------					 
#
# input: data_wa_gem_7w_v02_cs.dta
# output: data_wa_acf_v17_cs.dta
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
library(optimx)
library(boot)
library(car)
library(ggplot2)
library(gdata)
library(plyr)
library(stringr)
library(minqa)
library(numDeriv)
library(micEconCES)
library(lmtest)
library(MatchIt)



#================================================================================================
# 0.2 Load data
#================================================================================================


# load data set
# -------------
data <- read.dta(file.path(Path1,"/data_wa_gem_7w_v02_cs.dta"))


class(data)
dim(data)



#=================================================================================================
# 1) Data preparation                                                       
#=================================================================================================


#=================================================================================================
# 1.1 Inputs and outputs                                           
#=================================================================================================


# log inputs and output
# ---------------------
data$l <- log(data$bruttolohn)
data$k <- log(data$K_adj)
data$w <- log(data$wage)
data$wat1 <- log(data$UK_Code8601)
data$wat2 <- log(data$UK_Code8701)
data$f <- log(data$fremdeDL)
data$i <- log(data$intermediates)


# center inputs and outputs at the median
# ---------------------------------------
data$l_m <- log(data$bruttolohn)-log(median(data$bruttolohn,na.rm=TRUE))
data$k_m <- log(data$K_adj)-log(median(data$K_adj,na.rm=TRUE))
data$w_m <- log(data$wage)-log(median(data$wage,na.rm=TRUE))
data$wat1_m <- log(data$UK_Code8601)-log(median(data$UK_Code8601,na.rm=TRUE))
data$wat2_m <- log(data$UK_Code8701)-log(median(data$UK_Code8701,na.rm=TRUE))
data$f_m <- log(data$fremdeDL)-log(median(data$fremdeDL,na.rm=TRUE))
data$i_m <- log(data$intermediates)-log(median(data$intermediates,na.rm=TRUE))


#=================================================================================================
# 1.2 Topography                                               
#=================================================================================================

# replace missing river basins info based on federal states
# ---------------------------------------------------------
addmargins(table(data$WEG1_mean,useNA="ifany"))
data$WEG1_mean <- ifelse(is.na(data$WEG1_mean)==TRUE & is.na(data$FGE1_mean)==TRUE
                            & (data$bl==1 | data$bl==13),9
                            ,ifelse(is.na(data$WEG1_mean)==TRUE & is.na(data$FGE1_mean)==TRUE
                                    & (data$bl==2 | data$bl==11| data$bl==12
                                       | data$bl==14 | data$bl==15| data$bl==16),5
                                    ,ifelse(is.na(data$WEG1_mean)==TRUE & is.na(data$FGE1_mean)==TRUE
                                            & data$bl==3,4
                                            ,ifelse(is.na(data$WEG1_mean)==TRUE & is.na(data$FGE1_mean)==TRUE
                                                    & (data$bl==5 | data$bl==6 | data$bl==7
                                                       | data$bl==8| data$bl==10),2
                                                    ,ifelse(is.na(data$WEG1_mean)==TRUE & is.na(data$FGE1_mean)==TRUE
                                                            & data$bl==9,1
                                                            ,data$WEG1_mean)))))


addmargins(table(data$WEG1_mean,useNA="ifany"))


# generate fixed effects for river basins
# ---------------------------------------
data$WEG1_Donau <- ifelse((is.na(data$WEG1_mean)==FALSE & data$WEG1_mean==1) 
                           | (is.na(data$FGE1_mean)==FALSE & data$FGE1_mean==1000),1,0)
data$WEG1_Rhein <- ifelse((is.na(data$WEG1_mean)==FALSE & data$WEG1_mean==2)
                           | (is.na(data$FGE1_mean)==FALSE & data$FGE1_mean==2000),1,0)
data$WEG1_Ems <- ifelse((is.na(data$WEG1_mean)==FALSE & data$WEG1_mean==3)
                         | (is.na(data$FGE1_mean)==FALSE & data$FGE1_mean==3000),1,0)
data$WEG1_Weser <- ifelse((is.na(data$WEG1_mean)==FALSE & data$WEG1_mean==4)
                           | (is.na(data$FGE1_mean)==FALSE & data$FGE1_mean==4000),1,0)
data$WEG1_ElbOdCo <- ifelse((is.na(data$WEG1_mean)==FALSE & (data$WEG1_mean==5 | data$WEG1_mean==6 
                                                             | data$WEG1_mean==9))
                          | (is.na(data$FGE1_mean)==FALSE & (data$FGE1_mean==5000 
                                                             | data$FGE1_mean==6000
                                                             | data$FGE1_mean>=9000)),1,0)
data$WEG1_Oder <- ifelse((is.na(data$WEG1_mean)==FALSE & data$WEG1_mean==6)
                          | (is.na(data$FGE1_mean)==FALSE & data$FGE1_mean==6000),1,0)




# average of topographic variables for multi-plant utilities
# ----------------------------------------------------------

mean_top <- aggregate(cbind("shareSV_mean"=data$shareSV
                            ,"shareWald_mean"=data$shareWald
                            ,"shareWass_mean"=data$shareWass
                            ,"shareA_mean"=data$shareA
                            ,"shareLW_mean"=data$shareLW)
                      ,by=list("unr"=data$unr),mean,na.rm=TRUE)
# overview
dstat(mean_top[,c(2:6)])

# merge new mean-variables to existing dataset
data <- merge(data,mean_top,by="unr",all.x=TRUE)


# log height
# ----------
data$hm <- log(data$hoehe_m_mean)
data$pd[data$hm==-Inf] <- NA
data$pd[data$hm==Inf] <- NA
dstat(data$hm,d=2)



#=================================================================================================
# 1.3 Population structure                                              
#=================================================================================================

# log population density
# ----------------------
data$pd <- log(data$popdens_corr)
data$pd[data$pd==-Inf] <- NA
data$pd[data$pd==Inf] <- NA
dstat(data$pd,d=2)


# age structure
# -------------
# Shares
dstat(data$s_young,d=2)
dstat(data$s_old,d=2)
# Growth
dstat(data$g_young,d=2)
dstat(data$g_old,d=2)



#=================================================================================================
# 2) Sample definition                                                     
#=================================================================================================


# Keep only pure water utilities (no sewerage)
# -------------------------------------------
data <- subset(data, aw==0 & wa==1)
addmargins(table(data$Jahr,useNA="ifany"))


# Drop all observations with nonpositive (<=0) values in inputs or outputs
# ------------------------------------------------------------------------
# Note: Reason - logs. Discards all private utilities as we do not have info on capital stock of
# private firms (capital data stems from JAB).
data <- subset(data, K_adj>0 & bruttolohn>0 & vorleistung>0 & UK_Code8701>0)
dim(data)
addmargins(table(data$Jahr))


# Drop all utilities with missings in covariates
# ----------------------------------------------
addmargins(table(data$Jahr,useNA="ifany"))
data_w <- subset(data
                 ,is.na(data$shareWV)==FALSE
                 & is.na(data$shareWass_mean)==FALSE & data$shareWass_mean<=1
                 & is.na(data$shareWald_mean)==FALSE & data$shareWald_mean<=1
                 & is.na(data$shareLW_mean)==FALSE & data$shareLW_mean<=1
                 & is.na(data$hm)==FALSE
                 & is.na(data$pd)==FALSE
                 & is.na(data$pd_t1_corr)==FALSE & abs(data$pd_t1_corr)<=1
                 & is.na(data$w_m)==FALSE
                 & is.na(data$wat2_m)==FALSE
                 & is.na(data$g_old)==FALSE
                 & is.na(data$g_young)==FALSE
                 & is.na(data$s_old)==FALSE
                 & is.na(data$s_young)==FALSE
                 & is.na(data$f_m)==FALSE
                 & is.na(data$i_m)==FALSE
                 & BWS==0
                 )  

addmargins(table(data_w$Jahr,useNA="ifany"))


# Define output measurement
# ---------------------
# total water supplied (incl. bulk water supply) [median-corrected]
data_w$y_m <- data_w$wat2_m
# total water supplied (incl. bulk water supply) [m3]
data_w$y <- data_w$UK_Code8701*1000


# recode data as panel data: i = utility's id, t = year
# -----------------------------------------------------
data_p <- pdata.frame(data_w, index=c("id","Jahr"),row.names=FALSE)
pdim(data_p)


#=================================================================================================
# 3) Plain OLS model                                                
#=================================================================================================

# run plain OLS model to obtain starting values for GMM model

# ---------------------------------------------------
# Appendix, Table 11: Production function estimates
# ---------------------------------------------------

# production function model (plain OLS)
# ------------------------------------
first_stage_OLS_m2 <- lm(y_m ~
                           # production function inputs
                          l_m + f_m + k_m + i_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + I(0.5*i_m^2) + l_m:f_m + l_m:k_m + l_m:i_m + f_m:k_m + f_m:i_m + k_m:i_m
                         # control for input price bias in labour with wage
                         + w_m 
                         # control for raw water composition (reference: groundwater)
                         + shareQW_mean
                         # control for vertical integration
                         + shareFB_mean
                         # control for customer structure
                         + shareHH_mean + shareWV
                         # control for topography
                         + shareWald_mean + shareWass_mean + shareLW_mean + hm
                         # control for river basin (reference: Donau)
                         + WEG1_ElbOdCo + WEG1_Ems + WEG1_Weser + WEG1_Rhein
                         # population structure
                         + pd + s_young + s_old
                         ,data_p)
summary(first_stage_OLS_m2)



# save coefficients to use them as starting values in GMM
betas_basic_m <- as.vector(first_stage_OLS_m2$coefficients)
length(betas_basic_m)



#=================================================================================================
# 4) Structural production function estimation                                             
#=================================================================================================

#=================================================================================================
# 4.1 First-stage estimation (OLS)                         
#=================================================================================================

# First stage OLS estimation in ACF (2005). Eliminates error u_it.

# Model production function as a translog function:

# Output: total water supplied
# Inputs: labour (wage bill), external services, capital, intermediates

# Production function 
# -------------------
first_stage_m <- lm(y_m ~
                      # production function inputs
                          l_m + f_m + k_m + i_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + I(0.5*i_m^2) + l_m:f_m + l_m:k_m + l_m:i_m + f_m:k_m + f_m:i_m + k_m:i_m
                    # control for input price bias in labour with wage
                    + w_m 
                    # control for raw water composition (reference: groundwater)
                    + shareQW_mean
                    # control for vertical integration
                    + shareFB_mean
                    # control for customer structure
                    + shareHH_mean + shareWV
                    # control for topography
                    + shareWald_mean + shareWass_mean + shareLW_mean + hm
                    # control for river basin (reference: Donau)
                    + WEG1_ElbOdCo + WEG1_Ems + WEG1_Weser + WEG1_Rhein
                    # population structure
                    + pd + s_young + s_old
                    # proxy terms
                    + l_m:k_m:f_m:i_m 
                    + l_m:I(i_m^2) + l_m:I(k_m^2) + l_m:I(f_m^2) 
                    + f_m:I(k_m^2) + f_m:I(l_m^2) + f_m:I(i_m^2)
                    + k_m:I(f_m^2)  + k_m:I(l_m^2) + k_m:I(i_m^2) 
                    + i_m:I(l_m^2) + i_m:I(k_m^2) + i_m:I(f_m^2) 
                    + I(l_m^2):I(k_m^2) + I(l_m^2):I(i_m^2) + I(l_m^2):I(f_m^2)
                    + I(f_m^2):I(k_m^2) + I(f_m^2):I(i_m^2) + I(i_m^2):I(k_m^2)
                    + k_m:I(l_m^2):i_m + f_m:I(l_m^2):i_m + k_m:I(l_m^2):f_m
                    + l_m:I(k_m^2):i_m + l_m:I(k_m^2):f_m + f_m:I(k_m^2):i_m
                    + l_m:I(f_m^2):k_m + l_m:I(f_m^2):i_m + i_m:I(f_m^2):k_m 
                    + f_m:I(i_m^2):k_m + f_m:I(i_m^2):l_m  + l_m:I(i_m^2):k_m
                    + i_m:I(l_m^2):I(k_m^2) + i_m:I(l_m^2):I(f_m^2) + i_m:I(k_m^2):I(f_m^2)
                    + k_m:I(l_m^2):I(i_m^2) + k_m:I(l_m^2):I(f_m^2) + k_m:I(i_m^2):I(f_m^2)
                    + l_m:I(k_m^2):I(i_m^2) + l_m:I(k_m^2):I(f_m^2) + l_m:I(i_m^2):I(f_m^2)
                    + f_m:I(l_m^2):I(k_m^2) + f_m:I(l_m^2):I(i_m^2) + f_m:I(k_m^2):I(i_m^2) 
                    + I(l_m^2):I(k_m^2):I(f_m^2)
                    + I(l_m^2):I(k_m^2):I(i_m^2)
                    + I(f_m^2):I(i_m^2):I(l_m^2)
                    + I(f_m^2):I(i_m^2):I(k_m^2)
                    ,data_p)
summary(first_stage_m)


# store coefficients from the first stage
# ---------------------------------------
betas1 <- as.vector(first_stage_m$coefficients)

# Predict Phi
# -----------
data_p$Phi <- first_stage_m$fitted.values
length(data_p$Phi)
length(which(is.na(data_p$Phi)==FALSE))
cor(data_p$Phi,data_p$y_m)

# Construct Lag-Phi
# -----------------
data_p$lag_Phi <- lag(data_p$Phi)
length(data_p$lag_Phi)
length(which(is.na(data_p$lag_Phi)==FALSE))

# Store residuals for later use
# -----------------------------
data_p$exp_u_it <- exp(first_stage_m$residuals)
 


#=================================================================================================
# 4.2  Second-stage estimation: Preparing the lags
#=================================================================================================

# Combine all inputs in a matrix (full first-stage sample)
# --------------------------------------------------------
# Note: The order must be identical to that of the OLS estimation's coefficients. 
Inputs <- as.matrix(cbind(rep(1,nrow(data_p)),data_p$l_m,data_p$f_m,data_p$k_m,data_p$i_m
                          ,0.5*(data_p$l_m)^2,0.5*(data_p$f_m)^2,0.5*(data_p$k_m)^2
                          ,0.5*(data_p$i_m)^2,data_p$w_m
                          ,data_p$shareQW_mean,data_p$shareFB_mean,data_p$shareHH_mean
                          ,data_p$shareWV,data_p$shareWald_mean,data_p$shareWass_mean
                          ,data_p$shareLW_mean,data_p$hm
                          ,data_p$WEG1_ElbOdCo,data_p$WEG1_Ems,data_p$WEG1_Weser,data_p$WEG1_Rhein
                          ,data_p$pd,data_p$s_young,data_p$s_old
                          ,data_p$l_m*data_p$f_m,data_p$l_m*data_p$i_m,data_p$l_m*data_p$k_m
                          ,data_p$f_m*data_p$k_m,data_p$f_m*data_p$i_m,data_p$i_m*data_p$k_m))
dim(Inputs)


# Drop all observations with lag(phi) == missing
# ----------------------------------------------
data_gmm <- subset(data_p,is.na(lag_Phi)==FALSE)
pdim(data_gmm)
n <- nrow(data_gmm)


# Generate input matrix based on second-stage sample (at time t)
# ---------------------------------------------------------------
Inputs_gmm_all <- as.matrix(cbind(rep(1,nrow(data_gmm)),data_gmm$l_m,data_gmm$f_m,data_gmm$k_m
                                  ,data_gmm$i_m,0.5*(data_gmm$l_m)^2,0.5*(data_gmm$f_m)^2
                                  ,0.5*(data_gmm$k_m)^2,0.5*(data_gmm$i_m)^2
                              ,data_gmm$w_m,data_gmm$shareQW_mean
                              ,data_gmm$shareFB_mean,data_gmm$shareHH_mean,data_gmm$shareWV
                              ,data_gmm$shareWald_mean,data_gmm$shareWass_mean,data_gmm$shareLW_mean
                              ,data_gmm$hm
                              ,data_gmm$WEG1_ElbOdCo,data_gmm$WEG1_Ems,data_gmm$WEG1_Weser
                              ,data_gmm$WEG1_Rhein
                              ,data_gmm$pd,data_gmm$s_young,data_gmm$s_old
                              ,data_gmm$l_m*data_gmm$f_m,data_gmm$l_m*data_gmm$i_m
                              ,data_gmm$l_m*data_gmm$k_m,data_gmm$f_m*data_gmm$k_m
                              ,data_gmm$f_m*data_gmm$i_m,data_gmm$i_m*data_gmm$k_m))
dim(Inputs_gmm_all)

# Generate input matrix based on second-stage sample (at time t-1)
# -----------------------------------------------------------------
lag_Inputs <- as.matrix(cbind(rep(1,nrow(data_p)),lag(data_p$l_m),lag(data_p$f_m)
                              ,lag(data_p$k_m),lag(data_p$i_m),0.5*(lag(data_p$l_m))^2
                              ,0.5*(lag(data_p$f_m))^2,0.5*(lag(data_p$k_m))^2
                              ,0.5*(lag(data_p$i_m))^2
                              ,lag(data_p$w_m)
                              ,lag(data_p$shareQW_mean)
                              ,lag(data_p$shareFB_mean),lag(data_p$shareHH_mean)
                              ,lag(data_p$shareWV),lag(data_p$shareWald_mean)
                              ,lag(data_p$shareWass_mean),lag(data_p$shareLW_mean)
                              ,lag(data_p$hm)
                              ,lag(data_p$WEG1_ElbOdCo),lag(data_p$WEG1_Ems)
                              ,lag(data_p$WEG1_Weser),lag(data_p$WEG1_Rhein)
                              ,lag(data_p$pd),lag(data_p$s_young),lag(data_p$s_old)
                              ,lag(data_p$l_m)*lag(data_p$f_m),lag(data_p$l_m)*lag(data_p$i_m)
                              ,lag(data_p$l_m)*lag(data_p$k_m),lag(data_p$f_m)*lag(data_p$k_m)
                              ,lag(data_p$f_m)*lag(data_p$i_m),lag(data_p$i_m)*lag(data_p$k_m)))
lag_Inputs_gmm_all <- na.omit(lag_Inputs)
dim(lag_Inputs_gmm_all)


# Choose inputs whose coefficients are updated in 2nd stage (at time t)
# ---------------------------------------------------------------------
Inputs_gmm <- Inputs_gmm_all
dim(Inputs_gmm)

# Choose inputs whose coefficients are not updated in the 2nd stage (at time t)
# -----------------------------------------------------------------------------
Inputs_fixed <- as.matrix(rep(0,nrow(data_gmm)))
dim(Inputs_fixed)

# Choose inputs whose coefficients are updated in 2nd stage (at time t-1)
# -----------------------------------------------------------------------
lag_Inputs_gmm <- lag_Inputs_gmm_all
dim(lag_Inputs_gmm)

# Choose inputs whose coefficients are not updated in the 2nd stage (at time t-1)
# -------------------------------------------------------------------------------
lag_Inputs_fixed <- as.matrix(rep(0,nrow(data_gmm)))
dim(lag_Inputs_fixed)


# Choose instruments for moment function
# --------------------------------------
instr <- cbind(rep(1,nrow(data_p)),data_p$l_m,lag(data_p$f_m),data_p$k_m,lag(data_p$i_m)
               ,data_p$l_m^2,(lag(data_p$f_m))^2,data_p$k_m^2,(lag(data_p$i_m))^2,data_p$w_m
               ,data_p$shareQW_mean,data_p$shareFB_mean,data_p$shareHH_mean
               ,data_p$shareWV,data_p$shareWald_mean,data_p$shareWass_mean
               ,data_p$shareLW_mean,data_p$hm
               ,data_p$WEG1_ElbOdCo,data_p$WEG1_Ems,data_p$WEG1_Weser,data_p$WEG1_Rhein
               ,data_p$pd,data_p$s_young,data_p$s_old
               ,data_p$l_m*lag(data_p$f_m),data_p$l_m*lag(data_p$i_m),data_p$l_m*data_p$k_m
               ,lag(data_p$f_m)*data_p$k_m,lag(data_p$i_m)*lag(data_p$f_m)
               ,lag(data_p$i_m)*data_p$k_m)
instr_gmm <- na.omit(instr)
dim(instr_gmm)
                   

 
#=================================================================================================
# 4.3  Second-stage estimation: GMM optimisation     
#=================================================================================================


# The GMM's objective function is the moment condition E[(Z'v)'*(Z'v)]=0.

# Choose starting values
# ----------------------
initial_betas <- betas_basic_m
betas_fixed <- 0

# Run GMM optimisation routine
# ----------------------------
optimisation <- optimx(par=initial_betas,fn=gmm_moment_condition, method=c("nlminb","nlm"))
print(optimisation)


# Choose best optimisation routine (lowest value for objective function)
# ----------------------------------------------------------------------
j <- which.min(optimisation$value)

# Store coefficients from the second stage (betas)
# ------------------------------------------------
betas_acf <- rbind(optimisation$p1[j],optimisation$p2[j],optimisation$p3[j],optimisation$p4[j]
                    ,optimisation$p5[j],optimisation$p6[j],optimisation$p7[j],optimisation$p8[j]
                    ,optimisation$p9[j],optimisation$p10[j],optimisation$p11[j],optimisation$p12[j]
                   ,optimisation$p13[j],optimisation$p14[j],optimisation$p15[j],optimisation$p16[j]
                   ,optimisation$p17[j],optimisation$p18[j],optimisation$p19[j],optimisation$p20[j]
                   ,optimisation$p21[j],optimisation$p22[j],optimisation$p23[j],optimisation$p24[j]
                   ,optimisation$p25[j],optimisation$p26[j],optimisation$p27[j],optimisation$p28[j]
                   ,optimisation$p29[j],optimisation$p30[j],optimisation$p31[j])



#=================================================================================================
# 4.4 Bootstrapping the SE              
#=================================================================================================

# ---------------------------------------------------
# Table 7 and Appendix, Table 11
# ---------------------------------------------------

# In this step, we bootstrap the SE for the coefficients from the second stage.
# We use the function 'ClusterBootSE' (see beginning of script) that draws the B bootstrap 
# subsamples from the full data set and the function 'boot.acf' that summarises the ACF algorithm.

# The function 'clusterBootSE' requires 3 inputs:
# (1) data: The data set from which the B bootstrap samples are to be drawn. It should be the
#     main data set without panel structure as available at the end of section (1).
# (2) method: Choose the numerical approach for the optimisation routine in the ACF algorithm.
#     It should be identical to the one used in section (4.3).
# (3) B: Number of replications. 

# Boostrap the SE
# ---------------
date()
clusterBootSE(data=data_w,method=rownames(optimisation)[j],B=500)
date()

# Check: The first column should correspond to the coefficients 'beta22' from section (4.3). 

betas_final <- betas_acf


#=================================================================================================
# 5) Results			                               
#=================================================================================================

#=================================================================================================
# 5.1 Output elasticities
#=================================================================================================


# Output elasticity for labour
# ----------------------------
# elasticity_l = b_l + b_ll*l + b_lf*f + b_kl*k + b_li*i
data_p$elasticity_lohn <- (betas_final[2] + betas_final[6]*Inputs[,2] 
                           + betas_final[26]*Inputs[,3] + betas_final[27]*Inputs[,4]
                           + betas_final[28]*Inputs[,5])


# Output elasticity for external services
# ----------------------------------------
# elasticity_f = b_f + b_ff*f + b_lf*l + b_kf*k + b_fi*i
data_p$elasticity_fd <- (betas_final[3] + betas_final[7]*Inputs[,3] + betas_final[26]*Inputs[,2] 
                          + betas_final[29]*Inputs[,4] + betas_final[30]*Inputs[,5])



# Output elasticity for capital
# -----------------------------
# elasticity_k = b_k + b_kk*k + b_lk*l + b_kf*f + b_ki*i
data_p$elasticity_cap <- (betas_final[4] + betas_final[8]*Inputs[,4] + betas_final[27]*Inputs[,2] 
                          + betas_final[29]*Inputs[,3] + betas_final[31]*Inputs[,5])



# Output elasticity for intermediates
# -----------------------------------
# elasticity_v = b_i + b_ii*i + b_li*l + b_ki*k + b_fi*f
data_p$elasticity_vorl <- (betas_final[5] + betas_final[9]*Inputs[,5] + betas_final[28]*Inputs[,2] 
                           + betas_final[30]*Inputs[,3] + betas_final[31]*Inputs[,4])



# Returns to scale
data_p$rts <- data_p$elasticity_lohn + data_p$elasticity_vorl + data_p$elasticity_fd + data_p$elasticity_cap


# summary statistics output elasticities and returns to scale
# -----------------------------------------------------------
dstat(as.data.frame(data_p$elasticity_lohn),d=3)
dstat(as.data.frame(data_p$elasticity_vorl),d=3)
dstat(as.data.frame(data_p$elasticity_fd),d=3)
dstat(as.data.frame(data_p$elasticity_cap),d=3)
dstat(as.data.frame(data_p$rts),d=3)


#=================================================================================================
# 5.2 Productivity levels (TFP)
#=================================================================================================

#=================================================================================================
# 5.2.1 Calculate productivity (TFP)
#=================================================================================================

# productivity values (logs)
# --------------------------
data_p$omega2 <- data_p$Phi - Inputs%*%betas_final
data_gmm$omega2 <- data_gmm$Phi - Inputs_gmm_all%*%betas_final

# productivity values (levels)
# ----------------------------
data_p$omega2e <- exp(data_p$Phi - Inputs%*%betas_final)

# error term (for estimating marginal costs of production)
# --------------------------------------------------------
data_p$const_u_it <- exp(betas_acf[1])*data_p$exp_u_it


#=================================================================================================
# 5.2.2 Productivity dispersion
#=================================================================================================

# full sample
# -----------
dstat(data_p$omega2e,d=3)
dstat(exp(data_gmm$omega2),d=3)

# growing regions
# ---------------
data_p1 <- subset(data_p,pd_kum_l_corr>0)
dstat(data_p1$omega2e,d=3)


# shrinking regions
# -----------------
data_p2 <- subset(data_p,pd_kum_l_corr<=0)
dstat(data_p2$omega2e,d=3)


#=================================================================================================
# 5.3 Productivity growth (law of motion)
#=================================================================================================


# Calculate lag-omega (omega_t-1) for the LOM
# -------------------------------------------
data_gmm$lag_omega2 <- data_gmm$lag_Phi - lag_Inputs_gmm_all%*%betas_final
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("id","Jahr"),row.names=FALSE)



# ---------------------------------------------
# Table 8: Productivity and demographic changes
# ----------------------------------------------

# Productivity growth full sample
# -------------------------------
AR1_expost <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) 
                  + pd_t1_corr + g_young + g_old
                  ,data=data_gmm,model="pooling",effect="time")
summary(AR1_expost,vcov=vcovHC(AR1_expost))



# Productivity growth in growing regions
# --------------------------------------
data_in <- subset(data_gmm,pd_kum_l_corr>0)

AR1_expost2 <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3)
                   + pd_t1_corr + g_young + g_old
                  ,data=data_in,model="pooling",effect="time")
summary(AR1_expost2,vcov=vcovHC(AR1_expost2))


# Productivity growth in shrinking regions
# --------------------------------------
data_de <- subset(data_gmm,pd_kum_l_corr<=0)

AR1_expost3 <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3)
                   + pd_t1_corr + g_young + g_old
                   ,data=data_de,model="pooling",effect="time")
summary(AR1_expost3,vcov=vcovHC(AR1_expost3))



#=================================================================================================
# 5.4 Robustness check: dynamic panel
#=================================================================================================

# ------------------------------------------------------------
# Table 12, appendix: Demography and productivity (system GMM)
# ------------------------------------------------------------

# recode data as paneanel data
# -------------------------
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","Jahr"),row.names=FALSE)


# full sample
# -----------
dp1 <- pgmm(omega2 ~ lag(omega2,1) + pd_t1_corr + g_young + g_old | lag(omega2,2:99)
            ,effect="twoways",transformation="ld",data=data_p)
summary(dp1, robust=TRUE)


# growing regions
# ---------------
data_dp <- subset(data_p, is.na(data_p$pd_kum_l_corr)==FALSE)
data_dp <- pdata.frame(as.data.frame(data_dp),index=c("id","Jahr"),row.names=FALSE)

dp3 <- pgmm(omega2 ~ lag(omega2,1) + pd_t1_corr + g_young + g_old | lag(omega2,2:99)
            ,effect="twoways",transformation="ld",data=data_dp[data_dp$pd_kum_l_corr>0,])
summary(dp3, robust=TRUE)


# shrinking regions
# -----------------
dp4 <- pgmm(omega2 ~ lag(omega2,1) + pd_t1_corr + g_young + g_old | lag(omega2,2:99)
            ,effect="twoways",transformation="ld",data=data_dp[data_dp$pd_kum_l_corr<=0,])
summary(dp4, robust=TRUE)



#=================================================================================================
# 6) Save data set
#=================================================================================================

# save data set
# -------------
write.dta(data_p,paste(Path1,"data_wa_acf_v17_cs.dta",sep=""),version=10)


#=================================================================================================
date()
#========================================== End of file ==========================================