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
# Function 'gmm_moment_condition'                                                                    
# ----------------------------- 
# This function, describing the optimisation routine, builds the core of the ACF algorithm. It
# minimises the objective function (the moment conditions), while the equation from the Markov process
# and those calculating TFP are constraints in the optimisation process. 
#
# In total, there are 5 constraints:
#                                                           
# (1) omega_it = Phi_it - Inputs_it * betas                                                        
# (2) omega_it-1 = Phi_it-1 - Inputs_it-1 * betas                                                  
# (3) omega_pol = 1 + omega_it-1 + (omega_it-1)^2 + (omega_it-1)^3 + controls            
# (4) g_p = inv(omega_pol'*omega_pol)*omega_pol'*omega                                             
# (5) innovation_it = omega_it - omega_pol*g_p                                                     
#                                                                                                  
# gmm_moment condition: The innovation in productivity (v_it), which forms the moment
# condition together with the betas, is calculated in constraint (5). After that, the 
# objective function of the optimisation process, the sample moment condition, is 
# calculated in 'moment_condition'. The output is a scalar that the optimisation
# routine seeks to minimise.
#================================================================================================                                                          


gmm_moment_condition <- function(betas){
  omega <<- data_gmm$Phi - Inputs_gmm%*%betas -Inputs_fixed%*%betas_fixed
  lag_omega <<- data_gmm$lag_Phi-lag_Inputs_gmm%*%betas - lag_Inputs_fixed%*%betas_fixed
  omega_pol <<- cbind(rep(1,n),lag_omega,lag_omega^2,lag_omega^3,data_gmm$pd_t1_corr
                      ,data_gmm$g_young,data_gmm$g_old)
  AR1 <<- lm(omega ~ lag_omega+ I(lag_omega^2) + I(lag_omega^3)
             + data_gmm$pd_t1_corr + data_gmm$g_young + data_gmm$g_old)
  g_b <<- as.vector(AR1$coefficients)
  innovation <<- omega - omega_pol%*%g_b
  moment_condition <- t(t(instr_gmm)%*%innovation)%*%(t(instr_gmm)%*%innovation)
  return(as.vector(moment_condition))
}




#================================================================================================
# Function boot.acf										                                                             
# -----------------
# Part I of the bootstrapping procedure for estimating the standard errors of the production 
# function's parameters.										                                                                
# It contains the full ACF algorithm (first + second stage) and returns the coefficients of the
# second stage.    		   
#================================================================================================



boot.acf <- function(data,indices,method){
  data_boot <<- data[indices,]
  invisible(capture.output(data_bp <<- pdata.frame(data_boot, index=c("id","Jahr"))))
  first_stage_m <<- lm(y_m ~ l_m + f_m + k_m + i_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                       + I(0.5*i_m^2) + l_m:f_m + l_m:k_m + l_m:i_m + f_m:k_m + f_m:i_m + k_m:i_m 
                       + w_m + shareQW_mean + shareFB_mean
                       + shareHH_mean + shareWV + shareWald_mean + shareWass_mean + shareLW_mean 
                       + hm + WEG1_ElbOdCo + WEG1_Ems + WEG1_Weser + WEG1_Rhein
                       + pd + s_young + s_old                        + l_m:k_m:f_m:i_m 
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
                       ,data_bp)
  data_bp$Phi <<- first_stage_m$fitted.values
  data_bp$lag_Phi <<- lag(data_bp$Phi)
  data_bp$exp_u_it <<- exp(first_stage_m$residuals)
  Inputs <<- as.matrix(cbind(rep(1,nrow(data_bp)),data_bp$l_m,data_bp$f_m,data_bp$k_m,data_bp$i_m
                             ,0.5*(data_bp$l_m)^2,0.5*(data_bp$f_m)^2,0.5*(data_bp$k_m)^2
                             ,0.5*(data_bp$i_m)^2
                             ,data_bp$w_m,data_bp$shareQW_mean,data_bp$shareFB_mean
                             ,data_bp$shareHH_mean,data_bp$shareWV,data_bp$shareWald_mean
                             ,data_bp$shareWass_mean,data_bp$shareLW_mean,data_bp$hm
                             ,data_bp$WEG1_ElbOdCo,data_bp$WEG1_Ems
                             ,data_bp$WEG1_Weser,data_bp$WEG1_Rhein,data_bp$pd,data_bp$s_young
                             ,data_bp$s_old
                             ,data_bp$l_m*data_bp$f_m,data_bp$l_m*data_bp$i_m
                             ,data_bp$l_m*data_bp$k_m,data_bp$f_m*data_bp$k_m
                             ,data_bp$f_m*data_bp$i_m,data_bp$i_m*data_bp$k_m))
  data_gmm <<- subset(data_bp,is.na(lag_Phi)==FALSE)
  n <<- nrow(data_gmm)
  Inputs_gmm <<- as.matrix(cbind(rep(1,nrow(data_gmm)),data_gmm$l_m,data_gmm$f_m,data_gmm$k_m
                                 ,data_gmm$i_m,0.5*(data_gmm$l_m)^2,0.5*(data_gmm$f_m)^2
                                 ,0.5*(data_gmm$k_m)^2,0.5*(data_gmm$i_m)^2
                                 ,data_gmm$w_m,data_gmm$shareQW_mean,data_gmm$shareFB_mean
                                 ,data_gmm$shareHH_mean,data_gmm$shareWV,data_gmm$shareWald_mean
                                 ,data_gmm$shareWass_mean,data_gmm$shareLW_mean,data_gmm$hm
                                 ,data_gmm$WEG1_ElbOdCo,data_gmm$WEG1_Ems
                                 ,data_gmm$WEG1_Weser,data_gmm$WEG1_Rhein,data_gmm$pd
                                 ,data_gmm$s_young,data_gmm$s_old
                                 ,data_gmm$l_m*data_gmm$f_m,data_gmm$l_m*data_gmm$i_m
                                 ,data_gmm$l_m*data_gmm$k_m,data_gmm$f_m*data_gmm$k_m
                                 ,data_gmm$f_m*data_gmm$i_m,data_gmm$i_m*data_gmm$k_m))
  lag_Inputs <<- as.matrix(cbind(rep(1,nrow(data_bp)),lag(data_bp$l_m),lag(data_bp$f_m)
                                 ,lag(data_bp$k_m),lag(data_bp$i_m),0.5*(lag(data_bp$l_m))^2
                                 ,0.5*(lag(data_bp$f_m))^2,0.5*(lag(data_bp$k_m))^2
                                 ,0.5*(lag(data_bp$i_m))^2,lag(data_bp$w_m)
                                 ,lag(data_bp$shareQW_mean),lag(data_bp$shareFB_mean)
                                 ,lag(data_bp$shareHH_mean),lag(data_bp$shareWV)
                                 ,lag(data_bp$shareWald_mean),lag(data_bp$shareWass_mean)
                                 ,lag(data_bp$shareLW_mean),lag(data_bp$hm)
                                 ,lag(data_bp$WEG1_ElbOdCo),lag(data_bp$WEG1_Ems)
                                 ,lag(data_bp$WEG1_Weser),lag(data_bp$WEG1_Rhein)
                                 ,lag(data_bp$pd),lag(data_bp$s_young),lag(data_bp$s_old)
                                 ,lag(data_bp$l_m)*lag(data_bp$f_m)
                                 ,lag(data_bp$l_m)*lag(data_bp$i_m)
                                 ,lag(data_bp$l_m)*lag(data_bp$k_m)
                                 ,lag(data_bp$f_m)*lag(data_bp$k_m)
                                 ,lag(data_bp$f_m)*lag(data_bp$i_m)
                                 ,lag(data_bp$i_m)*lag(data_bp$k_m)))
  lag_Inputs_gmm <<- na.omit(lag_Inputs)
  Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  instr <<- cbind(rep(1,nrow(data_bp)),data_bp$l_m,lag(data_bp$f_m),data_bp$k_m,lag(data_bp$i_m)
                  ,data_bp$l_m^2,(lag(data_bp$f_m))^2,data_bp$k_m^2,(lag(data_bp$i_m))^2,data_bp$w_m
                  ,data_bp$shareQW_mean,data_bp$shareFB_mean,data_bp$shareHH_mean,data_bp$shareWV
                  ,data_bp$shareWald_mean,data_bp$shareWass_mean,data_bp$shareLW_mean,data_bp$hm
                  ,data_bp$WEG1_ElbOdCo,data_bp$WEG1_Ems,data_bp$WEG1_Weser,data_bp$WEG1_Rhein
                  ,data_bp$pd,data_bp$s_young,data_bp$s_old,data_bp$l_m*lag(data_bp$f_m)
                  ,data_bp$l_m*lag(data_bp$i_m),data_bp$l_m*data_bp$k_m
                  ,lag(data_bp$f_m)*data_bp$k_m,lag(data_bp$i_m)*lag(data_bp$f_m)
                  ,lag(data_bp$i_m)*data_bp$k_m)
  instr_gmm <<- na.omit(instr)
  starting_values <<- lm(y_m ~l_m + f_m + k_m + i_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + I(0.5*i_m^2) + l_m:f_m + l_m:k_m + l_m:i_m + f_m:k_m + f_m:i_m + k_m:i_m 
                         + w_m + shareQW_mean + shareFB_mean + shareHH_mean + shareWV 
                         + shareWald_mean + shareWass_mean + shareLW_mean + hm + WEG1_ElbOdCo 
                         + WEG1_Ems + WEG1_Weser + WEG1_Rhein + pd + s_young + s_old
                         ,data_bp)
  betas_basic_m <<- as.vector(starting_values$coefficients)
  initial_betas <<- betas_basic_m[c(1:31)]
  betas_fixed <<- 0
  optimisation_boot <<- optimx(par=initial_betas,fn=gmm_moment_condition, method=method)
  betas_acf_boot <<- rbind(optimisation_boot$p1[1],optimisation_boot$p2[1],optimisation_boot$p3[1]
                           ,optimisation_boot$p4[1],optimisation_boot$p5[1],optimisation_boot$p6[1]
                           ,optimisation_boot$p7[1],optimisation_boot$p8[1],optimisation_boot$p9[1]
                           ,optimisation_boot$p10[1],optimisation_boot$p11[1]
                           ,optimisation_boot$p12[1],optimisation_boot$p13[1]
                           ,optimisation_boot$p14[1],optimisation_boot$p15[1]
                           ,optimisation_boot$p16[1],optimisation_boot$p17[1]
                           ,optimisation_boot$p18[1],optimisation_boot$p19[1]
                           ,optimisation_boot$p20[1],optimisation_boot$p21[1]
                           ,optimisation_boot$p22[1],optimisation_boot$p23[1]
                           ,optimisation_boot$p24[1],optimisation_boot$p25[1]
                           ,optimisation_boot$p26[1],optimisation_boot$p27[1]
                           ,optimisation_boot$p28[1],optimisation_boot$p29[1]
                           ,optimisation_boot$p30[1],optimisation_boot$p31[1])
  return(betas_acf_boot)
}


#=================================================================================================
# Funktion clusterBootSE									                                                         
# ----------------------
# Part II of the bootstrapping procedure for estimating the standard errors of the production 
# function's parameters. 		
# The function draws the observations for the bootstrap sample. It applies clustering, i.e. the
# function does not draw single observations but all observations of a firm. 							                                                         
#
# source: https://diffuseprior.wordpress.com/2013/01/12/the-cluster-bootstrap		        
#================================================================================================


clusterBootSE<-function(data,method,B){
  # Define index variable
  clusters<-unique(data[,"id"])
  # Generate empty matric for storing the ACF coefficients
  sterrs <- matrix(NA, nrow=B, ncol=31)
  # Start sampling
  for(i in 1:B){
    # Sample from firm IDs
    units<-sample(clusters,size=length(clusters),replace=T)
    # In the main sample, identify all observations t of the firm IDs that are part of the
    # current subsample b
    df.bs<-sapply(units,function(x)which(data[,"id"]==x))
    # Draw these observations from the main sample and store them in a new data frame
    df.bs<-data[unlist(df.bs),]
    # Apply ACF algorithm to subsample b and store coefficients in row 'i'. The function uses
    # All observations from subsample b (1:dim(df.bs)[1]) to calculate the ACF coefficients 
    sterrs[i,]<-boot.acf(data=df.bs,method=method,1:dim(df.bs)[1])
    # Tells us about the progress of the job
    cat("\r",i*100/B," % done ");flush.console()
  }
  # Table with the ACF coefficients from the main sample (first column), SE calculated from 
  # the variance of all subsamples 1...B (second column), and t-values (third column)
  val1 <- cbind(boot.acf(data=data,method=method,indices=1:dim(data)[1]),apply(sterrs,2,sd))
  val <- cbind(val1,val1[,1]/val1[,2])
  colnames(val) <- c("Estimate","Std. Error","t-value")
  cat("\n")
  return(val)
}




#================================================================================================
# 0.3 Load data
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
#     It should be identical to the one used in section (4.4).
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