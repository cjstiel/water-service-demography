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
#					FUNCTIONS
#
#
#	CONTENT: User-written functions for the analysis
#
#	OUTLINE: function 'dstat'
#			 function 'gmm_moment_condition'
#			 function 'boot.acf'
#			 function 'clusterBootSE'
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
#================================================================================================



#================================================================================================
# Define functions to be used in the analysis              	        
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
  val1 <- cbind(boot.acf(data=data,method=method,indices=1:dim(data_w)[1]),apply(sterrs,2,sd))
  val <- cbind(val1,val1[,1]/val1[,2])
  colnames(val) <- c("Estimate","Std. Error","t-value")
  cat("\n")
  return(val)
}



#=================================================================================================
date()
#========================================== End of file ==========================================
