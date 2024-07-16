## Functions to be used commonly in scripts
# It also generates common labels for figures
## PBH
## March 2023


# Label for PM2.5 and for temperature ----
lab_pm25 <- expression(paste("Monthly ",PM[2.5]," [",mu,"g/",m^3,"]",""))
lab_temp <- "Monthly land temperature [°C]"
lab_mr <- "75+ all-cause monthly \n mortality rate [per 1,000]"
lab_mr2 <- "75+ all-cause monthly mortality rate [per 1,000]"
lab_rr <- expression(paste("Percentage change in monthly mortality rate for a 10 ",mu,"g/",m^3," increase in ",PM[2.5],""))
lab_rr_temp <- expression(paste("Percentage change in monthly mortality rate for a 1°C increase in temperature"))

lab_rr_line1 <- "Percentage change in monthly mortality"
lab_rr_line2 <- expression(paste("rate for a 10 ",mu,"g/",m^3," increase in ",PM[2.5],""))
lab_rr_line2_temp <- "rate for a 1°C increase in temperature"


# Function to get data from fitted models ----
getModelInfo <- function(mod,name,ols=F,baseRate=0, robustSE=T, data_df=df){
  
  coef <- if(robustSE) summary(mod)$coefficients else summary(mod)$coefficients$cond
  
  # clustered standard errors by commune
  cluster_se <- if(robustSE) vcovCL(mod, cluster = data_df$commune) else vcov(mod)$cond
  cluster_se <- sqrt(diag(cluster_se))
  
  # all
  out <- data.frame(name=name,param=rownames(coef),est=coef[,1],se=cluster_se,N=nobs(mod),bic=BIC(mod))
  
  if(ols){
  # % effect for OLS (linear)  = (coef/base rate)*100
    out <- out %>% 
      mutate(rr=est/baseRate*100,
             rr_low=(est-1.96*se)/baseRate*100, # by the huge number of n, the t-stat converges to 1.96 for 5%
             rr_high=(est+1.96*se)/baseRate*100)
  } else {
  out <- out %>% 
    mutate(rr=exp(est)*100-100,
           rr_low=exp(est-1.96*se)*100-100, # by the huge number of n, the t-stat converges to 1.96 for 5%
           rr_high=exp(est+1.96*se)*100-100)  
  }
  
  return(out)
}





# EoF