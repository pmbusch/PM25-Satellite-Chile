## Functions to be used commonly in scripts
## PBH
## March 2023


# Label for PM2.5 and for temperature ----
lab_pm25 <- expression(paste("",PM[2.5]," Exposure [",mu,"g/",m^3,"]",""))
lab_temp <- "Land Temperature [°C]"
lab_mr <- "75+ Mortality Rate \n All-Cause [per 1,000 habs]"
lab_mr2 <- "75+ Mortality Rate All-Cause [per 1,000 habs]"
lab_rr <- expression(paste("Percentage change in Mortality rate by 10 ",mu,"g/",m^3," ",PM[2.5],""))


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