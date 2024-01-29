  ```{r setup, include=FALSE}
  knitr::opts_chunk$set(echo = TRUE)
  ```

## R Markdown  

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents.

  #packages
  install.packages("metagear")
  
  ```{r load packages,message=FALSE,warning=FALSE}
  
  # Load libraries 
  library(data.table)
  library(metafor)
  library(metagear)
  
  # read data
  library(readxl)
  d1 <- read_excel("data/meta_regression_copy1.xlsx", sheet = 1)
  d1 <- as.data.table(d1)
  
  ```

## Supplement the SD when missing
  
  ```{r Supplement the standard deviation missing value_Common Method}
  
d2 <- d1

NUE

  # Identify missing SD values (both NA and empty string) for NUEC_SD
    missing_sd_nuer <- is.na(d2$NUER_SD) | d2$NUER_SD == ""
    missing_sd_nuec <- is.na(d2$NUEC_SD) | d2$NUEC_SD == ""
  
  # Calculate mean CV for non-missing values for NUEC_SD
    CV_NUEr <- mean(d2$NUER_SD[!missing_sd_nuer] / d2$NUER_mean[!missing_sd_nuer], na.rm = TRUE)
    CV_NUEc <- mean(d2$NUEC_SD[!missing_sd_nuec] / d2$NUEC_mean[!missing_sd_nuec], na.rm = TRUE)
  
  # Impute missing SD values based on the mean CV for NUEC_SD
    d2$NUER_SD[missing_sd_nuer] <- d2$NUER_mean[missing_sd_nuer] * 1.25 * CV_NUEr
    d2$NUEC_SD[missing_sd_nuec] <- d2$NUEC_mean[missing_sd_nuec] * 1.25 * CV_NUEc
    
SOC   

    missing_sd_socc <- is.na(d2$SOCC_SD) | d2$SOCC_SD == ""
    CV_socc <- mean(d2$SOCC_SD[!missing_sd_socc] / d2$SOCC_mean[!missing_sd_socc], na.rm = TRUE)
    d2$SOCC_SD[missing_sd_socc] <- d2$SOCC_mean[missing_sd_socc] * 1.25 * CV_socc
     
    missing_sd_socr <- is.na(d2$SOCR_SD) | d2$SOCR_SD == ""
    CV_socr <- mean(d2$SOCR_SD[!missing_sd_socr] / d2$SOCR_mean[!missing_sd_socr], na.rm = TRUE)
    d2$SOCR_SD[missing_sd_socr] <- d2$SOCR_mean[missing_sd_socr] * 1.25 * CV_socr

pH

    missing_sd_phc <- is.na(d2$pHC_SD) | d2$pHC_SD == ""
    CV_phc <- mean(d2$pHC_SD[!missing_sd_phc] / d2$pHC_mean[!missing_sd_phc], na.rm = TRUE)
    d2$pHC_SD[missing_sd_phc] <- d2$pHC_mean[missing_sd_phc] * 1.25 * CV_phc
    
    missing_sd_phr <- is.na(d2$pHR_SD) | d2$pHR_SD == ""
    CV_phr <- mean(d2$pHR_SD[!missing_sd_phr] / d2$pHR_mean[!missing_sd_phr], na.rm = TRUE)
    d2$pHR_SD[missing_sd_phr] <- d2$pHR_mean[missing_sd_phr] * 1.25 * CV_phr

SBD

    missing_sd_sbdc <- is.na(d2$SBDC_SD) | d2$SBDC_SD == ""
    CV_sbdc <- mean(d2$SBDC_SD[!missing_sd_sbdc] / d2$SBDC_mean[!missing_sd_sbdc], na.rm = TRUE)
    d2$SBDC_SD[missing_sd_sbdc] <- d2$SBDC_mean[missing_sd_sbdc] * 1.25 * CV_sbdc
    
    missing_sd_sbdr <- is.na(d2$SBDR_SD) | d2$SBDR_SD == ""
    CV_sbdr <- mean(d2$SBDR_SD[!missing_sd_sbdr] / d2$SBDR_mean[!missing_sd_sbdr], na.rm = TRUE)
    d2$SBDR_SD[missing_sd_sbdr] <- d2$SBDR_mean[missing_sd_sbdr] * 1.25 * CV_sbdr

  # clean up column names
  d2 <- as.data.table(d2)
  setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
  setnames(d2,tolower(colnames(d2)))
  
  # change column names
  colnames(d2)[colnames(d2) == "Soil_texture"] <- "stexture"
  colnames(d2)[colnames(d2) == "rainfall (mm)"] <- "rain"
  colnames(d2)[colnames(d2) == "irrigation_amount (mm)"] <- "irr"
  colnames(d2)[colnames(d2) == "N_fertilizer (kg/ha)"] <- "n_fer"
  colnames(d2)[colnames(d2) == "P_fertilizer (kg/ha)"] <- "p_fer"
  colnames(d2)[colnames(d2) == "K_fertilizer (kg/ha)"] <- "k_fer"
  colnames(d2)[colnames(d2) == "Bulk_density (g/cm3)"] <- "sbd"
  colnames(d2)[colnames(d2) == "S_pH(water)"] <- "sph"
  colnames(d2)[colnames(d2) == "S_SOC(g/kg)"] <- "soc"
  colnames(d2)[colnames(d2) == "S_TN(g/kg)"] <- "stn"
  colnames(d2)[colnames(d2) == "S_C:N"] <- "scn"
  colnames(d2)[colnames(d2) == "B_pH(water)"] <- "bph"
  colnames(d2)[colnames(d2) == "B_TotalC (g/kg)"] <- "btc"
  colnames(d2)[colnames(d2) == "B_TotalN  (g/kg)"] <- "btn"
  colnames(d2)[colnames(d2) == "B_C:N"] <- "bcn"
  colnames(d2)[colnames(d2) == "biochar_rate (t/ha)"] <- "brate"
  
  
  
  #output Excel
  library(xlsx)
  write.xlsx(d2, file = "data/d2.xlsx")
  ```
_______________________________________________________________________________
  
## Estimate meta-analytical response measure (ROM Method)
  
  
  ```{r Calculate effect size_ROM}
  
  # calculate effect size 
  # output Excel
library(metafor)

Y
  es21y <- escalc(measure = "MD", data = d2, 
                 m1i = yr_mean, sd1i = yr_sd, n1i = yr_n,
                 m2i = yc_mean, sd2i = yc_sd, n2i = yc_n)
  write.xlsx(es21y,file = "data/es21y.xlsx")
  
NUE
  es21nue <- escalc(measure = "MD", data = d2, 
                 m1i = nuer_mean, sd1i = nuer_sd, n1i = nuer_n,
                 m2i = nuec_mean, sd2i = nuec_sd, n2i = nuec_n )
  write.xlsx(es21nue,file = "data/es21nue.xlsx")
  
SOC
  es21soc <- escalc(measure = "MD", data = d2, 
                 m1i = socr_mean, sd1i = socr_sd, n1i = socr_n,
                 m2i = socc_mean, sd2i = socc_sd, n2i = socc_n)
  write.xlsx(es21soc,file = "data/es21soc.xlsx")
  
pH
  es21ph <- escalc(measure = "MD", data = d2, 
                 m1i = phr_mean, sd1i = phr_sd, n1i = phr_n,
                 m2i = phc_mean, sd2i = phc_sd, n2i = phc_n)
  write.xlsx(es21ph,file = "data/es21ph.xlsx")
  
SBD
  es21sbd <- escalc(measure = "MD", data = d2, 
                 m1i = sbdr_mean, sd1i = sbdr_sd, n1i = sbdr_n,
                 m2i = sbdc_mean, sd2i = sbdc_sd, n2i = sbdc_n)
  write.xlsx(es21sbd,file = "data/es21sbd.xlsx")

  ```
_______________________________________________________________________________
  
##scaling of the variables to unit variance

  d3 <- d2
  
#numeric values
  
  # Convert columns to numeric
  d3$rain <- as.numeric(d3$rain)
  d3$irr <- as.numeric(d3$irr)
  d3$n_fer <- as.numeric(d3$n_fer)
  d3$p_fer <- as.numeric(d3$p_fer)
  d3$k_fer <- as.numeric(d3$k_fer)
  d3$sbd <- as.numeric(d3$sbd)
  d3$sph <- as.numeric(d3$sph)
  d3$soc <- as.numeric(d3$soc)
  d3$stn <- as.numeric(d3$stn)
  d3$scn <- as.numeric(d3$scn)
  d3$bph <- as.numeric(d3$bph)
  d3$btc <- as.numeric(d3$btc)
  d3$btn <- as.numeric(d3$btn)
  d3$bcn <- as.numeric(d3$bcn)
  d3$brate <- as.numeric(d3$brate)
  
  # scale function
  d3[, rain_scaled := scale(rain)]
  d3[, irr_scaled := scale(irr)]
  d3[, n_fer_scaled := scale(n_fer)]
  d3[, p_fer_scaled := scale(p_fer)]
  d3[, k_fer_scaled := scale(k_fer)]
  d3[, sbd_scaled := scale(sbd)]
  d3[, sph_scaled := scale(sph)]
  d3[, soc_scaled := scale(soc)]
  d3[, stn_scaled := scale(stn)]
  d3[, scn_scaled := scale(scn)]
  d3[, bph_scaled := scale(bph)]
  d3[, btc_scaled := scale(btc)]
  d3[, btn_scaled := scale(btn)]
  d3[, bcn_scaled := scale(bcn)]
  d3[, brate_scaled := scale(brate)]
  
# Non-numeric values 
  
  d3[, experiment_type_scaled := scale(experiment_type)]
  d3[, crop_scaled := scale(crop)]
  d3[, crop_type_scaled := scale(crop_type)]
  d3[, water_management_scaled := scale(water_magement)]
  d3[, soil_texture_scaled := scale(stexture)]
  
_______________________________________________________________________________  
  
# make forest plots per group treatments

  ```{
    r forest plot per site_factor_ROM,warning=FALSE
  }

library(metafor)
  
  
Yield (y)

# convert to data.tables
  d4y <- as.data.table(es21y)

# what are the site specific parameters to be assessed
  d4y <- data.table(site_factors =  c('ALL',unique(d02$tillage)))

# what are labels
  
  # Create a data frame for forest plot
  d_forest <- data.table(
    stexture = d2$stexture,
    experiment_type = d2$experiment_type,
    crop = d2$crop,
    crop_type = d2$crop_type,
    water_management = d2$water_management,
    rain = d2$rain,
    irr = d2$irr,
    n_fer = d2$n_fer,
    p_fer = d2$p_fer,
    k_fer = d2$k_fer,
    sbd = d2$sbd,
    sph = d2$sph,
    soc = d2$soc,
    stn = d2$stn,
    scn = d2$scn,
    bph = d2$bph,
    btc = d2$btc,
    btn = d2$btn,
    bcn = d2$bcn,
    brate = d2$brate,
  )
  
  # Run Forest Plot for Yield
  forest(
    yi = d_forest$yc_mean,   # Assuming yc_mean is the yield mean column
    vi = d_forest$yc_sd,     # Assuming yc_sd is the yield standard deviation column
    slab = d_forest$stexture,
    psize = 0.9,
    cex = 1,
    xlab = "Change in Yield",
    header = "Site-Specific Factors",
    col = "#CC0000",
    lwd = 2
  )
  
  
  
  
  
  
  d02.treat[treatment=='ALL',desc := 'All']
  d02.treat[treatment=='EE',desc := 'Enhanced Efficiency']
  d02.treat[treatment=='CF',desc := 'Combined fertilizer']
  d02.treat[treatment=='RES',desc := 'Residue retention']
  d02.treat[treatment=='RFP',desc := 'Fertilizer placement']
  d02.treat[treatment=='RFR',desc := 'Fertilizer rate']
  d02.treat[treatment=='ROT',desc := 'Crop rotation']
  d02.treat[treatment=='RFT',desc := 'Fertilizer timing']
  d02.treat[treatment=='OF',desc := 'Organic fertilizer']
  d02.treat[treatment=='RT',desc := 'Reduced tillage']
  d02.treat[treatment=='NT',desc := 'No tillage']
  d02.treat[treatment=='CC',desc := 'Crop cover']
  d02.treat[treatment=='BC',desc := 'Biochar']

# a list to store the coefficients
  out2 = out3 = list()

# make a for loop to do a main analysis per treatment
  for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    # run without selection to estimate overall mean
    r_nue <- rma.mv(yi,vi, data=d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    # run for selected treatment
    r_nue <- rma.mv(yi,vi, data=d02[tillage==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  }
  
  # save output in a list
  out2[[i]] <- data.table(mean = as.numeric(r_nue$b),
                          se = as.numeric(r_nue$se),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
  }

# convert lists to vector
  out2 <- rbindlist(out2)

# plot for NUE
  forest(out2$mean, out2$se, slab=out2$label, psize=0.9, cex=1, xlab="Change in NUE (%)", header="Treatment", col="#CC0000", lwd=2)
  
  
  ```
_______________________________________________________________________________


  
  

