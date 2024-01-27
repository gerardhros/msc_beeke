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
  d3[, rainfall := as.numeric(rain)]
  d3[, irrigation_amountmm := as.numeric(irr)]
  d3[, n_fertilizerkg_ha := as.numeric(n_fer)]
  d3[, p_fertilizerkg_ha := as.numeric(p_fer)]
  d3[, k_fertilizerkg_ha := as.numeric(k_fer)]
  d3[, bulk_densityg_cm3 := as.numeric(sbd)]
  d3[, s_phwater := as.numeric(sph)]
  d3[, s_socg_kg := as.numeric(soc)]
  d3[, s_tng_kg := as.numeric(stn)]
  d3[, soc := as.numeric(soc)]
  d3[, s_c_n := as.numeric(scn)]
  d3[, b_phwater := as.numeric(bph)]
  d3[, b_totalcg_kg := as.numeric(btc)]
  d3[, b_totalng_kg := as.numeric(btn)]
  d3[, b_c_n := as.numeric(bcn)]
  d3[, biochar_ratet_ha := as.numeric(brate)]
  
  #scaling
  
  d3[, rainfallmm_scaled := scale(rain)]
  d3[, irrigation_amountmm_scaled := scale(irr)]
  d3[, n_fertilizerkg_ha_scaled := scale(n_fer)]
  d3[, p_fertilizerkg_ha_scaled := scale(p_fer)]
  d3[, k_fertilizerkg_ha_scaled := scale(k_fer)]
  d3[, bulk_densityg_cm3_scaled := scale(sbd)]
  d3[, s_phwater_scaled := scale(sph)]
  d3[, s_socg_kg_scaled := scale(soc)]
  d3[, s_tng_kg_scaled := scale(stn)]
  d3[, soc_scaled := scale(soc)]
  d3[, s_c_n_scaled := scale(scn)]
  d3[, b_phwater_scaled := scale(bph)]
  d3[, b_totalcg_kg_scaled := scale(btc)]
  d3[, b_totalng_kg_scaled := scale(btn)]
  d3[, b_c_n_scaled := scale(bcn)]
  d3[, biochar_ratet_ha_scaled := scale(brate)]
  
  # Non-numeric values 
  
  d3[, experiment_type_scaled := scale(experiment_type)]
  d3[, crop_scaled := scale(crop)]
  d3[, crop_type_scaled := scale(crop_type)]
  d3[, water_management_scaled := scale(water_magement)]
  d3[, soil_texture_scaled := scale(stexture)]
  
  