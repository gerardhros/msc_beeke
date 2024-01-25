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
  
  d2<-d1

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
    
  #output Excel
  library(xlsx)
  write.xlsx(d2, file = "data/d2.xlsx")
  ```


  
  
  