#title: "Meta regression with Metafor"
#author: "Beeke von Felde"
#date: "2023-08-22"

  #packages
  install.packages("metagear")

  # Load libraries 
  library(data.table)
  library(metafor)
  library(metagear)
  
  # read data
  library(readxl)
  d1 <- read_excel("data/meta_regression/meta_regression_copy2.xlsx", sheet = 1)
  d1 <- as.data.table(d1)
  
#_______________________________________________________________________________

##organize original data
  d2 <- copy(d1)  
  
  # clean up column names
  d2 <- as.data.table(d2)
  setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
  setnames(d2,tolower(colnames(d2)))
  
  # change column names
  setnames(d2,
           old = c("rainfallmm", "irrigation_amountmm", "n_fertilizerkg_ha", "p_fertilizerkg_ha",
                   "k_fertilizerkg_ha", "bulk_densityg_cm3", "s_phwater", "s_socg_kg",
                   "s_tng_kg", "s_c:n", "b_phwater", "b_totalcg_kg", "b_totalng_kg",
                   "b_c:n", "biochar_ratet_ha"),
           new = c("rain", "irr", "n_fer", "p_fer", "k_fer", "sbd", "sph", "soc", "stn", "scn",
                   "bph", "btc", "btn", "bcn", "brate"),
           skip_absent = TRUE)
  
  # update the missing values site parameters
  d2[is.na(rain), rain := median(d2$rain,na.rm=TRUE)]
  d2[is.na(irr), irr := median(d2$irr,na.rm=TRUE)]

  #modify the unit for field studies and grain to kg/ha:
  
  d2[crop_type == "grain" & yc_mean <= 100 & experiment_type == "field", yc_sd := yc_sd * 1000]
  d2[crop_type == "grain" & yc_mean <= 100 & experiment_type == "field", yc_mean := yc_mean * 1000]
  d2[crop_type == "grain" & yr_mean <= 100 & experiment_type == "field", yr_sd := yr_sd * 1000]
  d2[crop_type == "grain" & yr_mean <= 100 & experiment_type == "field", yr_mean := yr_mean * 1000]
  
  d2[crop_type == "vegetable" & yc_mean <= 100 & experiment_type == "field", yc_sd := yc_sd * 1000]
  d2[crop_type == "vegetable" & yc_mean <= 100 & experiment_type == "field", yc_mean := yc_mean * 1000]
  d2[crop_type == "vegetable" & yr_mean <= 100 & experiment_type == "field", yr_sd := yr_sd * 1000]
  d2[crop_type == "vegetable" & yr_mean <= 100 & experiment_type == "field", yr_mean := yr_mean * 1000]
  
  d2[crop_type == "vegetable" & yc_mean <= 300 & experiment_type == "field", yc_sd := yc_sd * 100]
  d2[crop_type == "vegetable" & yc_mean <= 300 & experiment_type == "field", yc_mean := yc_mean * 100]
  d2[crop_type == "vegetable" & yr_mean <= 300 & experiment_type == "field", yr_sd := yr_sd * 100]
  d2[crop_type == "vegetable" & yr_mean <= 300 & experiment_type == "field", yr_mean := yr_mean * 100]
  
  
  #output Excel
  library(xlsx)
  fwrite(d2, file = "data/d2.csv")
#_______________________________________________________________________________

## Supplement the SD when missing
  
  # NUE
  
  # Identify missing SD values (both NA and empty string) for NUEC_SD
  missing_sd_nuer <- is.na(d2$nuer_sd) | d2$nuer_sd == ""
  missing_sd_nuec <- is.na(d2$nuec_sd) | d2$nuec_sd == ""
  
  # Calculate mean CV for non-missing values for NUEC_SD
  CV_NUEr <- mean(d2$nuer_sd[!missing_sd_nuer] / d2$nuer_mean[!missing_sd_nuer], na.rm = TRUE)
  CV_NUEc <- mean(d2$nuec_sd[!missing_sd_nuec] / d2$nuec_mean[!missing_sd_nuec], na.rm = TRUE)
  
  # Impute missing SD values based on the mean CV for NUEC_SD
  d2$nuer_sd[missing_sd_nuer] <- d2$nuer_mean[missing_sd_nuer] * 1.25 * CV_NUEr
  d2$nuec_sd[missing_sd_nuec] <- d2$nuec_mean[missing_sd_nuec] * 1.25 * CV_NUEc
  
  # SOC
  
  missing_sd_socc <- is.na(d2$socc_sd) | d2$socc_sd == ""
  CV_socc <- mean(d2$socc_sd[!missing_sd_socc] / d2$socc_mean[!missing_sd_socc], na.rm = TRUE)
  d2$socc_sd[missing_sd_socc] <- d2$socc_mean[missing_sd_socc] * 1.25 * CV_socc
  
  missing_sd_socr <- is.na(d2$socr_sd) | d2$socr_sd == ""
  CV_socr <- mean(d2$socr_sd[!missing_sd_socr] / d2$socr_mean[!missing_sd_socr], na.rm = TRUE)
  d2$socr_sd[missing_sd_socr] <- d2$socr_mean[missing_sd_socr] * 1.25 * CV_socr
  
  # pH
  
  missing_sd_phc <- is.na(d2$phc_sd) | d2$phc_sd == ""
  CV_phc <- mean(d2$phc_sd[!missing_sd_phc] / d2$phc_mean[!missing_sd_phc], na.rm = TRUE)
  d2$phc_sd[missing_sd_phc] <- d2$phc_mean[missing_sd_phc] * 1.25 * CV_phc
  
  missing_sd_phr <- is.na(d2$phr_sd) | d2$phr_sd == ""
  CV_phr <- mean(d2$phr_sd[!missing_sd_phr] / d2$phr_mean[!missing_sd_phr], na.rm = TRUE)
  d2$phr_sd[missing_sd_phr] <- d2$phr_mean[missing_sd_phr] * 1.25 * CV_phr
  
  # SBD
  
  missing_sd_sbdc <- is.na(d2$sbdc_sd) | d2$sbdc_sd == ""
  CV_sbdc <- mean(d2$sbdc_sd[!missing_sd_sbdc] / d2$sbdc_mean[!missing_sd_sbdc], na.rm = TRUE)
  d2$sbdc_sd[missing_sd_sbdc] <- d2$sbdc_mean[missing_sd_sbdc] * 1.25 * CV_sbdc
  
  missing_sd_sbdr <- is.na(d2$sbdr_sd) | d2$sbdr_sd == ""
  CV_sbdr <- mean(d2$sbdr_sd[!missing_sd_sbdr] / d2$sbdr_mean[!missing_sd_sbdr], na.rm = TRUE)
  d2$sbdr_sd[missing_sd_sbdr] <- d2$sbdr_mean[missing_sd_sbdr] * 1.25 * CV_sbdr
  
#_______________________________________________________________________________
  
## Estimate meta-analytical response measure (MD Method)
  
  # calculate effect size 

library(metafor)

  #Y
  es21y <- escalc(measure = "MD", data = d2, 
                 m1i = yr_mean, sd1i = yr_sd, n1i = yr_n,
                 m2i = yc_mean, sd2i = yc_sd, n2i = yc_n)
  #write.xlsx(es21y,file = "data/es21y.xlsx")
  
  #NUE
  es21nue <- escalc(measure = "MD", data = d2, 
                    m1i = nuer_mean, sd1i = nuer_sd, n1i = nuer_n,
                    m2i = nuec_mean, sd2i = nuec_sd, n2i = nuec_n )
  #write.xlsx(es21nue,file = "data/es21nue.xlsx")
  
  #SOC
  es21soc <- escalc(measure = "MD", data = d2, 
                    m1i = socr_mean, sd1i = socr_sd, n1i = socr_n,
                    m2i = socc_mean, sd2i = socc_sd, n2i = socc_n)
  #write.xlsx(es21soc,file = "data/es21soc.xlsx")
  
  #pH
  es21ph <- escalc(measure = "MD", data = d2, 
                   m1i = phr_mean, sd1i = phr_sd, n1i = phr_n,
                   m2i = phc_mean, sd2i = phc_sd, n2i = phc_n)
  #write.xlsx(es21ph,file = "data/es21ph.xlsx")
  
  #SBD
  es21sbd <- escalc(measure = "MD", data = d2, 
                    m1i = sbdr_mean, sd1i = sbdr_sd, n1i = sbdr_n,
                    m2i = sbdc_mean, sd2i = sbdc_sd, n2i = sbdc_n)
  #write.xlsx(es21sbd,file = "data/es21sbd.xlsx")    
#_______________________________________________________________________________
  
##scaling of the variables to unit variance

  d3 <- copy(d2)
  
#numeric values
  
  #check classes
  #str(d3) 
  
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
  
  #d3[, experiment_type_scaled := scale(experiment_type)]
  #d3[, crop_scaled := scale(crop)]
  #d3[, crop_type_scaled := scale(crop_type)]
  #d3[, water_management_scaled := scale(water_magement)]
  #d3[, soil_texture_scaled := scale(stexture)]
  
  library(xlsx)
  fwrite(d3, file = "data/d3.csv")
  
#_______________________________________________________________________________  
  
# make forest plots per group treatments

#for yield:

##histogram pro crop type:

hist_grain <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "grain"])
hist_grain <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "grain"])

hist_fruit <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "fruit"])
hist_fruit <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "fruit"])

hist_industrial <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "industrial"])
hist_industrial <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "industrial"])  

hist_legumes <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "legumes"])
hist_legumes <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "legumes"])  

hist_peanut <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop == "peanut"])
hist_peanut <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop == "peanut"])

hist_tubers <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "tubers"])
hist_tubers <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "tubers"])

hist_vegetable <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "vegetable"])
hist_vegetable <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "vegetable"])

hist_others <- hist(d2$yc_mean[d2$experiment_type == "field" & d2$crop_type == "others"])
hist_others <- hist(d2$yr_mean[d2$experiment_type == "field" & d2$crop_type == "others"])

#_______________________________________________________________________________

##ggplot for field studies and crop_type == "grain"

es21y = as.data.table(es21y)
#filter the data set and only select the data where yi is missing
es21y_grain = es21y[!is.na(yi) & experiment_type == "field" & crop_type == "grain"]
setorder(es21y_grain,-yi)
es21y_grain[,id:=.I]
es21y_grain[,yi_cor := (exp(yi)-1)*100]
es21y_grain[,vi_cor := (exp(vi)-1)*100]
library(ggplot2)
ggplot(data=es21y_grain,aes(x=id,y=yi)) + geom_line() + geom_errorbar( aes(x=id, ymin=yi-sqrt(vi), ymax=yi+sqrt(vi)), width=0.4, colour="orange", alpha=0.9, linewidth=1.3) + theme_bw() + ggtitle('crop yield response to biochar addition on grain crop') + xlab("study-id") + ylab("log response ratio")  

##forest plot

res <- rma(yi, vi, data=es21y)
forest(res)

#_______________________________________________________________________________

##main factor analysis for log response ratio for yield

dy_grain <- copy(es21y_grain)

# update the missing values site parameters
dy_grain[is.na(irr), irr := median(dy_grain$irr,na.rm=TRUE)]












  

