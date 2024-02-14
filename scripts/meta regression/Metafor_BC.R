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
  d1 <- read_excel("data/meta_regression/meta_regression_copy2.xlsx", sheet = 3)
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
                   "k_fertilizerkg_ha", "soil_texture", "bulk_densityg_cm3", "s_phwater", "s_socg_kg",
                   "s_tng_kg", "s_c:n", "b_phwater", "b_totalcg_kg", "b_totalng_kg",
                   "b_c:n", "biochar_ratet_ha"),
           new = c("rain", "irr", "n_fer", "p_fer", "k_fer", "texture", "sbd", "sph", "soc", "stn", "scn",
                   "bph", "btc", "btn", "bcn", "brate"),
           skip_absent = TRUE)
  
  d2$rain <- as.numeric(d2$rain)
  d2$irr <- as.numeric(d2$irr)
  d2$n_fer <- as.numeric(d2$n_fer)
  d2$p_fer <- as.numeric(d2$p_fer)
  d2$k_fer <- as.numeric(d2$k_fer)
  d2$sbd <- as.numeric(d2$sbd)
  d2$sph <- as.numeric(d2$sph)
  d2$soc <- as.numeric(d2$soc)
  d2$stn <- as.numeric(d2$stn)
  d2$scn <- as.numeric(d2$scn)
  d2$bph <- as.numeric(d2$bph)
  d2$btc <- as.numeric(d2$btc)
  d2$btn <- as.numeric(d2$btn)
  d2$bcn <- as.numeric(d2$bcn)
  d2$brate <- as.numeric(d2$brate)
  
 # modify the unit for site properties:
  # bc total carbon from % to g/kg
  d2[btc <= 100, btc := btc * 10]
  # bc total nitrogen from % to g/kg
  d2[btn <= 2, btc := btc * 10]
  
  
 # modify the unit for field studies and grain to kg/ha:
  
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
  
 # update the missing values site parameters
  d2[is.na(rain), rain := median(d2$rain,na.rm=TRUE)]
  d2[is.na(irr), irr := median(d2$irr,na.rm=TRUE)]
  d2[is.na(n_fer), n_fer := median(d2$n_fer,na.rm=TRUE)]
  d2[is.na(p_fer), p_fer := median(d2$p_fer,na.rm=TRUE)]
  d2[is.na(k_fer), k_fer := median(d2$k_fer,na.rm=TRUE)]
  d2[is.na(sbd), sbd := median(d2$sbd,na.rm=TRUE)]
  d2[is.na(sph), sph := median(d2$sph,na.rm=TRUE)]
  d2[is.na(soc), soc := median(d2$soc,na.rm=TRUE)]
  d2[is.na(stn), stn := median(d2$stn,na.rm=TRUE)]
  d2[is.na(scn), scn := median(d2$scn,na.rm=TRUE)]
  d2[is.na(bph), bph := median(d2$bph,na.rm=TRUE)]
  d2[is.na(btc), btc := median(d2$btc,na.rm=TRUE)]
  d2[is.na(btn), btn := median(d2$btn,na.rm=TRUE)]
  d2[is.na(bcn), bcn := median(d2$bcn,na.rm=TRUE)]
  d2[is.na(brate), brate := median(d2$brate,na.rm=TRUE)]
  
#_______________________________________________________________________________

## Supplement the SD when missing
  
  # NUE
  
  # Identify missing SD values (both NA and empty string) for NUEC_SD
  missing_sd_nuer <- is.na(d2$nuer_sd) | d2$nuer_sd == ""
  missing_sd_nuec <- is.na(d2$nuec_sd) | d2$nuec_sd == ""
  
  # Calculate mean CV for non-missing values for NUEC_SD
  CV_nuer <- mean(d2$nuer_sd[!missing_sd_nuer] / d2$nuer_mean[!missing_sd_nuer], na.rm = TRUE)
  CV_nuec <- mean(d2$nuec_sd[!missing_sd_nuec] / d2$nuec_mean[!missing_sd_nuec], na.rm = TRUE)
  
  # Impute missing SD values based on the mean CV for NUEC_SD
  d2$nuer_sd[missing_sd_nuer] <- d2$nuer_mean[missing_sd_nuer] * 1.25 * CV_NUEr
  d2$nuec_sd[missing_sd_nuec] <- d2$nuec_mean[missing_sd_nuec] * 1.25 * CV_NUEc
  
  # Yield
  missing_sd_yc <- is.na(d2$yc_sd) | d2$yc_sd == ""
  CV_yc <- mean(d2$yc_sd[!missing_sd_yc] / d2$yc_mean[!missing_sd_yc], na.rm = TRUE)
  d2$yc_sd[missing_sd_yc] <- d2$yc_mean[missing_sd_yc] * 1.25 * CV_yc
  
  missing_sd_yr <- is.na(d2$yr_sd) | d2$yr_sd == ""
  CV_yr <- mean(d2$yr_sd[!missing_sd_yr] / d2$yr_mean[!missing_sd_yr], na.rm = TRUE)
  d2$yr_sd[missing_sd_yr] <- d2$yr_mean[missing_sd_yr] * 1.25 * CV_yr
  
  
  #SOC
  
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
  
  #output Excel
  library(xlsx)
  fwrite(d2, file = "data/d2.csv")
#_______________________________________________________________________________
  
## Estimate meta-analytical response measure (MD Method)
  
  # calculate effect size 

library(metafor)

  #Y
  es21y <- escalc(measure = "MD", data = d2, 
                 m1i = yr_mean, sd1i = yr_sd, n1i = yr_n,
                 m2i = yc_mean, sd2i = yc_sd, n2i = yc_n)
  fwrite(es21y, file = "data/es21y.csv")
  
  #NUE
  es21nue <- escalc(measure = "MD", data = d2, 
                    m1i = nuer_mean, sd1i = nuer_sd, n1i = nuer_n,
                    m2i = nuec_mean, sd2i = nuec_sd, n2i = nuec_n )
  fwrite(es21nue, file = "data/es21nue.csv")
  
  #SOC
  es21soc <- escalc(measure = "MD", data = d2, 
                    m1i = socr_mean, sd1i = socr_sd, n1i = socr_n,
                    m2i = socc_mean, sd2i = socc_sd, n2i = socc_n)
  fwrite(es21soc, file = "data/es21soc.csv")
  
  #pH
  es21ph <- escalc(measure = "MD", data = d2, 
                   m1i = phr_mean, sd1i = phr_sd, n1i = phr_n,
                   m2i = phc_mean, sd2i = phc_sd, n2i = phc_n)
  fwrite(es21ph, file = "data/es21ph.csv")
  
  #SBD
  es21sbd <- escalc(measure = "MD", data = d2, 
                    m1i = sbdr_mean, sd1i = sbdr_sd, n1i = sbdr_n,
                    m2i = sbdc_mean, sd2i = sbdc_sd, n2i = sbdc_n)
  fwrite(es21sbd, file = "data/es21sbd.csv")  
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
  ggplot(data = es21y_grain, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on grain') + 
    xlab("study-id") + 
    ylab("log response ratio")
  #forest plot
  res <- rma(yi, vi, data=es21y)
  forest(res)

##ggplot for field studies and crop_type == "fruit"

  es21y = as.data.table(es21y)
  #filter the data set and only select the data where yi is missing
  es21y_fruit = es21y[!is.na(yi) & experiment_type == "field" & crop_type == "fruit"]
  setorder(es21y_fruit,-yi)
  es21y_fruit[,id:=.I]
  es21y_fruit[,yi_cor := (exp(yi)-1)*100]
  es21y_fruit[,vi_cor := (exp(vi)-1)*100]
  library(ggplot2)
  ggplot(data = es21y_fruit, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on fruit') + 
    xlab("study-id") + 
    ylab("log response ratio")
  #forest plot
  res <- rma(yi, vi, data=es21y)
  forest(res)

# ggplot for field studies and crop_type == "veg"

  es21y = as.data.table(es21y)
  # filter the data set and only select the data where yi is missing
  es21y_veg = es21y[!is.na(yi) & experiment_type == "field" & crop_type == "vegetable"]
  setorder(es21y_veg, -yi)
  es21y_veg[, id := .I]
  es21y_veg[, yi_cor := (exp(yi) - 1) * 100]
  es21y_veg[, vi_cor := (exp(vi) - 1) * 100]
  library(ggplot2)
  ggplot(data = es21y_veg, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on vegetables') + 
    xlab("study-id") + 
    ylab("log response ratio")

# ggplot for field studies and crop_type == "legumes"
  
  es21y = as.data.table(es21y)
  # filter the data set and only select the data where yi is missing
  es21y_leg = es21y[!is.na(yi) & experiment_type == "field" & crop_type == "legumes"]
  setorder(es21y_leg, -yi)
  es21y_leg[, id := .I]
  es21y_leg[, yi_cor := (exp(yi) - 1) * 100]
  es21y_leg[, vi_cor := (exp(vi) - 1) * 100]
  library(ggplot2)
  ggplot(data = es21y_leg, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on legumes') + 
    xlab("study-id") + 
    ylab("log response ratio")
  
# ggplot for field studies and crop_type == "tubers"
  
  es21y = as.data.table(es21y)
  # filter the data set and only select the data where yi is missing
  es21y_tubers = es21y[!is.na(yi) & experiment_type == "field" & crop_type == "tubers"]
  setorder(es21y_tubers, -yi)
  es21y_tubers[, id := .I]
  es21y_tubers[, yi_cor := (exp(yi) - 1) * 100]
  es21y_tubers[, vi_cor := (exp(vi) - 1) * 100]
  library(ggplot2)
  ggplot(data = es21y_tubers, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on tubers') + 
    xlab("study-id") + 
    ylab("log response ratio")

# ggplot for field studies and crop_type == "others"
  
  es21y = as.data.table(es21y)
  # filter the data set and only select the data where yi is missing
  es21y_others = es21y[!is.na(yi) & experiment_type == "field" & crop_type == "others"]
  setorder(es21y_others, -yi)
  es21y_others[, id := .I]
  es21y_others[, yi_cor := (exp(yi) - 1) * 100]
  es21y_others[, vi_cor := (exp(vi) - 1) * 100]
  library(ggplot2)
  ggplot(data = es21y_others, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on others (tea, tobacco, zizyphus, tall fescue, cocoyam, and pasture)') + 
    xlab("study-id") + 
    ylab("log response ratio")

# ggplot for field studies and all crop_types
  es21y = as.data.table(es21y)
  # filter the data set and only select the data where yi is missing
  es21y_all = es21y[!is.na(yi) & experiment_type == "field"]
  setorder(es21y_all, -yi)
  es21y_all[, id := .I]
  es21y_all[, yi_cor := (exp(yi) - 1) * 100]
  es21y_all[, vi_cor := (exp(vi) - 1) * 100]
  library(ggplot2)
  ggplot(data = es21y_all, aes(x = id, y = yi)) + 
    geom_line() + 
    geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                  width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
    theme_bw() + 
    ggtitle('crop yield response to biochar addition on all crop types)') + 
    xlab("study-id") + 
    ylab("log response ratio")
  
#_______________________________________________________________________________

#Meta-regression for main factors
##main factor analysis for log response ratio for yield

# Load the data.table package
library(data.table)

d4y <- copy(es21y)

# Convert data frame to data.table
d4y <- as.data.table(d4y)

# include only field studies
d4y <- d4y[experiment_type == "field"]

# what are the factors to be evaluated
var.site <- c("rain", "irr", "texture", "water_management", "sbd", "sph", "soc", "stn", "scn")
var.crop <- c("crop", "crop_type", "n_fer", "p_fer", "k_fer")
var.bc <- c("bph", "btc", "btn", "bcn", "brate")

# i select only one example

# the columns to be assessed
var.sel <- c(var.site,var.crop,var.bc)

# run without a main factor selection to estimate overall mean
ry_0 <- rma.mv(yi, vi, data = d4y, random = list(~ 1|studyid), method = "REML", sparse = TRUE)


# objects to store the effects per factor as well summary stats of the meta-analytical models
out1.est = out1.sum = list()

# evaluate the impact of treatment (column rain) on yield given site properties
for(i in var.sel){
  
  # check whether the column is a numeric or categorical variable
  vartype = is.character(d4y[, rain])
  
  # run with the main factor treatment
  if (vartype) {
    # run a meta-regression model for main categorical variable
    ry_1 <- rma.mv(yi, vi, mods = ~factor(varsel) - 1, 
                  data = d4y[, .(yi, vi, studyid, varsel = rain)], 
                  random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  } else {
    # run a meta-regression model for main numerical variable
    ry_1 <- rma.mv(yi, vi, mods = ~varsel, 
                  data = d4y[, .(yi, vi, studyid, varsel = rain)], 
                  random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  }
  
  # save output in a list: the estimated impact of the explanatory variable
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)', '', rownames(ry_1$b)),
                              mean = round(as.numeric(ry_1$b), 3),
                              se = round(as.numeric(ry_1$se), 3),
                              ci.lb = round(as.numeric(ry_1$ci.lb), 3),
                              ci.ub = round(as.numeric(ry_1$ci.ub), 3),
                              pval = round(as.numeric(ry_1$pval), 3))
  
  # save output in a list: the summary stats collected
  out1.sum[[i]] <- data.table(var = i,
                              AIC = ry_1$fit.stats[4, 2],
                              ll = ry_1$fit.stats[1, 2],
                              ll_impr = round(100 * (1 - ry_1$fit.stats[1, 2] / ry_0$fit.stats[1, 2]), 2),
                              r2_impr = round(100 * max(0, (sum(ry_0$sigma2) - sum(ry_1$sigma2)) / sum(ry_0$sigma2)), 2),
                              pval = round(anova(ry_1, ry_0)$pval, 3)
  )
  
}

# merge output into a data.table
out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)
print(out1.sum)
print(out1.est)  

#_______________________________________________________________________________
#Meta-regression for main factors with interactions

# make a function to extract relevant model statistics
estats <- function(model_new, model_base) {
  out <- data.table(
    AIC = model_new$fit.stats[4, 2],
    ll = model_new$fit.stats[1, 2],
    ll_impr = round(100 * (1 - model_new$fit.stats[1, 2] / model_base$fit.stats[1, 2]), 2),
    r2_impr = round(100 * max(0, (sum(model_base$sigma2) - sum(model_new$sigma2)) / sum(model_base$sigma2)), 2),
    pval = round(anova(model_new, model_base)$pval, 3)
  )
  return(out)
}
# run without a main factor selection to estimate overall mean
ry_0 <- rma.mv(yi,vi, data = d4y,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# 1. make a simple meta-regression model without interaction but with more than one explanatory variable

# here just "rain"
ry_1 <- rma.mv(yi,vi, 
                  mods = ~rain - 1, 
                  data = d4y,
                  random = list(~ 1|studyid), method="REML",sparse = TRUE)

# show stats and improvements
out = estats(model_new = ry_1,model_base = ry_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(ry_1)


  

