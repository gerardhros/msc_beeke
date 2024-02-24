#title: "Meta regression with Metafor"
#author: "Beeke von Felde"
#date: "2023-08-22"

library(data.table)
library(metafor)
library(metagear)

#_______________________________________________________________________________
#   bulk density
#_______________________________________________________________________________

library(readxl)
d2bd <- read_excel("data/meta_regression/merged_data_ALL.xlsx")
d2bd <- as.data.table(d2bd)

#remove rows with NA in the "sbdc_mean" and "sbdr_mean"
d2bd <- d2bd[!is.na(d2bd$sbdc_mean), ]
d2bd <- d2bd[!is.na(d2bd$sbdr_mean), ]

#remove other responses
d2bd <- d2bd %>% 
  select(
    -yc_mean, -yc_sd, -yc_n, -yr_mean, -yr_sd, -yr_n,
    -socc_mean, -socc_sd, -socc_n, -socr_mean, -socr_sd, -socr_n,
    -phc_mean, -phc_sd, -phc_n, -phr_mean, -phr_sd, -phr_n,
    -nuec_mean, -nuec_sd, -nuec_n, -nuer_mean, -nuer_sd, -nuer_n
  )


#_______________________________________________________________________________

##scaling of the variables to unit variance
d2bd[, rain_scaled := scale(rain)]
d2bd[, irr_scaled := scale(irr)]
d2bd[, n_fer_scaled := scale(n_fer)]
d2bd[, p_fer_scaled := scale(p_fer)]
d2bd[, k_fer_scaled := scale(k_fer)]
d2bd[, sbd_scaled := scale(sbd)]
d2bd[, sph_scaled := scale(sph)]
d2bd[, soc_scaled := scale(soc)]
d2bd[, stn_scaled := scale(stn)]
d2bd[, bph_scaled := scale(bph)]
d2bd[, btc_scaled := scale(btc)]
d2bd[, btn_scaled := scale(btn)]
d2bd[, brate_scaled := scale(brate)]
d2bd[, clay_scaled := scale(clay)]
d2bd[, sand_scaled := scale(sand)]

# Supplement the SD when missing for bd

# Identify missing SD values for sbdc_sd
missing_sd_bdr <- is.na(d2bd$sbdr_sd) | d2bd$sbdr_sd == ""
missing_sd_bdc <- is.na(d2bd$sbdc_sd) | d2bd$sbdc_sd == ""

# Calculate mean CV for non-missing values for sbdc_sd
cv_bdr <- mean(d2bd$sbdr_sd[!missing_sd_bdr] / d2bd$sbdr_mean[!missing_sd_bdr], na.rm = TRUE)
cv_bdc <- mean(d2bd$sbdc_sd[!missing_sd_bdc] / d2bd$sbdc_mean[!missing_sd_bdc], na.rm = TRUE)

# Impute missing SD values based on the mean CV for sbdc_sd
d2bd$sbdr_sd[missing_sd_bdr] <- d2bd$sbdr_mean[missing_sd_bdr] * 1.25 * cv_bdr
d2bd$sbdc_sd[missing_sd_bdc] <- d2bd$sbdc_mean[missing_sd_bdc] * 1.25 * cv_bdc

# Output the data to a CSV file
fwrite(d2bd, file = "data/meta_regression/bd/d2bd.csv")

#_______________________________________________________________________________

## Estimate meta-analytical response measure (MD Method)
# calculate effect size 

library(metafor)

#Y
es1bd <- escalc(measure = "SMD", data = d2bd, 
                 m1i = sbdr_mean, sd1i = sbdr_sd, n1i = sbdr_n,
                 m2i = sbdc_mean, sd2i = sbdc_sd, n2i = sbdc_n)
fwrite(es1bd, file = "data/meta_regression/bd/es1bd.csv")

#_______________________________________________________________________________  

# make forest plots per group treatments
## no category "fruits" and "other"
## too little data point for "legumes" and "tubers"

hist_grain_bdc <- hist(d2bd$sbdc_mean[d2bd$crop_type == "grain"])
hist_grain_bdr <- hist(d2bd$sbdr_mean[d2bd$crop_type == "grain"])

hist_vegetable_bdc <- hist(d2bd$sbdc_mean[d2bd$crop_type == "vegetable"])
hist_vegetable_bdr <- hist(d2bd$sbdr_mean[d2bd$crop_type == "vegetable"])

hist_fruit <- hist(d2bd$sbdc_mean[d2bd$crop_type == "fruit"])
hist_fruit <- hist(d2bd$sbdr_mean[d2bd$crop_type == "fruit"])

hist_industrial <- hist(d2bd$sbdc_mean[d2bd$crop_type == "industrial"])
hist_industrial <- hist(d2bd$sbdr_mean[d2bd$crop_type == "industrial"])  

hist_legumes <- hist(d2bd$sbdc_mean[d2bd$crop_type == "legumes"])
hist_legumes <- hist(d2bd$sbdr_mean[d2bd$crop_type == "legumes"])  

hist_tubers <- hist(d2bd$sbdc_mean[d2bd$crop_type == "tubers"])
hist_tubers <- hist(d2bd$sbdr_mean[d2bd$crop_type == "tubers"])

hist_sbd <- hist(d2bd$sbdc_mean[d2bd$sbd])
hist_stn <- hist(d2bd$sbdc_mean[d2bd$stn])
hist_soc <- hist(d2bd$sbdc_mean[d2bd$soc])
hist_sph <- hist(d2bd$sbdc_mean[d2bd$sph])
hist_btn <- hist(d2bd$sbdc_mean[d2bd$btn])
hist_btc <- hist(d2bd$sbdc_mean[d2bd$btc])
hist_bph <- hist(d2bd$sbdc_mean[d2bd$bph])
hist_brate <- hist(d2bd$sbdc_mean[d2bd$brate])
hist_clay <- hist(d2bd$sbdc_mean[d2bd$clay])
hist_sand <- hist(d2bd$sbdc_mean[d2bd$sand])
hist_irr <- hist(d2bd$sbdc_mean[d2bd$irr])
hist_rain <- hist(d2bd$sbdc_mean[d2bd$rain])
hist_n_fer <- hist(d2bd$sbdc_mean[d2bd$n_fer])
hist_p_fer <- hist(d2bd$sbdc_mean[d2bd$p-fer])
hist_k_fer <- hist(d2bd$sbdc_mean[d2bd$k_fer])
#texture, water management

#_______________________________________________________________________________

es1bd = as.data.table(es1bd)
es1bd = es1bd[abs(yi)<=10]

##ggplot for field studies and crop_type == "grain"
es1bd_grain = es1bd[!is.na(yi) & crop_type == "grain"]
setorder(es1bd_grain, -yi)
es1bd_grain[, id := .I]
es1bd_grain[, yi_cor := (exp(yi) - 1) * 100]
es1bd_grain[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pbdgrain <- ggplot(data = es1bd_grain, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 0.9) + 
  theme_bw() + ylim(-10, 10) +
  ggtitle('bulk density response to biochar addition on grain') + 
  xlab("study-id") + 
  ylab("log response ratio")
pbdgrain
ggsave(plot = pbdgrain, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/pbdgrain.jpg', 
       width = 20, height = 7, unit = 'cm')


# ggplot for field studies and crop_type == "veg"

es1bd_veg = es1bd[!is.na(yi) & crop_type == "vegetable"]
setorder(es1bd_veg, -yi)
es1bd_veg[, id := .I]
es1bd_veg[, yi_cor := (exp(yi) - 1) * 100]
es1bd_veg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pbdveg <- ggplot(data = es1bd_veg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('bulk density response to biochar addition on vegetables') + 
  xlab("study-id") + 
  ylab("log response ratio")
pbdveg
ggsave(plot=pbdveg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/pbdveg.jpg',
       width = 15, height = 7,unit='cm')

# ggplot for field studies and all crop_types

es1bd_all = es1bd[!is.na(yi)]
setorder(es1bd_all, -yi)
es1bd_all[, id := .I]
es1bd_all[, yi_cor := (exp(yi) - 1) * 100]
es1bd_all[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pbdall <- ggplot(data = es1bd_all, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('bulk density response to biochar addition on all crop types)') + 
  xlab("study-id") + 
  ylab("log response ratio")
pbdall
ggsave(plot=pbdall, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/pbdall.jpg',
       width = 15, height = 7,unit='cm')


#_______________________________________________________________________________

#Meta-regression for main factors
##main factor analysis for log response ratio for yield

d3bd <- copy(es1bd)

# Convert data frame to data.table
library(data.table)
d3bd <- as.data.table(d3bd)

# what are the factors to be evaluated
var.site <- c("rain_scaled", "irr_scaled", "texture", "clay_scaled", "sand_scaled", 
              "water_management", "sbd_scaled", "sph_scaled", "soc_scaled", "stn_scaled")
var.crop <- c("crop", "crop_type", "n_fer_scaled", "p_fer_scaled", "k_fer_scaled")
var.bc <- c("bph_scaled", "btc_scaled", "btn_scaled", "brate_scaled")
# i select only one example

# the columns to be assessed
var.sel <- c(var.site,var.crop,var.bc)

# run without a main factor selection to estimate overall mean
rbd_0 <- rma.mv(yi, vi, data = d3bd, random = list(~ 1|studyid), method = "REML", sparse = TRUE)


# objects to store the effects per factor as well summary stats of the meta-analytical models
out2.est = out2.sum = list()

# evaluate the impact of treatment (column rain) on yield given site properties
for(i in var.sel){
  
  # check whether the column is a numeric or categorical variable
  vartype = is.character(d3bd[, get(i)])
  
  # run with the main factor treatment
  if (vartype) {
    # run a meta-regression model for main categorical variable
    rbd_1 <- rma.mv(yi, vi, mods = ~factor(varsel) - 1, 
                     data = d3bd[, .(yi, vi, studyid, varsel = get(i))], 
                     random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  } else {
    # run a meta-regression model for main numerical variable
    rbd_1 <- rma.mv(yi, vi, mods = ~varsel, 
                     data = d3bd[, .(yi, vi, studyid, varsel = get(i))], 
                     random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  }
  
  # save output in a list: the estimated impact of the explanatory variable
  out2.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)', '', rownames(rbd_1$b)),
                              mean = round(as.numeric(rbd_1$b), 3),
                              se = round(as.numeric(rbd_1$se), 3),
                              ci.lb = round(as.numeric(rbd_1$ci.lb), 3),
                              ci.ub = round(as.numeric(rbd_1$ci.ub), 3),
                              pval = round(as.numeric(rbd_1$pval), 3))
  
  # save output in a list: the summary stats collected
  out2.sum[[i]] <- data.table(var = i,
                              AIC = rbd_1$fit.stats[4, 2],
                              ll = rbd_1$fit.stats[1, 2],
                              ll_impr = round(100 * (1 - rbd_1$fit.stats[1, 2] / rbd_0$fit.stats[1, 2]), 2),
                              r2_impr = round(100 * max(0, (sum(rbd_0$sigma2) - sum(rbd_1$sigma2)) / sum(rbd_0$sigma2)), 2),
                              pval = round(anova(rbd_1, rbd_0)$pval, 3)
  )
  
}

# merge output into a data.table
out2.sum <- rbindlist(out2.sum)
out2.est <- rbindlist(out2.est)
print(out2.sum)
print(out2.est)
# save out.sum for supporting information
#library(data.table)
#data.table(out2.sum, caption = 'Summary Statistics - SMD')
#data.table(out2.est, caption = 'out.est - SMD')

#library(openxlsx)

#write.xlsx(out2.est, file = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/yield/out1_est.xlsx")

#_______________________________________________________________________________

##visualize out.est - make plots for each coefficient 

earthtone_colors <- c(
  "darkred", "darkorange", "rosybrown", "olivedrab", "chocolate",
  "saddlebrown", "darkgoldenrod", "maroon", "peru", "sienna", 
  "brown", "darkolivegreen", "lightsalmon", "tan", "goldenrod", 
  "coral", "tomato", "sandybrown", "firebrick", "indianred", 
  "darksalmon"
)

library(ggplot2)

#crop type______
crop_type_data <- out2.est[var == "crop_type"]
bar_crop_type_bd <- ggplot(crop_type_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crop Type", y = "Relative change in bd (%)", 
       title = "Standardized Mean Difference Response by Crop Type due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_type_bd
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/SMD_bar_crop_type_bd.jpg", 
       plot = bar_crop_type_bd, 
       width = 20, height = 10, units = "cm")


#crop______
crop_data <- out2.est[var == "crop"]
bar_crop_bd <- ggplot(crop_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crops", y = "Relative change in bd (%)", 
       title = "Standardized Mean Difference Response by Crops due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_bd
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/SMD_bar_crop_bd.jpg", 
       plot = bar_crop_bd, 
       width = 20, height = 10, units = "cm")

#soil texture______

texture_data <- out2.est[var == "texture"]
bar_texture_bd <- ggplot(texture_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Soil Texture", y = "Relative change in bd (%)", 
       title = "Standardized Mean Difference Response by Soil Texture due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_texture_bd
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/SMD_bar_texture_bd.jpg", 
       plot = bar_texture_bd, 
       width = 20, height = 10, units = "cm")

#water_management______

water_management_data <- out2.est[var == "water_management"]
bar_water_management_bd <- ggplot(water_management_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Water Management", y = "Relative change in bd (%)", 
       title = "Standardized Mean Difference Response by Water Management due to Biochar application") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_water_management_bd
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/SMD_bar_water_management_bd.jpg", 
       plot = bar_water_management_bd, 
       width = 20, height = 10, units = "cm")

#_____

#numeric coefficients (scaled)

num <- out2.est[var %in% c('clay_scaled', 'sand_scaled', 'sbd_scaled', 'sph_scaled', 'soc_scaled', 
                           'stn_scaled', 'rain_scaled', 'irr_scaled', 'n_fer_scaled', 'p_fer_scaled', 
                           'k_fer_scaled', 'bph_scaled', 'btc_scaled', 'btn_scaled', 'brate_scaled')
                & varname != 'intrcpt']
num$var <- factor(num$var, levels = c('clay_scaled', 'sand_scaled', 'sbd_scaled', 'sph_scaled', 'soc_scaled', 
                                      'stn_scaled', 'rain_scaled', 'irr_scaled', 'n_fer_scaled', 'p_fer_scaled', 
                                      'k_fer_scaled', 'bph_scaled', 'btc_scaled', 'btn_scaled', 'brate_scaled'))
num
bar_num_bd <- ggplot(num, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Variable", y = "Relative change in bd (%)", 
       title = "Standardized Mean Difference Response due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))+
  annotate("rect", xmin = -Inf, xmax = 7.5, ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.2) + 
  annotate("rect", xmin = 7.5, xmax = 11.5, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.2) + 
  annotate("rect", xmin = 11.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) 
bar_num_bd
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/bd/SMD_bar_num_bd.jpg", 
       plot = bar_num_bd, 
       width = 20, height = 10, units = "cm")

#_______________________________________________________________________________
#Meta-regression for main factors with interactions


# make a function to extract relevant model statistics
estats_bd <- function(model_new, model_base) {
  out <- data.table(
    AIC = model_new$fit.stats[4, 2],
    ll = model_new$fit.stats[1, 2],
    ll_impr = round(100 * (1 - model_new$fit.stats[1, 2] / model_base$fit.stats[1, 2]), 2),
    r2_impr = round(100 * max(0, (sum(model_base$sigma2) - sum(model_new$sigma2)) / sum(model_base$sigma2)), 2),
    pval = round(anova(model_new, model_base)$pval, 3)
  )
  return(out)
}

d4bd <- copy(d3bd)
# run without a main factor selection to estimate overall mean
rbd_0 <- rma.mv(yi,vi, data = d4bd,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# 1. make a simple meta-regression model without interaction but with more than one explanatory variable

rbd_1 <- rma.mv(yi,vi, 
                 mods = ~texture * bph + water_management * sph + k_fer + brate -1, 
                 data = d4bd,
                 random = list(~ 1|studyid), method="REML",sparse = TRUE) 
out = estats_bd(model_new = rbd_1,model_base = rbd_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(rbd_1)


# from first check i see that some crop types behave similarly, so i combine them
d4bd[,crtype2 := crop_type]
d4bd[crop_type %in% c('grain', 'industrial', 'legumes'), crtype2 := 'grouped']
