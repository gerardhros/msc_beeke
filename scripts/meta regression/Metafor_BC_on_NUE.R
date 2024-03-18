#title: "Meta regression with Metafor"
#author: "Beeke von Felde"
#date: "2023-08-22"

#packages
#install.packages("metagear")

# Load libraries 
library(data.table)
library(metafor)
library(metagear)

# read data
library(readxl)
d0all <- read_excel("data/meta_regression/meta_regression_copy3.xlsx", sheet = "meta_regression")
d0all <- as.data.table(d0all)


#add coefficients from lon and lat
library(readr)
dcoe <- read_csv("data/meta_regression/coefficients.csv")
library(data.table)
dcoe <- as.data.table(dcoe)

library(dplyr)

d0all$lon <- as.numeric(as.character(d0all$lon))
d0all$lat <- as.numeric(as.character(d0all$lat))
dcoe$lon <- as.numeric(as.character(dcoe$lon))
dcoe$lat <- as.numeric(as.character(dcoe$lat))

# Convert 'lon' and 'lat' to character type in both data frames
d0all$lon <- as.character(d0all$lon)
d0all$lat <- as.character(d0all$lat)
dcoe$lon <- as.character(dcoe$lon)
dcoe$lat <- as.character(dcoe$lat)

# Perform a full join (so "unknown" is also included)
d1all <- full_join(d0all, dcoe, by = c('lon', 'lat', 'studyid'))

# merging
#d1 = merge(d0, dcoe, by = c('lon', 'lat', 'studyid'), all.x = TRUE)

d1all <- as.data.table(d1all)


# For rows where 'rainfall (mm)' is NA and 'pre_mean' is not NA, copy 'pre_mean' to 'rainfall (mm)'
d1all$`rainfall (mm)`[is.na(d1all$`rainfall (mm)`) & !is.na(d1all$pre_mean)] <- d1all$pre_mean[is.na(d1all$`rainfall (mm)`) & !is.na(d1all$pre_mean)]

d1all$`Bulk_density (g/cm3)`[is.na(d1all$`Bulk_density (g/cm3)`) & !is.na(d1all$'BD / 100')] <- d1all$'BD / 100'[is.na(d1all$`Bulk_density (g/cm3)`) & !is.na(d1all$'BD / 100')]

d1all$'phw ALL' <- rowMeans(d1all[, c("phw_mean_0_5", "phw_mean_15_30", "phw_mean_5_15")], na.rm = TRUE)
d1all$`phw ALL`<- d1all$`phw ALL` /10
d1all$`S_pH(water)`[is.na(d1all$`S_pH(water)`) & !is.na(d1all$`phw ALL`)] <- d1all$`phw ALL`[is.na(d1all$`S_pH(water)`) & !is.na(d1all$`phw ALL`)]

# Divide all values in the column "clay ALL" and "sand ALL" by 10
d1all$`clay ALL` <- d1all$`clay ALL` / 10
d1all$`sand ALL` <- d1all$`sand ALL` / 10

# clean up column names
d1all <- as.data.table(d1all)
setnames(d1all,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d1all))))
setnames(d1all,tolower(colnames(d1all)))

library(dplyr)
d1all <- d1all %>%
  select(
    -id, -bdod_mean_0_5, -bdod_mean_15_30, -bdod_mean_5_15, -bd, -bd_100,
    -cec_mean_0_5, -cec_mean_15_30, -cec_mean_5_15, -clay_mean_0_5, -clay_mean_15_30,
    -clay_mean_5_15, -ntot_mean_0_5, -ntot_mean_15_30, -ntot_mean_5_15, -tot_n,
    -tot_n_100, -phw_mean_0_5, -phw_mean_15_30, -phw_mean_5_15, -sand_mean_0_5,
    -sand_mean_15_30, -sand_mean_5_15, -silt_mean_0_5, -silt_mean_15_30,
    -silt_mean_5_15, -soc_mean_0_5, -soc_mean_15_30, -soc_mean_5_15, -soc,
    -soc_100, -pre_mean, -pre_sd, -tmp_mean, -tmp_sd, -pet_mean, -pet_sd,
    -genzname, -genz, -gens, -phwall
  )

setnames(d1all,
         old = c("rainfallmm", "irrigation_amountmm", "n_fertilizerkg_ha", "p_fertilizerkg_ha",
                 "k_fertilizerkg_ha", "soil_texture", "bulk_densityg_cm3", "s_phwater", "s_socg_kg",
                 "s_tng_kg", "b_phwater", "b_totalcg_kg", "b_totalng_kg", "biochar_ratet_ha"),
         new = c("rain", "irr", "n_fer", "p_fer", "k_fer", "texture", "sbd", "sph", "soc", "stn",
                 "bph", "btc", "btn", "brate"),
         skip_absent = TRUE)

d1all$rain <- as.numeric(d1all$rain)
d1all$irr <- as.numeric(d1all$irr)
d1all$n_fer <- as.numeric(d1all$n_fer)
d1all$p_fer <- as.numeric(d1all$p_fer)
d1all$k_fer <- as.numeric(d1all$k_fer)
d1all$sbd <- as.numeric(d1all$sbd)
d1all$sph <- as.numeric(d1all$sph)
d1all$soc <- as.numeric(d1all$soc)
d1all$stn <- as.numeric(d1all$stn)
d1all$bph <- as.numeric(d1all$bph)
d1all$btc <- as.numeric(d1all$btc)
d1all$btn <- as.numeric(d1all$btn)
d1all$brate <- as.numeric(d1all$brate)
d1all$clay <- as.numeric(d1all$clay)
d1all$sand <- as.numeric(d1all$sand)

# modify the unit for site properties:
# bc total carbon from % to g/kg
d1all[btc <= 100, btc := btc * 10]
# bc total nitrogen from % to g/kg
d1all[btn <= 2, btn := btn * 10]
#replace "unknown" to "rainfed" in the column "water_managemnt"
d1all[water_management == "unknown", water_management := "rainfed"]

#replace missing values with the median
d1all[is.na(rain), rain := median(d1all$rain, na.rm = TRUE)]
d1all[is.na(irr), irr := median(d1all$irr, na.rm = TRUE)]
d1all[is.na(n_fer), n_fer := median(d1all$n_fer, na.rm = TRUE)]
d1all[is.na(p_fer), p_fer := median(d1all$p_fer, na.rm = TRUE)]
d1all[is.na(k_fer), k_fer := median(d1all$k_fer, na.rm = TRUE)]
d1all[is.na(sbd), sbd := median(d1all$sbd, na.rm = TRUE)]
d1all[is.na(sph), sph := median(d1all$sph, na.rm = TRUE)]
d1all[is.na(soc), soc := median(d1all$soc, na.rm = TRUE)]
d1all[is.na(stn), stn := median(d1all$stn, na.rm = TRUE)]
d1all[is.na(bph), bph := median(d1all$bph, na.rm = TRUE)]
d1all[is.na(btc), btc := median(d1all$btc, na.rm = TRUE)]
d1all[is.na(btn), btn := median(d1all$btn, na.rm = TRUE)]
d1all[is.na(brate), brate := median(d1all$brate, na.rm = TRUE)]
d1all[is.na(clay), clay := median(d1all$clay,na.rm=TRUE)]
d1all[is.na(sand), sand := median(d1all$sand,na.rm=TRUE)]


library(writexl)
file_path <- "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/merged_data_ALL.xlsx"
write_xlsx(d1all, file_path)

#_______________________________________________________________________________
#   NUE
#_______________________________________________________________________________

library(readxl)
d2nue <- read_excel("data/meta_regression/merged_data_ALL.xlsx")
d2nue <- as.data.table(d2nue)

#remove rows with NA in the "NUEC_mean" and "NUER_mean"
d2nue <- d2nue[!is.na(d2nue$nuec_mean), ]
d2nue <- d2nue[!is.na(d2nue$nuer_mean), ]

#remove all NUE above 100 and multiply the values below 1 with 100 (because it is likely a fraction)
d2nue <- d2nue[nuec_mean <= 100]
d2nue <- d2nue[nuer_mean <= 100]
d2nue[nuec_mean < 1, nuec_mean := nuec_mean * 100]
d2nue[nuer_mean < 1, nuer_mean := nuer_mean * 100]

#combine "millet" and "sorghum" to sorghum
d2nue$crop <- ifelse(d2nue$crop %in% c('millet', 'sorghum'), 'sorghum', d2nue$crop)

d2nue <- d2nue %>% 
  select(
    -yc_mean, -yc_sd, -yc_n, -yr_mean, -yr_sd, -yr_n,
    -socc_mean, -socc_sd, -socc_n, -socr_mean, -socr_sd, -socr_n,
    -phc_mean, -phc_sd, -phc_n, -phr_mean, -phr_sd, -phr_n,
    -sbdc_mean, -sbdc_sd, -sbdc_n, -sbdr_mean, -sbdr_sd, -sbdr_n
  )


#_______________________________________________________________________________

##scaling of the variables to unit variance
d2nue[, rain_scaled := scale(rain)]
d2nue[, irr_scaled := scale(irr)]
d2nue[, n_fer_scaled := scale(n_fer)]
d2nue[, p_fer_scaled := scale(p_fer)]
d2nue[, k_fer_scaled := scale(k_fer)]
d2nue[, sbd_scaled := scale(sbd)]
d2nue[, sph_scaled := scale(sph)]
d2nue[, soc_scaled := scale(soc)]
d2nue[, stn_scaled := scale(stn)]
d2nue[, bph_scaled := scale(bph)]
d2nue[, btc_scaled := scale(btc)]
d2nue[, btn_scaled := scale(btn)]
d2nue[, brate_scaled := scale(brate)]
d2nue[, clay_scaled := scale(clay)]
d2nue[, sand_scaled := scale(sand)]

# Supplement the SD when missing for NUE

# Identify missing SD values for NUEC_SD
missing_sd_nuer <- is.na(d2nue$nuer_sd) | d2nue$nuer_sd == ""
missing_sd_nuec <- is.na(d2nue$nuec_sd) | d2nue$nuec_sd == ""

# Calculate mean CV for non-missing values for NUEC_SD
cv_nuer <- mean(d2nue$nuer_sd[!missing_sd_nuer] / d2nue$nuer_mean[!missing_sd_nuer], na.rm = TRUE)
cv_nuec <- mean(d2nue$nuec_sd[!missing_sd_nuec] / d2nue$nuec_mean[!missing_sd_nuec], na.rm = TRUE)

# Impute missing SD values based on the mean CV for NUEC_SD
d2nue$nuer_sd[missing_sd_nuer] <- d2nue$nuer_mean[missing_sd_nuer] * 1.25 * cv_nuer
d2nue$nuec_sd[missing_sd_nuec] <- d2nue$nuec_mean[missing_sd_nuec] * 1.25 * cv_nuec

# Output the data to a CSV file
fwrite(d2nue, file = "data/meta_regression/NUE/d2nue.csv")

#_______________________________________________________________________________

## Estimate meta-analytical response measure (SMD Method)
# calculate effect size 

library(metafor)

es1nue <- escalc(measure = "SMD", data = d2nue, 
                m1i = nuer_mean, sd1i = nuer_sd, n1i = nuer_n,
                m2i = nuec_mean, sd2i = nuec_sd, n2i = nuec_n)
fwrite(es1nue, file = "data/meta_regression/NUE/es1nue.csv")

#_______________________________________________________________________________  

# make forest plots per group treatments
## no category "fruits" and "other"
## too little data point for "legumes" and "tubers"

hist_grain_nuec <- hist(d2nue$nuec_mean[d2nue$crop_type == "grain"])
hist_grain_nuer <- hist(d2nue$nuer_mean[d2nue$crop_type == "grain"])

hist_industrial_nuec <- hist(d2nue$nuec_mean[d2nue$crop_type == "industrial"])
hist_industrial_nuer <- hist(d2nue$nuer_mean[d2nue$crop_type == "industrial"])

hist_vegetable_nuec <- hist(d2nue$nuec_mean[d2nue$crop_type == "vegetable"])
hist_vegetable_nuer <- hist(d2nue$nuer_mean[d2nue$crop_type == "vegetable"])

hist_sbd <- hist(d2nue$nuec_mean[d2nue$sbd])
hist_stn <- hist(d2nue$nuec_mean[d2nue$stn])
hist_soc <- hist(d2nue$nuec_mean[d2nue$soc])
hist_sph <- hist(d2nue$nuec_mean[d2nue$sph])
hist_btn <- hist(d2nue$nuec_mean[d2nue$btn])
hist_btc <- hist(d2nue$nuec_mean[d2nue$btc])
hist_bph <- hist(d2nue$nuec_mean[d2nue$bph])
hist_brate <- hist(d2nue$nuec_mean[d2nue$brate])
hist_clay <- hist(d2nue$nuec_mean[d2nue$clay])
hist_sand <- hist(d2nue$nuec_mean[d2nue$sand])
hist_irr <- hist(d2nue$nuec_mean[d2nue$irr])
hist_rain <- hist(d2nue$nuec_mean[d2nue$rain])
hist_n_fer <- hist(d2nue$nuec_mean[d2nue$n_fer])
hist_p_fer <- hist(d2nue$nuec_mean[d2nue$p-fer])
hist_k_fer <- hist(d2nue$nuec_mean[d2nue$k_fer])
#texture, water management

#_______________________________________________________________________________

es1nue = as.data.table(es1nue)
es1nue = es1nue[abs(yi)<=10]

##ggplot for field studies and crop_type == "grain"
es1nue_grain = es1nue[!is.na(yi) & crop_type == "grain"]
setorder(es1nue_grain, -yi)
es1nue_grain[, id := .I]
es1nue_grain[, yi_cor := (exp(yi) - 1) * 100]
es1nue_grain[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pnuegrain <- ggplot(data = es1nue_grain, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 0.9) + 
  theme_bw() + ylim(-10, 10) +
  ggtitle('NUE response to biochar addition on grain') + 
  xlab("study-id") + 
  ylab("log response ratio")
pnuegrain
ggsave(plot = pnuegrain, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/pnuegrain.jpg', 
       width = 20, height = 7, unit = 'cm')


# ggplot for field studies and crop_type == "veg"

es1nue_veg = es1nue[!is.na(yi) & crop_type == "vegetable"]
setorder(es1nue_veg, -yi)
es1nue_veg[, id := .I]
es1nue_veg[, yi_cor := (exp(yi) - 1) * 100]
es1nue_veg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pnueveg <- ggplot(data = es1nue_veg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on vegetables') + 
  xlab("study-id") + 
  ylab("log response ratio")
pnueveg
ggsave(plot=pnueveg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/pnueveg.jpg',
       width = 15, height = 7,unit='cm')

# ggplot for field studies and all crop_types

es1nue_all = es1nue[!is.na(yi)]
setorder(es1nue_all, -yi)
es1nue_all[, id := .I]
es1nue_all[, yi_cor := (exp(yi) - 1) * 100]
es1nue_all[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pnueall <- ggplot(data = es1nue_all, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on all crop types)') + 
  xlab("study-id") + 
  ylab("log response ratio")
pnueall
ggsave(plot=pnueall, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/pnueall.jpg',
       width = 15, height = 7,unit='cm')


#_______________________________________________________________________________

#Meta-regression for main factors
##main factor analysis for log response ratio for yield

d3nue <- copy(es1nue)

# Convert data frame to data.table
library(data.table)
d3nue <- as.data.table(d3nue)

d3nue[,crop2 := crop]
d3nue[crop %in% c('oats', 'barley'), crop2 := 'wheat']

# what are the factors to be evaluated
var.site <- c("rain_scaled", "irr_scaled", "texture", "clay_scaled", "sand_scaled", 
              "water_management", "sbd_scaled", "sph_scaled", "soc_scaled", "stn_scaled")
var.crop <- c("crop2", "crop_type", "n_fer_scaled", "p_fer_scaled", "k_fer_scaled")
var.bc <- c("bph_scaled", "btc_scaled", "btn_scaled", "brate_scaled")
# i select only one example

# the columns to be assessed
var.sel <- c(var.site,var.crop,var.bc)

# run without a main factor selection to estimate overall mean
rnue_0 <- rma.mv(yi, vi, data = d3nue, random = list(~ 1|studyid), method = "REML", sparse = TRUE)


# objects to store the effects per factor as well summary stats of the meta-analytical models
out2.est = out2.sum = list()

# evaluate the impact of treatment (column rain) on yield given site properties
for(i in var.sel){
  
  # check whether the column is a numeric or categorical variable
  vartype = is.character(d3nue[, get(i)])
  
  # run with the main factor treatment
  if (vartype) {
    # run a meta-regression model for main categorical variable
    rnue_1 <- rma.mv(yi, vi, mods = ~factor(varsel) - 1, 
                   data = d3nue[, .(yi, vi, studyid, varsel = get(i))], 
                   random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  } else {
    # run a meta-regression model for main numerical variable
    rnue_1 <- rma.mv(yi, vi, mods = ~varsel, 
                   data = d3nue[, .(yi, vi, studyid, varsel = get(i))], 
                   random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  }
  
  # save output in a list: the estimated impact of the explanatory variable
  out2.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)', '', rownames(rnue_1$b)),
                              mean = round(as.numeric(rnue_1$b), 3),
                              se = round(as.numeric(rnue_1$se), 3),
                              ci.lb = round(as.numeric(rnue_1$ci.lb), 3),
                              ci.ub = round(as.numeric(rnue_1$ci.ub), 3),
                              pval = round(as.numeric(rnue_1$pval), 3))
  
  # save output in a list: the summary stats collected
  out2.sum[[i]] <- data.table(var = i,
                              AIC = rnue_1$fit.stats[4, 2],
                              ll = rnue_1$fit.stats[1, 2],
                              ll_impr = round(100 * (1 - rnue_1$fit.stats[1, 2] / rnue_0$fit.stats[1, 2]), 2),
                              r2_impr = round(100 * max(0, (sum(rnue_0$sigma2) - sum(rnue_1$sigma2)) / sum(rnue_0$sigma2)), 2),
                              pval = round(anova(rnue_1, rnue_0)$pval, 3)
  )
  
}

# merge output into a data.table
out2.sum <- rbindlist(out2.sum)
out2.est <- rbindlist(out2.est)
print(out2.sum)
print(out2.est)
# save out.sum for supporting information

out2.est_wi <- out2.est[varname != 'intrcpt']
library(openxlsx)
write.xlsx(out2.est_wi, file = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/NUE/out2_est_wi.xlsx")

#_______________________________________________________________________________

##visualize out.est - make plots for each coefficient 

#earthtone_colors <- c(
  "darkred", "darkorange", "rosybrown", "olivedrab", "chocolate",
  "saddlebrown", "darkgoldenrod", "maroon", "peru", "sienna", 
  "brown", "darkolivegreen", "lightsalmon", "tan", "goldenrod", 
  "coral", "tomato", "sandybrown", "firebrick", "indianred", 
  "darksalmon"
)

library(ggplot2)

#crop type______
crop_type_data <- out2.est[var == "crop_type"]
bar_crop_type_nue <- ggplot(crop_type_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "Crop Type", y = "Relative change in NUE", 
       title = "SMD Response on NUE by Crop Type due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_type_nue
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/SMD_bar_crop_type_nue.jpg", 
       plot = bar_crop_type_nue, 
       width = 20, height = 10, units = "cm")


#crop______

crop_data <- out2.est[var == "crop2" & varname != "knol khol"]
bar_crop_nue <- ggplot(crop_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity" , fill = "grey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "", y = "Relative change in NUE", 
       title = "SMD Response on NUE by Crops due to Biochar application") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 0.5, size = 10),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_nue
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/SMD_bar_crop_nue.jpg", 
       plot = bar_crop_nue, 
       width = 20, height = 7, units = "cm")


#soil texture______

texture_data <- out2.est[var == "texture"]
bar_texture_nue <- ggplot(texture_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Soil Texture", y = "Relative change in NUE", 
       title = "SMD Response on NUE by Soil Texture due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_texture_nue
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/SMD_bar_texture_nue.jpg", 
       plot = bar_texture_nue, 
       width = 20, height = 10, units = "cm")

#water_management______

water_management_data <- out2.est[var == "water_management"]
bar_water_management_nue <- ggplot(water_management_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity", fill ="grey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "Water Management", y = "Relative change in NUE", 
       title = "SMD Response by Water Management due to Biochar application") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_water_management_nue
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/SMD_bar_water_management_nue.jpg", 
       plot = bar_water_management_nue, 
       width = 20, height = 10, units = "cm")

#_____

#numeric coefficients (scaled)
num_nue <- out2.est[varname == "varsel"]

num_nue$var <- gsub("_scaled", "", num_nue$var)

# Define the order of levels for the var factor
var_order <- c('clay', 'sand', 'sbd', 'sph', 'soc', 
               'stn', 'rain', 'irr', 'n_fer', 'p_fer', 
               'k_fer', 'bph', 'btc', 'btn', 'brate')

# Reorder the levels of the var factor
num_nue$var <- factor(num_nue$var, levels = var_order)
num_nue
bar_num_nue <- ggplot(num_nue, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity", fill = "dimgrey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "", y = "Relative change in NUE", 
       title = "SMD Response on NUE due to Biochar application") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))+
  annotate("rect", xmin = -Inf, xmax = 7.5, ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.2) + 
  annotate("rect", xmin = 7.5, xmax = 11.5, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.2) + 
  annotate("rect", xmin = 11.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) 
bar_num_nue
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/SMD_bar_num_nue.jpg", 
       plot = bar_num_nue, 
       width = 20, height = 6, units = "cm")

#_______________________________________________________________________________
#Meta-regression for main factors with interactions


# make a function to extract relevant model statistics
estats_nue <- function(model_new, model_base) {
  out <- data.table(
    AIC = model_new$fit.stats[4, 2],
    ll = model_new$fit.stats[1, 2],
    ll_impr = round(100 * (1 - model_new$fit.stats[1, 2] / model_base$fit.stats[1, 2]), 2),
    r2_impr = round(100 * max(0, (sum(model_base$sigma2) - sum(model_new$sigma2)) / sum(model_base$sigma2)), 2),
    pval = round(anova(model_new, model_base)$pval, 3)
  )
  return(out)
}

d4nue <- copy(d3nue)
# run without a main factor selection to estimate overall mean
rnue_0 <- rma.mv(yi,vi, data = d4nue,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# 1. make a simple meta-regression model without interaction but with more than one explanatory variable
d4nue[,crop_type3 := crop_type]
d4nue[crop_type %in% c('grain', 'industrial'), crop_type3 := 'grain_indus']

d4nue[,water3 := water_management]
d4nue[water_management %in% c('irrigation', 'irrigation and rain'), water3 := 'irr']
d4nue$water3 <- factor(d4nue$water3)
d4nue$water3 <- relevel(d4nue$water3, ref = "flooded")

rnue_1 <- rma.mv(yi,vi, 
               mods = ~ crop_type3 * clay_scaled + soc_scaled : btn_scaled + stn_scaled * n_fer_scaled + brate_scaled + sph_scaled  -1, 
               data = d4nue,
               random = list(~ 1|studyid), method="REML",sparse = TRUE) 
out = estats_nue(model_new = rnue_1,model_base = rnue_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(rnue_1)

summary_output_nue <- capture.output(summary(rnue_1))

# Specify the file path where you want to save the summary
file_path <- "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/nue/summary_nue.txt"

# Write the captured output to the file
writeLines(summary_output_nue, file_path)


#visualize

# Extract coefficient names from the model object
coeff_names_nue <- names(coef(rnue_1))
##remove "_scaled"
coeff_names_nue <- gsub("_scaled", "", coeff_names_nue)

# Renaming coefficients
coeff_names_nue <- gsub("crop_type3grain_indus", "grain&industrial", coeff_names_nue)
coeff_names_nue <- gsub("crop_type3vegetable", "vegetable", coeff_names_nue)
#coeff_names_nue <- gsub("water_managementirrigation", "w_irr", coeff_names_nue)
#coeff_names_nue <- gsub("water_managementirrigation and rain", "irr", coeff_names_nue)
#coeff_names_nue <- gsub("water_managementrainfed", "rain", coeff_names_nue)

# Create the data frame
summary_nue <- data.frame(
  Coefficients = coeff_names_nue,
  Estimate = summary(rnue_1)$b,
  PValue = summary(rnue_1)$pval
)
summary_nue$Significance <- ifelse(summary_nue$PValue < .001, '***',
                                     ifelse(summary_nue$PValue < .01, '**',
                                            ifelse(summary_nue$PValue < .05, '*',
                                                   ifelse(summary_nue$PValue < .1, '.', ' '))))

summary_nue$Coefficients <- gsub("&", "\n&", summary_nue$Coefficients)
summary_nue$Coefficients <- gsub(":", ":\n", summary_nue$Coefficients)


sum_nue <- ggplot(summary_nue, aes(x=reorder(Coefficients, Estimate), y=Estimate, fill=Significance)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Significance), vjust=1.5, color="black") +
  scale_fill_manual(values=c('***'='olivedrab', '**'='yellowgreen', '*'='gold', '.'='grey', ' ' = 'darksalmon')) +
  labs(title="Parameter Estimates with Significance Levels for NUE",
       x="", y="Parameter estimate") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        legend.position = "none")
sum_nue
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/NUE/sum_nue.jpg", 
       plot = sum_nue, 
       width = 20, height = 6, units = "cm")

