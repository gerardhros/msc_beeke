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
d0 <- read_excel("data/meta_regression/meta_regression_copy2.xlsx", sheet = "meta_regression")
d0 <- as.data.table(d0)

#_______________________________________________________________________________

#add coefficients from lon and lat

library(readr)
dcoe <- read_csv("data/meta_regression/coefficients.csv")
library(data.table)
dcoe <- as.data.table(dcoe)

library(dplyr)

#d0$lon <- as.numeric(as.character(d0$lon))
#d0$lat <- as.numeric(as.character(d0$lat))
#dcoe$lon <- as.numeric(as.character(dcoe$lon))
#dcoe$lat <- as.numeric(as.character(dcoe$lat))

# Convert 'lon' and 'lat' to character type in both data frames
d0$lon <- as.character(d0$lon)
d0$lat <- as.character(d0$lat)
dcoe$lon <- as.character(dcoe$lon)
dcoe$lat <- as.character(dcoe$lat)

# Perform a full join (so "unknown" is also included)
d1 <- full_join(d0, dcoe, by = c('lon', 'lat', 'studyid'))

# merging
#d1 = merge(d0, dcoe, by = c('lon', 'lat', 'studyid'), all.x = TRUE)

d1 <- as.data.table(d1)


# For rows where 'rainfall (mm)' is NA and 'pre_mean' is not NA, copy 'pre_mean' to 'rainfall (mm)'
d1$`rainfall (mm)`[is.na(d1$`rainfall (mm)`) & !is.na(d1$pre_mean)] <- d1$pre_mean[is.na(d1$`rainfall (mm)`) & !is.na(d1$pre_mean)]

d1$`Bulk_density (g/cm3)`[is.na(d1$`Bulk_density (g/cm3)`) & !is.na(d1$'BD / 100')] <- d1$'BD / 100'[is.na(d1$`Bulk_density (g/cm3)`) & !is.na(d1$'BD / 100')]

d1$'phw ALL' <- rowMeans(d1[, c("phw_mean_0_5", "phw_mean_15_30", "phw_mean_5_15")], na.rm = TRUE)
d1$`phw ALL`<- d1$`phw ALL` /10
d1$`S_pH(water)`[is.na(d1$`S_pH(water)`) & !is.na(d1$`phw ALL`)] <- d1$`phw ALL`[is.na(d1$`S_pH(water)`) & !is.na(d1$`phw ALL`)]

# Divide all values in the column "clay ALL" and "sand ALL" by 10
d1$`clay ALL` <- d1$`clay ALL` / 10
d1$`sand ALL` <- d1$`sand ALL` / 10

#remove rows with NA in the "YC_mean"
d1 <- d1[!is.na(d1$YC_mean), ]

library(writexl)
file_path <- "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/yield/d1_merged_data_yield.xlsx"
write_xlsx(d1, file_path)

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
                 "s_tng_kg", "b_phwater", "b_totalcg_kg", "b_totalng_kg", "biochar_ratet_ha"),
         new = c("rain", "irr", "n_fer", "p_fer", "k_fer", "texture", "sbd", "sph", "soc", "stn",
                 "bph", "btc", "btn", "brate"),
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
d2$bph <- as.numeric(d2$bph)
d2$btc <- as.numeric(d2$btc)
d2$btn <- as.numeric(d2$btn)
d2$brate <- as.numeric(d2$brate)

# modify the unit for site properties:
# bc total carbon from % to g/kg
d2[btc <= 100, btc := btc * 10]
# bc total nitrogen from % to g/kg
d2[btn <= 2, btc := btc * 10]


# modify the unit for field studies and grain to kg/ha:

#d2[crop_type == "grain" & yc_mean <= 100, yc_sd := yc_sd * 1000]
#d2[crop_type == "grain" & yc_mean <= 100, yc_mean := yc_mean * 1000]
#d2[crop_type == "grain" & yr_mean <= 100, yr_sd := yr_sd * 1000]
#d2[crop_type == "grain" & yr_mean <= 100, yr_mean := yr_mean * 1000]

#d2[crop_type == "vegetable" & yc_mean <= 100, yc_sd := yc_sd * 1000]
#d2[crop_type == "vegetable" & yc_mean <= 100, yc_mean := yc_mean * 1000]
#d2[crop_type == "vegetable" & yr_mean <= 100, yr_sd := yr_sd * 1000]
#d2[crop_type == "vegetable" & yr_mean <= 100, yr_mean := yr_mean * 1000]

#d2[crop_type == "vegetable" & yc_mean <= 300, yc_sd := yc_sd * 100]
#d2[crop_type == "vegetable" & yc_mean <= 300, yc_mean := yc_mean * 100]
#d2[crop_type == "vegetable" & yr_mean <= 300, yr_sd := yr_sd * 100]
#d2[crop_type == "vegetable" & yr_mean <= 300, yr_mean := yr_mean * 100]

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
d2[is.na(bph), bph := median(d2$bph,na.rm=TRUE)]
d2[is.na(btc), btc := median(d2$btc,na.rm=TRUE)]
d2[is.na(btn), btn := median(d2$btn,na.rm=TRUE)]
d2[is.na(brate), brate := median(d2$brate,na.rm=TRUE)]

#_______________________________________________________________________________

##scaling of the variables to unit variance

d3 <- copy(d2)

#numeric values

#check classes
#str(d3) 

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
d3[, bph_scaled := scale(bph)]
d3[, btc_scaled := scale(btc)]
d3[, btn_scaled := scale(btn)]
d3[, brate_scaled := scale(brate)]


fwrite(d3, file = "data/meta_regression/NUE/d3.csv")

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

#output 
fwrite(d2, file = "data/meta_regression/NUE/d2.csv")

#_______________________________________________________________________________

## Estimate meta-analytical response measure (MD Method)

# calculate effect size 

library(metafor)

esnue <- escalc(measure = "SMD", data = d2, 
                  m1i = nuer_mean, sd1i = nuer_sd, n1i = nuer_n,
                  m2i = nuec_mean, sd2i = nuec_sd, n2i = nuec_n )
fwrite(esnue, file = "data/meta_regression/NUE/esnue.csv")

#_______________________________________________________________________________  

# make forest plots per group treatments

##histogram pro crop type:

hist_grain <- hist(d2$nuec_mean[d2$crop_type == "grain"])
hist_grain <- hist(d2$nuer_mean[d2$crop_type == "grain"])

hist_fruit <- hist(d2$nuec_mean[d2$crop_type == "fruit"])
hist_fruit <- hist(d2$nuer_mean[d2$crop_type == "fruit"])

hist_industrial <- hist(d2$nuec_mean[d2$crop_type == "industrial"])
hist_industrial <- hist(d2$nuer_mean[d2$crop_type == "industrial"])  

hist_legumes <- hist(d2$nuec_mean[d2$crop_type == "legumes"])
hist_legumes <- hist(d2$nuer_mean[d2$crop_type == "legumes"])  

hist_tubers <- hist(d2$nuec_mean[d2$crop_type == "tubers"])
hist_tubers <- hist(d2$nuer_mean[d2$crop_type == "tubers"])

hist_vegetable <- hist(d2$nuec_mean[d2$crop_type == "vegetable"])
hist_vegetable <- hist(d2$nuer_mean[d2$crop_type == "vegetable"])

hist_others <- hist(d2$nuec_mean[d2$crop_type == "others"])
hist_others <- hist(d2$nuer_mean[d2$crop_type == "others"])

#_______________________________________________________________________________

##ggplot for field studies and crop_type == "grain"

esnue = as.data.table(es21y)
#filter the data set and only select the data where yi is missing
esnue_grain = esnue[!is.na(yi) & crop_type == "grain"]
setorder(esnue_grain,-yi)
esnue_grain[,id:=.I]
esnue_grain[,yi_cor := (exp(yi)-1)*100]
esnue_grain[,vi_cor := (exp(vi)-1)*100]
library(ggplot2)
pgrain <- ggplot(data = esnue_grain, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 0.9) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on grain') + 
  xlab("study-id") + 
  ylab("log response ratio")
#show and save plot
pgrain
ggsave(plot=pgrain, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/pgrain.jpg', 
       width = 20, height = 7,unit='cm')


##ggplot for field studies and crop_type == "fruit"

esnue = as.data.table(esnue)
esnue_fruit = esnue[!is.na(yi) & crop_type == "fruit"]
setorder(esnue_fruit, -yi)
esnue_fruit[, id := .I]
esnue_fruit[, yi_cor := (exp(yi) - 1) * 100]
esnue_fruit[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pfruit <- ggplot(data = esnue_fruit, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on fruit') + 
  xlab("Study-ID") + 
  ylab("Log Response Ratio")
pfruit
ggsave(plot = pfruit, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/pfruit.jpg',
       width = 15, height = 7, unit = 'cm')


# ggplot for field studies and crop_type == "veg"

esnue = as.data.table(esnue)
esnue_veg = esnue[!is.na(yi) & crop_type == "vegetable"]
setorder(esnue_veg, -yi)
esnue_veg[, id := .I]
esnue_veg[, yi_cor := (exp(yi) - 1) * 100]
esnue_veg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pveg <- ggplot(data = esnue_veg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on vegetables') + 
  xlab("Study-ID") + 
  ylab("Log Response Ratio")
pveg
ggsave(plot = pveg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/pveg.jpg',
       width = 15, height = 7, unit = 'cm')


# ggplot for field studies and crop_type == "legumes"

esnue = as.data.table(esnue)
# filter the data set and only select the data where yi is missing
esnue_leg = esnue[!is.na(yi) & crop_type == "legumes"]
setorder(esnue_leg, -yi)
esnue_leg[, id := .I]
esnue_leg[, yi_cor := (exp(yi) - 1) * 100]
esnue_leg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pleg <- ggplot(data = esnue_leg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on legumes') + 
  xlab("study-id") + 
  ylab("log response ratio")
pleg
ggsave(plot=pleg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/pleg.jpg',
       width = 15, height = 7,unit='cm')

# ggplot for field studies and crop_type == "tubers"

esnue = as.data.table(esnue)
esnue_tubers = esnue[!is.na(yi) & crop_type == "tubers"]
setorder(esnue_tubers, -yi)
esnue_tubers[, id := .I]
esnue_tubers[, yi_cor := (exp(yi) - 1) * 100]
esnue_tubers[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
ptub <- ggplot(data = esnue_tubers, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on tubers') + 
  xlab("Study-ID") + 
  ylab("Log Response Ratio")
ptub
ggsave(plot = ptub, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ptub.jpg',
       width = 15, height = 7, unit = 'cm')


# ggplot for field studies and crop_type == "others"

esnue = as.data.table(esnue)
esnue_others = esnue[!is.na(yi) & crop_type == "others"]
setorder(esnue_others, -yi)
esnue_others[, id := .I]
esnue_others[, yi_cor := (exp(yi) - 1) * 100]
esnue_others[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pother <- ggplot(data = esnue_others, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('NUE response to biochar addition on others 
          (Tea, Tobacco, Zizyphus, Tall Fescue, Cocoyam, and Pasture)') + 
  xlab("Study-ID") + 
  ylab("Log Response Ratio")
pother
ggsave(plot = pother, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/pother.jpg',
       width = 15, height = 7, unit = 'cm')




#_______________________________________________________________________________

#Meta-regression for main factors
##main factor analysis for log response ratio for yield

# Load the data.table package

d4 <- copy(esnue)

# Convert data frame to data.table
library(data.table)
d4 <- as.data.table(d4)

# what are the factors to be evaluated
var.site <- c("rain", "irr", "texture", "water_management", "sbd", "sph", "soc", "stn")
var.crop <- c("crop", "crop_type", "n_fer", "p_fer", "k_fer")
var.bc <- c("bph", "btc", "btn", "brate")

# i select only one example

# the columns to be assessed
var.sel <- c(var.site,var.crop,var.bc)

# run without a main factor selection to estimate overall mean
rnue_0 <- rma.mv(yi, vi, data = d4, random = list(~ 1|studyid), method = "REML", sparse = TRUE)


# objects to store the effects per factor as well summary stats of the meta-analytical models
out1.est = out1.sum = list()

# evaluate the impact of treatment (column rain) on yield given site properties
for(i in var.sel){
  
  # check whether the column is a numeric or categorical variable
  vartype = is.character(d4[, get(i)])
  
  # run with the main factor treatment
  if (vartype) {
    # run a meta-regression model for main categorical variable
    rnue_1 <- rma.mv(yi, vi, mods = ~factor(varsel) - 1, 
                   data = d4[, .(yi, vi, studyid, varsel = get(i))], 
                   random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  } else {
    # run a meta-regression model for main numerical variable
    rnue_1 <- rma.mv(yi, vi, mods = ~varsel, 
                   data = d4[, .(yi, vi, studyid, varsel = get(i))], 
                   random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  }
  
  # save output in a list: the estimated impact of the explanatory variable
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)', '', rownames(rnue_1$b)),
                              mean = round(as.numeric(rnue_1$b), 3),
                              se = round(as.numeric(rnue_1$se), 3),
                              ci.lb = round(as.numeric(rnue_1$ci.lb), 3),
                              ci.ub = round(as.numeric(rnue_1$ci.ub), 3),
                              pval = round(as.numeric(rnue_1$pval), 3))
  
  # save output in a list: the summary stats collected
  out1.sum[[i]] <- data.table(var = i,
                              AIC = rnue_1$fit.stats[4, 2],
                              ll = rnue_1$fit.stats[1, 2],
                              ll_impr = round(100 * (1 - rnue_1$fit.stats[1, 2] / rnue_0$fit.stats[1, 2]), 2),
                              r2_impr = round(100 * max(0, (sum(rnue_0$sigma2) - sum(rnue_1$sigma2)) / sum(rnue_0$sigma2)), 2),
                              pval = round(anova(rnue_1, rnue_0)$pval, 3)
  )
  
}

# merge output into a data.table
out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)
print(out1.sum)
print(out1.est)

#write.xlsx(out1.est, file = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/NUE/out1_est.xlsx")

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
crop_type_data <- out1.est[var == "crop_type"]
bar_crop_type <- ggplot(crop_type_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crop Type", y = "Mean", title = "Mean by Crop Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_type
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_crop_type.jpg", 
       plot = bar_crop_type, 
       width = 15, height = 8, units = "cm")


#crop______
crop_data <- out1.est[var == "crop"]
bar_crop <- ggplot(crop_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crops", y = "Mean", title = "Mean by Crops") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_crop.jpg", 
       plot = bar_crop, 
       width = 15, height = 8, units = "cm")

#soil texture______

texture_data <- out1.est[var == "texture"]
bar_texture <- ggplot(texture_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Soil Texture", y = "Mean", title = "Mean by Soil Texture") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"))
bar_texture
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_texture.jpg", 
       plot = bar_texture, 
       width = 15, height = 8, units = "cm")

#water_management______

water_management_data <- out1.est[var == "water_management" & varname != "unknown"]
bar_water_management <- ggplot(water_management_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Water Management", y = "Mean", title = "Mean by Water Management") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"))
bar_water_management
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_water_management.jpg", 
       plot = bar_water_management, 
       width = 15, height = 8, units = "cm")


#______________________________________________

#numeric coefficients (x=varname, y=value of the coefficients)

num <- out1.est[var %in% c('rain', 'irr', 'sbd', 'sph', 'soc', 'stn',
                           'n_fer', 'p_fer', 'k_fer', 'bph','btc','btn', 
                           'brate') & varname != 'intrcpt']
num
bar_num <- ggplot(num, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Variable", y = "Mean", title = "Mean by Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))
bar_num
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_num.jpg", 
       plot = bar_num, 
       width = 15, height = 8, units = "cm")
#_____

bcprop <- out1.est[var %in%  c('bph','btc','btn','brate') & varname != 'intrcpt']
bcprop
bar_bcprop <- ggplot(bcprop, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Variable", y = "Mean", title = "Effect of biochar properties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))
bar_bcprop
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_biocharproperties.jpg", 
       plot = bar_bcprop, 
       width = 15, height = 8, units = "cm")

#_____
climate <- out1.est[var %in% c('rain', 'irr', 'n_fer', 'p_fer', 'k_fer') & varname != 'intrcpt']
climate
bar_climate <- ggplot(climate, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Variable", y = "Mean", title = "Climate & fertilizer parameters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))
bar_climate
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_climate&fertilizer.jpg", 
       plot = bar_climate, 
       width = 15, height = 8, units = "cm")

#______

soil <- out1.est[var %in% c('sbd', 'sph', 'soc', 'stn') & varname != 'intrcpt']
bar_soil <- ggplot(soil, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Variable", y = "Mean", title = "Soil Parameters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))
bar_soil
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/SMD_bar_soil.jpg", 
       plot = bar_soil, 
       width = 15, height = 8, units = "cm")



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

d5y <- copy(d4y)
# run without a main factor selection to estimate overall mean
ry_0 <- rma.mv(yi,vi, data = d5y,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# 1. make a simple meta-regression model without interaction but with more than one explanatory variable

ry_1 <- rma.mv(yi,vi, 
               mods = ~texture * bph + water_management * sbd + crop_type -1, 
               data = d5y,
               random = list(~ 1|studyid), method="REML",sparse = TRUE) 
out = estats(model_new = ry_1,model_base = ry_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(ry_1)