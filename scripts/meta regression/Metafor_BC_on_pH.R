#title: "Meta regression with Metafor"
#author: "Beeke von Felde"
#date: "2023-08-22"

library(data.table)
library(metafor)
library(metagear)

#_______________________________________________________________________________
#   pH
#_______________________________________________________________________________

library(readxl)
d2ph <- read_excel("data/meta_regression/merged_data_ALL.xlsx")
d2ph <- as.data.table(d2ph)

#remove rows with NA in the "phc_mean" and "phr_mean"
d2ph <- d2ph[!is.na(d2ph$phc_mean), ]
d2ph <- d2ph[!is.na(d2ph$phr_mean), ]

# Replace NA values in phr_n and phc_n with 2
library(dplyr)
d2ph <- d2ph %>%
  mutate(
    phr_n = ifelse(is.na(phr_n), 2, phr_n),
    phc_n = ifelse(is.na(phc_n), 2, phc_n)
  )

#remove other responses
d2ph <- d2ph %>% 
  select(
    -yc_mean, -yc_sd, -yc_n, 
    -yr_mean, -yr_sd, -yr_n,
    -socc_mean, -socc_sd, -socc_n, 
    -socr_mean, -socr_sd, -socr_n,
    -sbdc_mean, -sbdc_sd, -sbdc_n, 
    -sbdr_mean, -sbdr_sd, -sbdr_n,
    -nuec_mean, -nuec_sd, -nuec_n,
    -nuer_mean, -nuer_sd, -nuer_n
  )


#_______________________________________________________________________________

##scaling of the variables to unit variance
d2ph[, rain_scaled := scale(rain)]
d2ph[, irr_scaled := scale(irr)]
d2ph[, n_fer_scaled := scale(n_fer)]
d2ph[, p_fer_scaled := scale(p_fer)]
d2ph[, k_fer_scaled := scale(k_fer)]
d2ph[, sbd_scaled := scale(sbd)]
d2ph[, sph_scaled := scale(sph)]
d2ph[, soc_scaled := scale(soc)]
d2ph[, stn_scaled := scale(stn)]
d2ph[, bph_scaled := scale(bph)]
d2ph[, btc_scaled := scale(btc)]
d2ph[, btn_scaled := scale(btn)]
d2ph[, brate_scaled := scale(brate)]
d2ph[, clay_scaled := scale(clay)]
d2ph[, sand_scaled := scale(sand)]

# Supplement the SD when missing for NUE

# Identify missing SD values for phc_SD
missing_sd_phr <- is.na(d2ph$phr_sd) | d2ph$phr_sd == ""
missing_sd_phc <- is.na(d2ph$phc_sd) | d2ph$phc_sd == ""

# Calculate mean CV for non-missing values for phc_SD
cv_phr <- mean(d2ph$phr_sd[!missing_sd_phr] / d2ph$phr_mean[!missing_sd_phr], na.rm = TRUE)
cv_phc <- mean(d2ph$phc_sd[!missing_sd_phc] / d2ph$phc_mean[!missing_sd_phc], na.rm = TRUE)

# Impute missing SD values based on the mean CV for phc_SD
d2ph$phr_sd[missing_sd_phr] <- d2ph$phr_mean[missing_sd_phr] * 1.25 * cv_phr
d2ph$phc_sd[missing_sd_phc] <- d2ph$phc_mean[missing_sd_phc] * 1.25 * cv_phc

# Output the data to a CSV file
fwrite(d2ph, file = "data/meta_regression/ph/d2ph.csv")

#_______________________________________________________________________________

## Estimate meta-analytical response measure (SMD Method)
# calculate effect size 

library(metafor)
es1ph <- escalc(measure = "SMD", data = d2ph, 
                 m1i = phr_mean, sd1i = phr_sd, n1i = phr_n,
                 m2i = phc_mean, sd2i = phc_sd, n2i = phc_n)
es1ph=as.data.table(es1ph)
es1ph=es1ph[yi<=10]
fwrite(es1ph, file = "data/meta_regression/ph/es1ph.csv")

#_______________________________________________________________________________  

# make forest plots per group treatments
## no category "fruits", "industrial" and "other"
## too little data point for "legumes" and "tubers"

hist_grain_phc <- hist(d2ph$phc_mean[d2ph$crop_type == "grain"])
hist_grain_phr <- hist(d2ph$phr_mean[d2ph$crop_type == "grain"])

hist_vegetable_phc <- hist(d2ph$phc_mean[d2ph$crop_type == "vegetable"])
hist_vegetable_phr <- hist(d2ph$phr_mean[d2ph$crop_type == "vegetable"])

hist_sbd <- hist(d2ph$phc_mean[d2ph$sbd])
hist_stn <- hist(d2ph$phc_mean[d2ph$stn])
hist_soc <- hist(d2ph$phc_mean[d2ph$soc])
hist_sph <- hist(d2ph$phc_mean[d2ph$sph])
hist_btn <- hist(d2ph$phc_mean[d2ph$btn])
hist_btc <- hist(d2ph$phc_mean[d2ph$btc])
hist_bph <- hist(d2ph$phc_mean[d2ph$bph])
hist_brate <- hist(d2ph$phc_mean[d2ph$brate])
hist_clay <- hist(d2ph$phc_mean[d2ph$clay])
hist_sand <- hist(d2ph$phc_mean[d2ph$sand])
hist_irr <- hist(d2ph$phc_mean[d2ph$irr])
hist_rain <- hist(d2ph$phc_mean[d2ph$rain])
hist_n_fer <- hist(d2ph$phc_mean[d2ph$n_fer])
hist_p_fer <- hist(d2ph$phc_mean[d2ph$p-fer])
hist_k_fer <- hist(d2ph$phc_mean[d2ph$k_fer])
#texture, water management

#_______________________________________________________________________________

es1ph = as.data.table(es1ph)
es1ph = es1ph[abs(yi)<=10]

##ggplot for field studies and crop_type == "grain"
es1ph_grain = es1ph[!is.na(yi) & crop_type == "grain"]
setorder(es1ph_grain, -yi)
es1ph_grain[, id := .I]
es1ph_grain[, yi_cor := (exp(yi) - 1) * 100]
es1ph_grain[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pphgrain <- ggplot(data = es1ph_grain, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 0.9) + 
  theme_bw() + ylim(-10, 10) +
  ggtitle('pH response to biochar addition on grain') + 
  xlab("study-id") + 
  ylab("log response ratio")
pphgrain
ggsave(plot = pphgrain, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/pphgrain.jpg', 
       width = 20, height = 10, unit = 'cm')


# ggplot for field studies and crop_type == "veg"

es1ph_veg = es1ph[!is.na(yi) & crop_type == "vegetable"]
setorder(es1ph_veg, -yi)
es1ph_veg[, id := .I]
es1ph_veg[, yi_cor := (exp(yi) - 1) * 100]
es1ph_veg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pphveg <- ggplot(data = es1ph_veg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('pH response to biochar addition on vegetables') + 
  xlab("study-id") + 
  ylab("log response ratio")
pphveg
ggsave(plot=pphveg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/pphveg.jpg',
       width = 20, height = 10,unit='cm')

#_______________________________________________________________________________

#Meta-regression for main factors
##main factor analysis for log response ratio for yield

d3ph <- copy(es1ph)

# Convert data frame to data.table
library(data.table)
d3ph <- as.data.table(d3ph)

#group crop "lettuce" and "cabbage" together to "salad" because of to little studies.
#group oats and barley to wheat
d3ph[,crop3 := crop]
d3ph[crop %in% c('lettuce', 'cabbage'), crop3 := 'salad']
d3ph[crop %in% c('oats', 'barley'), crop3 := 'wheat']

#rename unknown crops
d3ph[crop == "unknown", crop3 := paste("unknown", crop_type)]


# what are the factors to be evaluated
var.site <- c("rain_scaled", "irr_scaled", "texture", "clay_scaled", "sand_scaled", 
              "water_management", "sbd_scaled", "sph_scaled", "soc_scaled", "stn_scaled")
var.crop <- c("crop3", "crop_type", "n_fer_scaled", "p_fer_scaled", "k_fer_scaled")
var.bc <- c("bph_scaled", "btc_scaled", "btn_scaled", "brate_scaled")
# i select only one example

# the columns to be assessed
var.sel <- c(var.site,var.crop,var.bc)

# run without a main factor selection to estimate overall mean
rph_0 <- rma.mv(yi, vi, data = d3ph, random = list(~ 1|studyid), method = "REML", sparse = TRUE)


# objects to store the effects per factor as well summary stats of the meta-analytical models
out3.est = out3.sum = list()

# evaluate the impact of treatment (column rain) on yield given site properties
for(i in var.sel){
  
  # check whether the column is a numeric or categorical variable
  vartype = is.character(d3ph[, get(i)])
  
  # run with the main factor treatment
  if (vartype) {
    # run a meta-regression model for main categorical variable
    rph_1 <- rma.mv(yi, vi, mods = ~factor(varsel) - 1, 
                     data = d3ph[, .(yi, vi, studyid, varsel = get(i))], 
                     random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  } else {
    # run a meta-regression model for main numerical variable
    rph_1 <- rma.mv(yi, vi, mods = ~varsel, 
                     data = d3ph[, .(yi, vi, studyid, varsel = get(i))], 
                     random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  }
  
  # save output in a list: the estimated impact of the explanatory variable
  out3.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)', '', rownames(rph_1$b)),
                              mean = round(as.numeric(rph_1$b), 3),
                              se = round(as.numeric(rph_1$se), 3),
                              ci.lb = round(as.numeric(rph_1$ci.lb), 3),
                              ci.ub = round(as.numeric(rph_1$ci.ub), 3),
                              pval = round(as.numeric(rph_1$pval), 3))
  
  # save output in a list: the summary stats collected
  out3.sum[[i]] <- data.table(var = i,
                              AIC = rph_1$fit.stats[4, 2],
                              ll = rph_1$fit.stats[1, 2],
                              ll_impr = round(100 * (1 - rph_1$fit.stats[1, 2] / rph_0$fit.stats[1, 2]), 2),
                              r2_impr = round(100 * max(0, (sum(rph_0$sigma2) - sum(rph_1$sigma2)) / sum(rph_0$sigma2)), 2),
                              pval = round(anova(rph_1, rph_0)$pval, 3)
  )
  
}

# merge output into a data.table
out3.sum <- rbindlist(out3.sum)
out3.est <- rbindlist(out3.est)
print(out3.sum)
print(out3.est)
# save out.sum for supporting information
#library(data.table)
#data.table(out3.sum, caption = 'Summary Statistics - SMD')
#data.table(out3.est, caption = 'out.est - SMD')

out3.est_wi <- out3.est[varname != 'intrcpt']
out3.est_wi
library(openxlsx)
write.xlsx(out3.est_wi, file = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/ph/out3_est_wi.xlsx")

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
crop_type_data <- out3.est[var == "crop_type"]
bar_crop_type_ph <- ggplot(crop_type_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crop Type", y = "Relative change in pH", 
       title = "SMD Response by Crop Type due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_type_ph
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/SMD_bar_crop_type_ph.jpg", 
       plot = bar_crop_type_ph, 
       width = 20, height = 10, units = "cm")


#crop______
crop_data <- out3.est[var == "crop3" & varname != "unknown unknown"]
bar_crop_ph <- ggplot(crop_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "Crops", y = "Relative change in soil pH", 
       title = "Standardized Mean Difference Response by Crops due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_ph
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/SMD_bar_crop_ph.jpg", 
       plot = bar_crop_ph, 
       width = 20, height = 7, units = "cm")

#soil texture______

texture_data <- out3.est[var == "texture"]
bar_texture_ph <- ggplot(texture_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Soil Texture", y = "Relative change in pH", 
       title = "SMD Response by Soil Texture due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_texture_ph
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/SMD_bar_texture_ph.jpg", 
       plot = bar_texture_ph, 
       width = 20, height = 10, units = "cm")

#water_management______

water_management_data <- out3.est[var == "water_management"]
bar_water_management_ph <- ggplot(water_management_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Water Management", y = "Relative change in pH", 
       title = "SMD Response by Water Management due to Biochar application") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_water_management_ph
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/SMD_bar_water_management_ph.jpg", 
       plot = bar_water_management_ph, 
       width = 20, height = 10, units = "cm")

#_____

#numeric coefficients (scaled)
##remove the "_scaled"
num_ph$var <- gsub("_scaled", "", num_ph$var)

# Define the order of levels for the var factor
var_order <- c('clay', 'sand', 'sbd', 'sph', 'soc', 
               'stn', 'rain', 'irr', 'n_fer', 'p_fer', 
               'k_fer', 'bph', 'btc', 'btn', 'brate')

# Reorder the levels of the var factor
num_ph$var <- factor(num_ph$var, levels = var_order)
num_ph
bar_num_ph <- ggplot(num_ph, aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity", fill = "dimgrey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "Variable", y = "Relative change in soil pH", 
       title = "SMD Response on ph due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))+
  annotate("rect", xmin = -Inf, xmax = 7.5, ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.2) + 
  annotate("rect", xmin = 7.5, xmax = 11.5, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.2) + 
  annotate("rect", xmin = 11.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) 
bar_num_ph
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/SMD_bar_num_ph.jpg", 
       plot = bar_num_ph, 
       width = 20, height = 6, units = "cm")

#_______________________________________________________________________________
#Meta-regression for main factors with interactions


# make a function to extract relevant model statistics
estats_ph <- function(model_new, model_base) {
  out <- data.table(
    AIC = model_new$fit.stats[4, 2],
    ll = model_new$fit.stats[1, 2],
    ll_impr = round(100 * (1 - model_new$fit.stats[1, 2] / model_base$fit.stats[1, 2]), 2),
    r2_impr = round(100 * max(0, (sum(model_base$sigma2) - sum(model_new$sigma2)) / sum(model_base$sigma2)), 2),
    pval = round(anova(model_new, model_base)$pval, 3)
  )
  return(out)
}

d4ph <- copy(d3ph)
# run without a main factor selection to estimate overall mean
rph_0 <- rma.mv(yi,vi, data = d4ph,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# 1. make a simple meta-regression model without interaction but with more than one explanatory variable

rph_1 <- rma.mv(yi,vi, 
                 mods = ~ sph_scaled + soc_scaled + clay_scaled + sbd_scaled * bph_scaled + rain_scaled + btc_scaled + brate_scaled -1, 
                 data = d4ph,
                 random = list(~ 1|studyid), method="REML",sparse = TRUE) 
out = estats_ph(model_new = rph_1,model_base = rph_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(rph_1)

summary_output_ph <- capture.output(summary(rph_1))

# Specify the file path where you want to save the summary
file_path <- "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/ph/summary_ph.txt"

# Write the captured output to the file
writeLines(summary_output_ph, file_path)

# visualize
# Extract coefficient names from the model object
coeff_names_ph <- names(coef(rph_1))

# Create the data frame
summary_ph <- data.frame(
  Coefficients = coeff_names_ph,
  Estimate = summary(rph_1)$b,
  PValue = summary(rph_1)$pval
)
summary_ph$Significance <- ifelse(summary_ph$PValue < .001, '***',
                                     ifelse(summary_ph$PValue < .01, '**',
                                            ifelse(summary_ph$PValue < .05, '*',
                                                   ifelse(summary_ph$PValue < .1, '.', ' '))))
# Define the order of levels for the var factor
#sum_ph_order  <- c('clay', 'sbd', 'sph', 'soc', 'rain', 'bph', 'btc', 'brate', 'sbd:bph')
# Reorder the levels of the var factor
#summary_ph$Coefficients <- factor(summary_ph$Coefficients, levels = sum_ph_order)


sum_ph <- ggplot(summary_ph, aes(x=reorder(Coefficients, Estimate), y=Estimate, fill=Significance)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Significance), vjust=1.5, color="black") +
  scale_fill_manual(values=c('***'='olivedrab', '**'='orange', '*'='darksalmon', '.'='red', ' ' = 'grey')) +
  labs(title="Parameter Estimates with Significance Levels",
       x="Coefficients", y="Parameter estimate") +
  theme_minimal()
sum_ph
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/ph/sum_ph.jpg", 
       plot = sum_ph, 
       width = 20, height = 7, units = "cm")
