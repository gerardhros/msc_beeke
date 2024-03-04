#title: "Meta regression with Metafor"
#author: "Beeke von Felde"
#date: "2023-08-22"

#packages
#install.packages("metagear")

# Load libraries 
library(data.table)
library(metafor)
library(metagear)

library(readxl)
d2 <- read_excel("data/meta_regression/merged_data_ALL.xlsx")
d2 <- as.data.table(d2)
##organize original data

#remove rows with NA in the "NUEC_mean" and "NUER_mean"
d2 <- d2[!is.na(d2$yc_mean), ]
d2 <- d2[!is.na(d2$yr_mean), ]

#remove other responses than yield
d2 <- d2 %>% 
  select(
    -nuec_mean, -nuec_sd, -nuec_n, -nuer_mean, -nuer_sd, -nuer_n,
    -socc_mean, -socc_sd, -socc_n, -socr_mean, -socr_sd, -socr_n,
    -phc_mean, -phc_sd, -phc_n, -phr_mean, -phr_sd, -phr_n,
    -sbdc_mean, -sbdc_sd, -sbdc_n, -sbdr_mean, -sbdr_sd, -sbdr_n)

# modify the unit for field studies and grain to kg/ha:

d2[crop_type == "grain" & yc_mean <= 100, yc_sd := yc_sd * 1000]
d2[crop_type == "grain" & yc_mean <= 100, yc_mean := yc_mean * 1000]
d2[crop_type == "grain" & yr_mean <= 100, yr_sd := yr_sd * 1000]
d2[crop_type == "grain" & yr_mean <= 100, yr_mean := yr_mean * 1000]

d2[crop_type == "vegetable" & yc_mean <= 100, yc_sd := yc_sd * 1000]
d2[crop_type == "vegetable" & yc_mean <= 100, yc_mean := yc_mean * 1000]
d2[crop_type == "vegetable" & yr_mean <= 100, yr_sd := yr_sd * 1000]
d2[crop_type == "vegetable" & yr_mean <= 100, yr_mean := yr_mean * 1000]

d2[crop_type == "vegetable" & yc_mean <= 300, yc_sd := yc_sd * 100]
d2[crop_type == "vegetable" & yc_mean <= 300, yc_mean := yc_mean * 100]
d2[crop_type == "vegetable" & yr_mean <= 300, yr_sd := yr_sd * 100]
d2[crop_type == "vegetable" & yr_mean <= 300, yr_mean := yr_mean * 100]

##scaling of the variables to unit variance

d2[, rain_scaled := scale(rain)]
d2[, irr_scaled := scale(irr)]
d2[, n_fer_scaled := scale(n_fer)]
d2[, p_fer_scaled := scale(p_fer)]
d2[, k_fer_scaled := scale(k_fer)]
d2[, sbd_scaled := scale(sbd)]
d2[, sph_scaled := scale(sph)]
d2[, soc_scaled := scale(soc)]
d2[, stn_scaled := scale(stn)]
d2[, bph_scaled := scale(bph)]
d2[, btc_scaled := scale(btc)]
d2[, btn_scaled := scale(btn)]
d2[, brate_scaled := scale(brate)]
d2[, clay_scaled := scale(clay)]
d2[, sand_scaled := scale(sand)]


#_______________________________________________________________________________

## Supplement the SD when missing

# Yield
missing_sd_yc <- is.na(d2$yc_sd) | d2$yc_sd == ""
CV_yc <- mean(d2$yc_sd[!missing_sd_yc] / d2$yc_mean[!missing_sd_yc], na.rm = TRUE)
d2$yc_sd[missing_sd_yc] <- d2$yc_mean[missing_sd_yc] * 1.25 * CV_yc

missing_sd_yr <- is.na(d2$yr_sd) | d2$yr_sd == ""
CV_yr <- mean(d2$yr_sd[!missing_sd_yr] / d2$yr_mean[!missing_sd_yr], na.rm = TRUE)
d2$yr_sd[missing_sd_yr] <- d2$yr_mean[missing_sd_yr] * 1.25 * CV_yr

 
fwrite(d2, file = "data/meta_regression/yield/d2.csv")

#_______________________________________________________________________________

## Estimate meta-analytical response measure (MD Method)
# calculate effect size 

library(metafor)

#Y
es21y <- escalc(measure = "SMD", data = d2, 
                m1i = yr_mean, sd1i = yr_sd, n1i = yr_n,
                m2i = yc_mean, sd2i = yc_sd, n2i = yc_n)
fwrite(es21y, file = "data/meta_regression/yield/es21y.csv")

#_______________________________________________________________________________  

# make forest plots per group treatments

#for yield:

##histogram pro crop type:

hist_grain <- hist(d2$yc_mean[d2$crop_type == "grain"])
hist_grain <- hist(d2$yr_mean[d2$crop_type == "grain"])

hist_fruit <- hist(d2$yc_mean[d2$crop_type == "fruit"])
hist_fruit <- hist(d2$yr_mean[d2$crop_type == "fruit"])

hist_industrial <- hist(d2$yc_mean[d2$crop_type == "industrial"])
hist_industrial <- hist(d2$yr_mean[d2$crop_type == "industrial"])  

hist_legumes <- hist(d2$yc_mean[d2$crop_type == "legumes"])
hist_legumes <- hist(d2$yr_mean[d2$crop_type == "legumes"])  

hist_tubers <- hist(d2$yc_mean[d2$crop_type == "tubers"])
hist_tubers <- hist(d2$yr_mean[d2$crop_type == "tubers"])

hist_vegetable <- hist(d2$yc_mean[d2$crop_type == "vegetable"])
hist_vegetable <- hist(d2$yr_mean[d2$crop_type == "vegetable"])

hist_others <- hist(d2$yc_mean[d2$crop_type == "others"])
hist_others <- hist(d2$yr_mean[d2$crop_type == "others"])

#_______________________________________________________________________________

es21y = as.data.table(es21y)
es21y = es21y[abs(yi)<=10]

##ggplot for field studies and crop_type == "grain"
es21y_grain = es21y[!is.na(yi) & crop_type == "grain"]
setorder(es21y_grain,-yi)
es21y_grain[,id:=.I]
es21y_grain[,yi_cor := (exp(yi)-1)*100]
es21y_grain[,vi_cor := (exp(vi)-1)*100]
library(ggplot2)
pgrain <- ggplot(data = es21y_grain, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 0.9) + 
  theme_bw() + ylim(-10,10) +
  ggtitle('crop yield response to biochar addition on grain') + 
  xlab("study-id") + 
  ylab("log response ratio")
pgrain
ggsave(plot=pgrain, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/pgrain.jpg', 
       width = 20, height = 7,unit='cm')


##ggplot for field studies and crop_type == "fruit"

es21y_fruit = es21y[!is.na(yi) & crop_type == "fruit"]
setorder(es21y_fruit,-yi)
es21y_fruit[,id:=.I]
es21y_fruit[,yi_cor := (exp(yi)-1)*100]
es21y_fruit[,vi_cor := (exp(vi)-1)*100]
library(ggplot2)
pfruit <-  ggplot(data = es21y_fruit, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('crop yield response to biochar addition on fruit') + 
  xlab("study-id") + 
  ylab("log response ratio")
#show and save plot
pfruit
ggsave(plot=pfruit, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/pfruit.jpg',
       width = 15, height = 7,unit='cm')


# ggplot for field studies and crop_type == "veg"

es21y_veg = es21y[!is.na(yi) & crop_type == "vegetable"]
setorder(es21y_veg, -yi)
es21y_veg[, id := .I]
es21y_veg[, yi_cor := (exp(yi) - 1) * 100]
es21y_veg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pveg <- ggplot(data = es21y_veg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('crop yield response to biochar addition on vegetables') + 
  xlab("study-id") + 
  ylab("log response ratio")
pveg
ggsave(plot=pveg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/pveg.jpg',
       width = 15, height = 7,unit='cm')

# ggplot for field studies and crop_type == "legumes"

es21y_leg = es21y[!is.na(yi) & crop_type == "legumes"]
setorder(es21y_leg, -yi)
es21y_leg[, id := .I]
es21y_leg[, yi_cor := (exp(yi) - 1) * 100]
es21y_leg[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pleg <- ggplot(data = es21y_leg, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('crop yield response to biochar addition on legumes') + 
  xlab("study-id") + 
  ylab("log response ratio")
pleg
ggsave(plot=pleg, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/pleg.jpg',
       width = 15, height = 7,unit='cm')

# ggplot for field studies and crop_type == "tubers"

es21y_tubers = es21y[!is.na(yi) & crop_type == "tubers"]
setorder(es21y_tubers, -yi)
es21y_tubers[, id := .I]
es21y_tubers[, yi_cor := (exp(yi) - 1) * 100]
es21y_tubers[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
ptub <- ggplot(data = es21y_tubers, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('crop yield response to biochar addition on tubers') + 
  xlab("study-id") + 
  ylab("log response ratio")
ptub
ggsave(plot=ptub, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/ptub.jpg',
       width = 15, height = 7,unit='cm')

# ggplot for field studies and crop_type == "others"

es21y_others = es21y[!is.na(yi) & crop_type == "others"]
setorder(es21y_others, -yi)
es21y_others[, id := .I]
es21y_others[, yi_cor := (exp(yi) - 1) * 100]
es21y_others[, vi_cor := (exp(vi) - 1) * 100]
library(ggplot2)
pother <- ggplot(data = es21y_others, aes(x = id, y = yi)) + 
  geom_line() + 
  geom_errorbar(aes(x = id, ymin = yi - sqrt(vi), ymax = yi + sqrt(vi)),
                width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.3) + 
  theme_bw() + 
  ggtitle('crop yield response to biochar addition on others (tea, tobacco, zizyphus, tall fescue, cocoyam, and pasture)') + 
  xlab("study-id") + 
  ylab("log response ratio")
pother
ggsave(plot=pother, 
       filename = 'C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/pother.jpg',
       width = 15, height = 7,unit='cm')

#_______________________________________________________________________________

#Meta-regression for main factors
##main factor analysis for log response ratio for yield

# Load the data.table package

d3y <- copy(es21y)

# Convert data frame to data.table
library(data.table)
d3y <- as.data.table(d3y)

# what are the factors to be evaluated
var.site <- c("rain_scaled", "irr_scaled", "texture", "clay_scaled", "sand_scaled", 
             "water_management", "sbd_scaled", "sph_scaled", "soc_scaled", "stn_scaled")
var.crop <- c("crop", "crop_type", "n_fer_scaled", "p_fer_scaled", "k_fer_scaled")
var.bc <- c("bph_scaled", "btc_scaled", "btn_scaled", "brate_scaled")
# i select only one example

# the columns to be assessed
var.sel <- c(var.site,var.crop,var.bc)

# run without a main factor selection to estimate overall mean
ry_0 <- rma.mv(yi, vi, data = d3y, random = list(~ 1|studyid), method = "REML", sparse = TRUE)


# objects to store the effects per factor as well summary stats of the meta-analytical models
out1.est = out1.sum = list()

# evaluate the impact of treatment (column rain) on yield given site properties
for(i in var.sel){
  
  # check whether the column is a numeric or categorical variable
  vartype = is.character(d3y[, get(i)])
  
  # run with the main factor treatment
  if (vartype) {
    # run a meta-regression model for main categorical variable
    ry_1 <- rma.mv(yi, vi, mods = ~factor(varsel) - 1, 
                   data = d3y[, .(yi, vi, studyid, varsel = get(i))], 
                   random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  } else {
    # run a meta-regression model for main numerical variable
    ry_1 <- rma.mv(yi, vi, mods = ~varsel, 
                   data = d3y[, .(yi, vi, studyid, varsel = get(i))], 
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
# save out.sum for supporting information
#library(data.table)
#data.table(out1.sum, caption = 'Summary Statistics - SMD')
data.table(out1.est, caption = 'out.est - SMD')

out1.est_wi <- out1.est[varname != 'intrcpt']
library(openxlsx)

write.xlsx(out1.est_wi, file = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/yield/out1_est_wi.xlsx")

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
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crop Type", y = "Relative change of yield", 
       title = "SMD Response on Yield by Crop Type due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop_type
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/SMD_bar_crop_type.jpg", 
       plot = bar_crop_type, 
       width = 20, height = 10, units = "cm")


#crop______
crop_data <- out1.est[var == "crop"]
bar_crop <- ggplot(crop_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Crops", y = "Relative change of yield", 
       title = "SMD Response on Yield by Crops due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_crop
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/SMD_bar_crop.jpg", 
       plot = bar_crop, 
       width = 20, height = 10, units = "cm")

#soil texture______

texture_data <- out1.est[var == "texture"]
bar_texture <- ggplot(texture_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Soil Texture", y = "Relative change of yield", 
       title = "SMD Response on Yield by Soil Texture due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_texture
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/SMD_bar_texture.jpg", 
       plot = bar_texture, 
       width = 20, height = 10, units = "cm")

#water_management______

water_management_data <- out1.est[var == "water_management" & varname != "unknown"]
bar_water_management <- ggplot(water_management_data, aes(x = varname, y = mean, fill = varname)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Water Management", y = "Relative change of yield", 
       title = "SMD Response on Yield by Water Management due to Biochar application") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_water_management
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/SMD_bar_water_management.jpg", 
       plot = bar_water_management, 
       width = 20, height = 10, units = "cm")


#_____

#numeric coefficients (scaled)

num_y <- out1.est[var %in% c('clay_scaled', 'sand_scaled', 'sbd_scaled', 'sph_scaled', 'soc_scaled', 
                           'stn_scaled', 'rain_scaled', 'irr_scaled', 'n_fer_scaled', 'p_fer_scaled', 
                           'k_fer_scaled', 'bph_scaled', 'btc_scaled', 'btn_scaled', 'brate_scaled')
                & varname != 'intrcpt']
num_y$var <- factor(num$var, levels = c('clay_scaled', 'sand_scaled', 'sbd_scaled', 'sph_scaled', 'soc_scaled', 
                                      'stn_scaled', 'rain_scaled', 'irr_scaled', 'n_fer_scaled', 'p_fer_scaled', 
                                      'k_fer_scaled', 'bph_scaled', 'btc_scaled', 'btn_scaled', 'brate_scaled'))
num_y
bar_num <- ggplot(num_y, aes(x = var, y = mean, fill = var)) +
   geom_bar(stat = "identity", fill = "dimgrey") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
  labs(x = "Variable", y = "Relative change of yield", 
       title = "SMD Response on Yield due to Biochar application") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  
        panel.background = element_rect(fill = "white", colour = "white"))+
  annotate("rect", xmin = -Inf, xmax = 7.5, ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.2) + 
  annotate("rect", xmin = 7.5, xmax = 11.5, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.2) + 
  annotate("rect", xmin = 11.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) 
bar_num
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/SMD_bar_num.jpg", 
       plot = bar_num, 
       width = 20, height = 10, units = "cm")

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

d4y <- copy(d3y)
# run without a main factor selection to estimate overall mean
ry_0 <- rma.mv(yi,vi, data = d4y,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# from first check i see that some crop types behave similarly, so i combine them
d4y[,crop_type2 := crop_type]
d4y[crop_type %in% c('fruit', 'industrial', 'vegetable'), crop_type2 := 'fruit_ind_veg']
d4y[crop_type %in% c('tubers', 'others'), crop_type2 := 'tubers_and_others']

# 1. make a simple meta-regression model without interaction but with more than one explanatory variable
ry_1 <- rma.mv(yi,vi, 
               mods = ~ crop_type2 + clay + soc + water_management * sph + n_fer * btn + brate -1, 
               data = d4y,
               random = list(~ 1|studyid), method="REML",sparse = TRUE) 
out = estats(model_new = ry_1,model_base = ry_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(ry_1)

summary_output_yield <- capture.output(summary(ry_1))

# Specify the file path where you want to save the summary
file_path <- "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_regression/yield/summary_yield.txt"

# Write the captured output to the file
writeLines(summary_output_yield, file_path)

library(metafor)
library(ggplot2)

# Assuming ry_1 is your model object
estimates <- coef(ry_1)
se <- sqrt(diag(vcov(ry_1)))
variables <- names(estimates)

# Create a data frame for ggplot
df <- data.frame(Variable = variables, Estimate = estimates, SE = se)

# Create the plot
sum_y <- ggplot(df, aes(x = Variable, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() + 
  xlab('Variables') +
  ylab('Estimates')
sum_y
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/yield/summary_y.jpg", 
       plot = sum_y, 
       width = 20, height = 15, units = "cm")
    
    
    
    
    