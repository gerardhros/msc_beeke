# first always require the packages that are needed 

require(readxl)
require(data.table)

# load meta-analytical models for main models with covariates, and select only relevant columns

#library(readxl)
#d1 <- read_xlsx("C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/meta_model_R_copy1.xlsx", sheet = 2)
#d1 <- as.data.table(d1)           #to convert the object into a data table instead of a normal table
#d1 <- d1[,.(response_variable, moderators, group, response, SE, n)]

# add simplified grouping for moderators to simplify joining with integrator data later
# groups are unique


## Meta model

# Main
library(readxl)
m1.main <- read_excel("C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/meta_model_R_copy1.xlsx", sheet = 4)
library(data.table)
m1.main <- as.data.table(m1.main)
m1.main <- m1.main[,.(ind_code, ES, SE, n)]

m1.main[, wm.sd := 1/sqrt(sum(1 / SE^2)), by = c('ind_code')]
m1.main[, wm.mean := sum((ES / SE^2)) / sum(1 / SE^2), by = ind_code]

m1.main.mean <- dcast(data = m1.main, 
                      formula = ind_code ~ ., 
                      value.var = 'wm.mean', 
                      fun.aggregate = mean)
setnames(m1.main.mean, old = ".", new = "wm_mean")
m1.main.sd <- dcast(data = m1.main, 
                    formula = ind_code ~ ., 
                    value.var = 'wm.sd', 
                    fun.aggregate = mean)
setnames(m1.main.sd, old = ".", new = "wm_sd")

#merge m1.main.mean and m1.main.sd
m1.main.merged = merge(m1.main.mean, m1.main.sd, by = c("ind_code"), all.x = TRUE)

fwrite(m1.main.merged, file = "data/meta_model/m1.main.merged.csv")


earthtone_colors <- c(
  "darkred", "darkorange", "rosybrown", "olivedrab", "chocolate",
  "saddlebrown", "darkgoldenrod", "maroon", "peru", "sienna", 
  "brown", "darkolivegreen", "lightsalmon", "tan", "goldenrod", 
  "coral", "tomato", "sandybrown", "firebrick", "indianred", 
  "darksalmon"
)

library(ggplot2)
bar_m1_merged <- ggplot(m1.main.merged, aes(x = ind_code, y = wm_mean, fill = ind_code)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = wm_mean - wm_sd, ymax = wm_mean + wm_sd), width = 0.2) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Indicator Code", y = "Weighted Mean", 
       title = "Weighted Mean and Standard Deviation by Indicator") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))
bar_m1_merged
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/model/bar_m1_merged.jpg", 
       plot = bar_m1_merged, 
       width = 20, height = 10, units = "cm")

#______________________________________________________________________________
# Covariates

library(readxl)
m1.covar <- read_excel("C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/meta_model_R_copy1.xlsx", sheet = 5)
library(data.table)
m1.covar <- as.data.table(m1.covar)

m1.covar <- m1.covar[,.(ind_code, `moderator 1`, `group 1`, `moderator 2`, `group 2`, ES, SE, n)]

#only by ind_code and moderator 1 to get the average estimates of certain combinations of groups
m1.covar[, wm.sd := 1/sqrt(sum(1 / SE^2)), by = c('ind_code','moderator 1')]
m1.covar[, wm.mean := sum((ES / SE^2)) / sum(1 / SE^2) , by = c('ind_code','moderator 1')]

m1.covar <- m1.covar %>%
  select(-c(`group 1`, `group 2`, ES, SE, n))

m1.cov.mean <- dcast(data = m1.covar, 
                     formula = ind_code ~ `moderator 1`, 
                     value.var = 'wm.mean', 
                     fun.aggregate = mean)
setnames(m1.cov.mean, colnames(m1.cov.mean)[-c(0:1)], paste0('wm_mean_', colnames(m1.cov.mean)[-c(0:1)]))
m1.cov.sd <- dcast(data = m1.covar, 
                   formula = ind_code ~ `moderator 1`, 
                   value.var = 'wm.sd', 
                   fun.aggregate = mean)
setnames(m1.cov.sd, colnames(m1.cov.sd)[-c(0:1)], paste0('wm_sd_', colnames(m1.cov.sd)[-c(0:1)]))

#merge m1.main.mean and m1.main.sd
m1.cov.merged <- merge(m1.cov.mean, m1.cov.sd, 
                       by = c("moderator 1"), 
                       all.x = TRUE)

# remove "NaN" and replace by NA
m1.cov.merged[, (names(m1.cov.merged)) := lapply(.SD, function(x) ifelse(is.numeric(x) & is.nan(x), NA, x))]
# merge "soil ph" and "soil pH" and "bc ph" and "bc pH"


fwrite(m1.cov.merged, file = "data/meta_model/m1.cov.merged.csv")



#visualize

m2.cov.merged <- copy(m1.cov.merged)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming m2.cov.merged is already loaded
# Create a subset for individual means where moderator 2 is not NA
individual_means <- m2.cov.merged %>% 
  filter(!is.na(`moderator 2`))

# Create a subset for weighted means where moderator 2 is NA
weighted_means <- m2.cov.merged %>% 
  filter(is.na(`moderator 2`))

# Calculate min and max SE for each moderator
se_ranges <- individual_means %>%
  group_by(`moderator 1`) %>%
  summarise(min_SE = min(SE), max_SE = max(SE), mean_ES = mean(ES))

# Create the ggplot
change_npool <- ggplot() +
  geom_point(data = individual_means, aes(x = `moderator 1`, y = 'ES'), color = "blue", size = 3, shape = 16) +  # Individual mean values
  geom_point(data = weighted_means, aes(x = `moderator 1`, y = `wm_N pool`), color = "red", size = 3, shape = 15) +  # Weighted mean values
  geom_errorbar(data = weighted_means, aes(x = `moderator 1`, ymin = `wm_N pool` - `wm_sd_N pool`, ymax = `wm_N pool` + `wm_sd_N pool`), color = "darkgrey", width = 0.2) +  # Error bars for weighted means
  geom_errorbar(data = se_ranges, aes(x = `moderator 1`, ymin = mean_ES - min_SE, ymax = mean_ES + max_SE), color = "blue", width = 0.2) +  # Error bars for the range of SE for each group
  coord_flip() +
  labs(x = "Change in N pool (%)", y = "Management Practices", 
       title = "Weighted means of N pool changes due to management practices") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))

# Display the plot
print(change_npool)

# Save the plot
ggsave(filename = "change_npool.jpg", plot = change_npool, width = 20, height = 10, units = "cm")



#bar chart:
earthtone_colors <- c(
  "darkred", "darkorange", "rosybrown", "olivedrab", "chocolate",
  "saddlebrown", "darkgoldenrod", "maroon", "peru", "sienna", 
  "brown", "darkolivegreen", "lightsalmon", "tan", "goldenrod", 
  "coral", "tomato", "sandybrown", "firebrick", "indianred", 
  "darksalmon"
)

#ash content

library(ggplot2)
bar_cov_ash <- ggplot(m1.cov.merged [m1.cov.merged$`moderator 1` == "ash content" & !is.nan(`wm_N pool`) ],
                      aes(x = group_label, y = `wm_N pool`, fill = group_label)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = `wm_N pool` - `wm_sd_N pool`, ymax = `wm_N pool` + `wm_sd_N pool`), 
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = earthtone_colors) +
  labs(x = "Group Label", y = "Mean N pool", 
       title = "Mean N pool with Standard Deviation by Group Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
bar_cov_ash
ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/model/bar_cov_ash.jpg", 
       plot = bar_cov_ash, 
       width = 20, height = 10, units = "cm")

# remove duplicates
#m2.main <- unique(m1.main[,.('ind_code', 'wm.mean', 'wm.sd')])
#m2.covar <- unique(m1.covar[,.('ind_code', `moderator 1`, `group 1`, `moderator 2`, `group 2`, 'ES', 'SE', 'n', 'wm.mean' ,'wm.sd')])



# mean model responses for main ma-models (without covariates)
m1.mean <- dcast(m1.main,ind_code, value.var = 'wm.mean')
setnames(m1.mean,colnames(m1.mean)[-1],paste0('mean_',colnames(m1.mean)[-1]))
m1.sd <- dcast(m1.main,ind_code, value.var = 'wm.sd')
setnames(m1.sd,colnames(m1.sd)[-1],paste0('sd_',colnames(m1.sd)[-1]))

# mean model responses for main ma-models with covariates
m1.cov.mean <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.mean')
setnames(m1.cov.mean,colnames(m1.cov.mean)[-c(1:3)],paste0('mean_',colnames(m1.cov.mean)[-c(1:3)]))
m1.cov.sd <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.sd')
setnames(m1.cov.sd,colnames(m1.cov.sd)[-c(1:3)],paste0('sd_',colnames(m1.cov.sd)[-c(1:3)]))

# safe as an excel
library(openxlsx)
write.xlsx(m1.main, "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/m1.main.xlsx", rowNames = FALSE)
write.xlsx(m1.covar, "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/m1.covar.xlsx", rowNames = FALSE)

#______________________________________________________________________________
#moderator description

moderator_description <- read_excel("C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/meta_model_R_copy1.xlsx", sheet = 5)

#______________________________________________________________________________
#control treatment description


_________________________________
m1.main.mean <- dcast(data = m1.main, 
                 formula = ES + SE + n + ind_code ~ ., 
                 value.var = 'wm.mean', 
                 fun.aggregate = mean)
setnames(m1.main.mean, old = ".", new = "wm_mean")
m1.main.sd <- dcast(data = m1.main, 
               formula = ES + SE + n + ind_code ~ ., 
               value.var = 'wm.sd', 
               fun.aggregate = mean)
setnames(m1.main.sd, old = ".", new = "wm_sd")

#merge m1.main.mean and m1.main.sd
m1.main.merged = merge(m1.main.mean, m1.main.sd, by = c("ind_code", "ES", "SE", "n"), all.x = TRUE)

library(dplyr)
unique_data <- m1.main.merged %>%
  select(ind_code, wm_mean, wm_sd) %>%
  distinct()
_________________________________


