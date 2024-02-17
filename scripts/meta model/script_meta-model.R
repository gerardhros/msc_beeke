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

# Covariates

library(readxl)
m1.covar <- read_excel("C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/meta_model_R_copy1.xlsx", sheet = 5)
library(data.table)
m1.covar <- as.data.table(m1.covar)
m1.covar <- m1.covar[,.(ind_code, `moderator 1`, `group 1`, `moderator 2`, `group 2`, ES, SE, n)]

m1.covar[, wm.sd := 1/sqrt(sum(1 / SE^2)), by = c('ind_code','moderator 1', 'group 1', 'moderator 2', 'group 2')]
m1.covar[, wm.mean := sum((ES / SE^2)) / sum(1 / SE^2) , by = c('ind_code','moderator 1', 'group 1', 'moderator 2', 'group 2')]

# remove duplicates
m2.main <- unique(m1.main[,.('ind_code', 'wm.mean', 'wm.sd')])
m2.covar <- unique(m1.covar[,.('ind_code', `moderator 1`, `group 1`, `moderator 2`, `group 2`, 'ES', 'SE', 'n', 'wm.mean' ,'wm.sd')])

# mean model responses for main ma-models (without covariates)
#m2.mean <- dcast(m2.main,ind_code, value.var = 'wm.mean')
#setnames(m2.mean,colnames(m2.mean)[-1],paste0('mean_',colnames(m2.mean)[-1]))
#m2.sd <- dcast(m2.main,ind_code, value.var = 'wm.sd')
#setnames(m2.sd,colnames(m2.sd)[-1],paste0('sd_',colnames(m2.sd)[-1]))

# mean model responses for main ma-models with covariates
#m2.cov.mean <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.mean')
#setnames(m2.cov.mean,colnames(m2.cov.mean)[-c(1:3)],paste0('mean_',colnames(m2.cov.mean)[-c(1:3)]))
#m2.cov.sd <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.sd')
#setnames(m2.cov.sd,colnames(m2.cov.sd)[-c(1:3)],paste0('sd_',colnames(m2.cov.sd)[-c(1:3)]))

# safe as an excel
library(openxlsx)
write.xlsx(m1.main, "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/m1.main.xlsx", rowNames = FALSE)
write.xlsx(m1.covar, "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/data/meta_model/m1.covar.xlsx", rowNames = FALSE)




