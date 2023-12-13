# first always require the packages that are needed 

require(readxl)
require(data.table)

# load meta-analytical models for main models with covariates, and select only relevant columns

d1 <- read_xlsx("data/meta_model_v1.xlsx", sheet = 2)
d1 <- as.data.table(d1)           #to convert the object into a data table instead of a normal table
d1 <- d1[,.(response_variable, moderators, group, response, SE, n)]

# add simplified grouping for moderators to simplify joining with integrator data later
# groups are unique

## Example sheet "main"

> meta_model_R_copy1 <- read_excel("data/meta_model_R_copy1.xlsx")
> View(meta_model_R_copy1)
> m1.main <- as.data.table(m1.main)
> m1.main <- m1.main[,.(ind_code, ES, SE, n)]