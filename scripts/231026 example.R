# add example script 1

# add new senetnce

require(readxl)
require(data.table)

d1 <- read_xlsx("data/meta_model_v1.xlsx", sheet = 2)
d1 <- as.data.table(d1)
