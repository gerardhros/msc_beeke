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

> m1.main <- read_excel("data/meta_model_R_copy1.xlsx", sheet = 4)
> m1.main <- as.data.table(m1.main)
> m1.main <- m1.main[,.(ind_code, ES, SE, n)]

## Meta model

> library(data.table)

# Main

> setDT(m1.main)

> m1.main[,.(wm.mean = sum((ES / SE^2)) / sum(1 / SE^2)) , by = ind_code]

> m1.main <- m1.main[-(17:22), ]    #remove row 17:22 (paper Mel22)

m1.main
ind_code        ES        SE   n     wm.sd   wm.mean
1:                  Y  14.45000 0.2576531 381 0.2397237 14.856061
2:             N-pool  13.97000 0.2346939 381 0.2346939 13.970000
3:                WHC  14.28000 0.9005102 381 0.8179602 12.604103
4:       bulk density  -9.60000 0.3629243  76 0.3396137 -9.649734
5:           porosity  10.90000 0.3207238  35 0.2551696  9.468653
6:                SOC  36.10000 7.0884740 227 3.4111722 46.781032
7:           porosity   7.00000 0.4212004 346 0.2551696  9.468653
8: soil water content  10.80000 1.2033115 209 1.2033115 10.800000
9:                  Y  17.40000 0.6856666 337 0.2397237 14.856061
10:                WHC   4.70000 1.9556509  57 0.8179602 12.604103
11:            soil ph  12.00000 0.6138819 184 0.6138819 12.000000
12:                  Y  57.00000 5.2154195 102 0.2397237 14.856061
13:                CEC  30.05076 3.2196085  48 3.2196085 30.050761
14:                SOC  50.00000 3.8913871 104 3.4111722 46.781032
15:    base saturation  49.61832 4.7385366  37 4.7385366 49.618321
16:       bulk density -10.00000 0.9631404  27 0.3396137 -9.649734


# Example sheets "Covariates"

> m1.covar <- m1.covar[,.(ind_code, `moderator 1`, `group 1`, `moderator 2`, `group 2`, ES, SE, n)]

> m1.covar <- m1.covar[-(502:526), ]    #remove row 502:526 (paper Mel22)

> m1.covar[, wm.sd := 1/sqrt(sum(1 / SE^2)), by = c('ind_code','moderator 1', 'group 1', 'moderator 2', 'group 2')]
> m1.covar[,wm.mean := sum((ES / SE^2)) / sum(1 / SE^2) , by = c('ind_code','moderator 1', 'group 1', 'moderator 2', 'group 2')]

ind_code moderator 1          group 1 moderator 2 group 2       ES         SE   n     wm.sd  wm.mean
1:        Y   crop type       industrial        <NA>    <NA> 16.69000  0.9936295  33 0.9936295 16.69000
2:        Y   crop type            grain        <NA>    <NA> 11.61000  0.3382568 236 0.2927265 11.38904
3:        Y   crop type       vegetables        <NA>    <NA> 18.99000  0.4862442 112 0.4786170 19.89546
4:        Y     climate water management        <NA>    <NA> 15.96000  1.2261811 118 0.7663773 16.48345
5:        Y     climate water management        <NA>    <NA> 18.34000  1.0459184 151 0.7663773 16.48345
---                                                                                                     
497:        Y         SOC              low        <NA>    <NA> 59.64523 12.5005656  51 0.4943164 12.65068
498:        Y         SOC              med        <NA>    <NA> 29.04656  4.6382189 156 0.5607949 17.84325
499:        Y         SOC             high        <NA>    <NA> 27.93792  3.8463279 116 0.4324889 10.40483
500:        Y         SOC        very high        <NA>    <NA> 19.06874  3.3938187 125 0.4317134 10.32414
501:     <NA>        <NA>             <NA>        <NA>    <NA>       NA         NA  NA        NA       NA

# safe as an excel
> library(openxlsx)
> write.xlsx(m1.main, "data/m1.main.xlsx", rowNames = FALSE)
> write.xlsx(m1.covar, "data/m1.covar.xlsx", rowNames = FALSE)

