#read data table
#estimate missing SD
  calculate the coefficient of variation (relative error)
  example luncheng 
  d3[is.na(nuet_sd), nuet_sd := mean(d3$nuet_sd/d3$nuet_mean,na.rm=T) * 1.25 * nuet_mean]
  d3[is.na(nuec_sd), nuec_sd := mean(d3$nuec_sd/d3$nuec_mean,na.rm=T) * 1.25 * nuec_mean]
#calculate the effect size
  escalc is used as a function from the metafor package
  example luncheng
  d3 <- escalc(measure = "ROM", data = d3,
               m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
               m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )
  ROM = log response ratio -> you need the mean, SD and n
  m1i = treatment 
  m2i = control
  the original table will be expanded with "yi" and "vi" which is the log response ratio and the variance (SD squared)
#scaling of the variables to unit variance
  scaling helps to interpret the model parameters
  it changes the value but not the relative differences
  z score (standardization: How far is the observation deviating from the mean?)
  = observation - mean/SD
  if you plot that the values are between -2 and 2 (single line)
  if you do it for all the parameters, the model trains coefficience 
  you can compare coefficients because you will multiply in the model the coefficients with the value between -2 and 2
  if one coefficient is higher the parameter is more important (if there are factors of 30 and 20000 you can not interpret the coefficience anymore)
  in the description lateron you have to keep in mind: if e.x. "pH is increasing with 1 SD than NUE in decreasing with ... SD of NUE" Interpretation is within a scaled units
  to make it logic again (for the discussion) you need to unscale the values -> plots of original pH
#main factor analysis
  in this case: SOC, rainfall, irrigation, crop type, ...
  ##create an (empty) list to store the coefficients
    out2 = out3 = list()
  ##make a for loop to do a main analysis per treatment
    For each category in the treatment you do following steps within the {} -> repeat within all the categories 
    for(i in d02.treat$treatment){
      if(i=='ALL')
#meta regression with the metafor package
  You can calculate the average response independent of all the categories
      In this case for NUE and it runs all the different management options.
  ##run without selection to estimate overall mean
      {r_nue <- rma.mv(yi,vi, data=d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
      }
    requires response ratio, ES, variance, source of the data, error structure (studyid)
    studiyid to differenciate error 
    choose a method: RMEL 
    sparse = TRUE -> because the data set has missing values
  ##run for selected treatment
      {r_nue <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
      }
    management == i --> you need a subset in the model --> for each regression it runs the output --> stored in list out2 
  ##save output in a list
    out2[[i]] <- data.table(mean = as.numeric(r_nue$b),
                            se = as.numeric(r_nue$se),
                            pval = round(as.numeric(r_nue$pval),4),
                            label =  paste0(d02.treat[treatment==i,desc],' (n=',r_nue$k,')')
    )}
    calculates the mean (estimate), SE, pval
    safe output in a data table (stored i "out2")
    -1*100 to convert log response ratio into percentage, also do for SE 
    retrieve p-value, make p-value numeric and round with 4 decimals( otherwise file becomes too big)
    make a label so you can lateron see what happend -> what was the treatment and I paste with the number of observations
  ##convert list to vector
    combine the list to one table -> *that table can alredy be part of the report or in form of a table (n, mean, SE, value)*
    out2 <- rbindlist(out2)
    __________________________
    in my case:
      just one management practice (biochar application) and several site factors so instead of "management" take the "site factors"

#Meta-regression for main factors
    
    Below the code to do a main factor analysis for all columns selected by var.sel.
    For each model the output is saved in out.est, with the estimate, se, pvalue, and confidence intervals.
    For each model also the summary stats are saved in out.sum, with the variable name, the AIC criterium, log likelihood, and the relative improvement in log likelyhood and explained variance in comparison with an "empty" model without a "moderator" or "explanatory" variable.
    Since character variables are analyses as a factor, and numeric variables as regression, the model equation is slightly different.
    
    This analysis has to be done for each of the main factors.
    This gives information on how much of the variation in the response variable can be explained by site properties (soil, fertilizer, climate, and so on)
    
    Please, ensure that the explanatory variables do not contain NA values (they currently have)
    easiest solution is to replace them with the median value.     
    
  ## run a meta-regression model for main categorial variable
    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~factor(varsel)-1, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
    
    } else {
      
      # run a meta-regression model for main numerical variable
      r_nue_1 <- rma.mv(yi,vi, 
                        mods = ~varsel, 
                        data = d02[,.(yi,vi,studyid,varsel = get(i))],
                        random = list(~ 1|studyid), method="REML",sparse = TRUE)
    }
    
  "If" statement: If ... is true, 
  -1 means that you do not include an intercept
  you create the varsel -> a subset of the original one, take subset of the colums "response", "variance", "studyID"
  
#Meta-regression for main factors with interactions
    
    Below the code to do a meta-regression with multiple explanatory factors.
    Be aware, this is a manual process. 
    So, you start with the results of the main factor analysis, and look how the model performance will become better by adding interactions.
    This might also include adding squared or squared-root transformations to test for non-linear behaviour.
 
    *meeting 16.01 see notes*
      Is the model improving? If yes, leave that single parameter in. Next step: add an interaction
      a + b = two main effects
      a * b = a + b + a * b = main effects + the interaction between both
      for these you estimate the coefficients
      If interactions are not significant you can leave them out.
      If interactions are significant but main factors are not significant anymore. In that case scip the main factors and just add interaction
      Never add more than two interactions 
      Think before which site condition will be relevant (What has to be in there (even though they dont have a significance))
      If two groups are almost the same you can combine the groups to simplify in.
      
#merge output into a data.table
    out1.sum <- rbindlist(out1.sum)
    out1.est <- rbindlist(out1.est)
    print(out1.sum)
    print(out1.est)
    write.csv(out1.sum, file="E:/ROMresult1.csv")
    write.csv(out1.est, file="E:/ROMresult2.csv")  

Book: repole github booklets 