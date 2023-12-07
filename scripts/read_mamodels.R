# function to load the default meta-analytical models from a given filename
# and the different models are combined using inverse weighting on SD

#' @param fname (string) a filename where to find the excel with ma model results
#'
#' @export
lmam <- function(fname){
  
  # load meta-analytical models for main models without covariates
  m1.main <- as.data.table(readxl::read_xlsx(fname,sheet='main'))
  setnames(m1.main,gsub('-1','',colnames(m1.main)))
  
  # remove cases that are not correct
  m1.main<- m1.main[!(n==1 & dyr==1 & SEyr==0.1)] # "markers" in the template file
  
  # load meta-analytical models for main models with covariates, and select only relevant columns
  m1.covar <- as.data.table(readxl::read_xlsx(fname,sheet='covariates'))
  setnames(m1.covar,gsub('-1','',colnames(m1.covar)))
  m1.covar <- m1.covar[,.(ind_code, mods = `moderator/factor` ,man_code,group,SEyr,dyr,n)]
  
  # remove models without SE
  m1.covar <- m1.covar[!is.na(SEyr)]
  
  # add simplified grouping for moderators to simplify joining with integrator data later
  # groups are unique
  m1.covar[grepl('texture',mods), mods := 'cov_soil']
  m1.covar[grepl('crop',mods), mods := 'cov_crop']
  m1.covar[grepl('SOC',mods), mods := 'cov_soc']
  m1.covar[grepl('rate',mods), mods := 'cov_fert']
  m1.covar[grepl('pH',mods), mods := 'cov_ph']
  m1.covar[grepl('durat',mods), mods := 'cov_duration']
  m1.covar[grepl('climate',mods), mods := 'cov_clim']
  m1.covar <- m1.covar[!mods %in% c('cov_ph','cov_duration')]
  m1.covar[grepl('cov_soil',mods) & grepl('coarse|sand',group), group2 := 'coarse']
  m1.covar[grepl('cov_soil',mods) & grepl('medium|loam',group), group2 := 'medium']
  m1.covar[grepl('cov_soil',mods) & grepl('fine|clay|silt',group), group2 := 'fine']
  m1.covar[grepl('cov_crop',mods) & grepl('maiz',group), group2 := 'maize']
  m1.covar[grepl('cov_crop',mods) & grepl('cerea',group), group2 := 'cereals']
  m1.covar[grepl('cov_crop',mods) & grepl('root',group), group2 := 'rootcrops']
  m1.covar[grepl('cov_clim',mods) & grepl('subtropical',group), group2 := 'subtropical']
  m1.covar[grepl('cov_clim',mods) & grepl('temperate',group), group2 := 'temperate']
  m1.covar[grepl('cov_soc|cov_fert',mods) & grepl('low|no fert',group), group2 := 'low']
  m1.covar[grepl('cov_soc|cov_fert',mods) & grepl('medium',group), group2 := 'medium']
  m1.covar[grepl('cov_soc|cov_fert',mods) & grepl('high',group), group2 := 'high']
  m1.covar[grepl('cov_fert',mods),group2 := paste0('f',group2)]
  m1.covar[grepl('cov_soc',mods),group2 := paste0('c',group2)]
  m1.covar <- m1.covar[!is.na(group2)][,group := group2][,group2 := NULL]
  
  # calculate the weighed mean models per measure
  m1.main[,wm.sd := 1/sqrt(sum(1 / SEyr^2)) , by=c('ind_code','man_code')]
  m1.main[,wm.mean := sum((dyr / SEyr^2)) / sum(1 / SEyr^2) , by=c('ind_code','man_code')]
  m1.covar[,wm.sd := 1/sqrt(sum(1 / SEyr^2)) , by=c('ind_code','man_code','mods','group')]
  m1.covar[,wm.mean := sum((dyr / SEyr^2)) / sum(1 / SEyr^2) , by=c('ind_code','man_code','mods','group')]
  
  # remove duplicates
  m2.main <- unique(m1.main[,.(ind_code,man_code,wm.mean,wm.sd)])
  m2.covar <- unique(m1.covar[,.(ind_code,man_code,mods,group,wm.mean,wm.sd)])
  
  # mean model responses for main ma-models (without covariates)
  m2.mean <- dcast(m2.main,man_code ~ ind_code, value.var = 'wm.mean')
  setnames(m2.mean,colnames(m2.mean)[-1],paste0('mean_',colnames(m2.mean)[-1]))
  m2.sd <- dcast(m2.main,man_code ~ ind_code, value.var = 'wm.sd')
  setnames(m2.sd,colnames(m2.sd)[-1],paste0('sd_',colnames(m2.sd)[-1]))
  
  # mean model responses for main ma-models with covariates
  m2.cov.mean <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.mean')
  setnames(m2.cov.mean,colnames(m2.cov.mean)[-c(1:3)],paste0('mean_',colnames(m2.cov.mean)[-c(1:3)]))
  m2.cov.sd <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.sd')
  setnames(m2.cov.sd,colnames(m2.cov.sd)[-c(1:3)],paste0('sd_',colnames(m2.cov.sd)[-c(1:3)]))
  
  # Monte Carlo requires SD values, which are missing for covariates
  # Here I tried to remove them to see if that
  # if (covar==FALSE) {
  #   # add the meta-analytical models into one list
  #   out = list(ma_mean = m2.mean, ma_sd = m2.sd)
  #
  # } else {
  # add the meta-analytical models into one list
  out = list(ma_mean = m2.mean, ma_sd = m2.sd,ma_cov_mean = m2.cov.mean, ma_cov_sd = m2.cov.sd)
  # }
  
  
  # return
  return(out)
}
