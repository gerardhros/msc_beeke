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
m1.main <- read_excel("data/meta_model/meta_model_R_copy1.xlsx", sheet = 4)
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

#make all values of "wm_mean" positive
m1.main.merged$'wm_mean' <- abs(m1.main.merged$'wm_mean')

#fwrite(m1.main.merged, file = "data/meta_model/m1.main.merged.csv")

#vilualize 

library(ggplot2)
m1.main.merged$ind_code <- gsub("base saturation", "base\nsaturation", m1.main.merged$ind_code)
m1.main.merged$ind_code <- gsub("soil water content", "soil water\ncontent", m1.main.merged$ind_code)
m1.main.merged$ind_code <- gsub("Y", "crop\nyield", m1.main.merged$ind_code)


bar_m1_merged <- ggplot(m1.main.merged, 
                        aes(x = ind_code, y = wm_mean, 
                            fill = ifelse(ind_code == "bulk density", "olivedrab", "darkgrey"))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = wm_mean - wm_sd, ymax = wm_mean + wm_sd), width = 0.2) +
  scale_fill_identity() +
  labs(x = "", y = "Weighted Mean ES (%)", 
       title = "Weighted Mean ES and Standard Deviation by Indicator") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"))

bar_m1_merged

ggsave(filename = "C:/Users/beeke/OneDrive/Wageningen/Master thesis/R Studio/msc_beeke/figures/model/bar_m1_merged.jpg", 
       plot = bar_m1_merged, 
       width = 20, height = 7, units = "cm")

#______________________________________________________________________________
# Covariates

m1.covar <- read_excel("data/meta_model/meta_model_R_copy1.xlsx", sheet = 5)
m1.covar <- as.data.table(m1.covar)
m1.covar <- m1.covar[,.(ind_code, mod1 = `moderator 1`, group1 = `group 1`, mod2=`moderator 2`, group2=`group 2`, ES, SE, n)]

#make all ES of porosity and bulk density positive
m1.covar$'ES' <- abs(m1.covar$'ES')


#only by ind_code and moderator 1 to get the average estimates of certain combinations of groups
#m1.covar[, wm.sd := 1/sqrt(sum(1 / SE^2)), by = c('ind_code','mod1')]
#m1.covar[, wm.mean := sum((ES / SE^2)) / sum(1 / SE^2) , by = c('ind_code','mod1')]

#m1.covar[,c('group1','group2','ES','SE','n') := NULL]


fwrite(m1.covar, file = "data/meta_model/m1.cov.indivitual_wmES.csv")

# --- test ----
library(ggplot2)
library(dplyr)
library(patchwork)

# Calculate summary statistics
m1.cov2 <- m1.covar[, .(pmin = min(ES),
                        pmax = max(ES),
                        pmean = sum((ES / SE^2)) / sum(1 / SE^2),
                        psd = 1/sqrt(sum(1 / SE^2))), by = .(ind_code, mod1)]

# Remove SOC and soil water content
#m1.cov2 <- m1.cov2[!(m1.cov2$ind_code == "SOC" | m1.cov2$ind_code == "SWC"), ]


# Ensure that pmin is not equal to pmax for visualization purposes
m1.cov2[pmin == pmax, c('pmin', 'pmax') := .(pmin * 0.99, pmax * 1.01)]

# Define consistent factor levels for all plots
all_levels <- c('bc dose', 'bc ph', 'bc c content', 'bc n content', 'bc C:N', 'bc hydrogen', 'bc CEC', 'ash content', 'particle size',
                'soil ph', 'SOC', 'soil N', 'soil C:N', 'SSA', 'CEC',
                'soil texture', 'bulk density', 
                'N rate', 'climate', 'crop type', 'water management', 'time frame')

# Adjust mod1 based on all_levels and mod1n for consistent plot heights
m1.cov2[, mod1 := factor(mod1, levels = all_levels)]
m1.cov2[, mod1n := as.integer(factor(mod1, levels = all_levels))]


fwrite(m1.cov2, file = "data/meta_model/m1.cov.wmES.csv")

plot_list <- list()

# Generate plots for each ind_code
for (i in unique(m1.cov2$ind_code)) {
  # Subset data for the current ind_code
  current_plot_data <- m1.cov2[ind_code == i]
  current_ind_data <- m1.covar[ind_code == i & mod1 %in% current_plot_data$mod1]
  current_ind_data[, mod1n := as.integer(factor(mod1, levels = all_levels))]
  
  # Create the plot
  p <- ggplot(current_plot_data, aes(x = pmean, y = mod1n)) +
    geom_segment(aes(x = pmin, xend = pmax, y = mod1n, yend = mod1n), color = "grey", size = 3, lineend = "round") +
    geom_point(data = current_ind_data, aes(x = ES), color = 'darkorange', size = 1.5, alpha = 0.5) +
    geom_point(color = 'olivedrab', size = 2) +
    #geom_errorbar(aes(ymin = mod1n, ymax = mod1n, xmin = pmean - psd, xmax = pmean + psd), width = 0.5) +
    scale_y_continuous(name = '', 
                       breaks = 1:length(all_levels), 
                       labels = all_levels, 
                       limits = c(0.5, length(all_levels) + 0.5)) +
    xlim(0, 75) + 
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14)) +
    labs(x = paste('Change in', i, '(%)'))
  
  # Store the plot
  plot_list[[i]] <- p
}

#2x3 plot
six_plots <- plot_list[c("yield", "N pool", "WHC", "bulk density", "porosity", "soil pH", "SOC", "SWC")]
six_plots <- six_plots[1:6]

#second_two_plots <- plot_list[5:6]

combined_plot <- wrap_plots(six_plots, ncol = 2, nrow = 3) +
  plot_annotation(tag_levels = 'a') + 
  plot_annotation(title = 'Change due to Biochar application', theme = theme(plot.title = element_text(hjust = 0.5, size = 18))) +
  plot_layout(guides = 'collect') +
  theme(plot.tag = element_text(size = 12))

combined_plot

# Save the first combined plot
ggsave(filename = "figures/model/combined_plot.jpg", 
       plot = combined_plot, width = 30, height = 35, units = "cm")










##two 2x2 plots
#first_two_plots <- plot_list[1:4]
#second_two_plots <- plot_list[5:8]

#apply_theme <- function(p) {
  p + theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 11),
          plot.background = element_rect(fill = "white", colour = "white"))
}

#theme_set(theme(plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm")))


#combined_plot_first <- wrap_plots(six_plots, ncol = 2, nrow = 3) +
  plot_annotation(tag_levels = 'a') + 
  plot_annotation(title = 'Change due to biochar application', theme = theme(plot.title = element_text(hjust = 0.5, size = 18))) +
  plot_layout(guides = 'collect') +
  theme(plot.tag = element_text(size = 12))

#combined_plot_first

#combined_plot_second <- wrap_plots(second_four_plots, ncol = 2, nrow = 2) +
#plot_annotation(tag_levels = list(c('e', 'f', 'g', 'h'))) +
  plot_annotation(title = 'Change due to biochar application', theme = theme(plot.title = element_text(hjust = 0.5, size = 18))) +
  plot_layout(guides = 'collect')

#combined_plot_second

# Save the first combined plot
#ggsave(filename = "figures/model/combined_plot_first.jpg", 
       plot = combined_plot_first, width = 30, height = 22, units = "cm")

# Save the second combined plot
#ggsave(filename = "figures/model/combined_plot_second.jpg", 
       plot = combined_plot_second, width = 30, height = 22, units = "cm")























#___________________________________________
m1.cov2 <- m1.covar[,list(pmin = min(ES),
                          pmax = max(ES),
                          pmean = sum((ES / SE^2)) / sum(1 / SE^2),
                          psd = 1/sqrt(sum(1 / SE^2))),by=c('ind_code','mod1')]

m1.cov2[,mod1n := as.integer(as.factor(mod1))]
m1.cov2[pmin==pmax,c('pmin','pmax') := list(pmin * 0.99, pmax * 1.01)]

# Define all levels
all_levels <- c('bc dose','bc ph','bc c content', 'bc n content', 'bc C:N', 'bc hydrogen', 'bc CEC' ,'ash content', 'particle size',
                'soil ph','SOC','soil N','soil C:N', 'SSA', 'CEC',
                'soil texture','bulk density',
                'N rate','climate','crop type','water management','time frame')
m1.cov2[, mod1 := factor(mod1, levels = all_levels)]

for(i in unique(m1.cov2$ind_code)){
  
  i = 'Y'
  
  # subset only the moderators that are present (so skipping NA)
  m1.cov2.plot <-  m1.cov2[ind_code==i]
  
    
  # Include only relevant levels for the current ind_code
  relevant_levels <- all_levels[all_levels %in% unique(m1.cov2[ind_code == i, mod1])]
  
  m1.cov2.plot[,mod2 := factor(mod1, levels = relevant_levels)]
  m1.cov2.plot[,mod1n := length(relevant_levels) - as.numeric(mod2) + 1]
  m1.cov2.plot[,mod1n := 18-as.numeric(mod2)]

  # collect individual estimates
  m1.cov2.ind <- m1.covar[ind_code == i & mod1 %in% m1.cov2.plot$mod1]
  
  # Include only relevant levels for the current ind_code in m1.cov2.ind
  relevant_levels_ind <- all_levels[all_levels %in% unique(m1.cov2.ind$mod1)]
  
  m1.cov2.ind[,mod2 := factor(mod1, levels = relevant_levels_ind)]
  m1.cov2.ind[,mod1n := length(relevant_levels_ind) - as.numeric(mod2) + 1]
  m1.cov2.ind[,mod1n := 18-as.numeric(mod2)]
}

# make the plot
p1 <- m1.cov2.plot %>% ggplot()+
  geom_rect(aes(xmin = pmin,xmax = pmax,
                ymin=mod1n - 0.2,
                ymax=mod1n+0.2),fill = 'blue',alpha=0.2) + 
  xlim(0,80) + theme_bw()+
  scale_y_continuous(name ='mods', 
                     breaks = m1.cov2.plot$mod1n, 
                     labels = m1.cov2.plot$mod2, 
                     limits = c(1.5,17.5))+
  geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
  geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
  theme(panel.grid.minor = element_blank())+
  ylab('covariate controlling yield response')+
  xlab('change in yield (%)') +
  ggtitle('change in yield due to biochar application (%)') +
  geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)
p1

# make the plot
p2 <- m1.cov2.plot %>% ggplot()+
  geom_rect(aes(xmin = pmin,xmax = pmax,
                ymin=mod1n - 0.2,
                ymax=mod1n+0.2),fill = 'blue',alpha=0.2) + 
  xlim(0,80) + theme_bw()+
  scale_y_continuous(name ='mods', 
                     breaks = m1.cov2.plot$mod1n, 
                     labels = m1.cov2.plot$mod2, 
                     limits = c(1.5,17.5))+
  geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
  geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
  theme(panel.grid.minor = element_blank())+
  ylab('covariate controlling N pool response')+
  xlab('change in N pool (%)') +
  ggtitle('change in N pool due to biochar application (%)') +
  geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)
p2

# make the plot
p3 <- m1.cov2.plot %>% ggplot()+
  geom_rect(aes(xmin = pmin,xmax = pmax,
                ymin=mod1n - 0.2,
                ymax=mod1n+0.2),fill = 'olivedrab',alpha=0.2) + 
  xlim(0,80) + theme_bw()+
  scale_y_continuous(name ='mods', 
                     breaks = m1.cov2.plot$mod1n, 
                     labels = m1.cov2.plot$mod2, 
                     limits = c(1.5,17.5))+
  geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
  geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
  theme(panel.grid.minor = element_blank())+
  ylab('covariate controlling bulk density response')+
  xlab('change in bulk density (%)') +
  ggtitle('change in bulk density due to x (%)') +
  geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)

require(patchwork)
p4 <- p1 + p2 + p3
p4 
ggsave(filename = "figures/model/p4.jpg", 
       plot = p4, 
       width = 20, height = 7, units = "cm")

# make the plot
p5 <- m1.cov2.plot %>% ggplot()+
  geom_rect(aes(xmin = pmin,xmax = pmax,
                ymin=mod1n - 0.2,
                ymax=mod1n+0.2),fill = 'blue',alpha=0.2) + 
  xlim(0,80) + theme_bw()+
  scale_y_continuous(name ='mods', 
                     breaks = m1.cov2.plot$mod1n, 
                     labels = m1.cov2.plot$mod2, 
                     limits = c(1.5,17.5))+
  geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
  geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
  theme(panel.grid.minor = element_blank())+
  ylab('covariate controlling yield response')+
  xlab('change in yield (%)') +
  ggtitle('change in yield due to x (%)') +
  geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)
p5



#___________________________________________________________

# --- test ----

m1.cov2[,mod1n := as.integer(as.factor(mod1))]
m1.cov2[pmin==pmax,c('pmin','pmax') := list(pmin * 0.99, pmax * 1.01)]


for(i in unique(m1.cov2$ind_code)){
  
  i = 'Y'
  
  # subet only the moderaters that are present (so skipping NA)
  m1.cov2.plot <-  m1.cov2[ind_code==i]
  m1.cov2.plot[,mod2 := factor(mod1,
                               levels = c('bc dose','bc ph','bc c content', 'bc C:N','ash content',
                                          'soil ph','SOC','soil N','soil C:N',
                                          'soil texture','bulk density',
                                          'N rate','climate','crop type','water management','time frame'))
  ]
  m1.cov2.plot[,mod1n := 18-as.numeric(mod2)]
  
  # collect individual estimates
  m1.cov2.ind <- m1.covar[ind_code == i & mod1 %in% m1.cov2.plot$mod1]
  m1.cov2.ind[,mod2 := factor(mod1,
                              levels = c('bc dose','bc ph','bc c content', 'bc C:N','ash content',
                                         'soil ph','SOC','soil N','soil C:N',
                                         'soil texture','bulk density',
                                         'N rate','climate','crop type','water management','time frame'))
  ]
  m1.cov2.ind[,mod1n := 18-as.numeric(mod2)]
  m1.cov2.ind <- unique(m1.cov2.ind[,.(ind_code,mod2,ES,mod1n)])
  
  # make the plot
  p1 <- m1.cov2.plot %>% ggplot()+
    geom_rect(aes(xmin = pmin,xmax = pmax,
                  ymin=mod1n - 0.2,
                  ymax=mod1n+0.2),fill = 'blue',alpha=0.2) + 
    xlim(0,80) + theme_bw()+
    scale_y_continuous(name ='mods', 
                       breaks = m1.cov2.plot$mod1n, 
                       labels = m1.cov2.plot$mod2, 
                       limits = c(1.5,17.5))+
    geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
    geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
    theme(panel.grid.minor = element_blank())+
    ylab('covariate controlling yield response')+
    xlab('change in yield (%)') +
    ggtitle('change in yield due to biochar application (%)') +
    geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)
  
  ggsave(plot = p1, filename = paste0('products/image_',tolower(gsub(' ','_',i)),'.jpg'),
         width = 12,height = 10,units='cm') 
  
}

# make the plot
p1 <- m1.cov2.plot %>% ggplot()+
  geom_rect(aes(xmin = pmin,xmax = pmax,
                ymin=mod1n - 0.2,
                ymax=mod1n+0.2),fill = 'blue',alpha=0.2) + 
  xlim(0,80) + theme_bw()+
  scale_y_continuous(name ='mods', 
                     breaks = m1.cov2.plot$mod1n, 
                     labels = m1.cov2.plot$mod2, 
                     limits = c(1.5,17.5))+
  geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
  geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
  theme(panel.grid.minor = element_blank())+
  ylab('covariate controlling yield response')+
  xlab('change in yield (%)') +
  ggtitle('change in yield due to x (%)') +
  geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)

# make the plot
p2 <- m1.cov2.plot %>% ggplot()+
  geom_rect(aes(xmin = pmin,xmax = pmax,
                ymin=mod1n - 0.2,
                ymax=mod1n+0.2),fill = 'blue',alpha=0.2) + 
  xlim(0,80) + theme_bw()+
  scale_y_continuous(name ='mods', 
                     breaks = m1.cov2.plot$mod1n, 
                     labels = m1.cov2.plot$mod2, 
                     limits = c(1.5,17.5))+
  geom_point(aes(x=pmean,y=mod1n),col='black',size=3,alpha=0.8) +
  geom_errorbar(aes(y=mod1n,xmin = pmean - psd, xmax = pmean + psd),linewidth=0.8,width=0.4)+
  theme(panel.grid.minor = element_blank())+
  ylab('covariate controlling yield response')+
  xlab('change in yield (%)') +
  ggtitle('change in yield due to x (%)') +
  geom_point(data = m1.cov2.ind,aes(x=ES,y=mod1n),col='red',size=2)


require(patchwork)
p3 <- p1 + p2


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


