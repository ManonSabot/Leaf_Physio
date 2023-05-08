#Load packages
library(tidyverse)
library(ggpubr)

# LOAD AND ORGANIZE DATA----

# Output data frame with all simulations
all_simulations = read.csv(
  'HW_drought_test/all_site_spp_simulations.csv') %>%
  rename(treatment = site_spp)


# Output data frames for individual simulations
drought_only = 
  subset(all_simulations, treatment=='drought_only')[,-1]
HW_only_constD = 
  subset(all_simulations, treatment=='HW_only_constD')[,-1]
HW_only_varD_pwr = 
  subset(all_simulations, treatment=='HW_only_varD_pwr')[,-1]
HW_only_varD_RH60 = 
  subset(all_simulations, treatment=='HW_only_varD_RH60')[,-1]
HW_and_drought_constD = 
  subset(all_simulations, treatment=='HW_and_drought_constD')[,-1]
HW_and_drought_varD_pwr = 
  subset(all_simulations, treatment=='HW_and_drought_varD_pwr')[,-1]
HW_and_drought_varD_RH60 = 
  subset(all_simulations, treatment=='HW_and_drought_varD_RH60')[,-1]

# Define function to reorganize data frames for plotting
create_plotting_df = function(df)
{
  df_1 = cbind(doy = df$doy, 
               Tair = df$Tair, 
               VPD = df$VPD, 
               Ps = df$Ps,
               A = stack(df[1:9]),
               E = stack(df[10:18]),
               gs = stack(df[19:27]),
               gb = stack(df[28:36]),
               Ci = stack(df[38:46]),
               Tleaf = stack(df[47:55]),               
               Pleaf = stack(df[56:64]),
               Rublim = stack(df[65:73]),
               ref = stack(df[74:76]),
               gmin.std2. = df$gmin.std2.
  )
  return(df_1)
}

# Apply function to all data frames
drought_only_1 = create_plotting_df(drought_only)
HW_only_constD = create_plotting_df(HW_only_constD)
HW_only_varD_pwr = create_plotting_df(HW_only_varD_pwr)
HW_only_varD_RH60 = create_plotting_df(HW_only_varD_RH60)
HW_and_drought_constD = create_plotting_df(HW_and_drought_constD)
HW_and_drought_varD_pwr = create_plotting_df(HW_and_drought_varD_pwr)
HW_and_drought_varD_RH60 = create_plotting_df(HW_and_drought_varD_RH60)

# Define function to plot time series
plot_time_series = function(df, yval, yind, ref, scaling, 
                            ylab, reflab, ybreaks){
  p = ggplot(df, 
             aes(x = doy)) + 
    geom_point(size = 1.7, position=position_dodge(width=0.5), 
               aes(y = {{yval}}, color = {{yind}})) + 
    geom_line(position=position_dodge(width=0.5), 
              aes(y = {{yval}}, linetype = {{yind}}, color = {{yind}})) +
    geom_line(position=position_dodge(width=0.5), aes(y = {{ref}}/scaling)) +
    theme_classic() + 
    scale_x_continuous(name = 'Day of year') + 
    scale_y_continuous(name = ylab,
                       breaks = ybreaks,
                       sec.axis = sec_axis(~. * scaling,
                                           name = reflab))
}

HW_Tleaf_v_time = plot_time_series(df = HW_only_constD, 
                 y = Tleaf.values, 
                 yind = Tleaf.ind, 
                 ref = Tair, 
                 scaling = 100, 
                 ylab = "Stomatal conductance (Tleaf)", 
                 reflab = 'Air temperature (°C)', 
                 ybreaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4)
                 )
HW_Tleaf_v_time

# Plot time series of stomatal conductance for heatwave only simulation
HW_gs_v_time = ggplot(HW_only_data, 
           aes(x = hour)) + 
  geom_point(size = 1.7, position=position_dodge(width=0.5), 
             aes(y = gs.values, shape = gs.ind, color = gs.ind)) + 
  geom_line(position=position_dodge(width=0.5), 
            aes(y = gs.values, linetype = gs.ind, color = gs.ind)) +
  geom_line(position=position_dodge(width=0.5), aes(y = Tair/100)) +
  theme_classic() + 
  scale_x_continuous(name = "Hours", breaks = c(24,48,72,96,120,144,168)) + 
  scale_y_continuous(name = "Stomatal conductance (gs)",
                     breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4),
                     sec.axis = sec_axis(~. * 100,
                                         name = 'Air temperature (°C)'))
# View plot
HW_gs_v_time

# Plot time series of leaf temperatures for all plants
HW_Tleaf_v_time = ggplot(
  subset(HW_only_data, Tleaf.values != 9999 & hour > 15 & hour < 180 & hod==12), 
                      aes(x = hour)) + 
  geom_point(size = 1.7, position=position_dodge(width=0.5), 
             aes(y = Tleaf.values, shape = Tleaf.ind, color = Tleaf.ind)) + 
  geom_line(position=position_dodge(width=0.5), 
            aes(y = Tleaf.values, linetype = Tleaf.ind, color = Tleaf.ind)) +
  geom_line(position=position_dodge(width=0.5), aes(y = Tair)) +
  theme_classic() + 
  scale_x_continuous(name = "Hours", breaks = c(24,48,72,96,120,144,168)) + 
  scale_y_continuous(name = "Leaf temperature (°C)",
                     sec.axis = sec_axis(~.*1,
                                         name = 'Air temperature (°C)'))
# View plot
HW_Tleaf_v_time


#Create faceted plot of time series for individual plants
colors  <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40")
gsvtime_all_plot = p +
  facet_wrap(~doy)+
  theme(strip.background = element_blank())+
  theme(strip.background =element_rect(fill="lightgrey"))
gsvtime_all_plot


#CREATE PLOTTING FUNCTIONS----
#Define function for plotting predictions vs observations
pred_v_obs_plot = function(y){
  ggplot(HW_drought_test,
         aes(Tleaf-Tair, y-Tair))+
    geom_point(aes(color=Tair)) +
    scale_color_gradient(low = 'blue', high = 'orange') +
    geom_smooth(method='lm', color='black')+
    theme_classic() +
    labs(x = "Observed Tleaf-Tair")+
    geom_abline(slope=1,color='red')
}


#Define function for plotting different predictions against one another
comp_pred_plot = function(x, y){
  ggplot(HW_drought_test,
         aes(x-Tair, y-Tair))+
    geom_point(aes(color=Tair)) +
    scale_color_gradient(low = 'blue', high = 'orange') +
    geom_smooth(method='lm', color='black')+
    theme_classic() +
    geom_abline(slope=1,color='red')
}

#Define function for adding a regression line to plot
##Doesn't work :(
add_regression = function(plot, x, y){
  plot +
    stat_cor(all_simulations,
             aes(x, y, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             label.x = 15, label.y = 36
    ) +
    stat_regline_equation(all_simulations,
                          aes(x, y),
                          label.x = 15, label.y = 38)
}

#PREDICTIONS VS MEDLYN PLOTS----

# Medlyn with variable gmin
const_v_std_plot = comp_pred_plot(
  x = HW_only$Tleaf.std.,
  y = HW_only$Tleaf.const.
)
typeof(HW_only$Tleaf.const.)

const_v_std_plot +
  stat_cor(data =HW_only,
           aes(x=Tleaf.std.-Tair, y=Tleaf.const.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0.5, label.y = 1.8
  )+
  stat_regline_equation(data=HW_only,
                        aes(x=Tleaf.std.-Tair, y=Tleaf.const.-Tair),
                        label.x = 0.5, label.y = 1.9) +
  labs(x = "Medlyn Tleaf-Tair",
       y = "Tleaf-Tair (const)")


#PREDICTIONS VS OBSERVATIONS PLOTS----

#Medlyn
Medlyn_v_obs_plot = pred_v_obs_plot(
  y = all_simulations$Tleaf.std.) +
  labs(y = "Predicted Tleaf-Tair (Medlyn)") +
  theme(legend.position="none")
Medlyn_v_obs_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf-Tair, y=Tleaf.std.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = -2.5, label.y = 1.7
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf-Tair, y=Tleaf.std.-Tair),
                        label.x = -2.5, label.y = 2)


# TEMPERATURE PLOTS----
Tdiff_plot = ggplot(Tdiff_df,
                    aes(x = Tair, y = Tdiff))+
  geom_point(aes(color=factor(site_spp))) +
  geom_smooth(method='lm', color='black')+
  theme_classic() +
  labs(x = "Tair",
       y = "Tleaf-Tair")

Tdiff_plot +
  stat_cor(data=Tdiff_df,
           aes(x=Tair, y=Tdiff, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 20, label.y = 65
  )+
  stat_regline_equation(data=Tdiff_df,
                        aes(x=Tair, y=Tdiff),
                        label.x = 20, label.y = 70)
# ADDITIONAL PLOTS----
ggplot(new_simulations,
       aes(x = Tleaf.std2., y = gmin.std2.))+
  geom_point(aes(color=Tair)) +
  scale_color_gradient(low = 'blue', high = 'orange') +
  geom_smooth(method='lm', color='black')+
  theme_classic() +
  labs(x = "Predicted Tleaf",
       y = 'Predicted minimum stomatal conductance')+
  geom_abline(slope=1,color='red')

ggplot(filter(new_simulations, Tleaf.std2. < 30),
       aes(x = Tleaf.std2., y = gmin.std2.))+
  geom_point(aes(color=Tair)) +
  scale_color_gradient(low = 'blue', high = 'orange') +
  geom_smooth(method='lm', color='black')+
  theme_classic() +
  labs(x = "Predicted Tleaf",
       y = 'Predicted minimum stomatal conductance')+
  geom_abline(slope=1,color='red')

ggplot(new_simulations,
       aes(x = gs.Treg., y = gs))+
  geom_point(aes(color=site_spp)) +
  geom_smooth(method='lm', color='black')+
  theme_classic() +
  labs(x = "Predicted gs",
       y = 'Observed gs')+
  geom_abline(slope=1,color='red') +
  stat_regline_equation(data=new_simulations,
                        aes(x=gs.Treg., y=gs))

pred = data.frame(new_simulations$gs, 
                  new_simulations$gs.Treg.,
                  new_simulations$Tair)
View(pred)
