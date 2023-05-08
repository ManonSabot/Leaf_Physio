library(tidyverse)
library(ggpubr)

# Upload data frame with all simulations
pmax_std_sim = read.csv('pmax_std_gmin/all_site_spp_simulations.csv') %>%
  rename(treatment = site_spp)

# Output data frames for individual simulations
drought = 
  subset(pmax_std_sim, treatment=='drought_only')[,-1]
HW = 
  subset(pmax_std_sim, treatment=='HW_only_varD_RH60')[,-1]
HW_and_drought = 
  subset(pmax_std_sim, treatment=='HW_and_drought_varD_RH60')[,-1]

# Function to reformat data frames for ease of plotting
create_plotting_df = function(df)
{
  df1 = cbind(doy = df$doy, 
               Tair = df$Tair, 
               VPD = df$VPD, 
               Ps = df$Ps,
               A = stack(df[1:4]),
               E = stack(df[5:8]),
               gs = stack(df[9:12]),
               gb = stack(df[13:16]),
               Ci = stack(df[19:22]),
               Tleaf = stack(df[23:26]),               
               Pleaf = stack(df[27:30]),
               Rublim = stack(df[33:36])
  )
  
  df1 = df1[c(6,1:5,7,9,11,13,15,17,19)]
  colnames(df1)[1] = 'model'
  df1 = mutate(df1, 
               model = str_sub(model, 3,-2),
               Tdif = Tleaf.values-Tair)
  
  df2 = cbind(doy = df$doy, 
               Tair = df$Tair, 
               VPD = df$VPD,
               gmin = stack(df[17:18]),
               k = stack(df[31:32])
               )
  df2 = mutate(df2, 
               gmin.ind = str_sub(gmin.ind, 6,-2))
  df2 = mutate(df2, 
               k.ind = str_sub(k.ind, 3,-2))
  
  output = list(df1, df2)
  return(output)
}

drought_only_out = create_plotting_df(drought)
drought_1 = drought_only_out[[1]]
drought_2 = drought_only_out[[2]]

HW_only_out = create_plotting_df(HW)
HW_1 = HW_only_out[[1]]
HW_2 = HW_only_out[[2]]

HW_and_drought_out = create_plotting_df(HW_and_drought)
HW_and_drought_1 = HW_and_drought_out[[1]]
HW_and_drought_2 = HW_and_drought_out[[2]]

plot_time_series = function(df, # Data frame for specific simulation
                            yval, # Response variable
                            ylab, # y-axis label
                            ybreaks # Specify breaks for y-axis
                            )
{
  p = ggplot(df, 
             aes(x = doy)) + 
    geom_point(size = 1.7, position=position_dodge(width=0.5), 
               aes(y = {{yval}}, color = model, shape = model)) + 
    geom_line(position=position_dodge(width=0.5), 
              aes(y = {{yval}}, linetype = model, color = model)) +
    theme_classic() + 
    scale_x_continuous(name = 'Day of year',
                       breaks = seq(1,14, by=2)) + 
    scale_y_continuous(name = ylab,
                       breaks = ybreaks) +
    scale_shape_manual(values=1:9) +
    theme(axis.title = element_text(size = 7),
          axis.text.y = element_text(size = 7))
}

plot_alldays = function(df)
{
  Avt_zoom = plot_time_series(df = df,
                              yval = A.values,
                              ylab = 'A(umol m-2 s-1)',
                              ybreaks = seq(0,20,by=5))
  Gsvt_zoom = plot_time_series(df = df,
                               yval = gs.values,
                               ylab = 'gs (mol m-2 s-1)',
                               ybreaks = seq(0,0.5,by=0.1))
  Evt_zoom = plot_time_series(df = df,
                              yval = E.values,
                              ylab = 'E (mmol m-2 s-1)',
                              ybreaks = seq(0,8,by=2))
  Pleafvt_zoom = plot_time_series(df = df,
                                  yval = Pleaf.values,
                                  ylab = 'LWP (MPa)',
                                  ybreaks = seq(-3,0,by=1))
  Tleafvt_zoom = plot_time_series(df = df,
                                  yval = Tleaf.values,
                                  ylab = 'Tleaf (°C)',
                                  ybreaks = seq(25,55,by=10)) +
    geom_line(aes(y=Tair))
  Tdifvt_zoom = plot_time_series(df = df,
                                 yval = Tdif,
                                 ylab = 'Tleaf-Tair (°C)',
                                 ybreaks = seq(0,2,by=1)) + 
    scale_y_continuous(limits = c(0,2), name = 'Tleaf-Tair (°C)')
  
  plots = list(Avt_zoom, Gsvt_zoom, Evt_zoom, Pleafvt_zoom, Tleafvt_zoom, 
               Tdifvt_zoom)
  final_plot = ggarrange(plots[[1]] + rremove('xlab') + rremove('x.text'), 
                         plots[[2]] + rremove('xlab') + rremove('x.text'),
                         plots[[3]] + rremove('xlab') + rremove('x.text'),
                         plots[[4]] + rremove('xlab') + rremove('x.text'),
                         plots[[5]] + rremove('xlab') + rremove('x.text'),
                         plots[[6]],
                         ncol = 1, nrow=6, common.legend = T)
}

plot_HWzoom = function(df)
{
  Avt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                 yval = A.values,
                                 ylab = 'A(umol m-2 s-1)',
                                 ybreaks = seq(0,20,by=5))
  Gsvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                  yval = gs.values,
                                  ylab = 'gs (mol m-2 s-1)',
                                  ybreaks = seq(0,0.5,by=0.1))
  Evt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                 yval = E.values,
                                 ylab = 'E (mmol m-2 s-1)',
                                 ybreaks = seq(0,8,by=2))
  Pleafvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                     yval = Pleaf.values,
                                     ylab = 'LWP (MPa)',
                                     ybreaks = seq(-3,0,by=1))
  Tleafvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                     yval = Tleaf.values,
                                     ylab = 'Leaf temperature (°C)',
                                     ybreaks = seq(-3,0,by=1)) +
    geom_line(aes(y=Tair))
  
  plots = list(Avt_zoom, Gsvt_zoom, Evt_zoom, Pleafvt_zoom, Tleafvt_zoom)
  final_plot = ggarrange(plots[[1]] + rremove('xlab') + rremove('x.text'), 
                         plots[[2]] + rremove('xlab') + rremove('x.text'),
                         plots[[3]] + rremove('xlab') + rremove('x.text'),
                         plots[[4]] + rremove('xlab') + rremove('x.text'),
                         plots[[5]],
                         ncol = 1, nrow=5, common.legend = T)
}


drought_plots = plot_alldays(drought_1)
drought_plots

HW_plots = plot_alldays(HW_1)
HW_plots

HWdrought_plots = plot_alldays(HW_and_drought_1)
HWdrought_plots


HW_zoom_plots = plot_HWzoom(HW_1)
HW_zoom_plots

HWdrought_zoom_plots = plot_HWzoom(HW_and_drought_1)
HWdrought_zoom_plots
