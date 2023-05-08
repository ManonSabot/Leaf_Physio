library(tidyverse)
library(ggpubr)
library(plantecophys)
library(tealeaves)

# Define functions ----

# Function to reformat data frames for ease of plotting
create_plotting_df = function(df)
{
  df1 = cbind(doy = df$doy, 
              Tair = df$Tair, 
              VPD = df$VPD, 
              Ps = df$Ps,
              A = stack(df[1:6]),
              E = stack(df[7:12]),
              gs = stack(df[13:18]),
              gb = stack(df[19:24]),
              Ci = stack(df[25:30]),
              Tleaf = stack(df[31:36]),               
              Pleaf = stack(df[37:42]),
              Rublim = stack(df[43:48])
  )
  
  df1 = df1[c(6,1:5,7,9,11,13,15,17,19)]
  colnames(df1)[1] = 'gs_value'
  df1 = mutate(df1, 
               gs_value = str_sub(gs_value, 8,-2),
               Tdif.values = Tleaf.values-Tair)
  
  return(df1)
}

plot_time_series = function(df, # Data frame for specific simulation
                            yval, # Response variable
                            ylab, # y-axis label
                            ybreaks, # Specify breaks for y-axis
                            ylim = NULL # Specify limits of y-axis
)
{
  if(is.null(ylim))
  {
    p = ggplot(df, 
               aes(x = doy)) + 
      geom_point(size = 1.7, position=position_dodge(width=0.5), 
                 aes(y = {{yval}}, color = gs_value, shape = gs_value)) + 
      geom_line(position=position_dodge(width=0.5), 
                aes(y = {{yval}}, linetype = gs_value, color = gs_value)) +
      theme_classic() + 
      scale_x_continuous(name = 'Day of year',
                         breaks = seq(1,14, by=2)) + 
      scale_y_continuous(name = ylab,
                         breaks = ybreaks) +
      scale_shape_manual(values=1:9) +
      theme(axis.title = element_text(size = 7),
            axis.text.y = element_text(size = 7))
  }
  p = ggplot(df, 
             aes(x = doy)) + 
    geom_point(size = 1.7, position=position_dodge(width=0.5), 
               aes(y = {{yval}}, color = gs_value, shape = gs_value)) + 
    geom_line(position=position_dodge(width=0.5), 
              aes(y = {{yval}}, linetype = gs_value, color = gs_value)) +
    theme_classic() + 
    scale_x_continuous(name = 'Day of year',
                       breaks = seq(1,14, by=2)) + 
    scale_y_continuous(name = ylab,
                       breaks = ybreaks,
                       limits = ylim) +
    scale_shape_manual(values=1:9) +
    theme(axis.title = element_text(size = 7),
          axis.text.y = element_text(size = 7))
}

plot_alldays = function(df)
{
  Avt = plot_time_series(df = df,
                         yval = A.values,
                         ylab = 'A (umol m-2 s-1)',
                         ybreaks = seq(0,20,by=5))
  Gsvt = plot_time_series(df = df,
                          yval = gs.values,
                          ylab = 'gs (mol m-2 s-1)',
                          ybreaks = seq(0,0.5,by=0.1))
  Evt = plot_time_series(df = df,
                         yval = E.values,
                         ylab = 'E (mmol m-2 s-1)',
                         ybreaks = seq(0,20,by=5))
  Pleafvt = plot_time_series(df = df,
                             yval = Pleaf.values,
                             ylab = 'LWP (MPa)',
                             ybreaks = seq(-10,0,by=2))
  Tleafvt = plot_time_series(df = df,
                             yval = Tleaf.values,
                             ylab = 'Tleaf (°C)',
                             ybreaks = seq(25,55,by=10)) +
    geom_line(aes(y=Tair))
  Tdifvt = plot_time_series(df = df,
                            yval = Tdif.values,
                            ylab = 'Tleaf-Tair (°C)',
                            ybreaks = seq(-1, 1.4, by=0.6),
                            ylim = c(-1.06,1.7))
  
  plots = list(Avt, Gsvt, Evt, Pleafvt, Tleafvt, Tdifvt)
  final_plot = ggarrange(plots[[1]] + rremove('xlab') + rremove('x.text'), 
                         plots[[2]] + rremove('xlab') + rremove('x.text'),
                         plots[[3]] + rremove('xlab') + rremove('x.text'),
                         plots[[4]] + rremove('xlab') + rremove('x.text'),
                         plots[[5]] + rremove('xlab') + rremove('x.text'),
                         plots[[6]],
                         ncol = 1, nrow=6, legend = 'none')
}

plot_HWzoom = function(df)
{
  Avt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                              yval = A.values,
                              ylab = 'A (umol m-2 s-1)',
                              ybreaks = seq(0,2,by=0.5))
  Gsvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                               yval = gs.values,
                               ylab = 'gs (mol m-2 s-1)',
                               ybreaks = seq(0,0.5,by=0.1))
  Evt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                              yval = E.values,
                              ylab = 'E (mmol m-2 s-1)',
                              ybreaks = seq(0,20,by=5))
  Pleafvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                  yval = Pleaf.values,
                                  ylab = 'LWP (MPa)',
                                  ybreaks = seq(-10,0,by=2))
  Tleafvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                                  yval = Tleaf.values,
                                  ylab = 'Tleaf (°C)',
                                  ybreaks = seq(44,54, by=2)) +
    geom_line(aes(y=Tair))
  Tdifvt_zoom = plot_time_series(df = subset(df, doy > 6 & doy <13),
                            yval = Tdif.values,
                            ylab = 'Tleaf-Tair (°C)',
                            ybreaks = seq(-1, 1.4, by=0.6),
                            ylim = c(-1.06,1.7))
  
  plots = list(Avt_zoom, Gsvt_zoom, Evt_zoom, Pleafvt_zoom, Tleafvt_zoom, 
               Tdifvt_zoom)
  final_plot = ggarrange(plots[[1]] + rremove('xlab') + rremove('x.text'), 
                         plots[[2]] + rremove('xlab') + rremove('x.text'),
                         plots[[3]] + rremove('xlab') + rremove('x.text'),
                         plots[[4]] + rremove('xlab') + rremove('x.text'),
                         plots[[5]] + rremove('xlab') + rremove('x.text'),
                         plots[[6]],
                         ncol = 1, nrow=6, legend = 'none')
}


# With original Tleaf equation----
## Data upload----
const_gs_out = read.csv('HW_drought_ConstGs/all_site_spp_simulations.csv') 

drought = 
  subset(const_gs_out, site_spp=='drought_only')[,-1]
HW = 
  subset(const_gs_out, site_spp=='HW_only_varD_RH60')[,-1]
HW_and_drought = 
  subset(const_gs_out, site_spp=='HW_and_drought_varD_RH60')[,-1]


## Plotting----
all_simulations = list(drought, HW, HW_and_drought)
all_simulations_1 = lapply(all_simulations, create_plotting_df)
all_plots = lapply(all_simulations_1, plot_alldays)

drought_plots = all_plots[[1]]
HW_plots = all_plots[[2]]
HW_and_drought_plots = all_plots[[3]]

drought_plots
HW_plots
HW_and_drought_plots

HW_simulations = all_simulations_1[c(2,3)]
all_HW_zoom_plots = lapply(HW_simulations, plot_HWzoom)

HW_zoom_plots = all_HW_zoom_plots[[1]]
HW_and_drought_zoom_plots = all_HW_zoom_plots[[2]]

HW_zoom_plots
HW_and_drought_zoom_plots

# With new Tleaf equation----

## Data upload----
const_gs_out_v2 = read.csv(
  'HW_drought_ConstGs_v2/all_site_spp_simulations.csv') %>%
  rename(simulation = site_spp)

drought_v2 = 
  subset(const_gs_out_v2, simulation=='drought_only')[,-1]
HW_v2 = 
  subset(const_gs_out_v2, simulation=='HW_only_varD_RH60')[,-1]
HW_and_drought_v2 = 
  subset(const_gs_out_v2, simulation=='HW_and_drought_varD_RH60')[,-1]

all_simulations_v2 = list(drought_v2, HW_v2, HW_and_drought_v2)

# Plotting----

all_simulations_v2_1 = lapply(all_simulations_v2, create_plotting_df)
all_plots_v2 = lapply(all_simulations_v2_1, plot_alldays)

drought_plots_v2 = all_plots_v2[[1]]
HW_plots_v2 = all_plots_v2[[2]]
HW_and_drought_plots_v2 = all_plots_v2[[3]]

drought_plots_v2
HW_plots_v2
HW_and_drought_plots_v2

HW_simulations_v2 = all_simulations_v2_1[c(2,3)]
all_HW_zoom_plots_v2 = lapply(HW_simulations_v2, plot_HWzoom)

HW_zoom_plots_v2 = all_HW_zoom_plots_v2[[1]]
HW_and_drought_zoom_plots_v2 = all_HW_zoom_plots_v2[[2]]

HW_zoom_plots_v2
HW_and_drought_zoom_plots_v2

## Alternative Tleaf calculations----
gs_vals = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5)

# With plantecophys
HW_maxTleaf_plantecophys = sapply(gs_vals, function(x){
  Tleaf = FindTleaf(gs = x, 
                    Tair = 43, 
                    VPD = 5,
                    Wind = 0,
                    Wleaf = 0.07,
                    PPFD = 1000)
  Tdif = Tleaf - 43
  return(c(gs = x,Tleaf,Tdif))
}) 

HW_maxTleaf_plantecophys

# With tealeaves
leaf_par  <- make_leafpar(
  replace = list(
    leafsize = set_units(0.07, 'm')
  )
)

enviro_par <- make_enviropar(
  replace = list(
    T_air = set_units(43, "degC"),
    RH = set_units(0.5)
  )
)

constants  <- make_constants()

# Now there should be 4 combinations (high and low g_sw crossed with high and low T_air)
T_leaf <- tleaf(leaf_par, enviro_par, constants, 
                    quiet = TRUE, set_units = FALSE)
T_leaf
