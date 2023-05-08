#Load packages
library(tidyverse)
library(ggpubr)

#LOAD AND ORGANIZE DATA----

#Load data
met_n_gs_simulations = read.csv(
  'observed_gs_Richmond/all_site_spp_simulations.csv')
met_n_const10_simulations = read.csv(
  'const10_gs_n_met/all_site_spp_simulations.csv')
met_n_const1_simulations = read.csv(
  'const1_gs_n_met/all_site_spp_simulations.csv')
met_n_const1_simulations = read.csv(
  'const1_gs_n_met/all_site_spp_simulations.csv')
met_n_const0.01_simulations = read.csv(
  'const0.01_gs_n_met/all_site_spp_simulations.csv')
new_simulations = read.csv(
  'Richmond_Eucalyptus/all_site_spp_simulations.csv') %>%
  filter(Tleaf.TpDep. <9999)

#Change label from 'obs' to 'driv' for gs-driven predictions
met_n_gs_simulations1 = met_n_gs_simulations %>%
  rename(A.driv. = A.obs.,
         E.driv. = E.obs.,
         gs.driv. = gs.obs.,
         gb.driv. = gb.obs.,
         Ci.driv. = Ci.obs.,
         Tleaf.driv. = Tleaf.obs.,
         Pleaf.driv. = Pleaf.obs.,
         Rublim.driv. = Rublim.obs.)

#Select predictions for constant gs simulations and relabel as 'const[value]'
const10_simulations = met_n_const10_simulations %>%
  select(A.obs., E.obs., gb.obs., 
         Ci.obs., Tleaf.obs., Pleaf.obs., Rublim.obs.) %>%
  rename(A.const10. = A.obs.,
         E.const10. = E.obs.,
         gb.const10. = gb.obs.,
         Ci.const10. = Ci.obs.,
         Tleaf.const10. = Tleaf.obs.,
         Pleaf.const10. = Pleaf.obs.,
         Rublim.const10. = Rublim.obs.)

const1_simulations = met_n_const1_simulations %>%
  select(A.obs., E.obs., gb.obs., 
         Ci.obs., Tleaf.obs., Pleaf.obs., Rublim.obs.) %>%
  rename(A.const1. = A.obs.,
         E.const1. = E.obs.,
         gb.const1. = gb.obs.,
         Ci.const1. = Ci.obs.,
         Tleaf.const1. = Tleaf.obs.,
         Pleaf.const1. = Pleaf.obs.,
         Rublim.const1. = Rublim.obs.)

const0.1_simulations = met_n_const0.1_simulations %>%
  select(A.obs., E.obs., gb.obs., 
         Ci.obs., Tleaf.obs., Pleaf.obs., Rublim.obs.) %>%
  rename(A.const0.1. = A.obs.,
         E.const0.1. = E.obs.,
         gb.const0.1. = gb.obs.,
         Ci.const0.1. = Ci.obs.,
         Tleaf.const0.1. = Tleaf.obs.,
         Pleaf.const0.1. = Pleaf.obs.,
         Rublim.const0.1. = Rublim.obs.)

const0.01_simulations = met_n_const0.01_simulations %>%
  select(A.obs., E.obs., gb.obs., 
         Ci.obs., Tleaf.obs., Pleaf.obs., Rublim.obs.) %>%
  rename(A.const0.01. = A.obs.,
         E.const0.01. = E.obs.,
         gb.const0.01. = gb.obs.,
         Ci.const0.01. = Ci.obs.,
         Tleaf.const0.01. = Tleaf.obs.,
         Pleaf.const0.01. = Pleaf.obs.,
         Rublim.const0.01. = Rublim.obs.)

#Create dataframe with all simulations
all_simulations = cbind(met_n_gs_simulations1, 
                        const10_simulations,
                        const1_simulations, 
                        const0.1_simulations,
                        const0.01_simulations)

#Define variables for air, leaf temperature of observations and Medlyn predictions
Tair = new_simulations$Tair
Tleaf = new_simulations$Tleaf 
Med_Tleaf = new_simulations$Tleaf.std.
Tdiff = Tleaf-Tair
Med_Tdiff = Med_Tleaf-Tair
site_spp = new_simulations$site_spp

#Combine temp variables into a dataframe
Tdiff_df = data.frame(Tair, Tleaf, Med_Tleaf, Tdiff, Med_Tdiff, site_spp)

#CREATE PLOTTING FUNCTIONS----
#Define function for plotting predictions vs observations
pred_v_obs_plot = function(y){
  ggplot(new_simulations,
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
  ggplot(new_simulations,
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
TpDep_v_std_plot = comp_pred_plot(
  x = new_simulations$Tleaf.std.,
  y = new_simulations$Tleaf.TpDep.
)


TpDep_v_std_plot +
  stat_cor(data =new_simulations,
           aes(x=Tleaf.std.-Tair, y=Tleaf.TpDep.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0, label.y = 2.1
  )+
  stat_regline_equation(data=new_simulations,
                        aes(x=Tleaf.std.-Tair, y=Tleaf.TpDep.-Tair),
                        label.x = 0, label.y = 2.4) +
  labs(x = "Medlyn Tleaf-Tair",
       y = "Tleaf-Tair (TpDep)")

#Sperry
Sperry_v_Medlyn_plot = comp_pred_plot(
  x = all_simulations$Tleaf.std., 
  y = all_simulations$Tleaf.pmax.)

Sperry_v_Medlyn_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf.std.-Tair, y=Tleaf.pmax.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0, label.y = 2
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf.std.-Tair, y=Tleaf.pmax.-Tair),
                        label.x = 0, label.y = 2.2) +
  labs(x = "Medlyn Tleaf-Tair",
       y = "Sperry Tleaf-Tair")

#gs driven
gsdriven_v_Medlyn_plot = comp_pred_plot(
  x = all_simulations$Tleaf.std., 
  y = all_simulations$Tleaf.driv.)

gsdriven_v_Medlyn_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf.std.-Tair, y=Tleaf.driv.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0, label.y = 2
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf.std.-Tair, y=Tleaf.driv.-Tair),
                        label.x = 0, label.y = 2.2) +
  labs(x = "Medlyn Tleaf-Tair",
       y = "gs-driven Tleaf-Tair")

#Constant gs
constgs0.01_v_Medlyn_plot = comp_pred_plot(
  x = all_simulations$Tleaf.std., 
  y = all_simulations$Tleaf.const0.01.)

constgs0.01_v_Medlyn_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf.std.-Tair, y=Tleaf.const0.01.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0, label.y = 2.8
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf.std.-Tair, y=Tleaf.const0.01.-Tair),
                        label.x = 0, label.y = 3) +
  labs(x = "Medlyn Tleaf-Tair",
       y = "Constant gs (0.01) Tleaf-Tair")


#PREDICTIONS VS OBSERVATIONS PLOTS----

#Medlyn with variable gmin
TpDep_v_obs_plot = pred_v_obs_plot(
  y = new_simulations$Tleaf.TpDep.) +
  labs(y = "Predicted Tleaf-Tair (TpDep)") +
  theme(legend.position="none")
TpDep_v_obs_plot +
  stat_cor(data=new_simulations,
           aes(x=Tleaf-Tair, y=Tleaf.TpDep.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = -2.5, label.y = 2.3
  )+
  stat_regline_equation(data=new_simulations,
                        aes(x=Tleaf-Tair, y=Tleaf.TpDep.-Tair),
                        label.x = -2.5, label.y = 2.6)

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

#Sperry
Sperry_v_obs_plot = pred_v_obs_plot(
  y = all_simulations$Tleaf.pmax.) +
  labs(y = "Predicted Tleaf-Tair (Sperry)")
Sperry_v_obs_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf-Tair, y=Tleaf.pmax.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = -2.5, label.y = 1.7
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf-Tair, y=Tleaf.pmax.-Tair),
                        label.x = -2.5, label.y = 2)

#gs driven
gsdriven_v_obs_plot = pred_v_obs_plot(
  y = all_simulations$Tleaf.driv.) +
  labs(y = "Predicted Tleaf-Tair (gs-driven)")
gsdriven_v_obs_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf-Tair, y=Tleaf.driv.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = -2.5, label.y = 1.7
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf-Tair, y=Tleaf.driv.-Tair),
                        label.x = -2.5, label.y = 2)

#Constant gs
constgs0.01_v_obs_plot = pred_v_obs_plot(
  y = all_simulations$Tleaf.const0.01.) +
  labs(y = "Predicted Tleaf-Tair (const gs = 0.01)")
constgs0.01_v_obs_plot +
  stat_cor(data=all_simulations,
           aes(x=Tleaf-Tair, y=Tleaf.obs.-Tair, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = -2.7, label.y = 2.35
  )+
  stat_regline_equation(data=all_simulations,
                        aes(x=Tleaf-Tair, y=Tleaf.obs.-Tair),
                        label.x = -2.7, label.y = 2.5)

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
