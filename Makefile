all: simulations plots

simulations:
	src/simulations/sim_met_driven.py -r
	src/simulations/sim_met_driven.py

plots:
  src/plots/plot_goodness_of_fit.py
	src/plots/plot_skill_met_calibs.py
	src/plots/plot_met_driven.py
