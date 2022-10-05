## General instructions

To make sure the model and support files are properly set up, simply type:
```
make -f setup.mk
```
> N.B.: You need anaconda to use the existing environment files within which
> to run the model. By default, python3.10 will be used to run the model.
> If you would rather install the dependencies locally, you will need python.

Followed by:
```
source activate gs_formulations
```

&nbsp;

To generate model outputs, performance metrics, and figures, type:
```
make
```

&nbsp;

### Model runs for all input files in `input/all_site_spp/`

```
src/simulations/sim_met_driven.py -r
```

And to generate performance metrics and process files for plotting:

```
src/simulations/sim_met_driven.py
```

&nbsp;

### Plotting against observations

To get a sense of the models' "goodness-of-fit", you can use:
```
src/plots/plot_goodness_of_fit.py
```

But for a proper assessment of their skill that relies on several statistical
metrics of performance, including advanced metrics which characterize
specific aspects of model behaviour, then you'll need:
```
src/plots/plot_skill_met_calibs.py
```

Finally, a series of functional relationships between different variables can
be assessed from:
```
src/plots/plot_met_driven.py
```

&nbsp;

## The model

The model used here is a leaf-level adaptation of the **TractLSM**
[(Sabot et al., 2019)](https://doi.org/10.5281/zenodo.3566722)), modified to
embeds 12 gas exchange schemes. The **TractLSM** is further described
[here](https://github.com/ManonSabot/Profit_Maximisation_European_Forests).
Consulting the ReadMe in `src/TractLSM/` might also help get an idea of the
model structure and of the processes accounted for.
