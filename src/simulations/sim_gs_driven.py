#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Code that runs met- and gs-driven simulations of leaf-level variables,
also computes some performance metrics.

"""

__title__ = "gs-driven model simulations"
__author__ = "Manon E. B. Sabot"
__version__ = "1.0 (06.10.2022)"
__email__ = "m.e.b.sabot@gmail.com"


# ======================================================================

# general modules
import argparse  # read in the user input
import os  # check for paths
import sys  # check for files, versions
import numpy as np  # array manipulations, math operators
import pandas as pd  # read/write dataframes, csv files
from scipy import stats  # compute the performance metrics
import warnings  # ignore warnings

# change the system path to load modules from TractLSM
script_dir = os.path.dirname(os.path.realpath(sys.argv[0]))
sys.path.append(os.path.abspath(os.path.join(script_dir, '..')))

# own modules
from TractLSM.Utils import get_main_dir  # get the project's directory
from TractLSM.Utils import read_csv  # read in files
from TractLSM import hrun  # run model

# ignore these warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)
pd.options.mode.chained_assignment = None


# ======================================================================

def main(runsim=False):

    """
    Main function: either runs the leaf-energy balance equilibriates
                   with the hydraulics (knowing gs) or analyses the
                   outputs against site-level observations.

    Arguments:
    ----------
    runsim: bool
        if True, runs the model. Otherwise analyses the model outputs

    Returns:
    --------
    .csv files that contain all the simulations, plus analyses files,
    all stored under 'output/observed_gs/'.

    """

    base_dir = get_main_dir()  # working paths

    # path to input files
    ipath = os.path.join(os.path.join(base_dir, 'input'), 'Anne_HW2_obs_gs')

    if not os.path.isdir(ipath):  # make dir
        os.makedirs(ipath)

    # driving + model output file names
    xfiles = [e[2:][0] for e in os.walk(ipath)][0]
    yfiles = xfiles.copy()
    site_spp = [e.split('.')[0] for e in xfiles]

    # output dir paths
    opath = ipath.replace('input', 'output')

    if not os.path.isdir(opath):  # create dir
        os.makedirs(opath)

    # check whether the output files exist
    if sum([e in os.listdir(ipath.replace('input', 'output'))
            for e in yfiles]) != len(yfiles):
        run_simulations(ipath, xfiles, yfiles)

    if not runsim:
        analyse_simulations(ipath, xfiles, yfiles)

    return


def run_simulations(ipath, xfiles, yfiles):

    """
    Calls TractLSM to run the leaf energy-balance and hydraulics
    with a known gs.

    Arguments:
    ----------
    ipath: string
        path to the folder where the input files are

    xfiles: list
        names of files that contains the input met & parameter data

    yfiles: list
        names under which to save the model output files

    Returns:
    --------
    .csv files that contain all the simulations, under
    'output/observed_gs/'.

    """

    for ifile, ofile in zip(xfiles, yfiles):  # loop over the files

        # output dir paths
        opath = ipath.replace('input', 'output')

        # model output file name
        fname = os.path.join(opath, ofile)

        if not os.path.isfile(fname):  # run model and save output
            df, __ = read_csv(os.path.join(ipath, ifile))  # input

            # add the necessary extra variables
            df['Ps_pd'] = df['Ps'].copy()
            df['sw'] = 0.  # add sw (not used) or it won't run
            df['scale2can'] = 1.  # no leaf scaling of any kind
            df.fillna(method='ffill', inplace=True)

            # run model
            __ = hrun(fname, df, len(df.index), 'Farquhar', models=['GsDriv'],
                      resolution='low')

    return


def combine_dfs(df1, df2, df3, identity):

    """
    Combines input, output, and obs data during daytime hours.

    Arguments:
    ----------
    df1: pandas dataframe
        dataframe containing all input data & params

    df2: pandas dataframe
        dataframe containing all output data

    df3: pandas dataframe
        dataframe containing the leaf-level observed data

    identity: string
        site_species combination

    Returns:
    --------
    df2: pandas dataframe
        dataframe containing all input & output data but no params

    """

    # merge the inputs / outputs from all the simulations + observations
    df2 = df2.merge(df1, left_index=True, right_index=True,
                    suffixes=('_x', ''))
    df2 = df2.drop('year', axis=1)
    df2 = df2.merge(df3, left_index=True, right_index=True,
                    suffixes=('_x', ''))

    # drop duplicated columns
    df2 = df2[df2.columns[~df2.columns.str.endswith('_x')]]

    # only keep the variables, i.e. remove parameters
    df2 = df2[[c for c in list(df2) if ((len(df2[c].unique()) > 1) or
              ('gs' in c) or ('Rublim' in c) or ('gb' in c) or
              ('Pleaf' in c))]]
    df2 = df2.drop(df2.filter(like='Ps(').columns, axis=1)

    # restrict to the hod when photosynthesis happens
    df2 = df2[df2[df2.filter(like='Tleaf(').columns].sum(axis=1) > 0.]

    # add the info about the site x spp combination
    df2['site_spp'] = identity

    return df2


def calc_rank(sub):

    """
    Calculates quantile ranks for values in sub.

    Arguments:
    ----------
    sub: array or pandas series
        data to rank

    Returns:
    --------
    The quantile ranks of the data.

    """

    return np.array([stats.percentileofscore(sub, a, 'strict') / 100.
                     for a in sub])


def calc_perf(df1, df2, var='Tleaf', metric='NSE'):

    """
    Calculates performance metrics for given sets of paired model and
    observation variables.

    Arguments:
    ----------
    df1: pandas dataframe
        dataframe containing all model & observation data

    df2: pandas dataframe
        dataframe containing the performance outputs

    var: string
        variable to evaluate, e.g., Tleaf, Ci, Pleaf

    metric: string
        metric of performance to compute

    Returns:
    --------
    Fills up df2, the dataframe containing the performance outputs

    """

    length = len(df1.filter(like='%s(' % (var)).columns)

    if var == 'Tleaf':
        idx1 = 0

    elif var == 'Ci':
        idx1 = length

    elif var == 'Pleaf':
        idx1 = 2 * length

    elif var == 'E':
        idx1 = 3 * len(df1.filter(like='%s(' % (var)).columns)

    elif var == 'A':
        idx1 = 4 * len(df1.filter(like='%s(' % (var)).columns)

    idx2 = df1['site_spp'].iloc[0]

    try:

        for i, e in enumerate(df1.filter(like='%s(' % (var)).columns):

            # mask invalid data
            df3 = df1.copy()[np.logical_and(~np.isclose(df1[e], 9999.),
                                            ~np.isnan(df1[var]))]

            if metric == 'R':
                perf, __ = stats.pearsonr(df3[var], df3[e])

            elif metric == 'NSE':
                perf = 1. - (((df3[e] - df3[var]) ** 2.).sum() /
                             ((df3[var] - df3[var].mean()) ** 2.).sum())

            elif (metric == 'BIC') or (metric == 'RBIC'):
                N = 1

                if (('std' in e) or ('sox2' in e) or ('wue' in e) or
                    ('cgn' in e) or ('lcst' in e) or ('cap' in e) or
                   ('mes' in e)):
                    N = 2

                elif 'cmax' in e:
                    N = 3

                elif 'tuz' in e:
                    N = 4

                rss = ((df3[e] - df3[var]) ** 2.).sum()
                perf = len(df3) * np.log(rss / len(df3)) + N * np.log(len(df3))

            elif metric == 'MASE':
                perf = ((df3[var] - df3[e] - (df3[var] - df3[e]).abs().min())
                        .abs().mean() / df3[var].diff()[1:].abs().mean())

            if np.logical_and('std' in e, var == 'Pleaf'):
                perf = np.nan

            if df2.loc[idx1 + i].isnull().all().all():  # columns empty
                df2.loc[idx1 + i, 'model'] = (e.split('%s(' % (var))[1]
                                               .split(')')[0])
                df2.loc[idx1 + i, 'variable'] = var

            df2.loc[idx1 + i, idx2] = perf

        if metric == 'RBIC':
            sub = df2.loc[idx1:idx1 + i, idx2]
            df2.loc[idx1:idx1 + i, idx2] = calc_rank(sub)

    except Exception:
        pass

    return


def performance(df, which='NSE'):

    """
    Calculates metrics of performance that characterize the model's
    ability to simulate Tleaf, Ci, Pleaf, E, A.

    Arguments:
    ----------
    df1: pandas dataframe
        dataframe containing all model & observation data

    which: string
        metric of performance to compute

    Returns:
    --------
    A dataframe that contains information about the model's
    performance in a given metric.

    """

    cols = ['mean', 'model', 'variable']
    perf = pd.DataFrame(columns=list(df['site_spp'].unique()) + cols,
                        index=np.arange(5 * len(df.filter(like='Tleaf(')
                                                  .columns)))

    # calculate perf by site x species
    df.groupby('site_spp').apply(calc_perf, df2=perf, metric=which)
    df.groupby('site_spp').apply(calc_perf, df2=perf, var='Ci',
                                 metric=which)
    df.groupby('site_spp').apply(calc_perf, df2=perf, var='Pleaf',
                                 metric=which)
    df.groupby('site_spp').apply(calc_perf, df2=perf, var='E', metric=which)
    df.groupby('site_spp').apply(calc_perf, df2=perf, var='A', metric=which)

    # make sure data is of the right type
    for c in perf.columns[:perf.columns.get_loc('model')]:

        perf[c] = perf[c].astype('float64')

    return perf


def compute_performance_metrics(df, opath):

    """
    Wrapper around the performance function that computes each metric
    of performance and stores the output in .csv files.

    Arguments:
    ----------
    df: pandas dataframe
        dataframe containing all model & observation data

    opath: string
        path to the folder where the output files ought to be stored

    Returns:
    --------
    A series of .csv files named after the metric of performance which
    they contain information on.

    """

    fname = os.path.join(opath, 'all_Rs.csv')

    if not os.path.isfile(fname):
        rs = performance(df, which='R')
        rs['mean'] = rs.iloc[:, :rs.columns.get_loc('mean')].mean(axis=1)
        rs.to_csv(fname, index=False, na_rep='', encoding='utf-8')

    fname = os.path.join(opath, 'all_NSEs.csv')

    if not os.path.isfile(fname):
        nses = performance(df)

        # transform from NNSEs to NSEs (NNSEs used due to inf bound)
        nses['mean'] = (2. - 1. / (1 / (2. -
                        nses.iloc[:, :nses.columns.get_loc('mean')]))
                        .mean(axis=1))
        nses.to_csv(fname, index=False, na_rep='', encoding='utf-8')

    fname = os.path.join(opath, 'all_MASEs.csv')

    if not os.path.isfile(fname):
        mases = performance(df, which='MASE')
        mases['mean'] = (mases.iloc[:, :mases.columns.get_loc('mean')]
                         .mean(axis=1))
        mases.to_csv(fname, index=False, na_rep='', encoding='utf-8')

    fname = os.path.join(opath, 'all_BICs.csv')

    if not os.path.isfile(fname):
        bics = performance(df, which='BIC')
        bics['mean'] = (bics.iloc[:, :bics.columns.get_loc('mean')]
                        .mean(axis=1))
        bics.to_csv(fname, index=False, na_rep='', encoding='utf-8')

    return


def analyse_simulations(ipath, xfiles, yfiles):

    """
    Combines all the simulation outputs and analyses the performance of
    the model using four different statistical metrics.

    Arguments:
    ----------
    ipath: string
        path to the folder where the input files are

    xfiles: list
        names of files that contains the input met & parameter data

    yfiles: list
        names of files that contains the model outputs

    Returns:
    --------

    """

    # all run info
    fname = os.path.join(ipath.replace('input', 'output'),
                         'all_site_spp_simulations.csv')

    if not os.path.isfile(fname):  # create file if it doesn't exist

        for ifile, ofile in zip(xfiles, yfiles):  # loop over the files

            # load model input data into a dataframe
            df, __ = read_csv(os.path.join(ipath, ifile))
            df.fillna(method='ffill', inplace=True)

            # read model output file
            opath = ipath.replace('input', 'output')
            df2, __ = read_csv(os.path.join(opath, ofile))

            # read leaf-level observations
            df3, __ = read_csv(os.path.join(os.path.dirname(
                               ipath.replace('input', 'observations')),
                               '%s.csv' % (ofile.split('.')[0])))
            df3 = df3.drop('year', axis=1)

            # combine the dataframes
            df = combine_dfs(df, df2, df3, ofile.split('.')[0])

            try:  # append new combined df to previously combined dfs
                dfs = dfs.append(df, ignore_index=True)

            except NameError:  # first run
                dfs = df.copy()

        # remove duplicates and save
        dfs.drop_duplicates(inplace=True)
        columns = dfs.columns.to_list()  # modify column order
        columns.remove('site_spp')  # modify column order
        dfs[['site_spp'] + columns].to_csv(fname, index=False, na_rep='',
                                           encoding='utf-8')

    else:
        dfs = (pd.read_csv(fname).dropna(axis=0, how='all')
                 .dropna(axis=1, how='all').squeeze())

    # compute performance metrics
    compute_performance_metrics(dfs, ipath.replace('input', 'output'))

    return


# ======================================================================

if __name__ == "__main__":

    # define the argparse settings to read run set up file
    description = "run simulations or analyse simulation outputs?"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-r', '--runsim', action='store_true',
                        help='run the model')
    args = parser.parse_args()

    main(runsim=args.runsim)
