# -*- coding: utf-8 -*-

"""
The profit maximisation algorithm that maximizes instantaneous carbon
gain minus summed hydraulic and thermal costs.

This file is part of the TractLSM model.


Please refer to the terms of the MIT License, which you should have
received along with the TractLSM.

Reference:
----------
* Sperry, J. S., Venturas, M. D., Anderegg, W. R., Mencuccini, M.,
  Mackay, D. S., Wang, Y., & Love, D. M. (2017). Predicting stomatal
  responses to the environment from the optimization of photosynthetic
  gain and hydraulic cost. Plant, cell & environment, 40(6), 816-830.

"""

__title__ = "Profit maximisation algorithm"
__author__ = "Camille Sicangco"
__version__ = "3.0 (29.11.2018)"
__email__ = "camillesicangco@gmail.com"

# ======================================================================

# general modules
import numpy as np  # array manipulations, math operators

# own modules
from TractLSM import conv, cst  # unit converter & general constants
from TractLSM.SPAC import hydraulics, hydraulic_cost
from TractLSM.SPAC import leaf_energy_balance, leaf_temperature
from TractLSM.SPAC import calc_photosynthesis, rubisco_limit
from TractLSM.CH2OCoupler import Ci_sup_dem
from TractLSM.CH2OCoupler import A_trans


# ======================================================================

def photo_gain(p, trans, photo, res, inf_gb=False):
    """
    Calculates the photosynthetic carbon gain of a plant, where the
    photosynthetic rate (A) is evaluated over the array of leaf water
    potentials (P) and, thus transpiration (E), and normalized by the
    instantaneous maximum A over the full range of E.

    Arguments:
    ----------
    p: recarray object or pandas series or class containing the data
        time step's met data & params

    trans: array
        transpiration [mol m-2 s-1], values depending on the possible
        leaf water potentials (P) and the Weibull parameters b, c

    photo: string
        either the Farquhar model for photosynthesis, or the Collatz
        model

    res: string
        either 'low' (default), 'med', or 'high' to run the optimising
        solver

    inf_gb: bool
        if True, gb is prescrived and very large

    Returns:
    --------
    gain: array
        unitless instantaneous photosynthetic gains for possible values
        of Ci minimized over the array of E

    Ci: array
        intercellular CO2 concentration [Pa] for which A(P) is minimized
        to be as close as possible to the A predicted by either the
        Collatz or the Farquhar photosynthetis model

    mask: array
        where Ci is valid vs. not

    """

    # get all Cis(P)
    Ci, mask = Ci_sup_dem(p, trans, photo=photo, res=res, inf_gb=inf_gb)

    try:
        A_P = A_trans(p, trans[mask], Ci, inf_gb=inf_gb)  # A supply
        gain = A_P / np.nanmax(A_P)  # photo gain, soil P excluded

        if np.nanmax(A_P) < 0.:  # when resp >> An everywhere
            gain *= -1.

    except ValueError:  # if trans is "pre-opimised" for
        gain = 0.

    return gain, Ci, mask


def thermal_cost(p, trans, inf_gb=False):
    """
    Calculates the thermal cost function that reflects rapidly increasing
    values of minimum fluorescence (F0), indicative of damage to
    Photosystem II, at temperatures above Tcrit.

    Arguments:
    ----------
    p: recarray object or pandas series or class containing the data
        time step's met data & params

    trans: array
        transpiration [mol m-2 s-1], values depending on the possible
        leaf water potentials (P) and the Weibull parameters b, c

    res: string
        either 'low' (default), 'med', or 'high' to run the optimising
        solver

    inf_gb: bool
        if True, gb is prescrived and very large

    Returns:
    --------
    gain: array
        unitless instantaneous photosynthetic gains for possible values
        of Ci minimized over the array of E

    Ci: array
        intercellular CO2 concentration [Pa] for which A(P) is minimized
        to be as close as possible to the A predicted by either the
        Collatz or the Farquhar photosynthetis model

    mask: array
        where Ci is valid vs. not

    """
    # calculate leaf temperature
    Tleaf, __ = leaf_temperature(p, trans, inf_gb=inf_gb)

    # estimate r
    r = 2. / (p.T50 - p.Tcrit)

    # get relative minimum fluorescence value
    F0 = 1 / (1 + np.exp(-r * (Tleaf - p.T50)))
    # F0 = (p.F0_max - p.F0_min) / (1 + np.exp(-r * (Tleaf - p.T50))) + p.F0_min

    # calculate thermal cost
    cost = F0
    # cost = (F0 - p.F0_min) / (p.F0_max - p.F0_min)

    return cost


def profit_therm2(p, photo='Farquhar', res='low', inf_gb=False, deriv=False):
    """
    Finds the instantaneous profit maximization, following the
    optimization criterion for which, at each instant in time, the
    stomata regulate canopy gas exchange and pressure to achieve the
    maximum profit, which is the maximum difference between the
    normalized photosynthetic gain and the hydraulic cost function.

    Arguments:
    ----------
    p: recarray object or pandas series or class containing the data
        time step's met data & params

    photo: string
        either the Farquhar model for photosynthesis, or the Collatz
        model

    res: string
        either 'low' (default), 'med', or 'high' to run the optimising
        solver

    inf_gb: bool
        if True, gb is prescrived and very large

    deriv: bool
        if True, uses the derivative form of the optimality criterion

    Returns:
    --------
    An: float
        net photosynthetic assimilation rate [umol m-2 s-1] at maximum
        profit

    Ci: float
        intercellular CO2 concentration [Pa] at maximum profit

    rublim: bool
        'True' if the C assimilation is rubisco limited, 'False'
        otherwise

    trans: float
        transpiration rate [mmol m-2 s-1] at maximum profit

    gs: float
        stomatal conductance [mol m-2 s-1] at maximum profit

    gb: float
        leaf boundary layer conductance [mol m-2 s-1] at maximum profit

    Tleaf: float
        leaf temperature [degC] at maximum profit

    Pleaf: float
        leaf water potential [MPa] at maximum profit

    """

    # hydraulics and reduction factor
    P, trans = hydraulics(p, res=res)
    cost_hy, __, k = hydraulic_cost(p, P)

    # thermal cost
    cost_th = thermal_cost(p, trans)

    # expression of optimization
    cost = cost_hy + cost_th
    gain, Ci, mask = photo_gain(p, trans, photo, res, inf_gb=inf_gb)
    expr = gain - cost[mask]

    # deal with edge cases by rebounding the solution
    gc, gs, gb, __ = leaf_energy_balance(p, trans[mask], inf_gb=inf_gb)

    if deriv:  # derivative form
        expr = np.abs(np.gradient(expr, P[mask]))

    try:
        if inf_gb:  # check on valid range
            check = expr[gc > cst.zero]

        else:  # further constrain the realm of possible gs
            check = expr[np.logical_and(gc > cst.zero, gs < 1.5 * gb)]

        idx = np.isclose(expr, max(check))

        if deriv:  # derivative form
            idx = np.isclose(expr, min(check))

        idx = [list(idx).index(e) for e in idx if e]

        if inf_gb:  # check for algo. "overshooting" due to inf. gb
            while Ci[idx[0]] < 2. * p.gamstar25:

                idx[0] -= 1

                if idx[0] < 3:
                    break

        # optimized where Ci for both photo models are close
        Ci = Ci[idx[0]]
        trans = trans[mask][idx[0]]  # mol.m-2.s-1
        gs = gs[idx[0]]
        Pleaf = P[mask][idx[0]]
        k = k[mask][idx[0]]

        try:  # is Tleaf one of the input fields?
            Tleaf = p.Tleaf

        except (IndexError, AttributeError, ValueError):  # calc. Tleaf
            Tleaf, __ = leaf_temperature(p, trans, gs=gs, inf_gb=inf_gb)

            # calculate gmin
            Q10a = 1.2  # Riederer & Muller 2006
            Q10b = 4.8  # Riederer & Muller 2006
            if Tleaf < p.Tp:  # solve for gmin (Cochard 2021); this introduces a problem if Tleaf is not an input field
                gmin = p.gmin_20 * pow(Q10a, (Tleaf - 20) / 10)
            else:
                gmin = p.gmin_Tp * pow(Q10b, (Tleaf - p.Tp) / 10)

            # solve for gs as max of previously calculated gs versus gmin
            gs = np.maximum(gs, gmin)

        # calc. optimal An
        An, Aj, Ac = calc_photosynthesis(p, trans, Ci, photo=photo,
                                         Tleaf=Tleaf, inf_gb=inf_gb)

        # rubisco- or electron transport-limitation?
        rublim = rubisco_limit(Aj, Ac)

        if ((np.isclose(trans, cst.zero, rtol=cst.zero, atol=cst.zero) and
             (An > 0.)) or (idx[0] == len(P) - 1) or
                any(np.isnan([An, Ci, trans, gs, Tleaf, Pleaf]))):
            An, Ci, trans, gs, gb, Tleaf, Pleaf, k = (9999.,) * 7

        elif not np.isclose(trans, cst.zero, rtol=cst.zero, atol=cst.zero):
            trans *= conv.MILI  # mmol.m-2.s-1

        return An, Ci, rublim, trans, gs, gb, Tleaf, Pleaf, k

    except ValueError:  # no opt

        return (9999.,) * 8
