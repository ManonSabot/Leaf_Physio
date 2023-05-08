# -*- coding: utf-8 -*-

"""
Solving the leaf-energy balance and water status when gs is known, by
iteration on the air temperature to get the leaf temperature for which
the Penman-Monteith energy balance conditions are satisfied.

"""

__title__ = "Iterative solving with a known gs"
__author__ = "Manon E. B. Sabot"
__version__ = "1.0 (06.10.2022)"
__email__ = "m.e.b.sabot@gmail.com"


# ======================================================================

# general modules
import numpy as np  # array manipulations, math operators

# own modules
from TractLSM import conv, cst  # unit converter & general constants
from TractLSM.SPAC import hydraulics, fwWP
from TractLSM.SPAC import leaf_temperature, calc_photosynthesis, rubisco_limit
from TractLSM.CH2OCoupler import calc_trans


# ======================================================================

def solve_gs_driv(p, photo='Farquhar', res='low', iter_max=40, threshold_conv=0.1,
              inf_gb=False):

    """
    Checks the energy balance by looking for convergence of the new leaf
    temperature with the leaf temperature predicted by the previous
    iteration. Then returns the corresponding An, E, Ci, etc.

    Arguments:
    ----------
    p: recarray object or pandas series or class containing the data
        time step's met data & params

    photo: string
        either the Farquhar model for photosynthesis, or the Collatz
        model

    res: string
        either 'low' (default), 'med', or 'high' to solve for leaf water
        potential

    iter_max: int
        maximum number of iterations allowed on the leaf temperature
        before reaching the conclusion that the system is not energy
        balanced

    threshold_conv: float
        convergence threshold for the new leaf temperature to be in
        energy balance

    inf_gb: bool
        if True, gb is prescrived and very large

    Returns:
    --------
    An: float
        net photosynthetic assimilation rate [umol m-2 s-1]

    Ci: float
        intercellular CO2 concentration [Pa]

    rublim: bool
        'True' if the C assimilation is rubisco limited, 'False'
        otherwise

    trans: float
        transpiration rate [mmol m-2 s-1]

    gs: float
        stomatal conductance [mol m-2 s-1]

    gb: float
        leaf boundary layer conductance [mol m-2 s-1]

    new_Tleaf: float
        leaf temperature [degC]

    Pleaf: float
        leaf water potential [MPa]

    """

    # initial state
    Cs = p.CO2  # Pa

    try:   # is Tleaf one of the input fields?
        Tleaf = p.Tleaf

    except (IndexError, AttributeError, ValueError):  # calc. Tleaf
        Tleaf = p.Tair  # deg C

    # hydraulics
    P, E = hydraulics(p, res=res)

    # iter on the solution until it is stable enough
    iter = 0

    while True:

        # calculate trans, gw, gb, mol.m-2.s-1
        trans, real_zero, gw, gb, __ = calc_trans(p, Tleaf, p.gs,
                                                  inf_gb=inf_gb)

        # calculate An, Ci
        An, Aj, Ac = calc_photosynthesis(p, 0., Cs, photo=photo, Tleaf=Tleaf,
                                         gsc=conv.U * conv.GcvGw * p.gs)
        Ci = Cs - p.Patm * conv.FROM_MILI * An / (p.gs * conv.GcvGw)  # Pa

        try:  # is Tleaf one of the input fields?
            new_Tleaf = p.Tleaf

        except (IndexError, AttributeError, ValueError):  # calc. Tleaf
            new_Tleaf, __ = leaf_temperature(p, trans, Tleaf=Tleaf, gs=p.gs,
                                             inf_gb=inf_gb)

        # update Cs (Pa)
        boundary_CO2 = p.Patm * conv.FROM_MILI * An / (gb * conv.GbcvGb)
        Cs = np.maximum(cst.zero, np.minimum(p.CO2, p.CO2 - boundary_CO2))

        # force stop when atm. conditions yield E < 0. (non-physical)
        if (iter < 1) and (not real_zero):
            real_zero = None

        # check for convergence
        if ((real_zero is None) or (iter >= iter_max) or ((iter >= 1) and
            real_zero and (abs(Tleaf - new_Tleaf) <= threshold_conv) and not
           np.isclose(p.gs, cst.zero, rtol=cst.zero, atol=cst.zero))):
            break

        # no convergence, iterate on leaf temperature
        Tleaf = new_Tleaf
        iter += 1

    # calc. leaf water potential
    Pleaf = P[np.nanargmin(np.abs(trans - E))]

    # rubisco- or electron transport-limitation?
    rublim = rubisco_limit(Aj, Ac)

    if ((np.isclose(trans, cst.zero, rtol=cst.zero, atol=cst.zero) and
        (An > 0.)) or np.isclose(Ci, 0., rtol=cst.zero, atol=cst.zero) or
        (Ci < 0.) or np.isclose(Ci, p.CO2, rtol=cst.zero, atol=cst.zero) or
        (Ci > p.CO2) or (real_zero is None) or (not real_zero) or
       any(np.isnan([An, Ci, trans, p.gs, new_Tleaf, Pleaf]))):
        An, Ci, trans, gb, new_Tleaf, Pleaf = (9999.,) * 6

    elif not np.isclose(trans, cst.zero, rtol=cst.zero, atol=cst.zero):
        trans *= conv.MILI  # mmol.m-2.s-1

    return An, Ci, rublim, trans, p.gs, gb, new_Tleaf, Pleaf
