""
Two population demographic models.
"""
import numpy

from dadi import Numerics, PhiManip, Integration
from dadi.Spectrum_mod import Spectrum

def const_nu1_split_mig((nu2, T, m), (n1,n2), pts):
    """
    Split into two populations of specifed size, with migration.

    nu1: Size of population 1 after split.
    nu2: Size of population 2 after split.
    T: Time in the past of split (in units of 2*Na generations) 
    m: Migration rate between populations (2*Na*m)
    n1,n2: Shape of resulting SFS
    pts: Number of grid points to use in integration.
    """
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)

    phi = Integration.two_pops(phi, xx, T, 1.0, nu2, m12=m, m21=m)

    sfs = Spectrum.from_phi(phi, (n1,n2), (xx,xx))
    return sfs


def bottleneck(params, ns, pts):
    nuB,nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, nuB)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs


def bottleneck_fixedNb(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.029)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs


def bottleneck_fixedNb_002(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.002)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs

def bottleneck_fixedNb_05(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.05)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs


def bottleneck_fixedNb_2(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.2)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs

def bottleneck_fixedNb_4(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.4)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs

def bottleneck_fixedNb_1(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.1)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs

def bottleneck_fixedNb_15(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 1.5)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs


def bottleneck_fixedNb_20(params, ns, pts):
    nuF,TB,TF = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 2.0)
    phi = Integration.one_pop(phi, xx, TF, nuF)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs



def growth(params, ns, pts):
    nu,T = params
    xx = Numerics.default_grid(pts)
    phi = PhiManip.phi_1D(xx)

    nu_func = lambda t: numpy.exp(numpy.log(nu) * t/T)
    phi = Integration.one_pop(phi, xx, T, nu_func)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs

def bottleneckAndGrowth(params, ns, pts):
    nuB,nu,TB,T = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, nuB)

    nu_func = lambda t: numpy.exp(numpy.log(nu) * t/T)
    phi = Integration.one_pop(phi, xx, T, nu_func)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs


def bottleneckAndGrowth_fixedNb(params, ns, pts):
    nu,TB,T = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = Integration.one_pop(phi, xx, TB, 0.029)

    nu_func = lambda t: numpy.exp(numpy.log(nu) * t/T)
    phi = Integration.one_pop(phi, xx, T, nu_func)

    sfs = Spectrum.from_phi(phi, ns, (xx,))
    return sfs

def bottleneck_2d((nuB,nuF,TB,TF), (n1,n2), pts):
    
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)
    phi = Integration.two_pops(phi, xx, TB, 1.0, nuB)
    phi = Integration.two_pops(phi, xx, TF, 1.0, nuF)

    sfs = Spectrum.from_phi(phi, (n1,n2), (xx,xx))
    return sfs


def bottleneckAndGrowth_2d(params, ns, pts):
    nuB,nu,TB,T = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)
    phi = Integration.two_pops(phi, xx, TB, 1.0, nuB)

    nu_func = lambda t: numpy.exp(numpy.log(nu) * t/T)
    phi = Integration.two_pops(phi, xx, T, nu_func)

    sfs = Spectrum.from_phi(phi, ns, (xx,xx))
    return sfs


def bottleneck_2d_fixNb((nuF,TB,TF), (n1,n2), pts):
    
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)
    phi = Integration.two_pops(phi, xx, TB, 1.0, 0.029)
    phi = Integration.two_pops(phi, xx, TF, 1.0, nuF)

    sfs = Spectrum.from_phi(phi, (n1,n2), (xx,xx))
    return sfs




def bottleneck_owmig_2d((nuB,nuF,TB,TF, m), (n1,n2), pts):
    
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)
    phi = Integration.two_pops(phi, xx, TB, 1.0, nuB)
    phi = Integration.two_pops(phi, xx, TF, 1.0, nuF, m12=m, m21=0)

    sfs = Spectrum.from_phi(phi, (n1,n2), (xx,xx))
    return sfs



def bottleneckAndGrowth_owmig_2d(params, ns, pts):
    nuB,nu,TB,T, m = params
    xx = Numerics.default_grid(pts)

    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)
    phi = Integration.two_pops(phi, xx, TB, 1.0, nuB)

    nu_func = lambda t: numpy.exp(numpy.log(nu) * t/T)
    phi = Integration.two_pops(phi, xx, T, nu_func, m12=0, m21=m)

    sfs = Spectrum.from_phi(phi, ns, (xx,xx))
    return sfs



def split_mig(params, ns, pts):
    nu1, nu2, T, m = params
    xx = Numerics.default_grid(pts)
    phi = PhiManip.phi_1D(xx)
    phi = PhiManip.phi_1D_to_2D(xx, phi)

    phi = Integration.two_pops(phi, xx, T, nu1, nu2, m12=0, m21=m)
    fs = Spectrum.from_phi(phi, ns, (xx,xx))
    return fs

