(*  Orpie -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003-2004  Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 2,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at 
 *  <pelzlpj@eecs.umich.edu>.
 *)

(* const.ml
 * a couple of items related to entry of physical constants *)

open Constants


let translate_symbol sym =
   if sym      = "NA"    then avagadro_number
   else if sym = "k"     then boltzmann
   else if sym = "Vm"    then molar_volume
   else if sym = "R"     then universal_gas
   else if sym = "stdT"  then standard_temperature
   else if sym = "stdP"  then standard_pressure
   else if sym = "sigma" then stefan_boltzmann
   else if sym = "c"     then light_speed
   else if sym = "eps0"  then permittivity
   else if sym = "u0"    then permeability
   else if sym = "g"     then acceleration_gravity
   else if sym = "G"     then newton_gravitation
   else if sym = "h"     then planck_h
   else if sym = "hbar"  then dirac_hbar
   else if sym = "e"     then electron_charge
   else if sym = "me"    then electron_mass
   else if sym = "mp"    then proton_mass
   else if sym = "alpha" then fine_structure
   else if sym = "phi"   then magnetic_flux_quantum
   else if sym = "F"     then faraday
   else if sym = "Rinf"  then rydberg
   else if sym = "a0"    then bohr_radius
   else if sym = "uB"    then bohr_magneton
   else if sym = "uN"    then nuclear_magneton
   else if sym = "lam0"  then photon_wavelength
   else if sym = "f0"    then photon_frequency
   else if sym = "lamc"  then compton_wavelength
   else if sym = "c3"    then wien
   else raise (Invalid_argument ("unknown constant \"" ^ sym ^ "\""))


let constant_symbols = "NA\nk\nVm\nR\nstdT\nstdP\nsigma\nc\ne\neps0\n" ^
"u0\ng\nG\nh\nhbar\nme\nmp\nalpha\nphi\nF\nRinf\na0\nuB\nuN\nlam0\nf0\nlamc\nc3\n"




(* arch-tag: DO_NOT_CHANGE_feaa2de7-c265-4226-9e4d-ce896ec87b44 *)
