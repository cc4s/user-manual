#!/bin/bash

##########################################################
#
# SPECIFY PARAMETERS BELOW
#
##########################################################
#
# * Define path to VASP gamma-only binary:
#
VASP="./vasp_gam"
#
# * Define energy cut-offs for the plane-wave basis sets:
#
enc=400
egw=300
#
# * Define basis set size. nbfp=11 corresponds to 10 virtual natural orbitals per occupied orbital.
#   Feel free to change the value for nbfp to check basis-set convergence of energies.
#
nbfp=11
#
# N.B.: A POTCAR file for boron and nitrogen needs to be present in the current directory
#
##########################################################

echo "++++++++++++++++++++++++++++++"
echo "Check if POTCAR file is present"
echo "++++++++++++++++++++++++++++++"


if test -f "POTCAR"; then
    echo "POTCAR file found."
else
    echo "POTCAR file not found. Please provide one for boron and nitrogen."
    exit
fi


echo "++++++++++++++++++++++++++++++"
echo "Preparing all input files for VASP calculations."
echo "++++++++++++++++++++++++++++++"

cat >KPOINTS<<!
Automatically generated mesh
       0
gamma
 1 1 1
!

cat >POSCAR<<!
rBN
1.0
2.48778000 0.00000000 0.00000000
0.00000000 4.30896266 0.00000000
0.00000000 1.43631949 6.45855008
B N
4 4
Direct
0 0 0
0.5 0 0.5
0.5 0.5 0
0 0.5 0.5
0.5 0.8333333333 0
0 0.8333333333 0.5
0 0.3333333333 0
0.5 0.3333333333 0.5
!

echo "++++++++++++++++++++++++++++++"
echo "Going to run VASP calculation for the following POSCAR file"
echo "++++++++++++++++++++++++++++++"
cat POSCAR

echo "++++++++++++++++++++++++++++++"
echo "Removing WAVECAR file to ensure a fresh start"
echo "++++++++++++++++++++++++++++++"
rm WAVECAR

echo "++++++++++++++++++++++++++++++"
echo "RUN DFT to get a converged guess for HF"
echo "++++++++++++++++++++++++++++++"
cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
EDIFF = 1E-6
!
cat INCAR
$VASP
cp OUTCAR OUTCAR.DFT


echo "++++++++++++++++++++++++++++++"
echo "RUN HF"
echo "++++++++++++++++++++++++++++++"
cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
EDIFF = 1E-6
LHFCALC=.TRUE.
AEXX=1.0
ALGO=C
!
cat INCAR
$VASP
cp OUTCAR OUTCAR.HF


nb=`awk <OUTCAR "/maximum number of plane-waves:/ { print \\$5*2-1 }"`
echo "++++++++++++++++++++++++++++++"
echo "For the given plane-wave basis energy cut-off, there will be a total of " $nb " bands/orbitals."
echo "++++++++++++++++++++++++++++++"

echo "++++++++++++++++++++++++++++++"
echo "Compute Fock matrix and diagonalize in full basis"
echo "++++++++++++++++++++++++++++++"
cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
EDIFF = 1E-6
LHFCALC=.TRUE.
AEXX=1.0
ISYM=-1
ALGO = sub ; NELM = 1
NBANDS = $nb
!
cat INCAR
$VASP
cp OUTCAR OUTCAR.HFdiag

echo "++++++++++++++++++++++++++++++"
echo "Compute MP2 pair correlation energies in the CBS limit via extrapolation"
echo "++++++++++++++++++++++++++++++"
cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
LHFCALC=.TRUE.
AEXX=1.0
ISYM=-1
ALGO = MP2 
NBANDS = $nb
LSFACTOR=.TRUE.
!
rm WAVEDER
cat INCAR
$VASP
cp OUTCAR OUTCAR.MP2-CBS


echo "++++++++++++++++++++++++++++++"
echo "Compute approximate MP2 natural orbitals"
echo "see https://doi.org/10.1021/ct200263g for more details"
echo "The natural orbitals will be written to WAVECAR.FNO"
echo "++++++++++++++++++++++++++++++"
cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
LHFCALC=.TRUE.
AEXX=1.0
ISYM=-1
ALGO = MP2NO ;
NBANDS = $nb
LAPPROX=.TRUE.
!
rm WAVEDER
cat INCAR
$VASP
cp OUTCAR OUTCAR.MP2-NOs

nocc=`awk <OUTCAR "/NELEC/ { print \\$3/2 }"`

echo "++++++++++++++++++++++++++++++"
echo "This system contains $nocc occupied bands/orbitals."
echo "++++++++++++++++++++++++++++++"


echo "++++++++++++++++++++++++++++++"
echo "Preparing output for CC4S calculations."
echo "++++++++++++++++++++++++++++++"

nvirt=`echo "$nbfp - 1" | bc `
echo "Using " $nvirt " virtual orbitals per occupied orbital."

nbno=`awk <OUTCAR "/NELEC/ { print \\$3/2*$nbfp }"`
echo "Using " $nbno " bands/orbitals in total."


cp WAVECAR.FNO WAVECAR

echo "++++++++++++++++++++++++++++++"
echo "Recanonicalizing " $nbno "natural orbitals/bands using HF diagonalizer."
echo "++++++++++++++++++++++++++++++"
cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
EDIFF = 1E-6
LHFCALC=.TRUE.
AEXX=1.0
ISYM=-1
ALGO = sub ; NELM = 1
NBANDS = $nbno
NBANDSHIGH = $nbno
!
rm WAVEDER
cat INCAR
$VASP
cp OUTCAR OUTCAR.HFdiag-NOs

echo "++++++++++++++++++++++++++++++"
echo "Computing and writing CC4S output using " $nbno " bands."
echo "++++++++++++++++++++++++++++++"

cat >INCAR <<!
ENCUT = $enc
SIGMA=0.0001
EDIFF = 1E-5
LHFCALC=.TRUE.
AEXX=1.0
ISYM=-1
ALGO=CC4S
NBANDS = $nbno
NBANDSHIGH = $nbno
ENCUTGW=$egw
ENCUTGWSOFT=$egw
!
cat INCAR
$VASP
cp OUTCAR OUTCAR.CC4S
