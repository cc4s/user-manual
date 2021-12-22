#!/bin/bash


export OMP_NUM_THREADS=1

VASPBIN="/home/fs71337/grueneis4/src/vasp-edge/bin/vasp_gam"
CC4SBIN="/home/fs71337/grueneis4/src/demo/cc4s/build/icc_impi/bin/Cc4s"
NTASKS=48 # number of processes
VASP="mpirun -machinefile machinefile -np $NTASKS $VASPBIN"
CC4S="mpirun -machinefile machinefile -np $NTASKS $CC4SBIN"

enc=150 # ENCUT   for the whole calculation
egw=100 # ENCUTGW for the whole calculation


cat <<EOF | tee POSCAR
Li4 H4
1.0
4.000174 0.000000 0.000000
0.000000 4.000174 0.000000
0.000000 0.000000 4.000174
Li H
4 4
direct
0.000000 0.000000 0.000000
0.000000 0.500000 0.500000
0.500000 0.000000 0.500000
0.500000 0.500000 0.000000
0.500000 0.000000 0.000000
0.500000 0.500000 0.500000
0.000000 0.000000 0.500000
0.000000 0.500000 0.000000
EOF

cat <<EOF | tee KPOINTS
Gamma Point Mesh
       0
gamma
 1 1 1
EOF

test -f WAVECAR && rm WAVECAR

cat <<EOF | tee INCAR
#              =======================================
#              RUN DFT to get a converged guess for HF
#              =======================================
ENCUT = $enc
EDIFF = 1E-6
EOF
$VASP
cp OUTCAR OUTCAR.DFT

cat <<EOF | tee INCAR
#                          ================
#                          RUN Hartree-Fock
#                          ================
ENCUT   = $enc
EDIFF   = 1E-6
LHFCALC =.TRUE.
AEXX    = 1.0
ALGO    = C
EOF
$VASP
cp OUTCAR OUTCAR.HF


nb=$(awk <OUTCAR '
      /maximum number of plane-waves:/ {
       print $5*2-1
      }
    ')




cat <<EOF | tee INCAR
#              ========================================
#              Diagonalize Fock Operator with all bands
#              ========================================
ENCUT   = $enc
EDIFF   = 1E-6
LHFCALC = .TRUE.
AEXX    = 1.0
ISYM    = -1
ALGO    = sub
NELM    = 1
NBANDS  = $nb
EOF
$VASP
cp OUTCAR OUTCAR.HFdiag



cat <<EOF | tee INCAR
#                ====================================
#                Dump CC4S input using $nb bands.
#                ====================================
ENCUT      = $enc
EDIFF      = 1E-6
LHFCALC    = .TRUE.
AEXX       = 1.0
ISYM       = -1
ALGO       = CC4S
NBANDS     = $nb
NBANDSHIGH = $nb
EOF
$VASP
cp OUTCAR OUTCAR.CC4S


cat <<EOF | tee cc4s.in
#                     =========================
#                     Run CC4S using $nb bands.
#                     =========================
- name: TensorReader
  in:
    fileName: "EigenEnergies.yaml"
  out:
    tensor: EigenEnergies

- name: TensorReader
  in:
    fileName: "CoulombVertex.yaml"
  out:
    tensor: CoulombVertex

- name: TensorReader
  in:
    fileName: "CoulombVertexSingularVectors.yaml"
  out:
    tensor: CoulombVertexSingularVectors

- name: TensorReader
  in:
    fileName: "GridVectors.yaml"
  out:
    tensor: GridVectors

- name: TensorReader
  in:
    fileName: "CoulombPotential.yaml"
  out:
    tensor: CoulombPotential
- name: DefineHolesAndParticles
  in:
    eigenEnergies: EigenEnergies
  out:
    slicedEigenEnergies: EigenEnergies

- name: SliceOperator
  in:
    slicedEigenEnergies: EigenEnergies
    operator: CoulombVertex
  out:
    slicedOperator: CoulombVertex
- name: CoulombIntegralsFromVertex
  in:
    slicedCoulombVertex: CoulombVertex
  out:
    coulombIntegrals: CoulombIntegrals
- name: CcsdEnergyFromCoulombIntegrals
  in:
    integralsSliceSize: 100
    slicedEigenEnergies: EigenEnergies
    coulombIntegrals: CoulombIntegrals
    slicedCoulombVertex: CoulombVertex
    maxIterations: 1
    energyConvergence: 1.0E-5
    amplitudesConvergence: 1.0E-5
    mixer:
      type: DiisMixer
  out:
    amplitudes: Amplitudes

- name: StructureFactor
  in:
    amplitudes: Amplitudes
    slicedCoulombVertex: CoulombVertex
    coulombVertexSingularVectors: CoulombVertexSingularVectors
    coulombPotential: CoulombPotential
  out:
    structureFactor: SF
    deltaIntegrals: DeltaInt
    nij: Nij

- name: FiniteSizeCorrection
  in:
    gridVectors: GridVectors
    structureFactor: SF
  out:
    corrected: Corrected
    uncorrected: Uncorrected

- name: Atrip
  in:
    slicedEigenEnergies: EigenEnergies
    amplitudes: Amplitudes
    coulombIntegrals: CoulombIntegrals
  out:
    {}
EOF

$CC4S -i cc4s.in | tee  cc4s.stdout
