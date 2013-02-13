#!/bin/bash

(python /home/m/Projects/mpi/amycsrosetta/asynuclein/strand_colors2.py ; runghc silentToPyMol.hs best.out ) > script.pml

pymol S_*.pdb script.pml
