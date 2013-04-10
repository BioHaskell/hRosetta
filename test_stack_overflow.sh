#!/bin/bash

time ghc --make -O3 -rtsopts -prof -fno-prof-count-entries -fforce-recomp scripts/selectBest.hs  && time scripts/selectBest 10 examples/silent/stack_overflow.out.gz stack_overflow_test +RTS -xc -Pa -hc -K1K 
#time ghc --make -O3 -rtsopts -prof -fno-prof-count-entries -auto-all -caf-all -fforce-recomp scripts/selectBest.hs  && time scripts/selectBest 10 examples/silent/stack_overflow.out stack_overflow_test +RTS -xc -Pa -hc -K1K 
#time ghc --make -O3 -rtsopts -prof -auto-all -caf-all -fforce-recomp scripts/selectBest.hs  && time scripts/selectBest 10 examples/silent/stack_overflow.out stack_overflow_test +RTS -xc -Pa -hc -K1K 
#time ghc --make -O1 -rtsopts -prof -auto-all -caf-all -fforce-recomp scripts/selectBest.hs  && time scripts/selectBest 10 examples/silent/stack_overflow.out stack_overflow_test +RTS -xc -Pa -hc -K1K 

date # to facilitate comparison of runs
exit 0
echo "Reduction of stack space is crucial for quick testing!"
cat - > output <<EOF
[1 of 4] Compiling Rosetta.Util     ( Rosetta/Util.hs, Rosetta/Util.o )
[2 of 4] Compiling Rosetta.SS       ( Rosetta/SS.hs, Rosetta/SS.o )
[3 of 4] Compiling Rosetta.Silent   ( Rosetta/Silent.hs, Rosetta/Silent.o )
[4 of 4] Compiling Main             ( scripts/selectBest.hs, scripts/selectBest.o )
Linking scripts/selectBest ...

real	0m19.903s
user	0m17.125s
sys	0m1.892s
*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
  Rosetta.Silent.parseSilentEventLine.parse,
  called from Rosetta.Silent.parseSilentEventLine,
  called from Rosetta.Silent.parseSilentEvents,
  called from Rosetta.Silent.parseSilent.(...),
  called from Rosetta.Silent.parseSilent,
  called from Rosetta.Silent.parseSilentFile,
  called from Rosetta.Silent.processSilentFile,
  called from Main.main
*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
  Rosetta.Silent.parseSilentEventLine.parse,
  called from Rosetta.Silent.parseSilentEventLine,
  called from Rosetta.Silent.parseSilentEvents,
  called from Rosetta.Silent.parseSilent.(...),
  called from Rosetta.Silent.parseSilent,
  called from Rosetta.Silent.parseSilentFile,
  called from Rosetta.Silent.processSilentFile,
  called from Main.main
Stack space overflow: current size 8388608 bytes.
Use `+RTS -Ksize -RTS' to increase it.

real	62m48.724s
user	36m48.066s
sys	0m5.936s

EOF
