#!/bin/bash

runghc scripts/selectBest.hs 1 examples/silent/orig.out test.out; head -3 examples/silent/orig.out| tail -1; head -3 test.out|tail -1
