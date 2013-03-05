#!/bin/bash

runghc scripts/selectBest.hs 1 examples/silent/orig.out test.out; head -4 examples/silent/orig.out| tail -1; head -4 test.out|tail -1
