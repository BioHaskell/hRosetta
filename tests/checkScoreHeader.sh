#!/bin/bash

runghc scripts/selectBest.hs 1 examples/silent/orig.out test.out; head -2 examples/silent/orig.out| tail -1; head -2 test.out|tail -1
