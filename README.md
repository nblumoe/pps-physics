## Introduction

An Anglican example for inference on a simple play-clj physics simulation.
The project structure follows the play-clj conventions (see section "Contents" below).

To run the inference:

- start the simulation container via `(-main)` in `pps-physics.core.desktop-launcher`
- run the simulation with inference via `(run-gravity-inference 10)` (10 posterior samples in that
  case) in `pps-physics.pp`
- results will be appended to `results/gravity/results.edn` for each run of `run-gravity-inference`
- plot results with `(plot-gravity-results)` in `pps-physics.pp`

## Contents

* `android/src` Android-specific code
* `desktop/resources` Images, audio, and other files
* `desktop/src` Desktop-specific code
* `desktop/src-common` Cross-platform game code
