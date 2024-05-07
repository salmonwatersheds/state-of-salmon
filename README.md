README
================
Stephanie Peacock
2024-May-07

## state-of-salmon

### Overview

This repo contains code and data that informs the Pacific Salmon
Foundation’s State of Salmon Report.

### Files

- `code` contains R code for data compiltation and analysis:
  - `1_regions-expansions.R` expands spawner abundance from stream-level
    spawner surveys to regional aggregate abundance.
  - `2_compile-regional-data.R` compiles regional estimates of spawners
    and returns from various sources in `data/`, or uses the expanded
    spawner abundances from 1. where regional data were not available,
    and smooths these time series using a geometric running mean over
    the generation length for the species/region. Outputs the raw and
    smoothed time series as `output/sps-data.csv`.
  - `3_calc-metrics.R` reads in `output/sps-data.csv` and calculates the
    current status and trends reported on in the State of Salmon report.
  - other supporting code in `colours.R`, `expansion-functions.R`, and
    `functions.R` are sourced in the above files and contain functions
    for expansion, smoothing, and plotting output.
- `data` has source data sets used in `2_compile-regional-data.R`.
- `output` contains processed data and summaries that are usually
  produced by R code in `code` and used in plotting.
- `docs` contains Technical Documentation on the methods - see
  <https://salmonwatersheds.github.io/state-of-salmon/>

### Acknowledgements

Code here is written by Steph Peacock and Eric Hertz. PSF’s State of
Salmon reporting is led by Eileen Jones and funded by the [Pacific
Salmon Foundation](www.psf.ca).

### More information

- Technical Documentation of the data sources and approach can be found
  at <https://salmonwatersheds.github.io/state-of-salmon/>.
- Output data sets provided here in `output` are available through the
  Salmon Watershed Program’s [Data
  Library](https://data.salmonwatersheds.ca/) with associated metdata
  and citation information.
- If you have questions or would like more information, contact Steph
  Peacock <speacock@psf.ca>.
