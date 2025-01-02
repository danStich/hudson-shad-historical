# hudson-shad-historical
Simulated population response of Hudson River American shad to historical changes in habitat

## Overview
This repository contains simulation and plotting code, and results for an analysis of historical, longitudinal, and latitudinal changes in simulated American shad (*Alosa sapidissima*) population abundance in the Hudson River, NY, USA. The code are preliminary and under development as part of an ongoing study.

## Files and Directories
`hudson-river-hrf-parallel-sequential-dam-removal.R` is a simulation script for R that runs the analysis for our research project. It relies on parallel processing with the `snowfall` package to speed up simulations that are run using the `anadrofish` R package, version 2.1.0. Installation instructions and examples for the latter are available here: [https://github.com/danStich/anadrofish](https://github.com/danStich/anadrofish). 

`plots.R` contains code for compiling the simulation results and creating a plot.

`results/` folder contains the simulation output in `.rda` files and the exported figure for our study.

## Funding
This project was supported through a grant from the Hudson River Foundation to the Research Foundations for SUNY Oneonta and CUNY Queens.
