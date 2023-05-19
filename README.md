# Statistical analysis of the PREPARE study

## Background

This repository contains R markdown files for the analysis of data measured during the Pre-emptive Pharmacogenomic Testing for Preventing Adverse Drug Reactions (PREPARE) study as conducted by the Ubiquitous Pharmacogenomics Consortium. For details on study design, analysis, and interpretation we refer to the following publications:

  * Generating evidence for precision medicine: considerations made by the Ubiquitous Pharmacogenomics Consortium when designing and operationalizing the PREPARE study (https://doi.org/10.1097/FPC.0000000000000405)
  * A 12-gene pharmacogenetic panel to prevent adverse drug reactions: an open-label, multicentre, controlled, cluster-randomised crossover implementation study (https://doi.org/10.1016/S0140-6736%2822%2901841-4)

## Structure & Scope

This repository contains reproducible R markdown files to reproduce analyses conducted in the papers above. The code has been slightly modified to run outside the production environment. Readability has been increased by removing unneeded analyses. Original files have been archived for potential future review.

Structure of the repository:

  * src: source files and R-markdown files to conduct the analyses
  * src/PREPARErun.R: main file to run all analyses
  * md: markdown html output of the analyses
  * src/data: data files (not included)
  * src/results: intermediate results of longer pre-computations (not included)

## Full reproduction run

This repository lacks two folders required for a full reproduction of the analyses: _src/data_, _src/results_. These data have not been included based on privacy constraints and the data sharing policy of the consortium. Please contact corresponding authors about data availability. Original analyses have been run using R 4.1.1 under operating system openSUSE 15.1. Current output was produced with R 4.2.3/openSUSE Tumbleweed(20230328) leading to slight numeric differences.

Otherwise, a full code review is possible. The file _RgenericAllRaw.R_ contains an internal library which has been condensed to a single file for reproducible analyses for the PREPARE study. This library is maintained using a testing battery (implemented with https://github.com/sboehringer/testme) which is not included in this repository.

## Citing this repository

If you want to cite this repository, please use the URL of this repository (https://github.com/KFTleiden/PREPARE) together with the citations given above.

## Author & License

Stefan Böhringer (s.boehringer _at_ lumc.nl) has authored all code in this repository. Permission is granted to use the code under a GNU Lesser General Public License (LGPL 3.0, https://www.gnu.org/licenses/lgpl-3.0.en.html). Copyright remains with the author.
