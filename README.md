# Spatiotemporal Clustering and Poverty Forecasting Using ST-DBSCAN, CBWM, and GSTAR

## Overview

This repository contains an R script developed for research on regional poverty analysis and forecasting across six provinces in Sulawesi, Indonesia.

The study explores the integration of **Spatiotemporal Density-Based Spatial Clustering of Applications with Noise (ST-DBSCAN)**, **Cluster-Based Weight Matrix (CBWM)**, and **Generalized Space-Time Autoregressive (GSTAR)** models to investigate spatial and temporal patterns in regional poverty data.

The research focuses on constructing cluster-based spatial weight matrices and evaluating their potential contribution to regional poverty forecasting.

## Research Objectives

The main objectives of this research are to:

* Identify spatiotemporal clusters in regional poverty data using ST-DBSCAN.
* Construct cluster-based spatial weight matrices based on the resulting cluster structure.
* Integrate cluster-based weighting approaches into the GSTAR modeling framework.
* Compare alternative spatial weighting approaches for poverty forecasting.
* Evaluate clustering quality and forecasting performance using quantitative metrics.

## Methodology

The analytical workflow consists of the following stages:

1. **Data preprocessing:** Prepare regional poverty data and the geographical information required for spatial analysis.
2. **Spatiotemporal clustering:** Apply ST-DBSCAN to identify clusters based on spatial, temporal, and attribute-related information.
3. **Cluster validation:** Evaluate clustering quality using the Silhouette Coefficient (SC), Davies–Bouldin Index (DBI), and Density-Based Clustering Validation (DBCV).
4. **Spatial weight matrix construction:** Develop cluster-based spatial weight matrices and prepare alternative weighting structures for comparison.
5. **GSTAR modeling:** Estimate GSTAR models using the selected spatial weight matrices.
6. **Forecasting evaluation:** Assess predictive performance using error metrics and rolling-origin evaluation.

## Spatial Weighting Approaches

The research compares the following spatial weighting approaches:

* Inverse Distance Weighting (IDW)
* Normalized Cross-Correlation Function (NCCF)
* Cluster-Based Weight Matrix with IDW (CBWM-IDW)
* Cluster-Based Weight Matrix with NCCF (CBWM-NCCF)

These approaches are evaluated to investigate how alternative spatial weighting structures affect GSTAR forecasting performance.

## Evaluation Metrics

### Clustering Validation

* **Silhouette Coefficient (SC):** Evaluates cluster cohesion and separation.
* **Davies–Bouldin Index (DBI):** Assesses the similarity between clusters based on their dispersion and separation.
* **Density-Based Clustering Validation (DBCV):** Evaluates clustering quality using density-based structure.

### Forecasting Performance

* **Root Mean Squared Error (RMSE)**
* **Mean Absolute Error (MAE)**
* **Mean Absolute Percentage Error (MAPE)**

Rolling-origin evaluation is also used to assess forecasting performance across multiple forecast origins.

## Repository Contents

The repository currently contains:

* **R analysis script:** Implements the research workflow for ST-DBSCAN clustering, CBWM construction, GSTAR modeling, and forecasting evaluation.
* **README.md:** Documents the research objectives, methodology, and repository usage.
* **.gitignore:** Specifies files and directories excluded from Git tracking.

## Requirements

This project uses the R programming language.

To reproduce the analysis, users need:

* R installed on their computer.
* RStudio or another compatible R development environment.
* The R packages required by the analysis script.
* The regional poverty dataset and geographical files required by the script.

Install the required packages according to the package dependencies specified in the R script.

## How to Use

1. Clone or download this repository.
2. Open the R script in RStudio.
3. Prepare the input dataset and geographical files required by the analysis.
4. Update the file paths in the script to match the local environment.
5. Ensure that all required R packages are installed.
6. Run the script and inspect the generated clustering, modeling, and forecasting outputs.

**Note:** The input dataset and geographical files are not included in this repository. Users must provide the required files and configure the corresponding paths before running the analysis.

## Research Scope

The study focuses on poverty data from six provinces in Sulawesi, Indonesia.

The research investigates the potential integration of spatiotemporal clustering and cluster-based spatial weighting within the GSTAR framework for regional time-series forecasting.

The reported results should be interpreted in accordance with the final validated model specifications and empirical evaluation.

## Reproducibility

Reproducibility depends on the availability of the input data, geographical files, R environment, package versions, and model specifications.

For consistent results, users should document the software environment and ensure that all preprocessing and modeling procedures are applied consistently.

## License

No license has currently been assigned to this repository. Please contact the repository owner for permission before reusing or redistributing the code.

## Keywords

`R Programming` · `ST-DBSCAN` · `CBWM` · `GSTAR` · `Spatial Statistics` · `Spatiotemporal Analysis` · `Poverty Forecasting` · `Time-Series Analysis`
