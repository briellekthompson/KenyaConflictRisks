---
layout: page
nav_order: 2
title: Trans Nzoia Analysis
description: Trans Nzoia Analysis
---

## Table of contents
{: .no_toc .text-delta }
1. TOC
{:toc}


## Selecting Trans Nzoia

I conducted the same analysis as the Kenya-wide analysis except I narrowed in on one specific county in Kenya. I selected Trans Nzoia because it is a cropland region with high raindfed crop area with a large number of farming and water related conflicts.

<p align="center">
  <img src="{{ 'TransNzoia_details.png' | relative_url }}" width="900" height="700" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Figure displaying information about Trans Nzoia
</em>
</p>


## Data Preparation 

I ran the same data preparation as the Kenya-wide analysis to fix data to the spatial extent of Trans Nzoia
 
<p align="center">
  <img src="{{ 'conflict_v_climatedata_tn.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em>Figure displaying the farming related conflicts against the two climate data sets through time for Trans Nzoia.</em>
</p>

## Model Building 

I ran the same models as the Africa-wide analysis to determine whether a similar outcome, that over all unrest is the greatest predictor of farming conflicts, could be found for a smaller region 

Given that the data is time dependent, I conducted a timeseries analysis (using an AR1 model) to determing how well the different covariate data sets explains the farming conflict data. The following data streams were integrated as predictor variables: total conflicts, average precipitation anomaly, average temperature anomaly, year. 


The models included all combinations of these variables. 
The models included:

* Full: farming conflict ~ total conflicts + average precipitation anomaly + average temperature anomaly + year#Unrest only: farming conflict ~ total conflicts 
* Climate only: farming conflict ~ average precipitation anomaly + average temperature anomaly
* Precipitation only: farming conflict ~ average precipitation anomaly 
* Temperature only: farming conflict ~ average temperature anomaly
* Unrest trend: farming conflict ~ total conflicts + year
* Climate trend: farming conflict ~ average precipitation anomaly + average temperature anomaly + year
* Precipitation trend: farming conflict ~ average precipitation anomaly + year
* Temperature trend: farming conflict ~ average temperature anomaly + year
* Trend only: farming conflict ~ year

## Outcomes: 

Again, the top model was the unrest only model, meaning that total conflicts is a high predictor to farming related conflicts.
However, the top model using only the climate data was the precipitation only model. 


The models and the respective AIC results are below, the lowest AIC represents the model that best fit the data, indicated with **

| Model              | AIC  |       
|:------------------ |:--- -|
| Unrest only**      | 51.4 |  
| Unrest trend       | 52.5 |
| Full               | 55.0 |
| Precipitation only | 69.5 |
| Temperature only   | 69.9 | 
| Trend only         | 69.9 |
| Precipitation trend| 71.1 |
| Climate only       | 71.4 |
| Temperature trend  | 71.9 | 
| Climate trend      | 72.6 | 

<p align="center">
  <img src="{{ 'Observed_vs_fitted_tn.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Displayed how well each of the models performed in predicting the data (farming conflict data shown in brown)
</em>
</p>


<p align="center">
  <img src="{{ 'Observed_vs_bestfit_tn.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Figure displaying the top model against the true data
</em>
</p>


## Diagnostics:

<p align="center">
  <img src="{{ 'Diagnostics_tn.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Displaying diagnostic reports
</em>
</p>



