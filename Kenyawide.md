---
layout: page
title: Kenya-wide Analysis
nav_order: 2
description: >-
    Kenya-wide Analysis.
---


## Table of contents
{: .no_toc .text-delta }
1. TOC
{:toc}


## Analysis Question 

Are farming/herding related conflicts a reflection climate conditions,  a function of overall unrest? Or both?
The steps and outcomes of this analysis are summarized below. 

## Data Preparation 

  * Spatial match
    - Climate data: Filtered the climate data to match the spatial extent for the whole country of Kenya. Note that I did not use the land cover or the rainfed area data for the analysis below, but I conducted the filtering for those datasets

    - Conflict data: Filtered the conflict data to those that were in Kenya

  * Conflict data filtering:
    - I attempted to filter the conflict data that was related to farming, herding, or water. To do this I filtered out any conflict data event (row) that contained the following words or derivatives of these words: farm, herd, cattle, crop, livestock, flock, harvest, graz, pasture, plantation, stock, agricult, cultivat, plants, animal, ranch, water


<p align="center">
  <img src="{{ 'conflict_data.png' | relative_url }}" width="500" height="300" alt="Farming-related conflicts by year">
  <br>
  <em>Figure displaying total number of farming related conflicts through time, with colors depicting the % of events that are farming related; point size reflects fatalities per year.</em>
</p>


  * Temporal match: 
    - Climate data: I summarized the temperature and precipitation anomalies data to be the average for each year across space
    - Conflict data: I created two statistics 1) a statistic of the total number conflict each year, and 2) statistic of the total number of farming related conflicts each year
    

<p align="center">
  <img src="{{ 'conflict_v_climatedata.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em>Figure displaying the farming related conflicts against the two climate data sets through time.</em>
</p>


## Model Building 

Given that the data is time dependent, I conducted a timeseries analysis (using an AR1 model) to determine how well the different covariate data sets explains the farming conflict data. The following data streams were integrated as predictor variables: total conflicts, average precipitation anomaly, average temperature anomaly, year


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

The analysis reveals that the top predictor of farming/water related conflicts is due to overall unrest in the country (i.e., the model had the highest AIC value). However, the climatic variable that best predicts the number of farming/water related conflicts is temperature (i.e., the 4th top model according to AIC).  Where a higher temperature contributes to lower number of conflicts (i.e., had a negative slope in the model). 


The models and the respective AIC results are below. The AIC is a metric of how well the model fits the data, the lowest AIC represents the model that best fit the data, indicated with **

| Model              | AIC |       
|:------------------ |:----|
| Unrest only**      | 152 |  
| Unrest trend       | 154 |
| Full               | 155 |
| Temperature trend  | 174 | 
| Climate trend      | 175 |
| Trend only         | 177 |
| Precipitation trend| 179 |
| Temperature only   | 183 | 
| Precipitation only | 183 |
| Climate only       | 185 |


<p align="center">
  <img src="{{ 'Observed_vs_fitted_kenya.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Displayed how well each of the models performed in predicting the data (farming conflict data shown in brown)
</em>
</p>


The analysis reveals that the top predictor of farming/water related conflicts is due to overall unrest in the country (i.e., the model had the highest AIC value). However, the climatic variable that best predicts the number of farming/water related conflicts is temperature (i.e., the 4th top model according to AIC).  Where a higher temperature contributes to lower number of conflicts (i.e., had a negative slope in the model). 

<p align="center">
  <img src="{{ 'observed_vs_bestfit_kenya.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Figure displaying the top model against the true data
</em>
</p>

The summary information from the model revealed that for the unrest only model, each 10 total conflicts is associated with the addition of 0.75 farming conflicts (slope of the model) 


## Diagnostics:

<p align="center">
  <img src="{{ 'Diagnostics_kenya_bestmod.png' | relative_url }}" width="800" height="600" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Displaying diagnostic reports
</em>
</p>
