---
layout: page
title: Final Graphicical Representation
description: Final Graphical Representation
nav_order: 4
---

## Final Graphic: 

* If I were to create only one data vizualization, this final grapic reveals a gif of the best model compared to other models for the Africa wide analysis. 
* The takeaway is that overall civil unrest is the greatest predictor of farming related conflicts

<p align="center">
  <img src="{{ 'analysis.gif' | relative_url }}" width="900" height="700" alt="Farming conflicts vs climate anomalies over time">
  <br>
  <em> Gif displaying the time series models against the true data
</em>
</p>

The models refer to: 
* Full: farming conflict ~ total conflicts + average precipitation anomaly + average temperature anomaly + year#Unrest only: farming conflict ~ total conflicts
* Climate only: farming conflict ~ average precipitation anomaly + average temperature anomaly
* Precipitation only: farming conflict ~ average precipitation anomaly
* Temperature only: farming conflict ~ average temperature anomaly
* Unrest trend: farming conflict ~ total conflicts + year
* Climate trend: farming conflict ~ average precipitation anomaly + average temperature anomaly + year
* Precipitation trend: farming conflict ~ average precipitation anomaly + year
* Temperature trend: farming conflict ~ average temperature anomaly + year
* Trend only: farming conflict ~ year
