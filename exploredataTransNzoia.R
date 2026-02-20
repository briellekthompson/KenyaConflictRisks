library(tidyverse)
library(data.table)
library(sf)
library(dplyr)
library(rnaturalearth)
library(terra)
library(gridExtra)
library(cowplot)
library(performance)
library(tmap)

#### annomalies ####
#1. precip + temp:
path <- here::here('GWSC-Interview-Data', 'Precip-and-Temp-Monthly-Anomalies')
file_name <- paste(path, 'precip_in_tn.csv',sep = '/')
precip_tn <- fread(file_name)

file_name <- paste(path, 'temp_in_tn.csv',sep = '/')
temp_tn <- fread(file_name)

#check location/date names are equal
all.equal(precip_tn$long, temp_tn$long)
all.equal(precip_tn$lat, temp_tn$lat)
all.equal(precip_tn$date, temp_tn$date)

precip_temp_tn <- cbind(precip_tn, temp_anom_C = temp_tn$temp_anom_C)

precip_temp_summ_tn <- precip_temp_tn %>%
  group_by(date) %>%
  summarize(
    mean_precip = mean(precip_anom_mm, na.rm = TRUE),
    mean_temp = mean(temp_anom_C, na.rm = TRUE)
  )

#precip_summ_tn

p1 <- ggplot(data = precip_temp_summ_tn) +
  geom_point(aes(x = date, y = mean_precip), color = "#1B9E77FF")+
  geom_line(aes(x = date, y = mean_precip), color = "#1B9E77FF") +
  geom_hline(aes(yintercept = 0), color = 'black',linetype = "dashed", lwd = 1 )+
  xlab("Year") + ylab("Average precipitation anomaly (mm)")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
  ) 

t1 <- ggplot(data = precip_temp_summ_tn) +
  geom_point(aes(x = date, y = mean_temp), color = "darkslateblue")+
  geom_line(aes(x = date, y = mean_temp), color = "darkslateblue")+
  geom_hline(aes(yintercept = 0), color = 'black',linetype = "dashed", lwd = 1 )+
  xlab("Year") + ylab("Average temperature anomaly (\u00B0C)")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
  ) 




plot_grid(nrow = 2, t1, p1) 

cor(precip_temp_summ_tn$mean_precip, precip_temp_summ_tn$mean_temp)

##### annual dat #####
precip_temp_tn$year <- year(precip_temp_tn$date)

precip_temp_summ_tn_yr <- precip_temp_tn %>%
  group_by(year) %>%
  summarize(
    mean_precip = mean(precip_anom_mm, na.rm = TRUE),
    mean_temp = mean(temp_anom_C, na.rm = TRUE)
  )

#precip_summ_tn

p1a <- ggplot(data = precip_temp_summ_tn_yr) +
  geom_point(aes(x = year, y = mean_precip), color = "#1B9E77FF")+
  geom_line(aes(x = year, y = mean_precip), color = "#1B9E77FF") +
  geom_hline(aes(yintercept = 0), color = 'black',linetype = "dashed", lwd = 1 )+
  xlab("Year") + ylab("Average precipitation anomaly (mm) ")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
  ) 

p2a <- ggplot(data = precip_temp_summ_tn_yr) +
  geom_point(aes(x = year, y = mean_temp), color = "darkslateblue")+
  geom_line(aes(x = year, y = mean_temp), color = "darkslateblue")+
  geom_hline(aes(yintercept = 0), color = 'black',linetype = "dashed", lwd = 1 )+
  xlab("Year") + ylab("Average temperature anomaly (\u00B0C) ")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
  ) 

plot_grid(nrow = 2, p2a, p1a) 

cor(precip_temp_summ_tn_yr$mean_precip, precip_temp_summ_tn_yr$mean_temp)

#### conflict ####
path <- here::here('GWSC-Interview-Data', 'Conflict-Events')
conflict_TN_df <- fread(paste(path, 'conflict_tn.csv',sep = '/'))

pattern <- "(farm|herd|cattle|crop|livestock|flock|harvest|graz|pasture|plantation|stock|agricult|cultivat|plants|animal|ranch|water)"

conflict_TN <- conflict_TN_df %>%
  mutate(
    farmconflict = if_else(
      if_any(everything(), ~ str_detect(as.character(.x), regex(pattern, ignore_case = TRUE))),
      "yes", "no"
    )
  )

conflict_TN_sum <- conflict_TN %>% filter(farmconflict == "yes")

##Save subset
path <- here::here('GWSC-Interview-Data', 'Conflict-Events')
file_name = paste(path, 'conflict_tn_farm.csv',sep = '/')
fwrite(conflict_TN_sum,file_name)

#make summary through year
farmconflit_summ_tn <- conflict_TN %>%
  mutate(farm_c = (farmconflict == "yes")) %>%
  group_by(YEAR) %>%
  summarise(
    n_total          = n(),
    farm_n           = sum(farm_c, na.rm = TRUE),
    farm_pct         = 100 * mean(farm_c, na.rm = TRUE),  # percentage of conflicts that are farm-related
    farm_fatalities  = sum(if_else(farm_c, as.numeric(FATALITIES), 0), na.rm = TRUE)
  ) %>%
  arrange(YEAR)


farmconflit_summ_tn <- farmconflit_summ_tn %>%
  mutate(
    fatality_bin = cut(
      farm_fatalities,
      breaks = c(-Inf, 0, 10, 50, 100, Inf),
      labels = c("0", "1–10", "10–50", "50–100", "100+"),
      right = TRUE, include.lowest = TRUE
    )
  )


c1 <- ggplot(farmconflit_summ_tn) +
  geom_line(aes(YEAR, farm_n), color = "black") +
  geom_point(aes(YEAR, farm_n, size = farm_fatalities, fill = farm_pct),
             shape = 21, color = "black", stroke = 0.35) +
  # scale_size_manual(
  #   name = "Farming/water related \nconflict fatalities",
  #   values = c("0" = 1, "1–10" = 2, "10–50" = 4, "50–100" = 6, "100+" = 8),
  #   breaks = c("0", "1–10", "10–50", "50–100", "100+")
  # ) +
  scale_fill_gradient("% of total conflict events", low = "gold1", high = "red3", na.value = NA)+
  guides(size = guide_legend(override.aes = list(stroke = 0.35))) +
  xlab("Year") + ylab("# of farming/water related conflict events") +
  theme_bw()

c1

#### conflict v other dat ####
##--- 1. Combine data
farmdf <- farmconflit_summ_tn
colnames(farmdf)[1] <- "year"
dat.combined <- full_join(farmdf, precip_temp_summ_tn_yr, by = "year")

##--- 2. Compute a linear mapping so farm_n fits the mean_temp scale
range_y1 <- range(dat.combined$mean_temp, na.rm = TRUE)  # mean_temp range
range_y2 <- range(dat.combined$farm_n,    na.rm = TRUE)  # farm_n range

b <- diff(range_y1) / diff(range_y2)
a <- range_y1[1] - b * range_y2[1]

dat_plot <- dat.combined |>
  mutate(farm_n_scaled = a + b * farm_n)

##--- 3.Make the plot

g1 <- ggplot(dat_plot, aes(x = year)) +
  geom_point(aes(y = mean_temp, color = "Average temperature anomaly (\u00B0C)"),
             na.rm = TRUE) +
  geom_line(aes(y = mean_temp, color = "Average temperature anomaly (\u00B0C)"),
            lwd = 1, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = 'slateblue',linetype = "dashed", lwd = 0.5)+
  geom_point(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
             na.rm = TRUE) +
  geom_line(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
            lwd = 1, na.rm = TRUE) +
  scale_y_continuous(
    name = "Average temperature anomaly (\u00B0C)",
    sec.axis = sec_axis(~ (.-a)/b, name = "# of farming/water related \nconflict events")  # inverse of scaling
  ) +
  scale_color_manual(
    values = c("Average temperature anomaly (\u00B0C)" = "darkslateblue", 
               "# of farming/water related conflict events" = "chocolate4"),
    name = NULL
  ) +
  labs(x = "Year") +
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
        legend.position = "none"
  ) 


g1

# 2) Compute a linear mapping so farm_n fits the mean_precip scale
range_y1 <- range(dat.combined$mean_precip, na.rm = TRUE)  # mean_temp range
range_y2 <- range(dat.combined$farm_n,    na.rm = TRUE)  # farm_n range

b <- diff(range_y1) / diff(range_y2)
a <- range_y1[1] - b * range_y2[1]

dat_plot <- dat.combined |>
  mutate(farm_n_scaled = a + b * farm_n)

g2 <- ggplot(dat_plot, aes(x = year)) +
  geom_point(aes(y = mean_precip, color = "Average precipitation anomaly (mm)"),
             na.rm = TRUE) +
  geom_line(aes(y = mean_precip, color = "Average precipitation anomaly (mm)"),
            lwd = 1, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "#5CB99D",linetype = "dashed", lwd = 0.5)+
  geom_point(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
             na.rm = TRUE) +
  geom_line(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
            lwd = 1,  na.rm = TRUE) +
  scale_y_continuous(
    name = "Average precipitation \nanomaly (mm)",
    sec.axis = sec_axis(~ (.-a)/b, name = "# of farming/water related \nconflict events")  # inverse of scaling
  ) +
  scale_color_manual(
    values = c("Average precipitation anomaly (mm)" = "#1B9E77FF", 
               "# of farming/water related conflict events" = "chocolate4"),
    name = NULL
  ) +
  labs(x = "Year") +
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
        legend.position = "none"
  ) 

g2


legendp <- ggplot(dat_plot, aes(x = year)) +
  
  geom_point(aes(y = mean_temp, color = "Average temperature anomaly (\u00B0C)"),
             na.rm = TRUE) +
  geom_line(aes(y = mean_temp, color = "Average temperature anomaly (\u00B0C)"),
            lwd = 1, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = 'slateblue',linetype = "dashed", lwd = 0.5)+
  geom_point(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
             na.rm = TRUE) +
  geom_line(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
            lwd = 1, na.rm = TRUE) +
  
  geom_point(aes(y = mean_precip, color = "Average precipitation anomaly (mm)"),
             na.rm = TRUE) +
  geom_line(aes(y = mean_precip, color = "Average precipitation anomaly (mm)"),
            lwd = 1, na.rm = TRUE) +
  geom_hline(aes(yintercept = 0), color = "#5CB99D",linetype = "dashed", lwd = 0.5)+
  geom_point(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
             na.rm = TRUE) +
  geom_line(aes(y = farm_n_scaled, color = "# of farming/water related conflict events"),
            lwd = 1,  na.rm = TRUE) +
  scale_y_continuous(
    name = "Mean temperature",
    sec.axis = sec_axis(~ (.-a)/b, name = "# of farming/water related conflict events")  # inverse of scaling
  ) +
  scale_color_manual(
    values = c("Average temperature anomaly (\u00B0C)" = "darkslateblue",
               "Average precipitation anomaly (mm)" = "#1B9E77FF", 
               "# of farming/water related conflict events" = "chocolate4"),
    name = NULL
  ) +
  labs(x = "Year") +
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13)
  ) 

legend <- cowplot::get_plot_component(legendp, "guide-box", return_all = TRUE)
legend <- legend[[1]]

top_row <- plot_grid(
  g1, g2,
  ncol = 2, rel_widths = c(0.8, 0.8), align = "hv"
)

bottom_row <- plot_grid(
  legend, NULL,
  ncol = 2, rel_widths = c(0.99, 0.01)  # keep same column widths as top row
)

#Stack rows, making the legend row shorter
compareplot <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(0.7, 0.1))

compareplot

#### build timeseries model model ####
#Question: Are farm conflicts just a reflection of overall unrest? 
#Or are they uniquely sensitive to climate conditions?
# explicitly to test whether farm conflicts are driven by general unrest
library(nlme)

#Pre analysis data format
colnames(farmconflit_summ_tn)[1:2] <- c("year", "n_total_conflicts")
conflict_dat_wcovs <- merge(farmconflit_summ_tn, precip_temp_summ_tn_yr)
conflict_df <- conflict_dat_wcovs %>% arrange(year)

##--- 1.  looking at correlation
print(cor(conflict_dat_wcovs[, c("n_total_conflicts","mean_precip","mean_temp","year")], use="complete.obs"))

##--- 2. define model forms: 
forms <- list(
  full          = farm_n ~ n_total_conflicts + mean_precip + mean_temp + year,
  
  unrest_only   = farm_n ~ n_total_conflicts,
  climate_only  = farm_n ~ mean_precip + mean_temp,
  precip_only  = farm_n ~ mean_precip ,
  temp_only  = farm_n ~ mean_temp,
  
  unrest_trend  = farm_n ~ n_total_conflicts + year,
  climate_trend = farm_n ~ mean_precip + mean_temp + year,
  precip_trend = farm_n ~ mean_precip +  year,
  temp_trend = farm_n ~ mean_temp + year,
  
  trend_only    = farm_n ~ year
)

##--- 3. Fit AR1 models 
fits <- lapply(forms, function(fm) {
  gls(fm, data = conflict_df, correlation = corAR1(form = ~ year), method = "ML")
})

##--- 4.Compare model outcomes with AIC 
aics <- tibble(
  model = names(fits),
  AIC   = sapply(fits, AIC)
) %>% arrange(AIC)

print(aics)

# Example: unrest_only vs full
anova(fits$unrest_only, fits$full)

##--- 5. diagnostics for top model
#diagnostics: 
par(mfrow = c(1, 2))
plot(fitted(fits$unrest_only), resid(fits$unrest_only), pch = 16,
     xlab = "Fitted", ylab = "Residuals",
     main = "Residuals vs Fitted (REML)")
abline(h = 0, col = "red")
acf(resid(fits$unrest_only, type = "normalized"), main = "ACF of GLS residuals")
par(mfrow = c(1, 1))


##--- 6. re run best model with REML
best_name <- aics$model[1]
best_form <- forms[[best_name]]

m_best_reml <- gls(best_form, data = conflict_df,
                   correlation = corAR1(form = ~ year),
                   method = "REML")

summary(m_best_reml)
confint(m_best_reml)
intervals(m_best_reml)$corStruct  # AR(1) phi and CI

#interpretation: 
#slope for total conflicts = 0.075 (SE 0.0090, p < 0.001)
#slope for total conflicts = 0.3 (SE 0.05, p < 0.001)
#Substantive effect: Each +10 total conflicts is associated with +3 farm conflicts 

#corr + resids: 
#remaining autocorrelation is fine enough
#correnlation though is -0.811 which is kinda high but could be reduced by centering the data with the mean

##--- 7. create model vs predicted
#models:
conflict_dat_wcovs$unrest_only <- predict(fits$unrest_only)
conflict_dat_wcovs$unrest_trend <- predict(fits$unrest_trend)
conflict_dat_wcovs$climate_only <- predict(fits$climate_only)
conflict_dat_wcovs$climate_trend <- predict(fits$climate_trend)
conflict_dat_wcovs$precip_only <- predict(fits$precip_only)
conflict_dat_wcovs$pecip_trend <- predict(fits$precip_trend)
conflict_dat_wcovs$temp_only <- predict(fits$temp_only)
conflict_dat_wcovs$temp_trend <- predict(fits$temp_trend)
conflict_dat_wcovs$full <- predict(fits$full)
conflict_dat_wcovs$trend <- predict(fits$trend)


ggplot(conflict_dat_wcovs, aes(x = year)) +
  geom_line(aes(y = farm_n, color = "Data"), lwd = 2) +
  geom_line(aes(y = unrest_only, color = "Unrest only"), lwd = 0.8) +
  geom_line(aes(y = unrest_trend, color = "Unrest trend"), lwd = 0.8) +
  geom_line(aes(y = climate_only, color = "Climate only"), lwd = 0.8) +
  geom_line(aes(y = climate_trend, color = "Climate trend"), lwd = 0.8) +
  geom_line(aes(y = precip_only, color = "Precip. only"), lwd = 0.8) +
  geom_line(aes(y = pecip_trend, color = "Precip. trend"), lwd = 0.8) +
  geom_line(aes(y = temp_only, color = "Temp. only"), lwd = 0.8) +
  geom_line(aes(y = temp_trend, color = "Temp. trend"), lwd = 0.8) +
  geom_line(aes(y = full, color = "Full model"), lwd = 0.8) +
  geom_line(aes(y = trend, color = "Trend model"), lwd = 0.8) +
  
  scale_color_manual(
    values = c("Data" = "chocolate4",
               "Unrest only" = "blue3",
               "Unrest trend" = "lightblue",
               "Climate only" = "green4",
               "Climate trend" = "lightgreen",
               "Precip. only" = "purple3",
               "Precip. trend" = "plum1",
               "Temp. only" = "gold3",
               "Temp. trend" = "lightgoldenrod1",
               "Full model" = "black",
               "Trend model" = "gray"),
    name = "Models"
  ) +
  labs(y = "# of farming/water related conflict events",
       title = "Observed vs Fitted (All models)")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
        legend.position = "bottom"
  ) 

ggplot(conflict_dat_wcovs, aes(x = year)) +
  geom_line(aes(y = farm_n, color = "Data"), lwd = 2) +
  geom_line(aes(y = unrest_only, color = "Unrest only"), lwd = 0.8) +
  
  scale_color_manual(
    values = c("Data" = "chocolate4",
               "Unrest only" = "darkblue"),
    name = "Models"
  ) + labs(y = "# of farming/water related conflict events",
           title = "Observed vs Fitted (Top Model = Unrest only)")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
        legend.position = "bottom"
  ) 

#### TABLE ####

#Table of AIC model ranked (a measure of how well the model fits the data, the lowest AIC = best model, indicated with **) 

#  | Model              | AIC  |       
#  |:------------------ |:--- -|
#  | Unrest only**      | 51.4 |  
#  | Unrest trend       | 52.5 |
#  | Full               | 55.0 |
#  | Precipitation only | 69.5 |
#  | Temperature only   | 69.9 | 
#  | Trend only         | 69.9 |
#  | Precipitation trend| 71.1 |
#  | Climate only       | 71.4 |
#  | Temperature trend  | 71.9 | 
#  | Climate trend      | 72.6 | 



