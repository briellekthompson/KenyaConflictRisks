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
file_name <- paste(path, 'precip_in_ke.csv',sep = '/')
precip_ke <- fread(file_name)

file_name <- paste(path, 'temp_in_ke.csv',sep = '/')
temp_ke <- fread(file_name)

#check location/date names are equal
all.equal(precip_ke$long, temp_ke$long)
all.equal(precip_ke$lat, temp_ke$lat)
all.equal(precip_ke$date, temp_ke$date)

precip_temp_ke <- cbind(precip_ke, temp_anom_C = temp_ke$temp_anom_C)

precip_temp_summ_ke <- precip_temp_ke %>%
  group_by(date) %>%
  summarize(
    mean_precip = mean(precip_anom_mm, na.rm = TRUE),
    mean_temp = mean(temp_anom_C, na.rm = TRUE)
  )

#precip_summ_ke

p1 <- ggplot(data = precip_temp_summ_ke) +
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

t1 <- ggplot(data = precip_temp_summ_ke) +
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



plot_grid(nrow = 1, t1, p1) 

cor(precip_temp_summ_ke$mean_precip, precip_temp_summ_ke$mean_temp)

##### annual dat #####
precip_temp_ke$year <- year(precip_temp_ke$date)

precip_temp_summ_ke_yr <- precip_temp_ke %>%
  group_by(year) %>%
  summarize(
    mean_precip = mean(precip_anom_mm, na.rm = TRUE),
    mean_temp = mean(temp_anom_C, na.rm = TRUE)
  )

#precip_summ_ke

p1a <- ggplot(data = precip_temp_summ_ke_yr) +
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

p2a <- ggplot(data = precip_temp_summ_ke_yr) +
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

cor(precip_temp_summ_ke_yr$mean_precip, precip_temp_summ_ke_yr$mean_temp)


#### conflict ####
path <- here::here('GWSC-Interview-Data', 'Conflict-Events')
file_name = paste(path, 'conflict_kenya.csv',sep = '/')
conflict_kenya <- fread(file_name)

pattern <- "(farm|herd|cattle|crop|livestock|flock|harvest|graz|pasture|plantation|stock|agricult|cultivat|plants|animal|ranch|water)"

conflict_kenya <- conflict_kenya %>%
  mutate(
    farmconflict = if_else(
      if_any(everything(), ~ str_detect(as.character(.x), regex(pattern, ignore_case = TRUE))),
      "yes", "no"
    )
  )

conflict_kenya_sum <- conflict_kenya %>% filter(farmconflict == "yes")

##Save subset
path <- here::here('GWSC-Interview-Data', 'Conflict-Events')
file_name = paste(path, 'conflict_kenya_farm.csv',sep = '/')
fwrite(conflict_kenya_sum,file_name)

#make summary through year
farmconflit_summ <- conflict_kenya %>%
  mutate(farm_c = (farmconflict == "yes")) %>%
  group_by(YEAR) %>%
  summarise(
    n_total          = n(),
    farm_n           = sum(farm_c, na.rm = TRUE),
    farm_pct         = 100 * mean(farm_c, na.rm = TRUE),  # percentage of conflicts that are farm-related
    farm_fatalities  = sum(if_else(farm_c, as.numeric(FATALITIES), 0), na.rm = TRUE)
  ) %>%
  arrange(YEAR)


farmconflit_summ <- farmconflit_summ %>%
  mutate(
    fatality_bin = cut(
      farm_fatalities,
      breaks = c(-Inf, 0, 10, 50, 100, Inf),
      labels = c("0", "1–10", "10–50", "50–100", "100+"),
      right = TRUE, include.lowest = TRUE
    )
  )


c1 <- ggplot(farmconflit_summ) +
  geom_line(aes(YEAR, farm_n), color = "chocolate4", lwd = 1) +
  geom_point(aes(YEAR, farm_n, size = fatality_bin, fill = farm_pct),
             shape = 21, color = "chocolate4", stroke = 0.35) +
  scale_size_manual(
    name = "Farming/water related \nconflict fatalities",
    values = c("0" = 1, "1–10" = 2, "10–50" = 4, "50–100" = 6, "100+" = 8),
    breaks = c("0", "1–10", "10–50", "50–100", "100+")
  ) +
  scale_fill_gradient("% of total conflict events", low = "gold1", high = "red3", na.value = NA)+
  guides(size = guide_legend(override.aes = list(stroke = 0.35))) +
  xlab("Year") + ylab("# of farming/water related conflict events") +
  theme_bw()

c1

#### conflict vs other dat ####
##--- 1. Combine data
farmdf <- farmconflit_summ
colnames(farmdf)[1] <- "year"
dat.combined <- full_join(farmdf, precip_temp_summ_ke_yr, by = "year")

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




##### GIF ####
library(maptiles)
library(terra)

conflict_pts <- st_as_sf(conflict_kenya,
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs = 4326,
                       remove = FALSE)


kenya <- rnaturalearth::ne_countries(scale = "large", country = "Kenya", returnclass = "sf")
baserast <- get_tiles(kenya, provider = "CartoDB.Positron", zoom = 7, crop = TRUE)
conflict_pts_3857 <- st_transform(conflict_pts, 3857)
kenya_3857 <- st_transform(kenya, 3857)

animation <- tm_shape(baserast) + tm_rgb() +
  tm_shape(kenya_3857) + tm_borders(col = "gray30", lwd = 1) +
  tm_shape(conflict_pts)+
  tm_bubbles(
    size = "FATALITIES", 
    col = 'black',
    fill = 'darkred'
  ) +
  tm_facets(pages = "YEAR") +
  tm_layout(
    panel.label.size     = 2.1,
    panel.label.fontface = 2,
    panel.label.height   = 2.2,
    panel.label.bg.color = "white",
    legend.text.size     = 1.5,
    legend.title.size    = 1.7,
    legend.outside        = TRUE,         # put legend outside the map frame
    legend.outside.position = "right",    # move it to the right
    legend.stack          = "vertical",   # vertical items (typical for size legend)
    outer.margins = c(0.02, 0.10, 0.02, 0.02)
  )

# Save the animated map as a gif file
tmap_animation(
  animation,
  file = paste0(here::here(), "/conflict.gif"), #uncomment to save the animation
  delay = 80)

#### build timeseries model model ####
#Question: Are farm conflicts just a reflection of overall unrest? 
#Or are they uniquely sensitive to climate conditions?
# explicitly to test whether farm conflicts are driven by general unrest
library(nlme)

#Pre analysis data format
colnames(farmconflit_summ)[1:2] <- c("year", "n_total_conflicts")
conflict_dat_wcovs <- merge(farmconflit_summ, precip_temp_summ_ke_yr)
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
#slope for total conflicts = 0.075 (SE 0.0090, p < 0.001; 95% CI [0.057, 0.093])
#Substantive effect: Each +10 total conflicts is associated with +0.75 farm conflicts 
#(95% CI +0.57 to +0.93).
#Equivalently, +100 total conflicts → +7.5 farm conflicts (95% CI +5.7 to +9.3).
#sp “marginally, about 7–9% of additional total conflicts are 
#farm-related,” recognizing this is a marginal share 
#(the intercept means the overall share is not constant across the range).

#corr + resids: 
#remaining autocorrelation is mild (matches your ACF: small lag‑1 spike).

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


plot1 <- ggplot(conflict_dat_wcovs, aes(x = year)) +
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


plot_grid(plot1, plot2, labels = c("A)", "B)"))

plot2 <- ggplot(conflict_dat_wcovs, aes(x = year)) +
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


plot2b <- ggplot(conflict_dat_wcovs, aes(x = year)) +
  geom_line(aes(y = unrest_trend, color = "Unrest trend"), lwd = 0.8) +
  geom_line(aes(y = climate_only, color = "Climate only"), lwd = 0.8) +
  geom_line(aes(y = climate_trend, color = "Climate trend"), lwd = 0.8) +
  geom_line(aes(y = precip_only, color = "Precip. only"), lwd = 0.8) +
  geom_line(aes(y = pecip_trend, color = "Precip. trend"), lwd = 0.8) +
  geom_line(aes(y = temp_only, color = "Temp. only"), lwd = 0.8) +
  geom_line(aes(y = temp_trend, color = "Temp. trend"), lwd = 0.8) +
  geom_line(aes(y = full, color = "Full model"), lwd = 0.8) +
  geom_line(aes(y = trend, color = "Trend model"), lwd = 0.8) +
  geom_line(aes(y = farm_n, color = "Data"), lwd = 2) +
  geom_line(aes(y = unrest_only, color = "Unrest only"), lwd = 0.8) +

  
  scale_color_manual(
    values = c("Data" = "chocolate4",
               "Unrest only" = "blue3",
               "Unrest trend" = "gray80",
               "Climate only" = "gray80",
               "Climate trend" = "gray80",
               "Precip. only" = "gray80",
               "Precip. trend" = "gray80",
               "Temp. only" = "gray80",
               "Temp. trend" = "gray80",
               "Full model" = "gray80",
               "Trend model" = "gray80"),
    name = "Models"
  ) +
  labs(y = "# of farming/water related conflict events",
       title = "Observed vs Fitted (Top Model = Unrest only)")+
  theme_bw() + 
  theme(strip.background=element_rect(color="white",
                                      fill="white"),
        panel.border = element_rect(color = "gray", size = 1.5), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 13),
        legend.position = "bottom"
  ) 

plot2b

##### FINAL GRAPHIC ####
library(magick)
#plot1 then plot2 GIF

png("plot_a.png", width = 800, height = 600, res = 120); print(plot1); dev.off()
png("plot_b.png", width = 800, height = 600, res = 120); print(plot2b); dev.off()

# --- Read them as magick images ---
img_a <- image_read("plot_a.png")
img_b <- image_read("plot_b.png")

# --- Build the animation by repeating frames ---
cycles <- 10      # how many A→B toggles
hold   <- 8       # how many frames to hold each plot

# Create a sequence A, B, A, B, ... (as a list)
seq_list <- rep(list(img_a, img_b), cycles)

# For each element, repeat it 'hold' times, then concatenate all
frames_list <- lapply(seq_list, function(im) rep(im, hold))
frames <- do.call(c, frames_list)   # 'c()' concatenates magick-image vectors

# Optional: add a short crossfade each time (insert morphed frames)
# crossfade <- image_morph(c(img_a, img_b), frames = 6)  # make between A and B once
# Then insert 'crossfade' after each B or A as you like.

# --- Animate and write ---
gif <- image_animate(frames, fps = 5, optimize = TRUE, loop = 0)  # loop=0 for infinite
image_write(gif, paste0(here::here(), "/analysis.gif"))
 


#### TABLES ####

#Full: farming conflict ~ total conflicts + average precipitation anomaly + average temperature anomaly + year
#Unrest only: farming conflict ~ total conflicts 
#Climate only: farming conflict ~ average precipitation anomaly + average temperature anomaly
#Precipitation only: farming conflict ~ average precipitation anomaly 
#Temperature only: farming conflict ~ average temperature anomaly
#Unrest trend: farming conflict ~ total conflicts + year
#Climate trend: farming conflict ~ average precipitation anomaly + average temperature anomaly + year
#Precipitation trend: farming conflict ~ average precipitation anomaly + year
#Temperature trend: farming conflict ~ average temperature anomaly + year
#Trend only: farming conflict ~ year

#Table of AIC model ranked (a measure of how well the model fits the data, the lowest AIC = best model, indicated with **) 

#  | Model              | AIC |       
#  |:------------------ |:----|
#  | Unrest only**      | 152 |  
#  | Unrest trend       | 154 |
#  | Full               | 155 |
#  | Temperature trend  | 174 | 
#  | Climate trend      | 175 |
#  | Trend only         | 177 |
#  | Precipitation trend| 179 |
#  | Temperature only   | 183 | 
#  | Precipitation only | 183 |
#  | Climate only       | 185 |


  
###############################################################################
#------------------------------OLD CODE GLM ----------------------------------#
###############################################################################
# #### build glm model ####
# #combine data:
# colnames(farmconflit_summ)[1:2] <- c("year", "n_total_conflicts")
# 
# conflict_dat_wcovs <- merge(farmconflit_summ, precip_temp_summ_ke_yr)
# 
# 
# #build linear model: 
# 
# m_full <- glm(
#   farm_n ~ n_total_conflicts + mean_precip  + mean_temp  + year,
#   data = conflict_dat_wcovs, 
#   family = poisson(link = "log")
# )
# 
# summary(m_full)
# performance::check_overdispersion(m_full) #over dispersion so new model:
# 
# 
# m_full <- MASS::glm.nb(
#   farm_n ~ n_total_conflicts + mean_precip  + mean_temp  + year,
#   data = conflict_dat_wcovs
# )
# 
# summary(m_full)
# 
# #make models: 
# m_noconf   <- MASS::glm.nb(farm_n ~ mean_precip + mean_temp + year, data = conflict_dat_wcovs)
# m_noprecip <- MASS::glm.nb(farm_n ~ n_total_conflicts + mean_temp + year, data = conflict_dat_wcovs)
# m_notemp   <- MASS::glm.nb(farm_n ~ n_total_conflicts + mean_precip + year, data = conflict_dat_wcovs)
# m_noyear   <- MASS::glm.nb(farm_n ~ n_total_conflicts + mean_precip + mean_temp, data = conflict_dat_wcovs)
# 
# m_onlyconf   <- MASS::glm.nb(farm_n ~ n_total_conflicts, data = conflict_dat_wcovs)
# m_onlyprecip <- MASS::glm.nb(farm_n ~ mean_precip, data = conflict_dat_wcovs)
# m_onlytemp  <- MASS::glm.nb(farm_n ~ mean_temp, data = conflict_dat_wcovs)
# 
# m_precipconf <- MASS::glm.nb(farm_n ~ mean_precip + n_total_conflicts, data = conflict_dat_wcovs)
# m_tempconf  <- MASS::glm.nb(farm_n ~ mean_temp + n_total_conflicts, data = conflict_dat_wcovs)
# m_preciptemp<- MASS::glm.nb(farm_n ~ mean_precip + mean_temp, data = conflict_dat_wcovs)
# 
# 
# m_quad <- MASS::glm.nb( farm_n ~ n_total_conflicts + poly(mean_precip, 2, raw = TRUE) + 
#                           poly(mean_temp, 2, raw = TRUE) + year,
#                         data = conflict_dat_wcovs)
# 
# m_int <- MASS::glm.nb(farm_n ~ n_total_conflicts + mean_precip * mean_temp + year, data = conflict_dat_wcovs)
# 
# 
# cands <- list(
#   full      = m_full,
#   noconf    = m_noconf,
#   noprecip  = m_noprecip,
#   notemp    = m_notemp,
#   noyear    = m_noyear,
#   quad      = m_quad,
#   int       = m_int,
#   onlyconf  = m_onlyconf, 
#   onlyprecip = m_onlyprecip,
#   onlytemp = m_onlytemp, 
#   precipconf = m_precipconf,
#   tempconf = m_tempconf, 
#   preciptemp = m_preciptemp
#   
# )
# 
# aic_tab <- MuMIn::model.sel(cands, rank = "AICc")
# print(aic_tab)
# 
# # Make sure MuMIn is attached (or use MuMIn:: prefixes)
# library(MuMIn)
# 
# # Compute AICc for each candidate model safely
# aiccs <- sapply(cands, function(m) MuMIn::AICc(m))
# 
# # Identify the best model by name
# best_name <- names(which.min(aiccs))
# best_mod  <- cands[[best_name]]
# 
# # Inspect the AICc table
# print(aiccs[order(aiccs)])
# 
# # Now run diagnostics on the best model
# res <- DHARMa::simulateResiduals(best_mod)
# plot(res)
# DHARMa::testTemporalAutocorrelation(res, time = conflict_dat_wcovs$year)
# 
# performance::check_collinearity(m_noyear)