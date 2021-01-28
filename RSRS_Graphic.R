# RSRS Special Issue: Innovation in Regional Graphics


# 1. R Session Setup ------------------------------------------------------

### Libs
library(sf)
library(tmap)
library(tidyverse)
library(h3jsr)
library(magick)
library(ceramic)

## Setup
tmap_mode("plot") # Static plotting - works well with basemaps from ceramic
windowsFonts("Times" = windowsFont("Times New Roman")) # Font selection
Sys.setenv(MAPBOX_API_KEY=
             "pk.eyJ1Ijoic2dwYmFsbGEiLCJhIjoiY2tncnZxc3FqMGhzaTJ6bzcxNTk3bDNldCJ9.tYEAVCBey8y5tzXx9i0lPw") # Ceramic API Key

# 2. Data -----------------------------------------------------------------

## Data - Patterns for Chicago MSA Retail Locations
db <- st_read("Data/Patterns_for_RSRS.gpkg") %>%
  st_transform(4326) %>%
  rename(march_2nd_visits = w27_visits, march_9th_visits = w28_visits, march_16th_visits = w29_visits,
         march_23rd_visits = w30_visits, march_30th_visits = w31_visits, april_6th_visits = w32_visits)

## Data - Chicago MSA Boundary
bbox <- st_read("Data/Chicago_Metro_Area.shp") %>%
  st_transform(4326) %>%
  select(geometry)

## Data - Chicago MSA Boundary w/ Buffer (better basemap)
bm_bbox <- st_read("Data/Ceramic_Bbox.shp") %>%
  st_transform(4326)

## Extract Basemap for Static Mapping
basemap <- ceramic::cc_location(loc = bm_bbox, zoom = 9,
                                base_url = "https://basemaps.cartocdn.com/rastertiles/dark_all/{zoom}/{x}/{y}.png")


# 3. Calculation of Retail Mobility ---------------------------------------

## Construct a H3 Grid for Chicago MSA
bbox_h3 <- polyfill(bbox, 7, FALSE)
bbox_h3 <- h3_to_polygon(unlist(bbox_h3$h3_polyfillers), simple = FALSE)
bbox_h3 <- st_transform(bbox_h3, crs = 4326)

## Join Patterns to the Grid - Calculating Total Visits in each H3
db_h3 <- st_join(bbox_h3, db, join = st_contains)
db_h3 <- db_h3 %>%
  group_by(h3_address) %>%
  dplyr::summarise("02/03" = sum(march_2nd_visits), 
                   "09/03" = sum(march_9th_visits), 
                   "16/03" = sum(march_16th_visits),
                   "23/03" = sum(march_23rd_visits), 
                   "30/03" = sum(march_30th_visits), 
                   "06/04" = sum(april_6th_visits)) %>%
  drop_na() %>%
  st_as_sf()


# 4. Static Maps ----------------------------------------------------------

## Here i build each map individually, writing them out so they can be animated in section 5

## 2nd March 
p1 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "02/03", style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf),
          labels = c("< 2,500", "2,500 to 4,999", "5,000 to 9,999", "10,000 to 19,999", 
                     "20,000 to 29,999", "> 30,000"),
          palette = "inferno", alpha = 0.4,
          title = "Visits to Retail Stores") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title = "WB 02/03",
            title.position = c("left", "bottom"), title.color = "white", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1,
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange")
tmap_save(p1, "week1.png")

## 9th March
p2 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "09/03", style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf),
          labels = c("< 2,500", "2,500 to 4,999", "5,000 to 9,999", "10,000 to 19,999", 
                     "20,000 to 29,999", "> 30,000"),
          palette = "inferno", alpha = 0.4,
          title = "Visits to Retail Stores") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title = "WB 09/03",
            title.position = c("left", "bottom"), title.color = "white", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1,
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange")
tmap_save(p2, "week2.png")

## 16th March 
p3 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "16/03", style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf),
          labels = c("< 2,500", "2,500 to 4,999", "5,000 to 9,999", "10,000 to 19,999", 
                     "20,000 to 29,999", "> 30,000"),
          palette = "inferno", alpha = 0.4,
          title = "Visits to Retail Stores") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title = "WB 16/03",
            title.position = c("left", "bottom"), title.color = "white", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1,
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange")
tmap_save(p3, "week3.png")

## 23rd March
p4 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "23/03", style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf),
          labels = c("< 2,500", "2,500 to 4,999", "5,000 to 9,999", "10,000 to 19,999", 
                     "20,000 to 29,999", "> 30,000"),
          palette = "inferno", alpha = 0.4,
          title = "Visits to Retail Stores") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title = "WB 23/03",
            title.position = c("left", "bottom"), title.color = "white", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1,
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange")
tmap_save(p4, "week4.png")


## 30th March
p5 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "30/03", style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf),
          labels = c("< 2,500", "2,500 to 4,999", "5,000 to 9,999", "10,000 to 19,999", 
                     "20,000 to 29,999", "> 30,000"),
          palette = "inferno", alpha = 0.4,
          title = "Visits to Retail Stores") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title = "WB 30/03",
            title.position = c("left", "bottom"), title.color = "white", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1,
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange")
tmap_save(p5, "week5.png")

## 6th April
p6 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "06/04", style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf),
          labels = c("< 2,500", "2,500 to 4,999", "5,000 to 9,999", "10,000 to 19,999", 
                     "20,000 to 29,999", "> 30,000"),
          palette = "inferno", alpha = 0.4,
          title = "Visits to Retail Stores") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title = "WB 06/04",
            title.position = c("left", "bottom"), title.color = "white", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1,
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange")
tmap_save(p6, "week6.png")


# 5. Animation  -----------------------------------------------------------

## Assemble GIF
list.files(pattern = '*.png') %>%
  image_read() %>%
  image_join() %>%
  magick::image_animate(fps = 1, loop = 100) %>%
  image_write("out.gif")
