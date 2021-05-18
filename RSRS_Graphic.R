# RSRS Special Issue: Innovation in Regional Graphics


# 1. R Session Setup ------------------------------------------------------

### Libs
library(sf)
library(tmap)
library(tidyverse)
library(h3jsr)
library(ceramic)
library(magick)
options(scipen = 999)

## Setup
tmap_mode("plot") # Static plotting - works well with basemaps from ceramic
windowsFonts("Times" = windowsFont("Times New Roman")) # Font selection
Sys.setenv(MAPBOX_API_KEY=
             "pk.eyJ1Ijoic2dwYmFsbGEiLCJhIjoiY2tncnZxc3FqMGhzaTJ6bzcxNTk3bDNldCJ9.tYEAVCBey8y5tzXx9i0lPw") # Ceramic API Key

# 2. Data -----------------------------------------------------------------

## Full patterns
full <- st_read("Data/Chicago_Retail_POINTS_PATTERNS.gpkg") %>%
  st_transform(4326) %>%
  select(safegraph_place_id, top_category, sub_category, w27_visits, w28_visits, w29_visits, 
         w30_visits, w31_visits, w32_visits) %>%
  drop_na()

## Lookup of SafeGraph Categories -> Retail Aggregation
lookup <- data.table::fread("Data/SafeGraph_Places_Categories_LDC.csv", header = TRUE)
lookup <- lookup %>% select(-c(V4))

## Merge on the LDC categories, clean and extract only features we want
ptns <- merge(full, lookup, by = c("top_category", "sub_category"), all.x = TRUE)
ptns_clean <- ptns %>%
  mutate_if(is.character, as.factor) %>%
  select(safegraph_place_id, top_category, sub_category, ldc_aggregation,
         w27_visits, w28_visits, w29_visits, w30_visits, w31_visits, w32_visits) %>%
  rename(march_02 = w27_visits, march_09 = w28_visits, march_16 = w29_visits, 
         march_23 = w30_visits, march_30 = w31_visits, april_06 = w32_visits)

## Chicago MSA Boundary
bbox <- st_read("Data/Chicago_Metro_Area.shp") %>%
  st_transform(4326) %>%
  select(geometry)

## Basemap 
bm_bbox <- rgdal::readOGR("Data/Ceramic_Bbox.shp")
basemap <- ceramic::cc_location(loc = bm_bbox, zoom = 9,
                                base_url = "https://basemaps.cartocdn.com/rastertiles/dark_all/{zoom}/{x}/{y}.png")

## Map Labels
lbls <- st_read("Data/Labels.shp")

# 3. Calculation of Retail Visits by Retail Type --------------------------

## Calculate total and proportional weekly visits by Retail Type
db_by_type <- ptns_clean %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  group_by(ldc_aggregation) %>%
  dplyr::summarise("02/03" = sum(march_02), 
                   "09/03" = sum(march_09), 
                   "16/03" = sum(march_16),
                   "23/03" = sum(march_23), 
                   "30/03" = sum(march_30), 
                   "06/04" = sum(april_06)) %>%
  drop_na() %>%
  gather(week, total_weekly_visits, 2:7) %>%
  arrange(ldc_aggregation) %>%
  mutate_if(is.character, as.factor) %>%
  filter(ldc_aggregation != "MISC") %>%
  group_by(week) %>%
  mutate(percent = total_weekly_visits/sum(total_weekly_visits) * 100) %>%
  mutate(pos = (cumsum(total_weekly_visits) - 0.5 * total_weekly_visits)) %>%
  mutate(label = paste0(sprintf("%0.1f", percent), "%"))

## Reorder weeks
db_by_type$week <- factor(db_by_type$week, levels = c("02/03", "09/03", "16/03", "23/03", "30/03", "06/04"))

## Visualise
ggplot(db_by_type, aes(x = week, y = total_weekly_visits, fill = ldc_aggregation)) +
  geom_bar(position = position_stack(), stat= "identity", width = 1, alpha = 0.75) +
  scale_fill_manual(values = c("#ffffcc", "#fed976", "#fd8d3c", "#bd0026") , name = "Retail Type", labels = c("Comparison", "Convenience", "Leisure", "Services")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 4, colour = "White", family = "Times") +
  ylab("Total Visits") +
  xlab("Week Beginning") +
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.background = element_rect(colour = "black", fill = "black", color = NA), legend.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(colour="White"), axis.text = element_text(colour = "White", family = "Times", size = 12),
        axis.ticks = element_line(colour = "White"), text = element_text(colour = "White", family = "Times", size = 12))
ggsave("Outputs/barchart.tiff", dpi = 800)

# 3. Mapping of Retail Mobility ---------------------------------------

## Construct a H3 Grid for Chicago MSA
bbox_h3 <- polyfill(bbox, 7, FALSE)
bbox_h3 <- h3_to_polygon(unlist(bbox_h3$h3_polyfillers), simple = FALSE)
bbox_h3 <- st_transform(bbox_h3, crs = 4326)

## Join Patterns to the Grid - Calculating Total Visits in each H3
db_h3 <- st_join(bbox_h3, ptns_clean, join = st_contains)
db_h3 <- db_h3 %>%
  group_by(h3_address) %>%
  dplyr::summarise(march_02_total = sum(march_02),
                   march_09_total = sum(march_09),
                   march_16_total = sum(march_16),
                   march_23_total = sum(march_23),
                   march_30_total = sum(march_30),
                   april_06_total = sum(april_06)) %>%
  drop_na() %>%
  st_as_sf()

## Map Template
tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "march_02_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 02/03", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)


# 4. Static Maps ----------------------------------------------------------

## WB 02/03
t1 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "march_02_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 02/03", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)
tmap_save(t1, "Outputs/Maps/t1.tiff", dpi = 800)


## WB 09/03
t2 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "march_09_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 09/03", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)
tmap_save(t2, "Outputs/Maps/t2.tiff", dpi = 800)


## WB 16/03
t3 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "march_16_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 16/03", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)
tmap_save(t3, "Outputs/Maps/t3.tiff", dpi = 800)


## WB 23/03
t4 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "march_23_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 23/03", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)
tmap_save(t4, "Outputs/Maps/t4.tiff", dpi = 800)


## WB 30/03
t5 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "march_30_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 30/03", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)
tmap_save(t5, "Outputs/Maps/t5.tiff", dpi = 800)


## WB 06/03
t6 <- tm_shape(basemap) +
  tm_rgb() +
  tm_shape(db_h3) +
  tm_fill(col = "april_06_total", palette = "-YlOrRd", title = "Visits to Retail Places",
          style = "fixed", breaks = c(1, 2500, 5000, 10000, 20000, 30000, Inf), alpha = 0.75) +
  tm_shape(lbls) +
  tm_dots(size = 0.05, col = "white") +
  tm_text("Name", col = "white", size = 0.75, just = "bottom", ymod = 0.25, alpha = 1,
          fontface = "bold", shadow = TRUE) +
  tm_layout(title = "WB 06/04", frame = FALSE, legend.position = c("left", "bottom"),
            legend.frame = FALSE, legend.show = TRUE, legend.bg.alpha = 0.8,
            title.position = c("left", "bottom"), title.color = "White", 
            fontfamily = "Times", title.fontfamily = "Times", legend.text.color = "white",
            title.size = 1, attr.color = "white",
            legend.text.size = 0.75, legend.title.size = 1, bg.color = "grey85",
            outer.margins = c(0,0,0,0)) +
  tm_shape(bbox) + 
  tm_fill(col = "orange", alpha = 0.05) +
  tm_borders(col = "orange", alpha = 0.5, lwd = 2) + 
  tm_add_legend("fill", col = "black", size = 0.8, labels = "Chicago Metro Area",
                border.col = "orange") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.75, breaks = c(0, 25, 50),
               text.color = "white", bg.color = "white", bg.alpha = 0.15)
tmap_save(t6, "Outputs/Maps/t6.tiff", dpi = 800)


# 5. Animation  -----------------------------------------------------------

## Swap wd
setwd("Outputs/Maps")

## Assemble GIF
list.files(pattern = '*.tiff') %>%
  image_read() %>%
  image_join() %>%
  image_animate(fps = 1, loop = 100) %>%
  image_write("RSRS.gif")
