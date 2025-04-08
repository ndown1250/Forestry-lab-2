setwd("C:/Users/ndown/OneDrive/Documents/R/Forestry Lab 9/ForL2/files")

# Read inventory data

install.packages("terra")
install.packages("tmap")
install.packages("sf")

filepath <- 'C:/Users/ndown/OneDrive/Documents/R/Forestry Lab 9/ForL2/files/'
library(terra)
dem <- rast(paste0(filepath, "unit2.img"))

# Extract Slope and Aspect

slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)

aspect <- terrain(dem, v = "aspect", unit = "degrees")

library(tmap)
ttm()
tm_shape(slope, alpha = 0.5) + tm_raster(style = "cont", alpha = 0.6, title = "Slope(deg)")
tm_shape(aspect) + tm_raster(style = "cont")
# The degree range of each direction is as follows: 
# 0-90 degrees is N, 90-180 is E, 180-270 is S, 270-360 is W

# Reclassifying Aspect
### Aspect Classification Matrix
asp_class <- matrix(c(0,45,1,
                      45,90,1,
                      90,135,2,
                      135,180,2,
                      180,225,3,
                      225,270,3,
                      270,315,4,
                      315,360,4),
                    ncol = 3, byrow = TRUE)

### Reclassify Aspect
asp <- classify(aspect, asp_class)

### Visualize Reclassified Aspect
ttm()
tm_shape(asp) + tm_raster(style = "cat", palette = c("white","blue","green","yellow","red"),
                          labels = c(NA, "North","East","South","West"), alpha = 0.2)

# Visualize Sample Forest Inventory Plots
### Read Summary Table and Shapefile
sum_u2 <- read.csv(paste0(filepath, "sum_u2.csv"))
library(sf)
svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017.shp"))
svy_pts <- st_transform(svy_pts, 32616)
survey_pts <- subset(svy_pts, Unit == '2')

### Merge Summary Table with Plot Locations
sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)

unique(sum_u2$Plot)
unique(survey_pts$Plot)

### Convert to sf Format
sum_u2 <- st_as_sf(sum_u2, coords = c("X","Y"), crs = 32616)
sum_u2

# Create Circular Plots
### Circular Buffer Zones
sf_plot <- st_buffer(sum_u2, dist = 17.83)

# Unify Coordinate Systems
### Check CRS
crs(sf_plot, proj = T)
crs(asp, proj = T)

### Transform CRS
asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# Visualization
### Dominant Species by Aspect
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow","red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South","West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(title = "Dominant Species by Aspect",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)

### Dominant Species by Slope
ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant Species by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

### Basal Area Distribution
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow","red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South","West")) +
tm_shape(sf_plot) +
  tm_polygons('BA', title = "Basal Area (sq_ft/acre)", palette =
                "brewer.spectral") +
  tm_layout(title = "Basal Area Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

### TPA Distribution
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow","red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South","West")) +
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

### Biomass Distribution
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow","red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South","West")) +
tm_shape(sf_plot) +
  tm_polygons('bm_tonpa', title = "Biomass (tons/ac)", palette =
                "brewer.spectral") +
  tm_layout(title = "Biomass Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()


