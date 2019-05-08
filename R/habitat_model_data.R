library(raster)
library(rgdal)
library(rgeos)
library(dismo)
library(viridis)
library(foreach)
library(maptools)
source("R/functions.R")

# Load spatial mask layer
ch_rst <- raster("data/grids/VIC_GDA9455_GRID_CENTRALHIGHLANDS_100.tif")

# Reduce to smaller area for purpose of application paper example
ch_rst <- crop(ch_rst, extent(ch_rst, 1, 750, 624, 1373))

# Gather spatial information from mask layer
ch_res <- res(ch_rst)
ch_extent <- extent(ch_rst)
ch_proj <- ch_rst@crs

# Create clipping mask that is X kilometers larger than extent in both directions
km_offset <- 10
ch_extent_offset <- extent(ch_rst) + km_offset*(1000)

# Create window for focal operations
buffer_radius <- 564
window <- focalWeight(raster(ncols=11, nrows=11, xmn=0),
                      buffer_radius,
                      type = 'circle')

# Load and process raw spatial data

system(paste0("gdalwarp -overwrite -tr 100 100 -s_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -t_srs '+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -te ", paste(ch_extent_offset[1], ch_extent_offset[3], ch_extent_offset[2], ch_extent_offset[4]), " ", getwd(), "/data/grids/raw/GRID_NVIS5_1_AUST_EXT_MVS/aus5_1e_mvs/w001000.adf ", getwd(), "/data/grids/raw/NVIS_GDA9455.tif")) # reproject to project coordinate system
NVIS <- raster("data/grids/raw/NVIS_GDA9455.tif")
NVIS <- calc(NVIS, fun = function(x) {x[x != 3] <- NA; return(x) }) # only retain wet sclerophyll Eucalyptus forest
NVIS[!is.na(NVIS)] <- 1

PROP_WET_FOREST <- focal(NVIS, window, na.rm = TRUE) 
PROP_WET_FOREST[is.na(PROP_WET_FOREST)] <- 0
PROP_WET_FOREST <- crop(PROP_WET_FOREST, ch_extent)


OG100 <- readOGR("data/shapefiles", "OG100", stringsAsFactors = FALSE)
OG100 <- subset(OG100, X_OGDESC == "Old Growth Forest")
OG100 <- spTransform(OG100, ch_proj) # reproject to project coordinate system
OG100 <- crop(OG100, ch_extent_offset)
OG100r <- rasterize(OG100, raster(res = ch_res, ext = ch_extent_offset, crs = ch_proj), 1)

#writeRaster(OG100r, file = "data/grids/raw/OLDGROWTH_GDA9455.tif")
#system(paste0("gdal_proximity.py ", getwd(), "/data/grids/raw/OLDGROWTH_GDA9455.tif ", getwd(), "/data/grids/raw/VIC_GDA9455_DIST_OLDGROWTH.tif -values 1 -distunits GEO")) # reproject to project coordinate system


PROP_OLD_GROWTH <- focal(OG100r, window, na.rm = TRUE) 
PROP_OLD_GROWTH[is.na(PROP_OLD_GROWTH)] <- 0
PROP_OLD_GROWTH <- crop(PROP_OLD_GROWTH, ch_extent)


system(paste0("gdalwarp -overwrite -tr 100 100 -s_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -t_srs '+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -te ", paste(ch_extent_offset[1], ch_extent_offset[3], ch_extent_offset[2], ch_extent_offset[4]), " ", getwd(), "/data/grids/raw/alpsbk_aust_y2009_sf1a2.tif ", getwd(), "/data/grids/raw/AUSCOVER_GDA9455.tif")) # reproject to project coordinate system
AUSCOVER <- raster("data/grids/raw/AUSCOVER_GDA9455.tif")
AUSCOVER <- calc(AUSCOVER, fun = function(x) {x[x != 44 & x != 54 & x != 55 & x != 64] <- NA; return(x) }) # only retain tall forest (>30m)
AUSCOVER[!is.na(AUSCOVER)] <- 1

PROP_TALL_FOREST <- focal(AUSCOVER, window, na.rm = TRUE) 
PROP_TALL_FOREST[is.na(PROP_TALL_FOREST)] <- 0
PROP_TALL_FOREST <- crop(PROP_TALL_FOREST, ch_extent)

gg_env_vars <- stack(PROP_WET_FOREST, PROP_OLD_GROWTH, PROP_TALL_FOREST) #Combine all maps to single stack
names(gg_env_vars) <- c("PROP_WF", "PROP_OG", "PROP_TF")

# Write out covariates to binary file
save(gg_env_vars, file="data/gg_env_vars")

# Load in RFA spatial boundary, subset, and reproject
rfa_boundary <- readOGR(dsn="data/shapefiles", layer="RFA")
rfa_boundary <- rfa_boundary[rfa_boundary@data$NAME == "CENTRAL HIGHLANDS", ]
rfa_boundary <- spTransform(rfa_boundary, ch_proj)

# Load in Greater Glider presence and absence data and reproject
gg_pres_abs <- read.csv(file="data/tabular/vicAtlasGGpresabs.csv")

coords <- data.frame(X = gg_pres_abs$LongitudeGDA94,
                     Y = gg_pres_abs$LatitudeGDA94)
coordinates(coords) <- c("X", "Y")
proj4string(coords) <- CRS("+init=epsg:4283") # GDA94
GG_POINTS <- data.frame("PA" = gg_pres_abs[, 1], spTransform(coords, ch_proj)@coords)

# Plot boundary and points
plot(rfa_boundary)
points(GG_POINTS[GG_POINTS$PA == 0, 2:3], col = "darkred")
points(GG_POINTS[GG_POINTS$PA == 1, 2:3], pch = 2)

# Sample covariate values - be sure to use select correct data
cov_sample <- as.data.frame(extract(gg_env_vars, GG_POINTS[, 2:3]))

# Create modelling dataset - be sure to use select correct data
gg_model_data <- cbind("PA" = GG_POINTS[, 1], cov_sample)

# Check for NA values and omit accordingly:
sum(is.na(gg_model_data))
gg_model_data <- na.omit(gg_model_data)

# Review pairwise correlation of covariates:
cor(gg_model_data[, 2:4])

# Review covariate data types
str(gg_model_data)

# Review data ranges
apply(gg_model_data[, 2:4], 2, range)

# Split the data into training and testing data
set.seed(123)
gg_sample <- sample(nrow(gg_model_data), round(0.75 * nrow(gg_model_data))) # the row indexes of our 75% sample
gg_model_data_75 <- gg_model_data[gg_sample, ] # 75% of the data used to train the model
gg_model_data_25 <- gg_model_data[-gg_sample, ] # 25% of the data used to test the model

# Write the modelling data out to csv files and as an R object
write.csv(gg_model_data, file = "data/gg_model_data.csv")
write.csv(gg_model_data_25, file = "data/gg_model_data_25.csv")
write.csv(gg_model_data_75, file = "data/gg_model_data_75.csv")
save("gg_model_data", "gg_model_data_25", "gg_model_data_75", file = "data/gg_model_datasets")

