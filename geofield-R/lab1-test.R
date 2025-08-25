#
# Test for Labs 1-?
# 
# Outcomes:
#   Basic  

library(terra)

# TEST
# points collected sitting in Office
pts <- vect('tmp-data/office-test.gpx')
pts$name[grep('Wpt.*',pts$name)] = 'PS224_mo10'

plot(pts, 'name')
p1 <- geom(pts)[,c('geom', 'x', 'y')]

# apply over columns
# mean
apply(p1, 2, mean)[2:3]
# standard deviation - one (sample sd)
format(apply(p1, 2, sd)[2:3], scientific = FALSE)

# create new central coordinate
ptc <- vect(matrix(apply(p1, 2, mean)[2:3], ncol=2), type='points', crs='epsg:4326')
plot(pts, col='black', cex=0.6)
points(ptc, col='red', pch=3, cex=1.2)

# earth info
# LONGITUDE - Equatorial radius = 6,378,137 m
# Distance per degree at equator
# 2πR/360 = (2π×6371000)/360 ≈ 111,319.5 meters - average radius
# 2πR/360 = (2π×6,378,137)/360 ≈ 111,319.5 meters - at equator
# 6,378,137 × 0.76397 = 85,044.75 meters
(pi/180) * 6378137 * 0.76397
#
# LATITUDE - 
#

# conversion to meters for SD
#  1deg longitude = 111,320 × cos(lat_radians)
# or long_m = long_d x cos(lat_rad) x R (earths radius)
# 111,320 × cos(latitude) ≈ 84,963.6 meters
apply(p1,2,sd)[2] * ((2*pi*6.371e6)/360) * cos(apply(p1, 2, mean)[3] * pi/180)
apply(p1,2,sd)[2] * ((2*pi*6.378137e6)/360) * cos(apply(p1, 2, mean)[3] * pi/180)
# empirical approximation 
apply(p1,2,sd)[2] * 111320 * cos(apply(p1, 2, mean)[3] * pi/180) 
sd_lon = apply(p1, 2, sd)[2] * 8.5e4
cat('SD for longitude is \n', sd_lon ,'\n')
# LONGITUDE SD - 1.93 m
# 
# LAT
#  1deg latitude ≈ 111,132 meters (all latitudes)
sd_lat = apply(p1, 2, sd)[3] * 111132
cat('SD for latitude is \n', sd_lat ,'\n')
# LATITUDE SD - 4.96 m

# THOUGHTS
# it's common for latitude error to be greater than longitude because
# GPS satellites orbit in inclined paths (spread out more east-west) at mid-latitudes
# leads to better geometric precision east-west
# GDOP - geometric dilution of precision - tells you how good GPS geometry is for precise postioning
# HDOP - horizontal & VDOP - vertical 
# other
# multipath error - depends on orientation of local objects (canyon, trees, street, etc.)
# sample size may also impact this


# PLOT
uvu <- vect('tmp-data/shp/campus/buildings-2018-uvu.geojson')
uv <- project(uvu, 'EPSG:32612') # WGS84 - UTM Zone 12N 
putm <- project(pts, uv)
pcutm <- project(ptc, uv)
plot(uv, col=adjustcolor('grey', 0.4))
points(putm, col='black', cex=0.6)
points(pcutm, col='red', pch=3, cex=1.2)
# zoom in
plot(ext(buffer(putm, 10)))
points(putm, col='black', cex=0.6)
points(pcutm, col='red', pch=3, cex=1.2)

# # #
# PLOT sd - v1
plot(ext(buffer(putm, 5)))
points(putm, col='black', cex=0.6)
points(pcutm, col='red', pch=3, cex=1.2)
# lon
plot(buffer(pcutm, sd_lon), border='grey', lty=2, add=T)
plot(buffer(pcutm, sd_lon*2), border='grey', lty=2, add=T)
plot(buffer(pcutm, sd_lon*3), border='grey', lty=2, add=T)
# lat
plot(buffer(pcutm, sd_lat), border='black', lty=3, add=T)
plot(buffer(pcutm, sd_lat*2), border='black', lty=3, add=T)
plot(buffer(pcutm, sd_lat*3), border='black', lty=3, add=T)


# # #
# More complex SD PLOT
# Create temporary placeholder
circle_buf <- buffer(pcutm, width = 1)
# scale X and Y separately
# center geometry at 0,0
center <- crds(pcutm)
geom_centered <- geom(circle_buf)
geom_centered[, "x"] <- geom_centered[, "x"] - center[1]
geom_centered[, "y"] <- geom_centered[, "y"] - center[2]
# scale x and y separate
scale_x <- sd_lon
scale_y <- sd_lat
geom_centered[, "x"] <- geom_centered[, "x"] * scale_x
geom_centered[, "y"] <- geom_centered[, "y"] * scale_y
# translate back to original location
geom_scaled <- geom_centered
geom_scaled[, "x"] <- geom_scaled[, "x"] + center[1]
geom_scaled[, "y"] <- geom_scaled[, "y"] + center[2]

# recreate polygon and plot
elliptical_buf <- vect(list(geom_scaled), type = "polygons", crs = crs(pcutm))
# plot
plot(ext(buffer(putm, 5)))
points(putm, col='black', cex=0.6)
points(pcutm, col='red', pch=3, cex=1.2)
plot(buffer(pcutm, sd_lon), border='grey', lty=2, add=T)
plot(buffer(pcutm, sd_lat), border='black', lty=2, add=T)
plot(elliptical_buf, border = "blue", add=T, lty=3)
# add sd - calculate from projected data

# function
buffer_sd_ellipse <- function(center_pts_utm, sd_x, sd_y){
  #
  # Creates an elliptical buffer with different radii in the
  # north–south and east–west directions
  # requires utm center point
  #
  crs_text <- crs(center_pts_utm) # check
  if(!grepl("UTM", crs_text, ignore.case = TRUE)){
    stop("Data not in UTM coordinates...")}
  circle_buf <- buffer(center_pts_utm, width = 1)
  # center geometry at 0,0
  center <- crds(center_pts_utm)
  geom_centered <- geom(circle_buf)
  geom_centered[, "x"] <- geom_centered[, "x"] - center[1]
  geom_centered[, "y"] <- geom_centered[, "y"] - center[2]
  # scale x and y separate 
  geom_centered[, "x"] <- geom_centered[, "x"] * sd_x
  geom_centered[, "y"] <- geom_centered[, "y"] * sd_y
  # translate back to original location
  geom_scaled <- geom_centered
  geom_scaled[, "x"] <- geom_scaled[, "x"] + center[1]
  geom_scaled[, "y"] <- geom_scaled[, "y"] + center[2]
  # return polygon buffer
  buffer_ellip <- vect(list(geom_scaled), 
                       type = "polygons", crs = crs(center_pts_utm))
  return(buffer_ellip)
}

# # # 
# FINAL SD plot
plot(ext(buffer(putm, 5)))
points(putm, col='black', cex=0.6)
points(pcutm, col='red', pch=3, cex=1.2)
plot(elliptical_buf, border = "gray10", add=T, lty=3)
plot(buffer_sd_ellipse(pcutm, sd_lon*2, sd_lat*2), border = "gray60", add=T, lty=3)
plot(buffer_sd_ellipse(pcutm, sd_lon*3, sd_lat*3), border = "gray80", add=T, lty=3)
legend('topright', )

# HOW many sig figures?
# if 0.00001 is 1 meter
# only report accuracy up to 0.0001


# reproject and plot (WGS84)
# elliptical_buf_ll <- project(elliptical_buf, "EPSG:4326")


# # # # # # # # # # # # # # # # # #
 # # # # # # # # #
# # # # # # # # # # # # # # # # # #
# LAB 1
# outcomes:
#  - measure autonomous GNSS with phone
#  - export as csv, gpx, geojson, etc.
#  - visualize
#  - calculate statistics



# Must use app that allows you to log location rather than specify point on basemap
# recommended:
#  Gaia
#  ...

# INITIAL DISCUSSION: 
# How Phone GPS works - limitations
# Naming conventions and organization
# Correct measurement techniques

# MEASURE
# measure 5 corners of ponds (3x each)
# measure point against building and another in open location?
# EXPORT: as csv, gpx, or geojson (ensure that coordinates Lat Long are produced)
#   email generally works best
#   other options?
# NAMING and EXPORT reminders

# IMPORTING & VISUALIZATION
# ArcGIS Pro, QGIS, or R (for now)
# conveying information


# ANALYZE
# create equation of SD - step-by-step
# use provided equation
# conversions of sd (accuracy vs precision) 
#  - Lat ~ 1deg = 111km (tips) - 0.00001 is 1 meter / 
#  - Long (depends on Lat) ~ 1 deg = 85 km
# unit conversions
#
# why conversions don't work with lat/long???
#   coordinate systems...

# (optional)
# NEW visualization w/ center points
# PCS conversion
# use standard deviation buffer

# DISCUSS
# SD - most and least point
# do these measurements make sense based on visualization?
# do they exhibit a pattern?
# effective for delineation?
# 

# (optional)
# LOAD in custom Basemap ?




# # # # # # # # # # # # # # # # # #
# # # # # # # # #
# # # # # # # # # # # # # # # # # #
##############
## LAB 2

# calculate distances between points