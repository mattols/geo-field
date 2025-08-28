#
# Lab 1 - values only
#
#

# read in points
p1 <- matrix(c(
  -111.71452, 40.27809,
  -111.71459, 40.27809,
  -111.71453, 40.27816,
  -111.71454, 40.27809,
  -111.71452, 40.27805,
  -111.71451, 40.27806,
  -111.71454, 40.27806,
  -111.71452, 40.27803,
  -111.71453, 40.27807,
  -111.71455, 40.27799
), ncol = 2, byrow = TRUE)
colnames(p1) <- c("x", "y")


plot(p1[,1],p1[,2], asp=1)
apply(p1, 2, mean)
# convert to meters
apply(p1, 2, sd) * c(84963, 111319) # conversions

# convert distances
x_diff_m <- p1[,1] - apply(p1, 2, mean)[1] * 111132
y_diff_m <- p1[,2] - apply(p1, 2, mean)[2] * 84964
sqrt((x_diff_m)^2 + (y_diff_m)^2)
  

sqrt((p1[,1] - apply(p1, 2, mean)[1])^2 + (p1[,2] - apply(p1, 2, mean)[2])^2) 


# Approximate constants
R <- 6378137 # equatorial radius - average is 6371000
deg_to_rad <- pi / 180
lat_mean <- mean(p1[,2])
lat_mean_rad <- lat_mean * deg_to_rad

# Convert all points to meters relative to center
x_m <- (p1[,1] - mean(p1[,1])) * deg_to_rad * R * cos(lat_mean_rad)
y_m <- (p1[,2] - mean(p1[,2])) * deg_to_rad * R

# Standard deviations in meters
sd_x <- sd(x_m)
sd_y <- sd(y_m)
cat('Std dev. for \n longitude:', sd_x , 'm',
    '\n latitude:', sd_y, 'm')

# p1_meters <- cbind(x = x_m, y = y_m)

plot(x_m, y_m, asp=1)


# CREATE ELLIPSE
# Center point (in meters)
center_x <- 0
center_y <- 0

# Create angles
theta <- seq(0, 2*pi, length.out = 200)

# Parametric equation of an ellipse
ellipse_x <- center_x + sd_x * cos(theta)
ellipse_y <- center_y + sd_y * sin(theta)
scale_factor <- 2  # for 2-sigma buffer
ellipse_x2 <- center_x + scale_factor * sd_x * cos(theta)
ellipse_y2 <- center_y + scale_factor * sd_y * sin(theta)

# Plot the original points (asp - aspect ratio of the x and y same unit)
plot(x_m, y_m, asp = 1, pch = 19, col = "black", cex=0.6,
     xlab = "East–West", ylab = "North–South",
     main = "GPS points with sigma")

# Add sd ellipses
lines(ellipse_x, ellipse_y, col = adjustcolor("firebrick", 0.6), lty = 2)
lines(ellipse_x2, ellipse_y2, col = adjustcolor("orange3", 0.6), lty = 2)

# Add center point
points(0, 0, pch = 3, col = "gray", cex = 1.2)

# legend
legend("topright",                        # position (can also use "bottomleft", etc.)
       legend = c("\u03C3 (68%)", "2\u03C3 (95%)"),  # labels with sigma symbol
       col = c("firebrick", "orange3"),          # matching colors
       lwd = 2,                           # line width: x2
       lty = 2,                     # line type: dashed
       bty = "n")                         # no box around the legend

legend("bottomright",
       legend = c(
         paste0("\u03C3: ", round(sd_x, 2), " m E–W, ", round(sd_y, 2), " m N–S")),
       bty = "n",  # No box
       cex = 0.9)





# 1. Compute the mean (centroid) point
mean_point <- colMeans(p1)  # gives c(mean_x, mean_y)

# 2. Compute Euclidean distances from each point to the mean point
distances <- sqrt((p1[,1] - mean_point[1])^2 + (p1[,2] - mean_point[2])^2)

# 3. Find index of closest and farthest point
closest_index <- which.min(distances)
farthest_index <- which.max(distances)

distances[which.min(distances)] * 111132
distances[which.max(distances)] * 84964

# 4. View the results
p1[closest_index, ]   # Closest point to mean
p1[farthest_index, ]  # Farthest point from mean


plot(p1, asp=1)
text(p1[, 1], p1[, 2], labels = 1:10)
points(mean_point[1], mean_point[2], col='black', pch=3)
points(p1[closest_index, 1], p1[closest_index, 2], col='green', pch=19)
points(p1[farthest_index, 1], p1[farthest_index, 2], col='red', pch=19)


# normally 7
options(digits = 12)

library(terra)
# Create points
pc <- vect(matrix(c(-111.7153362, 40.27748851), ncol=2, 
                      byrow=TRUE), type="points")
pc <- vect(matrix(mean_point, ncol=2), type="points", crs='epsg:4326')

# Assign coordinate system information (WGS84)
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs"

# Create points
pp <- vect(p1, type="points")
pp2 <- vect(p1, type="points")

# Assign coordinate system information (WGS84)
# crs(pp) <- "+proj=longlat +datum=WGS84 +no_defs"
crs(pp) <- "EPSG:4326"
crs(pp2) <- 'EPSG:4269'
# projection issues
# pp2 = project(pp, "EPSG:2767") # this is projected
# pp2 = project(pp, "EPSG:4269") # NAD83
# pp2 = project(pp, "EPSG:4267") # NAD27
pp2 = project(pp, 'EPSG:6318') # NAD83 - 2011 North AMerican Fixed
pp_igs = project(pp, 'EPSG:9014') # IGS08
pp3=pp2;crs(pp3) = "EPSG:4326"

# or
pp3 = project(pp2, 'EPSG:4326')

plot(pp)
plot(pp2,col='green', add=T, pch=3)
plot(pp3,col='red', add=T, pch=3)

geom(pp)
geom(pp3)
geom(pp_igs)


# define
p1_wgs <- vect(p1, type="points", crs='epsg:4326') # WGS 84
# incorrect
p1_nad83_1 <- vect(p1, type="points", crs='epsg:4269') # NAD83 (1986) - oudated
p1_nad83_2 <- vect(p1, type="points", crs='epsg:6783') # NAD83 (CORS96) 
p1_nad83_3 <- vect(p1, type="points", crs='epsg:6318') # NAD83 (2011) - includes tectonic motion

# project to same reference frame (still incorrect)
p1_nad83_1 <- project(p1_nad83_1, 'epsg:4326')
p1_nad83_2 <- project(p1_nad83_2, 'epsg:4326')
p1_nad83_3 <- project(p1_nad83_3, 'epsg:4326')

# distance
distance(p1_wgs, p1_nad83_1, pairwise=T)
distance(p1_wgs, p1_nad83_2, pairwise=T)
distance(p1_wgs, p1_nad83_3, pairwise=T)


nad83_1986_crs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
p1n <- vect(p1, type="points", crs=nad83_1986_crs)
p1n <- project(p1n, 'epsg:4326')
distance(p1_wgs, p1n, pairwise=T)

# NO
# https://www.ngs.noaa.gov/TOOLS/Htdp/HTDP-user-guide.pdf
# nad83_2011_helmert <- '+proj=longlat +ellps=GRS80 +towgs84=1.00390,-1.90961,-0.54117,0.02678138,-0.00042027,0.01093206,-0.05109 +no_defs'
nad83_2011_helmert <- "+proj=longlat +ellps=GRS80 +towgs84=1.004,-1.910,-0.541,0.02678,-0.00042,0.01093,-0.05109 +no_defs"
p_nad83_2011 <- vect(p1, type="points", crs=nad83_2011_helmert)
p_nad83_2011 <- project(p_nad83_2011, 'EPSG:4326')
distance(p1_wgs, p_nad83_2011, pairwise=T)
# differences of 2.95

plot(p1_wgs)
plot(p1n, add=T, col='red', pch=3)


terra::crs(pp, describe=TRUE)
terra::crs(pp2, describe=TRUE)
terra::crs(pp3, describe=TRUE)


library(sf)

# Create a point in WGS84
pt_wgs84 <- st_sfc(st_point(c(-111.71452, 40.27809)), crs = 4326)

# Transform to NAD83 using PROJ pipeline
pt_nad83 <- st_transform(pt_wgs84, crs = 4269)

# Compare coordinates
st_coordinates(pt_wgs84)
st_coordinates(pt_nad83)

# Difference in meters using geodetic distance
sf::st_distance(pt_wgs84, pt_nad83)

################
ponds1 = read.csv('~/Downloads/ponds1.csv')

tolower(gsub('[[:punct:]]', '', gsub('*._(.*)','', ponds1$Title) ))
ponds1$Title = tolower(sub("^(.{2}).*$", "\\1", ponds1$Title))

pd1 = ponds1[order(ponds1$Title),]

write.csv(pd1, 'tmp-data/pond_measurements2023.csv', row.names = F)


ponds1 = read.csv('tmp-data/pond_measurements2023.csv')
head(ponds1)

aggregate(cbind(Latitude, Longitude) ~ Title, data = ponds1, FUN = mean)


library(sf)

# 1. Create point in WGS84
pt_wgs84 <- st_sfc(st_point(c(-111.71452, 40.27809)), crs = 4326)

# 2. Correctly transform to NAD83
pt_nad83 <- st_transform(pt_wgs84, 4269)

# 3. Mislabel NAD83 point as WGS84 WITHOUT transforming it
# This simulates a real-world CRS mistake!
pt_nad83_mislabeled <- st_set_crs(pt_nad83, 4326)

# 4. Compute the distance between original WGS84 and mislabeled NAD83
st_distance(pt_wgs84, pt_nad83_mislabeled)


