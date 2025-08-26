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

plot(x_m, y_m)


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

