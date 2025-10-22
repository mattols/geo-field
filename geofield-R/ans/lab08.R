#
# GEOG 4100
# Lab 08 Answers
# 2025
#

# 1 FOV at 20, 50, 100, 250
# Load necessary library
library(dplyr)

# Drone specifications
drones <- data.frame(
  Drone = c("Phantom 4 Pro", "Mavic Air 2", "Mini 4 Pro", "Mavic 3 Enterprise"),
  Sensor_Width_mm = c(13.2, 6.3, 6.3, 13.2),
  Sensor_Height_mm = c(8.8, 4.7, 4.7, 8.8),
  Focal_Length_mm = c(8.8, 4.5, 3.5, 24),
  Image_Width_px = c(5472, 8000, 8064, 5280),
  Image_Height_px = c(3648, 6000, 6048, 3956)
)

# Altitudes to evaluate (in meters)
altitudes <- c(20, 50, 100, 250)

# FOV function (in radians)
calc_fov_rad <- function(sensor_mm, focal_mm) {
  2 * atan(sensor_mm / (2 * focal_mm))
}

# Ground FOV in meters
calc_ground_fov <- function(fov_rad, height_m) {
  2 * height_m * tan(fov_rad / 2)
}

# GSD in meters per pixel
calc_gsd <- function(ground_fov_m, pixels) {
  ground_fov_m / pixels
}

# Store results
results <- data.frame()

# Loop over drones and altitudes
for (i in 1:nrow(drones)) {
  drone <- drones[i, ]
  
  h_fov_rad <- calc_fov_rad(drone$Sensor_Width_mm, drone$Focal_Length_mm)
  v_fov_rad <- calc_fov_rad(drone$Sensor_Height_mm, drone$Focal_Length_mm)
  
  for (h in altitudes) {
    h_fov_ground <- calc_ground_fov(h_fov_rad, h)
    v_fov_ground <- calc_ground_fov(v_fov_rad, h)
    
    gsd_width <- calc_gsd(h_fov_ground, drone$Image_Width_px)
    gsd_height <- calc_gsd(v_fov_ground, drone$Image_Height_px)
    
    results <- rbind(results, data.frame(
      Drone = drone$Drone,
      Altitude_m = h,
      FOV_Width_m = round(h_fov_ground, 2),
      FOV_Height_m = round(v_fov_ground, 2),
      GSD_Width_cm = round(gsd_width * 100, 2),
      GSD_Height_cm = round(gsd_height * 100, 2)
    ))
  }
}

# View the results
print(results)

#         Drone         Altitude_m FOV_Width_m FOV_Height_m GSD_Width_cm GSD_Height_cm
# 1       Phantom 4 Pro         20        30.0        20.00         0.55          0.55
# 2       Phantom 4 Pro         50        75.0        50.00         1.37          1.37
# 3       Phantom 4 Pro        100       150.0       100.00         2.74          2.74
# 4       Phantom 4 Pro        250       375.0       250.00         6.85          6.85
# 5         Mavic Air 2         20        28.0        20.89         0.35          0.35
# 6         Mavic Air 2         50        70.0        52.22         0.88          0.87
# 7         Mavic Air 2        100       140.0       104.44         1.75          1.74
# 8         Mavic Air 2        250       350.0       261.11         4.38          4.35
# 9          Mini 4 Pro         20        36.0        26.86         0.45          0.44
# 10         Mini 4 Pro         50        90.0        67.14         1.12          1.11
# 11         Mini 4 Pro        100       180.0       134.29         2.23          2.22
# 12         Mini 4 Pro        250       450.0       335.71         5.58          5.55
# 13 Mavic 3 Enterprise         20        11.0         7.33         0.21          0.19
# 14 Mavic 3 Enterprise         50        27.5        18.33         0.52          0.46
# 15 Mavic 3 Enterprise        100        55.0        36.67         1.04          0.93
# 16 Mavic 3 Enterprise        250       137.5        91.67         2.60          2.32







# [RESULTS]
# Table from above???
#   
#   **Answers:**
#   
# 1. Above
# 2. Above
# 3. Higher altitudes decreases GSD and spatial resolution.
# 4. DJI Mini 4 Pro - Shorter focal length yields a higher footprint
# 5. 23.7
# 6. 6 m/s
# 7. 1.65 sec
# 8. 30 meters
# 9. 66.76 meters
# 10. 114 images (6 across and 19 along) (side space is 42m front space is 24m)
