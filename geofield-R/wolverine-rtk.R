#
# RTK - Brighton
# 2024-25 glacier velocity
#

library(terra)

# read in csv files
df24_full <- read.csv("~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/brighton-0830.csv")
df24 <- df24_full[36:nrow(df24),]
dfp24 <- df24_full[1:35,]
# 2025
dfw25 <- read.csv("~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/Brighton-20250907/wolv-20250907.csv")
dfp25 <- read.csv("~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/Brighton-20250907/brighton-20250907.csv")

# base station
base24 = df24[df24$Name==33,c('Base.easting', 'Base.northing')]
base25 = dfw25[dfw25$Name==33,c('Base.easting', 'Base.northing')]
# opus
opus_base = c(448876.619, 4493420.068)

# plot base locations
plot(buffer(vect(cbind(448876.619, 4493420.068), crs='epsg:6341'),2))
plot(buffer(vect(cbind(448876.619, 4493420.068), crs='epsg:6341'),1), lty=2, add=T)
plot(buffer(vect(cbind(448876.619, 4493420.068), crs='epsg:6341'),0.5), lty=3, add=T)
plot(vect(df24, geom=c('Base.easting', 'Base.northing'), crs='epsg:6341'),pch=1,add=T)
plot(vect(dfw25, geom=c('Base.easting', 'Base.northing'), crs='epsg:6341'),pch=2,add=T)
plot(vect(cbind(448876.619, 4493420.068), crs='epsg:6341'), add=T, pch=3)

# adjust 2024
delta_coords24 <- opus_base - base24
df24[, "EastingOPUS"] <- df24[, "Easting"] + as.numeric(delta_coords24['Base.easting'])
df24[, "NorthingOPUS"] <- df24[, "Northing"] + as.numeric(delta_coords24['Base.northing'])
v24shift <- vect(df24, geom=c("EastingOPUS","NorthingOPUS"), crs='epsg:6341', keepgeom=TRUE)
# original
v24 = vect(df24, geom=c('Easting','Northing'), crs='epsg:6341', keepgeom=TRUE)

# adjust 2025
delta_coords25 <- opus_base - base25
dfw25[, "EastingOPUS"] <- dfw25[, "Easting"] + as.numeric(delta_coords25['Base.easting'])
dfw25[, "NorthingOPUS"] <- dfw25[, "Northing"] + as.numeric(delta_coords25['Base.northing'])
v25shift <- vect(dfw25, geom=c("EastingOPUS","NorthingOPUS"), crs='epsg:6341', keepgeom=TRUE)
# original
v25 = vect(dfw25, geom=c('Easting','Northing'), crs='epsg:6341', keepgeom=TRUE)

# reorder
v25o <- v25shift[match(v24shift$Name, v25shift$Name),]
v24o <- v24shift[!is.na(match(v24shift$Name, v25shift$Name)),]

# distance & plots
distance(v25o, v24o, pairwise=T)
mean(distance(v25o, v24o, pairwise=T))
sd(distance(v25o, v24o, pairwise=T))
# 8 points along the centerline
# ave 2024-25 is 1.8 cm (stdev 0.5 cm)
# 

bearing_dir <- NULL
for (i in 1:nrow(v24o)){
  bearing_dir = c(bearing_dir, bearing(v24o[i,], v25o[i,]))
}
bearing(v24o[1,], v25o[1,])

ptsw = v24o
ptsw$distance = distance(v24o, v25o, pairwise=T)
ptsw$bearing = bearing_dir

# Helper function to convert bearing and distance into a line
create_arrow <- function(x, y, distance, bearing_deg) {
  # Convert bearing to radians, from degrees clockwise from North
  bearing_rad <- (90 - bearing_deg) * pi / 180
  
  # Calculate end point
  dx <- distance * cos(bearing_rad)
  dy <- distance * sin(bearing_rad)
  
  x2 <- x + dx
  y2 <- y + dy
  
  # Return a 2-point line geometry
  return(rbind(c(x, y), c(x2, y2)))
}

# Get coordinates of points
coords <- crds(ptsw)

# Extract distance and bearing
dists <- ptsw$distance
bearings <- ptsw$bearing

# Create list of line geometries
lines_list <- vector("list", length(dists))
for (i in seq_along(dists)) {
  geom <- create_arrow(coords[i, 1], coords[i, 2], dists[i], bearings[i])
  lines_list[[i]] <- vect(geom, type = "lines", crs = crs(ptsw))
}

# Combine into a single SpatVector
arrows <- do.call(rbind, lines_list)

# plot point 1 bearing
plot(ptsw[1,], col = "blue", pch = 16, main = "Arrows Showing Distance and Bearing")
lines(arrows[1,], col = "red", lwd = 2)

text(pts, labels = pts$ID, pos = 3, cex = 0.8)  # If there's an ID field



# # # # # # #

library(sf)
library(ggplot2)
library(dplyr)

# Convert points to sf
pts_sf <- st_as_sf(ptsw)

# Compute arrow end points
bearing_rad <- (90 - pts_sf$bearing) * pi / 180
pts_sf <- pts_sf %>%
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2],
    dx = distance * cos(bearing_rad),
    dy = distance * sin(bearing_rad),
    xend = x + dx,
    yend = y + dy
  )

# plot point 1 bearing
ggplot() +
  geom_sf(data = pts_sf[1,], color = "blue", size = 2) +
  geom_segment(data = pts_sf[1,],
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "grey", linewidth = 1) +
  theme_minimal() +
  labs(title = "Direction Arrows from Points")

# # # # # # # 



#### Protalus Rampart
p24 = vect(dfp24, geom=c('Easting','Northing'), crs='epsg:6341', keepgeom=TRUE)
p25 = vect(dfp25, geom=c('Easting','Northing'), crs='epsg:6341', keepgeom=TRUE)

plot(p24, pch=1)
text(p24, cex=1)
points(p25, cex=0.5, col='red')
# Save

# remove
# 30, 27, 23, 22, 20, 15, 10, 9, 7, 6, 5, 4, 3

p24o = p24[!p24$Name%in%c(3:7,9:10,15,20,22:23,27:28,30),]
# reorder
p25o <- p25[match(p24o$Name, p25$Name),]
p24o <- p24o[!is.na(match(p24o$Name, p25$Name)),]

##
distance(p24o, p25o, pairwise=T)
mean(distance(p24o, p25o, pairwise=T))
sd(distance(p24o, p25o, pairwise=T))
# 17 points
# mean 1.7 cm (stdev 1.5 cm)

# writeVector(v24o, '~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/wolverine-rg-20240830.geojson', overwrite=T)
# writeVector(v25o, '~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/wolverine-rg-20250907.geojson', overwrite=T)
# 
# writeVector(p24o, '~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/wolverine-pr-20240830.geojson', overwrite=T)
# writeVector(p25o, '~/OneDrive - Utah Valley University/Projects/2-glacier-projects/rtk-wasatch/Wolverine-cirque/wolverine-pr-20250907.geojson', overwrite=T)
# 


# # # # # # # 
# # # # # # # 


# leaflet
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addCircles(data = project(v25o,'epsg:4326'), radius= 0.01, color='black') %>% 
  addCircles(data = project(v24o,'epsg:4326'), radius= 0.01, color='red')
