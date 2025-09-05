

library(terra)

# csv RTK
df0 = read.csv('~/OneDrive - Utah Valley University/Courses/GEOG4100/4100_2025/data/olson-ponds-rtk/Ols0902/olson-ponds09022025.csv')
head(df0)

df_sub = df0[,c('Name','Easting','Northing', 'Elevation')]
df_sub$Name2 = gsub('(^.*)-.*','\\1',df_sub$Name)
aggregate(cbind(Easting, Northing) ~ Name2, data = df_sub, FUN = mean)
aggregate(cbind(Easting, Northing) ~ Name2, data = df_sub, FUN = sd)

# epsg:6318
# https://epsg.io/6318
# 6349 - NAVD height
# https://spatialreference.org/ref/epsg/6349/

# from df
v_base = vect(cbind(439178.4, 4458812), crs='epsg:6341')
# OPUS - new base
opus_base = vect(cbind(439180.244, 4458812.428), crs='epsg:6341')

# vect obj
# v = vect(df0, geom=c('Longitude','Latitude'), crs='epsg:6341')
v = vect(df0, geom=c('Longitude','Latitude'), crs='epsg:6318')

vm = vect(df0, geom=c('Easting','Northing'), crs='epsg:6341')

plot(v)

library(leaflet)
leaflet() %>%
  addTiles() %>%
  # cannot zoom in further
  # setView(lng = mean(df0$Longitude),lat = mean(df0$Latitude), zoom=25) %>% 
  addMarkers(data = v)

# plot meters
plot(ext(buffer(vm, 20)))
plot(vm, 'Name', add=T)
plot(v_base, add=T, pch=3,col='red')
plot(opus_base, add=T, pch=3,col='black')



# writeVector(v, '~/Desktop/rtk_pts_0902.geojson', overwrite=T)
# writeVector(vm, '~/Desktop/rtk_pts_0902_meters.geojson')

# P2
plot(v[4:6,], asp=1)


# OPUS coords
# LAT:   40 16 39.29837       0.004(m)        40 16 39.31187      0.004(m)
# E LON:  248 17  4.51118      0.004(m)       248 17  4.45006      0.004(m)
# W LON:  111 42 55.48882      0.004(m)       111 42 55.54994      0.004(m)
# EL HGT:     1382.045(m)     0.029(m)              1381.318(m)   0.029(m)
# ORTHO HGT:  1399.240(m)     0.038(m) [NAVD88 (Computed using GEOID18)]
# 
# UTM COORDINATES                               STATE PLANE COORDINATES
# UTM (Zone 12)                                 SPC (4302 UT C)
# Northing (Y) [meters]     4458812.428          2215872.800
# Easting (X)  [meters]      439180.244           481681.033
# Convergence  [degrees]    -0.46252222          -0.13798889
# Point Scale                0.99964553           0.99992860
# Combined Factor            0.99942885           0.99971186


distance(v_base, opus_base)
# 1.89 meters!!!

coords_v <- geom(v_base)[, c("x", "y")]
coords_o <- geom(opus_base)[, c("x", "y")]

# Calculate the change
delta_coords <- coords_o - coords_v

# Rename columns
colnames(delta_coords) <- c("delta_x", "delta_y")

# View result
delta_coords



# CREATE NEW
g_vm <- geom(vm)

# Apply delta (assumes same order and number of rows)
g_vm[, "x"] <- g_vm[, "x"] + delta_coords["x"]
g_vm[, "y"] <- g_vm[, "y"] + delta_coords["y"]

# Rebuild new SpatVector from updated geometry
vm_shifted <- vect(g_vm, type = "points", crs = crs(vm))

# If vm had attributes, you can copy them
values(vm_shifted) <- values(vm)

# Plot to verify
plot(vm, col = "red", pch = 16)
points(vm_shifted, col = "blue", pch = 16)
legend("topright", legend = c("Original", "Shifted"), col = c("red", "blue"), pch = 16)


# writeVector(vm_shifted, '~/Desktop/rtk_pts_0902_m_shift.geojson', overwrite=T)

