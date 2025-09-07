#
# Lab 3 student data
#

library(terra)

# csv RTK
df0 = read.csv('~/OneDrive - Utah Valley University/Courses/GEOG4100/4100_2025/data/Lab3/Lab3-EWPJ.csv')
head(df0)

df_sub = df0[,c('Name','Easting','Northing', 'Elevation')]
df_sub$Name2 = gsub('(^p[1-5])[A-z]','\\1',df_sub$Name)

# LIST MEAN AND SD
aggregate(cbind(Easting, Northing) ~ Name2, data = df_sub, FUN = mean)
# convert to meters
aggregate(cbind(Easting, Northing) ~ Name2, data = df_sub, FUN = sd)
# Conversion (if needed)
# Long 84928 # (84928.47)
# Lat 111320 # (111319.5)

# epsg:6318
# https://epsg.io/6318
# 6349 - NAVD height
# https://spatialreference.org/ref/epsg/6349/

# from df
base_coords <- df0[1,c('Base.easting','Base.northing')]
v_base = vect(matrix(base_coords,nrow=1, byrow=T), crs='epsg:6341')

# OPUS correction
#                         UTM COORDINATES       STATE PLANE COORDINATES
#                         UTM (Zone 12)         SPC (4302 UT C)
# Northing (Y) [meters]     4458809.238          2215869.588
# Easting (X)  [meters]      439176.501           481677.307
# Convergence  [degrees]    -0.46255000          -0.13801667
# Point Scale                0.99964554           0.99992860
# Combined Factor            0.99942886           0.99971186
# OPUS - new base
opus_base = vect(cbind(439176.501, 4458809.238), crs='epsg:6341')

# how far off?
distance(v_base, opus_base)
# 1.73 meters (1.7295)


# ALL points - vect obj
# if using CRS only
v = vect(df0, geom=c('Longitude','Latitude'), crs='epsg:6318')
# for projected data
vm = vect(df0, geom=c('Easting','Northing'), crs='epsg:6341')


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

# SAVE
# writeVector(vm, '~/Desktop/rtk_pts_2025_EWPJ.geojson', overwrite=T)
# writeVector(vm, '~/Desktop/rtk_pts_2025_EWPJ.kml', overwrite=T)

# P2
plot(v[4:6,], asp=1)



distance(v_base, opus_base)
# 1.89 meters!!!

coords_v <- geom(v_base)[, c("x", "y")]
coords_o <- geom(opus_base)[, c("x", "y")]

# Calculate the change
delta_coords <- coords_o - coords_v
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


# writeVector(vm_shifted, '~/Desktop/rtk_pts_2025_EWPJ_shift.geojson', overwrite=T)
# writeVector(vm_shifted, '~/Desktop/rtk_pts_2025_EWPJ_shift.kml', overwrite=T)
