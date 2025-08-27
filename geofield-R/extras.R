# EXTRAS


Spatial data can be visualized by plotting with the same base `plot` functions.

Optional Mapping with `leaflet` can make things look quite a bit cooler.

Install and load the `leaflet` package:
  
  ```r
install.packages("leaflet")
library(leaflet)
```

Plot points interactively:
  
  ```r
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = c(-111.715283, -111.715451), lat = c(40.27754852, 40.2775552), popup = c("Point 1", "Point 2"))
```

