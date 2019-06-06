### CREATE DATASET ###

# Define number of datapoints
n = 23

# Pick random numbers for location value
set.seed(n)
df = data.frame(x = -87.600730 + rnorm(n)/100,
                y =  41.791076 + rnorm(n)/100)

# Add columns of random values to the data
df$a = runif(n,min = -2, max = 2)
df$b = runif(n,min = -2, max = 2)




### CREATE MAP ###

# Install and initiate leaflet package
# install.packages("leaflet")
library(leaflet)

# Create base map without formatting
leaflet() %>%
  addTiles() %>%  # Add a basemap - default is OpenStreetMap
  addMarkers(lng = df$x,lat = df$y)


# Create base map without formatting
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = df$x,lat = df$y)


# Change dot size and fill
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = df$x,
                   lat = df$y,
                   radius = 10,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.8)


# Define function to color markers based on data value
pal = colorNumeric("RdGy", -2:2)

# Color markers based on value of a
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = df$x,
                   lat = df$y,
                   radius = 10,
                   color = pal(df$a),
                   stroke = TRUE,
                   fillOpacity = 0.8)


# Color markers based on value of b
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = df$x,
                   lat = df$y,
                   radius = 10,
                   color = pal(df$b),
                   stroke = TRUE,
                   fillOpacity = 0.8)