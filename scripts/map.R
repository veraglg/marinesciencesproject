library(leaflet)

samples <- data.frame(
  lat = c(37.8320, 36.5479),
  lng = c(-5.0369, -4.6152),
  label = c("La Breña Reservoir", "Los Boliches Beach")
)

pal <- colorFactor(palette = "Accent", domain = samples$label)

samples |> leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
  addProviderTiles("Esri.WorldImagery") |> 
  addCircleMarkers(~lng, ~lat, color = ~pal(label), label = ~label, opacity = 1) |> 
  setView(lat = 37, lng = -5, zoom = 6) |>
  addLegend(position = "bottomright", title = "",
            pal = pal, values = ~label, opacity = 1) |>
  addMiniMap()
