library(dplyr)
library(leaflet)
# iconUrl = "D:/VIDEOS_CANAL/HS/usuarios_qgis/data/qgisbr.png",
#iconWidth = 30, iconHeight = 30
oceanIcons <- iconList(
  ship = makeIcon(iconUrl = "Icons/leaf-green.png",
                  iconWidth = 18, iconHeight = 18),
  pirate = makeIcon(iconUrl = "Icons/leaf-red.png",
                    iconWidth = 24, iconHeight = 24)
)

# Some fake data
df <- sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
  ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
  ))
)
leaflet(df) %>% addTiles() %>%
  # Select from oceanIcons based on df$type
  addMarkers(icon = ~oceanIcons[type])


quakes1 <- quakes[1:10,]

leafIcons <- icons(
  iconUrl = ifelse(quakes1$mag < 4.6,
                   "Icons/leaf-green.png",
                   "Icons/leaf-red.png"
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "Icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes1) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons)
