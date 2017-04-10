library(leaflet)
library(htmlwidgets)
library(RSQLite)
#library(plotly)
library(ggplot2)
library(ggmap)

db <- dbConnect(dbDriver("SQLite"), "database.sqlite")

stations <- dbGetQuery(db, "SELECT * FROM station")
trips <- dbGetQuery(db, "
SELECT s.lat start_lat,
       s.long start_long,
       e.lat end_lat,
       e.long end_long,
       s.city scity,
       e.city ecity,
       s.name s_name,
       e.name e_name,
       COUNT(DISTINCT t.Id) num_trips
FROM trip t
INNER JOIN station s ON t.start_station_id=s.id
INNER JOIN station e ON t.end_station_id=e.id
WHERE scity = ecity
  AND t.start_station_id != t.end_station_id
GROUP BY s.lat, s.long, e.lat, e.long
ORDER BY scity, num_trips DESC")

# ------------- FUNCIONES DE GUARDADO Y RUTEADO  -------------
saveas <- function(map, file){
    class(map) <- c("saveas",class(map))
    attr(map,"filesave")=file
    map
}

print.saveas <- function(x, ...){
    class(x) = class(x)[class(x)!="saveas"]
    htmltools::save_html(x, file=attr(x,"filesave"))
}

leg <-function(sLo, sLa, dLo, dLa){
    route_all <- route(from= c(lon = as.numeric(sLo), lat = as.numeric(sLa)), to = c(lon = as.numeric(dLo), lat = as.numeric(dLa)), structure = "route")
    return (route_all)
}


# ------------- MAPAS -------------
normal_map <- "http://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png"
ugly_map <- "https://api.mapbox.com/styles/v1/kwalkertcu/cijvq6bl0006o3slwlamsjawk/tiles/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjaW9jenN1OGwwNGZsdjRrcWZnazh2OXVxIn0.QJrmnV9lJzdXHkH95ERdjw"
mb_attribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>"



mapa <- leaflet(width = 1400, height = 700) %>% addTiles(urlTemplate = normal_map, attribution = mb_attribution, group = "Comun") %>% addProviderTiles(providers$CartoDB.Positron, group = "Black and White") %>%
setView(lng = -122.403534, lat = 37.787774, zoom = 14) %>%
addLayersControl(
    baseGroups = c("Comun", "Black and White"),
    overlayGroups = c("Rutas 1", "Rutas 2", "Rutas 3", "Rutas 4", "Rutas 5"),
    options = layersControlOptions(collapsed = FALSE)
    ) %>%
addEasyButton(easyButton(
    icon="fa-crosshairs", title="San Francisco",
    onClick=JS("function(btn, map){ map.setView([37.787774, -122.403534], 14); }"))) %>%
addEasyButton(easyButton(
        icon="fa-crosshairs", title="San Jose",
        onClick=JS("function(btn, map){ map.setView([37.341113, -121.888550], 14); }"))) %>%
addEasyButton(easyButton(
        icon="fa-crosshairs", title="Palo Alto",
        onClick=JS("function(btn, map){ map.setView([37.438697, -122.156250], 14); }"))) %>%
addEasyButton(easyButton(
        icon="fa-crosshairs", title="Mountain View",
        onClick=JS("function(btn, map){ map.setView([37.397277, -122.087978], 14); }"))) %>%
addEasyButton(easyButton(
        icon="fa-crosshairs", title="Redwood City",
        onClick=JS("function(btn, map){ map.setView([37.487624, -122.221194], 14); }")))



estaciones <- do.call(rbind, stations)
estaciones <- data.frame(estaciones)                    #filas son los campos, por alguna razon

#estaciones[, 1:10]
#nrow(estaciones)
#ncol(estaciones)

for (n in 1:70){
    long <- as.character(estaciones[4,n])
    lati <- as.character(estaciones[3,n])
    nombre <- estaciones[2,n]
    mapa = mapa %>% addAwesomeMarkers(lng = as.numeric(long), lat = as.numeric(lati), popup=as.character(nombre))
}



viajes <- do.call(rbind, trips)
viajes <- data.frame(viajes)                            #filas son los campos, por alguna razon
ciudades <- viajes['scity',]

viajes_mv <- viajes[, which(ciudades=='Mountain View')]     # Divido el data frame por ciudad
viajes_pa <- viajes[, which(ciudades=='Palo Alto')]
viajes_rc <- viajes[, which(ciudades=='Redwood City')]
viajes_sf <- viajes[, which(ciudades=='San Francisco')]
viajes_sj <- viajes[, which(ciudades=='San Jose')]

#viajes_mv
#viajes_pa
#viajes_rc
#viajes_sf
#viajes_sj

viajes_x_ciudad <- list(viajes_mv, viajes_pa, viajes_rc, viajes_sf, viajes_sj)

for (m in 1:5){
    ciudad <- viajes_x_ciudad[[m]]

    for(n in 1:5){
        id <- paste("Rutas", n)
        ruta <- leg(as.character(ciudad[2,n]), as.character(ciudad[1,n]), as.character(ciudad[4,n]), as.character(ciudad[3,n]))

        mapa = mapa %>%  addPolylines(ruta$lon, ruta$lat, fill = FALSE, color = "red", weight = 7,
                          highlightOptions = highlightOptions(color = "#FFAEAE", weight = 5, bringToFront = TRUE),
                          label = paste("Viajes desde", as.character(ciudad[7,n]), "\r\n hasta", as.character(ciudad[8,n]), "=", as.character(ciudad[9,n])),
                          group = id
                          )
    }
}


mapa %>% saveWidget(file = "Leaflet.html",
                selfcontained = FALSE)



#mapa %>% saveas("index.html")
dbDisconnect(db)
