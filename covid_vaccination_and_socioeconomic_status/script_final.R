
library(tidyverse)
library(dplyr)
library(janitor)
library(primer.data)
library(readxl)
library(lubridate)
library(ggthemes)
library(patchwork)
library(ggplot2)
library(hrbrthemes)
library(ggdist)
library(shinythemes)
library(tidycensus)
library(maps)
library(geojsonio)
library(ggplot2)
library(mapproj)
library(leaflet)
library(RColorBrewer)


# CODE FOR MAP : Vax rates 
vax <- read_csv("final_data/data_map.csv")
mybins <- seq(0, 100, by=20)
mypalette <- colorBin( palette="Spectral", domain=vax$vax, na.color="transparent", bins=mybins)

mytext <- paste(
  "City: ", vax$city2, "<br/>", 
  "Percent of Vaccinated: ", vax$vax_rate, sep="") %>%
  lapply(htmltools::HTML)

  # Final Map
vax_map <- leaflet(vax) %>% 
    addTiles()  %>% 
    setView( lat= 32.8, lng=35 , zoom=6.42) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addCircleMarkers(~long, ~lat, 
                     fillColor = ~mypalette(vax_rate), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                     label = mytext,
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    ) %>%
    addLegend( pal=mypalette, values=~vax_rate, opacity=0.9, title = "Percent Vaccinated", position = "bottomright" )
  
vax_map
# radius=~vax_rate/5


# CODE FOR MAP ACTIVE CASES
palette_rev <- rev(brewer.pal(6, "RdYlGn"))
mybins2 <- seq(0, 12, by=2)
mypalette2 <- colorBin(palette=palette_rev, domain=vax$vax, na.color="transparent", bins=mybins2)

mytext2 <- paste(
  "City: ", vax$city2, "<br/>", 
  "Active Cases per 10000 People: ", vax$active, sep="") %>%
  lapply(htmltools::HTML)

# Final Map
active_map <- leaflet(vax) %>% 
  addTiles()  %>% 
  setView(lat= 32.8, lng=35 , zoom=6.42) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = ~mypalette2(active), fillOpacity = 0.7, color="white", radius=~active*4, stroke=FALSE,
                   label = mytext2,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette2, values=~active, opacity=0.9, title = "Active Cases per 10000", position = "bottomright" )
active_map


#______________________________________________________________________________________

