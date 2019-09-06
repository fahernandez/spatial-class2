library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tmap)

st_layers(here("foss4g_R_workshop.gpkg"))

cycle_hire_27700 <- st_read(
  dsn = here("foss4g_R_workshop.gpkg"), 
  layer = "cycle_hire_27700")

london_boroughs_27700 <- st_read(
  dsn = here("foss4g_R_workshop.gpkg"), 
  layer = "london_boroughs_27700")

boroughs_centroids_27700<- st_read(
  dsn = here("foss4g_R_workshop.gpkg"), 
  layer = "boroughs_centroids_27700")

london_boroughs_27700 %>% # pipe data to
  ggplot() +                # a ggplot function
  geom_sf(                # precise that it will be a spatial geometry
    aes(                  # provide some aesthetics
      geometry = geom,    # the geometry column (usually auto detected)
      fill = count)       # we want the polygon color to change following the count. Rellenar cada geometria con polygonos
  ) -> g # store it in g

g # display g

g <- g +
  scale_fill_viridis_c(
    guide = guide_legend(title = "Hires") # legend title
  )
g

g <- g + 
  theme_bw() + 
  ggtitle("Cycle hire points", subtitle = "in London's boroughs")
g

# Checkear si un punto esta en el rio o en el espacio vacio

ggplot() +  geom_sf(data = london_boroughs_27700) + # add boroughs shape to the map  
  geom_sf(data = boroughs_centroids_27700, # add the boroughs centroids>
          aes(size = boroughs_centroids_27700$count), # fix size of points (by area)
          color = 'red', alpha = 1/5)+ # set points colour and transparency
  ggtitle("Cycle hire points", subtitle = "in London's boroughs") + # set the map title
  theme(legend.position = 'left') + # Legend position
  scale_size_area(name = 'Hires',max_size=10) # 0 value means 0 area + legend title


tmap_mode("view")
tm_shape(london_boroughs_27700) + 
  tm_polygons("count")


tmap_mode("plot")

tm_shape(london_boroughs_27700) + 
  tm_polygons("count", convert2density = TRUE) +
  tm_bubbles(size = "count", col = "red") +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(size = 2, position=c("right", "top"))


tmap_mode("view")
tm_basemap("Stamen.Watercolor") +
  tm_shape(london_boroughs_27700) + tm_polygons("count", convert2density = TRUE) + tm_bubbles(size = "count", col = "red") +
  tm_tiles("Stamen.TonerLabels")

