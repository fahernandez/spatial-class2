library(dplyr)
library(sf) # upgraded version of sp
library(spData)
library(here)

# read files
list.files(system.file("shapes", package = "spData"))
cycle_hire <- st_read(system.file("shapes/cycle_hire.geojson", package="spData"))
str(cycle_hire)

cycle_hire <- cycle_hire %>% 
  mutate(slots = nbikes +  nempty)

data(lnd) # load the dataset in memory
lnd

# 
cycle_hire_27700 <- cycle_hire %>%
  st_transform(crs = st_crs(27700)) # Codigo de la proyeccion oficial de Gran Bretaña
# Todas las columnas geometricas son automaticamente transformadas

london_27700 <- lnd %>%
  st_transform(crs = st_crs(27700)) # Codigo de la proyeccion oficial de Gran Bretaña

plot(london_27700$geometry)
plot(cycle_hire_27700$geometry, 
     col = "red",  # color
     cex = 0.5,    # size of symbol
     add = TRUE)

# Plot no respeta solo el bbox
cycle_hire_27700 %>% inner_join(
  london_27700 %>%
    st_drop_geometry(), # we don't need the geometry here
  by = c( "area" = "NAME")
)

# Usa las columnas con geometrias para hacer join
# Solo se puede usar una geometria por fila
# Las proyecciones deben ser las mismas
cycle_hire_27700 <- cycle_hire_27700 %>% st_join(london_27700 %>% select(GSS_CODE))

cycle_hire_27700 %>% filter(is.na(GSS_CODE))

# Resumiendo datos
cycle_hire_by_area <- cycle_hire_27700 %>%
  filter(!is.na(GSS_CODE)) %>% # remove NAs
  st_drop_geometry() %>% # let's put geometry aside
  group_by(GSS_CODE) %>%  # group data by GSS_CODE
  tally(name = "count", sort= TRUE) # Aggregate, similar a count
cycle_hire_by_area

# Podria haber mas de un GSS_CODE por polygono

cycle_hire_by_area_sum <- cycle_hire_27700 %>%
  filter(!is.na(GSS_CODE)) %>% # remove NAs
  st_drop_geometry() %>% # let's put geometry aside
  group_by(GSS_CODE) %>%  # group data by GSS_CODE
  summarise(sum = sum(nbikes), count = n()) # Aggregate
cycle_hire_by_area_sum

aggregate(cycle_hire_27700["nbikes"], by = list(cycle_hire_27700$"GSS_CODE"),
          FUN = sum, na.rm = TRUE)

boroughs_centroids <- london_27700 %>%
  select(NAME, GSS_CODE) %>% # only keep useful columns
  st_centroid()

# Cuales estan contenidos en cycle hire
london_27700 %>% 
  filter(NAME == "Wandsworth") %>% 
  st_contains(cycle_hire_27700)

cycle_hire_27700 %>% filter(id == "614") %>% 
  st_within(london_27700) # borough at index 22


london_27700[unlist(cycle_hire_27700 %>% filter(id == "614") %>% st_within(london_27700)),]

# extension de geopackage
london_27700 %>% left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "london_boroughs_27700", 
    layer_options = "OVERWRITE=true")

boroughs_centroids %>%
  left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "boroughs_centroids_27700", 
    layer_options = "OVERWRITE=true")

cycle_hire_27700 %>%
  left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "cycle_hire_27700",
    layer_options = "OVERWRITE=true")

list.files(here())

st_layers(dsn = here("foss4g_R_workshop.gpkg"))
