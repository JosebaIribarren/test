
# Joseba Iribarren

# Julio 2024

# Script de modelo de generación de caminos para Chile desde la region de Valparaiso a 
# la region de Los Lagos. Separando el proceso por region.

# Si se desea guardar pasos intermedios hay que descomentar todas las partes que dice "guardar".



# 0) Paquetes
require(pacman)
pacman::p_load(tidyverse,
               terra,
               sf,
               roads,
               raster)

# poner la carpeta del script como working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# 1 Cargar el archivos ----

# COSTOS
costRaster <- raster("input/Costos_aoi.tif")

# Caminos
redvial <- st_read("input/redvial19.shp")

# Landings
landings <- st_read("input/Puntos_prueba.shp")

# Regiones
regiones <- st_read("input/regiones.shp")


# 2 Trasformar datos ----

# trasformar el tipo de raster
costRaster <- as(costRaster, "SpatRaster")

# Use setMinMax to update the values
costRaster <- setMinMax(costRaster)


# transformacion capas vectoriales
redvial <- st_cast(redvial, "LINESTRING")
landings <- st_cast(landings, "POINT")

# ver las capas
# plot(costRaster, col = gray.colors(256))
# plot(st_geometry(redvial), col = 'green', add=T)
# plot(landings$geometry, col = 'red', add=T)




# 3 Loop para separar los datos por region
dir.create("rasters")
dir.create("caminos")
dir.create("landings")

max <- global(costRaster, fun = "max", na.rm = TRUE) %>% dplyr::select(max) %>% as.double()

rasters_list <- list()
caminos_list <- list()
landings_list <- list()
for (i in 1:nrow(regiones)) {

  poly <- regiones[i, ]
  
  # RASTER DE COSTOS
  raster_cortado <- terra::crop(costRaster, poly)

  raster_cortado <- mask(raster_cortado, poly)
  
  raster_cortado <- terra::extend(raster_cortado, c(15, 15))
  raster_cortado[is.na(raster_cortado)] <- max
  raster_cortado[raster_cortado == 0] <- max

  rasters_list[[i]] <- raster_cortado

  # # Guardar
  # writeRaster(rasters_list[[i]], paste0("rasters/Costos_", poly[[2]], ".tif"), overwrite=TRUE)

  # CAMINOS
  camino_cortado <- sf::st_intersection(redvial, poly)

  caminos_list[[i]] <- st_cast(camino_cortado, "LINESTRING")

  # # Guardar
  # st_write(st_geometry(caminos_list[[i]]), paste0("caminos/camino_", poly[[2]], ".shp"), append=FALSE)

  # LANDINGS
  landing_cortado <- sf::st_intersection(landings, poly)

  landings_list[[i]] <- st_cast(landing_cortado, "POINT")

  # # Guardar
  # st_write(st_geometry(landings_list[[i]]), paste0("landings/landings_", poly[[2]], ".shp"), append=FALSE)
  Sys.sleep(2)
}

rm(costRaster, redvial, landings, regiones,
   camino_cortado, raster_cortado, landing_cortado, poly)


landings_list %>% length()
rasters_list %>% length()
caminos_list %>% length()

p <- 9
plot(rasters_list[[p]])
plot(st_geometry(landings_list[[p]]), add = T)
plot(st_geometry(caminos_list[[p]]), add = T)


# 4 Loop de generación de caminos. Metodo ilcp ----
dir.create("output")

output_caminos_list <- list()
for (i in 1:length(rasters_list)){
  projRoads_ilcp <- roads::projectRoads(landings = landings_list[[i]],
                                        weightRaster = rasters_list[[i]], 
                                        roads = caminos_list[[i]], 
                                        roadMethod = 'ilcp',
                                        roadsInWeight = F)
  
  output_caminos_list[[i]] <- st_geometry(projRoads_dlcp$roads)
  
  # # Guardar
  # st_write(output_caminos[[i]], paste0("output/output_camino",regiones[i, ][[2]], ".shp", append=FALSE))
  Sys.sleep(10)
}


# 5 combinar y exportar output

combined_output_caminos <- c(st_geometry(output_caminos_list[[1]]),
                             st_geometry(output_caminos_list[[2]]),
                             st_geometry(output_caminos_list[[3]]),
                             st_geometry(output_caminos_list[[4]]),
                             st_geometry(output_caminos_list[[5]]),
                             st_geometry(output_caminos_list[[6]]),
                             st_geometry(output_caminos_list[[7]]),
                             st_geometry(output_caminos_list[[8]]),
                             st_geometry(output_caminos_list[[9]]))

combined_output_caminos <- st_as_sf(st_sfc(combined_output_caminos))

st_write(combined_output_caminos, "output/output_camino.shp", append=FALSE)



