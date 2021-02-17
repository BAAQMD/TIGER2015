#' Convert spatial data to TopoJSON
#'
#' @details
#' Relies on Node: `geo2topo`
#'
#' @examples
#' library(sf)
#' library(leaflet)
#' library(TIGER2015)
#' CA_counties_sf <- st_as_sf(TIGER2015::TIGER2015_CA_counties)
#' topojson_str <- geo2topo_str(CA_counties_sf)
#' leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addTopoJSON(topojson_str)
#'
#' @export
geo2topo_str<- function (geodata, layer, ..., verbose = getOption("verbose")) {

  msg <- function (...) if (isTRUE(verbose)) message("[geo2topo_str] ", ...)

  geojson_file <- geo2json_file(geodata, layer, overwrite = TRUE, ..., verbose = verbose)
  msg("importing and converting ", basename(geojson_file))

  geo2topo_bin <- "/Users/dholstius/.node_modules_global/bin/geo2topo" # FIXME: hardcoded
  msg("invoking ", geo2topo_bin)
  topojson_str <- system2(geo2topo_bin, args = geojson_file, stdout = TRUE)

  return(topojson_str)

}

geo2json_file <- function (geodata, layer, overwrite = TRUE, ..., verbose = getOption("verbose")) {

  msg <- function (...) if (isTRUE(verbose)) message("[geo2json_file] ", ...)

  if (missing(layer)) {
    layer <- deparse(substitute(geodata))
  }

  if (inherits(geodata, "Spatial")) {
    geodata <- st_as_sf(geodata) # FIXME: lazy, shouldn't modify a variable
  }

  msg("(re)projecting to EPSG 4326")
  geodata_lnglat <- st_transform(geodata, st_crs(4326))

  tmpfn <- tempfile(fileext = ".geojson")
  msg("writing layer ", layer, " to ", basename(tmpfn))

  write_geojson <- function (geodata, path, ...) st_write(geodata, dsn = path, driver = "GeoJSON", ...)
  write_geojson(geodata_lnglat, tmpfn, quiet = TRUE)

  return(tmpfn)

}

