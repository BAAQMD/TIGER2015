use_geodata <- function (geodata, overwrite = TRUE) {

  (id_var <- select_vars(names(geodata), matches("GEOID")))
  (area_vars <- select_vars(names(geodata), matches("ALAND"), matches("AWATER")))

  keep_vars <- union(id_var, area_vars)
  tidied_geodata <- geodata[, keep_vars]
  row.names(tidied_geodata) <- geodata[[id_var]]
  tidied_geodata <- spChFIDs(tidied_geodata, row.names(tidied_geodata))

  obj_name <- deparse(substitute(geodata))
  file_name <- here::here("data", paste0(obj_name, ".rda"))
  obj_list <- setNames(list(tidied_geodata), obj_name)

  assign(obj_name, tidied_geodata)
  save(list = obj_name, file = file_name)

}
