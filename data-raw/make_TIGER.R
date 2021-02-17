library(tigris)

options(tigris_use_cache = TRUE)
options(tigris_year = 2015)

make_TIGER_CA_CBSAs <- function () {

  simplified_TIGER_CBSAs <-
    tigris::combined_statistical_areas(cb = TRUE)

  CA_CBSA_NAMES <-
    simplified_TIGER_CBSAs[["NAME"]] %>%
    keep(~ str_detect(., pattern = "CA"))

  detailed_CBSAs <-
    tigris::combined_statistical_areas(cb = FALSE)

  TIGER2015_CA_CBSAs <-
    detailed_CBSAs %>%
    subset(.$NAME %in% CA_CBSA_NAMES)

  return(TIGER2015_CA_CBSAs)

}

make_TIGER_CA_counties <- function () {

  TIGER2015_CA_county_polygons <-
    tigris::counties(state = "CA")

  return(TIGER2015_CA_counties)

}

make_TIGER_SFBA_counties <- function (TIGER2015_CA_counties) {

  TIGER2015_SFBA_counties <-
    TIGER2015_CA_counties %>%
    subset(.$COUNTYFP %in% SFBA_COUNTY_FIPS_CODES)

  return(TIGER2015_SFBA_counties)

}


make_TIGER_CA_tracts <- function () {

  TIGER2015_CA_tracts <-
    tigris::tracts(state = "CA")

  return(TIGER2015_CA_tracts)

}

make_TIGER_SFBA_tracts <- function (TIGER2015_CA_tracts) {

  TIGER2015_SFBA_tracts <-
    TIGER2015_CA_tracts %>%
    subset(.$COUNTYFP %in% SFBA_COUNTY_FIPS_CODES)

  return(TIGER2015_SFBA_tracts)

}

make_TIGER_SFBA_blkgrps <- function () {

  TIGER2015_SFBA_blkgrps <-
    tigris::block_groups(state = "06", county = SFBA_COUNTY_FIPS_CODES)

  n_features <- nrow(TIGER2015_SFBA_blkgrps)
  message(n_features, " features in TIGER2015_SFBA_blkgrps")
  stopifnot(n_features > 0)

  return(TIGER2015_SFBA_blkgrps)

}


make_TIGER_SFBA_blocks <- function () {

  TIGER2015_SFBA_blocks <-
    tigris::blocks(state = "06", county = SFBA_COUNTY_FIPS_CODES)

  n_features <- nrow(TIGER2015_SFBA_blocks)
  message(n_features, " features in TIGER2015_SFBA_blocks")
  stopifnot(n_features > 0)

  return(TIGER2015_SFBA_blocks)

}
