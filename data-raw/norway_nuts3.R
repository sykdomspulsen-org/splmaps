library(data.table)

gen_norway_nuts3_position_geolabels <- function(x_year_end, insert = F) {
  location_code <- NULL
  long <- NULL
  lat <- NULL
  stopifnot(x_year_end %in% c("2017", "2019", "2020"))

  if (x_year_end == 2017) {
    label_positions <- data.table(
      location_code = c(
        "county01", "county02", "county03", "county04",
        "county05", "county06", "county07", "county08",
        "county09", "county10", "county11", "county12",
        "county14", "county15", "county16", "county17",
        "county18", "county19", "county20"
      ),
      long = c(
        11.266137, 11.2, 10.72028, 11.5,
        9.248258, 9.3, 10.0, 8.496352,
        8.45, 7.2, 6.1, 6.5,
        6.415354, 7.8, 9.650999, 11.546003,
        14.8, 19.244275, 24.7
      ),

      lat = c(
        59.33375, 60.03851, 59.98, 61.26886,
        61.25501, 60.3, 59.32481, 59.47989,
        58.6, 58.4, 58.7, 60.25533,
        61.6, 62.5, 63.43384, 67.29402,
        66.5, 68.9, 69.6
      )
    )
  } else if (x_year_end == 2019) {
    label_positions <- data.table(
      location_code = c(
        "county01", "county02", "county03", "county04",
        "county05", "county06", "county07", "county08",
        "county09", "county10", "county11", "county12",
        "county14", "county15", "county18", "county19",
        "county20", "county50"
      ),
      long = c(
        11.266137, 11.2, 10.72028, 11.5, 9.248258, 9.3, 10.0, 8.496352,
        8.45, 7.2, 6.1, 6.5, 6.415354, 7.8, 14.8, 19.244275, 24.7, 11
      ),

      lat = c(
        59.33375, 60.03851, 59.98, 61.26886, 61.25501, 60.3, 59.32481, 59.47989,
        58.6, 58.4, 58.7, 60.25533, 61.6, 62.5, 66.5, 68.9, 69.6, 63
      )
    )
  } else if (x_year_end == 2020) {
    label_positions <- rbindlist(list(
      data.table(
        location_code = "county30",
        long = 8.85,
        lat = 60.60
      ),
      data.table(
        location_code = "county03",
        long = 10.72028,
        lat = 59.98
      ),
      data.table(
        location_code = "county34",
        long = 11.0,
        lat = 61.86886
      ),
      data.table(
        location_code = "county38",
        long = 8.5,
        lat = 59.32481
      ),
      data.table(
        location_code = "county42",
        long = 7.8,
        lat = 58.3
      ),
      data.table(
        location_code = "county11",
        long = 6.1,
        lat = 58.7
      ),
      data.table(
        location_code = "county46",
        long = 6.5,
        lat = 61.45
      ),
      data.table(
        location_code = "county15",
        long = 7.8,
        lat = 62.5
      ),
      data.table(
        location_code = "county18",
        long = 14.8,
        lat = 66.75
      ),
      data.table(
        location_code = "county54",
        long = 22.94275,
        lat = 69.5
      ),
      data.table(
        location_code = "county50",
        long = 12.0,
        lat = 64.15
      )
    ))
    # fix oslo insert if desired
    if(insert){
      label_positions[location_code=="county03", long := 20.85]
      label_positions[location_code=="county03", lat := 62]
    }
  }

  return(label_positions)
}

# insert for oslo/akershus?
# split the country in 2?
gen_norway_nuts3_map <- function(x_year_end, insert = FALSE, split = FALSE, return_sf=FALSE) {
  stopifnot(x_year_end %in% c("2017", "2019", "2020"))

  . <- NULL
  id <- NULL
  location_code <- NULL
  long <- NULL
  lat <- NULL
  long_center <- NULL
  lat_center <- NULL
  group <- NULL
  hole <- NULL
  piece <- NULL
  long_diff <- NULL
  lat_diff <- NULL

  if (x_year_end == 2017) {
    spdf <- geojsonio::geojson_read(
      fs::path("data-raw", "files", "Fylker17.geojson"),
      what = "sp"
    )
    spdf_simple <- rgeos::gSimplify(spdf, tol=2000, topologyPreserve = F)
    # pryr::object_size(spdf_simple)
  } else if (x_year_end == 2019) {
    spdf <- geojsonio::geojson_read(
      fs::path("data-raw", "files", "Fylker19.geojson"),
      what = "sp"
    )
    spdf_simple <- rgeos::gSimplify(spdf, tol=2000, topologyPreserve = F)
  } else if (x_year_end == 2020) {
    spdf <- geojsonio::geojson_read(
      fs::path("data-raw", "files", "Fylker20.geojson"),
      what = "sp"
    )
    spdf$navn <- NULL
    spdf_simple <- rgeos::gSimplify(spdf, tol=2000, topologyPreserve = F)
  }

  if(return_sf){
    spgeo <- sp::spTransform(spdf_simple, sp::CRS("+proj=longlat +datum=WGS84"))
    x <- sf::st_as_sf(spgeo)
    x$location_code <- sprintf("county%s", formatC(as.numeric(spdf$fylkesnummer), width = 2, flag = "0"))
    return(x)
  }

  spdf_simple$fylkesnummer <- spdf$fylkesnummer
  spdf_fortified <- broom::tidy(spdf_simple, region = "fylkesnummer")
  setDT(spdf_fortified)

  # convert from UTM to latlong
  utm <- spdf_fortified[, c("long", "lat")]
  sputm <- sp::SpatialPoints(utm, proj4string = sp::CRS("+proj=utm +zone=33 +datum=WGS84"))
  spgeo <- sp::spTransform(sputm, sp::CRS("+proj=longlat +datum=WGS84"))
  spgeo <- as.data.table(spgeo)

  spdf_fortified[, long := spgeo$long]
  spdf_fortified[, lat := spgeo$lat]

  spdf_fortified[, location_code := sprintf("county%s", formatC(as.numeric(id), width = 2, flag = "0"))]

  if (insert) {
    extra <- spdf_fortified[location_code %in% c("county03")]
    extra[, long_center := mean(long), by = .(location_code)]
    extra[, lat_center := mean(lat), by = .(location_code)]
    extra[, long := long + 10 + (long - long_center) * 10]
    extra[, lat := lat + 2 + (lat - lat_center) * 10]
    extra[, long_center := NULL]
    extra[, lat_center := NULL]
    extra[, group := paste0("x", group)]

    spdf_fortified <- rbind(spdf_fortified, extra)
  }

  if (split) {
    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), long := (long - mean(long)) * 0.60 + mean(long) - 17]
    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), lat := (lat - mean(lat)) * 0.70 + mean(lat) - 5.5]

    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), long_center := mean(long)]
    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), lat_center := mean(lat)]

    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), long_diff := long - long_center]
    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), lat_diff := lat - lat_center]

    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), long_diff := long_diff * cos(-0.05 * pi) + lat_diff * sin(-0.05 * pi)]
    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), lat_diff := -1 * long_diff * sin(-0.05 * pi) + lat_diff * cos(-0.05 * pi)]

    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), long := long_diff + long_center]
    spdf_fortified[location_code %in% c("county18", "county19", "county20", "county54"), lat := lat_diff + lat_center]

    spdf_fortified[, long_center := NULL]
    spdf_fortified[, lat_center := NULL]
    spdf_fortified[, long_diff := NULL]
    spdf_fortified[, lat_diff := NULL]
  }

  spdf_fortified[, hole := NULL]
  spdf_fortified[, piece := NULL]
  spdf_fortified[, id := NULL]

  return(invisible(spdf_fortified))
}

# ***************************** #
# map default ----

## 2020 ----
norway_nuts3_map_b2020_default_dt <- gen_norway_nuts3_map(x_year_end=2020)
usethis::use_data(norway_nuts3_map_b2020_default_dt, overwrite = TRUE, version = 3, compress = "xz")
norway_nuts3_map_b2020_default_sf <- gen_norway_nuts3_map(x_year_end=2020, return_sf = T)
usethis::use_data(norway_nuts3_map_b2020_default_sf, overwrite = TRUE, version = 3, compress = "xz")

## 2019 ----
norway_nuts3_map_b2019_default_dt <- gen_norway_nuts3_map(x_year_end=2019)
usethis::use_data(norway_nuts3_map_b2019_default_dt, overwrite = TRUE, version = 3, compress = "xz")
norway_nuts3_map_b2019_default_sf <- gen_norway_nuts3_map(x_year_end=2019, return_sf = TRUE)
usethis::use_data(norway_nuts3_map_b2019_default_sf, overwrite = TRUE, version = 3, compress = "xz")

## 2017 ----
norway_nuts3_map_b2017_default_dt <- gen_norway_nuts3_map(x_year_end=2017)
usethis::use_data(norway_nuts3_map_b2017_default_dt, overwrite = TRUE, version = 3, compress = "xz")
norway_nuts3_map_b2017_default_sf <- gen_norway_nuts3_map(x_year_end=2017, return_sf = T)
usethis::use_data(norway_nuts3_map_b2017_default_sf, overwrite = TRUE, version = 3, compress = "xz")

# ***************************** #
# map insert oslo ----

## 2020 ----
norway_nuts3_map_b2020_insert_oslo_dt <- gen_norway_nuts3_map(x_year_end=2020, insert = T)
usethis::use_data(norway_nuts3_map_b2020_insert_oslo_dt, overwrite = TRUE, version = 3, compress = "xz")

## 2019 ----
norway_nuts3_map_b2019_insert_oslo_dt <- gen_norway_nuts3_map(x_year_end=2019, insert = T)
usethis::use_data(norway_nuts3_map_b2019_insert_oslo_dt, overwrite = TRUE, version = 3, compress = "xz")

## 2017 ----
norway_nuts3_map_b2017_insert_oslo_dt <- gen_norway_nuts3_map(x_year_end=2017, insert = T)
usethis::use_data(norway_nuts3_map_b2017_insert_oslo_dt, overwrite = TRUE, version = 3, compress = "xz")

# ***************************** #
# map split ----

## 2020 ----
norway_nuts3_map_b2020_split_dt <- gen_norway_nuts3_map(x_year_end=2020, split=T)
usethis::use_data(norway_nuts3_map_b2020_split_dt, overwrite = TRUE, version = 3, compress = "xz")

# ***************************** #
# labels default ----

## 2020 ----
norway_nuts3_position_geolabels_b2020_default_dt <- gen_norway_nuts3_position_geolabels(x_year_end = 2020)
usethis::use_data(norway_nuts3_position_geolabels_b2020_default_dt, overwrite = TRUE, version = 3, compress = "xz")

## 2019 ----
norway_nuts3_position_geolabels_b2019_default_dt <- gen_norway_nuts3_position_geolabels(x_year_end = 2019)
usethis::use_data(norway_nuts3_position_geolabels_b2019_default_dt, overwrite = TRUE, version = 3, compress = "xz")

## 2017 ----
norway_nuts3_position_geolabels_b2017_default_dt <- gen_norway_nuts3_position_geolabels(x_year_end = 2017)
usethis::use_data(norway_nuts3_position_geolabels_b2017_default_dt, overwrite = TRUE, version = 3, compress = "xz")

# ***************************** #
# labels insert oslo ----

## 2020 ----
norway_nuts3_position_geolabels_b2020_insert_oslo_dt <- gen_norway_nuts3_position_geolabels(x_year_end = 2020, insert = T)
usethis::use_data(norway_nuts3_position_geolabels_b2020_insert_oslo_dt, overwrite = TRUE, version = 3, compress = "xz")

## 2019 ----
norway_nuts3_position_geolabels_b2019_insert_oslo_dt <- gen_norway_nuts3_position_geolabels(x_year_end = 2019, insert = T)
usethis::use_data(norway_nuts3_position_geolabels_b2019_insert_oslo_dt, overwrite = TRUE, version = 3, compress = "xz")

## 2017 ----
norway_nuts3_position_geolabels_b2017_insert_oslo_dt <- gen_norway_nuts3_position_geolabels(x_year_end = 2017, insert = T)
usethis::use_data(norway_nuts3_position_geolabels_b2017_insert_oslo_dt, overwrite = TRUE, version = 3, compress = "xz")





