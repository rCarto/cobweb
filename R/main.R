#' Get Time and Distance Between Points
#'
#' Use this function to get time and distance between points
#'
#' @param x a data.frame
#' @param src names of the variables containing source coordinates (WGS84)
#' @param dst names of the variables containing destination coordinates (WGS84)
#' @param ncl number of cores
#'
#' @return a matrix containing duration (minutes) and distance (km) is returned.
#' @export
#' @importFrom osrm osrmRoute
#' @examples
#' library(sf)
#'
#' p1 <- st_as_sf(data.frame(x = 2, y = 45), coords = c("x", "y"), crs = 4326)
#' p2 <- st_as_sf(data.frame(x = 10, y = 45), coords = c("x", "y"), crs = 4326)
#'
#' z1 <- st_buffer(p1, 50000)
#' z2 <- st_buffer(p2, 50000)
#'
#' pts1 <- st_sample(z1, 100)
#' pts2 <- st_sample(z2, 100)
#'
#' tab <- data.frame(st_coordinates(pts1), st_coordinates(pts2))
#' names(tab) <- c("Xs", "Ys", "Xd", "Yd")
#' res <- get_routes(x = tab, src = c("Xs", "Ys"), dst = c("Xd", "Yd"), ncl = 8)
#' cbind(tab, res)
get_routes <- function(x, src, dst, ncl = 1){
  x <- as.matrix(x[, c(src, dst)])
  ny <- nrow(x)
  sequence <- unique(c(seq(1, ny, 500), ny + 1))
  lseq <- length(sequence) - 1
  ml <- list()
  for  (i in 1:lseq) {
    ml[[i]] <- x[(sequence[i]):(sequence[i + 1] - 1),
                 c(src, dst)]
  }
  cl <- parallel::makeCluster(ncl)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
  roads <- foreach::`%dopar%`(
    foreach::foreach(
      ml = ml,
      .packages = "osrm",
      .combine = rbind,
      .inorder = FALSE
    ),
    {
      l <- vector("list", nrow(ml))
      for( i in seq_along(l)){
        l[[i]] <- osrmRoute(src = ml[i, src],
                            dst = ml[i, dst],
                            osrm.server = "http://172.17.201.183:5000/",
                            osrm.profile = "car",
                            overview = FALSE, )
      }
      l <- do.call(rbind, l)
      l
    }
  )
  # cat("routes ok")
  roads
}

#'
#'
#' #' cobweb
#' #'
#' #' Build a city cobweb
#' #' @param city city name with postal code if possible and country name in english
#' #' @param filename filename of the map produced
#' #' @param buffer area radius
#' #' @param n number of points
#' #' @return a map is exported
#' #' @import sf stplanr mapsf grDevices osrm
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' cobweb("Marseille, 13000 France")
#' #' }
#' cobweb <- function(city, filename = "map.png", buffer = 10000, n = 1000){
#'   v_raw <- suppressMessages(tidygeocoder::geo(city,
#'                                               progress_bar = FALSE,
#'                                               verbose = FALSE))
#'   v_raw <- v_raw[1,]
#'   if(is.na(v_raw$lat)){stop("City not found")}
#'   v <- st_as_sf(data.frame(v_raw), coords = c("long", "lat"),
#'                 crs = "EPSG:4326")
#'   v <- st_transform(v, "EPSG:3035")
#'   zone <- st_buffer(v, buffer)
#'   eu <- st_read(system.file("gpkg/x.gpkg", package = "cobweb"),
#'                         layer = "x", quiet = TRUE
#'   )
#'   zone <- suppressWarnings(st_intersection(zone, eu))
#'   pts <- st_sample(x = zone, size = n)
#'   pts_r <- st_transform(pts, 4326)
#'
#'   cc <- st_coordinates(pts_r)
#'   df <- data.frame(srcx = v_raw$long, srcy = v_raw$lat,
#'                    dstx = cc[,1], dsty = cc[, 2])
#'   x <- get_routes(df, ncl = 8, crs = "EPSG:3035")
#'
#'   x <- suppressWarnings(st_intersection(x, st_geometry(zone)))
#'   x <- st_collection_extract(x, "LINESTRING")
#'   x <- suppressWarnings(st_cast(x, "LINESTRING"))
#'
#'   a <- unique(x[, c('duration', 'distance')])
#'   a$f <- 1
#'   roads <- invisible(overline2(sl = a, attrib = "f", quiet = TRUE))
#'
#'   mf_theme("green")
#'   mf_export(x = roads, filename = filename, width = 900)
#'   mf_map(roads, "f", "grad", breaks = "geom", add = TRUE,
#'          leg_pos= NA, col = "green")
#'   mf_map(v, pch = "\u066D", add = TRUE, col = "red", cex = 7)
#'   mf_title(city)
#'   invisible(dev.off())
#' }
#'
#'
