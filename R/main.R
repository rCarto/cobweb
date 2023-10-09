# Get Time and Distance Between Points
# Use this function to get time and distance between points
#
# @param x a data.frame
# @param src names of the variables containing source coordinates (WGS84)
# @param dst names of the variables containing destination coordinates (WGS84)
# @param ncl number of cores
#
# @return a matrix containing duration (minutes) and distance (km) is returned.
# @export
# @importFrom osrm osrmRoute
# @examples
# library(sf)
#
# p1 <- st_as_sf(data.frame(x = 2, y = 45), coords = c("x", "y"), crs = 4326)
#p2 <- st_as_sf(data.frame(x = 10, y = 45), coords = c("x", "y"), crs = 4326)
#
# z1 <- st_buffer(p1, 50000)
# z2 <- st_buffer(p2, 50000)
#
# pts1 <- st_sample(z1, 100)
# pts2 <- st_sample(z2, 100)
#
# tab <- data.frame(st_coordinates(pts1), st_coordinates(pts2))
# names(tab) <- c("Xs", "Ys", "Xd", "Yd")
# res <- get_routes(x = tab, src = c("Xs", "Ys"), dst = c("Xd", "Yd"), ncl = 8)
# cbind(tab, res)
#
get_routes <- function(x,
                       srcX = "srcx", srcY = "srcy",
                       dstX = "dstx", dstY = "dsty",
                       ncl = 6, url = "http://0.0.0.0:5000/"){
  x <- as.matrix(x[, c(srcX, srcY, dstX, dstY)])
  ny <- nrow(x)
  chunk_s <- round(ny/ncl, 0)
  sequence <- unique(c(seq(1, ny, chunk_s), ny + 1))
  lseq <- length(sequence) - 1
  ml <- list()
  for  (i in 1:lseq) {
    ml[[i]] <- x[(sequence[i]):(sequence[i + 1] - 1), ]
  }
  cl <- parallel::makeCluster(ncl)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  roads <- foreach::`%dopar%`(
    foreach::foreach(
      ml = ml,
      .packages = c("osrm"),
      .combine = rbind,
      .inorder = FALSE
    ),
    {
      l <- vector("list", nrow(ml))
      for( i in seq_along(l)){
        l[[i]] <-
          tryCatch(
            osrmRoute(src = ml[i, c(srcX, srcY)],
                      dst = ml[i, c(dstX, dstY)],
                      osrm.server = url,
                      osrm.profile = "car",
                      overview = "full"),
            error = function(x){NA})
      }
      l <- do.call(rbind, l[!is.na(l)])
      l
    }
  )
  roads
}

#' cobweb
#'
#' Build a city cobweb
#' @param city city name with postal code if possible and country name in English
#' @param filename file name of the map produced
#' @param span max road distance in km
#' @param nroad number of roads
#' @param ncl number of cores
#' @param verbose output messages
#' @param url OSRM server URL
#'
#' @return Nothing is returned, a map is exported.
#' @import sf stplanr mapsf grDevices osrm tictoc
#' @export
#'
#' @examples
#' \dontrun{
#' cobweb("Marseille, 13000 France")
#' }
cobweb <- function(city, span = 2000, nroad = 1000, ncl = 1,
                   filename = "map.png", verbose = FALSE,
                   url = "http://0.0.0.0:5000/"){
  verbose <- !verbose


  tic("Getting the city position")
  v_raw <- suppressMessages(tidygeocoder::geo(city, progress_bar = FALSE,
                                              verbose = FALSE))
  v_raw <- v_raw[1,]
  if(is.na(v_raw$lat)){stop("City not found!")}
  toc(quiet = verbose)


  tic("Preparing queries")
  v <- st_as_sf(data.frame(v_raw), coords = c("long", "lat"), crs = "EPSG:4326")
  v <- st_transform(v, "EPSG:3035")
  zone <- st_buffer(v, span * 1000)
  x <- st_read(system.file("gpkg/eur.gpkg", package = "cobweb"), quiet = TRUE)
  pts <- st_sample(x = st_intersection(st_geometry(zone), st_geometry(x)),
                   size = nroad)
  pts_r <- st_transform(pts, 4326)
  cc <- st_coordinates(pts_r)
  df <- data.frame(srcx = v_raw$long, srcy = v_raw$lat,
                   dstx = cc[,1], dsty = cc[, 2])
  toc(quiet = verbose)


  tic("Getting the roads from OSRM")
  x <- get_routes(df, ncl = ncl, url = url)
  toc(quiet = verbose)


  tic("Exporting the map")


  x <- st_transform(x, "EPSG:3035")
  x <- unique(x[, c("duration","distance")])
  x <- st_intersection(st_geometry(x), st_geometry(zone))
  mf_theme("iceberg")
  mf_export(x = x, filename = filename, width = 800)
  mf_map(x, col = "#BDD6DB80", add = TRUE)
  mf_map(v, pch = "\u066D", add = TRUE, col = "red", cex = 7)
  mf_title(city, pos = "left", fg = getOption("mapsf.fg"),
           bg = getOption("mapsf.bg"))
  mf_credits("Data: \u24d2 OpenStreetMap contributors, Routing: OSRM")
  invisible(dev.off())
  toc(quiet = verbose)
}




