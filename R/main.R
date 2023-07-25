get_routes <- function(x,
                       srcX = "srcx", srcY = "srcy",
                       dstX = "dstx", dstY = "dsty",
                       ncl = 5, crs){
  ny <- nrow(x)
  sequence <- unique(c(seq(1, ny, 500), ny + 1))
  lseq <- length(sequence) - 1
  ml <- list()
  for  (i in 1:lseq) {
    ml[[i]] <- x[(sequence[i]):(sequence[i + 1] - 1),
                 c(srcX, srcY, dstX, dstY)]
  }
  cl <- parallel::makeCluster(ncl)
  doParallel::registerDoParallel(cl,)
  roads <- foreach::`%dopar%`(
    foreach::foreach(
      ml = ml,
      .packages = c("osrm", "sf"),
      .combine = rbind,
      .inorder = FALSE
    ),
    {
      l <- vector("list", nrow(ml))
      for( i in seq_along(l)){
        l[[i]] <- osrmRoute(src = ml[i, c(srcX, srcY)],
                            dst = ml[i, c(dstX, dstY)],
                            osrm.server = "http://172.17.201.183:5000/",
                            osrm.profile = "car",
                            overview = "full")
      }
      l <- do.call(rbind, l)
      l
    }
  )
  parallel::stopCluster(cl)
  # cat("routes ok")

  if(!missing(crs)){
    roads <- st_transform(roads, crs)
  }

  roads
}




#' cobweb
#'
#' Build a city cobweb
#' @param city city name with postal code if possible and country name in english
#' @param filename filename of the map produced
#'
#' @return a map is exported
#' @import sf stplanr mapsf grDevices osrm
#' @export
#'
#' @examples
#' \dontrun{
#' cobweb("Marseille, 13000 France")
#' }
cobweb <- function(city, filename = "map.png"){
  v_raw <- suppressMessages(tidygeocoder::geo(city,
                                              progress_bar = FALSE,
                                              verbose = FALSE))
  v_raw <- v_raw[1,]
  if(is.na(v_raw$lat)){stop("City not found")}
    v <- st_as_sf(data.frame(v_raw), coords = c("long", "lat"),
                  crs = "EPSG:4326")
    v <- st_transform(v, "EPSG:3035")
    zone <- st_buffer(v, 150000)
    pts <- st_sample(x = zone, size = 500)
    pts_r <- st_transform(pts, 4326)

    cc <- st_coordinates(pts_r)
    df <- data.frame(srcx = v_raw$long, srcy = v_raw$lat,
                     dstx = cc[,1], dsty = cc[, 2])
    x <- get_routes(df, ncl = 8, crs = "EPSG:3035")

    x <- suppressWarnings(st_intersection(x, st_geometry(zone)))
    x <- st_collection_extract(x, "LINESTRING")
    x <- suppressWarnings(st_cast(x, "LINESTRING"))

    a <- unique(x[, c('duration', 'distance')])
    a$f <- 1
    roads <- invisible(overline2(sl = a, attrib = "f", quiet = TRUE))

    mf_theme("green")
    mf_export(x = roads, filename = filename, width = 900)
    mf_map(roads, "f", "grad", breaks = "geom", add = TRUE,
           leg_pos= NA, col = "green")
    mf_map(v, pch = "\u066D", add = TRUE, col = "red", cex = 7)
    mf_title(city)
    invisible(dev.off())
}
