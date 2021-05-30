#' add polylines to a leaflet map using Leaflet.glify
#'
#' @details
#'   MULTILINESTRINGs are currently not supported! Make sure you cast your data
#'   to LINETSRING first (e.g. using \code{sf::st_cast(data, "LINESTRING")}.
#'
#' @examples
#' if (interactive()) {
#' library(leaflet)
#' library(leafgl)
#' library(sf)
#'
#' storms = st_as_sf(atlStorms2005)
#'
#' cols = heat.colors(nrow(storms))
#'
#' leaflet() %>%
#'   addProviderTiles(provider = providers$CartoDB.Positron) %>%
#'   addGlPolylines(data = storms, color = cols, popup = TRUE, opacity = 1)
#' }
#'
#' @describeIn addGlPoints add polylines to a leaflet map using Leaflet.glify
#' @aliases addGlPolylines
#' @export addGlPolylines
addGlPolylines = function(map,
                          data,
                          color = cbind(0, 0.2, 1),
                          opacity = 0.6,
                          group = "glpolylines",
                          popup = NULL,
                          weight = 1,
                          layerId = NULL,
                          src = FALSE,
                          ...) {

  if (isTRUE(src)) {
    m = addGlPolylinesSrc(
      map = map
      , data = data
      , color = color
      , opacity = opacity
      , group = group
      , popup = popup
      , weight = weight
      , layerId = layerId
      , ...
    )
    return(m)
  }

  ## currently leaflet.glify only supports single (fill)opacity!
  opacity = opacity[1]

  if (is.null(group)) group = deparse(substitute(data))
  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
  stopifnot(inherits(sf::st_geometry(data), c("sfc_LINESTRING", "sfc_MULTILINESTRING")))
  if (inherits(sf::st_geometry(data), "sfc_MULTILINESTRING"))
    stop("Can only handle LINESTRINGs, please cast your MULTILINESTRING to LINESTRING using sf::st_cast",
         call. = FALSE)

  bounds = as.numeric(sf::st_bbox(data))

  # color
  args <- list(...)
  palette = "viridis"
  if ("palette" %in% names(args)) {
    palette <- args$palette
    args$palette = NULL
  }
  color <- makeColorMatrix(color, data, palette = palette)
  if (ncol(color) != 3) stop("only 3 column color matrix supported so far")
  color = as.data.frame(color, stringsAsFactors = FALSE)
  colnames(color) = c("r", "g", "b")

  cols = jsonify::to_json(color, digits = 3)

  # popup
  if (is.null(popup) || isFALSE(popup)) {
    # geom = sf::st_transform(sf::st_geometry(data), crs = 4326)
    geom = sf::st_geometry(data)
    data = sf::st_sf(id = 1:length(geom), geometry = geom)
  } else if (isTRUE(popup)) {
    data = data[, popup]
  } else {
    htmldeps <- htmltools::htmlDependencies(popup)
    if (length(htmldeps) != 0) {
      map$dependencies = c(
        map$dependencies,
        htmldeps
      )
    }
    popup = makePopup(popup, data)
    popup = jsonify::to_json(popup)
    geom = sf::st_geometry(data)
    data = sf::st_sf(id = 1:length(geom), geometry = geom)
  }

  # data
  if (length(args) == 0) {
    geojsonsf_args = NULL
  } else {
    geojsonsf_args = try(
      match.arg(
        names(args)
        , names(as.list(args(geojsonsf::sf_geojson)))
        , several.ok = TRUE
      )
      , silent = TRUE
    )
    if (inherits(geojsonsf_args, "try-error")) geojsonsf_args = NULL
    if (identical(geojsonsf_args, "sf")) geojsonsf_args = NULL
  }
  data = do.call(geojsonsf::sf_geojson, c(list(data), args[geojsonsf_args]))
  # data = geojsonsf::sf_geojson(data, ...)

  # dependencies
  map$dependencies = c(
    glifyDependencies()
    , map$dependencies
  )

  # weight is about double the weight of svg, so / 2

  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addGlifyPolylines'
    , data
    , cols
    , popup
    , opacity
    , group
    , weight
    , layerId
  )

  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )

}


### via src
addGlPolylinesSrc = function(map,
                             data,
                             color = cbind(0, 0.2, 1),
                             opacity = 0.8,
                             group = "glpolygons",
                             popup = NULL,
                             weight = 1,
                             layerId = NULL,
                             ...) {

  if (is.null(group)) group = deparse(substitute(data))
  if (is.null(layerId)) layerId = paste0(group, "-lns")
  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
  stopifnot(inherits(sf::st_geometry(data), c("sfc_LINESTRING", "sfc_MULTILINESTRING")))
  if (inherits(sf::st_geometry(data), "sfc_MULTILINESTRING"))
    stop("Can only handle LINESTRINGs, please cast your MULTILINESTRING ",
         "to LINESTRING using e.g. sf::st_cast")

  bounds = as.numeric(sf::st_bbox(data))

  # temp directories
  dir_data = tempfile(pattern = "glify_polylines_dat")
  dir.create(dir_data)
  dir_color = tempfile(pattern = "glify_polylines_col")
  dir.create(dir_color)
  dir_popup = tempfile(pattern = "glify_polylines_pop")
  dir.create(dir_popup)
  dir_weight = tempfile(pattern = "glify_polylines_wgt")
  dir.create(dir_weight)

  # data
  data_orig <- data
  geom = sf::st_geometry(data)
  data = sf::st_sf(id = 1:length(geom), geometry = geom)

  ell_args <- list(...)
  fl_data = paste0(dir_data, "/", layerId, "_data.js")
  pre = paste0('var data = data || {}; data["', layerId, '"] = ')
  writeLines(pre, fl_data)
  jsonify_args = try(
    match.arg(
      names(ell_args)
      , names(as.list(args(geojsonsf::sf_geojson)))
      , several.ok = TRUE
    )
    , silent = TRUE
  )
  if (inherits(jsonify_args, "try-error")) jsonify_args = NULL
  if (identical(jsonify_args, "sf")) jsonify_args = NULL
  cat('[', do.call(geojsonsf::sf_geojson, c(list(data), ell_args[jsonify_args])), '];',
      file = fl_data, sep = "", append = TRUE)

  map$dependencies = c(
    map$dependencies,
    glifyDependenciesSrc(),
    glifyDataAttachmentSrc(fl_data, layerId)
  )

  # color
  palette = "viridis"
  if ("palette" %in% names(ell_args)) {
    palette <- ell_args$palette
  }
  color <- makeColorMatrix(color, data_orig, palette = palette)
  if (ncol(color) != 3) stop("only 3 column color matrix supported so far")
  color = as.data.frame(color, stringsAsFactors = FALSE)
  colnames(color) = c("r", "g", "b")

  if (nrow(color) > 1) {
    fl_color = paste0(dir_color, "/", layerId, "_color.js")
    pre = paste0('var col = col || {}; col["', layerId, '"] = ')
    writeLines(pre, fl_color)
    cat('[', jsonify::to_json(color), '];',
        file = fl_color, append = TRUE)

    map$dependencies = c(
      map$dependencies,
      glifyColorAttachmentSrc(fl_color, layerId)
    )

    color = NULL
  }

  # popup
  if (!is.null(popup)) {
    htmldeps <- htmltools::htmlDependencies(popup)
    if (length(htmldeps) != 0) {
      map$dependencies = c(
        map$dependencies,
        htmldeps
      )
    }
    popup = makePopup(popup, data_orig)
    fl_popup = paste0(dir_popup, "/", layerId, "_popup.js")
    pre = paste0('var popup = popup || {}; popup["', layerId, '"] = ')
    writeLines(pre, fl_popup)
    cat('[', jsonify::to_json(popup), '];',
        file = fl_popup, append = TRUE)

    map$dependencies = c(
      map$dependencies,
      glifyPopupAttachmentSrc(fl_popup, layerId)
    )

  }

  # weight
  if (length(unique(weight)) > 1) {
    fl_weight = paste0(dir_weight, "/", layerId, "_weight.js")
    pre = paste0('var wgt = wgt || {}; wgt["', layerId, '"] = ')
    writeLines(pre, fl_weight)
    cat('[', jsonify::to_json(weight), '];',
        file = fl_weight, append = TRUE)

    map$dependencies = c(
      map$dependencies,
      glifyRadiusAttachmentSrc(fl_weight, layerId)
    )

    weight = NULL
  }

  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addGlifyPolylinesSrc'
    , color
    , weight
    , opacity
    , group
    , layerId
  )

  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )

}


