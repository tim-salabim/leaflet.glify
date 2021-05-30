#' add polygons to a leaflet map using Leaflet.glify
#'
#' @details
#'   MULTIPOLYGONs are currently not supported! Make sure you cast your data
#'   to POLYGON first (e.g. using \code{sf::st_cast(data, "POLYGON")}.
#'
#' @examples
#' if (interactive()) {
#' library(leaflet)
#' library(leafgl)
#' library(sf)
#'
#' gadm = st_as_sf(gadmCHE)
#' gadm = st_cast(gadm, "POLYGON")
#' cols = grey.colors(nrow(gadm))
#'
#' leaflet() %>%
#'   addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
#'   addGlPolygons(data = gadm, color = cols, popup = TRUE)
#' }
#'
#' @describeIn addGlPoints add polygons to a leaflet map using Leaflet.glify
#' @aliases addGlPolygons
#' @export addGlPolygons
addGlPolygons = function(map,
                         data,
                         color = cbind(0, 0.2, 1),
                         fillColor = color,
                         fillOpacity = 0.8,
                         group = "glpolygons",
                         popup = NULL,
                         layerId = NULL,
                         src = FALSE,
                         ...) {

  if (isTRUE(src)) {
    m = addGlPolygonsSrc(
      map = map
      , data = data
      , color = color
      , fillColor = fillColor
      , fillOpacity = fillOpacity
      , group = group
      , popup = popup
      , layerId = layerId
      , ...
    )
    return(m)
  }

  ## currently leaflet.glify only supports single (fill)opacity!
  fillOpacity = fillOpacity[1]

  if (is.null(group)) group = deparse(substitute(data))
  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
  stopifnot(inherits(sf::st_geometry(data), c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
  if (inherits(sf::st_geometry(data), "sfc_MULTIPOLYGON"))
    stop("Can only handle POLYGONs, please cast your MULTIPOLYGON to POLYGON using sf::st_cast",
         call. = FALSE)

  bounds = as.numeric(sf::st_bbox(data))

  # fillColor
  args <- list(...)
  palette = "viridis"
  if ("palette" %in% names(args)) {
    palette <- args$palette
    args$palette = NULL
  }
  fillColor <- makeColorMatrix(fillColor, data, palette = palette)
  if (ncol(fillColor) != 3) stop("only 3 column fillColor matrix supported so far")
  fillColor = as.data.frame(fillColor, stringsAsFactors = FALSE)
  colnames(fillColor) = c("r", "g", "b")

  cols = jsonify::to_json(fillColor, digits = 3)

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


  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addGlifyPolygons'
    , data
    , cols
    , popup
    , fillOpacity
    , group
    , layerId
  )

  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )

}



### via src
addGlPolygonsSrc = function(map,
                            data,
                            color = cbind(0, 0.2, 1),
                            fillColor = color,
                            fillOpacity = 0.6,
                            group = "glpolygons",
                            popup = NULL,
                            layerId = NULL,
                            ...) {

  if (is.null(group)) group = deparse(substitute(data))
  if (is.null(layerId)) layerId = paste0(group, "-pls")
  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
  stopifnot(inherits(sf::st_geometry(data), c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
  if (inherits(sf::st_geometry(data), "sfc_MULTIPOLYGON"))
    stop("Can only handle POLYGONs, please cast your MULTIPOLYGON ",
         "to POLYGON using e.g. sf::st_cast")

  bounds = as.numeric(sf::st_bbox(data))

  # temp directories
  dir_data = tempfile(pattern = "glify_polygons_dat")
  dir.create(dir_data)
  dir_color = tempfile(pattern = "glify_polygons_col")
  dir.create(dir_color)
  dir_popup = tempfile(pattern = "glify_polygons_pop")
  dir.create(dir_popup)

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
  fillColor <- makeColorMatrix(fillColor, data_orig, palette = palette)
  if (ncol(fillColor) != 3) stop("only 3 column fillColor matrix supported so far")
  fillColor = as.data.frame(fillColor, stringsAsFactors = FALSE)
  colnames(fillColor) = c("r", "g", "b")

  if (nrow(fillColor) > 1) {
    fl_color = paste0(dir_color, "/", layerId, "_color.js")
    pre = paste0('var col = col || {}; col["', layerId, '"] = ')
    writeLines(pre, fl_color)
    cat('[', jsonify::to_json(fillColor), '];',
        file = fl_color, append = TRUE)

    map$dependencies = c(
      map$dependencies,
      glifyColorAttachmentSrc(fl_color, layerId)
    )

    fillColor = NULL
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

  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addGlifyPolygonsSrc'
    , fillColor
    , fillOpacity
    , group
    , layerId
  )

  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )

}


# ### via attachments
# addGlPolygonsFl = function(map,
#                            data,
#                            color = cbind(0, 0.2, 1),
#                            opacity = 0.6,
#                            weight = 10,
#                            group = "glpolygons",
#                            popup = NULL,
#                            ...) {
#
#   if (is.null(group)) group = deparse(substitute(data))
#   if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
#   stopifnot(inherits(sf::st_geometry(data), c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
#   if (inherits(sf::st_geometry(data), "sfc_MULTIPOLYGON"))
#     stop("Can only handle POLYGONs, please cast your MULTIPOLYGON to POLYGON using sf::st_cast")
#
#   # temp directories
#   dir_data = tempfile(pattern = "glify_polygons_dt")
#   dir.create(dir_data)
#   dir_color = tempfile(pattern = "glify_polygons_cl")
#   dir.create(dir_color)
#   # dir_popup = tempfile(pattern = "glify_polygons_pop")
#   # dir.create(dir_popup)
#
#   # data
#   geom = sf::st_transform(sf::st_geometry(data), crs = 4326)
#   if (is.null(popup)) {
#     data = sf::st_sf(id = 1:length(geom),
#                      geometry = geom)
#   } else {
#     data = sf::st_transform(data[, popup], crs = 4326)
#   }
#   # crds = sf::st_coordinates(data)[, c(2, 1)]
#
#   fl_data = paste0(dir_data, "/", group, "_data.json")
#   cat(geojsonsf::sf_geojson(data), file = fl_data, append = FALSE)
#   data_var = paste0(group, "dt")
#
#   # color
#   if (ncol(color) != 3) stop("only 3 column color matrix supported so far")
#   color = as.data.frame(color, stringsAsFactors = FALSE)
#   colnames(color) = c("r", "g", "b")
#
#   jsn = jsonify::to_json(color)
#   fl_color = paste0(dir_color, "/", group, "_color.json")
#   color_var = paste0(group, "cl")
#   cat(jsn, file = fl_color, append = FALSE)
#
#   # popup
#   # if (!is.null(popup)) {
#   #   pop = jsonlite::toJSON(data[[popup]])
#   #   fl_popup = paste0(dir_popup, "/", group, "_popup.json")
#   #   popup_var = paste0(group, "pop")
#   #   cat(pop, file = fl_popup, append = FALSE)
#   # } else {
#   #   popup_var = NULL
#   # }
#
#   # dependencies
#   map$dependencies = c(
#     map$dependencies,
#     glifyDependencies(),
#     glifyDataAttachment(fl_data, group),
#     glifyColorAttachment(fl_color, group)
#   )
#
#   # if (!is.null(popup)) {
#   #   map$dependencies = c(
#   #     map$dependencies,
#   #     glifyPopupAttachment(fl_popup, group)
#   #   )
#   # }
#
#   leaflet::invokeMethod(map, leaflet::getMapData(map), 'addGlifyPolygonsFl',
#                         data_var, color_var, popup, opacity)
#
# }
