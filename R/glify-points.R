#' add points to a leaflet map using Leaflet.glify
#'
#' @description
#'   Leaflet.glify is a web gl renderer plugin for leaflet. See
#'   \url{https://github.com/robertleeplummerjr/Leaflet.glify} for details
#'   and documentation.
#'
#' @param map a leaflet map to add points/polygons to.
#' @param data sf/sp point/polygon data to add to the map.
#' @param color Object representing the color. Can be of class integer, character with
#'   color names, HEX codes or random characters, factor, matrix, data.frame, list, json or formula.
#'   See the examples or \link{makeColorMatrix} for more information.
#' @param fillColor fill color.
#' @param opacity feature opacity. Numeric between 0 and 1.
#'   Note: expect funny results if you set this to < 1.
#' @param fillOpacity fill opacity.
#' @param radius point size in pixels.
#' @param group a group name for the feature layer.
#' @param popup Object representing the popup. Can be of type character with column names,
#'   formula, logical, data.frame or matrix, Spatial, list or JSON. If the length does not
#'   match the number of rows in the dataset, the popup vector is repeated to match the dimension.
#' @param layerId the layer id
#' @param weight line width/thicknes in pixels for \code{addGlPolylines}.
#' @param src whether to pass data to the widget via file attachments.
#' @param hover Object representing the hover. Can be of type character with column names,
#'   formula, logical, data.frame or matrix, Spatial, list or JSON. If the length does not
#'   match the number of rows in the dataset, the popup vector is repeated to match the dimension.
#' @param hoverWait Amount of milliseconds to wait before the next hover event is triggered.
#' @param pane A string which defines the pane of the layer.
#' @param sensitivity A numeric value which exaggerates the size of the clickable
#'   area to make it easier to click a point/line.
#' @param sensitivityHover A numeric value which exaggerates the size of the hoverable
#'   area to make it easier to hover a point/line.
#' @param border Boolean value which determines if a border should be drawn.
#' @param ... Passed to \code{\link[jsonify]{to_json}} for the data coordinates,
#'   except for the \code{palette} argument, which is used for the coloring method.
#'
#' @describeIn addGlPoints add points to a leaflet map using Leaflet.glify
#' @examples
#' if (interactive()) {
#' library(leaflet)
#' library(leafgl)
#' library(sf)
#'
#' n = 1e5
#'
#' df1 = data.frame(id = 1:n,
#'                  x = rnorm(n, 10, 1),
#'                  y = rnorm(n, 49, 0.8))
#' pts = st_as_sf(df1, coords = c("x", "y"), crs = 4326)
#'
#' cols = topo.colors(nrow(pts))
#'
#' leaflet() %>%
#'   addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
#'   addGlPoints(data = pts, fillColor = cols, popup = TRUE)
#'
#' }
#'
#' @export addGlPoints
addGlPoints = function(map,
                       data,
                       fillColor = "#0033ff",
                       fillOpacity = 0.8,
                       radius = 10,
                       group = "glpoints",
                       popup = NULL,
                       layerId = NULL,
                       src = FALSE,
                       hover = NULL,
                       hoverWait = 250,
                       sensitivity = 0.1,
                       sensitivityHover = 0.03,
                       pane = "overlayPane",
                       ...) {

  if (isTRUE(src)) {
    m = addGlPointsSrc(
      map = map
      , data = data
      , fillColor = fillColor
      , fillOpacity = fillOpacity
      , radius = radius
      , group = group
      , popup = popup
      , layerId = layerId
      , hover = hover
      , hoverWait = hoverWait
      , sensitivity = sensitivity
      , sensitivityHover = sensitivityHover
      , pane = pane
      , ...
    )
    return(m)
  }

  ## currently leaflet.glify only supports single (fill)opacity!
  fillOpacity = fillOpacity[1]

  if (is.null(group)) group = deparse(substitute(data))
  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
  stopifnot(inherits(sf::st_geometry(data), c("sfc_POINT", "sfc_MULTIPOINT")))

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

  fillColor = jsonify::to_json(fillColor)

  # popup
  if (!is.null(popup)) {
    htmldeps <- htmltools::htmlDependencies(popup)
    if (length(htmldeps) != 0) {
      map$dependencies = c(
        map$dependencies,
        htmldeps
      )
    }
    popup = makePopup(popup, data)
    popup = jsonify::to_json(popup)
  }

  # hover
  if (!is.null(hover)) {
    htmldeps <- htmltools::htmlDependencies(hover)
    if (length(htmldeps) != 0) {
      map$dependencies = c(
        map$dependencies,
        htmldeps
      )
    }
    hover = makePopup(hover, data)
    hover = jsonify::to_json(hover)
  }

  # data
  # data = sf::st_transform(data, 4326)
  crds = sf::st_coordinates(data)[, c(2, 1)]
  # convert data to json
  if (length(args) == 0) {
    jsonify_args = NULL
  } else {
    jsonify_args = try(
      match.arg(
        names(args)
        , names(as.list(args(jsonify::to_json)))
        , several.ok = TRUE
      )
      , silent = TRUE
    )
  }
  if (inherits(jsonify_args, "try-error")) jsonify_args = NULL
  data = do.call(jsonify::to_json, c(list(crds), args[jsonify_args]))

  # dependencies
  map$dependencies = c(
    glifyDependencies()
    , map$dependencies
  )


  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addGlifyPoints'
    , data
    , fillColor
    , popup
    , fillOpacity
    , radius
    , group
    , layerId
    , hover
    , hoverWait
    , sensitivity
    , sensitivityHover
    , pane
  )

  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )


}


### via src
addGlPointsSrc = function(map,
                          data,
                          fillColor = "#0033ff",
                          fillOpacity = 1,
                          radius = 10,
                          group = "glpoints",
                          popup = NULL,
                          layerId = NULL,
                          hover = NULL,
                          hoverWait = 250,
                          sensitivity = 0.1,
                          sensitivityHover = 0.03,
                          pane = "overlayPane",
                          ...) {

  ## currently leaflet.glify only supports single (fill)opacity!
  fillOpacity = fillOpacity[1]

  if (is.null(group)) group = deparse(substitute(data))
  if (is.null(layerId)) layerId = paste0(group, "-pts")
  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
  stopifnot(inherits(sf::st_geometry(data), c("sfc_POINT", "sfc_MULTIPOINT")))

  bounds = as.numeric(sf::st_bbox(data))

  # temp directories
  dir_data = tempfile(pattern = "glify_points_dat")
  dir.create(dir_data)
  dir_color = tempfile(pattern = "glify_points_col")
  dir.create(dir_color)
  dir_popup = tempfile(pattern = "glify_points_pop")
  dir.create(dir_popup)
  dir_radius = tempfile(pattern = "glify_points_rad")
  dir.create(dir_radius)

  # data
  # data = sf::st_transform(data, 4326)
  crds = sf::st_coordinates(data)[, c(2, 1)]

  ell_args <- list(...)
  fl_data = paste0(dir_data, "/", layerId, "_data.js")
  pre = paste0('var data = data || {}; data["', layerId, '"] = ')
  writeLines(pre, fl_data)
  jsonify_args = try(
    match.arg(
      names(ell_args)
      , names(as.list(args(jsonify::to_json)))
      , several.ok = TRUE
    )
    , silent = TRUE
  )
  if (inherits(jsonify_args, "try-error")) jsonify_args = NULL
  if (identical(jsonify_args, "x")) jsonify_args = NULL
  cat('[', do.call(jsonify::to_json, c(list(crds), ell_args[jsonify_args])), '];',
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
  fillColor <- makeColorMatrix(fillColor, data, palette = palette)
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
    popup = makePopup(popup, data)
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

  # radius
  if (length(unique(radius)) > 1) {
    fl_radius = paste0(dir_radius, "/", layerId, "_radius.js")
    pre = paste0('var rad = rad || {}; rad["', layerId, '"] = ')
    writeLines(pre, fl_radius)
    cat('[', jsonify::to_json(radius), '];',
        file = fl_radius, append = TRUE)

    map$dependencies = c(
      map$dependencies,
      glifyRadiusAttachmentSrc(fl_radius, layerId)
    )

    radius = NULL
  }

  # leaflet::invokeMethod(
  #   map
  #   , leaflet::getMapData(map)
  #   , 'addGlifyPointsSrc'
  #   , fillOpacity
  #   , radius
  #   , group
  #   , layerId
  # )

  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addGlifyPointsSrc'
    , fillColor
    , radius
    , fillOpacity
    , group
    , layerId
    , hover
    , hoverWait
    , sensitivity
    , sensitivityHover
    , pane
  )

  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )

}


# ### via src
# addGlPointsSrc2 = function(map,
#                            data,
#                            color = cbind(0, 0.2, 1),
#                            opacity = 1,
#                            weight = 10,
#                            group = "glpoints",
#                            popup = NULL,
#                            layerId = NULL,
#                            ...) {
#
#   if (is.null(group)) group = deparse(substitute(data))
#   if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
#   stopifnot(inherits(sf::st_geometry(data), c("sfc_POINT", "sfc_MULTIPOINT")))
#
#   # temp directories
#   dir_data = tempfile(pattern = "glify_points_dat")
#   dir.create(dir_data)
#   dir_color = tempfile(pattern = "glify_points_col")
#   dir.create(dir_color)
#   dir_popup = tempfile(pattern = "glify_points_pop")
#   dir.create(dir_popup)
#
#   # data
#   data = sf::st_transform(data, 4326)
#   crds = sf::st_coordinates(data)[, c(2, 1)]
#
#   grp1 = paste0(group, "_1")
#   grp2 = paste0(group, "_2")
#
#   fl_data1 = paste0(dir_data, "/", grp1, "_data.json")
#   fl_data2 = paste0(dir_data, "/", grp2, "_data.json")
#   pre1 = paste0('var data = data || {}; data["', grp1, '"] = ')
#   writeLines(pre1, fl_data1)
#   cat('[', jsonify::to_json(crds[1:100, ], ...), '];',
#       file = fl_data1, sep = "", append = TRUE)
#   pre2 = paste0('var data = data || {}; data["', grp2, '"] = ')
#   writeLines(pre2, fl_data2)
#   cat('[', jsonify::to_json(crds[101:nrow(crds), ], ...), '];',
#       file = fl_data2, sep = "", append = TRUE)
#
#   # color
#   if (ncol(color) != 3) stop("only 3 column color matrix supported so far")
#   color = as.data.frame(color, stringsAsFactors = FALSE)
#   colnames(color) = c("r", "g", "b")
#
#   fl_color = paste0(dir_color, "/", group, "_color.json")
#   pre = paste0('var col = col || {}; col["', group, '"] = ')
#   writeLines(pre, fl_color)
#   cat('[', jsonify::to_json(color), '];',
#       file = fl_color, append = TRUE)
#
#   # popup
#   if (!is.null(popup)) {
#     fl_popup = paste0(dir_popup, "/", group, "_popup.json")
#     pre = paste0('var popup = popup || {}; popup["', group, '"] = ')
#     writeLines(pre, fl_popup)
#     cat('[', jsonify::to_json(data[[popup]]), '];',
#         file = fl_popup, append = TRUE)
#   } else {
#     popup = NULL
#   }
#
#   # dependencies
#   map$dependencies = c(
#     map$dependencies,
#     glifyDependenciesSrc(),
#     glifyDataAttachmentSrc(fl_data1, grp1),
#     glifyDataAttachmentSrc(fl_data2, grp1, TRUE),
#     glifyColorAttachmentSrc(fl_color, group)
#   )
#
#   if (!is.null(popup)) {
#     map$dependencies = c(
#       map$dependencies,
#       glifyPopupAttachmentSrc(fl_popup, group)
#     )
#   }
#
#   leaflet::invokeMethod(map, leaflet::getMapData(map), 'addGlifyPointsSrc2',
#                         group, opacity, weight, layerId)
#
# }
#
#
#
# ### via attachments
# addGlPointsFl = function(map,
#                          data,
#                          color = cbind(0, 0.2, 1),
#                          opacity = 1,
#                          weight = 10,
#                          group = "glpoints",
#                          popup = NULL,
#                          layerId = NULL,
#                          ...) {
#
#   if (is.null(group)) group = deparse(substitute(data))
#   if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
#   stopifnot(inherits(sf::st_geometry(data), c("sfc_POINT", "sfc_MULTIPOINT")))
#
#   # temp directories
#   dir_data = tempfile(pattern = "glify_points_dt")
#   dir.create(dir_data)
#   dir_color = tempfile(pattern = "glify_points_cl")
#   dir.create(dir_color)
#   dir_popup = tempfile(pattern = "glify_points_pop")
#   dir.create(dir_popup)
#
#   # data
#   data = sf::st_transform(data, 4326)
#   crds = sf::st_coordinates(data)[, c(2, 1)]
#
#   fl_data = paste0(dir_data, "/", group, "_data.json")
#   cat(jsonify::to_json(crds, digits = 7), file = fl_data, append = FALSE)
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
#   if (!is.null(popup)) {
#     pop = jsonify::to_json(data[[popup]])
#     fl_popup = paste0(dir_popup, "/", group, "_popup.json")
#     popup_var = paste0(group, "pop")
#     cat(pop, file = fl_popup, append = FALSE)
#   } else {
#     popup_var = NULL
#   }
#
#   # dependencies
#   map$dependencies = c(
#     map$dependencies,
#     glifyDependenciesFl(),
#     glifyDataAttachment(fl_data, group),
#     glifyColorAttachment(fl_color, group)
#   )
#
#   if (!is.null(popup)) {
#     map$dependencies = c(
#       map$dependencies,
#       glifyPopupAttachment(fl_popup, group)
#     )
#   }
#
#   leaflet::invokeMethod(map, leaflet::getMapData(map), 'addGlifyPointsFl',
#                         data_var, color_var, popup_var, opacity, weight, layerId)
#
# }
