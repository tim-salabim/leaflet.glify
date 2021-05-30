# helpers
glifyDependencies = function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.glify",
      '3.0.1',
      system.file("htmlwidgets/Leaflet.glify", package = "leafgl"),
      script = c(
        "GlifyUtils.js"
        , "addGlifyPoints.js"
        , "addGlifyPolygons.js"
        , "addGlifyPolylines.js"
        , "glify-browser.js"
        # , "glify-browser.js.map"
      )
    )
  )
}

# helpers
glifyDependenciesFl = function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.glify",
      '2.2.0',
      system.file("htmlwidgets/Leaflet.glify", package = "leafgl"),
      script = c(
        "GlifyUtils.js"
        , "addGlifyPoints.js"
        , "addGlifyPolygonsFl.js"
        , "addGlifyPolylines.js"
        , "glify.js"
      )
    )
  )
}

# helpers
glifyDependenciesSrc = function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.glify",
      '2.2.0',
      system.file("htmlwidgets/Leaflet.glify", package = "leafgl"),
      script = c(
        "GlifyUtils.js"
        , "addGlifyPointsSrc.js"
        , "addGlifyPolygonsSrc.js"
        , "addGlifyPolylinesSrc.js"
        , "glify-browser.js"
      )
    )
  )
}

glifyDataAttachmentSrc = function(fl_data, group, async = FALSE) {
  data_dir <- dirname(fl_data)
  data_file <- basename(fl_data)
  if (async) {
    list(
      htmltools::htmlDependency(
        name = paste0("pts_2", "dat"),
        version = 1,
        src = "",
        head = paste0('<script src="lib/', paste0(group, 'dat-1/'), data_file, '" async></script>')
      )
    )
  } else {
    list(
      htmltools::htmlDependency(
        name = paste0(group, "dat"),
        version = 1,
        src = c(file = data_dir),
        script = list(data_file)
      )
    )
  }
}

glifyColorAttachmentSrc = function(fl_color, group) {
  data_dir <- dirname(fl_color)
  data_file <- basename(fl_color)
  list(
    htmltools::htmlDependency(
      name = paste0(group, "col"),
      version = 1,
      src = c(file = data_dir),
      script = list(data_file)
    )
  )
}

glifyPopupAttachmentSrc = function(fl_popup, group) {
  data_dir <- dirname(fl_popup)
  data_file <- basename(fl_popup)
  list(
    htmltools::htmlDependency(
      name = paste0(group, "pop"),
      version = 1,
      src = c(file = data_dir),
      script = list(data_file)
    )
  )
}

glifyRadiusAttachmentSrc = function(fl_radius, group) {
  data_dir <- dirname(fl_radius)
  data_file <- basename(fl_radius)
  list(
    htmltools::htmlDependency(
      name = paste0(group, "rad"),
      version = 1,
      src = c(file = data_dir),
      script = list(data_file)
    )
  )
}

glifyDataAttachment = function(fl_data, group) {
  data_dir <- dirname(fl_data)
  data_file <- basename(fl_data)
  list(
    htmltools::htmlDependency(
      name = paste0(group, "dt"),
      version = 1,
      src = c(file = data_dir),
      attachment = list(data_file)
    )
  )
}


glifyColorAttachment = function(fl_color, group) {
  data_dir <- dirname(fl_color)
  data_file <- basename(fl_color)
  list(
    htmltools::htmlDependency(
      name = paste0(group, "cl"),
      version = 1,
      src = c(file = data_dir),
      attachment = list(data_file)
    )
  )
}

glifyPopupAttachment = function(fl_popup, group) {
  data_dir <- dirname(fl_popup)
  data_file <- basename(fl_popup)
  list(
    htmltools::htmlDependency(
      name = paste0(group, "pop"),
      version = 1,
      src = c(file = data_dir),
      attachment = list(data_file)
    )
  )
}
