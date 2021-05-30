context("test-leafgl-popup")

library(leaflet)
library(jsonify)
library(sf)

## POINTS #################################
test_that("popup-points-character", {
  ## Column Name Single ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = "state",
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), breweries91$state)
  rm(m)

  sfpoints <- sf::st_as_sf(breweries91)
  m <- leaflet() %>%
    addGlPoints(data = st_sfc(st_geometry(sfpoints)),
                popup = sfpoints$state,
                group = "grp")
  expect_is(m, "leaflet")
  expect_is(m$x$calls[[1]]$args[[2]], "json")
  expect_true(validate_json(m$x$calls[[1]]$args[[2]]))
  rm(m)

  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = breweries91$state,
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), breweries91$state)
  rm(m)

  ## Column Names ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = c("state", "address"),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  # expect_identical(from_json(m$x$calls[[2]]$args[[3]]), breweries91$state)
  rm(m)

  ## Single Random Character ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = "Text 1",
                group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), rep("Text 1", nrow(breweries91)))
  rm(m)

  ## Multiple Random Characters - (wrong length) ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPoints(data = breweries91,
                                    popup = c("Text 1", "Text 2"),
                                    group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]),
                   rep(c("Text 1","Text 2"), nrow(breweries91))[1:nrow(breweries91)])
  rm(m)

  ## Multiple Random Character (same length) ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = rep("Text 1", nrow(breweries91)),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), rep("Text 1", nrow(breweries91)))
})

test_that("popup-points-table", {
  ## Data.frame ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = as.data.frame(breweries91),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = as.data.frame(breweries91),
                group = "grp",
                src = TRUE)
  expect_is(m, "leaflet")

  ## Data.frame - wrong length ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = as.data.frame(breweries91)[1:4,],
                group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Matrix ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = as.matrix(as.data.frame(breweries91)),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-points-spatial", {
  ## SpatialPointsDataFrame ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = breweries91,
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = breweries91,
                group = "grp",
                src = TRUE)
  expect_is(m, "leaflet")

  ## Simple Feature ##############
  library(sf)
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = st_as_sf(breweries91),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
})

test_that("popup-points-formula", {
  ## Formula ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = ~sprintf("<b>State</b>: %s<br>
                                 <b>Address</b>: %s<br>
                                 <b>Brauerei</b>: %s,",
                                 state, address, brewery),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = ~sprintf("<b>State</b>: %s<br>
                                 <b>Address</b>: %s<br>
                                 <b>Brauerei</b>: %s,",
                                 state, address, brewery),
                group = "grp",
                src = TRUE)
  expect_is(m, "leaflet")
})

test_that("popup-points-list", {
  ## List with length 1 ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = as.list(data.frame(city="Berlin",
                                           district=5029, stringsAsFactors = F)),
                group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## List with length >1 ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
        addGlPoints(data = breweries91,
                    popup=as.list(data.frame(city=c("Vienna","Berlin"),
                                             district=c(1010,40302),
                                             stringsAsFactors = F)),
                    group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))


  ## List with same length ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = as.list(as.data.frame(breweries91)),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-points-json", {
  ## JSON - 1 column ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = jsonify::to_json(as.list(as.data.frame(breweries91[,1]))),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = jsonify::to_json(as.data.frame(breweries91[,1])),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## JSON - multiple columns ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = jsonify::to_json(as.list(as.data.frame(breweries91[,1:5]))),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- expect_warning(leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = jsonify::to_json(as.data.frame(breweries91[1,])),
                group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## JSON with same length ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = jsonify::to_json(as.list(as.data.frame(breweries91))),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-points-logical", {
  ## TRUE ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = TRUE,
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = TRUE,
                group = "grp",
                src = TRUE)
  expect_is(m, "leaflet")

  ## FALSE #################
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = FALSE,
                group = "grp")
  expect_is(m, "leaflet")
  expect_null(m$x$calls[[2]]$args[[3]])

  ## NULL #################
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = NULL,
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(is.null(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-points-shiny.tag", {
  library(shiny)
  ## Shiny.Tag - icon - Length 1 ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = shiny::icon("car"),
                group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  m <- expect_warning(leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = shiny::icon("car"),
                group = "grp", src = TRUE))
  expect_is(m, "leaflet")

  ## Shiny.Tag - icon - Length >1 ##############
  # m <- expect_warning(leaflet() %>% addTiles() %>%
  #                       addGlPoints(data = breweries91,
  #                                   popup = c(
  #                                     shiny::icon("car"),
  #                                     shiny::icon("wrench")
  #                                   ),
  #                                   group = "grp"))
  # expect_is(m, "leaflet")
  # expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-points-default", {
  ## POSIX* ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPoints(data = breweries91,
                                    popup = Sys.time(),
                                    group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## POSIX* - same length##############
  m <- leaflet() %>% addTiles() %>%
    addGlPoints(data = breweries91,
                popup = seq.POSIXt(Sys.time(), Sys.time()+1000,
                                   length.out = nrow(breweries91)),
                group = "grp")
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Date ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPoints(data = breweries91,
                                    popup = Sys.Date(),
                                    group = "grp"))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
})

## LINES #################################
storms = suppressWarnings(st_cast(st_as_sf(atlStorms2005), "LINESTRING"))
storms = st_transform(storms, 4326)

## TODO - not working in RStudio Pane /In Zomm Mode it works / Shiny?
test_that("popup-lines-character", {
  ## Column Name Single ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = "Name",
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), as.character(storms$Name))
  rm(m)

  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = storms$Name,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), as.character(storms$Name))
  rm(m)


  ## Column Names ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = c("Name", "MaxWind"),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  rm(m)

  ## Single Random Character ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPolylines(data = storms,
                                       popup = "Text 1",
                                       opacity = 1))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), rep("Text 1", nrow(storms)))
  rm(m)

  ## Multiple Random Characters - (wrong length) ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPolylines(data = storms,
                                    popup = c("Text 1", "Text 2"),
                                    opacity = 1))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]),
                   rep(c("Text 1","Text 2"), nrow(storms))[1:nrow(storms)])
  rm(m)

  ## Multiple Random Character (same length) ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = rep("Text 1", nrow(storms)),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), rep("Text 1", nrow(storms)))
})

test_that("popup-lines-table", {
  ## Data.frame ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = as.data.frame(storms),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Data.frame - wrong length ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPolylines(data = storms,
                                       popup = as.data.frame(storms)[1:4,],
                                       opacity = 1))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Matrix ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = as.matrix(as.data.frame(storms)),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-lines-spatial", {
  popups <- suppressWarnings(sf::as_Spatial(storms))
  ## SpatialLinesDataFrame ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = popups,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Simple Feature ##############
  library(sf)
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = storms,
                   opacity = 1)

  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
})

test_that("popup-lines-formula", {
  ## Formula ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = ~sprintf("<b>State</b>: %s<br>
                                 <b>Address</b>: %s<br>
                                 <b>Brauerei</b>: %s,",
                                    MinPress, MaxWind, Name),
                   opacity = 1)

  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
})

test_that("popup-lines-logical", {
  ## TRUE ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = TRUE,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(m$x$calls[[2]]$args[[3]])

  ## FALSE #################
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = FALSE,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_false(m$x$calls[[2]]$args[[3]])

  ## NULL #################
  m <- leaflet() %>% addTiles() %>%
    addGlPolylines(data = storms,
                   popup = NULL,
                   opacity = 1)

  expect_is(m, "leaflet")
  expect_true(is.null(m$x$calls[[2]]$args[[3]]))
})

## POLYGONS #################################
gadm = suppressWarnings(st_cast(st_as_sf(gadmCHE), "POLYGON"))

test_that("popup-polygon-character", {
  ## Column Name Single ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                  popup = "HASC_1",
                  opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), gadm$HASC_1)
  rm(m)

  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                  popup = gadm$HASC_1,
                  opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), gadm$HASC_1)
  rm(m)


  ## Column Names ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = c("HASC_1", "NAME_0"),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  rm(m)

  ## Single Random Character ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPolygons(data = gadm,
                                       popup = "Text 1",
                                       opacity = 1))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), rep("Text 1", nrow(gadm)))
  rm(m)

  ## Multiple Random Characters - (wrong length) ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPolygons(data = gadm,
                                       popup = c("Text 1", "Text 2"),
                                       opacity = 1))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]),
                   rep(c("Text 1","Text 2"), nrow(gadm))[1:nrow(gadm)])
  rm(m)

  ## Multiple Random Character (same length) ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = rep("Text 1", nrow(gadm)),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
  expect_identical(from_json(m$x$calls[[2]]$args[[3]]), rep("Text 1", nrow(gadm)))
})

test_that("popup-polygon-table", {
  ## Data.frame ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = as.data.frame(gadm),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Data.frame - wrong length ##############
  m <- expect_warning(leaflet() %>% addTiles() %>%
                        addGlPolygons(data = gadm,
                                       popup = as.data.frame(gadm)[1:4,],
                                       opacity = 1))
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Matrix ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = as.matrix(as.data.frame(gadm)),
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

})

test_that("popup-polygon-spatial", {
  ## SpatialPolygonsDataFrame ##############
  popups <- suppressWarnings(sf::as_Spatial(gadm))
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = popups,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))

  ## Simple Feature ##############
  library(sf)
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = gadm,
                   opacity = 1)

  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
})

test_that("popup-polygon-formula", {
  ## Formula ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = ~sprintf("<b>State</b>: %s<br>
                                 <b>Address</b>: %s<br>
                                 <b>Brauerei</b>: %s,",
                                    NAME_0, NAME_1, HASC_1),
                   opacity = 1)

  expect_is(m, "leaflet")
  expect_true(jsonify::validate_json(m$x$calls[[2]]$args[[3]]))
})

test_that("popup-polygon-logical", {
  ## TRUE ##############
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = TRUE,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(m$x$calls[[2]]$args[[3]])

  ## FALSE #################
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = FALSE,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_false(m$x$calls[[2]]$args[[3]])

  ## NULL #################
  m <- leaflet() %>% addTiles() %>%
    addGlPolygons(data = gadm,
                   popup = NULL,
                   opacity = 1)
  expect_is(m, "leaflet")
  expect_true(is.null(m$x$calls[[2]]$args[[3]]))
})


