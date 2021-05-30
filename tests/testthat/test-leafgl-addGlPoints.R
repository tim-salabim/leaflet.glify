context("test-leafgl-addGlPoints")

test_that("addGlPoints works", {
  library(leaflet)
  library(sf)

  n = 1e3
  df1 = data.frame(id = 1:n,
                   id2 = n:1,
                   x = rnorm(n, 10, 1),
                   y = rnorm(n, 49, 0.8))
  pts = st_as_sf(df1, coords = c("x", "y"), crs = 4326)

  m = leaflet() %>%
    addGlPoints(data = pts, group = "pts", digits = 5)

  expect_is(m, "leaflet")

  m = leaflet() %>%
    addGlPoints(data = pts, layerId = "someid", src = TRUE)
  expect_is(m, "leaflet")
  expect_identical(m$dependencies[[length(m$dependencies)]]$name, "glpointsdat")

  m = leaflet() %>%
    addGlPoints(data = pts, group = NULL)
  expect_is(m, "leaflet")

  m = leaflet() %>%
    addGlPoints(data = pts, layerId = NULL, group = NULL, src = TRUE)
  expect_is(m, "leaflet")
  expect_identical(m$dependencies[[length(m$dependencies)]]$name, "datadat")

  m = leaflet() %>%
    addGlPoints(data = breweries91, src = TRUE)
  expect_is(m, "leaflet")
  expect_identical(m$dependencies[[length(m$dependencies)]]$name, "glpointsdat")

  m = leaflet() %>%
    addGlPoints(data = breweries91, src = TRUE, radius = 5)
  expect_is(m, "leaflet")

  m = leaflet() %>%
    addGlPoints(data = breweries91, src = TRUE, radius = runif(nrow(breweries91), 1, 10))
  expect_is(m, "leaflet")
})
