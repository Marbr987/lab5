context("get_coordinates")

test_that("get_coordinates rejects errounous input", {
  expect_error(get_coordinates(43.2))
  expect_error(get_coordinates(c("Berlin", "Stockholm")))
  expect_error(get_coordinates("lksfdj"))
})

test_that("get_coordinates returns correct type", {
  expect_true(length(get_coordinates("Stockholm")) == 2)
  expect_true(typeof(get_coordinates("Lund")) == "double")
})

test_that("get_coordinates returns correct values", {
  expect_equal(get_coordinates("Stockholm"), c(longitude=18.07109, latitude=59.32512))
  expect_equal(get_coordinates("Umea"), c(longitude=20.26307, latitude=63.82566 ))
})