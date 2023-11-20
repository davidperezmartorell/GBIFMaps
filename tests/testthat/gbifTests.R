library("tidyverse")
library("testthat")
library("dplyr")
source("app.R")
source("R/downloadMap.R")
source("R/mapInLocal.R")
source("R/downloadRiverMap.R")
#source("R/footer.R")
source("R/downloadElevationsMap.R")




# TEST downloading local map
test_that("Test downloadMap function", {
  test_result <- downloadMap("ESP",1)
  expect_identical(test_result$COUNTRY[1], "Spain")
  expect_identical(test_result$GID_0[1] , "ESP")
  expect_identical(class(test_result) , c("sf","data.frame"))
})

# TEST test if map is in local
test_that("Test mapInLocal function", {
  test_result <- mapInLocal("ESP",1)
  expect_identical(test_result$COUNTRY[1], "Spain")
  expect_identical(test_result$GID_0[1] , "ESP")
  expect_identical(class(test_result) , c("sf","data.frame"))
  test_result <- mapInLocal("XYZ",1)
  expect_null(test_result, "Expected result to be NULL")
})

# TEST downloading local rivers map
test_that("Test downloadRiverMap function", {
  MapaBase <- downloadMap("ESP",1)
  test_result<-downloadRiverMap(MapaBase)
  expect_identical(test_result$featurecla[1], "River")
  expect_identical(test_result$min_zoom[1] , 7.1)
  expect_identical(test_result$min_label[1] , 8.1)
  expect_identical(class(test_result) , c("tbl_df", "tbl" ,"data.frame"))
})

# TEST downloading local rivers map
test_that("Test downloadElevationsMap function", {
  MapaBase <- downloadMap("ESP",1)
  test_result<-downloadElevationsMap(MapaBase, 1, "ESP")
  expect_identical(class(test_result), "data.frame")
  expect_true(!is.null(test_result$Altura) && any(test_result$x != 0))
  expect_true(!is.null(test_result$x) && any(test_result$x != 0))
  expect_true(!is.null(test_result$y) && any(test_result$x != 0))
})
source("R/downloadElevationsMap.R")
