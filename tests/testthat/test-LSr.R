test_that("output dimension (n)", {

  df = data.frame(y=c(1,2,3,4),x=c(4,5,6,7))
  m = LS.model(y~x,data=df)
  expect_equal(dim(as.matrix(df))[1], 4)
})

test_that("output dimension (d)", {

  df = data.frame(y=c(1,2,3,4),x=c(4,5,6,7))
  m = LS.model(y~x,data=df)
  expect_equal(dim(as.matrix(df))[2], 2)
})

