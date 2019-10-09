test_that("output dimension (n)", {

  df = data.frame(y=c(1,2,3,4),x=c(4,5,6,7))
  m = LS.model(y~x,data=df)
  p = LS.predict(m)
  expect_equal(dim(as.matrix(p))[1], 4)
})

test_that("newdata output", {
  df = data.frame(y=c(1,2,3,4),x=c(4,5,6,7))
  m = LS.model(y~x,data=df)
  nd = c(4,2)
  p = LS.predict(m,newdata=nd)
  expect_equal(dim(as.matrix(p))[1],2)
})
