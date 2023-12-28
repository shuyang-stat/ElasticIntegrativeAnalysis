test_that("`.fixA()` returns expected errors", {

  expect_error(.fixA(), "`A` must be a vector or factor")
  expect_error(.fixA(A = numeric(0)), "`A` must be a vector or factor")
  expect_error(.fixA(A = list(1, 2, 1, 2)), "`A` must be a vector or factor")

  expect_error(.fixA(A = c(1, 2, 1, 2, 1, 2)),
               "`obj.name` must be a character")
  expect_error(.fixA(A = c(1, 2, 1, 2, 1, 2), obj.name = 1),
               "`obj.name` must be a character")
  expect_error(.fixA(A = c(1, 2, 1, 2, 1, 2), obj.name = list("a", "b")),
               "`obj.name` must be a character")
  expect_error(.fixA(A = c(1, 2, 1, 2, 1, 2), obj.name = c("a", "b")),
               "`obj.name` must be a character")

  expect_error(.fixA(A = c(1, 2, 3, 1, 2, 3), obj.name = "test"),
               "`orig.levels` must be a vector of length 2")

  expect_error(.fixA(A = c(1, 2, 3, 1, 2, 3), obj.name = "test",
                     orig.levels = 1L:2L),
               "`test` must be binary")
})

test_that("`.fixA()` returns expected results", {

  # ensure that vectors of 0/1 are not changed
  A <- rep(1, 10L)
  expected <- rep(1L, 10L)
  expect_equal(.fixA(A, "test", orig.levels = c("0", "1")), expected)
  expect_equal(.fixA(A, "test", orig.levels = 0L:1L), expected)
  expect_equal(.fixA(A, "test", orig.levels = c(0.0, 1.0)), expected)

  A <- rep(0, 10L)
  expected <- rep(0L, 10L)
  expect_equal(.fixA(A, "test", orig.levels = c("0", "1")), expected)
  expect_equal(.fixA(A, "test", orig.levels = 0L:1L), expected)
  expect_equal(.fixA(A, "test", orig.levels = c(0.0, 1.0)), expected)

  A <- rep("a", 10L)
  expected <- rep(0L, 10L)
  expect_equal(.fixA(A, "test", orig.levels = c("a", "b")), expected)

  A <- rep("a", 10L)
  expected <- rep(1L, 10L)
  expect_equal(.fixA(A, "test", orig.levels = c("b", "a")), expected)

  A <- c(rep("a", 10L), rep("b", 10L))
  expected <- c(rep(0L, 10L), rep(1L, 10L))
  expect_equal(.fixA(A, "test", orig.levels = c("a", "b")), expected)

  A <- c(rep("a", 10L), rep("b", 10L))
  expected <- c(rep(1L, 10L), rep(0L, 10L))
  expect_equal(.fixA(A, "test", orig.levels = c("b", "a")), expected)

  A <- factor(rep("a", 10L), levels = c("a", "b"))
  expected <- rep(0L, 10L)
  expect_equal(.fixA(A, "test", orig.levels = c("a", "b")), expected)

  A <- factor(rep("a", 10L), levels = c("b", "a"))
  expected <- rep(1L, 10L)
  expect_equal(.fixA(A, "test", orig.levels = c("b", "a")), expected)

  A <- factor(c(rep("a", 10L), rep("b", 10L)), levels = c("a", "b"))
  expected <- c(rep(0L, 10L), rep(1L, 10L))
  expect_equal(.fixA(A, "test", orig.levels = c("a", "b")), expected)

  A <- factor(c(rep("a", 10L), rep("b", 10L)), levels = c("b", "a"))
  expected <- c(rep(1L, 10L), rep(0L, 10L))
  expect_equal(.fixA(A, "test", orig.levels = c("b", "a")), expected)
})

test_that("`.fixY()` returns expected errors", {

  expect_error(.fixY(), "`Y` must be a vector or factor")
  expect_error(.fixY(Y = numeric(0)), "`Y` must be a vector or factor")
  expect_error(.fixY(Y = list(1, 2, 1, 2)), "`Y` must be a vector or factor")

  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2)),
               "`outcome.type` must be one of {'cont', 'bin'}", fixed = TRUE)
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = 1),
               "`outcome.type` must be one of {'cont', 'bin'}", fixed = TRUE)
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = c("cont", "bin")),
               "`outcome.type` must be one of {'cont', 'bin'}", fixed = TRUE)
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = "Cont"),
               "`outcome.type` must be one of {'cont', 'bin'}", fixed = TRUE)
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = "binary"),
               "`outcome.type` must be one of {'cont', 'bin'}", fixed = TRUE)

  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = "bin"),
               "`obj.name` must be a character")
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = "bin",
                     obj.name = 1),
               "`obj.name` must be a character")
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = "bin",
                     obj.name = list("a", "b")),
               "`obj.name` must be a character")
  expect_error(.fixY(Y = c(1, 2, 1, 2, 1, 2), outcome.type = "bin",
                     obj.name = c("a", "b")),
               "`obj.name` must be a character")

  expect_error(.fixY(Y = c(1, 2, 3, 1, 2, 3), outcome.type = "bin", obj.name = "test"),
               "`test` is not binary")
  expect_error(.fixY(Y = c("a", "b", "c", "a", "b"), outcome.type = "cont", obj.name = "test"),
               "`test` must be numeric for `outcome.type` = 'cont'")

})

test_that("`fixY()` returns expected results", {
  Y <- 1:10
  expect_equal(.fixY(Y = Y, outcome.type = "cont", obj.name = "test"), Y)

  Y <- c("a", "b", "a", "b")
  expect_equal(.fixY(Y = Y, outcome.type = "bin", obj.name = "test"), c(0L, 1L, 0L, 1L))

  Y <- c(1:2, 1:2)
  expect_message(.fixY(Y = Y, outcome.type = "cont", obj.name = "test"),
                 paste0("* * * * * * * * * * WARNING * * * * * * * * * *\n",
                       "`outcome.type` = 'cont'; however, response provided in ",
                       "test has only 2 unique values"), fixed = TRUE)
})

test_that("`.fixNames()", {
  expect_error(.fixNames(),
               "`nms` must be NULL or character")
  expect_error(.fixNames(nms = 1),
               "`nms` must be NULL or character")
  expect_error(.fixNames(nms = NA),
               "`nms` must be NULL or character")

  expect_equal(.fixNames(nms = "test that"), "test.that")
  expect_equal(.fixNames(nms = c("test that", "test this")),
               c("test.that", "test.this"))
  expect_equal(.fixNames(nms = c("test.that", "test.this")),
               c("test.that", "test.this"))
  expect_equal(.fixNames(nms = c("test_that", "test_this")),
               c("test_that", "test_this"))
  expect_equal(.fixNames(nms = "test_that"),
               "test_that")
})

test_that("`.adjustModelCoding()`", {

  expect_error(.adjustModelCoding(c("X1", "X2", "X3"), c("x1", "X2", "X3")),
               "unrecognized model covariate provided")

  expect_equal(.adjustModelCoding(c("X1", "X2", "X3"), c("X1", "X2", "X3")),
               c("X1", "X2", "X3"))
  expect_equal(.adjustModelCoding(c("X1", "X2"), c("X1", "X2", "X3")),
               c("X1", "X2"))
  expect_equal(.adjustModelCoding(NULL, c("X1", "X2", "X3")),
               c("X1", "X2", "X3"))
  expect_true(is.null(.adjustModelCoding(1, c("X1", "X2", "X3"))))
})
