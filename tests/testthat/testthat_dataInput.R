test_that("`.extractResponseVariable()` returns expected errors", {
  expect_error(.extractResponseVariable(),
               "`formula` must be a formula of the form LHS ~ RHS")
  expect_error(.extractResponseVariable(formula = "y ~ 1"),
               "`formula` must be a formula of the form LHS ~ RHS")
  expect_error(.extractResponseVariable(formula =  ~ 1),
               "`formula` must be a formula of the form LHS ~ RHS")

  df <- data.frame("y" = 1L:10L, "x" = 1L:10L)
  expect_error(.extractResponseVariable(formula = y ~ x),
               "`data` must be a data.frame")
  expect_error(.extractResponseVariable(formula = y ~ x, data = data.matrix(df)),
               "`data` must be a data.frame")
  expect_error(.extractResponseVariable(formula = y ~ x, data = df$y),
               "`data` must be a data.frame")

  expect_error(.extractResponseVariable(formula = y ~ x1, data = df),
               "unable to extract response variable\n\t", fixed = TRUE)
})

test_that("`.extractResponseVariable()` returns expected results", {
  df <- data.frame("y" = 1L:10L, "x" = 1L:10L)
  expect_equal(.extractResponseVariable(formula = y ~ x, data = df) |> unname(),
               df$y)
  expect_equal(.extractResponseVariable(formula = x ~ y, data = df) |> unname(),
               df$x)
})

test_that("`.separateOutcomeModel()` returns expected errors", {
  expect_error(.separateOutcomeModel(),
               "`outcome.model` must be a formula of the form LHS ~ RHS")
  expect_error(.separateOutcomeModel(outcome.model = "y ~ 1"),
               "`outcome.model` must be a formula of the form LHS ~ RHS")
  expect_error(.separateOutcomeModel(outcome.model =  ~ 1),
               "`outcome.model` must be a formula of the form LHS ~ RHS")

  expect_error(.separateOutcomeModel(outcome.model = y ~ x),
               "`ps.model` must be a formula of the form LHS ~ RHS")
  expect_error(.separateOutcomeModel(outcome.model = y ~ x, ps.model = "y ~ 1"),
               "`ps.model` must be a formula of the form LHS ~ RHS")
  expect_error(.separateOutcomeModel(outcome.model = y ~ x, ps.model =  ~ 1),
               "`ps.model` must be a formula of the form LHS ~ RHS")
})

test_that("`.separateOutcomeModel() returns expected results", {
  expect_equal(.separateOutcomeModel(outcome.model = y ~ x*A, ps.model = A ~ x),
               list(ME = ~ 1 + x, CO = ~ 1 + x))
  expect_equal(.separateOutcomeModel(outcome.model = y ~ x+A, ps.model = A ~ x),
               list(ME = ~ 1 + x, CO = ~ 1))
  expect_warning(out <- .separateOutcomeModel(outcome.model = y ~ x, ps.model = A ~ x))
  expect_equal(out, list(ME = ~ 1 + x, CO = ~ 1))
})

test_that("`.isDI() is working", {
  expect_error(.isDI(),
               "`object` must be provided")
  expect_error(.isDI(object = matrix(1:10, 10, 6, dimnames = list(NULL, c("X", "Y", "A", "mainName", "contName", "psName")))),
               "`object` must be a named list containing elements X, Y, A, mainName, contName, psName")
  expect_error(.isDI(object = data.frame("X" = 1, "Y" = 1, "A" = 1, "mainName" = 1, "contName" = 1, "psName" = 1)),
               "`object` must be a named list containing elements X, Y, A, mainName, contName, psName")

  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = 1L,
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$X` must be a matrix with column names", fixed = TRUE)

  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = NULL,
                                   "contName" = 1L,
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$mainName` must be 1L or a character vector of X column headers",
               fixed = TRUE)
  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 2L,
                                   "contName" = 1L,
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$mainName` must be 1L or a character vector of X column headers",
               fixed = TRUE)
  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = c("x1", "X2"),
                                   "contName" = 1L,
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$mainName` must be 1L or a character vector of X column headers",
               fixed = TRUE)

  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = NULL,
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$contName` must be 1L or a character vector of X column headers",
               fixed = TRUE)
  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = 2L,
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$contName` must be 1L or a character vector of X column headers",
               fixed = TRUE)
  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = c("x1", "X2"),
                                   "psName" = 1L),
                     object.name = "data.rwe"),
               "`data.rwe$contName` must be 1L or a character vector of X column headers",
               fixed = TRUE)

  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = 1L,
                                   "psName" = NULL),
                     object.name = "data.rwe"),
               "`data.rwe$psName` must be 1L or a character vector of X column headers",
               fixed = TRUE)
  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = 1L,
                                   "psName" = 2L),
                     object.name = "data.rwe"),
               "`data.rwe$psName` must be 1L or a character vector of X column headers",
               fixed = TRUE)
  expect_error(.isDI(object = list("X" = matrix(1:10, 10, 3,
                                                dimnames = list(NULL, c("X1", "X2", "X3"))),
                                   "Y" = 1:10,
                                   "A" = rep(0L, 10),
                                   "mainName" = 1L,
                                   "contName" = 1L,
                                   "psName" = c("x1", "x2")),
                     object.name = "data.rwe"),
               "`data.rwe$psName` must be 1L or a character vector of X column headers",
               fixed = TRUE)

})
