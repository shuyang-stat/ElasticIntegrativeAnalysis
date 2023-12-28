test_that("`elasticHTE()` returns expected errors", {
    expect_error(elasticHTE(),
                 "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = matrix(1:10, 10, 3, dimnames = list(NULL, c("X", "Y", "A")))),
                 "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = list(matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                                            1:10, 1:10)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = list("X" = matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                                            1:10, 1:10)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = list("X" = matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                                            "Y" = 1:10, 1:10)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
    data.rct <- list("X" = matrix(1:10, 10, 4, dimnames = list(NULL, c("X1", "X2", "X3", "X4"))),
                     "Y" = 1:10, A = rep(0L, 10))

    expect_error(elasticHTE(data.rct = data.rct),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = data.rct,
                            data.rwe = matrix(1:10, 10, 3, dimnames = list(NULL, c("X", "Y", "A")))),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = data.rct,
                            data.rwe = list(matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                                            1:10, 1:10)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = data.rct,
                            data.rwe = list("X" = matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                                            1:10, 1:10)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
    expect_error(elasticHTE(data.rct = data.rct,
                            data.rwe = list("X" = matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                                            "Y" = 1:10, 1:10)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
    data.rwe <- list("X" = matrix(1:10, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                     "Y" = 1:10, A = rep(0L, 10))

    expect_error(elasticHTE(data.rct = list("X" = matrix(1:10, 10, 3),
                                            "Y" = 1:10, A = rep(0L, 10)),
                            data.rwe = data.rwe),
                 "`data.rwe$X` and `data.rct$X` must be matrices with column names", fixed = TRUE)
    expect_error(elasticHTE(data.rct = data.rct,
                            data.rwe = list("X" = matrix(1:10, 10, 3),
                                            "Y" = 1:10, A = rep(0L, 10))),
                 "`data.rwe$X` and `data.rct$X` must be matrices with column names", fixed = TRUE)

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = NA_character_),
                 "`mainName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = 2),
                 "`mainName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = c("x1", "X2", "X3")),
                 "`mainName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = c("X1", "X2", "X3", "X4")),
                 "`mainName` must be a character vector of X column headers")
    mainName <- c("X1", "X2", "X3")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = NA_character_),
                 "`contName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = 2),
                 "`contName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = c("x1", "X2", "X3")),
                 "`contName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = c("X1", "X2", "X3", "X4")),
                 "`contName` must be a character vector of X column headers")
    contName <- c("X1", "X2", "X3")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = NA_character_),
                 "`psName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = 2),
                 "`psName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = c("x1", "X2", "X3")),
                 "`psName` must be a character vector of X column headers")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = c("X1", "X2", "X3", "X4")),
                 "`psName` must be a character vector of X column headers")
    psName <- c("X1", "X2", "X3")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = NA_real_),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = "a"),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.0),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = -0.000001),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = c(0.1, 0.2)),
                 "`thres.psi` must be a positive scalar")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = NULL),
                 "`sieve.degree` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 0.0),
                 "`sieve.degree` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 1.1),
                 "`sieve.degree` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = c(1, 2)),
                 "`sieve.degree` must be a positive integer")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "Cont"))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = 1L))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "binary"))

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "GLM"))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "Sl"))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "sll"))

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = "a"),
                 "`outcome.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("gaussian")),
                 "`outcome.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian", TRUE)),
                 "`outcome.controls` must be a named list")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "GLM"))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "Sl"))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "sll"))

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = "a"),
                 "`ps.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("gaussian")),
                 "`ps.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian", TRUE)),
                 "`ps.controls` must be a named list")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = 0),
                 "`fixed` must be a logical")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = c(TRUE, FALSE)),
                 "`fixed` must be a logical")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = "a"),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = c(1, 2)),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 0),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = -1L),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 100.1),
                 "`n.pert` must be a positive integer")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = "a"),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = c(1, 2)),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 0),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = -1L),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 100.1),
                 "`n.boot` must be a positive integer")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = "a"),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = c(1, 2)),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = 0),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = -1L),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            mainName = mainName,
                            contName = contName,
                            psName = psName,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.method = "glm",
                            outcome.controls = list("family" = "gaussian"),
                            ps.method = "glm",
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = 100.1),
                 "`n.gamma` must be a positive integer")
})

test_that("`elasticHTE()` results in expected internal errors", {

  data.rct <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X 1", "X.1", "X3"))),
                   "Y" = 1:10, "A" = 1:10)
  data.rwe <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:10, "A" = 1:10)

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "not all model covariates are found in provided data")

  data.rwe <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X 1", "X.1", "X3"))),
                   "Y" = 1:10, "A" = 1:10)
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               paste("duplicate column headers found in X,",
                     "possibly due to required removal of spaces",
                     "please eliminate spaces from column header names in `data.rct$X`",
                     "and `data.rwe$X"), fixed = TRUE)

  data.rct <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:10, "A" = 1:10)
  data.rwe <- list("X" = matrix(1, 10L, 2L, dimnames = list(NULL, c("X1", "X2"))),
                   "Y" = 1:10, "A" = 1:10)

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "not all model covariates are found in provided data")
  data.rwe <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:10, "A" = 1:10)

  data.rct$X[1L, 1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$X[1L, 1L] <- 1

  data.rct$Y[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$Y[1L] <- 1

  data.rct$A[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$A[1L] <- 0L

  data.rwe$X[1L, 1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$X[1L, 1L] <- 1

  data.rwe$Y[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$Y[1L] <- 1

  data.rwe$A[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$A[1L] <- 0L

  data.rwe$ps <- rep(1, 10)
  expect_error(tryCatch(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
                        warning = function(e) {
                          expect_equal(e$message, "`ps` cannot be provided in `data.rwe`; input ignored")
                          data.rwe$ps <- NULL
                          tryCatch(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
                                   warning = function(e) {
                                      expect_equal(e$message,
                                                  paste("methods developed under the assumption that n >> m;",
                                                        "requested analysis has m/n = 1"), fixed = TRUE)
                                     suppressWarnings(elasticHTE(data.rct = data.rct, data.rwe = data.rwe))
                                     })
                          }),
           "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)
  data.rct$A <- rep(0L, 10L)

  data.rwe <- list("X" = matrix(1, 100, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = 1:100)
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)

  data.rwe <- list("X" = matrix(1, 100, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(0, 50), rep(1, 50)))

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "`data.rct$Y` is not binary",
               fixed = TRUE)

  data.rct <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = rep(0L, 10L), "A" = rep(0L, 10L))

  data.rwe <- list("X" = matrix(1, 100, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(2, 50), rep(3, 50)))

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)

  data.rwe <- list("X" = matrix(1, 100, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(0, 50), rep(1, 50)))

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "`data.rwe$Y` is not binary",
              fixed = TRUE)

  data.rct <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = c(rep(1,5), rep(0,5)), "A" = c(rep(0L, 5L), rep(1L, 5L)))

  data.rwe <- list("X" = matrix(1, 100, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(0, 50), rep(1, 50)))

  tryCatch(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont"),
           message = function(m) {
             expect_equal(m$message,
                          paste0("* * * * * * * * * * WARNING * * * * * * * * * *\n",
                                 "`outcome.type` = 'cont'; however, response provided in data.rct$Y has only 2 unique values\n"),
                          fixed = TRUE)
             NULL
           })

})


test_that("`.elasticHTE()` returns expected results", {

  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(300), 100, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(100)
    data.rct$A <- stats::rbinom(100, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(3000), 1000, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(1000)
    data.rwe$A <- stats::rbinom(1000, 1, 0.4)
  })

  withr::with_seed(2345L, {

    data.rct$est.ps <- TRUE
    data.rct$q <- rep(1.0, 100L)
    data.rwe$q <- rep(1.0, 1000L)
    n_rct <- 100L
    n_rwe <- 1000L
    thres.psi <- sqrt(log(1000))
    psi_list <- .psiEst(data.rwe = data.rwe,
                        data.rct = data.rct,
                        sieve.degree = 2L,
                        outcome.type = "cont",
                        mainName = c("X1", "X2", "X3"),
                        contName = c("X1", "X2", "X3"),
                        outcome.method = "glm",
                        outcome.controls = list("family" = "gaussian"),
                        psName = c("X1", "X2", "X3"),
                        ps.method = "glm",
                        ps.controls = list("family" = "quasibinomial"))

    perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                          data.rct = data.rct,
                                          n.pert = 100L,
                                          sieve.degree = 2L,
                                          outcome.type = "cont",
                                          mainName = c("X1", "X2", "X3"),
                                          contName = c("X1", "X2", "X3"),
                                          outcome.method = "glm",
                                          outcome.controls = list("family" = "gaussian"),
                                          psName = c("X1", "X2", "X3"),
                                          ps.method = "glm",
                                          ps.controls = list("family" = "quasibinomial"))

    Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                   V.eff = perm_result$V.eff,
                                                   rho = n_rct / n_rwe)

    sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

    Tstat <- .calculateTstat(eta = perm_result$eta,
                             inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

    mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
    mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

    nuispar <- .cGamma(mu1, mu2,
                       n.gamma = 1000L,
                       sqrt.V.rt_eff = sqrt_V_rt_eff,
                       sqrt.V.eff = perm_result$sqrt.V.eff,
                       Tstat1 = Tstat,
                       n.rwe = n_rwe,
                       fixed = FALSE)
    nuispar$eta <- perm_result$eta

    est_bias <- .bias(Icomb = nuispar$Icomb,
                      psi.rt = psi_list$psi["rt", ],
                      psi.eff = psi_list$psi["eff", ],
                      mu1 = mu1,
                      gamma = nuispar$gamma,
                      eta = nuispar$eta,
                      V.eff = perm_result$V.eff,
                      n.rwe = n_rwe)

    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias$elastic,
                     n.rwe = n_rwe, n.boot = 100L,
                     thres.psi = thres.psi,
                     Tstat = Tstat)

    psi <- rbind(psi_list$psi,
                 "elastic" = est_bias$elastic,
                 "elastic.debiased" = est_bias$elastic.debiased)
    ve <- rbind(perm_result$V.est,
                "elastic" = nuispar$V.elastic,
                "elastic.debiased" = nuispar$V.elastic)
    nuispar$V.elastic <- NULL

    obj <- c(list("call" = NA,
                  "psi" = psi, "ve" = ve,
                  "nuispar" = nuispar, "Tstat" = Tstat), cis)
    class(obj) <- c("elasticHTE", class(obj))

  })

  test_object <- withr::with_seed(2345L, elasticHTE(data.rct, data.rwe))
  test_object$call <- NA

  expect_equal(test_object, obj)

})


test_that("`.elasticHTE()` returns expected results; one covariate", {

  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(300), 100, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(100)
    data.rct$A <- stats::rbinom(100, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(3000), 1000, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(1000)
    data.rwe$A <- stats::rbinom(1000, 1, 0.4)
  })

  withr::with_seed(2345L, {

    data.rct$est.ps <- TRUE
    data.rct$q <- rep(1.0, 100L)
    data.rwe$q <- rep(1.0, 1000L)
    n_rct <- 100L
    n_rwe <- 1000L
    thres.psi <- sqrt(log(1000))
    psi_list <- .psiEst(data.rwe = data.rwe,
                        data.rct = data.rct,
                        sieve.degree = 2L,
                        outcome.type = "cont",
                        mainName = c("X1"),
                        contName = c("X2"),
                        outcome.method = "glm",
                        outcome.controls = list("family" = "gaussian"),
                        psName = c("X3"),
                        ps.method = "glm",
                        ps.controls = list("family" = "quasibinomial"))

    perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                          data.rct = data.rct,
                                          n.pert = 100L,
                                          sieve.degree = 2L,
                                          outcome.type = "cont",
                                          mainName = c("X1"),
                                          contName = c("X2"),
                                          outcome.method = "glm",
                                          outcome.controls = list("family" = "gaussian"),
                                          psName = c("X3"),
                                          ps.method = "glm",
                                          ps.controls = list("family" = "quasibinomial"))

    Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                   V.eff = perm_result$V.eff,
                                                   rho = n_rct / n_rwe)

    sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

    Tstat <- .calculateTstat(eta = perm_result$eta,
                             inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

    mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
    mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

    nuispar <- .cGamma(mu1, mu2,
                       n.gamma = 1000L,
                       sqrt.V.rt_eff = sqrt_V_rt_eff,
                       sqrt.V.eff = perm_result$sqrt.V.eff,
                       Tstat1 = Tstat,
                       n.rwe = n_rwe,
                       fixed = FALSE)
    nuispar$eta <- perm_result$eta

    est_bias <- .bias(Icomb = nuispar$Icomb,
                      psi.rt = psi_list$psi["rt", ],
                      psi.eff = psi_list$psi["eff", ],
                      mu1 = mu1,
                      gamma = nuispar$gamma,
                      eta = nuispar$eta,
                      V.eff = perm_result$V.eff,
                      n.rwe = n_rwe)

    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias$elastic,
                     n.rwe = n_rwe, n.boot = 100L,
                     thres.psi = thres.psi,
                     Tstat = Tstat)

    psi <- rbind(psi_list$psi,
                 "elastic" = est_bias$elastic,
                 "elastic.debiased" = est_bias$elastic.debiased)
    ve <- rbind(perm_result$V.est,
                "elastic" = nuispar$V.elastic,
                "elastic.debiased" = nuispar$V.elastic)
    nuispar$V.elastic <- NULL

    obj <- c(list("call" = NA,
                  "psi" = psi, "ve" = ve,
                  "nuispar" = nuispar, "Tstat" = Tstat), cis)
    class(obj) <- c("elasticHTE", class(obj))

  })

  test_object <- withr::with_seed(2345L,
                                  elasticHTE(data.rct, data.rwe,
                                             mainName = "X1",
                                             contName = "X2",
                                             psName = "X3"))
  test_object$call <- NA

  expect_equal(test_object, obj)

})


test_that("`.elasticHTE()` returns expected results; no covariate", {

  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(300), 100, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(100)
    data.rct$A <- stats::rbinom(100, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(3000), 1000, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(1000)
    data.rwe$A <- stats::rbinom(1000, 1, 0.4)
  })

  withr::with_seed(2345L, {

    data.rct$est.ps <- TRUE
    data.rct$q <- rep(1.0, 100L)
    data.rwe$q <- rep(1.0, 1000L)
    n_rct <- 100L
    n_rwe <- 1000L
    thres.psi <- sqrt(log(1000))
    psi_list <- .psiEst(data.rwe = data.rwe,
                        data.rct = data.rct,
                        sieve.degree = 2L,
                        outcome.type = "cont",
                        mainName = NULL,
                        contName = NULL,
                        outcome.method = "glm",
                        outcome.controls = list("family" = "gaussian"),
                        psName = NULL,
                        ps.method = "glm",
                        ps.controls = list("family" = "quasibinomial"))

    perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                          data.rct = data.rct,
                                          n.pert = 100L,
                                          sieve.degree = 2L,
                                          outcome.type = "cont",
                                          mainName = NULL,
                                          contName = NULL,
                                          outcome.method = "glm",
                                          outcome.controls = list("family" = "gaussian"),
                                          psName = NULL,
                                          ps.method = "glm",
                                          ps.controls = list("family" = "quasibinomial"))

    Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                   V.eff = perm_result$V.eff,
                                                   rho = n_rct / n_rwe)

    sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

    Tstat <- .calculateTstat(eta = perm_result$eta,
                             inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

    mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
    mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

    nuispar <- .cGamma(mu1, mu2,
                       n.gamma = 1000L,
                       sqrt.V.rt_eff = sqrt_V_rt_eff,
                       sqrt.V.eff = perm_result$sqrt.V.eff,
                       Tstat1 = Tstat,
                       n.rwe = n_rwe,
                       fixed = FALSE)
    nuispar$eta <- perm_result$eta

    est_bias <- .bias(Icomb = nuispar$Icomb,
                      psi.rt = psi_list$psi["rt", ],
                      psi.eff = psi_list$psi["eff", ],
                      mu1 = mu1,
                      gamma = nuispar$gamma,
                      eta = nuispar$eta,
                      V.eff = perm_result$V.eff,
                      n.rwe = n_rwe)

    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias$elastic,
                     n.rwe = n_rwe, n.boot = 100L,
                     thres.psi = thres.psi,
                     Tstat = Tstat)

    psi <- rbind(psi_list$psi,
                 "elastic" = est_bias$elastic,
                 "elastic.debiased" = est_bias$elastic.debiased)
    ve <- rbind(perm_result$V.est,
                "elastic" = nuispar$V.elastic,
                "elastic.debiased" = nuispar$V.elastic)
    nuispar$V.elastic <- NULL

    obj <- c(list("call" = NA,
                  "psi" = psi, "ve" = ve,
                  "nuispar" = nuispar, "Tstat" = Tstat), cis)
    class(obj) <- c("elasticHTE", class(obj))

  })

  test_object <- withr::with_seed(2345L,
                                  elasticHTE(data.rct, data.rwe,
                                             mainName = 1,
                                             contName = 1,
                                             psName = 1))
  test_object$call <- NA

  expect_equal(test_object, obj)

  data.rct$X <- matrix(NA, 100, 0L)
  data.rwe$X <- matrix(NA, 1000, 0L)

  test_object <- withr::with_seed(2345L,
                                  elasticHTE(data.rct, data.rwe,
                                             mainName = 1,
                                             contName = 1,
                                             psName = 1))
  test_object$call <- NA

  expect_equal(test_object, obj)


})
