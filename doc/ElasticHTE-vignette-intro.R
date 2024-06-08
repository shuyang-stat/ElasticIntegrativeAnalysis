## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment=NA)
opt <- options()
options(continue="  ", width=70, prompt=" ")
on.exit(options(opt))
library(ElasticIntegrative, quietly = TRUE)
set.seed(23456L)

## ----eval = FALSE-------------------------------------------------------------
#  dataInput(data, outcome.model, ps.model)

## ----load_cont_data-----------------------------------------------------------
data("elasticToy.cont", package = "ElasticIntegrative")

## ----summary_cont_rwe---------------------------------------------------------
summary(elasticToy.cont.rwe)

## ----summary_cont_rct---------------------------------------------------------
summary(elasticToy.cont.rct)

## ----create_data_rct----------------------------------------------------------
data_rct <- dataInput(data = elasticToy.cont.rct,
                      outcome.model = Y ~ X1 + X2*A,
                      ps.model = A ~ X1*X2)

## ----test1--------------------------------------------------------------------
isTRUE(all.equal(data_rct$Y, elasticToy.cont.rct$Y, 
                 check.attributes = FALSE))

## ----test2--------------------------------------------------------------------
all(c("X1", "X2") %in% data_rct$mainName)

## ----test3--------------------------------------------------------------------
all(c("X2") %in% data_rct$contName)

## ----test4--------------------------------------------------------------------
isTRUE(all.equal(data_rct$A, elasticToy.cont.rct$A, 
                 check.attributes = FALSE))

## ----test5--------------------------------------------------------------------
all(c("X1", "X2", "X1:X2") %in% data_rct$psName)

## ----test6--------------------------------------------------------------------
model_cov <- c(data_rct$mainName, 
               data_rct$contName, 
               data_rct$psName)
all(model_cov %in% colnames(data_rct$X))

## ----create_data_rwe----------------------------------------------------------
data_rwe <- dataInput(data = elasticToy.cont.rwe,
                      outcome.model = Y ~ X2*A,
                      ps.model = A ~ X1)

## ----run1, eval = FALSE-------------------------------------------------------
#  elasticHTE(data.rct, data.rwe, ...,
#             outcome.type = c("cont", "bin"),
#             ps.rct = NULL,
#             sieve.degree = 2L,
#             outcome.method = c("glm", "SL"),
#             outcome.controls = list("family" = "gaussian"),
#             ps.method = c("glm", "SL"),
#             ps.controls = list("family" = "quasibinomial"),
#             n.pert = 100L,
#             fixed = FALSE,
#             n.gamma = 1000L,
#             n.boot = 100L,
#             thres.psi = NULL)

## ----result1------------------------------------------------------------------
result1 <- withr::with_seed(1234L,
                            elasticHTE(data_rct, data_rwe))
result1

## ----print_class_result1------------------------------------------------------
print(is(result1))

## ----print_psi_result1--------------------------------------------------------
result1$psi

## ----print_variance-----------------------------------------------------------
result1$ve

## ----print_nuisance-----------------------------------------------------------
result1$nuispar

## ----print_test---------------------------------------------------------------
result1$Tstat

## ----print_CIs----------------------------------------------------------------
result1$CIs.inf
result1$CIs.sup

## ----print_conservative-------------------------------------------------------
result1$conservative

## ----print_settings-----------------------------------------------------------
result1$CI.settings

## ----run_result2--------------------------------------------------------------
result2 <- withr::with_seed(1234L, 
                            elasticHTE(data_rct, data_rwe, 
                                       ps.method = "SL", 
                                       ps.controls = list("family" = "quasibinomial",
                                                          "SL.library" = "SL.glm")))
result2

## ----load_bin_data------------------------------------------------------------
data("elasticToy.bin", package = "ElasticIntegrative")

## ----summary_bin_rwe----------------------------------------------------------
summary(elasticToy.bin.rwe)

## ----summary_bin_rct----------------------------------------------------------
summary(elasticToy.bin.rct)

## ----create_data_rct_bin------------------------------------------------------
data_rct <- dataInput(data = elasticToy.bin.rct,
                      outcome.model = Y ~ X1 + X2*A,
                      ps.model = A ~ X1*X2)

## ----create_data_rwe_bin------------------------------------------------------
data_rwe <- dataInput(data = elasticToy.bin.rwe,
                      outcome.model = Y ~ X1 + X2*A,
                      ps.model = A ~ X1*X2)

## ----result3------------------------------------------------------------------
result3 <- withr::with_seed(2345L,
                            elasticHTE(data_rct, data_rwe, outcome.type = "bin", 
                                       outcome.controls = list("family" = "quasibinomial")))
print(result3)

