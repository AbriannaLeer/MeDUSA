## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     local.mz_log_removed_rows                                                   ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("mz_log_removed_rows: HappyPath", {
  inp <- data.frame(
    mz <- c(50, 100, 150, 200, 1000),
    sam1 <- c(10, 20, 0, 0, 0),
    sam2 <- c(0, 30, 40, 0, 0),
    sam3 <- c(0, 0, 50, 60, 0),
    sam4 <- c(0, 0, 80, 90, 100)
  )
  expect_no_error(local.mz_log_removed_rows(inp, inp[1, 3], "Clever Accounting"))
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     local.ensure_mz                                                 ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("ensure_mz: no_mz provided", {
  load("testdata/mz_neg.Rdata")
  meta <- data.frame(read.csv("testdata/meta_neg.csv"))
  expect_error(
    local.ensure_mz(
      mztools_filter(mz_neg, meta, c("blue"), keep_mz = F),
      mztools_filter(mz_neg, meta, c("red"), keep_mz = F), "llama"
    ),
    "ERROR: llama: neither input dataframes have column mz"
  )
})

test_that("ensure_mz: mismatch rows", {
  load("testdata/mz_neg.Rdata")
  meta <- data.frame(read.csv("testdata/meta_neg.csv"))
  expect_error(
    local.ensure_mz(
      mztools_filter(mz_neg[1:10, ], meta, c("blue")),
      mztools_filter(mz_neg[1:9, ], meta, c("red")), "llama"
    ),
    "ERROR: llama: Dataframes have a different number of mz"
  )
})

test_that("ensure_mz: happyPath", {
  load("testdata/mz_neg.Rdata")
  load("testdata/asserts/ensure_mz_happy.Rdata")
  meta <- data.frame(read.csv("testdata/meta_neg.csv"))
  actual <- local.ensure_mz(
    mztools_filter(mz_neg, meta, c("blue")),
    mztools_filter(mz_neg, meta, c("red"))
  )
  expect_identical(actual, ensure_mz_happy)
})
