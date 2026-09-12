# Synthesized known-bug cases for checkGFWLossByDriver(), written before it ran on real data:
# each corruption below must fail and each benign perturbation must pass.

# A minimal extract satisfying every identity the guard checks; uniform values on purpose.
makeExtract <- function(nIso = 210, years = 2001:2025) {
  iso <- sprintf("C%02d", seq_len(nIso))
  drivers <- names(gfwDriverClasses)  # nolint: object_usage_linter.
  df <- expand.grid(iso = iso, year = years, driver = drivers,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$threshold <- as.integer(gfwCanopyThreshold)  # nolint: object_usage_linter.
  df$loss_ha <- gfwGlobalLossMha * 1e6 / nrow(df)  # nolint: object_usage_linter.
  df[, c("iso", "year", "threshold", "driver", "loss_ha")]
}

makeTotals <- function(df) {
  t <- aggregate(list(loss_ha = df$loss_ha), by = list(iso = df$iso, year = df$year), FUN = sum)
  t$threshold <- as.integer(gfwCanopyThreshold)  # nolint: object_usage_linter.
  t[, c("iso", "year", "threshold", "loss_ha")]
}

test_that("the guard passes clean data", {
  df <- makeExtract()
  expect_silent(checkGFWLossByDriver(df, makeTotals(df)))
})

test_that("a dropped digit in one country-year-driver is caught", {
  df <- makeExtract()
  totals <- makeTotals(df)              # totals built BEFORE the corruption, as on the server
  i <- which(df$iso == "C07" & df$year == 2020 & df$driver == "Shifting cultivation")
  expect_length(i, 1)                   # a corruption that matches no row tests nothing
  df$loss_ha[i] <- df$loss_ha[i] / 10   # the Curtis failure: 39 typed as 3
  expect_error(checkGFWLossByDriver(df, totals), "do not sum to the country total")
})

test_that("a missing driver class is caught", {
  df <- makeExtract()
  totals <- makeTotals(df)
  df <- df[df$driver != "Shifting cultivation", ]
  expect_error(checkGFWLossByDriver(df, totals), "do not match the class map")
})

test_that("a renamed driver class is caught", {
  df <- makeExtract()
  df$driver[df$driver == "Logging"] <- "Forest management"
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "do not match the class map")
})

test_that("an added driver class is caught", {
  df <- makeExtract()
  extra <- df[df$driver == "Wildfire", ]
  extra$driver <- "Flooding"
  df <- rbind(df, extra)
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "do not match the class map")
})

test_that("a driver file cut short is caught", {
  df <- makeExtract()
  totals <- makeTotals(df)               # totals complete, as when one of the two downloads fails
  df <- df[df$iso %in% head(unique(df$iso), 200), ]   # tail of the country list lost
  expect_error(checkGFWLossByDriver(df, totals), "do not cover the same country-years")
})

test_that("an extract mixing canopy thresholds is caught", {
  # The threshold value itself is checked in downloadGFWLossByDriver(); read catches mixing.
  df <- makeExtract()
  df$threshold[seq(1, nrow(df), 2)] <- 75L
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "mixes canopy thresholds")
})

test_that("missing and negative loss values are caught", {
  df <- makeExtract()
  df$loss_ha[5] <- NA_real_
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "missing and")
  df <- makeExtract()
  df$loss_ha[5] <- -1
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "negative values")
})

test_that("a global total far from the pinned reference is caught", {
  df <- makeExtract()
  df$loss_ha <- df$loss_ha * 1.2
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "global loss sums to")
})

test_that("a missing column is caught", {
  df <- makeExtract()
  expect_error(checkGFWLossByDriver(df[, -3], makeTotals(df)), "missing column")
})

# Benign perturbations: a guard that rejects everything is as useless as one that rejects nothing.

test_that("row order does not matter", {
  df <- makeExtract()
  totals <- makeTotals(df)
  expect_silent(checkGFWLossByDriver(df[sample(nrow(df)), ], totals))
})

test_that("floating point noise below tolerance passes", {
  df <- makeExtract()
  totals <- makeTotals(df)
  df$loss_ha <- df$loss_ha * (1 + 1e-12)
  expect_silent(checkGFWLossByDriver(df, totals))
})

test_that("a global total just inside the tolerance passes", {
  df <- makeExtract()
  df$loss_ha <- df$loss_ha * 1.04
  expect_silent(checkGFWLossByDriver(df, makeTotals(df)))
})
