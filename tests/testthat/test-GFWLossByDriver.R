# Positive tests for checkGFWLossByDriver().
#
# Written before the guard was ever run on the real extract. A clean run on good data is
# ambiguous between "the data is fine" and "the guard does nothing"; a synthesized case of
# each bug class is what separates those two. Every case below must FAIL, and the two
# benign perturbations at the end must PASS, or the guard is not doing its job.

# A minimal extract that satisfies every identity the guard checks. Values are uniform and
# contrived on purpose: the fixture exists to be broken one identity at a time, not to look
# like real loss.
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

test_that("a truncated download is caught", {
  df <- makeExtract()
  totals <- makeTotals(df)
  df <- df[seq_len(round(0.6 * nrow(df))), ]
  expect_error(checkGFWLossByDriver(df, totals))
})

test_that("losing the tail of the country list is caught", {
  df <- makeExtract(nIso = 150)
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "countries in the extract")
})

test_that("an extract mixing canopy thresholds is caught", {
  # The "did I get the threshold I asked for" check lives in downloadGFWLossByDriver(),
  # where the pin is defined. What read has to catch is a file holding more than one
  # threshold, which would silently double-count every country.
  df <- makeExtract()
  df$threshold[seq(1, nrow(df), 2)] <- 75L
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "mixes canopy thresholds")
})

test_that("a gap in the year series is caught", {
  df <- makeExtract()
  df <- df[df$year != 2013, ]
  expect_error(checkGFWLossByDriver(df, makeTotals(df)), "has gaps at 2013")
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

# The other half of the test. A guard that rejects everything is as useless as one that
# rejects nothing.

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
