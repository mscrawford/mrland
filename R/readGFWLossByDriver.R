# Driver class strings as they occur in the data, and their GAMS-safe names. Eight, not the
# paper's seven: the data carries `Unknown` (0.4 per cent of global loss).
gfwDriverClasses <- c("Permanent agriculture"        = "permanent_agriculture",
                      "Hard commodities"             = "hard_commodities",
                      "Shifting cultivation"         = "shifting_cultivation",
                      "Logging"                      = "logging",
                      "Wildfire"                     = "wildfire",
                      "Settlements & Infrastructure" = "settlements_infrastructure",
                      "Other natural disturbances"   = "other_natural_disturbances",
                      "Unknown"                      = "unknown")

# Global loss 2001-2025 in Mha for the dataset version and threshold pinned in
# downloadGFWLossByDriver(). Versions are immutable; update this deliberately after a re-pin.
gfwGlobalLossMha <- 542.81

# Integrity checks on the extract. These reconcile the file against its own schema, a second
# query of the same table (loss_totals.csv) and a pinned snapshot, not against an external
# source: a truncated download, a renamed class or a mixed threshold would otherwise become a
# plausible disturbance rate rather than an error.
checkGFWLossByDriver <- function(df, totals) {

  required <- c("iso", "year", "threshold", "driver", "loss_ha")
  if (!all(required %in% names(df))) {
    stop("GFWLossByDriver: loss_by_driver.csv is missing column(s) ",
         toString(setdiff(required, names(df))), ".")
  }

  observed <- sort(unique(df$driver))
  expected <- sort(names(gfwDriverClasses))
  if (!identical(observed, expected)) {
    stop("GFWLossByDriver: the driver classes in the data do not match the class map. ",
         "Only in data: ", toString(setdiff(observed, expected)), ". ",
         "Only in map: ", toString(setdiff(expected, observed)), ". ",
         "Re-run the preflight and update gfwDriverClasses before trusting any output.")
  }

  if (length(unique(df$threshold)) != 1L) {
    stop("GFWLossByDriver: the extract mixes canopy thresholds ",
         toString(sort(unique(df$threshold))),
         ". It must carry exactly one, the value pinned in downloadGFWLossByDriver().")
  }

  if (anyNA(df$loss_ha) || any(df$loss_ha < 0)) {
    stop("GFWLossByDriver: loss_ha contains ", sum(is.na(df$loss_ha)), " missing and ",
         sum(df$loss_ha < 0, na.rm = TRUE), " negative values.")
  }

  # The driver file summed over drivers must reproduce the separately queried totals file.
  byDriver <- stats::aggregate(list(drv = df$loss_ha),
                               by = list(iso = df$iso, year = df$year), FUN = sum)
  both <- merge(byDriver, totals[, c("iso", "year", "loss_ha")],
                by = c("iso", "year"), all = TRUE)
  if (anyNA(both$drv) || anyNA(both$loss_ha)) {
    onlyTot <- both[is.na(both$drv), c("iso", "year")]
    onlyDrv <- both[is.na(both$loss_ha), c("iso", "year")]
    stop("GFWLossByDriver: the driver file and the totals file do not cover the same ",
         "country-years. ", nrow(onlyTot), " only in totals (e.g. ",
         toString(utils::head(paste(onlyTot$iso, onlyTot$year), 5)), "), ",
         nrow(onlyDrv), " only in the driver file (e.g. ",
         toString(utils::head(paste(onlyDrv$iso, onlyDrv$year), 5)), ").")
  }
  off <- abs(both$drv - both$loss_ha) > 1e-6 * pmax(both$loss_ha, 1)
  if (any(off)) {
    stop("GFWLossByDriver: driver shares do not sum to the country total for ", sum(off),
         " country-years, e.g. ",
         toString(utils::head(paste0(both$iso[off], " ", both$year[off], " (",
                                     signif(both$drv[off], 6), " vs ",
                                     signif(both$loss_ha[off], 6), " ha)"), 3)), ".")
  }

  total <- sum(df$loss_ha) / 1e6
  if (abs(total - gfwGlobalLossMha) > 0.05 * gfwGlobalLossMha) {
    stop("GFWLossByDriver: global loss sums to ", round(total, 1), " Mha, but ",
         gfwGlobalLossMha, " Mha is expected for the pinned dataset version and canopy ",
         "threshold. Either the download is incomplete, or the pin in ",
         "downloadGFWLossByDriver() has changed - re-run the preflight before updating ",
         "gfwGlobalLossMha.")
  }

  return(invisible(df))
}

#' @title readGFWLossByDriver
#'
#' @description Reads the GFW tree cover loss by driver extract into a magpie object with
#' dimensions country x year x driver, in Mha of tree cover loss per year.
#'
#' @details The canopy threshold is pinned in [downloadGFWLossByDriver()] and stamped into
#' every row; this function only insists that the extract carries exactly one.
#'
#' @return magpie object, ISO country x 2001..2025 x eight driver classes, unit Mha
#' @author Michael Crawford
#' @importFrom magclass as.magpie magpiesort
#' @importFrom utils read.csv head
#' @seealso [downloadGFWLossByDriver()], [convertGFWLossByDriver()]
#' @examples
#' \dontrun{
#' a <- readSource("GFWLossByDriver")
#' }

readGFWLossByDriver <- function() {

  df <- read.csv("loss_by_driver.csv", stringsAsFactors = FALSE)
  totals <- read.csv("loss_totals.csv", stringsAsFactors = FALSE)
  checkGFWLossByDriver(df, totals)

  df$driver <- unname(gfwDriverClasses[df$driver])
  df$loss <- df$loss_ha / 1e6 # hectares to Mha

  x <- as.magpie(df[, c("iso", "year", "driver", "loss")], spatial = 1, temporal = 2)

  # Absent country-year-driver combinations carry no loss: zero, not NA.
  x[is.na(x)] <- 0

  return(magpiesort(x))
}
