# The eight literal strings that actually occur in the driver column, and the GAMS-safe
# names they become. Established EMPIRICALLY from the data, not from the publication: the
# paper documents seven classes, the data carries an eighth, `Unknown`, holding 0.4 per
# cent of global loss. A class map coded from the paper would have dropped it silently -
# which is the whole reason the preflight tabulates class strings before any mapping is
# written. The rename happens only after the guard has seen the source's own strings.
gfwDriverClasses <- c("Permanent agriculture"        = "permanent_agriculture",
                      "Hard commodities"             = "hard_commodities",
                      "Shifting cultivation"         = "shifting_cultivation",
                      "Logging"                      = "logging",
                      "Wildfire"                     = "wildfire",
                      "Settlements & Infrastructure" = "settlements_infrastructure",
                      "Other natural disturbances"   = "other_natural_disturbances",
                      "Unknown"                      = "unknown")

# Global tree cover loss over the full record, in Mha, for the dataset version and canopy
# threshold pinned in downloadGFWLossByDriver(). GFW dataset versions are immutable, so
# this is exact; the 5 per cent tolerance below makes it a truncation guard rather than a
# version tripwire. If the pin in the download function changes, re-run the preflight and
# update this number deliberately - do not widen the tolerance.
gfwGlobalLossMha <- 542.81

# The identities the extract must satisfy. None of these is a formality: a silently
# truncated CSV, a renamed driver class or a source folder built at another canopy
# threshold all yield a plausible-looking forest disturbance rate rather than an error.
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

  years <- sort(unique(df$year))
  if (!identical(years, seq(min(years), max(years)))) {
    stop("GFWLossByDriver: the year series ", min(years), "-", max(years),
         " has gaps at ", toString(setdiff(seq(min(years), max(years)), years)), ".")
  }

  if (anyNA(df$loss_ha) || any(df$loss_ha < 0)) {
    stop("GFWLossByDriver: loss_ha contains ", sum(is.na(df$loss_ha)), " missing and ",
         sum(df$loss_ha < 0, na.rm = TRUE), " negative values.")
  }

  if (length(unique(df$iso)) < 200) {
    stop("GFWLossByDriver: only ", length(unique(df$iso)), " countries in the extract; ",
         "the full table carries 216. The download is probably truncated.")
  }

  # The redundant identity: the driver breakdown summed over drivers must reproduce the
  # independently aggregated country totals. This is what makes a truncated or corrupted
  # driver file fail loudly instead of becoming a plausible forest disturbance rate.
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
#' @details The canopy density threshold is not restated here. It is pinned and verified in
#' [downloadGFWLossByDriver()] and stamped into every row of the extract, so this function
#' only insists that the data carries exactly one of them.
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

  # The extract holds only the country-year-driver combinations that carry loss, so
  # as.magpie leaves the rest empty. An absent combination means no loss was attributed to
  # that driver, which is zero and not unknown.
  x[is.na(x)] <- 0

  return(magpiesort(x))
}
