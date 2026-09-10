# Table 1 of Curtis et al. (2018) is hand-typed (inst/extdata/ForestLossDrivers/SOURCE.md).
# These identities of the printed table are checked on every read; a wrong cell does not
# fail otherwise, it becomes a wrong disturbance rate.
checkCurtisTable1 <- function(df) {
  globalLoss <- 314 # Mha 2001-2015, "Global" row of Table 1
  drivers <- c("deforestation", "shifting_agriculture", "forestry", "wildfire", "urbanization")

  total <- sum(df$treecoverloss_01_15)
  if (abs(total - globalLoss) > 2) {
    stop("ForestLossDrivers: regional tree cover loss sums to ", total, " Mha, but Table 1 of ",
         "Curtis et al. (2018) reports a global total of ", globalLoss, " Mha. forest_loss.csv is corrupt.")
  }

  offBy <- abs(rowSums(df[drivers]) - 100)
  if (any(offBy > 2)) {
    stop("ForestLossDrivers: driver shares do not sum to 100 per cent for ",
         toString(df$region[offBy > 2]), " in forest_loss.csv.")
  }

  implied <- 100 * df$treecoverloss_01_15 / total
  mismatch <- abs(implied - df$treecoverloss_pc_01_15) > 2
  if (any(mismatch)) {
    stop("ForestLossDrivers: absolute and relative tree cover loss disagree for ",
         toString(df$region[mismatch]), " in forest_loss.csv.")
  }

  return(invisible(df))
}

#' Read ForestLossDrivers
#'
#' Read-in an Forest loss data (range 2001-2015 but only single annual number her)
#' (Source:DOI: 10.1126/science.aau3445 Table 1).
#'
#'
#' @return magpie object of the Curtis et al., 2018 Data
#' @author Abhijeet Mishra
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("ForestLossDrivers")
#' }
#'
#' @importFrom magclass as.magpie
#' @importFrom madrat toolSubtypeSelect
#' @import readxl
#' @import countrycode
#' @importFrom stats complete.cases

readForestLossDrivers <- function() {
  ## Mapping file
  mapping <- read.csv("mapping.csv", header = TRUE, sep = ";")

  ## Magpie standard
  isoCountry <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "madrat")

  ## Merge Mappings
  fullMapping <- merge(mapping, isoCountry, by = "CountryCode")[, c(-2, -4)]
  colnames(fullMapping) <- c("CountryCode", "RegionCodeSource", "RegionCodeMAgPIE")

  ## Original Data
  file <- "forest_loss.csv"
  # This is Tree Cover loss in Mha at this stage, all drivers are in percentage
  df <- read.csv(file = file, header = TRUE, sep = ",")
  checkCurtisTable1(df)
  # Division by 100 because we change from percentage to proportion. Division by 15 to get annual data
  df[, -c(1:3)] <- (df$treecoverloss_01_15 * (df[, -c(1:3)] / 100)) / 15

  dfMag <- as.magpie(df[, -c(2, 3)], temporal = NULL, spatial = "region") ## This is Tree Cover loss in Mha

  ## FRA Forest Area
  # Convert is set to TRUE For Mha
  a <- collapseNames(readSource("FRA2020", "forest_area", convert = TRUE)[, , "naturallyRegeneratingForest"])

  ## Create iso level data based on forest data as weight
  forestLoss <- toolAggregate(x = dfMag, weight = setYears(a[, "y2015", ], NULL), rel = fullMapping,
                              from = "RegionCodeSource", to = "CountryCode")

  return(forestLoss)
}
