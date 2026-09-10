#' @title calcForestLossShare
#'
#' @description Calculates which share of natural forest area is lost per year to each
#' driver of forest loss.
#'
#' @param source Driver data to use, `"GFW"` (default) or `"Curtis"`. See
#' [calcForestFireLoss()].
#' @param period Years averaged to give the annual rate, for `source = "GFW"` only.
#'
#' @details The numerator is UMD tree cover loss (or, for Curtis, tree cover loss
#' transcribed from the paper) and the denominator is FRA 2020 naturally regenerating
#' forest area in 2010. Those two are not the same quantity: tree cover is not forest, and
#' loss detected in tree cover outside natural forest still lands in the numerator. The
#' mismatch is inherited from the original parameterisation and is left in place here on
#' purpose, so that switching the driver source moves the numbers for one reason only.
#' Changing the denominator is a separate decision.
#'
#' @return MAgPIE object with the share of natural forest area lost per year by driver
#' @author Abhijeet Mishra, Michael Crawford
#' @importFrom magclass setYears setNames
#' @importFrom madrat calcOutput readSource
#' @seealso [calcForestFireLoss()]
#' @examples
#' \dontrun{
#' calcOutput("ForestLossShare", aggregate = FALSE)
#' calcOutput("ForestLossShare", source = "Curtis", aggregate = FALSE)
#' }

calcForestLossShare <- function(source = "GFW", period = 2015:2024) {

  lostArea <- calcOutput("ForestFireLoss", source = source,  # nolint: undesirable_function_linter.
                         period = period, aggregate = FALSE)
  forestArea <- setYears(setNames(readSource("FRA2020", subtype = "forest_area",
                                             convert = TRUE)[, "y2010",
                                                             "naturallyRegeneratingForest"],
                                  NULL), NULL)

  lostShare <- lostArea / forestArea
  lostShare[is.infinite(lostShare)] <- 0
  lostShare[is.na(lostShare)] <- 0
  lostShare[lostShare > 1] <- 1

  return(list(x = lostShare,
              weight = forestArea,
              unit = "1",
              description = paste0("Share of natural forest area lost per year by driver (",
                                   source, ")"),  # nolint: undesirable_function_linter.
              isocountries = FALSE))
}
