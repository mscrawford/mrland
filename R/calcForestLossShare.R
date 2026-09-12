#' @title calcForestLossShare
#'
#' @description Calculates which share of natural forest area is lost per year to each
#' driver of forest loss.
#'
#' @param source Driver data to use, `"GFW"` (default) or `"Curtis"`. See
#' [calcForestFireLoss()].
#' @param period Years averaged to give the annual rate, for `source = "GFW"` only.
#'
#' @details For `source = "GFW"` numerator and denominator come from the same product at the
#' same canopy threshold: UMD tree cover loss by driver over UMD tree cover extent in 2000,
#' the base the loss is measured on. For `source = "Curtis"` the denominator stays FRA 2020
#' naturally regenerating forest area in 2010, reproducing the previous parameterisation
#' exactly. The `overall` column (FRA fire area) is divided by the same denominator as the
#' drivers of its source and exceeds 1 in a few savanna countries; GAMS loads it but never reads
#' it. Shares above 1 are clamped to 1; for a driver column this also warns with the country names.
#'
#' @return MAgPIE object with the share of natural forest area lost per year by driver
#' @author Abhijeet Mishra, Michael Crawford
#' @importFrom magclass setYears setNames getItems
#' @importFrom utils head
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

  forestArea <- switch(
    source,  # nolint: undesirable_function_linter.
    "GFW" = readSource("GFWLossByDriver", subtype = "extent", convert = TRUE),
    "Curtis" = readSource("FRA2020", subtype = "forest_area",
                          convert = TRUE)[, "y2010", "naturallyRegeneratingForest"],
    stop("calcForestLossShare: unknown source '", source,  # nolint: undesirable_function_linter.
         "'. Use \"GFW\" or \"Curtis\".")
  )
  forestArea <- setYears(setNames(forestArea, NULL), NULL)

  lostShare <- lostArea / forestArea
  lostShare[is.infinite(lostShare)] <- 0
  lostShare[is.na(lostShare)] <- 0
  # overall (FRA fire area over the driver denominator) exceeds 1 in savanna countries; GAMS
  # never reads it. A driver share above 1 would mean the numerator and denominator disagree.
  drivers <- setdiff(getItems(lostShare, 3), "overall")
  over <- lostShare[, , drivers] > 1
  if (any(over)) {
    warning("calcForestLossShare: ", sum(over), " driver share(s) above 1 clamped to 1 (",
            source, "): ",  # nolint: undesirable_function_linter.
            toString(utils::head(getItems(lostShare, 1)[apply(over, 1, any)], 5)), ".")
  }
  lostShare[lostShare > 1] <- 1

  return(list(x = lostShare,
              weight = forestArea,
              unit = "1",
              description = paste0("Share of natural forest area lost per year by driver (",
                                   source, ")"),  # nolint: undesirable_function_linter.
              isocountries = FALSE))
}
