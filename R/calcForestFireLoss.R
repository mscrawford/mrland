# Driver classes that reach f35_forest_lost_share. Module 35 resets the age structure of the
# "lost" area (the land stays forest) and module 52 books the flux as land-use-change CO2, so a
# class qualifies only if it leaves the land as forest, is not decided endogenously by MAgPIE,
# and is a land-use rather than a natural process. Of the eight GFW classes only shifting
# cultivation passes. The others are published by mrvalidation::calcValidTreeCoverLoss:
# wildfire and other natural disturbances fail the third test, the rest the second.
gfwModelDrivers <- c("shifting_cultivation")

#' @title calcForestFireLoss
#'
#' @description Annual forest area loss by driver, Mha per year, from the Sims et al. (2025)
#' driver product (`source = "GFW"`, default) or the Curtis et al. (2018) table
#' (`source = "Curtis"`), with the FRA 2020 fire series bound on as `overall`.
#'
#' @param source `"GFW"` (default) or `"Curtis"`. Curtis reproduces the previous behaviour of
#' this function exactly and keeps earlier input revisions regenerable.
#' @param period Years averaged into the annual rate, GFW only (default 2015:2024). Curtis
#' carries a single 2001-2015 mean.
#'
#' @details For GFW only the classes in `gfwModelDrivers` are returned. `overall` is the FRA
#' fire series averaged over its own years (2000-2017), as before; GAMS loads it but never
#' reads it.
#'
#' @return MAgPIE object with forest area lost per year by driver, Mha
#' @author Abhijeet Mishra, Michael Crawford
#' @importFrom magclass mbind setNames dimSums getYears getItems
#' @importFrom madrat readSource
#' @seealso [readGFWLossByDriver()], [readForestLossDrivers()]
#' @examples
#' \dontrun{
#' calcOutput("ForestFireLoss", aggregate = FALSE)
#' calcOutput("ForestFireLoss", source = "Curtis", aggregate = FALSE)
#' }

calcForestFireLoss <- function(source = "GFW", period = 2015:2024) {

  fao <- setNames(readSource("FRA2020", subtype = "forest_fire", convert = TRUE), "overall")
  fao <- dimSums(fao, dim = 2) / length(getYears(fao)) # mean annual area lost to fire

  drivers <- switch(
    source,  # nolint: undesirable_function_linter.
    "Curtis" = readSource("ForestLossDrivers"),
    "GFW" = {
      x <- readSource("GFWLossByDriver", convert = TRUE)
      wanted <- paste0("y", period)
      absent <- setdiff(wanted, getYears(x))
      if (length(absent) > 0) {
        stop("calcForestFireLoss: the GFW record does not cover ", toString(absent),
             ". It runs ", min(getYears(x, TRUE)), "-", max(getYears(x, TRUE)), ".")
      }
      absent <- setdiff(gfwModelDrivers, getItems(x, 3))
      if (length(absent) > 0) {
        stop("calcForestFireLoss: the GFW source does not carry driver class(es) ",
             toString(absent), ".")
      }
      dimSums(x[, wanted, gfwModelDrivers], dim = 2) / length(wanted)
    },
    stop("calcForestFireLoss: unknown source '", source,  # nolint: undesirable_function_linter.
         "'. Use \"GFW\" or \"Curtis\".")
  )

  out <- mbind(fao, drivers)

  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              description = paste0("Forest area lost per year by driver (",
                                   source, ")"),  # nolint: undesirable_function_linter.
              isocountries = FALSE))
}
