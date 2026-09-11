# The only driver class that belongs in f35_forest_lost_share. The parameter is not a
# deforestation rate: module 35 moves the "lost" area out of mature secondary-forest age
# classes and out of primary forest and straight back into the youngest age classes
# (35_natveg/pot_forest_may24/presolve.gms), so total forest area is conserved and only the
# age structure resets. The flux it generates is nevertheless booked as anthropogenic
# land-use-change CO2 (52_carbon/normal_dec17/equations.gms) and is priced under a land
# carbon policy. A class therefore qualifies only if it (a) leaves the land as forest,
# (b) is not something MAgPIE already decides for itself, and (c) is a land-use process
# rather than a natural one.
#
# Failing (b) - the observational counterpart to quantities the model solves for. These are
# published through mrvalidation::calcValidTreeCoverLoss() instead, where they benchmark the
# model rather than parameterise it; feeding them in here would double count:
#
#   permanent agriculture        - permanent conversion, endogenous via vm_lu_transitions
#   hard commodities             - permanent conversion (mining)
#   logging                      - regrowth, but endogenous via v35_hvarea_* and module 73
#   settlements & infrastructure - permanent conversion, and module 34 sets the urban pool
#   unknown                      - unattributable
#
# Failing (c) - natural disturbance. Wildfire and other natural disturbances pass (a) and
# (b) and were carried here until 2026-09-11. They come out because the flux they generate
# is labelled land-use change in the emission tree and large deforestation fires in
# magpie4::reportFireEmissions. Lightning fire in boreal Russia is neither. Wildfire carries
# a second, independent objection: the LPJmL run behind MAgPIE's natural-vegetation carbon
# densities had fire enabled, so its effect on the forest carbon stock is already in the
# asymptote and would be represented twice.
#
#   wildfire                     - 8.66 Mha/yr, 72% boreal; also inside the LPJmL densities
#   other natural disturbances   - 0.45 Mha/yr (insects, windthrow)
#
# (Measured 2026-09-11 from this function at the commit before they were dropped, against
# 3.17 Mha/yr for shifting cultivation.) Making either operative needs an accounting label
# outside the land-use-change tree first, and wildfire additionally needs the stock double
# count resolved. Both remain available through calcValidTreeCoverLoss().
gfwModelDrivers <- c("shifting_cultivation")

#' @title calcForestFireLoss
#'
#' @description Assembles annual forest area loss by driver, in Mha per year, from one of
#' two driver sources, with the FAO FRA 2020 fire column bound on as `overall`.
#'
#' @param source Driver data to use. `"GFW"` (default) is the WRI/Google DeepMind 1 km
#' dominant-driver product of Sims et al. (2025), crossed with UMD/Hansen annual tree
#' cover loss per country - see [readGFWLossByDriver()]. `"Curtis"` is the hand-transcribed
#' Table 1 of Curtis et al. (2018), seven regions spread over countries by FRA forest area;
#' it is kept so that earlier input revisions stay regenerable, and reproduces the previous
#' behaviour of this function exactly.
#' @param period Years averaged to give the annual rate, for `source = "GFW"` only. The
#' default 2015:2024 is the most recent complete decade in the GFW record. The Curtis
#' source carries a single 2001-2015 mean and ignores this argument.
#'
#' @details For `source = "GFW"` only `shifting_cultivation` is returned, the one driver
#' class that leaves the land as forest, is not decided endogenously by MAgPIE, and is a
#' land-use rather than a natural process - see `gfwModelDrivers` at the top of this file
#' for the other seven and why each is routed to mrvalidation instead.
#'
#' The `overall` column is the FRA 2020 fire series averaged over its own years,
#' 2000-2017, not over `period` - the two records barely overlap. That is the behaviour this
#' function has always had, and `overall` is loaded but never read on the GAMS side.
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
