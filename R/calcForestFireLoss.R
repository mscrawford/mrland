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
#' @details The `overall` column is the FRA 2020 fire series averaged over its own years,
#' 2000-2017, not over `period` - the two records barely overlap. That is the behaviour this
#' function has always had, and `overall` is loaded but never read on the GAMS side.
#'
#' @return MAgPIE object with forest area lost per year by driver, Mha
#' @author Abhijeet Mishra, Michael Crawford
#' @importFrom magclass mbind setNames dimSums getYears
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
      dimSums(x[, wanted, ], dim = 2) / length(wanted)
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
