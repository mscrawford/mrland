#' @title calcLivestockDistribution
#' @description Calculates gridded livestock distribution by species using GLW3/GLW4
#'   spatial snapshots (2010, 2015, 2020) linearly interpolated to requested FAO years,
#'   with constant extrapolation beyond the anchor range. Country totals come from
#'   \code{\link[mrcommons]{calcAnimalStocks}}. Eight species are mapped by GLW: cattle (Ct), buffaloes
#'   (Bf), sheep (Sh), goats (Gt), horses (Ho), pigs (Pg), chickens (Ch), ducks (Dk).
#'   Horses and ducks are not available in GLW4 2020 and fall back to the 2015
#'   spatial distribution. Camels (Cm), mules and asses (Mu) and turkeys, which in
#'   \code{\link[mrcommons]{calcAnimalStocks}} also hold geese (Tk), have no GLW map of
#'   their own and are distributed with the map of the closest mapped species: camels with
#'   goats, mules and asses with horses, turkeys and geese with chickens.
#'
#'   Where GLW has no pixel of a species in a country - mostly small island states, but
#'   also e.g. goats in DNK - the country's stock is spread by land area instead: by
#'   managed pasture and rangeland (or the \code{landProxy} area) for grazers and by total
#'   land (all nine land classes) for monogastrics, and as a last resort evenly over the
#'   country's cells. Country totals of the grid therefore match the FAO stocks, except for
#'   the 15 countries without a grid cell (of these, only MAC has livestock).
#'
#' @param output Type of output:
#'   \itemize{
#'     \item \code{"weight"}: dimensionless spatial downscaling weight within each
#'       country (sums to 1 per country per category per year).
#'     \item \code{"head"}: absolute livestock numbers (Million animals per grid cell).
#'     \item \code{"density"}: livestock per land area (Million animals per Mha).
#'       For ruminants, the denominator is the pasture/rangeland proxy area controlled
#'       by \code{landProxy}. For monogastrics (Pg, Ch, Dk, Tk), total grid-cell land area
#'       is used as the denominator.
#'   }
#' @param landProxy Land proxy controlling spatial allocation for the grazing species only.
#'   Monogastrics always use GLW spatial shares regardless of this setting:
#'   \itemize{
#'     \item \code{"glw"} (default): all categories use GLW spatial shares; land data are
#'       used only for countries where GLW has no pixel of a species (see description).
#'     \item \code{"pastRange"}: grazers (Ct, Bf, Sh, Gt, Ho, Cm, Mu) scaled by combined
#'       managed pasture and rangeland (\code{past + range}).
#'     \item \code{"speciesSpecific"}: cattle/buffalo (Ct, Bf) scaled by managed
#'       pasture (\code{past}); sheep, goats, horses, camels, mules and asses
#'       (Sh, Gt, Ho, Cm, Mu) scaled by rangeland (\code{range}).
#'   }
#' @param category Livestock category classification for output:
#'   \itemize{
#'     \item \code{"FAO"}: the eight GLW species (Ct, Bf, Sh, Gt, Ho, Pg, Ch, Dk) plus
#'       camels (Cm), mules and asses (Mu) and turkeys and geese (Tk).
#'     \item \code{"magpie"} (default): five MAgPIE livestock categories
#'       (livst_rum, livst_milk, livst_pig, livst_chick, livst_egg). FAO species are computed
#'       first and then aggregated using national dairy/broiler fractions from
#'       \code{\link[mrcommons]{calcAnimalStocks}}.
#'   }
#' @param selectyears Years to compute. Intersected with available FAO years.
#' @return A list with elements: \code{x} (magpie object at lpjcell resolution,
#'   67420 cells), \code{weight} (the land area each species was divided by for
#'   \code{output = "density"}, so that an aggregated call returns the area-weighted mean
#'   density; NULL for \code{"head"} and \code{"weight"}, which are summed. Summing the
#'   within-country shares of \code{output = "weight"} is only meaningful for aggregation
#'   that stays inside a country, such as \code{aggregate = "cluster"}), \code{unit}
#'   (character), \code{description} (character), and \code{isocountries}
#'   (FALSE — data is cellular, not country-level).
#' @author Bin Lin
#' @examples
#' \dontrun{
#' calcOutput("LivestockDistribution", output = "weight", aggregate = FALSE)
#' calcOutput("LivestockDistribution", output = "head",   aggregate = FALSE)
#' calcOutput("LivestockDistribution", output = "density", landProxy = "speciesSpecific",
#'            aggregate = FALSE)
#' calcOutput("LivestockDistribution", category = "magpie", aggregate = FALSE)
#' }
#' @importFrom magclass mbind setNames dimSums getYears getYears<- getItems time_interpolate
#' @importFrom magpiesets findset
#' @importFrom madrat calcOutput readSource
#' @importFrom mstools toolHoldConstant
#' @importFrom stats quantile

calcLivestockDistribution <- function(output = "head",
                                      landProxy = "glw",
                                      category  = "magpie",
                                      selectyears = paste0("y", 1961:2025)) {

  if (!output %in% c("weight", "head", "density")) {
    stop("output must be one of 'weight', 'head', or 'density'.")
  }
  if (!landProxy %in% c("glw", "pastRange", "speciesSpecific")) {
    stop("landProxy must be one of 'glw', 'pastRange', or 'speciesSpecific'.")
  }
  if (!category %in% c("FAO", "magpie")) {
    stop("category must be either 'FAO' or 'magpie'.")
  }

  # expand any named sets (e.g. "past"); keep explicit year strings (e.g. "y2011") as-is
  isYearStr <- function(s) grepl("^y[0-9]{4}$", s)
  selectyears <- sort(unique(unlist(lapply(selectyears, function(s) {
    if (isYearStr(s)) s else findset(s, noset = "original")
  }))))

  speciesAll      <- c("Ct", "Bf", "Sh", "Gt", "Ho", "Pg", "Ch", "Dk")
  speciesRuminant <- c("Ct", "Bf", "Sh", "Gt", "Ho")

  # read GLW spatial snapshots (heads/pixel); Ho and Dk missing in GLW4 2020, fall back to 2015
  glw2010 <- mbind(lapply(speciesAll, function(sp) {
    setNames(readSource("GLW3all", subtype = paste0("Da_", sp, "_2010")), sp)
  }))

  glw2015 <- mbind(lapply(speciesAll, function(sp) {
    setNames(readSource("GLW4", subtype = paste0("Da_", sp, "_2015")), sp)
  }))

  species2020    <- c("Ct", "Bf", "Sh", "Gt", "Pg", "Ch")
  glw2020        <- glw2015
  getYears(glw2020) <- "y2020"
  glw2020Partial <- mbind(lapply(species2020, function(sp) {
    x <- readSource("GLW4", subtype = paste0("Da_", sp, "_2020"))
    getYears(x) <- "y2020"
    setNames(x, sp)
  }))
  glw2020[, , species2020] <- glw2020Partial

  glw <- mbind(glw2010, glw2015, glw2020)
  glw[is.na(glw) | glw < 0] <- 0

  # calcAnimalStocks also reports camels, mules and asses, and turkeys (the item holds
  # geese as well), none of which GLW maps. They are distributed with the map of the
  # closest mapped species, so that the MAgPIE categories they feed into still add up to
  # the FAO country totals:
  #   Cm camels            -> goats    (arid rangeland, mapped in all three anchor years)
  #   Mu mules and asses   -> horses   (equids; like horses they use the 2015 map for 2020)
  #   Tk turkeys and geese -> chickens
  proxySpecies <- c(Cm = "Gt", Mu = "Ho", Tk = "Ch")
  glw <- mbind(glw, mbind(lapply(names(proxySpecies), function(sp) {
    setNames(glw[, , proxySpecies[[sp]]], sp)
  })))
  speciesAll      <- c(speciesAll, names(proxySpecies))
  speciesRuminant <- c(speciesRuminant, "Cm", "Mu")

  isoPerCell <- getItems(glw, dim = "iso", full = TRUE)

  # aggregate IPCC categories from calcAnimalStocks to GLW species
  animalStocks <- calcOutput("AnimalStocks", aggregate = FALSE)

  faoStocks <- mbind(
    setNames(dimSums(animalStocks[, , c("dairy cows",     "other cattle")],   dim = 3), "Ct"),
    setNames(dimSums(animalStocks[, , c("dairy buffalo",  "other buffalo")],  dim = 3), "Bf"),
    setNames(dimSums(animalStocks[, , c("dairy sheep",    "other sheep")],    dim = 3), "Sh"),
    setNames(dimSums(animalStocks[, , c("dairy goats",    "other goats")],    dim = 3), "Gt"),
    setNames(animalStocks[, , "horses"],                                                "Ho"),
    setNames(dimSums(animalStocks[, , c("market swine",   "breeding swine")], dim = 3), "Pg"),
    setNames(dimSums(animalStocks[, , c("poultry layers", "broilers")],       dim = 3), "Ch"),
    setNames(animalStocks[, , "ducks"],                                                 "Dk"),
    setNames(dimSums(animalStocks[, , c("dairy camels", "other camels")],  dim = 3), "Cm"),
    setNames(animalStocks[, , "mules and asses"],                                    "Mu"),
    setNames(animalStocks[, , "turkey"],                                             "Tk")
  )
  faoStocks[is.na(faoStocks) | faoStocks < 0] <- 0
  faoStocks[faoStocks == 0] <- 1e-6 # 1 animal

  # expandToGrid() fills the grid template by position, so the stocks have to hold the same
  # species in the same order as the GLW maps; otherwise every species would silently be
  # distributed with another species' country total
  if (!identical(getItems(faoStocks, dim = 3), getItems(glw, dim = 3))) {
    stop("faoStocks and glw must hold the same species in the same order: ",
         paste(getItems(faoStocks, dim = 3), collapse = ", "), " vs ",
         paste(getItems(glw, dim = 3), collapse = ", "))
  }

  availYears <- getYears(faoStocks)
  beyondFAO  <- selectyears[selectyears > max(availYears)]
  if (length(beyondFAO) > 0) {
    faoStocks <- toolHoldConstant(faoStocks, years = beyondFAO)
  }
  selectyears <- intersect(selectyears, getYears(faoStocks))
  if (length(selectyears) == 0) {
    stop("No requested years available in FAO data.")
  }
  faoStocks <- faoStocks[, selectyears, ]
  faoYears  <- getYears(faoStocks, as.integer = TRUE)

  land <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell",
                     nclasses = "nine", aggregate = FALSE)
  # land use comes in 5-year steps; interpolated to every FAO year and GLW anchor year, so that
  # heads and density use the same land area in each year
  land <- time_interpolate(land,
                           interpolated_year            = union(paste0("y", faoYears), c("y2010", "y2015", "y2020")),
                           integrate_interpolated_years = FALSE,
                           extrapolation_type           = "constant")

  # land proxy for ruminants: "pastRange" combines pasture and rangeland,
  # otherwise cattle/buffalo use pasture and the other grazers use rangeland
  proxyClasses <- list(Ct = "past", Bf = "past", Sh = "range", Gt = "range", Ho = "range",
                       Cm = "range", Mu = "range")
  getLandProxy <- function(sp, yrs) {
    classes <- if (landProxy == "pastRange") c("past", "range") else proxyClasses[[sp]]
    return(dimSums(land[, yrs, classes], dim = 3))
  }

  # expand country-level data to grid, preserving glw's full spatial structure
  expandToGrid <- function(countryData, yrStr, isoVec) {
    out <- glw[, "y2010", ]
    getYears(out) <- yrStr
    out[, , ] <- as.numeric(countryData[isoVec, yrStr, ])
    return(out)
  }

  # normalize so that grid values sum to 1 within each country;
  # uses tapply to avoid magclass dimensional algebra across different dim3 set names
  normalizeWithinCountry <- function(x, isoVec) {
    nCells <- dim(x)[1]
    xVec   <- as.numeric(x)
    nSlice <- length(xVec) / nCells
    outVec <- numeric(length(xVec))
    isoFac <- factor(isoVec)
    for (j in seq_len(nSlice)) {
      idx  <- seq_len(nCells) + (j - 1L) * nCells
      vec  <- xVec[idx]
      csum <- tapply(vec, isoFac, sum, na.rm = TRUE)
      cvec <- as.numeric(csum[isoFac])
      ok   <- !is.na(cvec) & cvec > 0
      outVec[idx[ok]] <- vec[ok] / cvec[ok]
    }
    out <- x
    out[, , ] <- outVec
    return(out)
  }

  # uniform share (1/n cells) within each country - last-resort fallback used only when
  # no spatial information (GLW, land proxy, or land at all) exists anywhere for that
  # country, e.g. small Pacific/Indian Ocean island states with literally no land-use
  # data recorded at 0.5 degree resolution (see @description).
  uniformWithinCountry <- function(x, isoVec) {
    nCells <- dim(x)[1]
    nSlice <- length(as.numeric(x)) / nCells
    isoFac <- factor(isoVec)
    nCellsPerCtry <- as.numeric(table(isoFac))
    names(nCellsPerCtry) <- levels(isoFac)
    perCellShare <- 1 / nCellsPerCtry[isoVec]
    out <- x
    out[, , ] <- rep(perCellShare, nSlice)
    out
  }

  # for cells where GLW = 0 but land proxy > 0, fill with country mean density;
  # applied once per GLW anchor year (2010/2015/2020, see densityAnchors below) so the
  # gap-fill uses that anchor's own real land proxy, not a placeholder.
  # All arithmetic uses as.numeric() to avoid magclass outer-product when dim3
  # set names differ between GLW (species) and land-area objects.
  # Densities are only computed for cells with at least 1e-6 Mha of land proxy and are capped
  # at the country's 99th percentile among cells with at least 1e-3 Mha, as a small area at one
  # anchor would otherwise give an extreme density that is multiplied with the larger area of
  # other years.
  fillGLWZeros <- function(baseGLW, cellArea, landYr, isoVec) {
    minArea <- 1e-6
    glwArr  <- as.numeric(baseGLW)
    areaArr <- as.numeric(cellArea)
    glwDensity <- baseGLW
    glwDensity[, , ] <- ifelse(areaArr >= minArea, glwArr / areaArr, 0)

    ctryHeads    <- dimSums(baseGLW, dim = c("x", "y"))

    positiveArea <- cellArea
    positiveArea[glwArr <= 0] <- 0
    positiveArea[is.na(positiveArea)] <- 0
    ctryArea <- dimSums(positiveArea, dim = c("x", "y"))
    ctryArea[ctryArea <= 0] <- NA

    ctryMeanDensity <- ctryHeads
    ctryMeanDensity[, , ] <- as.numeric(ctryHeads) / as.numeric(ctryArea)

    ctryMeanGrid <- baseGLW
    ctryMeanGrid[, , ] <- as.numeric(ctryMeanDensity[isoVec, , ])

    zeroCells <- glwArr == 0 & as.numeric(landYr) > 0
    glwDensity[zeroCells] <- ctryMeanGrid[zeroCells]
    glwDensity[is.na(glwDensity)] <- 0

    densArr <- as.numeric(glwDensity)
    valid   <- densArr > 0 & areaArr >= 1e-3
    capCtry <- tapply(densArr[valid], isoVec[valid], quantile, probs = 0.99, names = FALSE)
    capCell <- as.numeric(capCtry[isoVec])
    glwDensity[, , ] <- ifelse(!is.na(capCell) & densArr > capCell, capCell, densArr)
    return(glwDensity)
  }

  # if country total of primary is zero, fall back to land proxy then raw GLW shares;
  # if even that is zero (no spatial information anywhere for that country), split
  # evenly across the country's own cells as a last resort - see uniformWithinCountry()
  withFallback <- function(primary, fallback1, fallback2, isoVec) {
    share    <- normalizeWithinCountry(primary,   isoVec)
    primCtry <- dimSums(primary,   dim = c("x", "y"))
    fb1Ctry  <- dimSums(fallback1, dim = c("x", "y"))
    fb2Ctry  <- dimSums(fallback2, dim = c("x", "y"))

    expandFlag <- function(flag) {
      g <- primary
      g[, , ] <- as.numeric(flag[isoVec, , ])
      g
    }

    # blend fallback shares via plain numeric vectors to avoid dim3 mismatch
    shareArr <- as.numeric(share)
    fb1Share <- normalizeWithinCountry(fallback1, isoVec)
    pz  <- as.numeric(expandFlag(primCtry == 0)) > 0
    shareArr[pz] <- as.numeric(fb1Share)[pz]

    fb2Share <- normalizeWithinCountry(fallback2, isoVec)
    fb2 <- as.numeric(expandFlag(primCtry == 0 & fb1Ctry == 0)) > 0
    shareArr[fb2] <- as.numeric(fb2Share)[fb2]

    # last resort: GLW, land proxy and the final fallback are all zero for this country
    # (e.g. small island states with no land-use data recorded at all at 0.5 degree
    # resolution) - an uninformed uniform guess is still far better than the real FAO
    # total silently vanishing (leaving only the rounding epsilon added at the very end
    # of calcLivestockDistribution).
    fb3Share <- uniformWithinCountry(primary, isoVec)
    fb3 <- as.numeric(expandFlag(primCtry == 0 & fb1Ctry == 0 & fb2Ctry == 0)) > 0
    shareArr[fb3] <- as.numeric(fb3Share)[fb3]

    share[, , ] <- shareArr
    share[is.na(share) | is.nan(share) | is.infinite(share)] <- 0
    share
  }

  # linearly interpolate GLW snapshots to FAO years; constant extrapolation outside anchors
  # integrate_interpolated_years = FALSE because GLW is a stock variable, not a flow
  glwInterp <- time_interpolate(glw,
                                interpolated_year            = paste0("y", faoYears),
                                integrate_interpolated_years = FALSE,
                                extrapolation_type           = "constant")

  # land-adjusted ruminants: compute a land-proxy density at each GLW anchor year
  # (2010/2015/2020), paired with that same year's land proxy area, then interpolate the
  # density itself over time (same scheme as glwInterp above). At the anchors this
  # reproduces GLW in cells where GLW has animals; cells without GLW animals but with land
  # proxy area get the country mean density (see fillGLWZeros), so that grassland is not left
  # empty. In other years the result drifts from GLW in proportion to the land proxy change.
  if (landProxy != "glw") {
    densityAnchors <- mbind(lapply(c("y2010", "y2015", "y2020"), function(ayStr) {
      mbind(lapply(speciesRuminant, function(sp) {
        proxyAy <- getLandProxy(sp, ayStr)
        d <- fillGLWZeros(glw[, ayStr, sp], proxyAy, proxyAy, isoPerCell)
        setNames(d, sp)
      }))
    }))

    densityInterp <- time_interpolate(densityAnchors,
                                      interpolated_year            = paste0("y", faoYears),
                                      integrate_interpolated_years = FALSE,
                                      extrapolation_type           = "constant")
  }

  headsList <- lapply(faoYears, function(yr) {

    yrStr   <- paste0("y", yr)
    faoGrid <- expandToGrid(faoStocks, yrStr, isoPerCell)

    speciesOut <- lapply(speciesAll, function(sp) {

      if (sp %in% speciesRuminant && landProxy != "glw") {

        landYr  <- getLandProxy(sp, yrStr)
        baseGLW <- glwInterp[, yrStr, sp]
        dens    <- densityInterp[, yrStr, sp]

        # weight = interpolated land-proxy density (see densityAnchors/densityInterp
        # above) * this year's real land proxy area, with country fallback if the
        # country total is zero; rawAlloc and outSp use baseGLW as template to keep
        # species dim3 structure
        rawAlloc <- baseGLW
        rawAlloc[, , ] <- as.numeric(dens) * as.numeric(landYr)
        getYears(rawAlloc) <- yrStr
        share <- withFallback(rawAlloc, landYr, baseGLW, isoPerCell)
        outSp <- share
        outSp[, , ] <- as.numeric(share) * as.numeric(faoGrid[, yrStr, sp])

      } else if (sp %in% speciesRuminant) {
        # landProxy == "glw": ruminants use GLW spatial shares; countries without a GLW
        # pixel of the species fall back to managed pasture + rangeland, then total land
        pastRange <- land[, yrStr, "past"] + land[, yrStr, "range"]
        totalLand <- dimSums(land[, yrStr, ], dim = 3)

        share <- withFallback(glwInterp[, yrStr, sp], pastRange, totalLand, isoPerCell)
        outSp <- share
        outSp[, , ] <- as.numeric(share) * as.numeric(faoGrid[, yrStr, sp])

      } else {
        # monogastrics (Pg, Ch, Dk): primary = raw GLW shares. GLW has zero grid signal
        # for ~70 countries per product (mostly small island states with no satellite
        # pixel coverage); without a fallback their FAO stock silently vanishes from the
        # grid instead of just being misallocated. Fall back to total land area (all nine
        # classes summed), spreading the country's stock across whatever land it has
        # rather than concentrating it into a single land class.
        totalLand <- dimSums(land[, yrStr, ], dim = 3)
        baseGLW <- glwInterp[, yrStr, sp]
        share   <- withFallback(baseGLW, totalLand, totalLand, isoPerCell)
        outSp   <- share
        outSp[, , ] <- as.numeric(share) * as.numeric(faoGrid[, yrStr, sp])
      }

      setNames(outSp, sp)
    })

    mbind(speciesOut)
  })

  # aggregate the 8 FAO/GLW species to the 5 MAgPIE kli categories using national
  # dairy and egg-layer fractions from calcAnimalStocks
  aggregateToMagpieKli <- function(heads, animalStocks, isoPerCell) {
    yrs    <- getYears(heads)
    astExt <- setdiff(yrs, getYears(animalStocks))
    if (length(astExt) > 0) {
      animalStocks <- toolHoldConstant(animalStocks, years = astExt)
    }
    ast <- animalStocks[, yrs, ]

    safeFrac <- function(num, den) {
      f <- num
      f[, , ] <- ifelse(as.numeric(den) > 0, as.numeric(num) / as.numeric(den), 0)
      f
    }

    dairyCowFrac   <- safeFrac(ast[, , "dairy cows"],
                               dimSums(ast[, , c("dairy cows", "other cattle")], dim = 3))
    dairyBufFrac   <- safeFrac(ast[, , "dairy buffalo"],
                               dimSums(ast[, , c("dairy buffalo", "other buffalo")], dim = 3))
    dairySheepFrac <- safeFrac(ast[, , "dairy sheep"],
                               dimSums(ast[, , c("dairy sheep", "other sheep")], dim = 3))
    dairyGoatFrac  <- safeFrac(ast[, , "dairy goats"],
                               dimSums(ast[, , c("dairy goats", "other goats")], dim = 3))
    layerFrac      <- safeFrac(ast[, , "poultry layers"],
                               dimSums(ast[, , c("poultry layers", "broilers")], dim = 3))
    dairyCamelFrac <- safeFrac(ast[, , "dairy camels"],
                               dimSums(ast[, , c("dairy camels", "other camels")], dim = 3))

    expandFrac <- function(frac) {
      g <- heads[, , "Ct"]
      g[, , ] <- as.numeric(frac[isoPerCell, , ])
      g
    }

    fCt <- as.numeric(expandFrac(dairyCowFrac))
    fBf <- as.numeric(expandFrac(dairyBufFrac))
    fSh <- as.numeric(expandFrac(dairySheepFrac))
    fGt <- as.numeric(expandFrac(dairyGoatFrac))
    fCh <- as.numeric(expandFrac(layerFrac))
    fCm <- as.numeric(expandFrac(dairyCamelFrac))

    nCt <- as.numeric(heads[, , "Ct"])
    nBf <- as.numeric(heads[, , "Bf"])
    nSh <- as.numeric(heads[, , "Sh"])
    nGt <- as.numeric(heads[, , "Gt"])
    nHo <- as.numeric(heads[, , "Ho"])
    nPg <- as.numeric(heads[, , "Pg"])
    nCh <- as.numeric(heads[, , "Ch"])
    nDk <- as.numeric(heads[, , "Dk"])
    nCm <- as.numeric(heads[, , "Cm"])
    nMu <- as.numeric(heads[, , "Mu"])
    nTk <- as.numeric(heads[, , "Tk"])

    tmpl <- heads[, , "Ct"]
    mkKli <- function(vals, name) {
      x <- tmpl
      x[, , ] <- vals
      setNames(x, name)
    }

    out <- mbind(
      mkKli((1 - fCt) * nCt + (1 - fBf) * nBf + (1 - fSh) * nSh + (1 - fGt) * nGt + nHo +
              (1 - fCm) * nCm + nMu,                                                    "livst_rum"),
      mkKli(fCt * nCt + fBf * nBf + fSh * nSh + fGt * nGt + fCm * nCm,                  "livst_milk"),
      mkKli(nPg,                                                                        "livst_pig"),
      mkKli((1 - fCh) * nCh + nDk + nTk,                                                "livst_chick"),
      mkKli(fCh * nCh,                                                                  "livst_egg")
    )
    out[is.na(out) | out < 0] <- 0
    out
  }

  heads <- mbind(headsList)
  heads[is.na(heads) | heads < 0] <- 0

  if (category == "magpie") {
    heads <- aggregateToMagpieKli(heads, animalStocks, isoPerCell)
    speciesAll      <- c("livst_rum", "livst_milk", "livst_pig", "livst_chick", "livst_egg")
    speciesRuminant <- c("livst_rum", "livst_milk")
  }

  # heads and within-country shares are extensive and are summed when aggregated; a
  # density is not (see the density branch below)
  aggWeight <- NULL

  if (output == "head") {

    out  <- heads
    unit <- "Million animals"

  } else if (output == "weight") {

    out  <- normalizeWithinCountry(heads, isoPerCell)
    unit <- "1"

  } else {
    landDens <- land[, getYears(heads), ]

    denomList <- lapply(speciesAll, function(sp) {
      if (!sp %in% speciesRuminant) {
        denom <- dimSums(landDens, dim = 3)
      } else if (category == "magpie" || landProxy == "glw") {
        denom <- landDens[, , "past"] + landDens[, , "range"]
      } else {
        denom <- getLandProxy(sp, getYears(heads))
      }
      setNames(denom, sp)
    })
    names(denomList) <- speciesAll

    densityList <- lapply(speciesAll, function(sp) {
      denom <- denomList[[sp]]
      denom[as.numeric(denom) < 1e-6] <- NA
      dens <- heads[, , sp]
      dens[, , ] <- as.numeric(heads[, , sp]) / as.numeric(denom)
      dens[!is.finite(as.numeric(dens))] <- 0
      setNames(dens, sp)
    })

    out  <- mbind(densityList)
    unit <- "Million animals per Mha"

    # a density must not be summed over cells: aggregating it weights each cell by the
    # land area it was divided by, which gives the area-weighted mean density
    aggWeight <- mbind(denomList)
    aggWeight[!is.finite(as.numeric(aggWeight)) | as.numeric(aggWeight) < 0] <- 0
    aggWeight <- aggWeight + 1e-10 # madrat needs a positive weight in every group
  }

  # round to 6 digits; non-zero cells that would round to 0 are kept at 1e-6,
  # while genuine zeros (e.g. the Arctic) stay zero
  tiny <- out > 0 & out < 1e-6
  out  <- round(out, 6)
  out[tiny] <- 1e-6

  return(list(
    x            = out,
    weight       = aggWeight,
    unit         = unit,
    description  = paste0("Gridded livestock (GLW3/4 spatial anchors, FAO time series). ",
                          "output='", output, "', landProxy='", landProxy,
                          "', category='", category, "'"),
    isocountries = FALSE
  ))
}
