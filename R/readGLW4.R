#' @title readGLW4
#' @description Reads Gridded Livestock of the World version 4 (GLW 4)
#'   raster data for reference years 2015 (Harvard Dataverse) and 2020
#'   (FAO GIS Manager). Eight livestock species are available for 2015
#'   with dasymetric and areal weighting; six species for 2020 with
#'   dasymetric weighting only.
#'   Source catalogues:
#'   https://dataverse.harvard.edu/dataverse/glw_4
#'   https://data.apps.fao.org/catalog/iso/9d1e149b-d63f-4213-978b-317a8eb42d02
#' @param subtype Weighting method, livestock species, and reference year
#'   (\code{"<method>_<species>_<year>"}):
#'        \itemize{
#'        \item {Da}: Dasymetric weighting informed by Random Forest
#'        \item {Aw}: Areal weighting - 2015 only
#'        \itemize{
#'        \item \code{Ch}: Chicken
#'        \item \code{Ct}: Cattle
#'        \item \code{Pg}: Pigs
#'        \item \code{Sh}: Sheep
#'        \item \code{Gt}: Goats
#'        \item \code{Ho}: Horse (2015 only)
#'        \item \code{Dk}: Ducks (2015 only)
#'        \item \code{Bf}: Buffaloes
#'        }}
#'
#' @return A magpie object with 67420 lpjcell coordinates and gridded livestock
#'   counts in heads per 0.5-degree pixel (aggregated by sum). For 2020 data,
#'   native heads/km2 values are multiplied by cell area before aggregation to
#'   ensure unit consistency with 2015 data.
#' @author David M Chen, Bin Lin
#' @seealso \code{\link{readGLW3}}, \code{\link{correctGLW4}}
#' @examples
#' \dontrun{
#' readSource("GLW4", subtype = "Da_Ct_2015", convert = FALSE)
#' readSource("GLW4", subtype = "Da_Ct_2020", convert = FALSE)
#' }
#' @importFrom terra rast aggregate extract cellSize
#' @importFrom madrat toolSubtypeSelect
#' @importFrom mstools toolGetMappingCoord2Country
#' @importFrom magclass as.magpie getYears<-

readGLW4 <- function(subtype = "Da_Ct_2015") {

  subtypes2015 <- c(
    Da_Ch_2015 = "5_Ch_2015_Da.tif",
    Da_Ct_2015 = "5_Ct_2015_Da.tif",
    Da_Pg_2015 = "5_Pg_2015_Da.tif",
    Da_Sh_2015 = "5_Sh_2015_Da.tif",
    Da_Gt_2015 = "5_Gt_2015_Da.tif",
    Da_Ho_2015 = "5_Ho_2015_Da.tif",
    Da_Dk_2015 = "5_Dk_2015_Da.tif",
    Da_Bf_2015 = "5_Bf_2015_Da.tif",
    Aw_Ch_2015 = "6_Ch_2015_Aw.tif",
    Aw_Ct_2015 = "6_Ct_2015_Aw.tif",
    Aw_Pg_2015 = "6_Pg_2015_Aw.tif",
    Aw_Sh_2015 = "6_Sh_2015_Aw.tif",
    Aw_Gt_2015 = "6_Gt_2015_Aw.tif",
    Aw_Ho_2015 = "6_Ho_2015_Aw.tif",
    Aw_Dk_2015 = "6_Dk_2015_Aw.tif",
    Aw_Bf_2015 = "6_Bf_2015_Aw.tif"
  )

  subtypes2020 <- c(
    Da_Ct_2020 = "GLW4-2020.D-DA.CTL.tif",
    Da_Sh_2020 = "GLW4-2020.D-DA.SHP.tif",
    Da_Pg_2020 = "GLW4-2020.D-DA.PGS.tif",
    Da_Bf_2020 = "GLW4-2020.D-DA.BFL.tif",
    Da_Ch_2020 = "GLW4-2020.D-DA.CHK.tif",
    Da_Gt_2020 = "GLW4-2020.D-DA.GTS.tif"
  )

  if (subtype %in% names(subtypes2015)) {
    # 2015 data: units are heads/pixel -> aggregate by sum
    file <- toolSubtypeSelect(subtype, subtypes2015)
    r <- rast(file)
    r <- aggregate(r, fact = 6, fun = sum, na.rm = TRUE)
    unit <- "heads/pixel"
  } else {
    # 2020 data: native unit is heads/km2 -> multiply by cell area to get
    # heads/pixel at native resolution, then aggregate by sum to 0.5 degree
    file <- toolSubtypeSelect(subtype, subtypes2020)
    r    <- rast(file)
    r    <- r * cellSize(r, unit = "km")
    r    <- aggregate(r, fact = 6, fun = sum, na.rm = TRUE)
    unit <- "heads/pixel"
  }

  map <- toolGetMappingCoord2Country(pretty = TRUE)
  x <- as.magpie(extract(r, map[c("lon", "lat")], ID = FALSE), spatial = 1)
  x[is.nan(x)] <- NA
  dimnames(x) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t"        = NULL,
    "data"     = subtype
  )
  getYears(x) <- paste0("y", substr(subtype, nchar(subtype) - 3, nchar(subtype)))
  attr(x, "unit") <- unit
  return(x)

}
