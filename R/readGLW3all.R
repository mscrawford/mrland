#' @title readGLW3all
#' @description Reads Gridded Livestock of the World version 3 (GLW 3)
#'   raster data for reference year 2010, downloaded from Harvard Dataverse.
#'   Eight livestock species are available with both dasymetric and areal
#'   weighting.
#'   Source catalogue:
#'   https://www.fao.org/livestock-systems/global-distributions/en/
#' @param subtype Weighting method, livestock species, and reference year
#'   (\code{"<method>_<species>_2010"}):
#'   \itemize{
#'     \item {Da}: Dasymetric weighting informed by Random Forest
#'     \item {Aw}: Areal weighting (distributed uniformly in each census unit)
#'     \itemize{
#'       \item \code{Ct}: Cattle
#'       \item \code{Sh}: Sheep
#'       \item \code{Pg}: Pigs
#'       \item \code{Bf}: Buffaloes
#'       \item \code{Ch}: Chickens
#'       \item \code{Ho}: Horses
#'       \item \code{Gt}: Goats
#'       \item \code{Dk}: Ducks
#'     }
#'   }
#' @return A magpie object with 67420 lpjcell coordinates, one year (y2010),
#'   and gridded livestock counts in heads per 0.5-degree pixel.
#' @author Bin Lin
#' @seealso \code{\link{readGLW3}}, \code{\link{correctGLW3all}}
#' @examples
#' \dontrun{
#' readSource("GLW3all", subtype = "Da_Ct_2010", convert = FALSE)
#' }
#' @importFrom terra rast aggregate extract
#' @importFrom mstools toolGetMappingCoord2Country
#' @importFrom magclass as.magpie getYears<-

readGLW3all <- function(subtype = "Da_Ct_2010") {

  file <- toolSubtypeSelect(subtype, c(
    Da_Ct_2010 = "5_Ct_2010_Da.tif",
    Aw_Ct_2010 = "6_Ct_2010_Aw.tif",
    Da_Sh_2010 = "5_Sh_2010_Da.tif",
    Aw_Sh_2010 = "6_Sh_2010_Aw.tif",
    Da_Pg_2010 = "5_Pg_2010_Da.tif",
    Aw_Pg_2010 = "6_Pg_2010_Aw.tif",
    Da_Bf_2010 = "5_Bf_2010_Da.tif",
    Aw_Bf_2010 = "6_Bf_2010_Aw.tif",
    Da_Ch_2010 = "5_Ch_2010_Da.tif",
    Aw_Ch_2010 = "6_Ch_2010_Aw.tif",
    Da_Ho_2010 = "5_Ho_2010_Da.tif",
    Aw_Ho_2010 = "6_Ho_2010_Aw.tif",
    Da_Gt_2010 = "5_Gt_2010_Da.tif",
    Aw_Gt_2010 = "6_Gt_2010_Aw.tif",
    Da_Dk_2010 = "5_Dk_2010_Da.tif",
    Aw_Dk_2010 = "6_Dk_2010_Aw.tif"
  ))

  r <- rast(file)
  r <- aggregate(r, fact = 6, fun = sum, na.rm = TRUE)

  map <- toolGetMappingCoord2Country(pretty = TRUE)
  x <- as.magpie(extract(r, map[c("lon", "lat")], ID = FALSE), spatial = 1)
  dimnames(x) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t"        = NULL,
    "data"     = subtype
  )
  getYears(x) <- "y2010"
  attr(x, "unit") <- "heads/pixel"
  return(x)
}
