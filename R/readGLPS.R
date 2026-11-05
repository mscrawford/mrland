#' @title readGLPS
#' @description Reads Global Livestock Production System (GLPS) data.
#'   Chicken and pig subtypes are for reference year 2010; the ruminant
#'   subtype (Ruminant_2000) is for reference year ca. 2000.
#'   Three animal groups are available:
#'   \itemize{
#'     \item Chickens: backyard (extensive) and intensive management –
#'       continuous density raster (heads/pixel)
#'     \item Pigs: backyard (extensive), semi-intensive, and industrial –
#'       continuous density raster (heads/pixel)
#'     \item Ruminants: categorical production system map (LPS class code
#'       per pixel; aggregated to 0.5 degree by modal value)
#'   }
#' @param subtype Animal group and management system. Available
#'   options:
#'   \itemize{
#'     \item Chicken – \code{Ch_Ext_2010} (backyard/extensive),
#'       \code{Ch_Int_2010} (intensive)
#'     \item Pig – \code{Pg_Ext_2010} (backyard/extensive),
#'       \code{Pg_Int_2010} (semi-intensive),
#'       \code{Pg_Ind_2010} (industrial/intensive)
#'     \item Ruminant – \code{Ruminant_2000} (categorical LPS raster ca. 2000)
#'   }
#' @return A magpie object with 67420 lpjcell coordinates. Monogastric subtypes:
#'   heads/pixel. Ruminant_2000: categorical LPS class code per pixel.
#' @author Bin Lin
#' @examples
#' \dontrun{
#' readSource("GLPS", subtype = "Ch_Ext_2010", convert = FALSE)
#' readSource("GLPS", subtype = "Ruminant_2000", convert = FALSE)
#' }
#' @importFrom terra rast aggregate extract
#' @importFrom madrat toolSubtypeSelect
#' @importFrom mstools toolGetMappingCoord2Country
#' @importFrom magclass as.magpie getYears<-

readGLPS <- function(subtype = "Ch_Ext_2010") {

  monogastricFiles <- c(
    Ch_Ext_2010 = "06_ChExt_2010_Da.tif",
    Ch_Int_2010 = "07_ChInt_2010_Da.tif",
    Pg_Ext_2010 = "8_PgExt_2010_Da.tif",
    Pg_Int_2010 = "9_PgInt_2010_Da.tif",
    Pg_Ind_2010 = "10_PgInd_2010_Da.tif"
  )

  year <- paste0("y", substr(subtype, nchar(subtype) - 3, nchar(subtype)))

  if (subtype %in% names(monogastricFiles)) {
    file <- toolSubtypeSelect(subtype, monogastricFiles)
    r <- rast(file)
    r <- aggregate(r, fact = 6, fun = sum, na.rm = TRUE)
    unit <- "heads/pixel"
  } else if (subtype == "Ruminant_2000") {
    r <- rast("glps_gleam_61113_10km.tif")
    levels(r) <- NULL  # class codes, not labels
    r <- aggregate(r, fact = 6, fun = "modal", na.rm = TRUE)  # string: the generic needs a SpatRaster
    r[is.nan(r)] <- NA  # all-NA blocks
    unit <- "categorical (LPS class code)"
  } else {
    allSubtypes <- c(names(monogastricFiles), "Ruminant_2000")
    stop("Unknown subtype '", subtype, "'. Available: ",
         paste(allSubtypes, collapse = ", "))
  }

  map <- toolGetMappingCoord2Country(pretty = TRUE)
  x <- as.magpie(extract(r, map[c("lon", "lat")], ID = FALSE), spatial = 1)
  dimnames(x) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t"        = NULL,
    "data"     = subtype
  )
  getYears(x) <- year
  attr(x, "unit") <- unit
  return(x)
}
