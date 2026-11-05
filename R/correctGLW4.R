#' @title correctGLW4
#' @description Replaces NA values and any negative artefacts with zero in
#'   GLW 4 gridded livestock rasters (reference years 2015 and 2020).
#' @return Magpie object with NA and negative values replaced by 0.
#' @param x magpie object provided by the read function
#' @author Bin Lin
#' @seealso \code{\link{readGLW4}}
#' @examples
#' \dontrun{
#'   readSource("GLW4", subtype = "Da_Ct_2015", convert = "onlycorrect")
#' }
#' @importFrom madrat toolConditionalReplace

correctGLW4 <- function(x) {
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  return(x)
}
