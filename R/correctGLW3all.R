#' @title correctGLW3all
#' @description Correct GLW3all data
#' @return Magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Bin Lin
#' @seealso
#'   \code{\link{readGLW3all}}
#' @examples
#'
#' \dontrun{
#'   readSource("GLW3all", subtype = "Da_Ct_2010", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctGLW3all <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
