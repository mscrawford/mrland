#' @title correctGLPS
#' @description Replaces NA values and negative artefacts with zero for
#'   monogastric subtypes (chickens and pigs). Ruminant_2000 categorical data
#'   is left unchanged as 0 is not a valid LPS class.
#' @return Magpie object with NA and negative values replaced by 0 for
#'   monogastric subtypes; unchanged for Ruminant_2000.
#' @param x magpie object provided by the read function
#' @param subtype subtype string passed from readSource
#' @author Bin Lin
#' @seealso \code{\link{readGLPS}}
#' @examples
#' \dontrun{
#'   readSource("GLPS", subtype = "Ch_Ext_2010", convert = "onlycorrect")
#' }
#' @importFrom madrat toolConditionalReplace

correctGLPS <- function(x, subtype) {
  if (subtype != "Ruminant_2000") {
    x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  }
  return(x)
}
