# GADM codes that are not madrat ISO countries but sit inside one, so their loss is added
# to the host rather than discarded. Measured over 2001-2025 at a 30 per cent threshold:
# XKO 19 126 ha, ZNC 1740 ha, XAD 80 ha.
gfwIsoHosts <- c(XKO = "SRB",  # Kosovo, inside Serbia in madrat's country set
                 ZNC = "CYP",  # Northern Cyprus
                 XAD = "CYP")  # Akrotiri and Dhekelia, the sovereign base areas on Cyprus

# GADM placeholders for disputed territories. Their identity is not documented anywhere I
# could find, so they are dropped rather than guessed at. Together they hold 169 451 ha,
# 0.031 per cent of global loss over 2001-2025. Naming them in no_remove_warning is what
# keeps toolCountryFill() from warning, and is the record that the drop is deliberate.
gfwIsoDropped <- c("Z01", "Z06", "Z07")

#' @title convertGFWLossByDriver
#'
#' @description Reconciles the GADM country set used by GFW with madrat's ISO set.
#'
#' The Curtis source needed no convert function because its country layer was hand-built
#' against `regionmappingH12.csv`. This one does: GADM and madrat disagree on 6 codes
#' carrying loss, and on 39 madrat countries that GFW does not list at all. Handling the
#' difference explicitly is the point - a silent inner join would drop the first group
#' without trace.
#'
#' @param x magpie object as returned by [readGFWLossByDriver()]
#' @return magpie object on madrat's ISO country set, unit Mha
#' @author Michael Crawford
#' @importFrom madrat toolCountryFill
#' @importFrom magclass getItems
#' @seealso [readGFWLossByDriver()]
#' @examples
#' \dontrun{
#' a <- readSource("GFWLossByDriver", convert = TRUE)
#' }

convertGFWLossByDriver <- function(x) {

  before <- sum(x)

  for (code in names(gfwIsoHosts)) {
    host <- gfwIsoHosts[[code]]
    if (code %in% getItems(x, 1)) {
      if (host %in% getItems(x, 1)) {
        x[host, , ] <- x[host, , ] + x[code, , ]
      } else {
        getItems(x, 1)[getItems(x, 1) == code] <- host
        next
      }
      x <- x[setdiff(getItems(x, 1), code), , ]
    }
  }

  out <- toolCountryFill(x, fill = 0, no_remove_warning = gfwIsoDropped)

  # If a future dataset version moves a country with real loss onto a code madrat does not
  # know, that must fail here rather than quietly shrink the parameter.
  lost <- (before - sum(out)) / before
  if (lost > 0.005) {
    stop("GFWLossByDriver: reconciling GADM with madrat's ISO set dropped ",
         round(100 * lost, 3), " per cent of global loss, above the 0.5 per cent tolerance. ",
         "Codes not in madrat: ",
         toString(setdiff(getItems(x, 1), madrat::getISOlist())), ".")
  }

  return(out)
}
