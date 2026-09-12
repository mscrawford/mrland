# GADM codes that are not madrat ISO countries but sit inside one, so their loss is added
# to the host rather than discarded. Measured over 2001-2025 at a 30 per cent threshold:
# XKO 19 126 ha, ZNC 1740 ha, XAD 80 ha.
gfwIsoHosts <- c(XKO = "SRB",  # Kosovo, inside Serbia in madrat's country set
                 ZNC = "CYP",  # Northern Cyprus
                 XAD = "CYP")  # Akrotiri and Dhekelia, the sovereign base areas on Cyprus

# Undocumented GADM placeholders for disputed territories, dropped rather than guessed at. Loss
# 2001-2025: Z01, Z06 and Z07 carry 169 451 ha together, 0.031 per cent of the global total.
# Extent 2000: 6.5 Mha over all nine, 0.16 per cent, mostly Z07 and Z01.
gfwIsoDropped <- sprintf("Z%02d", 1:9)

#' @title convertGFWLossByDriver
#'
#' @description Reconciles the GADM country set used by GFW with madrat's ISO set.
#'
#' GADM and madrat disagree on 6 codes carrying loss and on 39 madrat countries GFW does
#' not list; both are handled explicitly rather than by a silent inner join.
#'
#' @param x magpie object as returned by [readGFWLossByDriver()]
#' @param subtype `"loss"` or `"extent"`, as passed to the read function; the reconciliation is
#' the same for both
#' @return magpie object on madrat's ISO country set, unit Mha
#' @author Michael Crawford
#' @importFrom madrat toolCountryFill
#' @importFrom magclass getItems
#' @seealso [readGFWLossByDriver()]
#' @examples
#' \dontrun{
#' a <- readSource("GFWLossByDriver", convert = TRUE)
#' }

convertGFWLossByDriver <- function(x, subtype = "loss") {

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

  # A future version moving real loss onto a code madrat does not know must fail here.
  lost <- (before - sum(out)) / before
  if (lost > 0.005) {
    stop("GFWLossByDriver: reconciling GADM with madrat's ISO set dropped ",
         round(100 * lost, 3), " per cent of global loss, above the 0.5 per cent tolerance. ",
         "Codes not in madrat: ",
         toString(setdiff(getItems(x, 1), madrat::getISOlist())), ".")
  }

  return(out)
}
