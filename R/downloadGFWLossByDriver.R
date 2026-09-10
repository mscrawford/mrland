gfwDataset <- "gadm__tcl__iso_change"
gfwVersion <- "v20260424"

# Canopy density in 2000, per cent, defining which pixels count as forest for the loss
# figures. 30 is GFW's forest convention. This is a real choice and not a detail: global
# loss 2001-2025 runs from 647 Mha at a 0 per cent threshold to 334 Mha at 75 per cent.
# This file is the single home of the pin - dataset, version and threshold - and the
# download verifies that what came back is what was asked for. readGFWLossByDriver() does
# not restate the value; it takes the threshold from the data, which carries it in every
# row, so the two can never drift apart.
gfwCanopyThreshold <- 30

#' @title downloadGFWLossByDriver
#'
#' @description Downloads tree cover loss by country, year and dominant driver from the
#' Global Forest Watch data-api table `gadm__tcl__iso_change`, which cross-tabulates the
#' UMD/Hansen annual tree cover loss product with the WRI/Google DeepMind 1 km
#' dominant-driver classification of Sims et al. (2025).
#'
#' Route notes, so that nobody re-derives them:
#' * **No credential is required.** `/dataset/{d}/{v}/query` is key-gated, but
#'   `/dataset/{d}/{v}/download/csv` is open and honours the full SQL, `GROUP BY`
#'   included. If you find yourself designing around an API key, you are on the wrong
#'   endpoint.
#' * **The pinned version is the reproducibility mechanism.** GFW dataset versions are
#'   immutable (`is_mutable: false`), so a pinned URL returns the same data indefinitely.
#'   This matches every other download function in the stack; none verifies a checksum and
#'   neither does this one.
#' * **The driver map is not the one in the paper.** `wri_google_tree_cover_loss_drivers`
#'   is updated annually. The paper's map is version `v20241121` and covers 2001-2022; the
#'   cross-tab pinned here was built in April 2026 on `v1.13`, which covers 2001-2025.
#'   Regional driver shares therefore do NOT reproduce the paper's table 3 or table 5
#'   exactly, most visibly in Africa, where the split between permanent agriculture and
#'   shifting cultivation moved by about 11 percentage points.
#' * **The version list is at the top level of the dataset response**, `data.versions`, not
#'   under `data.metadata`. `is_latest` is `false` on every version of this dataset and is
#'   not evidence that a newer one exists.
#'
#' @author Michael Crawford
#' @importFrom utils download.file URLencode read.csv bibentry person
#' @importFrom withr local_options
#' @seealso [readGFWLossByDriver()]
#' @examples
#' \dontrun{
#' madrat::downloadSource("GFWLossByDriver")
#' }

downloadGFWLossByDriver <- function() {

  dims <- paste("iso, umd_tree_cover_loss__year AS year,",
                "umd_tree_cover_density_2000__threshold AS threshold")
  where <- paste("FROM data WHERE umd_tree_cover_density_2000__threshold =",
                 gfwCanopyThreshold)

  # Two aggregations of the same table. The second is not a convenience: it is the only
  # redundant identity this source has. The extract is machine-generated, so it carries
  # none of the internal cross-checks a published table does, and a truncated or corrupted
  # driver file would otherwise be indistinguishable from real data. Summing the driver
  # file over drivers must reproduce the totals file country by country and year by year,
  # which readGFWLossByDriver() checks on every read.
  queries <- list(
    "loss_by_driver.csv" = paste("SELECT", dims,
                                 ", wri_google_tree_cover_loss_drivers__driver AS driver,",
                                 "SUM(umd_tree_cover_loss__ha) AS loss_ha", where,
                                 "GROUP BY iso, year, threshold, driver"),
    "loss_totals.csv" = paste("SELECT", dims,
                              ", SUM(umd_tree_cover_loss__ha) AS loss_ha", where,
                              "GROUP BY iso, year, threshold")
  )
  headers <- c(
    "loss_by_driver.csv" = "\"iso\",\"year\",\"threshold\",\"driver\",\"loss_ha\"",
    "loss_totals.csv" = "\"iso\",\"year\",\"threshold\",\"loss_ha\""
  )

  # madrat::downloadSource() wraps this call in withr::with_options(c(warn = 2)), so any
  # warning raised in here is a hard error. Keep this function warning-clean, not merely
  # error-free.
  local_options(timeout = max(3e6, getOption("timeout")))

  for (f in names(queries)) {
    url <- paste0("https://data-api.globalforestwatch.org/dataset/", gfwDataset, "/",
                  gfwVersion, "/download/csv?sql=",
                  URLencode(queries[[f]], reserved = TRUE))
    download.file(url, f, quiet = TRUE, mode = "wb")

    # A rejected query comes back as HTTP 400 or 500 and download.file() errors on both,
    # but any other non-CSV body would pass through silently and only fail much later,
    # inside read. One header check closes that.
    header <- readLines(f, n = 1, warn = FALSE)
    if (!identical(header, unname(headers[f]))) {
      stop("GFWLossByDriver: the endpoint did not return the expected CSV for ", f,
           ". First line was: ", substr(header, 1, 200))
    }

    # Did we get the threshold we asked for? Checked here, where the pin is defined.
    got <- unique(read.csv(f, stringsAsFactors = FALSE)$threshold)
    if (!identical(got, as.integer(gfwCanopyThreshold))) {
      stop("GFWLossByDriver: asked for canopy threshold ", gfwCanopyThreshold, " but ", f,
           " carries ", toString(got), ".")
    }
  }

  authors <- c(person(c("Michelle", "J."), "Sims"),
               person("Radost", "Stanimirova"),
               person("Anton", "Raichuk"),
               person("Maxim", "Neumann"),
               person("Jessica", "Richter"),
               person("Forrest", "Follett"),
               person("James", "MacCarthy"),
               person("Kristine", "Lister"),
               person("Christopher", "Randle"),
               person("Lindsey", "Sloat"),
               person("Elizabeth", "Esipova"),
               person("Jaelah", "Jupiter"),
               person("Charlotte", "Stanton"),
               person("Drew", "Morris"),
               person(c("Christy", "M."), "Slay"),
               person("Drew", "Purves"),
               person("Nancy", "Harris"))

  description <- paste("Annual UMD/Hansen tree cover loss 2001-2025, in hectares,",
                       "cross-tabulated by country and by the WRI/Google DeepMind 1 km",
                       "dominant-driver class, at a", gfwCanopyThreshold, "per cent canopy",
                       "density threshold. Eight driver classes are present: the seven of",
                       "Sims et al. (2025) plus 'Unknown'. NOTE: the driver map underlying",
                       "this extract is the annually updated 2001-2025 version",
                       "(wri_google_tree_cover_loss_drivers v1.13), not the 2001-2022",
                       "version published with the paper, so regional driver shares do not",
                       "reproduce the paper's tables.")

  reference <- bibentry("Article",
                        title = "Global drivers of forest loss at 1 km resolution",
                        author = authors,
                        year = "2025",
                        journal = "Environmental Research Letters",
                        volume = "20",
                        number = "7",
                        pages = "074027",
                        doi = "10.1088/1748-9326/add606")

  base <- paste0("https://data-api.globalforestwatch.org/dataset/", gfwDataset, "/",
                 gfwVersion)

  return(list(title = paste("Tree cover loss by country, year and dominant driver",
                            "(GFW gadm__tcl__iso_change)"),
              description = description,
              author = authors,
              doi = "10.1088/1748-9326/add606",
              url = base,
              license = "CC BY 4.0",
              version = paste0(gfwDataset, " ", gfwVersion, " (drivers map ",
                               "wri_google_tree_cover_loss_drivers v1.13, 2001-2025)"),
              unit = "ha of tree cover loss per country, year and driver",
              reference = reference))
}
