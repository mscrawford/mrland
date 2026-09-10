gfwDataset <- "gadm__tcl__iso_change"
gfwVersion <- "v20260424"

# Canopy density in 2000 (per cent) defining forest for the loss figures; 30 is GFW's
# convention. Not a detail: global loss 2001-2025 is 647 Mha at 0 per cent and 334 at 75.
# Dataset, version and threshold are pinned here only; the read side takes the threshold
# from the data.
gfwCanopyThreshold <- 30

#' @title downloadGFWLossByDriver
#'
#' @description Downloads tree cover loss by country, year and dominant driver from the
#' Global Forest Watch data-api table `gadm__tcl__iso_change`, which cross-tabulates the
#' UMD/Hansen annual tree cover loss product with the WRI/Google DeepMind 1 km
#' dominant-driver classification of Sims et al. (2025).
#'
#' Route notes:
#' * `/dataset/{d}/{v}/download/csv` is open and honours the full SQL (`GROUP BY` included);
#'   only `/query` needs an API key.
#' * Dataset versions are immutable (`is_mutable: false`), so the pinned URL is the
#'   reproducibility mechanism. No checksum, as elsewhere in the stack.
#' * The driver map is the annually updated `wri_google_tree_cover_loss_drivers` v1.13
#'   (2001-2025), not the paper's v20241121 (2001-2022). Regional shares therefore do not
#'   reproduce the paper's tables; Africa's permanent/shifting split moved by about 11 pp.
#' * Versions are listed under `data.versions`; `is_latest` is `false` on all of them.
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

  # Two aggregations of the same table; the read side checks that the first sums to the
  # second, which is what makes a truncated driver file fail rather than pass.
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

  # madrat::downloadSource() runs this under warn = 2: keep it warning-clean.
  local_options(timeout = max(3e6, getOption("timeout")))

  for (f in names(queries)) {
    url <- paste0("https://data-api.globalforestwatch.org/dataset/", gfwDataset, "/",
                  gfwVersion, "/download/csv?sql=",
                  URLencode(queries[[f]], reserved = TRUE))
    download.file(url, f, quiet = TRUE, mode = "wb")

    # download.file() errors on HTTP 400/500; any other non-CSV body is caught here.
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
