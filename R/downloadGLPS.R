#' @title downloadGLPS
#' @description Downloads Global Livestock Production System (GLPS) data from
#'   Harvard Dataverse. Three animal groups are available:
#'   \itemize{
#'     \item Chickens (2010): backyard (extensive) and intensive management
#'     \item Pigs (2010): backyard (extensive), semi-intensive, and industrial
#'     \item Ruminants (ca. 2000): categorical production system map
#'       (LG/MR/MI classes), based on Global Land Cover 2000 (GLC2k)
#'   }
#'   Source catalogues:
#'   https://www.fao.org/livestock-systems/production-systems/chicken/en/
#'   https://www.fao.org/livestock-systems/production-systems/pig/en/
#'   https://www.fao.org/livestock-systems/production-systems/ruminant/en/
#' @param subtype Animal group and management system. Available options:
#'   \itemize{
#'     \item Chicken – \code{Ch_Ext_2010} (backyard/extensive),
#'       \code{Ch_Int_2010} (intensive)
#'     \item Pig – \code{Pg_Ext_2010} (backyard/extensive),
#'       \code{Pg_Int_2010} (semi-intensive),
#'       \code{Pg_Ind_2010} (industrial/intensive)
#'     \item Ruminant – \code{Ruminant_2000} (categorical LPS raster ca. 2000:
#'       landless/LG, mixed rainfed/MR, mixed irrigated/MI classes)
#'   }
#' @return A list with dataset metadata (url, doi, title, author, version,
#'   release_date, unit, description, license, reference).
#' @author Bin Lin
#' @examples
#' \dontrun{
#' downloadSource("GLPS", subtype = "Ch_Ext_2010")
#' downloadSource("GLPS", subtype = "Pg_Ind_2010")
#' downloadSource("GLPS", subtype = "Ruminant_2000")
#' }
#' @importFrom utils download.file bibentry person
#' @importFrom withr local_options

downloadGLPS <- function(subtype = "Ch_Ext_2010") {

  dvBase <- "https://dataverse.harvard.edu/api/access/datafile/"

  # ---------------------------------------------------------------------------
  # Chicken and pig production system densities (dasymetric, heads/pixel)
  # Both species share the same Dataverse dataset: doi:10.7910/DVN/A7GQXG
  # Ch: backyard (extensive) vs. intensive management
  # Pg: backyard extensive vs. semi-intensive vs. industrial
  # ---------------------------------------------------------------------------
  monogastric <- list(
    Ch_Ext_2010 = c("06_ChExt_2010_Da.tif", "3226189"),
    Ch_Int_2010 = c("07_ChInt_2010_Da.tif", "3226193"),
    Pg_Ext_2010 = c("8_PgExt_2010_Da.tif",  "3298484"),
    Pg_Int_2010 = c("9_PgInt_2010_Da.tif",   "3298485"),
    Pg_Ind_2010 = c("10_PgInd_2010_Da.tif",  "3298486")
  )

  # ---------------------------------------------------------------------------
  # Ruminant livestock production systems (categorical raster, ca. 2000)
  # Based on Global Land Cover 2000 (GLC2k); published 2018 as v5.0.
  # LG = landless, MR = mixed rainfed, MI = mixed irrigated
  # Sub-classes: Y = temperate/boreal, A = arid/semi-arid,
  #              H = humid/sub-humid, T = tropical highlands
  # Data is packed in a 7z archive: doi:10.7910/DVN/WPDSZE
  # ---------------------------------------------------------------------------
  ruminant <- list(
    Ruminant_2000 = c("2_GlobalRuminantLPS_GIS.7z", "3227444")
  )

  allSubtypes <- c(names(monogastric), names(ruminant))
  if (!subtype %in% allSubtypes) {
    stop("Unknown subtype '", subtype, "'. Available: ",
         paste(allSubtypes, collapse = ", "))
  }

  local_options(timeout = 10000)

  if (subtype %in% names(monogastric)) {
    destfile <- monogastric[[subtype]][1]
    fileId   <- monogastric[[subtype]][2]
    url      <- paste0(dvBase, fileId)

    download.file(url, destfile = destfile, mode = "wb")

    systemDesc <- switch(subtype,
      Ch_Ext_2010 = "backyard (extensive) chicken production, small-scale dual-purpose flocks",
      Ch_Int_2010 = "intensive chicken production, specialised meat and egg operations",
      Pg_Ext_2010 = "backyard (extensive) pig production, small family scavenging units",
      Pg_Int_2010 = "semi-intensive pig production, small to medium commercial units",
      Pg_Ind_2010 = "industrial pig production, large-scale intensive units"
    )

    return(list(
      url          = url,
      doi          = "10.7910/DVN/A7GQXG",
      title        = "Global Livestock Production Systems \u2013 2010",
      author       = person("Timothy", "Robinson"),
      version      = "2010",
      release_date = "2018",
      unit         = "heads/pixel (birds/pixel for chickens)",
      description  = paste(
        "Global gridded livestock density for", systemDesc,
        "at ~10 km (0.0833 degree) resolution, reference year 2010.",
        "Dasymetric (Da) weighting informed by Random Forest.",
        "Units: absolute number of animals per pixel.",
        "Spatial reference: EPSG:4326 WGS84."
      ),
      license      = "CC BY 4.0",
      reference    = bibentry(
        "Article",
        title   = paste("Income disparities and the global distribution of",
                        "intensively farmed chicken and pigs"),
        author  = c(
          person("Marius", "Gilbert"),
          person("Giulia", "Conchedda"),
          person("Thomas P.", "Van Boeckel"),
          person("Giuseppina", "Cinardi"),
          person("Catherine", "Linard"),
          person("Ga\u00eblle", "Nicolas"),
          person("Weerapong", "Thanapongtharm"),
          person("Laura", "D'Aietti"),
          person("William", "Wint"),
          person("Scott H.", "Newman"),
          person("Timothy P.", "Robinson")
        ),
        year    = "2015",
        journal = "PLOS ONE",
        volume  = "10",
        number  = "7",
        pages   = "e0133381",
        doi     = "10.1371/journal.pone.0133381",
        url     = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0133381"
      )
    ))

  } else {
    archiveFile <- ruminant[["Ruminant_2000"]][1]
    fileId      <- ruminant[["Ruminant_2000"]][2]
    url         <- paste0(dvBase, fileId)

    download.file(url, destfile = archiveFile, mode = "wb")
    if (requireNamespace("archive", quietly = TRUE)) {
      archive::archive_extract(archiveFile)
    } else if (nchar(Sys.which("7z")) > 0) {
      system2("7z", c("x", archiveFile))
    } else if (nchar(Sys.which("7za")) > 0) {
      system2("7za", c("x", archiveFile))
    } else {
      stop("Cannot extract .7z file. Please install the 'archive' R package or 7z.")
    }
    file.remove(archiveFile)

    return(list(
      url          = url,
      doi          = "10.7910/DVN/WPDSZE",
      title        = "Global Ruminant Livestock Production Systems v5.0 \u2013 ca. 2000",
      author       = person("Timothy", "Robinson"),
      version      = "5.0",
      release_date = "2018",
      unit         = "categorical (LPS class code per pixel)",
      description  = paste(
        "Global categorical raster of ruminant livestock production systems",
        "at ~10 km (0.0833 degree) resolution, reference year ca. 2000",
        "(based on Global Land Cover 2000, GLC2k).",
        "LPS classes: LG = landless, MR = mixed rainfed, MI = mixed irrigated.",
        "Sub-classes: Y = temperate/boreal, A = arid/semi-arid,",
        "H = humid/sub-humid, T = tropical highlands.",
        "Covers cattle, buffalo, sheep and goats.",
        "Spatial reference: EPSG:4326 WGS84."
      ),
      license      = "CC BY 4.0",
      reference    = bibentry(
        "TechReport",
        title       = "Global Livestock Production Systems",
        author      = c(
          person("Timothy", "Robinson"),
          person("P. K.", "Thornton"),
          person("G.", "Franceschini"),
          person("R.", "Kruska"),
          person("F.", "Chiozza"),
          person("A.", "Notenbaert"),
          person("G.", "Cecchi"),
          person("M.", "Herrero"),
          person("M.", "Epprecht"),
          person("S.", "Fritz"),
          person("L.", "You"),
          person("G.", "Conchedda"),
          person("L.", "See")
        ),
        year        = "2011",
        institution = "FAO and ILRI",
        url         = "https://www.fao.org/livestock-systems/production-systems/en/"
      )
    ))
  }
}
