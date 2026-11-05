#' @title downloadGLW3all
#' @description Downloads Gridded Livestock of the World version 3 (GLW 3)
#'   raster data for reference year 2010 from Harvard Dataverse. Eight
#'   livestock species are available with both dasymetric and areal weighting.
#'   Source catalogue:
#'   https://www.fao.org/livestock-systems/global-distributions/en/
#' @param subtype Weighting method, livestock species, and reference year
#'   separated by underscores (\code{"<method>_<species>_2010"}). Available
#'   options:
#'   \itemize{
#'     \item Dasymetric (Da): \code{Da_Ct_2010}, \code{Da_Sh_2010},
#'       \code{Da_Pg_2010}, \code{Da_Bf_2010}, \code{Da_Ch_2010},
#'       \code{Da_Ho_2010}, \code{Da_Gt_2010}, \code{Da_Dk_2010}
#'     \item Areal (Aw): \code{Aw_Ct_2010}, \code{Aw_Sh_2010},
#'       \code{Aw_Pg_2010}, \code{Aw_Bf_2010}, \code{Aw_Ch_2010},
#'       \code{Aw_Ho_2010}, \code{Aw_Gt_2010}, \code{Aw_Dk_2010}
#'   }
#' @return A list with dataset metadata (url, doi, title, author, version,
#'   release_date, unit, description, license, reference).
#' @author Bin Lin
#' @examples
#' \dontrun{
#' downloadSource("GLW3all", subtype = "Da_Ct_2010")
#' downloadSource("GLW3all", subtype = "Aw_Sh_2010")
#' }
#' @importFrom utils download.file bibentry person
#' @importFrom withr local_options
#' @importFrom madrat toolSubtypeSelect

downloadGLW3all <- function(subtype = "Da_Ct_2010") {

  # ---------------------------------------------------------------------------
  # 2010 data: Harvard Dataverse, one dataset per species
  # DOIs:
  #   Bf: 10.7910/DVN/5U8MWI   Ct: 10.7910/DVN/GIVQ75
  #   Ch: 10.7910/DVN/SUFASB   Dk: 10.7910/DVN/ICHCBH
  #   Ho: 10.7910/DVN/7Q52MV   Gt: 10.7910/DVN/OCPH42
  #   Pg: 10.7910/DVN/33N0JG   Sh: 10.7910/DVN/BLWPZN
  # Download URL pattern: https://dataverse.harvard.edu/api/access/datafile/<id>
  # ---------------------------------------------------------------------------
  dvBase <- "https://dataverse.harvard.edu/api/access/datafile/"

  # Each entry: c(destfile, datafile_id, species_doi)
  glw3 <- list(
    Da_Ct_2010 = c("5_Ct_2010_Da.tif", "3195318", "10.7910/DVN/GIVQ75"),
    Aw_Ct_2010 = c("6_Ct_2010_Aw.tif", "3195319", "10.7910/DVN/GIVQ75"),
    Da_Sh_2010 = c("5_Sh_2010_Da.tif", "3195300", "10.7910/DVN/BLWPZN"),
    Aw_Sh_2010 = c("6_Sh_2010_Aw.tif", "3195301", "10.7910/DVN/BLWPZN"),
    Da_Pg_2010 = c("5_Pg_2010_Da.tif", "3195276", "10.7910/DVN/33N0JG"),
    Aw_Pg_2010 = c("6_Pg_2010_Aw.tif", "3195277", "10.7910/DVN/33N0JG"),
    Da_Bf_2010 = c("5_Bf_2010_Da.tif", "3195308", "10.7910/DVN/5U8MWI"),
    Aw_Bf_2010 = c("6_Bf_2010_Aw.tif", "3195309", "10.7910/DVN/5U8MWI"),
    Da_Ch_2010 = c("5_Ch_2010_Da.tif", "3195259", "10.7910/DVN/SUFASB"),
    Aw_Ch_2010 = c("6_Ch_2010_Aw.tif", "3195260", "10.7910/DVN/SUFASB"),
    Da_Ho_2010 = c("5_Ho_2010_Da.tif", "3195284", "10.7910/DVN/7Q52MV"),
    Aw_Ho_2010 = c("6_Ho_2010_Aw.tif", "3195285", "10.7910/DVN/7Q52MV"),
    Da_Gt_2010 = c("5_Gt_2010_Da.tif", "3195292", "10.7910/DVN/OCPH42"),
    Aw_Gt_2010 = c("6_Gt_2010_Aw.tif", "3195293", "10.7910/DVN/OCPH42"),
    Da_Dk_2010 = c("5_Dk_2010_Da.tif", "3195267", "10.7910/DVN/ICHCBH"),
    Aw_Dk_2010 = c("6_Dk_2010_Aw.tif", "3195268", "10.7910/DVN/ICHCBH")
  )

  if (!subtype %in% names(glw3)) {
    stop("Unknown subtype '", subtype, "'. Available: ",
         paste(names(glw3), collapse = ", "))
  }

  destfile   <- glw3[[subtype]][1]
  fileId     <- glw3[[subtype]][2]
  speciesDoi <- glw3[[subtype]][3]
  url        <- paste0(dvBase, fileId)

  local_options(timeout = 10000)
  download.file(url, destfile = destfile, mode = "wb")

  return(list(
    url          = url,
    doi          = speciesDoi,
    title        = "Gridded Livestock of the World - 2010 (GLW 3)",
    author       = person("Marius", "Gilbert"),
    version      = "2010",
    release_date = "2018",
    unit         = "heads/pixel (birds/pixel for chickens and ducks)",
    description  = paste(
      "Global gridded livestock density for", subtype,
      "at ~10 km (0.0833 degree) resolution, reference year 2010.",
      "Da = dasymetric weighting informed by Random Forest;",
      "Aw = areal weighting (uniform within census units).",
      "Units: absolute number of animals per pixel.",
      "Spatial reference: EPSG:4326 WGS84."
    ),
    license      = "CC0 1.0",
    reference    = bibentry(
      "Article",
      title   = paste("Global distribution data for cattle, buffaloes,",
                      "horses, sheep, goats, pigs, chickens and ducks in 2010"),
      author  = c(
        person("Marius", "Gilbert"),
        person("Ga\u00eblle", "Nicolas"),
        person("Giuseppina", "Cinardi"),
        person("Thomas P.", "Van Boeckel"),
        person("Sophie O.", "Vanwambeke"),
        person("G. R. William", "Wint"),
        person("Timothy P.", "Robinson")
      ),
      year    = "2018",
      journal = "Scientific Data",
      volume  = "5",
      pages   = "180227",
      doi     = "10.1038/sdata.2018.227",
      url     = "https://www.nature.com/articles/sdata2018227"
    )
  ))
}
