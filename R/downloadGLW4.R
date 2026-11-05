#' @title downloadGLW4
#' @description Downloads Gridded Livestock of the World version 4 (GLW 4)
#'   raster data for reference years 2015 (Harvard Dataverse) and 2020
#'   (FAO GIS Manager). Eight livestock species are available for 2015
#'   (dasymetric and areal weighting); six species for 2020 (dasymetric only).
#'   Source catalogues:
#'   https://dataverse.harvard.edu/dataverse/glw_4
#'   https://data.apps.fao.org/catalog/dataset/9d1e149b-d63f-4213-978b-317a8eb42d02
#' @param subtype Weighting method, livestock species, and reference year
#'   separated by underscores (\code{"<method>_<species>_<year>"}). Available
#'   options:
#'   \itemize{
#'     \item 2015 – dasymetric (Da): \code{Da_Ct_2015}, \code{Da_Sh_2015},
#'       \code{Da_Pg_2015}, \code{Da_Bf_2015}, \code{Da_Ch_2015},
#'       \code{Da_Ho_2015}, \code{Da_Gt_2015}, \code{Da_Dk_2015}
#'     \item 2015 – areal (Aw): \code{Aw_Ct_2015}, \code{Aw_Sh_2015},
#'       \code{Aw_Pg_2015}, \code{Aw_Bf_2015}, \code{Aw_Ch_2015},
#'       \code{Aw_Ho_2015}, \code{Aw_Gt_2015}, \code{Aw_Dk_2015}
#'     \item 2020 – dasymetric (Da) only: \code{Da_Ct_2020}, \code{Da_Sh_2020},
#'       \code{Da_Pg_2020}, \code{Da_Bf_2020}, \code{Da_Ch_2020},
#'       \code{Da_Gt_2020}
#'   }
#' @return A list with dataset metadata: \code{url}, \code{doi}, \code{title},
#'   \code{author}, \code{version}, \code{release_date}, \code{unit},
#'   \code{description}, \code{license}, and \code{reference}.
#' @author Bin Lin
#' @examples
#' \dontrun{
#' downloadSource("GLW4", subtype = "Da_Ct_2015")
#' downloadSource("GLW4", subtype = "Da_Ct_2020")
#' }
#' @importFrom utils download.file person bibentry
#' @importFrom withr local_options
#' @importFrom madrat toolSubtypeSelect

downloadGLW4 <- function(subtype = "Da_Ct_2015") {

  # ---------------------------------------------------------------------------
  # 2015 data: Harvard Dataverse
  # https://dataverse.harvard.edu/dataverse/glw_4
  # Download URL pattern: https://dataverse.harvard.edu/api/access/datafile/<id>
  # ---------------------------------------------------------------------------
  dvBase <- "https://dataverse.harvard.edu/api/access/datafile/"

  # Each entry: c(destfile, datafile_id, species_doi)
  # Per-species DOIs on Harvard Dataverse (each species is its own dataset):
  #   Bf: 10.7910/DVN/I1WCAB   Ct: 10.7910/DVN/LHBICE
  #   Ch: 10.7910/DVN/SXHLF3   Dk: 10.7910/DVN/S9ONXV
  #   Ho: 10.7910/DVN/JJGCTX   Gt: 10.7910/DVN/YYG6ET
  #   Pg: 10.7910/DVN/CIVCPB   Sh: 10.7910/DVN/VZOYHM
  speciesNames <- c(Ct = "cattle", Bf = "buffaloes", Sh = "sheep", Gt = "goats",
                    Ho = "horses", Pg = "pigs", Ch = "chickens", Dk = "ducks")

  dv2015 <- list(
    Da_Ct_2015 = c("5_Ct_2015_Da.tif", "6769711", "10.7910/DVN/LHBICE"),
    Aw_Ct_2015 = c("6_Ct_2015_Aw.tif", "6769710", "10.7910/DVN/LHBICE"),
    Da_Sh_2015 = c("5_Sh_2015_Da.tif", "6769626", "10.7910/DVN/VZOYHM"),
    Aw_Sh_2015 = c("6_Sh_2015_Aw.tif", "6769629", "10.7910/DVN/VZOYHM"),
    Da_Pg_2015 = c("5_Pg_2015_Da.tif", "6769654", "10.7910/DVN/CIVCPB"),
    Aw_Pg_2015 = c("6_Pg_2015_Aw.tif", "6769651", "10.7910/DVN/CIVCPB"),
    Da_Bf_2015 = c("5_Bf_2015_Da.tif", "6770179", "10.7910/DVN/I1WCAB"),
    Aw_Bf_2015 = c("6_Bf_2015_Aw.tif", "6770182", "10.7910/DVN/I1WCAB"),
    Da_Ch_2015 = c("5_Ch_2015_Da.tif", "6786792", "10.7910/DVN/SXHLF3"),
    Aw_Ch_2015 = c("6_Ch_2015_Aw.tif", "6786789", "10.7910/DVN/SXHLF3"),
    Da_Ho_2015 = c("5_Ho_2015_Da.tif", "6769681", "10.7910/DVN/JJGCTX"),
    Aw_Ho_2015 = c("6_Ho_2015_Aw.tif", "6769686", "10.7910/DVN/JJGCTX"),
    Da_Gt_2015 = c("5_Gt_2015_Da.tif", "6769696", "10.7910/DVN/YYG6ET"),
    Aw_Gt_2015 = c("6_Gt_2015_Aw.tif", "6769692", "10.7910/DVN/YYG6ET"),
    Da_Dk_2015 = c("5_Dk_2015_Da.tif", "6769700", "10.7910/DVN/S9ONXV"),
    Aw_Dk_2015 = c("6_Dk_2015_Aw.tif", "6769705", "10.7910/DVN/S9ONXV")
  )

  # ---------------------------------------------------------------------------
  # 2020 data: FAO GIS Manager (Google Cloud Storage)
  # Dasymetric weighting only; horses and ducks not available for 2020.
  # https://data.apps.fao.org/catalog/iso/9d1e149b-d63f-4213-978b-317a8eb42d02
  # ---------------------------------------------------------------------------
  faoBase <- paste0("https://storage.googleapis.com/fao-gismgr-glw4-2020-data/",
                    "DATA/GLW4-2020/MAPSET/D-DA/")
  faoCatalog <- paste0("https://data.apps.fao.org/catalog/dataset/",
                       "9d1e149b-d63f-4213-978b-317a8eb42d02")

  # Each entry: c(destfile, remote_filename)
  fao2020 <- list(
    Da_Ct_2020 = c("GLW4-2020.D-DA.CTL.tif", "GLW4-2020.D-DA.CTL.tif"),
    Da_Sh_2020 = c("GLW4-2020.D-DA.SHP.tif", "GLW4-2020.D-DA.SHP.tif"),
    Da_Pg_2020 = c("GLW4-2020.D-DA.PGS.tif", "GLW4-2020.D-DA.PGS.tif"),
    Da_Bf_2020 = c("GLW4-2020.D-DA.BFL.tif", "GLW4-2020.D-DA.BFL.tif"),
    Da_Ch_2020 = c("GLW4-2020.D-DA.CHK.tif", "GLW4-2020.D-DA.CHK.tif"),
    Da_Gt_2020 = c("GLW4-2020.D-DA.GTS.tif", "GLW4-2020.D-DA.GTS.tif")
  )

  allSubtypes <- c(names(dv2015), names(fao2020))
  if (!subtype %in% allSubtypes) {
    stop("Unknown subtype '", subtype, "'. Available: ",
         paste(allSubtypes, collapse = ", "))
  }

  local_options(timeout = 10000)

  if (subtype %in% names(dv2015)) {
    destfile   <- dv2015[[subtype]][1]
    fileId     <- dv2015[[subtype]][2]
    speciesDoi <- dv2015[[subtype]][3]
    species    <- speciesNames[[strsplit(subtype, "_")[[1]][2]]]
    url        <- paste0(dvBase, fileId)

    download.file(url, destfile = destfile, mode = "wb")

    return(list(
      url          = url,
      doi          = speciesDoi,
      title        = "Gridded Livestock of the World \u2013 2015 (GLW 4)",
      author       = person("Marius", "Gilbert"),
      version      = "2015",
      release_date = "2022",
      unit         = "heads/pixel (birds/pixel for chickens and ducks)",
      description  = paste(
        "Global gridded livestock density for", subtype,
        "at ~10 km (0.0833 degree) resolution, reference year 2015.",
        "Da = dasymetric weighting informed by Random Forest;",
        "Aw = areal weighting (uniform within census units).",
        "Units: absolute number of animals per pixel.",
        "Spatial reference: EPSG:4326 WGS84."
      ),
      license      = "CC0 1.0",
      # the 2015 data are published as Dataverse datasets, one per species; the related
      # publication the datasets point to is the 2010 article (doi:10.1038/sdata.2018.227)
      reference    = bibentry(
        "Misc",
        title        = paste0("Global ", species, " distribution in 2015 (5 minutes of arc)"),
        author       = c(
          person("Marius", "Gilbert"),
          person("Giuseppina", "Cinardi"),
          person("Daniele", "Da Re"),
          person("G. R. William", "Wint"),
          person("Dominik", "Wisser"),
          person("Timothy P.", "Robinson")
        ),
        year         = "2022",
        howpublished = "Harvard Dataverse",
        doi          = speciesDoi,
        url          = paste0("https://doi.org/", speciesDoi)
      )
    ))

  } else {
    destfile   <- fao2020[[subtype]][1]
    remoteFile <- fao2020[[subtype]][2]
    url        <- paste0(faoBase, remoteFile)

    download.file(url, destfile = destfile, mode = "wb")

    return(list(
      url          = url,
      # the FAO data catalogue record for this dataset carries no DOI
      doi          = "not available",
      title        = "Gridded Livestock of the World 2020 (GLW 4)",
      author       = person("Dominik", "Wisser", email = "Dominik.Wisser@fao.org"),
      version      = "2020",
      release_date = "2024-07-15",
      unit         = "heads/km2 (birds/km2 for chickens)",
      description  = paste(
        "Global gridded livestock density for", subtype,
        "at ~10 km (0.0833 degree) resolution, reference year 2020.",
        "Dasymetric (Da) weighting uses Random Forest modelling.",
        "Units: heads/km2 (birds/km2 for chickens).",
        "Spatial reference: EPSG:4326 WGS84.",
        "Note: horses and ducks are not available for reference year 2020."
      ),
      license      = "CC BY 4.0",
      reference    = bibentry(
        "Misc",
        title        = "Gridded livestock density (Global - 2020 - 10 km) - GLW4",
        author       = c(
          person("Dominik", "Wisser"),
          person("Giuseppina", "Cinardi")
        ),
        year         = "2024",
        howpublished = "FAO Data Catalogue",
        url          = faoCatalog
      )
    ))
  }
}
