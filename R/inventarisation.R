#' @title Download Remap catalogue
#' @author Alexey Solovyev
#' @description Download or import in a data frame the Remap catalogue for
#'  transcriptions factors.
#'
#'
#' @param targetDir The name of the directory to download the catalogue in.
#' @param fileName="" The name of the file after downloading.
#' If let empty, default url names will be applied.
#' @param version="2018" The year version of the catalog 2018 or 2015.
#' @param assembly="hg38" The  genomic version of assembly hg38 or hg19.
#' @param force=FALSE If FALSE (default), then no file is overwrited and the
#' user is given confirmation message.
#' @param store=TRUE If TRUE (default) then a file is downloaded and written
#' on the disk else it is only loaded as an R object.
#'
#' @return A data frame containing the Remap genomic regions if store = FALSE
#' else the path to the catalog file.
#'
#' @usage downloadRemapCatalog(targetDir, fileName = "",
#' version = "2018", assembly = "hg38", force = FALSE, store = TRUE)
#'
#' @examples
#' remapFile <- inventarisation()
#' remap <- inventarisation(mode=version)
#'
#' @export
inventarisation <- function(mode_res="",
                            version="",
                            species="",
                            assembly="",
                            type="") {


  # from
  # myUrl <- "http://tagc.univ-mrs.fr/remap/download/remapR"
  # myFile <- "remapR.csv"
  # filePath <- file.path(myUrl, myFile)
  # # to
  # toDir <- path.expand("~/tmp")
  # dir.create(toDir, showWarnings=FALSE)
  # # download and import
  # utils::download.file(filePath, file.path(toDir, myFile))

  myFrame <- remapR::loadFromUrlToTmp()
  tmpFrame <- myFrame[myFrame$remap_version == version
                      & myFrame$species == species
                      & myFrame$assembly == assembly
                      & myFrame$data_type == type, ]
  lenTmpFrame <- nrow(tmpFrame)
  repFunction <- tmpFrame[, "URL"]

  if (mode_res == "") {
    message("You've choosen $mode_res=''. In this mode you can see the different parametres of using in this function.")
    message("Parameter $mode_res can be 'path', 'version', 'species', 'assembly' or 'type'")
    message("If you chose $mode_res='path', you have to determine all others parameters to select file .bed")
    message("If you chose $mode_res = 'version', 'species', 'assembly' or 'type', you will see the set of your choice")
    for (i in seq_len(nrow(myFrame))) {
      print(myFrame[i,1:4])
    }
  } else if (mode_res == "path" & lenTmpFrame == 0) {
    message("I cannot choose file .bed for this set of parameters")
  } else if (mode_res == "path" & lenTmpFrame > 1) {
    message("I've choose more than one file .bed for this set of parameters")
    for (i in seq_len(nrow(tmpFrame))) {
      print(tmpFrame[i,1:4])
    }
  } else if (mode_res != "path"
             & mode_res != "version"
             & mode_res != "species"
             & mode_res != "assembly"
             & mode_res != "type") {
    message("Parameter $mode_res can be 'path', 'version', 'species', 'assembly' or 'type'. Type one of them.")
  } else if (mode_res == "version") {
    message("All possible variants of version are")
    print(unique(myFrame$remap_version))
  } else if (mode_res == "species") {
    message("All possible variants of species are")
    print(unique(myFrame$species))
  } else if (mode_res == "assembly") {
    message("All possible variants of assembly are")
    print(unique(myFrame$assembly))
  } else if (mode_res == "type") {
    message("All possible variants of type are")
    print(unique(myFrame$data_type))
  }
  return(repFunction)
}
