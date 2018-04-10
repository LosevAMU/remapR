#' @title Path to choosen set
#' @author Alexey Solovyev
#' @description Function gives possibility to select the set of data.
#'
#' @param mode_res Determines the type of answer, it can be
#' 'path', 'version', 'species', 'assembly' or 'type'.
#'
#' @param version Version of ReMap.
#' @param species Name of specie.
#' @param assembly Number of assembly.
#' @param type Type of data.
#'
#' @return Path to the chosen file.
#'
#' @usage inventarisation(mode_res = "", version = "", species = "", assembly = "", type = "")
#'
#' @examples
#' remapFile <- inventarisation(mode_res = "path", version = "2018",
#' species = "Homo sapiens", assembly = "hg38", type = "All peaks")
#'
#' which_version <- inventarisation(mode = "version")
#'
#' @export
inventarisation <- function(mode_res = "",
                            version = "",
                            species = "",
                            assembly = "",
                            type = "") {


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
