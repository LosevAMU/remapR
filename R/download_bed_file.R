#' @title Loads file from URL to tmp
#' @author Alexey Solovyev
#' @description Function loads bed-files from URL to the given directory.
#'
#' @param targetDir The name of the directory to download the file in, by default "~/tmp".
#' @param version Version of ReMap.
#' @param species Name of specie.
#' @param assembly Number of assembly.
#' @param type Type of data.
#' @param force It can be FALSE or TRUE. If FALSE (default), then no file is overwrited and the
#' user is given confirmation message.
#'
#' @return The path to the downloaded file.
#'
#' @usage downloadBedFile(targetDir = "", version = "2018", species = "Homo sapiens",
#'  assembly = "hg38", type = "Test peaks", force = FALSE)
#'
#' @examples
#' downloadBedFile(targetDir = "", version = "2018", species = "Homo sapiens",
#' assembly = "hg38", type = "Test peaks", force = FALSE)
#'
#' @export
downloadBedFile <- function(targetDir = "",
                            version = "2018",
                            species = "Homo sapiens",
                            assembly = "hg38",
                            type = "Test peaks",
                            force = FALSE) {

  url <- remapR::inventarisation(mode_res="path",
                                 version=version,
                                 species=species,
                                 assembly=assembly,
                                 type=type)

  fileName <- gsub(".gz", "", tail(unlist(strsplit(url, "/")), n = 1))
  if (targetDir == "") {
    targetDir <- path.expand("~/tmp")
  }

  filePath <-file.path(targetDir,fileName)
  fileExists <- file.exists(filePath)
  input <- "Y"

  if (!force && !fileExists) {
    message("File ", fileName," will be downloaded from ", url, " .")
    input <- readline(prompt=paste("Do you want to continue Y/N : "))
    while (input != "Y" && input != "N") {
      input <- readline(prompt="Please type Y or N and press Enter : ")
    }
  }
  if (input == "Y") {
    if (fileExists && !force) {
      message("The file ", fileName, " already exists. You may want to use
                    'force = TRUE' to overwrite this file.")
      return(filePath)
    } else {
      tempZipFile <- paste(tempfile(),".bed.gz", sep = "")
      utils::download.file(url, tempZipFile)
      R.utils::gunzip(tempZipFile, filePath, overwrite = force)
      unlink(tempZipFile)
      message("A file has been created at ", filePath)
      # unlink(filePath)

    }
  }
  return(file.path(targetDir, fileName))
}
