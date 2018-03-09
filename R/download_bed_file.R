#' @title Load file from URL to tmp
#' @author Alexey Solovyev
#' @description Load files.
#'
#' @param genome The name of the species to import the chromosomes from.
#'
#' @return The path to a file that contains the chromosome lengths.
#'
#' @usage loadChromFile <- function(genome)
#'
#' @examples
#' hg19ChromFile <- loadChromFile("hg19")
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
