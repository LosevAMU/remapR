% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_cell_files.R
\name{fetchCellFiles}
\alias{fetchCellFiles}
\title{Select cell line}
\usage{
fetchCellFiles(dirFrom="", cell="vcap_shctr_r1881")
}
\arguments{
\item{dirFrom}{Path to data, by default "~/tmp/RData/Output".}

\item{cell}{Name of line.}
}
\value{
Data Frame (class = "data.frame") of peaks of this line.
}
\description{
Function selects given cell line.
}
\examples{
myFrame <- fetchCellFiles(dirFrom = "", cell = "vcap_shctr_r1881")

}
\author{
Alexey Solovyev
}
