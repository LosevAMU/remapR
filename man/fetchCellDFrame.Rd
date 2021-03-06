% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_cell_DFrame.R
\name{fetchCellDFrame}
\alias{fetchCellDFrame}
\title{Select cell line}
\usage{
fetchCellDFrame(dataFrame="", cell="vcap_shctr_r1881")
}
\arguments{
\item{dataFrame}{Data frame.}

\item{cell}{Name of line.}
}
\value{
Data Frame (class = "data.frame") of peaks of this line.
}
\description{
Function selects given cell line.
}
\examples{
myFrame <- fetchCellDFrame(dataFrame = "", cell = "vcap_shctr_r1881")

}
\author{
Alexey Solovyev
}
