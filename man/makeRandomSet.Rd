% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_random_set.R
\name{makeRandomSet}
\alias{makeRandomSet}
\title{Constructor of random set}
\usage{
makeRandomSet(dirFrom = "", dirTo = "", pourcentage = 0.0125)
}
\arguments{
\item{dirFrom}{The path to input files. By default "~/tmp/RData/Output".}

\item{dirTo}{The path to output file. By default "~/tmp/RData/Output/Random data".}

\item{pourcentage}{Size of random set in percent of original size.}
}
\value{
In case of success it returns the word "Done".
}
\description{
Function constructs a random set of MACS-peaks.
}
\examples{
makeRandomSet(dirFrom = "", dirTo = "", pourcentage = 0.0125)

}
\author{
Alexey Solovyev
}
