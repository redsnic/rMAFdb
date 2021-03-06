% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMAFdb_object.R
\name{to.regular.MAF}
\alias{to.regular.MAF}
\title{Convert query to a regular MAF}
\usage{
to.regular.MAF(data.flow, maf.db)
}
\arguments{
\item{data.flow}{dbplyr data flow from the original database}

\item{maf.db}{original MAF database (to retrieve MAF structure and indexes)}
}
\value{
a data frame with the resulting MAF
}
\description{
Create a tibble representing a MAF in the same
format of the original one containing only the selected variants.
This procedure loads results in RAM, use with care.
}
