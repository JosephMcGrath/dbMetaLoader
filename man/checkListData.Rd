\name{checkListData}
\alias{checkMetadata}
\alias{checkPGConnection}

\title{
Checks standard lists for completeness.
}

\description{
Checks that all the parts of a standard list that will be required are present. Does not interact with the contained data however.
}

\usage{
checkListData(dataName, dataIn, requiredFields, silent = FALSE)
checkMetadata(metadataIn, requiredMeta = c("tablenameSq", "tablenameDq", "subset", "description", "provider", "dataset", "date_generated", "license", "attribution", "date_acquired"), silent = FALSE)
checkPGConnectionfunction(pgConnectionIn, requiredData = c("dbname", "host", "port", "user", "password" ), silent = FALSE)
}

\arguments{
  \item{dataName}{The label that will be applied to the data if errors are being reported.}
  \item{dataIn}{The data to have its names assessed.}
  \item{requiredFields}{A vector of names that are required for the data input.}
  \item{silent}{If errors/progress reports should be suppressed.}
  \item{metadataIn}{The metadata list to be assessed.}
  \item{requiredMeta}{The metadata labels that are required.}
  \item{pgConnectionIn}{The PostGreSQL connection info to be assessed.}
  \item{requiredData}{The required connection info to connect to PostGreSQL}
}

\value{Boolean value, TRUE if all data is present, FALSE if it is not.}

\author{Joseph McGrath}

\examples{
pgConInfo <- buildPGConnection("SpatialDBName", "postgres", "postgres")
checkPGConnection(pgConInfo)
}
