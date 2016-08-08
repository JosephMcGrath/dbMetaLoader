\name{loadingCleanup}
\alias{loadingCleanup}

\title{
Performs post-loading clean-up.
}

\description{
After loading in data, it is advisable to perform some maintenance on the new/updated table. Currently this is limited to vacuuming the table, but other operations may be added in later.
}

\usage{
loadingCleanup(pgConnectionIn, metadataIn)
}

\arguments{
  \item{pgConnectionIn}{A standard list of connection parameters to use.}
  \item{metadataIn}{The metadata of the file that was just loaded, to find it in the database.}
}

\value{Does not return anything.}

\author{Joseph McGrath}
