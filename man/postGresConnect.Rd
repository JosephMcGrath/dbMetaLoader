\name{postGresConnect}
\alias{postGresConnect}

\title{
Creates an RPostgreSQL connection object using a standard list of connection parameters.
}

\description{
Creates an RPostgreSQL connection object based on a standard list of connection parameters.
}

\usage{
postGresConnect(pgConnectionIn, silent = FALSE)
}

\arguments{
  \item{pgConnectionIn}{A standard list of connection parameters to use.}
  \item{silent}{If progress reports/errors should be reported.}
}

\value{Returns a RPostgreSQL connection object.}

\author{Joseph McGrath}
