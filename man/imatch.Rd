\name{imatch}
\alias{imatch}
\title{Interactive partial match}
\usage{
  imatch(search.that, in.that, ..., perfect.match = TRUE)
}
\arguments{
  \item{search.that}{The values to search for.}

  \item{in.that}{Where the values should be matched to.}

  \item{...}{Additional arguments to \code{\link{agrep}}.}
}
\value{
  The index in \code{in.that} corresponding to the ones in
  \code{search.that}.
}
\description{
  Search for partial matching between values and propose
  multiple choice to the user
}
\details{
  Press q to exit the process midway.
}
\examples{
\donttest{
search.that=c("volvo","toyota")
in.that=row.names(mtcars)
match=imatch(search.that,in.that)
}
}

