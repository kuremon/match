\name{imap}
\alias{imap}
\title{Interactive mapping}
\usage{
  imap(map.that, to.that, unique.mapping = TRUE,
    perfect.match = TRUE)
}
\arguments{
  \item{map.that}{The values to map.}

  \item{to.that}{Reference values to map to.}

  \item{unique.mapping}{Should the relationship be one to
  one.}

  \item{perfect.match}{Should the matching be perfect.}
}
\value{
  The values in \code{map.that} mapped to the ones
  \code{to.that}.
}
\description{
  Map values interactively to a reference
}
\details{
  Press q to exit the process midway.
}
\examples{
\donttest{
map.that=c("volvo","toyota")
to.that=row.names(mtcars)
map=imap(map.that,to.that)
}
}

