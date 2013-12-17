\name{find_membership}
\alias{find_groups}
\alias{find_membership}
\title{Find adjacency groups from a distance matrix}
\usage{
  find_membership(d, limit)

  find_groups(d, limit, reduce = TRUE)
}
\arguments{
  \item{d}{A distance matrix.}

  \item{limit}{The tradeoff for the adjacency groups.}

  \item{reduce}{Should the group of size 1 be removed from
  the final list?}

  \item{d}{A distance matrix.}

  \item{limit}{The tradeoff for the adjacency groups.}

  \item{reduce}{Should the group of size 1 be removed from
  the final list?}
}
\value{
  A list of groups.

  A list of groups.
}
\description{
  Find adjacency groups from a distance matrix
}
\examples{
\donttest{
petal=iris[c("Petal.Length","Petal.Width")]
d=dist(petal)
find_groups(as.matrix(d),0.25)
}
}

