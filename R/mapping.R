#' @title Coerce correspondance table into a mapping
#' @param data a data frame
#' @param from column of \code{data} that has to be mapped.
#' @param to column of \code{data} that contains the end result.
#' @export
as.mapping=function(data,from,to){
  mapping=data[[to]]
  names(mapping)=data[[from]]
  mapping
}

#' @title Find the origin value in a mapping
#' @param mapping See \code{\link{mapping}}
#' @param find end value of the mapping.
#' @return the origin value corresponding to \code{find} in \code{mapping}.
#' @export
find.in.mapping=function(mapping,find){
  names(mapping[mapping%in%find])
}