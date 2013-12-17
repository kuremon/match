#' @title Find adjacency groups from a distance matrix
#' @rdname find_groups
#' @param d A distance matrix.
#' @param limit The tradeoff for the adjacency groups.
#' @param reduce Should the group of size 1 be removed from the final list?
#' @return A list of groups.
#' @export
#' @examples
#' \donttest{
#' petal=iris[c("Petal.Length","Petal.Width")]
#' d=dist(petal)
#' find_groups(as.matrix(d),0.25)
#' }
#' @title Find adjacency groups from a distance matrix
#' @param d A distance matrix.
#' @param limit The tradeoff for the adjacency groups.
#' @param reduce Should the group of size 1 be removed from the final list?
#' @return A list of groups.
#' @export
#' @examples
#' \donttest{
#' petal=iris[c("Petal.Length","Petal.Width")]
#' d=dist(petal)
#' find_groups(as.matrix(d),0.25)
#' }
find_membership=function(d,limit){
  d=as.matrix(d)
  x=row.names(d)
  g=graph.adjacency(d<=limit, mode="undirected")
  cl=clusters(g)
  membership(cl)
}


#' @rdname find_groups
#' @export
find_groups=function(d,limit,reduce=TRUE){
  member=find_membership(d,limit)
  groups=split(x,member)
  if(reduce){
    groups=groups[(sapply(groups,length)>1)]
  }
  groups
}