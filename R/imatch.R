#' @title Interactive partial match 
#' @description Search for partial matching between values and propose multiple choice to the user 
#' @param search.that The values to search for.
#' @param in.that Where the values should be matched to.
#' @param ... Additional arguments to \code{\link{agrep}}.
#' @return The index in \code{in.that} corresponding to the ones in \code{search.that}.
#' @details Press q to exit the process midway.
#' @export
#' @examples
#' \donttest{
#' search.that=c("volvo","toyota")
#' in.that=row.names(mtcars)
#' match=imatch(search.that,in.that)
#' }
imatch=function(search.that,in.that,...,perfect.match=TRUE){
  if(anyDuplicated(in.that)) warning("Duplicates are present in the reference vector.")
  
  if(perfect.match){
    match=match(search.that,in.that)
  }else{
    match=rep(NA_integer_,length(search.that))
  }
  
  for(k in which(is.na(match))){
    str=search.that[[k]]
    result=agrep(str,in.that,...)
    l.result=length(result)
    if(l.result==0){
      message(str," could not be matched.")
    }else{
      message(str," :")
      cat("NA",fill=TRUE,labels=paste0("{",0,"}:"))
      for(j in seq(1,l.result)){
        cat(in.that[[result[j]]],fill=TRUE,labels=paste0("{",j,"}:"))
      }
      int=readline("So ...? ")
      if(int=="q"){
        warning("imatch was exited midway.")
        return(match)
      }
      int=as.integer(int)
      if((1<=int)&(int<=l.result))match[k]=result[int]
    }
  }
  match
}

#' @title Interactive mapping 
#' @description Map values interactively to a reference
#' @param map.that The values to map.
#' @param to.that Reference values to map to.
#' @param unique.mapping Should the relationship be one to one.
#' @param perfect.match Should the matching be perfect.
#' @return The values in \code{map.that} mapped to the ones \code{to.that}.
#' @details Press q to exit the process midway.
#' @export
#' @examples
#' \donttest{
#' map.that=c("volvo","toyota")
#' to.that=row.names(mtcars)
#' map=imap(map.that,to.that)
#' }
imap=function(map.that,to.that,unique.mapping=TRUE,perfect.match=TRUE){
  if(anyDuplicated(to.that)) warning("Duplicates are present in the reference vector.")
  
  if(perfect.match){
    map=match(map.that,to.that)
  }else{
    map=rep(NA_integer_,length(map.that))
  }
  
  prop=seq(to.that)
  for(k in which(is.na(map))){
    str=map.that[[k]]
    l.prop=length(prop)
    if(l.prop==0){
      message(str," could not be matched.")
    }else{
      message(str," :")
      cat("NA",fill=TRUE,labels=paste0("{",0,"}:"))
      for(j in seq(1,l.prop)){
        cat(to.that[[prop[j]]],fill=TRUE,labels=paste0("{",j,"}:"))
      }
      int=readline("So ...? ")
      if(int=="q"){
        warning("imatch was exited midway.")
        return(map)
      }
      int=as.integer(int)
      if((1<=int)&(int<=l.prop)){
        map[k]=prop[int]
        if(unique.mapping)prop=prop[-int]
      }
    }
  }
  map
}