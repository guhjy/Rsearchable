#' A class of searchable lists
#'
#' Define a searchable list object
#'
#' @param obj the input list to transform in a searchable list#'
#' @author Marcello Gallucci, \email{mcfanda@gmail.com}
#' @seealso \code{\link{Rsearchable}}
#' @keywords lists, searchable
#' @export


searchable<-setClass("searchable",contains="list")

setMethod("initialize", "searchable",
          function(.Object,slist=list()) {
            class(slist)<-c("searchable",class(slist))
            .names<-names.searchable(slist)
            if (is.null(.names))
              return(slist)
            if ("key" %in% .names)
              attr(slist,"key")<-"key"
            else
              attr(slist,"key")<-.names[1]
          slist<-indexes(slist)
          slist
          }
)


#' Names
#'
#'  get or set the names of an object of class Rsearchable
#'
#' @param obj an object of class Rsearchable
#' @author Marcello Gallucci, \email{mcfanda@gmail.com}
#' @seealso \code{\link{Rsearchable}}
#' @keywords lists, searchable
#' @export

names.searchable<-function(slist) {
  res<-NULL
  for (n in seq_along(slist))
    res<-c(res,names(slist[[n]]))
  unique(res)
}

setMethod("sample", "searchable",
          function(x, size, replace = FALSE, prob = NULL, criteria=NULL, fun="==") {
            if (is.null(criteria))
               sel<-x
            else
               sel<-lookup(x,criteria=criteria,fun=fun)
            if (length(sel)==0)
               return(NULL)
            samp<-base::sample(sel, size, replace = replace, prob = prob)
            searchable(samp)
})


#' Lookup a subset searchable list
#'
#' Return a subset of searchable list according to a set of criteria
#'
#' @param slist an object of class Rsearchable
#' @param criteria a list of criteria, with name equal to a Rseachable field name and value equal to value
#' @param fun a function to compare criteria names with values. Default \code{==}.
#'
#'
#'
#'
#' @author Marcello Gallucci, \email{mcfanda@gmail.com}
#' @seealso \code{\link{Rsearchable}}
#' @keywords lists, searchable
#' @export

lookup<-function(...) UseMethod("lookup")

lookup.searchable<-function(slist,criteria,fun="==") {

  funs<-rep(fun,length(criteria))
  funs<-funs[1:length(criteria)]
  names(funs)<-names(criteria)
  res<-list()
  for (n in seq_along(slist) ) {
    one<-slist[[n]]
    if   (  all(names(criteria) %in% names(one))) {
         crt2<-all(sapply(names(criteria),function(a) do.call(funs[a],list(one[[a]],criteria[[a]]))))
         if (crt2)
               res[[length(res)+1]]<-slist[[n]]
    }
  }
  res<-searchable(res)
  mainkey(res)<-mainkey(slist)
  res
}




#' Select a subset searchable list
#'
#' Return a subset of searchable list according to values of a give field
#'
#' @param slist an object of class Rsearchable
#' @param field the name of the field to select the children
#' @param values a list of values to match.
#'
#' @author Marcello Gallucci, \email{mcfanda@gmail.com}
#' @seealso \code{\link{Rsearchable}}
#' @keywords lists, searchable
#' @export

select<-function(...) UseMethod("select")

select.searchable<-function(slist,field,values) {

  res<-list()
  for (value in values)
     for (n in seq_along(slist) ) {
       one<-slist[[n]]
       if   ( field %in% names(one)) {
         crt2<-one[field]==value
       if (crt2)
        res[[length(res)+1]]<-slist[[n]]
    }
  }
  res<-searchable(res)
  mainkey(res)<-mainkey(slist)
  res
}






as.list.searchable<-function(x) {
  res<-list()
  .key<-mainkey(x)
  for (n in seq_along(x))
     res[[n]]<-x[[n]][[.key]]
  as.list(res)
}

getkey<-function(...) UseMethod("getkey")

getkey.searchable<-function(slist, key=mainkey(slist) ) {
  res<-list()
  for (n in seq_along(slist) ) {
    one<-slist[[n]]
    if (key %in% names(slist[[n]]))
         res[[length(res)+1]]<-slist[[n]][key]
  }
  searchable(res)
}

`mainkey<-`<-function(slist,...) {
  arg<-list(...)
  attr(slist,"key")<-arg$value
  slist
}
mainkey<-function(slist) {
  attr(slist,"key")
}

indexes<-function(x) {

  for (n in seq_along(x))
     x[[n]]$.index<-n
  x
}


sort.searchable<-function(x,key=mainkey(x),by=NULL,decreasing=FALSE) {
  if (! (key %in% names(x)))
    stop(paste('key "',key,'" not found in searchable list',sep = ""))
  k<-simplify2array(as.list(getkey(x,key)))
  o<-order(k,decreasing = decreasing)
  x[o]
}

unique.searchable<-function(x,key=mainkey(x),method="first") {
  .checkkey(x,key)
  records<-simplify2array(as.list(getkey(x,key)))
  uni<-sapply(unique(records), function(a) {
    crits<-list()
    crits[[key]]<-a
    a<-lookup(x,criteria = crits)
    if (length(a)>1) {
      if (method=="random")
        return(sample(a,1))
      if (method=="first")
        return(a[1])
      if (method=="last")
        return(a[length(a)])

    }
    else
      return(a)
  })
  searchable(uni)
}



##########  helper

.checkkey<-function(x,key) {
  if (! (key %in% names(x)))
    stop(paste('key "',key,'" not found in searchable list',sep = ""))
}


