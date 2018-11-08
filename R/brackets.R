#' @rdname brackets
#' @title Extract or replace Parts of a Multi-Header Data Frame
#' @description
#' The methods to extract parts of a multi-headed data frame.
#' Work the same way as the \code{\link{data.frame}}. The \code{[<-} and \code{[[<-}
#' are to be implemented.
#' 
#' @param x,object a \code{mlth.data.frame} object.
#' @param  i,j indices specifying elements to extract. 
#' Details in \link[base]{Extract} and \code{\link[base]{Extract.data.frame}}.
#' The list structure can be used to extract or re-arrange a multi-headed data frame,
#' see Details and Examples.
#' @param name a literal character string (use \link{backtick} when the name includes
#' spaces or special characters).
#' @param N a list which defines the \code{mlth.data.frame} structure to extract.
#' See Details section.
#' 
#' @details 
#' The function \code{namesList} returns the list representing the structure of
#' a \code{mlth.data.frame} object. The variables are represented by their names
#' (single character string or vector), the sub-tables are represented by named
#' list or character vector (when a sub-table does not include another sub-table.
#' See Examples.
#' 
#' The function \code{selectByList} selects the parts of a \code{mlth.data.frame}
#' corresponding to the list structure. \code{[[} and \code{[} call \code{selectByList}
#' when \code{i} is a list.
#' 
#' @examples 
#' L <- mlth(X = c('A', 'B', 'C'),
#' 		       Y = list(N = 1:3, M = 4:6))
#' 
#' L['Y']
#' L[['Y']]
#' L$Y ## Same as L[['Y']]
#' L$Y$M
#' 
#' L[1:2, ] ## Select rows
#' 
#' ## Select using a list structure
#' namesList(L)
#' L[[list(Y = c('N', 'M', 'N'), 'X')]]
#' selectByList(L,list(Y=c('N','M','N'),'X')) # Same
#' L[1:2, list(Y = c('N', 'M', 'N'), 'X')]
#' 
#' @export
# `[[.mlth.data.frame` <- function(x, i){
#   if (is.list(i)){
#     selectByList(x, i)
#   } else {
#     outp <- as.list(x)[[i]]
#     if (!isAtomic(outp)){
#       outp <- as.mlth.data.frame(outp, row.names = row.names(x))
#     }
#     return(outp)
#   }
# }

#' @rdname brackets
#' @export
#`$.mlth.data.frame` <- function(object, name){
#  object[[name]]
#}

selRow <- function(x, i){
  # i must be numeric
  if (isAtomic(x))
    return(x[i])
  else{
    outp <- lapply(x, selRow, i)
    structure(outp,
              names = names(x),
              class = c('mlth.data.frame', 'list'),
              row.names = row.names(x)[i])
  }
}

#' @rdname brackets
#' @export
`[.mlth.data.frame` <- function(x, i, j){
  na <- nargs()
  mi <- missing(i)
  mj <- missing(j)
  
  if (na == 1)		# A[]	-> A
    return(x)
  if (na == 2) {		# A[i]	-> as.list(A)[i]
    if (is.list(i))
      return(selectByList(x, i))
    else {
      if (is.character(i))
        i <- match(i, row.names(x))
      return(structure(as.list(x)[i],
                       names = names(x)[i],
                       class = c('mlth.data.frame', 'list'),
                       row.names = row.names(x)))
      #outp <- as.list(x)[i]
      #attributes(outp) <- attributes(x)
      #names(outp) <- names(x)[i]
      #return(outp)	
    }
  }
  
  # na == 3
  if (mi) { 
    if (mj) return(x) # A[, ]	-> A
    else return(x[j]) # A[, j]	-> A[j]
  } else { 
    if (mj) { # A[i, ]
      if (is.character(i))
        i <- match(i, row.names(x))
      outp <- selRow(x, i)
#      outp <- do.call('mlth', outp)
#      row.names(outp) <- if(length(row.names(x)) > 0 || chInd) row.names(x)[i] 
#      else NULL
      return(outp)
    } else return(x[j][i, ]) # A[i, j] -> A[j][, i]
  }
}

#' @rdname brackets
#' @export
namesList <- function(x) {
  outp <- NULL
  for (i in 1:length(x))
    if (is.list(x[[i]])) {
      subNames <- list(namesList(x[[i]]))
      names(subNames) <- names(x[i])
      outp <- c(outp, subNames)
    } else
      outp <- c(outp, names(x[i])) #! class x[i]
    return(outp)
}

#' @rdname brackets
#' @export
selectByList <- function(x, N){
  selectSublist<-function(L1, L2){
    if (length(L2) > 0) {
      outp <- NULL
      for (i in names(L2)) {
        outp[i] <- list(selectSublist(L1[[i]], L2[[i]]))
      }
      return(outp)
    } else return(L1)
  }
  
  transformNames <- function(N){
    NN <- N
    for (i in 1:length(N)) {
      Name <- names(N[i])
      if (is.list(N[[i]])) {
        NN[[i]] <- lapply(N[[i]], transformNames)
      } else if (length(Name) == 0 || nchar(Name) == 0) {
        names(NN)[i] <- N[[i]]
        NN[i] <- list(NULL)
      } else {
        listCall <- paste0('list(', paste0('`', N[[i]], '`=NULL',
                                           collapse = ','), ')')
        NN[[i]] <- eval(parse(text = listCall))
      }
    }
    return(NN)
  }
  
  xl <- as.list(x)
  
  if (is.character(N)) {
    xl <- xl[[N]]
  } else {
    N <- transformNames(N)
    xl <- selectSublist(xl, N)
  }
  
  mostattributes(xl) <- attributes(x)
#  xl <- do.call('mlth', c(xl, list(row.names = row.names(x))))
  
  return(xl)
}
