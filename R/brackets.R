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
`[[.mlth.data.frame` <- function(x, i) {
  outp <- unclass(x)
  
  if (is.list(i)) {
    outp <- selectByList(outp, i)
  } else {
    outp <- outp[[i]]
  }
  
  if (isAtomic(outp)) {
    return(setNames(outp, row.names(outp)))
  } else {
    return(do.call('mlth.data.frame',
                   c(outp, list(row.names = row.names(x)))))
  }
}

#' @rdname brackets
#' @export
`$.mlth.data.frame` <- function(object, name) {
  object[[name]]
}

#' @export
selRow <- function(x, i){
  # i must be numeric
  if (isAtomic(x))
    return(x[i])
  else{
    return(lapply(x, selRow, i))
#    outp <- lapply(x, selRow, i)
    # structure(outp,
    #           names = names(x),
    #           class = c('mlth.data.frame', 'list'),
    #           row.names = row.names(x)[i])
  }
}

#' @export
`[.mlth.data.frame` <- function(x, i, j) {
  na <- nargs()
  
  # A[]
  if (na == 1) {
    return(x)
  }
  
  rn <- row.names(x)
  outp <- unclass(x)

  if (na == 2) { # A[i]
    if (is.list(i))
      outp <- selectByList(outp, i)
    else {
      if (is.character(i))
        i <- match(i, names(outp))
      outp <- outp[i]
    }
  } else { # A[i, j]
    if (missing(i)) {
      i <- 1:nrow(x)
    }
    if (is.character(i))
      i <- match(i, rn)
    outp <- selRow(outp, i)
    rn <- rn[i]
    
    if (missing(j)) {
      j <- 1:length(outp)
    }
    if (is.character(j)) {
      j <- match(j, names(x))
    }
    if (is.list(j))
      outp <- selectByList(outp, j)
    else
      outp <- outp[j]
  }
  
  do.call('mlth.data.frame',
          c(outp, list(row.names = rn)))
}

#' @rdname brackets
#' @export
namesList <- function(x) {
  x <- unclass(x)
  outp <- NULL
  for (i in 1:length(x))
    if (isAtomic(x[[i]])) {
      outp <- c(outp, names(x[i])) #! class x[i]
    } else {
      subNames <- list(namesList(x[[i]]))
      names(subNames) <- names(x[i])
      outp <- c(outp, subNames)
    }
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

  return(xl)
}

