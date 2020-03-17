#' @export 
cbind.mlth.data.frame <- function(...) {
  L <- list(...)
  
  empty <- sapply(L, function(x) length(x) == 0)
  nonEmpty <-!empty
  
  if (sum(nonEmpty) == 0)
    return(mlth.data.frame())
  else if (sum(nonEmpty) == 1) {
    return(L[[which(nonEmpty)]])
  }
  
  L <- L[nonEmpty]
  
  #	hasRowNames<-sapply(L,function(x)length(row.names(x))>0)
  #	if (any(hasRowNames))
  #		if (!all(sapply(L[-1],identical,L[[1]])))
  #			warning('Some row.names do not match')
  
  outp <- as.mlth.data.frame(do.call('c', L))
  row.names(outp) <- row.names(L[[1]])
  return(outp)
}

#' @export
rbind.mlth.data.frame <- function(...) {
  L <- list(...)
  
  empty <- sapply(L, function(x) length(x) == 0)
  nonEmpty <- sapply(L, is.mlth.data.frame)
  
  if (!all(empty | nonEmpty))
    stop('All arguments must be mlth.data.frames or zero length')
  
  if (sum(nonEmpty) == 0)
    return(mlth.data.frame())
  else if (sum(nonEmpty) == 1) {
    return(L[[which(nonEmpty)]])
  }
  
  # sum(nonEmpty) >= 2
  compareStructure <- function(l1, l2) {
    if (is.list(l1) & is.list(l2)) {
      if (length(l1) != length(l2))
        return(FALSE)
      if (!identical(names(l1), names(l2)))
        return(FALSE)
      Ans <- TRUE
      for (i in 1:length(l1))
        Ans <- compareStructure(l1[[i]], l2[[i]]) & Ans
      return(Ans)
    } else if (!is.list(l1) & !is.list(l2)) return(TRUE)
    else return(FALSE)
  }
  
  L <- L[nonEmpty]
  
  equal <- sapply(L[-1], function(x) compareStructure(x, L[[1]]))
  if (!all(equal))
    stop('All mlth.data.frame arguments must have same structure')
  
  rbindLists <- function(...) {
    Map(function(...) {
      if (is.list(list(...)[[1]])) {
        Map(sys.function(), ...)
      } else unlist(list(...))
    }, ...)
  }
  
  outp <- as.mlth.data.frame(do.call('rbindLists', L))
  
  hasNames <- sapply(L, function(x) length(row.names(x)) > 0)
  if (any(hasNames) && !all(hasNames))
    for (i in which(!hasNames))
      row.names(L[[i]]) <- seq_len(nrow(L[[i]]))
  
  row.names(outp) <- unlist(sapply(L, row.names))
  
  return(outp)
}

# TODO: Check/implement cbind(A = ...), rbind(A = ...)
# TODO: Check row.names, especially when do.call('mlth.data.frame')
# TODO: Check/implement apply