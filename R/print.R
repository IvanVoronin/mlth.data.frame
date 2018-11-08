#' @title Print Multi-Header Data Frame
#' @description 
#' The method to print a \code{mlth.data.frame} object.
#' 
#' @param x a \code{mlth.data.frame} object.
#' @param digits number of digits after decimal point 
#' (default NULL, same as for \code{\link{print.data.frame}}).
#' The value is passed to \code{\link{format}} when printing the values,
#' work the same way as for \code{data.frame}.
#' @param justify justify header left, right or centre 
#' (default 'left', same as 'none').
#' 
#' @examples
#' (L <- mlth(X = c('A', 'B', 'C'),
#' 		        Y = list(N = 1:3, M = 4:6)))
#' 
#' print(L, justify = 'centre')
#' 
#' M <- mlth(A = rnorm(10),
#' 		       B=list(C = rnorm(10), D = rnorm(10)))
#' 
#' @export
#' @aliases print
print.mlth.data.frame <- function(x, digits = NULL, justify = 'left') {
  if (ncol(x) == 0) {
    cat('a multi-headed data frame with 0 columns and 0 rows\n')
    return(invisible(x))
  }

  # Complete string with filler character with horizontal justification and/or
  # width
  completeString <- function(s, filler = ' ',
                             justify = c("left", "right", "centre", "none"),
                             width=NULL) {
    if (length(width) == 0 | nchar(s) >= width) return(s)
    
    switch(justify,
           left = paste0(s, strrep(filler, width - nchar(s))),
           right = paste0(strrep(filler, width - nchar(s)), s),
           centre = paste0(strrep(filler, floor((width - nchar(s)) / 2)), s,
                           strrep(filler, ceiling((width - nchar(s)) / 2))),
           none = paste0(s, strrep(filler, width - nchar(s))))
  }

  # Make a header as a vector of rows
  makeHeader <- function(l, digits = NULL, justify = 'left') { 
    # TODO: check that 'digits' works
    if (!justify %in% c("left", "right", "centre", "none")){
      warning('Justify must be one of: "left", "right", "centre", "none".\nSetting default value "left"')
      justify <- 'left'
    }
    first <- l[[1]]
    if (!isAtomic(first)) {
      # Values is the vector of character strings - rows to print
      Values <- NULL
      for (i in 1:length(first)) {
        subValues <- makeHeader(first[i], digits = digits, justify = justify)
        if (length(Values) == 0) {
          Values <- subValues
        } else {
          # This adjusts number of rows when sub-tables are of different height
          if (length(Values) > length(subValues)) {
            subValues <- c(rep(format('', width = nchar(subValues[1])),
                               length(Values) - length(subValues)), subValues)
          } else if (length(Values) < length(subValues)) {
            Values <- c(rep(format('', width = nchar(Values[1])),
                            length(subValues) - length(Values)), Values)
          }
          Values <- paste(Values, subValues)
        }
      }
      Values <- format(Values, width = nchar(names(l)))
    } else {
      Values <- format(first,
                       digits = digits, na.encode = TRUE,
                       width = nchar(names(l)))
    }
    # If header is not wide enought it is completed by '-'
    if (length(Values) == 0)
      Name <- names(l)
    else
      Name <- completeString(names(l), filler = ifelse(is.list(first), '-', ' '),
                             width = nchar(Values[1]), justify = justify)
    return(c(Name, Values))
  }
  
  X <- list(` ` = list(` ` = row.names(x), ` ` = as.list(x)))
  outp <- makeHeader(X, digits = digits, justify = justify)[-(1:2)]

  cat(outp, sep='\n')
  if (nrow(x) == 0)
    cat('a multi-headed data frame with 0 rows\n')
  
  invisible(x)
}
