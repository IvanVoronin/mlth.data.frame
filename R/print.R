#' @title Print Multi-Header Data Frame
#' @description
#' The method to print a \code{mlth.data.frame} object.
#'
#' @param x a \code{mlth.data.frame} object.
#' @param roundNum number of digits after decimal point to show for the numeric values.
#' Default is 3. It only works when this is an integer. In all other cases (0, NA, NULL),
#' the values are formatted according to the standard \code{\link{print.data.frame}} arguments.
#'
#' @examples
#' (L <- mlth(
#'   X = c("A", "B", "C"),
#'   Y = list(N = 1:3, M = 4:6)
#' ))
#'
#' print(L, justify = "centre")
#'
#' M <- mlth(
#'   A = rnorm(10),
#'   B = list(C = rnorm(10), D = rnorm(10))
#' )
#'
#' print(M, roundNum = 4)
#' print(M, roundNum = FALSE)
#'
#' @aliases print
#' @export

print.mlth.data.frame <- function(x, roundNum = 3, ...) {
  ## I am removing 'justify' parameter because not once I used it
  ## over the last years
  if (ncol(x) == 0) {
    cat("a multi-headed data frame with 0 columns and 0 rows\n")
    return(invisible(x))
  }

  #  X <- list(' ' = list(' ' = row.names(x), ' ' = as.list(x)))
  #  outp <- makeHeader(X, digits = digits)[-(1:2)]
  outp <- render_table(x, roundNum = roundNum, ...)

  ## Render the row names
  rn <- row.names(x)
  if (length(rn) < length(outp[[1]])) {
    rn <- c(rep("", length(outp[[1]]) - length(rn)), rn)
  }

  rn <- sprintf(
    paste0("%-", max(sapply(rn, nchar)), "s"),
    rn
  )


  ## It is possible that if the row names are too long, no colums will be printed on the next step.
  ## I truncate the row names so that the widest column fit
  max_width <- options()$width - nchar(rn[1])
  widest_col <- max(sapply(outp, function(x) nchar(x[1])))

  if (max_width < widest_col) {
    rn <- paste0(
      substr(rn, 1, options()$width - widest_col - 3),
      "..."
    )
    max_width <- options()$width - nchar(rn[1])
  }


  ## Split the lines if they are too long
  while (length(outp) > 0) {
    cw <- sapply(outp, function(x) nchar(x[1]))
    selected_cols <- which(
      Reduce(sum, cw, accumulate = TRUE) <= max_width
    )
    if (length(selected_cols) == 0) {
      selected_cols <- 1
    } ## Print at least one column to avoid infinite loop
    cat(
      unlist(
        do.call("Map", c(list(rn), outp[selected_cols], f = paste0))
      ),
      sep = "\n"
    )
    outp <- outp[-selected_cols]
  }

  if (nrow(x) == 0) {
    cat("a multi-headed data frame with 0 rows\n")
  }

  invisible(x)
}

render_table <- function(l, ...) {
  outp <- list()
  roundNum <- list(...)$roundNum
  for (i in 1:length(l)) {
    if (isAtomic(l[[i]])) {
      if (!is.na(roundNum) && (roundNum >= 0) && is.numeric(l[[i]])) {
        l[[i]] <- sprintf(paste0("%0.", roundNum, "f"), l[[i]])
      }
      outp <- c(
        outp,
        list(
          capture.output(
            print(as.data.frame(l[i]), ..., row.names = FALSE)
          )
        )
      )
    } else {
      ## Generate a header
      tab <- render_table(l[[i]], ...)
      tab_l <- length(tab)
      cw <- sapply(tab, function(x) nchar(x[1]))
      header_width <- max(
        nchar(names(l)[i]) + 1,
        sum(cw)
      )
      header <- paste0(
        c(
          " ", names(l)[i],
          rep("-", header_width - nchar(names(l)[i]) - 1)
        ),
        collapse = ""
      )

      ## If the header is wider than the table, add spaces to the last column
      if (header_width > sum(cw)) {
        tab[[tab_l]] <- sapply(
          tab[[tab_l]],
          function(x) paste0(c(x, rep(" ", header_width - sum(cw))), collapse = ""),
          USE.NAMES = FALSE
        )
        cw <- sapply(tab, function(x) nchar(x[1]))
      }

      end <- Reduce(sum, cw, accumulate = TRUE)
      start <- c(1, end[-tab_l] + 1)

      outp <- c(
        outp,
        Map(
          c,
          Map(substr, header, start, end, USE.NAMES = FALSE),
          tab
        )
      )
    }
  }

  ## Check that all columns are of the same length and pad at the top
  ## with spaces
  max_len <- max(sapply(outp, length))
  outp <- lapply(
    outp,
    function(x) {
      if (length(x) < max_len) {
        cw <- nchar(x[1])
        x <- c(
          rep(
            paste0(rep(" ", cw), collapse = ""),
            max_len - length(x)
          ),
          x
        )
      }
      return(x)
    }
  )
  return(outp)
}


# # Complete string with filler character with horizontal justification and/or
# # width
# completeString <- function(
#   s, filler = ' ',
#   width = NULL
# ) {
#   if (length(width) == 0 | nchar(s) >= width) return(s)

#   paste0(s, strrep(filler, width - nchar(s)))
# }

# # Make a header as a vector of rows
# makeHeader <- function(l, digits = NULL) {
#   # TODO: check that 'digits' works
#   first <- l[[1]]
#   if (!isAtomic(first)) {
#     # Values is the vector of character strings - rows to print
#     Values <- NULL
#     for (i in 1:length(first)) {
#       subValues <- makeHeader(first[i], digits = digits)
#       if (length(Values) == 0) {
#         Values <- subValues
#       } else {
#         # This adjusts number of rows when sub-tables are of different height
#         if (length(Values) > length(subValues)) {
#           subValues <- c(rep(format('', width = nchar(subValues[1])),
#                              length(Values) - length(subValues)), subValues)
#         } else if (length(Values) < length(subValues)) {
#           Values <- c(rep(format('', width = nchar(Values[1])),
#                           length(subValues) - length(Values)), Values)
#         }
#         Values <- paste(Values, subValues)
#       }
#     }
#     Values <- format(Values, width = nchar(names(l)))
#   } else {
#     Values <- format(
#       first,
#       digits = digits, na.encode = TRUE,
#       width = nchar(names(l)))
#   }
#   # If header is not wide enought it is completed by '-'
#   if (length(Values) == 0)
#     Name <- names(l)
#   else
#     Name <- completeString(
#       names(l),
#       filler = ifelse(is.list(first), '-', ' '),
#       width = nchar(Values[1])
#     )

#   return(c(Name, Values))
# }
