#' @title Separate table header and table body
#' @description
#' Separate table header and table body to write it to a spreadsheet or to html or to whatever.
#' It returns dataframe with a header as an `attr(tbl, 'header')``.
#' @param tbl is a `mlth.data.frame` or `data.frame`. If `tbl` is a `data.frame`, the function returns it unchanged.
#' 
#' @details Also see `unpivotr::behead()``
#'
#' @export
behead <- function(tbl) UseMethod('behead', tbl)

#' @export
behead.default <- function(tbl) {
  tbl <- as.data.frame(tbl)
  attr(tbl, 'header') <- list()
  tbl
}

#' @export
behead.mlth.data.frame <- function(tbl) {
#  header <- list()
  
  # if (!is.mlth.data.frame(tbl)) {
  #   attr(tbl, 'header') <- list()
  #   return(tbl)
  # }
  
  #  if (is.mlth.data.frame(tbl)) {
  #   make_header_tree <- function(x) {
  #     if (isAtomic(x))
  #       return(1)
  #     lapply(x, make_header_tree)
  #   }
  
  collect_leaves <- function(tree, leaves = numeric(0)) {
    for (i in 1:length(tree)) {
      if (is.numeric(tree[[i]])) {
        leaves <- c(leaves, tree[i])
      } else {
        chop <- lapply(tree[[i]], collect_leaves)
        leaves <- c(leaves, chop)
      }
    }
    unlist(leaves)
  }
  
  trim_tree <- function(tree) {
    is_leave <- !sapply(tree, is.list)
    
    # Process branches
    for (i in which(!is_leave))
      tree[i] <- trim_tree(tree[[i]])
    
    if (!any(is_leave))
      return(tree)
    
    # Process leaves
    is_leave1 <- c(FALSE, is_leave)
    is_leave2 <- c(is_leave, FALSE)
    
    start <- is_leave2 & !is_leave1
    start <- start[-length(start)]
    end <- is_leave1 & !is_leave2
    end <- end[-1]
    
    start_i <- which(start)
    end_i <- which(end)
    
    if (length(start_i) > 0) {
      for (i in 1:length(start_i)) {
        si <- start_i[i]
        ei <- end_i[i]
        tree[si] <- sum(unlist(tree[si:ei]))
      }
    }
    names(tree)[si] <- ' '
    tree <- tree[which(!is_leave | start)]
    
    tree
  }
  
  cap <- attr(tbl, 'caption')
  note <- attr(tbl, 'note')
  
  header <- list()
  ht <- rapply(tbl, function(x) return(1), how = 'list')
  
  while (any(names(ht) != ' ')) {
    header <- c(header, list(collect_leaves(ht)))
    ht <- trim_tree(ht)
  }
  
  tbl <- setNames(as.data.frame(tbl), names(header[[1]]))
  header <- header[-1]
  
  
  #   header_tree <- ht
  #   
  #   while(is.list(header_tree)) {
  #     ch <- sapply(header_tree, 
  #                  function(x) 
  #                    if (is.list(x)) sum(sapply(x, sys.function())) else x )
  #     ns <- names(ch)
  #     ns[which(ns == '')] <- ' '
  #     ch <- setNames(ch, ns)
  #     header <- c(list(ch), header)
  #     header_tree <- Reduce(c, header_tree)
  #   }
  #   
  #   tbl <- setNames(as.data.frame(tbl), names(header[[1]]))
  #   header <- header[-1]
  # }
  
  attr(tbl, 'header') <- header
  attr(tbl, 'caption') <- cap
  attr(tbl, 'note') <- note
  
  tbl
}

#' @title Add complex header above the kable table
#' @description 
#' Add complex header above the kable table. It is supposed to be a part of `knitr - kable - kableExtra` pipeline. Relies on `kableExtra::add_header_above` when `behead` returns a table with complex header.
#' (E.g., when the table is `mlth.data.frame`.) 
#' @param kable_input is whatever kable input.
#' @param tbl is the initial table.
#' @param row.names shoul we include `row.names`?
#' @export
add_complex_header_above <- function(kable_input, tbl, row.names = NA) {
  outp <- kable_input
  
  header <- attr(behead(tbl), 'header')
  
  if (length(header) > 0 && 
      ((is.na(row.names) && length(row.names(tbl)) > 0 && !identical(row.names(tbl), as.character(1:nrow(tbl)))) || row.names) 
  ) {
    header <- lapply(header, function(x) c(' ' = 1, x))
  }
  
  for (i in header)
    outp <- add_header_above(outp, i)

  return(outp)
}

#' @title Register table for the output
#' @description Save the table into a global `OUTPUT` list to write is as an output spreadsheet later.
#' @param tbl is a `data.frame` or `mlth.data.frame` or any other input supported by `\link{write.xlsx.output}`.
#' @param name is table name in the `OUTPUT` list. Can be empty.
#' @param caption is table caption as a merged cell above the table.
#' @param note is table footnote as a merged cell below the table.
#' 
#' @return `tbl` with 'caption' and 'note' attributes
#' 
#' @export
register_output <- function(tbl, name = NULL, caption = NULL, note = NULL) {
  if (!exists('OUTPUT', where = globalenv()))
    OUTPUT <<- list()
  
  attr(tbl, 'caption') <- caption
  attr(tbl, 'note') <- note
  
  if (length(name) == 0) {
    OUTPUT <<- c(OUTPUT, list(tbl))
  } else {
    OUTPUT[[name]] <<- tbl
  }
  return(tbl)
}

#' @title Write tables to xlsx file
#' @description These are the writers to use for writing the tables to an xlsx file.
#' Different writers can rely on different packages, like `openxlsx` or `xlsx`.
#' My current package of choice is `openxlsx`.
#
#' @param tblList is a list of `data.frame`s. It is assumed that the input table can have `caption` and `note` attributes
#' and may accept beheaded `mlth.data.frame` (attribute `header`).
#' @param file is the name of xlsx file.
#' @param overwrite should we overwrite the file?
#' @details
#' It is important that tblList is a true list! `data.frame` is also a list and
#' the function will throw an error if `tblList` is `data.frame`.
#' 
#' @export
xlsx.writer.openxlsx <- function(tblList, file, overwrite) {
  if (is.data.frame(tblList))
    stop('tblList must be a true list, not data.frame or mlth.data.frame')
  
  require('openxlsx')
  wb <- openxlsx::createWorkbook()
  
  if (length(names(tblList)) == 0) 
    names(tblList) <- paste('Sheet', 1:length(tblList))
  
  for (sheet in names(tblList)) {
    curTbl <- tblList[[sheet]]
    addWorksheet(wb, sheet)
    startRow <- 1
    
    # ---------------------------------------------------------------------------------------------
    # Write caption
    if (length(attr(curTbl, 'caption')) > 0) {
      mergeCells(wb, sheet,
                 cols = c(1, ncol(curTbl) + as.numeric(length(row.names(curTbl)) > 0)),
                 rows = startRow)
      
      writeData(wb, sheet, 
                attr(curTbl, 'caption'),
                startCol = 1, startRow = startRow)
      startRow <- startRow + 1
    }

    # ---------------------------------------------------------------------------------------------    
    # Write header
    header <- attr(curTbl, 'header')
    startRow <- startRow + length(header)
    
    if (length(header) > 0) {
      for (i in 1:length(header)) {
        currCol <- 1
        for (j in 1:length(header[[i]])) {
          mergeCells(wb, sheet,
                     cols = 1:header[[i]][j] + currCol,
                     rows = startRow - i)
          writeData(wb, sheet, 
                    names(header[[i]])[j],
                    startCol = currCol + 1,
                    startRow = startRow - i)
          
          currCol <- currCol + header[[i]][j]
        }
      }
    }
    
    addStyle(wb, sheet,
             createStyle(textDecoration = 'bold'),
             rows = 1:startRow,
             cols = 1 + 1:ncol(curTbl),
             gridExpand = TRUE)
    
    # ---------------------------------------------------------------------------------------------
    # Write body    
    writeData(wb, sheet, 
              curTbl,
              startCol = 2, startRow = startRow)
    if (length(row.names(curTbl)) > 0) {
      writeData(wb, sheet, 
                row.names(curTbl),
                startCol = 1,
                startRow = startRow + 1)
    }
    startRow <- startRow + nrow(curTbl) + 1
    
    # ---------------------------------------------------------------------------------------------
    # Write note
    if (length(attr(curTbl, 'note')) > 0) {
      mergeCells(wb, sheet,
                 cols = c(1, ncol(curTbl) + as.numeric(length(row.names(curTbl)) > 0)),
                 rows = startRow)
      
      writeData(wb, sheet, 
                attr(curTbl, 'note'),
                startCol = 1, startRow = startRow)
    }
  }
  
  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)
}

#' @title Write registered output tables
#' @description
#' Write the contents of `OUTPUT` list to an `xlsx` file. This function is supposed to be used 
#' at the very end of the analysis when all output tables are prepared.
#' @param file is the name of `xlsx` file.
#' @param overwrite should we overwrite the existing output file?
#' @param writer is the function that writes list of tables into an xlsx file.
#'
#' @export
write.xlsx.output <- function(file, overwrite = TRUE, writer = xlsx.writer.openxlsx) {
  if (!exists('OUTPUT', where = globalenv()))
    stop('OUTPUT does not exist in globalenv, I have nothing to write')
  else
    x <- OUTPUT
  
  x <- lapply(x, behead)
  
  writer(tblList = x,
         file = file,
         overwrite = overwrite)
}

#' @rdname cor_helpers
#' @title Render correlation table
#' @description 
#' Render correlation table either as `mlth.data.frame` or as `kable` table.
#' @param x,y are tables (`matrix`, `data.frame`).
#' @param type is type of correlation: Pearson or Spearman.
#' @details When using `kable_cors`, include the following html-code to turn on popovers:
#' `<!--html_preserve-->`
#' `<script>`
#' `$(document).ready(function(){`
#' `   $('[data-toggle="popover"]').popover();` 
#' `});`
#' `</script>`
#' `<!--/html_preserve-->`
#' 
#' @export
kable_cors <- function(x, y = x, type = c('pearson', 'spearman')) {
  require('kableExtra')
  require('Hmisc')
  
  f <- function(r, p, n) {
    cell_spec(sprintf('%0.3f', r), 
              'html', bold = p < 0.05,
              escape = FALSE,
              popover = spec_popover(sprintf('p = %0.3f, n = %0.0f', p, n),
                                     position = 'bottom'))
  }
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  cors <- rcorr(x, y, type = type)
  
  cors <- lapply(cors, `[`, colnames(x), colnames(y))
  
  matrix(Map(f, cors$r, cors$P, cors$n),
         ncol = ncol(cors$r),
         dimnames = list(colnames(x), 
                         colnames(y)))
}

#' @rdname cor_helpers
#' @export
mlth_cors <- function(x, y = x, type = c('pearson', 'spearman')) {
  require('mlth.data.frame')
  require('Hmisc')
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  cors <- rcorr(x, y, type = type)
  
  cors <- lapply(cors, `[`, colnames(x), colnames(y))
  
  as.mlth.data.frame(Map(function(r, n, P) data.frame(r = r, n = n, p = P),
                         asplit(cors$r, 2), 
                         asplit(cors$n, 2), 
                         asplit(cors$P, 2)),
                     row.names = colnames(x))
}

# TODO: Write on Google Drive