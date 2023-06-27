#' @export
kable2 <- function(x, ...) UseMethod('kable2')

#' @export
kable2.default <- function(...) {
  # Additional arguments:
  # align_first
  # footnote
  # register_output
  # name
  dots <- list(...)
  
  if (length(dots$register_output) > 0 && dots$register_output) {
    dots <- register_output_internal(...)
  }
  
  if (length(dots$x) > 0)
    x <- dots$x
  else x <- dots[[1]]
  
  if (length(dots$align_first) > 0) {
    if (length(dots$align) == 0) {
      isn <- apply(x, 2, is.numeric)
      align <- ifelse(isn, 'r', 'l')
    } else if (length(dots$align) == 1) {
      align <- rep(dots$align, ncol(x))
    }
    
    align[1:length(dots$align_first)] <- dots$align_first
    
    dots <- list(...)
    dots$align_first <- NULL
    dots$align <- align
  }
  
  if (length(dots$footnote) > 0) {
    dots_note <- dots$footnote
    note <- dots_note[[1]]
    dots$footnote <- NULL
  } else {
    note <- NULL
  }
  
  # if (length(dots$register_output > 0) && dots$register_output) {
  #   register_output(
  #     x, 
  #     name = dots$name,
  #     caption = dots$caption,
  #     note = note
  #   )
  # }
  
  dots$name <- NULL
  dots$register_output <- NULL
  
  outp <- do.call(knitr::kable, dots)

  if (length(note) > 0) {
    outp <- do.call(
      'footnote',
      c(
        list(outp),
        dots_note
      )
    )
  }
  
  outp
}

#' @export
kable2.list <- function(l, ...) {
  # l is a list of data.frames or matrices
  dots <- list(...)
  
  if (length(dots$register_output) > 0 && dots$register_output) {
    dots <- register_output_internal(l, ...)
    dots[[1]] <- NULL
  }
  
#  l <- lapply(l, as.data.frame)
  rn <- character(0)
  
  if (length(dots$row.names) == 0 || is.na(dots$row.names)) {
    if (
      !all(
        sapply(
          l, 
          function(x) 
            identical(
              row.names(x), 
              as.character(1:nrow(x))
            )
        )
      )
    ) {
      rn <- Reduce('c', lapply(l, row.names))
    }
  } else if (dots$row.names) {
    rn <- Reduce('c', lapply(l, row.names))
  }
  
  tab <- do.call('rbind', l)
  if (length(rn) > 0)
    if (is.mlth.data.frame(tab)) {
      tab <- cbind(mlth.data.frame(' ' = rn), tab)
    } else
      tab <- cbind(' ' = rn, tab)
  
  dots$row.names <- FALSE
  
  kableExtra::pack_rows(
    do.call('kable2', c(list(tab), dots)),
    index = setNames(
      sapply(l, nrow),
      names(l)
    )
  )
}

#' @export
kable2.mlth.data.frame <- function(x, ...) {
  dots <- list(...)
  
  if (length(dots$register_output) > 0 && dots$register_output) {
    dots <- register_output_internal(x, ...)
    dots[[1]] <- NULL
  }
  
  outp <- do.call(
    'kable2',
    c(
      list(behead(x)), 
      dots
    )
  )

  add_complex_header_above(
    outp, x, 
    row.names = dots$row.names
  )
}

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
  tbl_out <- as.data.frame(tbl)
  attr(tbl_out, 'header') <- list()
  attr(tbl_out, 'caption') <- attr(tbl, 'caption')
  attr(tbl_out, 'note') <- attr(tbl, 'note')
  tbl_out
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
  
  collect_leaves <- function(tree) {
    pile <- numeric(0)
    for (i in 1:length(tree)) {
      if (is.numeric(tree[[i]])) {
        leaf <- tree[[i]]
        names(leaf) <- names(tree)[i]
        pile <- c(pile, leaf)
      } else {
        pile <- c(pile, collect_leaves(tree[[i]]))
      }
    }
    
    pile
  }

  trim_tree <- function(tree) {
    chop = 0
    trimmed = list()
    nm <- names(tree)
    
    if (!any(sapply(tree, is.list)))
      return(sum(unlist(tree)))
    
    for (i in 1:length(tree)) {
      if (is.list(tree[[i]])) {
        if (chop > 0) {
          trimmed <- c(trimmed, list(' ' = chop))
          chop <- 0
        }
        l <- list(trim_tree(tree[[i]]))
        names(l) <- nm[i]
        trimmed <- c(trimmed, l)
      } else {
        chop <- chop + tree[[i]]
      }
    }
    if (chop > 0) {
      trimmed <- c(trimmed, list(' ' = chop))
      chop <- 0
    }
    
    trimmed
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
  
  attr(tbl, 'header') <- header
  attr(tbl, 'caption') <- cap
  attr(tbl, 'note') <- note
  
  tbl
}

#' @export
behead.list <- function(tbl) {
  cap <- attr(tbl, 'caption')
  note <- attr(tbl, 'note')
  
  tbl <- lapply(tbl, behead)
  
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
add_complex_header_above <- function(
  kable_input, 
  tbl, 
  row.names = NA
) {
  # adapted from from knitr code
  # https://github.com/yihui/knitr/blob/1b40794a1a93162d87252e9aa9a65876933d729b/R/table.R
  has_rownames = function(x) {
    !is.null(row.names(x)) && 
      !identical(
        row.names(x), 
        as.character(seq_len(NROW(x)))
      )
  }
    
  outp <- kable_input
  
  header <- attr(behead(tbl), 'header')
  
  
  if (length(row.names) == 0 || is.na(row.names)) {      
    if (length(header) > 0 && has_rownames(tbl)) 
      header <- lapply(header, function(x) c(' ' = 1, x))
  } else if (row.names) {
    header <- lapply(header, function(x) c(' ' = 1, x))
  }
  
  for (i in header)
    outp <- add_header_above(outp, i)

  return(outp)
}

#' @title Render a table with layered rows
#' @description 
#' Render a table with layered rows using kable. 
#' It is supposed to be a list of tables that define the pieces of the output table.
#' @param l is a list of tables.
#' @param ... are parameters passed to kable.
#' @export
kable_collapse_rows <- function(l, ...) {
  # l is a list of data.frames or matrices
  dots <- list(...)
  #  l <- lapply(l, as.data.frame)
  rn <- character(0)
  
  if (length(dots$row.names) == 0 || is.na(dots$row.names)) {
    if (
      !all(
        sapply(
          l, 
          function(x) 
            identical(
              row.names(x), 
              as.character(1:nrow(x))
            )
        )
      )
    ) {
      rn <- Reduce('c', lapply(l, row.names))
    }
  } else if (dots$row.names) {
    rn <- Reduce('c', lapply(l, row.names))
  }
  
  tab <- do.call('rbind', l)
  if (length(rn) > 0)
    if (is.mlth.data.frame(tab)) {
      tab <- cbind(mlth.data.frame(' ' = rn), tab)
    } else
      tab <- cbind(' ' = rn, tab)
  
  dots$row.names <- FALSE
  
  kableExtra::pack_rows(
    do.call('kable', c(list(tab), dots)),
    index = setNames(
      sapply(l, nrow),
      names(l)
    )
  )
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
# FIXME: Strange behavior when called from a loop
register_output <- function(tbl, name = NULL, caption = NULL, note = NULL) {
  if (!exists('OUTPUT', where = globalenv())) {
    OUTPUT <- list()    
  } else {
    OUTPUT <- get(
      'OUTPUT',
      envir = globalenv()
    )
  }
  
  attr(tbl, 'caption') <- caption
  attr(tbl, 'note') <- note
  
  if (length(name) == 0) {
    OUTPUT <- c(OUTPUT, list(tbl))
  } else {
    OUTPUT[[name]] <- tbl
  }
  assign(
    'OUTPUT',
    OUTPUT,
    envir = globalenv()
  )
  return(tbl)
}

register_output_internal <- function(...) {
  # This function accepts same arguments as kable/kable2,
  # registers output and peels the dots from unnecessary args
  dots <- list(...)

  if (length(dots$register_output > 0) && dots$register_output) {
    if (length(dots$x) > 0)
      x <- dots$x
    else x <- dots[[1]]
    
    if (length(dots$footnote) > 0) {
      dots_note <- dots$footnote
      note <- dots_note[[1]]
    } else {
      note <- NULL
    }
    
    register_output(
      x, 
      name = dots$name,
      caption = dots$caption,
      note = note
    )
  }
  
  dots$register_output <- NULL
  
  dots
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
  
  empty_names <- which(names(tblList) == '')
  if (length(empty_names) > 0)
    names(tblList)[empty_names] <- paste0('Sheet', 1:length(empty_names))
  
  for (sheet in names(tblList)) {
    curTbl <- tblList[[sheet]]
    
    # Is this a list?
    this_is_list <- 
      is.list(curTbl) && 
      !is.data.frame(curTbl) && 
      !is.mlth.data.frame(curTbl)
      
    nc <- ncol(curTbl)
    if (length(nc) == 0)
      nc <- ncol(curTbl[[1]])
    if (length(nc) == 0)
      stop('something is wrong with the table: failed compute number of rows')
    
    has_rn <- length(row.names(curTbl) > 0)
    
    addWorksheet(wb, sheet)
    startRow <- 1
    
    # Write caption ------------------------------------------------------------
    if (length(attr(curTbl, 'caption')) > 0) {
      mergeCells(
        wb, sheet,
        cols = c(1, nc + as.numeric(has_rn)),
        rows = startRow
      )
      
      writeData(
        wb, sheet, 
        as.character(attr(curTbl, 'caption')),
        startCol = 1, startRow = startRow
      )
      startRow <- startRow + 1
    }

    # Write header -------------------------------------------------------------
    # if this is mlth.data.frame
    header <- attr(curTbl, 'header')
    
    startRow <- startRow + length(header)
    
    if (length(header) > 0) {
      for (i in 1:length(header)) {
        currCol <- 1
        for (j in 1:length(header[[i]])) {
          mergeCells(
            wb, sheet,
            cols = 1:header[[i]][j] + currCol,
            rows = startRow - i
          )
          writeData(
            wb, sheet, 
            names(header[[i]])[j],
            startCol = currCol + 1,
            startRow = startRow - i
          )
          
          currCol <- currCol + header[[i]][j]
        }
      }
    }
    
    addStyle(
      wb, sheet,
      createStyle(textDecoration = 'bold'),
      rows = 1:startRow,
      cols = 1 + 1:nc,
      gridExpand = TRUE
    )
    
    # Write body ---------------------------------------------------------------
    if (!this_is_list) {
      writeData(
        wb, sheet, 
        curTbl,
        startCol = 2, 
        startRow = startRow
      )
      
      if (has_rn) {
        writeData(
          wb, sheet, 
          row.names(curTbl),
          startCol = 1,
          startRow = startRow + 1
        )
      }
      startRow <- startRow + nrow(curTbl) + 1
    } else {
      # assuming curTbl is list
      writeData(
        wb, sheet, 
        as.data.frame(t(names(curTbl[[1]]))),
        startCol = 2,
        startRow = startRow,
        colNames = FALSE
      )
      startRow <- startRow + 1
      
      for (i in 1:length(curTbl)) {
        mergeCells(
          wb, sheet,
          cols = 1:(nc + 1),
          rows = startRow
        )
        addStyle(
          wb, sheet,
          createStyle(textDecoration = 'bold'),
          cols = 1,
          rows = startRow,
          gridExpand = TRUE
        )
        writeData(
          wb, sheet, 
          names(curTbl)[i],
          startCol = 1,
          startRow = startRow
        )
        startRow <- startRow + 1
       
        writeData(
          wb, sheet, 
          curTbl[[i]],
          startCol = 2, 
          startRow = startRow,
          colNames = FALSE
        )
        
        if (length(row.names(curTbl[[i]])) > 0) {
          writeData(
            wb, sheet, 
            row.names(curTbl[[i]]),
            startCol = 1,
            startRow = startRow
          )
        }
        
        startRow <- startRow + nrow(curTbl[[i]])
      }
    }
    
    # Write note ---------------------------------------------------------------
    if (length(attr(curTbl, 'note')) > 0) {
      mergeCells(
        wb, sheet,
        cols = c(1, nc + as.numeric(has_rn)),
        rows = startRow
      )
      
      writeData(
        wb, sheet, 
        as.character(attr(curTbl, 'note')),
        startCol = 1, startRow = startRow
      )
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
  
  writer(
    tblList = x,
    file = file,
    overwrite = overwrite
  )
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
    cell_spec(
      sprintf('%0.3f', r), 
      'html', bold = p < 0.05,
      escape = FALSE,
      popover = spec_popover(
        sprintf('p = %0.3f, n = %0.0f', p, n),
        position = 'bottom')
    )
  }
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  cors <- rcorr(x, y, type = type)
  
  cors <- lapply(cors, `[`, colnames(x), colnames(y))
  
  matrix(
    Map(f, cors$r, cors$P, cors$n),
    ncol = ncol(cors$r),
    dimnames = list(
      colnames(x), 
      colnames(y)
    )
  )
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
  
  as.mlth.data.frame(
    Map(
      function(r, n, P) data.frame(r = r, n = n, p = P),
      asplit(cors$r, 2), 
      asplit(cors$n, 2), 
      asplit(cors$P, 2)
    ),
    row.names = colnames(x)
  )
}

# TODO: Write on Google Drive