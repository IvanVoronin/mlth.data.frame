#' @title Write Multi-Header Data Frame to a File
#' @description
#' Write a \code{mlth.data.frame} object to a file (Excel, csv etc). The function
#' has many options to define and apply Excel styles, write several tables 
#' into the same file or same spreadsheet etc. The methods to write into other than Excel
#' formats are to be implemented.
#' @param x a \code{mlth.data.frame} object.
#' @param file the path to the output file.
#' @param sheetName the name of the spreadsheet to write the table.
#' @param row.names a logical value indicating if the row names are to be written.
#' @param append a logical value indicating if the table should be appended to an existing file.
#' If \code{TRUE} the file is read from disk.
#' @param showNA a logical value. If \code{FALSE}, NA values will be left blank.
#' @param digits a single integer value or an integer vector indicating the number of significant digits
#' to show.
#' @param data.format a character vector with Excel data format definitions (see Data Formats section).
#' @param apa a logical value inticating whether the table should be formatted according to the APA recomendation.
#' @param date.format a character vector with date formats (according to the Excel data format
#' definitions, see Data Formats section).
#' @param coord an integer vector indicating position of the top-left cell of the table on the spreadsheet
#' (row and column).
#' @param customStyles a list of the custom styles definitions (see Custom Styles section).
#' @param noStyles a logical value, when FALSE, the Excel styles are not used.
#' 
#' @details Unlike \code{\link[xlsx]{write.xlsx}}, \code{write.mlth} does not 
#' rewrite a spreadsheet completely when \code{append=TRUE}. It can overwrite certain values
#' if the table overlaps with existing values, though.
#'
#' @section Data Formats:
#' The data formats' definitions are character strings which must correspond to the Excel data format
#' definition (detailed descriptions \href{https://support.office.com/en-us/article/Number-format-codes-5026bbd6-04bc-48cd-bf33-80f18b4eae68}{here}
#' and \href{http://www.exceltactics.com/definitive-guide-custom-number-formats-excel/}{here}).
#' Data format describes how a value will be shown in the spreadsheet, changing data format does not change the value itself.
#' \code{data.format} is a vector which defines the data formats for all variables in the table.
#' The parameters \code{digits} and \code{date.format} were introduced to define the format of
#' numeric and date variables in a convenient way.
#' For each variable, if corresponding \code{data.format} is NA, the value from \code{digits} or \code{date.format}
#' is used to define the format, depending on the variable class. If both values are NA or empty, no formatting 
#' is performed on the corresponding variable.
#' The lengths of \code{digits} and \code{date.format} must be either 1 (the values are recycled then)
#' or correspond to the number of, respectively, numeric or date variables.
#' The data format of separate cells can be modified by means of \code{customStyles}.
#' 
#' @section Custom Styles:
#' The cell style is a set of parameters defining the appearance of the cell in a spreadsheet,
#' including text color, text alignment, borders, filling, font, data format, and cell protection
#' (details in \code{\link[xlsx]{CellStyle}}). The style of single cells can be modified by means of
#' \code{customStyles}. \code{customStyles} must be a list of style definitions, each definition must
#' be a named list. Style definition comprises one required element \code{mask} which points out 
#' which cells are to be modified and one or more compulsory optional arguments defining the style.
#' 
#' \code{mask} must be a logical matrix in which TRUE corresponds to the cells to modify. The size of 
#' \code{mask} must take into account \code{row.names} and \code{col.names}. E.g., if \code{row.names=TRUE},
#' \code{ncol(mask)} must be \code{ncol(x)+1}, as the row names are written into a separate column.
#' For the convenience of mask specification a function \code{\link{mask}} was included into the package.
#' 
#' The following arguments describing the style are accepted: \code{dataFormat, alignment, border, fill, font, cellProtection}.
#' Each of them must be a named list with the arguments passed to the corresponding function. 
#' E.g., \code{alignment} is passed to the function \code{\link[xlsx]{Alignment}}.
#' Custom styles does not overwrite the style attributes which are not defined: 
#' \code{alignment=list(horizontal='ALIGN_CENTER')} will not affect the vertical alignment of the cell value.
#' See corresponding \code{\link[xlsx]{xlsx}} style functions for possible argument values.
#' 
#' @examples
#' D <- replicate(10, rnorm(20))
#' D <- as.data.frame(D)
#' 
#' output <- lapply(D, function(x) mlth(Parametric = list(Mean = mean(x),
#'                                                        SD = sd(x)),
#' 			                               `Non-parametric` = list(Median = median(x),
#' 			                                                        MAD = mad(x))))
#' output <- do.call('rbind', output)
#' row.names(output) <- names(D)
#' output
#' 
#' ## Another way to build such table
#' output2 <- sapply(D, function(x)
#'  			c(mean(x), sd(x), median(x), mad(x)))
#' output2 <- mlth(Parametric = list(Mean = output2[1, ], SD = output2[2, ]),
#'	 			        `Non-parametric` = list(Median = output2[3, ], MAD = output2[4, ]),
#' 				         row.names = names(D))
#' output2
#' 
#' ## Write the output to an Excel file
#' write.mlth(output, file = 'example1.xlsx')
#' write.mlth(output, file = 'example1.xlsx', sheetName = 'Pretty table',
#' 			      append = TRUE, digits = 2, apa = TRUE)
#' ## Write with customized styles
#' write.mlth(output, file = 'example1.xlsx', sheetName = 'Pretty table 2',
#' 			      append = TRUE, digits = 2, apa = TRUE,
#' 			      customStyles = list(list(mask = !mask(output, c = 1, h = T, rn = T),
#' 										                 alignment = list(horizontal = 'ALIGN_CENTER')),
#' 							                  list(mask = mask(output, c = 1, h = T, rn = T),
#' 										                 font = list(isBold = TRUE))))
#' write.mlth(output, file = 'example1.xlsx', sheetName = 'Pretty table 2',
#' 			      append = TRUE, digits = 2, apa = TRUE, 
#' 			      coord = c(13, 1), header = FALSE,
#' 			      customStyles = list(list(mask = !mask(output, c = 1, h = F, rn = T),
#' 										                 alignment = list(horizontal = 'ALIGN_CENTER')),
#' 							                  list(mask = mask(output, c = 1, h = T, rn = T),
#' 										                 font = list(isBold = TRUE))))
#' 
#' @export
#' @aliases write

write.mlth <- function(x, file, sheetName = 'Sheet1', row.names = TRUE, 
                       header = TRUE, append = FALSE, showNA = TRUE,
                       digits = numeric(0), data.format = character(0), 
                       apa = FALSE, date.format = character(0),
                       coord = c(1, 1), customStyles = NULL, noStyles = FALSE) {
  # TODO: choose method depending on file extension
  if (!is.mlth.data.frame(x))
    x <- as.mlth.data.frame(x)
  library('xlsx')
  if (append && file.exists(file)) {
    wb <- loadWorkbook(file)
    shts <- getSheets(wb)
    sheet <- shts[[sheetName]]
  } else {
    wb <- createWorkbook(type = ifelse(grepl('xls$', file), 'xls', 'xlsx'))
  }
  if (!exists('sheet', inherits = FALSE) || is.null(sheet))
    sheet <- createSheet(wb, sheetName = sheetName)
  
  if (nrow(x) == 0)
    row.names <- FALSE
  if (row.names)
    if (length(row.names(x)) == 0)
      x <- cbind(mlth(` ` = 1:nrow(x)), x)
    else x <- cbind(mlth(` ` = row.names(x)), x)
    
    # Functions to compute height and width of the header
    computeHeight <- function(L){
      if (is.list(L)){
        max(sapply(L, computeHeight)) + 1
      } else 0
    }	
    
    Hrows <- ifelse(header, computeHeight(x), 0)
    Hcols <- ncol(x)
    
    if (header) {
      makeHeader <- function(L) {
        if (is.list(L)) {
          hghts <- sapply(L, computeHeight)
          
          Header <- character(0)
          for (i in 1:length(L)) {
            Bottom <- makeHeader(L[[i]])
            Top <- matrix(NA, nrow = max(hghts) + 1 - nrow(Bottom), ncol = ncol(Bottom))
            Top[nrow(Top), 1] <- names(L)[i]
            Header <- cbind(Header, rbind(Top, Bottom))
          }
          return(Header)
        } else return(matrix(NA, nrow = 0, ncol = 1))
      }
      
      Header <- makeHeader(x)
      cb <- CellBlock(sheet, coord[1], coord[2], nrow(Header), ncol(Header))
      CB.setMatrixData(cb, Header, 1, 1)
      
      headerCoords <- which(!is.na(Header), arr.ind = TRUE)
      toMerge <- cbind(headerCoords, apply(headerCoords, 1, function(x) {
        i <- x[2]
        while(i < Hcols && is.na(Header[x[1], i + 1]))
          i <- i + 1
        return(i - x[2])
      }))
      toMerge <- toMerge[which(toMerge[, 3] > 0), , drop = FALSE]
      if (nrow(toMerge) > 0)
        mapply(addMergedRegion, sheet = list(sheet),
               toMerge[, 1] + coord[1] - 1, toMerge[, 1] + coord[1] - 1,
               toMerge[, 2] + coord[2] - 1, toMerge[, 2] + toMerge[, 3] + coord[2] - 1)
    }
    
    addDataFrame(as.data.frame(x), sheet, col.names = FALSE, row.names = FALSE,
                 startRow = coord[1] + Hrows, startCol = coord[2])
    
    if (noStyles) {
      saveWorkbook(wb, file)
      return(invisible())
    }
    
    colClasses <- rapply(x, class)
    # data.format = character(0) - MS Excel data format
    # digits = NULL - applied only to numeric values when data.format is not defined
    # date.format = options('xlsx.date.format') - applied only to data values
    # if (length(data.format) == 0) data.format <- default (or no data format?)
    # else for X which is.na(data.format[X]) data.format[X] <- default
    if (length(data.format) == 0)
      data.format <- rep(NA, Hcols)
    else if (length(data.format) > 0 && length(data.format) != Hcols) {
      warning('The length of data.format do not match ncol\n')
      data.format <- rep_len(data.format, Hcols)
    }
    
    # Format dates
    Dates <- which(colClasses == 'Date')
    if (length(Dates) > 0){
      if (length(date.format) == 1)
        date.format <- rep(date.format, length(Dates))
      if (length(date.format) > 0 && length(date.format) != length(Dates)) {
        warning('The length of date.format do not match the number of Data variables\n')
        date.format <- rep_len(date.format, length(Dates))		
      }
      for (i in 1:length(Dates))
        if (is.na(data.format[Dates[i]]))
          if (is.na(date.format[i]))
            data.format[Dates[i]] <- unlist(options('xlsx.date.format'))
          else data.format[Dates[i]] <- date.format[i]
    }
    
    # Format numerics
    Numerics <- which(colClasses == 'numeric')
    if (length(Numerics) > 0) {
      if (length(digits) == 1)
        digits <- rep(digits, length(Numerics))
      if (length(digits) > 0 && length(digits) != length(Numerics)) {
        warning('The length of digits do not match the number of numeric variables\n')
        digits <- rep_len(digits, length(Numerics))
      }
      for (i in 1:length(Numerics))
        if (is.na(data.format[Numerics[i]]))
          if (!is.na(digits[i]))
            data.format[Numerics[i]] <- paste0('0.', strrep(0, digits[i]))	
    }	
    
    # Define and write styles
    if (header)		
      Styles <- list(HeaderStyle = list(mask = mask(x, 1:Hrows, h = TRUE),
                                        dataFormat = NULL,
                                        alignment = list(horizontal = 'ALIGN_CENTER'),
                                        border = NULL,
                                        fill = NULL,
                                        font = NULL,
                                        cellProtection = NULL),
                     DataStyle = list(mask =! mask(x, 1:Hrows, h = TRUE),
                                      dataFormat = NULL,
                                      alignment = NULL,
                                      border = NULL,
                                      fill = NULL,
                                      font = NULL,
                                      cellProtection = NULL))
    else
      Styles <- list(DataStyle = list(mask = mask(x, 1:nrow(x)),
                                      dataFormat = NULL,
                                      alignment = NULL,
                                      border = NULL,
                                      fill = NULL,
                                      font = NULL,
                                      cellProtection = NULL))
    
    
    updateStyles <- function(Styles, mask, ...) {
      # 1. For each style from Styles: if coord in Style$coord: 
      #										- copy Style
      #										- add args to copy
      #										- remove coord from Style$coord
      #										- add copy to Styles
      newStyles <- NULL
      for (i in 1:length(Styles)) {
        if (any(Styles[[i]]$mask & mask)) {
          newStyle <- Styles[[i]]
          newStyle$mask <- Styles[[i]]$mask & mask
          
          # Auxiliary function merge two lists. All elements must be named
          mergeLists <- function(smallList, bigList) {
            if (is.list(smallList)) {
              for (i in names(smallList)) {
                if (is.null(bigList[[i]]))
                  bigList[[i]] <- list()
                bigList[[i]] <- mergeLists(smallList[[i]], bigList[[i]])
              }
              return(bigList)
            } else return(smallList)
          }
          
          newStyle <- mergeLists(list(...), newStyle)
          
          if (all(Styles[[i]]$mask == mask))
            Styles[[i]] <- newStyle
          else {
            newStyles <- c(newStyles, list(newStyle))
            Styles[[i]]$mask <- Styles[[i]]$mask & !mask
          }
        }
      }
      return(c(Styles, newStyles))
    }
    
    for (i in 1:length(data.format))
      if (!is.na(data.format[i]))
        Styles <- updateStyles(Styles, mask(x, s = list(list(Hrows + 1:nrow(x), i)), h = header),
                               dataFormat = list(x = data.format[i]))		
    
    
    if (apa) {
      if (!header)
        Styles <- updateStyles(Styles, mask(x, 1, h = FALSE),
                               border = list(position = 'TOP'))
      else if (Hrows == 1)
        Styles <- updateStyles(Styles, mask(x, 1, h = TRUE),
                               border = list(position = c('TOP', 'BOTTOM')))
      else {
        Styles <- updateStyles(Styles, mask(x, 1, h = TRUE),
                               border = list(position = 'TOP'))
        Styles <- updateStyles(Styles, mask(x, Hrows, h = TRUE),
                               border = list(position = 'BOTTOM'))
      }
      Styles <- updateStyles(Styles, mask(x, Hrows + nrow(x), h = header),
                             border = list(position = 'BOTTOM'))
    }
    
    if (length(customStyles) > 0)
      for (i in customStyles)
        if (!identical(dim(i$mask), dim(mask(x, h = header))))
          warning('The mask of a custom style does not match the size of the table')
    else
      Styles <- do.call('updateStyles', c(Styles = list(Styles), i))			
    
    
    cells <- getCells(getRows(sheet, coord[1] + 1:(nrow(x) + Hrows) - 1), 
                      coord[2] + 1:ncol(x) - 1, simplify = FALSE)
    cells <- do.call('rbind', cells)
    
    for (S in Styles) {
      for (i in cells[which(S$mask)]) {
        DF <- if (length(S$dataFormat) == 0) NULL 
        else do.call('DataFormat', S$dataFormat)
        At <- if (length(S$alignment) == 0) NULL
        else do.call('Alignment', S$alignment)
        Br <- if (length(S$border) == 0) NULL
        else do.call('Border', S$border)
        Fl <- if (length(S$fill) == 0) NULL
        else do.call('Fill', S$fill)
        Ft <- if (length(S$font) == 0) NULL
        else do.call('Font', c(wb, S$font))
        CP <- if (length(S$cellProtection) == 0) NULL
        else do.call('CellProtection', S$cellProtection)
        
        setCellStyle(i, cellStyle = CellStyle(wb,
                                              dataFormat = DF,
                                              alignment = At,
                                              border = Br,
                                              fill = Fl,
                                              font = Ft,
                                              cellProtection = CP))
      }
    }
    
    saveWorkbook(wb, file)
}

#' @rdname mask
#' @title Specify a Matrix Mask
#' @description
#' Specify a mask based on the matrix or matrix-like object.
#' 
#' @param x \code{matrix} or \code{data.frame} or \code{mlth.data.frame}.
#' @param rows,cols rows and columns to be selected.
#' @param singleCells the list of single cells (or cell sets) to be selected.
#' Each element of \code{singleCells} must be a two-element vector specifying
#' rows and columns of the cells.
#' @param header a logical value indicating if the header rows should be appended to the mask.
#' @param rn a logical value indicating if the row names should be appended to the mask. 
#' 
#' @details 
#' \code{rows}, \code{cols} and \code{singleCells} must take into account if header and row names are included.
#' E.g., \code{cols=2} indicates the second column of the matrix when \code{rn=FALSE} and the first column when \code{rn=TRUE}.
#' 
#' @examples
#' L <- mlth(X = c('A', 'B', 'C'),
#' 		       Y = list(N = 1:3, M = 4:6))
#' mask(L)
#' mask(L, h = TRUE)
#' mask(L, c = 2, rows = 3)
#' mask(L, c = 2, rows = 3, h = TRUE)
#' 
#' @export

mask <- function(x, ...){
  UseMethod('mask', x)
}

#' @rdname mask
#' @export
mask.default <- function(x, rows = numeric(0), cols = numeric(0), 
                         singleCells = NULL) {
  M <- matrix(FALSE, ncol = ncol(x), nrow = nrow(x))
  if (length(rows) > 0)
    M[rows, ] <- TRUE
  if (length(cols) > 0)
    M[, cols] <- TRUE
  if (length(singleCells) > 0)
    for (i in singleCells)
      if (is.list(i))
        M[i[[1]], i[[2]]] <- TRUE
      else
        M[i[1], i[2]] <- TRUE
  return(M)
}

#' @rdname mask
#' @export
mask.mlth.data.frame<-function(x, rows = numeric(0), cols = numeric(0), 
                               singleCells = NULL, header = FALSE, rn = FALSE) {
  computeHeight <- function(L)
    if (is.list(L)){
      max(sapply(L, computeHeight)) + 1
    } else 0
  
  M <- matrix(FALSE, ncol = ncol(x), nrow = nrow(x))
  if (header)
    M <- rbind(matrix(FALSE, ncol = ncol(M), nrow = computeHeight(x)), M)
  if (rn)
    M <- cbind(rep(FALSE, nrow(M)), M)
  
  mask.default(M, rows = rows, cols = cols, singleCells = singleCells)
}
