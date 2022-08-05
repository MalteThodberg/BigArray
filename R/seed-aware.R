#### Check ####

.colsummary <- function(x,
                        rows = NULL, cols = NULL,
                        na.rm = FALSE, useNames = NA){
    # Check for package
    if(isFALSE(requireNamespace("biganalytics", quietly = TRUE))){
        stop("The seed-aware colSums2 method for BigArray requires the biganalytics package")
    }

    # Don't accept rows
    if(!is.null(rows)){
        stop("The seed-aware colSums2 method for BigArray does not support the rows argument")
    }

    # Don't accept rows
    if(!is.na(useNames)){
        stop("The seed-aware colSums2 method for BigArray does not support the useNames argument")
    }

    # Return big.matrix
    attach.big.matrix(paste0(x@filepath, ".desc.txt"),
                      readonly = TRUE)
}

#' Matrix summary stats for BigArray
#'
#' "Seed-aware" matrix summary statistics for BigArray. These should normally not
#' be called directly, but rather used via the DelayedMatrixStats package: See
#' examples below.
#'
#' @param x BigArraySeed.
#' @param rows Not supported for BigArray.
#' @param cols integer, character or logical: Subset of columns to use.
#' @param na.rm logical: Whether do disregards missing values.
#' @param useNames Not supported for BigArray.
#'
#' @note Requires the biganalytics package.
#'
#' @return A vector of column summary statistics (for colRanges a matrix).
#' @examples
#' library(DelayedMatrixStats)
#'
#' # Parallel options
#' DelayedArray:::set_verbose_block_processing(TRUE)
#' setAutoBlockSize(1e8) # Silly small block size to see what is going on
#'
#' # Write a large matrix to a BigArray
#' m <- replicate(n=10, rnorm(10000))
#' ba <- writeBigArray(m)
#'
#' # Seed aware method
#' colSums2(ba)
#'
#' # Force DelayedMatrixStats to use block processing
#' colSums2(ba, force_block_processing = TRUE)
#'
#' # The seed-aware method is only used when the input is a BigArray. E.g. first
#' # subsetting a BigArray will demote it to a DelayedArray, which can only use
#' # block-processing
#' ba_subset <- ba[1:100, 1:5]
#' class(ba_subset) # No longer a BigArray
#' colSums2(ba_subset) # Block-processed
#'
#' # All seed aware
#' colSums2(ba)
#' colMins(ba)
#' colMaxs(ba)
#' colProds(ba)
#' colMeans2(ba)
#' colVars(ba)
#' colSds(ba)
#' colRanges(ba)
#' @name seed-aware
NULL

#### DelayedMatrixStats ####

#' @rdname seed-aware
#' @export
setMethod("colSums2", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colsum(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colProds", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colprod(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colMeans2", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colmean(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colVars", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colvar(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colSds", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colsd(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colRanges", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colrange(bm, cols = cols, na.rm = na.rm)
          })

#### DelayedARray ####

#' @rdname seed-aware
#' @export
setMethod("colMins", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colmin(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colMins", "BigArray",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              colMins(seed(x), rows=rows, cols=cols,
                      na.rm=na.rm, useNames=useNames)
          })

#' @rdname seed-aware
#' @export
setMethod("colMaxs", "BigArraySeed",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              bm <- .colsummary(x=x, rows=rows, cols=cols,
                                na.rm=na.rm, useNames=useNames)
              biganalytics::colmax(bm, cols = cols, na.rm = na.rm)
          })

#' @rdname seed-aware
#' @export
setMethod("colMaxs", "BigArray",
          function(x, rows = NULL, cols = NULL,
                   na.rm = FALSE, useNames = NA){
              colMaxs(seed(x), rows=rows, cols=cols,
                      na.rm=na.rm, useNames=useNames)
          })
