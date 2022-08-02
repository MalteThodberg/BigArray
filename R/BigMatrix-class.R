#### BigArray and BigMatrix ####

#' BigArray: DelayedArray backend for BigMatrix.
#'
#' BigArray allows you to use the BigMatrix package as a backend for DelayedArrays, similarly to the default HDF5Array package.
#'
#' @param filepath character: Path to the basename of a BigMatrix
#' @param x matrix, DelayedArray, etc.: matrix-like object to be written block-by-block to disk.
#' @return A BigMatrix object (A 2D BigArray)
#'
#' @details Details on BigArraySeed, BigArray, BigMatrix, FileRealizationSink, read-only / open connections, to be added later...
#'
#' @export
#' @examples
#' #### Examples ####
#'
#' mat <- as.matrix(iris[,-5])
#'
#' # Coerce a matrix to a BigArray using a temporary BigMatrix backing file:
#' as(mat, "BigArray")
#'
#' # Use writeBigArray to specify the location of the backing file:
#' my_backing_file <- tempfile(pattern="BigMatrix")
#' writeBigArray(mat, filepath=tempfile())
#'
#' # Create a BigArray directly from a BigMatrix backing file
#' manual_backing_file <- tempfile(pattern="BigMatrix")
#' bm <- bigmemory::filebacked.big.matrix(
#'     nrow = 100,
#'     ncol = 25,
#'     type = "double",
#'     backingfile = paste0(basename(manual_backing_file), ".bmat"),
#'     backingpath = dirname(manual_backing_file),
#'     descriptorfile = paste0(basename(manual_backing_file), ".desc.txt"))
#' BigArray(manual_backing_file)
#'
#' # BigArray supports all DelayedArray operations. See vignette for examples.
#'
#' #### Sanity checks ####
#'
#' # Check that matrix can be converted to/from
#' dbl_mat <- replicate(n = 25, rnorm(1000))
#' int_mat <- round(dbl_mat * 100)
#'
#' dbl_fa <- as(dbl_mat, "BigArray")
#' int_fa <- as(int_mat, "BigArray")
#'
#' stopifnot(as.matrix(dbl_fa) == dbl_mat,
#'           as.matrix(int_fa) == int_mat)
setClass("BigArray",
         contains  = "DelayedArray",
         slots = c(seed = "BigArraySeed"))

#' @rdname BigArray-class
#' @export
setClass("BigMatrix",
         contains = "DelayedMatrix",
         slots = c(seed = "BigArraySeed"))

setMethod("DelayedArray", "BigArraySeed",
          function(seed) new_DelayedArray(seed, Class="BigArray"))

#' @rdname BigArray-class
#' @export
BigArray <- function(filepath){
    checkmate::assertString(filepath)

    # Allow BigArraySeed
    if (is(filepath, "BigArraySeed")){
        seed <- filepath
    }else{
        seed <- BigArraySeed(filepath)
    }

    # Return
    DelayedArray(seed)
}


# BigMatrix infrastructure
setMethod("matrixClass", "BigArray", function(x) "BigMatrix")
setAs("BigArray", "BigMatrix", function(from) new("BigMatrix", from))
setAs("BigMatrix", "BigArray", function(from) from)  # no-op
