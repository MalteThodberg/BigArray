#### BigArraySeed ####

# Class
.BigRealizationSink <- setClass("BigRealizationSink",
                          contains=c("big.matrix", "RealizationSink"))

# Constructor
BigRealizationSink <- function(dim, dimnames, type, filepath=tempfile()){
    # Set set names only if bigmemory allows it
    if(isTRUE(getOption("bigmemory.allow.dimnames"))){
       tmp <- dimnames
    }else{
        tmp <- NULL
    }

    # Create bigmatrix
    bm = filebacked.big.matrix(
        nrow = dim[1],
        ncol = dim[2],
        dimnames = tmp,
        type = type,
        backingfile = paste0(basename(filepath), ".bmat"),
        backingpath = dirname(filepath),
        descriptorfile = paste0(basename(filepath), ".desc.txt"))


    # Return
    as(bm, "BigRealizationSink")
}

# Overwrite some methods to be compatible with DelayedArray
setMethod("type", "BigRealizationSink", function(x){bigmemory::typeof(x)})

#' Internal functions
#'
#' Internal functions that should not be used called directly.
#'
#' @param x BigArraySeed, BigArray, BigRealizationSink
setMethod("dim", "BigRealizationSink", function(x){as.integer(callNextMethod())})

#### writeBigArray

setMethod("write_block", "BigRealizationSink", function(sink, viewport, block){
    callNextMethod()
    bigmemory::flush(sink)
})

#' @rdname BigArray-class
#' @export
writeBigArray <- function(x, filepath=tempfile()){
    sink <- BigRealizationSink(dim = dim(x),
                                dimnames = dimnames(x),
                                type = type(x),
                                filepath = filepath)
    BLOCK_write_to_sink(sink = sink, x = x)
    o <- as(sink, "BigArray")
    o
}

#### Coercions ####

# Internal
.sink2seed <- function(from){
    # Clean filenames
    from_dir <- dir.name(from)
    from_fname <- file.name(from)
    from_fname <- tools::file_path_sans_ext(from_fname)
    to_fname <- paste0(from_dir, from_fname)

    # Return as seed.
    BigArraySeed(to_fname)
}

setAs("BigRealizationSink", "BigArraySeed", .sink2seed)
setAs("BigRealizationSink", "BigArray", function(from) DelayedArray(as(from, "BigArraySeed")))
setAs("BigRealizationSink", "DelayedArray", function(from) as(from, "BigArray"))

#' @export
setAs("ANY", "BigArray", function(from) writeBigArray(from))

#' @export
setAs("DelayedArray", "BigArray", function(from) writeBigArray(from))

#' @export
setAs("DelayedMatrix", "BigArray", function(from) writeBigArray(from))
