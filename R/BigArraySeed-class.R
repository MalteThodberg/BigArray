#### BigArraySeed ####

# Class
.BigArraySeed <- setClass("BigArraySeed",
                           slots=c(filepath="character",
                                   dim="integer",
                                   dimnames="list",
                                   type="character"))

# Validity
#' @importFrom S4Vectors setValidity2
setValidity2("BigArraySeed", function(object) {
    # Re-load the seed
    bm <- suppressWarnings(tryCatch(attach.big.matrix(paste0(object@filepath, ".desc.txt"),
                                                             readonly = TRUE), error=function(e) e))

    # Error if file could not be read, or check everything matches
    if(!is(bm, "big.matrix")){
        o <- "Could not read big.matrix from BigArray location!"
    }else if(any(dim(bm) != dim(object))){
        o <- "Dimensions of the BigArray does not match the big.matric"
    }else if(bigmemory::typeof(bm) != type(object)){
        o <- "Type of the BigArray does not match the big.matrix"
    }else{
        o <- TRUE
    }

    # Return
    o
})

# Constructor
BigArraySeed <- function(filepath){
    # Safe filename
    filepath <- paste0(filepath, ".bmat")
    filepath <- tools::file_path_as_absolute(filepath)
    filepath <- tools::file_path_sans_ext(filepath)

    # Open/close connection
    bm <- attach.big.matrix(paste0(filepath, ".desc.txt"),
                            readonly = TRUE)

    # Build and return seed
    .BigArraySeed(filepath=filepath,
                   dim=as.integer(dim(bm)),
                   dimnames=dimnames(bm),
                   type=bigmemory::typeof(bm))
}

# Show method
setMethod("show", "BigArraySeed", function(object){
    cat("# BigArraySeed-object:\n")
    cat("filepath:", object@filepath, "\n")
    cat("rows:", object@dim[1], "\n")
    cat("columns:", object@dim[2], "\n")
    cat("type:", object@type)
})

# Extract array method
.extract_array_from_bigmatrix <- function(x, index){
    # Open big.matrix
    bm <- attach.big.matrix(paste0(x@filepath, ".desc.txt"),
                            readonly = TRUE)

    # Coerce
    bm <- as(bm, "BigRealizationSink")

    # Extract
    extract_array(x = bm, index = index)
}

setMethod("extract_array", "BigArraySeed", .extract_array_from_bigmatrix)

