#' Save/Load a BigArray-based SummarizedExperiment object
#'
#' Saves a SummarizedExperiment-object do disk where each assay is stored as a BigArray.
#' Modelled after \link[HDF5Array]{saveHDF5SummarizedExperiment} and \link[HDF5Array]{loadHDF5SummarizedExperiment}
#'
#' @param x SummarizedExperiment: All assays will be coerced to BigArray objects.
#' @param dir character: Path to directory where to save the data.
#' @param prefix character: Optional prefix to add to the saved files. Allows saving more than one object in the same directory
#' @param replace logical: Whether to overwrite/delete all files if directory already exists.
#'
#' @return A SummarizedExperiment object where each assay is stored as a BigArray.
#' @export
saveBigArraySummarizedExperiment <- function(x,
                                              dir = tempfile(pattern="dir"),
                                              prefix="",
                                              replace=FALSE){

    # Check SummarizedExperiment package is available
    if (!requireNamespace("SummarizedExperiment", quietly=TRUE)){
        stop("This function requires the SummarizedExperiment package!")
    }

    # Pre-checks
    checkmate::assertString(dir)
    checkmate::assertString(prefix)
    checkmate::assertFlag(replace)
    checkmate::assertClass(x, "SummarizedExperiment")

    # Make dir cascade
    if(dir.exists(dir)){
        if(isTRUE(replace)){
            if(prefix != ""){
                tmp <- length(list.files(dir, pattern = paste0("^", prefix)))
                if(tmp > 0){
                    stop("prefix already in use")
                }else{
                    message("Adding new files to directory...")
                }
            }else{
                message("Deleting contents of directory and adding new files...")
                file.remove(list.files(dir, full.names = TRUE))
            }
        }else{
            stop("Directory already exists (overwrite by setting replace=TRUE)")
        }
    }else{
        tmp <- dir.create(dir)

        if(isFALSE(tmp)){
            stop("Could not create directory")
        }else{
            message("Created new directory...")
        }
    }

    # Make files
    message("Setting up files...")
    rds_filepath <- tempfile(pattern=prefix, tmpdir=dir, fileext=".rds")
    n_assays <- length(SummarizedExperiment::assays(x))
    assay_filepaths <- replicate(n = n_assays, expr = tempfile(pattern=prefix, tmpdir=dir))

    # Write assays
    message("Writing BigArray data...")
    tmp <- isTRUE(getOption("bigmemory.allow.dimnames")) # Only write colnames if bigmemory allows them
    SummarizedExperiment::assays(x, withDimnames = tmp) <- S4Vectors::mendoapply(FUN = writeBigArray,
                                                             SummarizedExperiment::assays(x),
                                                             assay_filepaths)

    # Write RDS
    message("Writing RDS data...")
    saveRDS(x, file = rds_filepath)

    # Return
    invisible(x)
}

# Update the seed (Should be used carefully!)
.update_dir <- function(fa, dir){
    seed(fa)@filepath <- file.path(dir, basename(seed(fa)@filepath))
    fa
}

#' @rdname  saveBigArraySummarizedExperiment
#' @export
#' @examples
#' #### Examples ####
#'
#' library(SummarizedExperiment)
#'
#' nrow <- 200
#' ncol <- 6
#' counts <- matrix(as.integer(runif(nrow * ncol, 1, 1e4)), nrow)
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#' se0 <- SummarizedExperiment(assays=list(counts=counts), colData=colData)
#' se0
#'
#' ## Save 'se0' as an HDF5-based SummarizedExperiment object:
#' dir <- tempfile("h5_se0_")
#' h5_se0 <- saveBigArraySummarizedExperiment(se0, dir)
#' list.files(dir)
#'
#' h5_se0
#' assay(h5_se0)
#'
#' h5_se0b <- loadBigArraySummarizedExperiment(dir)
#' h5_se0b
#' assay(h5_se0b)
#'
#' #### Sanity checks ####
#'
#' a1 <- as(as.matrix(iris[,-5]), "BigArray")
#' a2 <- a1 * 10
#' a3 <- round(a2)
#' se1 <- SummarizedExperiment(assays=SimpleList(a1=a1, a2=a2),
#'                             rowData = iris[,5, drop=FALSE])
#' se2 <- SummarizedExperiment(assays=SimpleList(a3 = a3),
#'                             rowData = iris[,5, drop=FALSE])
#'
#' # Save to a directory
#' input_dir <- tempfile()
#' input_se1 <- saveBigArraySummarizedExperiment(se1, dir = input_dir)
#'
#' # Move and reload
#' output_dir <- paste0(input_dir, "_moved")
#' file.rename(input_dir, to = output_dir)
#' output_se1 <- loadBigArraySummarizedExperiment(output_dir)
#'
#' # Check reload works
#' stopifnot(all(a1 ==  assay(output_se1, "a1")),
#'           all(a2 ==  assay(output_se1, "a2")))
#'
#' # Check prefixes work
#' prefix_dir <- tempfile()
#' A1 <- saveBigArraySummarizedExperiment(se1, dir = prefix_dir, prefix = "A_")
#' B1 <- saveBigArraySummarizedExperiment(se2, dir = prefix_dir, prefix = "B_", replace = TRUE)
#'
#' A2 <- loadBigArraySummarizedExperiment(prefix_dir, prefix = "A_")
#' B2 <- loadBigArraySummarizedExperiment(prefix_dir, prefix = "B_")
#'
#' stopifnot(all(assay(A1) == assay(A2)),
#'           all(assay(B1) == assay(B2)))
loadBigArraySummarizedExperiment <- function(dir, prefix=""){
    # Check SummarizedExperiment package is available
    if (!requireNamespace("SummarizedExperiment", quietly=TRUE)){
        stop("This function requires the SummarizedExperiment package!")
    }

    # Pre-checks
    checkmate::assertString(dir)
    checkmate::assertString(prefix)
    checkmate::assertDirectoryExists(dir)

    # Find RDS
    message("Looking for RDS file...")
    rds <- list.files(dir, full.names = TRUE, pattern=".rds$")

    # Find specific prefix
    if(prefix != ""){
        tmp <- grepl(basename(rds), pattern = paste0("^", prefix))
        rds <- rds[tmp]
    }

    # Fail if more than one
    if(length(rds) != 1){
        stop("Directory must contain exactly one .rds file (with/without prefix)")
    }

    message("Reading RDS file...")
    o <- readRDS(rds)

    message("Updating paths...")
    SummarizedExperiment::assays(o) <- S4Vectors::endoapply(SummarizedExperiment::assays(o),
                                                            FUN = .update_dir, dir=dir)

    message("Checking BigArray data...")
    seeds <- S4Vectors::endoapply(SummarizedExperiment::assays(o), seed)
    tmp <- S4Vectors::endoapply(seeds, validObject)

    # Return
    o
}
