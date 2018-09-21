#' Gene set representation as an S3 class
#'
#' @param ... named character() vectors of gene sets. Each character vector is
#'     a gene set. The name of the character vector is the name of the gene set.
#'
#' @return An S3 'GeneSetS3' object.
#' @export
#'
#' @examples
#' GeneSetS3(set1 = c("A", "B", "C"), set2 = c("D", "E"))
GeneSetS3 <- function(...) {
    sets <- list(...)
    stopifnot(
        all(vapply(sets, is, logical(1), "character")),
        length(sets) == 0 || !is.null(names(sets)),
        all(nzchar(names(sets)))
    )
    class(sets) <- "GeneSetS3"
    sets
}

#' Print method for the S3 class gene set representation
#'
#' @param ... An S3 'GeneSetS3' object
#'
#' @return A list of descriptive characteristics of the object
#' @export
#'
#' @examples
#' GeneSetS3(set1 = letters, set2= LETTERS)
print.GeneSetS3 <- function(x) {
    cat(
        "S3 calls: ", class(x), "\n",
        "names(): ", paste(names(x), collapse=" "), "\n",
        "lengths(): ", paste(lengths(x), collapse=" "), "\n",
        sep=""
    )
}
