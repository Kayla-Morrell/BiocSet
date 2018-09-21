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
    class(sets) <- "GeneSetS3"
    sets
}