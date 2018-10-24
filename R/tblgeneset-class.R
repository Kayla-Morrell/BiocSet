#' Gene set representation as an S3 class tibble
#' @rdname geneset
#'
#' @param ... For `tbl_geneset()`, named character() vectors of gene sets. Each
#'    character vector is a gene set. The name of the character vector is the
#'    name of the gene set.
#'
#' @return For `tbl_geneset()`, an S3 'geneset' object in a tibble representation.
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @ examples
#' tbl_geneset(set1 = letters, set2 = LETTERS)

tbl_geneset <- function(...) {

}