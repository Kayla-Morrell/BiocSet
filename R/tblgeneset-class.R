#' Gene set representation as an S3 class tibble
#' @rdname geneset
#'
#' @param ... For `tbl_geneset()`, named character() vectors of gene
#'     sets. Each character vector is a gene set. The name of the
#'     character vector is the name of the gene set.
#'
#' @return For `tbl_geneset()`, an S3 'geneset' object in a tibble
#'     representation.
#'
#' @importFrom methods is
#' @importFrom tibble tibble
#' @importFrom dplyr distinct '%>%' filter
#'
#' @export
#'
#' @examples
#' tbl_geneset(set1 = letters, set2 = LETTERS)

tbl_geneset <- function(...) {
    args <- list(...)

    stopifnot(
	all(vapply(args, is, logical(1), "character")),
	length(args) == 0 || !is.null(names(args)),
	all(nzchar(names(args)))
    )

    tbl <- tibble(
    	gene = as.factor(unlist(args, use.names=FALSE)),
	set = as.factor(rep(names(args), lengths(args))) 
    )

    tbl <- tbl %>% distinct(gene,set)

    class(tbl) <- c("tbl_geneset",class(tbl))
    tbl
}

#' @rdname geneset
#'
#' @param x An object of class trunc_mat_tbl_geneset, used during
#'     printing of tbl_geneset
#' 
#' @export
format.trunc_mat_tbl_geneset <- function(x,...) {
    class <- sub("trunc_mat_", "", class(x)[1])
    names(x$summary) <- paste("A", class)
    NextMethod()
}

#' @rdname geneset
#'
#' @param .data The tibble used in filter
#'
#' @export
filter.tbl_geneset <- function(.data, ...) {
    tbl <-  NextMethod()
    class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}
