#' Gene set representation as an S3 class tibble
#' @rdname tblgeneset
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
#' @importFrom dplyr distinct '%>%' select mutate group_by ungroup
#'     summarise arrange
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
    	gene = as.character(unlist(args, use.names=FALSE)),
	set = factor(
            rep(names(args), lengths(args)),
            levels = names(args)

        )
    )

    tbl <- tbl %>% distinct(gene, set)

    subclass_tbl_geneset_base(tbl, "tbl_geneset")
}

is_tbl_geneset <- function(x) {
    all(c("gene", "set") %in% names(x)) &&
        is.character(x$gene) && is.factor(x$set)
}

#' @rdname tblgeneset
#'
#' @param x An object of class trunc_mat_tbl_geneset, used during
#'     printing of tbl_geneset
#'
#' @export
format.trunc_mat_tbl_geneset <- function(x, ...) {
    class <- sub("trunc_mat_", "", class(x)[1])
    names(x$summary) <- paste("A", class)
    NextMethod()
}

select.tbl_geneset <- function(.data, ...)
{
    tbl <- NextMethod("select", .data, gene, set, ...)
    class(tbl) <- class(.data)
    tbl
}

