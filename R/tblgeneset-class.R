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

    class(tbl) <- c("tbl_geneset", "tbl_geneset_base", class(tbl))
    tbl
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

#' @rdname tblgeneset
#'
#' @export
select.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    if (!is_tbl_geneset(tbl))
        class(tbl) <- setdiff(class(tbl), "tbl_geneset")
    tbl
}

#' @rdname tblgeneset
#'
#' @export
mutate.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    if (is_tbl_geneset(tbl))
        class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

#' @rdname tblgeneset
#'
#' @param add logical, whether to add to (add = TRUE) or override (add = FALSE)
#'     the existing groups. The default is add = FALSE.
#'
#' @export
group_by.tbl_geneset <- function(.data, ..., add = FALSE) {
    tbl <- NextMethod()
    if (is_tbl_geneset(tbl))
        class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

#' @rdname tblgeneset
#'
#' @export
ungroup.tbl_geneset <- function(x, ...) {
    tbl <- NextMethod()
    if (is_tbl_geneset(tbl))
        class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

#' @rdname tblgeneset
#'
#' @export
summarise.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    if (!is_tbl_geneset(tbl))
        class(tbl) <- setdiff(class(tbl), "tbl_geneset")
    tbl
}

#' @rdname tblgeneset
#'
#' @export
arrange.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}
