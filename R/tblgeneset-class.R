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
#' @importFrom dplyr distinct '%>%' filter select mutate group_by ungroup
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

    class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

is_tbl_geneset <- function(x) {
    all(c("gene", "set") %in% names(x)) &&
        is.character(x$gene) && is.factor(x$set)
}

#' @rdname geneset
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

#' @rdname geneset
#'
#' @param .data The tibble used in filter
#'
#' @export
filter.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

#' @rdname geneset
#'
#' @export
select.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    if (!is_tbl_geneset(tbl))
        class(tbl) <- setdiff(class(tbl), "tbl_geneset")
    tbl
}

#' @rdname geneset
#'
#' @export
mutate.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    if (is_tbl_geneset(tbl))
        class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

#' @rdname geneset
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

#' @rdname geneset
#'
#' @export
ungroup.tbl_geneset <- function(x, ...) {
    tbl <- NextMethod()
    if (is_tbl_geneset(tbl))
        class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

#' @rdname geneset
#'
#' @export
summarise.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    if (!is_tbl_geneset(tbl))
        class(tbl) <- setdiff(class(tbl), "tbl_geneset")
    tbl
}

#' @rdname geneset
#'
#' @export
arrange.tbl_geneset <- function(.data, ...) {
    tbl <- NextMethod()
    class(tbl) <- c("tbl_geneset", class(tbl))
    tbl
}

## src <- src_organism(dbpath = hg38light())
## go_genes <- inner_join(tbl(src, "id"), tbl(src, "id_go")) %>%
##     dplyr::select(entrez, ensembl, symbol, go, ontology) %>%
##     distinct() %>% collect()
