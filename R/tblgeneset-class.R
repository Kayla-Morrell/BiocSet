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
    args <- list(...)

    ## check for proper input
    stopifnot(
	all(vapply(args, is, logical(1), "character")),
	length(args) == 0 || !is.null(names(args)),
	all(nzchar(names(args)))
    )

    ## create tibble
    tbl <- tibble(
    	gene = as.factor(unlist(args, use.names=FALSE)),
	set = as.factor(rep(names(args), lengths(args))) 
    )

    ## want unique genes in sets
    tbl <- tbl %>% distinct(gene,set)

    ## set the class
    class(tbl) <- c("tbl_geneset",class(tbl))

    ## return the gene set as a tibble
    tbl
}