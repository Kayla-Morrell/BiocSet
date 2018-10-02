#' Gene set representation as in S3 class tibble
#' @rdname GeneSetS3tbl
#'
#' @param ... For `GeneSetS3tbl()`, named character() vectors of gene sets. Each
#'     character vector is a gene set. The name of the character vector is the
#'     name of the gene set.
#'
#' @return For `GeneSetS3tbl()`, an S3 'GeneSetS3' object in a tibble
#'    representation.
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' GeneSetS3tbl(set1 = letters, set2 = LETTERS)
GeneSetS3tbl <- function(...) {
    args <- list(...)
    stopifnot(
        all(vapply(args, is, logical(1), "character")),
        length(args) == 0 || !is.null(names(args)),
        all(nzchar(names(args)))
    )
    tbl <- tibble(
        gene = unlist(args, use.names=FALSE),
        set = rep(names(args), lengths(args))
    )
    class(tbl) <- c("GeneSetS3tbl",class(tbl))
    tbl
}

#' @rdname GeneSetS3tbl
#'
#' @param x An S3 'GeneSetS3tbl' object
#'
#' @export
#'
names.GeneSetS3tbl <- function(x) {
    unique(x$set)
}

#' @rdname GeneSetS3tbl
#'
#' @param use.names Logical indicating if the result should inherit the names
#'   from x.
#'
#' @export
#'
lengths.GeneSetS3tbl <- function(x, use.names=TRUE) {
    lengths <- as.vector(table(x$set))
    if(use.names)
        names(lengths) <- names(x)
    lengths
}

#' @rdname GeneSetS3tbl
#'
#' @export
#'
print.GeneSetS3tbl <- function(x,...) {
    cat(
        "S3 class: ", class(x)[1], "\n",
        "names(): ", paste(names(x), collapse=" "), "\n",
        "lengths(): ", paste(lengths(x), collapse=" "), "\n",
        sep=""
    )
}
