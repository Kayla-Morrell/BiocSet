#' Gene set representation as an S3 class
#' @rdname GeneSetS3
#'
#' @param ... For `GeneSetS3()`, named character() vectors of gene sets. Each
#'     character #vector is a gene set. The name of the character vector is the
#'     name of the gene set.
#'
#'
#' @return For `GeneSetS3()`, an S3 'GeneSetS3' object.
#' @importFrom methods is
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

#' @rdname GeneSetS3
#'
#' @param x An S3 'GeneSetS3' object
#'
#' @export
#'
print.GeneSetS3 = function(x,...) {
    cat(
        "S3 class: ", class(x), "\n",
        "names(): ", paste(names(x), collapse=" "), "\n",
        "lengths(): ", paste(lengths(x), collapse=" "), "\n",
        sep=""
    )
}

#' @rdname GeneSetS3
#'
#' @param i A numeric or character vector, indicy specifying elements to
#'    extract or replace
#' @param j A numeric or character vector, indicy specifying elements to
#'    extract or replace
#' @param drop logical, if TRUE the result is coerced to lowest possible
#'    dimension. Only works for extraction, not replacement
#'
#' @export
#'
`[.GeneSetS3` <- function(x, i, j, ..., drop = TRUE){
    result <- NextMethod()
    do.call("GeneSetS3",result)
}

#' @rdname GeneSetS3
#'
#' @return For `GeneSetS3_tbl()`, an S3 'GeneSetS3' object in a tibble
#'    representation.
#' @importFrom tibble tibble
#'
#' @export
#'
GeneSetS3_tbl <- function(...) {
    args <- list(...)
    tbl <- tibble(
        gene = unlist(args, use.names=FALSE),
        set = rep(names(args), lengths(args))
    )
    class(tbl) <- c("GeneSetS3_tbl",class(tbl))
    tbl
}

#' @rdname GeneSetS3
#'
#' @export
#'
names.GeneSetS3_tbl <- function(x) {
    unique(x$set)
}

#' @rdname GeneSetS3
#'
#' @param use.names Logical indicating if the result should inherit the names
#'   from x.
#'
#' @export
#'
lengths.GeneSetS3_tbl <- function(x, use.names=TRUE) {
    lengths <- as.vector(table(x$set))
    if(use.names)
        names(lengths) <- names(x)
    lengths
}

#' @rdname GeneSetS3
#'
#' @export
#'
print.GeneSetS3_tbl <- function(x,...) {
    cat(
        "S3 class: ", class(x)[1], "\n",
        "names(): ", paste(names(x), collapse=" "), "\n",
        "lengths(): ", paste(lengths(x), collapse=" "), "\n",
        sep=""
    )
}
