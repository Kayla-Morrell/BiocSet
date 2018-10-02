#' Gene set representation as an S4 class
#' @rdname GeneSetS4
#'
#' @slot gene charcter() vector of genes
#' @slot set factor() of gene sets
#'
#' @importFrom methods new
#' @export
#'
#' @example
#'
.GeneSetDf <- setClass(
    "GeneSetDf",
    slots = c(
        gene = "character",
        set = "factor"
    )
)

#' @rdname GeneSetS4
#'
#' @param x generic being set
#'
#' @export
.GeneSetDf_gene <- function(x)
    slot(x, "gene")

#' @rdname GeneSetS4
#'
#' @export
.GeneSetDf_gene <- function(x)
    slot(x,"set")

#' @rdname GeneSetS4
#'
#' @export
setMethod("gene", "GeneSetDf", .GeneSetDf_gene)

#' @rdname GeneSetS4
#'
#' @export
setMethod("set", "GeneSetDf", .GeneSetDf_set)

#' @rdname GeneSetS4
#'
#' @export
.GeneSetDf_names <- function(x)
    levels(set(x))

#' @rdname GeneSetS4
#'
#' @param use.names logical, should result inherit the names from x
#'
#' @expot
.GeneSetDf_lengths <- function(x, use.names=TRUE) {
    lengths <- as.vector(table(set(x)))
    if(use.names)
        names(lengths) <- names(x)
    lengths
}

#' @rdname GeneSetS4
#'
#' @export
setMethod("names", "GeneSetDf", .GeneSetDf_names)

#' @rdname GeneSetS4
#'
#' @export
setGeneric("lengths")

#' @rdname GeneSetS4
#'
#' @export
setMethod("lengths", "GeneSetDf", .GeneSetDf_lengths)

#' @rdname GeneSetS4
#'
#' @export
.GeneSetDf_validity <- function(object) {
    msg <- NULL

    if(length(gene(object))!=length(set(object)))
       msg <- c(msg, "Length of 'gene' must equal length of 'set'")

    if(is.null(msg)) TRUE else msg
}

#' @rdname GeneSetS4
#'
#' @export
setValidity("GeneSetDf", .GeneSetDf_validity)
