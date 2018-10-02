#' Gene set representation as an S4 class
#' @rdname GeneSetS4
#'
#' @slot gene charcter() vector of genes
#' @slot set factor() of gene sets
#'
#' @importFrom methods new
#' @importFrom methods slot
#' @importFrom methods show
#' @export
.GeneSetDf <- setClass(
    "GeneSetDf",
    slots = c(
        gene = "character",
        set = "factor"
    )
)

#' @rdname GeneSetS4
#'
#' @param x gene set
#'
#' @export
.GeneSetDf_gene <- function(x)
    slot(x, "gene")

#' @rdname GeneSetS4
#'
#' @export
.GeneSetDf_set <- function(x)
    slot(x,"set")

#' @rdname GeneSetS4
#'
#' @export
setGeneric("gene", function(x) standardGeneric("gene"))

#' @rdname GeneSetS4
#'
#' @export
setGeneric("set", function(x) standardGeneric("set"))

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
#' @export
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
#' @param object gene set being represented
#'
#' @export
.GeneSetDf_validity <- function(object) {
    msg <- NULL

    if(length(gene(object))!=length(set(object)))
       msg <- c(msg, "Length of 'gene' must equal length of 'set'")

    if(is.null(msg)) TRUE else msg
}

setValidity("GeneSetDf", .GeneSetDf_validity)

#' @rdname GeneSetS4
#'
#' @export
setMethod("show", "GeneSetDf", function(object){
    cat(
        "class: ", class(object), "\n",
        "names(): ", paste(names(object), collapse = " "), "\n",
        "lengths(): ", paste(lengths(object), collapse = " "), "\n",
        sep = ""
    )
})
