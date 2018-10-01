#' Gene set representation as an S4 class
#' @rdname GeneSetS4
#'
#' @slot gene charcter() vector of genes
#' @slot set factor() of gene sets
#'
#' @importFrom methods new
#' @export
#'
.GeneSetDf <- setClass(
    "GeneSetDf",
    slots = c(
        gene = "character",
        set = "factor"
    )
)
