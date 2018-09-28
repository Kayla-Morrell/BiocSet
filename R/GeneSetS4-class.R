#' Gene set representation as an S4 class
#' @rdname GeneSetS4
#'
#' @param
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#'
.GeneSetDf <- setClass(
    "GeneSetDf",
    slots = c(
        gene = "character",
        set = "factor"
    )
)
