setOldClass("tbl_gene")

setOldClass("tbl_set")

setOldClass("tbl_geneset")

#' @rdname geneset
#'
#' @slot gene The gene tibble from `tbl_geneset`
#' @slot set The set tibble from `tbl_geneset`
#' @slot geneset The geneset tibble created from user input
#' @slot active The tibble that is active
#'
#' @exportClass GeneSet

.GeneSet <- setClass(
    "GeneSet",
    slots = c(
        gene = "tbl_gene",
        set = "tbl_set",
        geneset = "tbl_geneset",
        active = "character"
    )
)
