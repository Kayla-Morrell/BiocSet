setOldClass("tbl_element")

setOldClass("tbl_set")

setOldClass("tbl_elementset")

#' @rdname biocset
#'
#' @slot element The element tibble from `tbl_elementset`
#' @slot set The set tibble from `tbl_elementset`
#' @slot elementset The elementset tibble created from user input
#' @slot active Character, indicates which tibble is active
#'
#' @exportClass BiocSet

.BiocSet <- setClass(
    "BiocSet",
    slots = c(
        element = "tbl_element",
        set = "tbl_set",
        elementset = "tbl_elementset",
        active = "character"
    )
)
