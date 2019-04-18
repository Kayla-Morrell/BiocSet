setOldClass("tbl_element")

setOldClass("tbl_set")

setOldClass("tbl_elementset")

#' @rdname elementset
#'
#' @slot element The element tibble from `tbl_elementset`
#' @slot set The set tibble from `tbl_elementset`
#' @slot elementset The elementset tibble created from user input
#' @slot active The tibble that is active
#'
#' @exportClass ElementSet

.ElementSet <- setClass(
    "ElementSet",
    slots = c(
        element = "tbl_element",
        set = "tbl_set",
        elementset = "tbl_elementset",
        active = "character"
    )
)
