#' Element set representation as an S3 class tibble
#' @rdname tblelementset
#' @name tblelementset
#' @param ... For \code{tbl_elementset}, named character vectors of gene sets. 
#'     Each character vector is a element set. The name of the character vector
#'     is the name of the element set.
#' @return For \code{tbl_elementset}, an S3 \code{elementset} object in a tibble
#'     representation.
#' @importFrom methods is
#' @importFrom tibble tibble
#' @importFrom dplyr distinct '%>%' select mutate group_by ungroup
#'     summarise arrange
#' @export
#' @examples
#' es <- tbl_elementset(set1 = letters, set2 = LETTERS)
tbl_elementset <- function(...) {
    args <- list(...)

    stopifnot(
        all(vapply(args, is, logical(1), "character")),
        length(args) == 0 || !is.null(names(args)),
        all(nzchar(names(args)))
    )

    tbl <- tibble(
        element = as.character(unlist(args, use.names=FALSE)),
        set = as.character(rep(names(args), lengths(args)))
    )

    tbl <- tbl %>% distinct(.data$element, .data$set)

    subclass_tbl_elementset_base(tbl, "tbl_elementset")
}

#' @rdname tblelementset
#' @name tblelementset
#' @param x A tibble that may or may not be an elementset
#' @return For \code{is_tbl_elementset}, a logical indicating if the tibble is 
#'     an elementset.
#' @export
#' @examples
#' 
#' is_tbl_elementset(es)
is_tbl_elementset <- function(x) {
    all(c("element", "set") %in% names(x)) &&
        is.character(x$element) && is.character(x$set)
}

format.trunc_mat_tbl_elementset <- function(x, ...) {
    class <- sub("trunc_mat_", "", class(x)[1])
    names(x$summary) <- paste("A", class)
    NextMethod()
}

select.tbl_elementset <- function(.data, ...)
{
    tbl <- NextMethod("select", .data, .data$element, .data$set, ...)
    class(tbl) <- class(.data)
    tbl
}

