#' Union on a single \code{BiocSet} object
#' @rdname union_single
#' @name union_single
#' @description This function performs a union within a single \code{BiocSet}
#'     object.
#' @param x A \code{BiocSet} object.
#' @param ... Additional arguments passed to function.
#' @importFrom dplyr pull
#' @return For \code{union_single}, a \code{BiocSet} object with a single set 
#'     \code{union} and unioned elements from x.
#' @export
#' @examples
#' es3 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])
#' union_single(es3)
union_single <- function(x, ...)
{
    element <- NULL
    unique_elements <- x %>% es_element() %>% pull(element)

    BiocSet(union = unique_elements)
}
