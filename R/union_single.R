#' @rdname set_op
#'
#' @importFrom dplyr pull
#'
#' @return For `union_single()`, a BiocSet object with a single set and unioned
#'     elements from x.
#'
#' @export
#'
#' @examples
#'
#' es3 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])
#'
#' union_single(es3)
union_single <- function(x, ...)
{
    unique_elements <- x %>% es_element() %>% pull(element)

    BiocSet(union = unique_elements)
}
