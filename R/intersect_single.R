#' Intersect on a single \code{BiocSet} object
#' @rdname intersect_single
#' @name intersect_single
#' @description This function performs an intersection within a single 
#'     \code{BiocSet} object.
#' @param x A \code{BiocSet} object.
#' @param ... Additional arguments passed to function.
#' @return A \code{BiocSet} object with a single set  'intersect' and 
#'     interesected elements from x.
#' @examples
#' \dontrun{
#' es1 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])
#' intersect_single(es1)
#' }
intersect_single <- function(x, ...)
{
    elements <- es_elementset(x)$element
    dup_list <- elements[duplicated(elements)]
    dup_elements <- x %>%
        filter_element(element %in% dup_list) %>%
        es_element() %>%
        pull(element)

    BiocSet(intersect = dup_elements)
}
