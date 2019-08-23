#' Set operations for BiocSets
#'
#' @rdname set_op
#'
#' @param x The first BiocSet to perform operations on
#' @param y The second BiocSet object used
#' @param ... other arguments passed on to methods
#'
#' @importFrom dplyr union
#' 
#' @return For union, a tibble with the results of the union of x or x and y.
#' 
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
#' es2 <- BiocSet(set1 = letters[c(2:4)], set2 = LETTERS[c(2:4)])
#' union(es1, es2)
union.BiocSet <- function(x, y, ...)
{
    element <- union(es_element(x), es_element(y), ...)
    set <- union(es_set(x), es_set(y), ...)
    elementset <- union(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}

#' @rdname set_op
#'
#' @importFrom dplyr pull
#'
#' @return A BiocSet object with a single set and unioned elements
#'
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])
#' union_single(es1) 
union_single <- function(x, ...)
{
    unique_elements <- x %>% es_element() %>% pull(element)

    BiocSet(union = unique_elements)
}

#' @rdname set_op
#'
#' @importFrom dplyr intersect
#' 
#' @return For intersect, a tibble with the intersect of x and y 
#'
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#' intersect(es1, es2)
intersect.BiocSet <- function(x, y, ...)
{
    element <- intersect(es_element(x), es_element(y), ...)
    set <- intersect(es_set(x), es_set(y), ...)
    elementset <- intersect(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}

#' @rdname set_op
#'
#' @return A BiocSet object with a single set and interesected elements
#'
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
