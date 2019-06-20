union_1arg <- function(x, ...)
{
    #unique_list <- unique(es_element(x)$element)
    #x %>% filter(element %in% unique_list) %>% es_element()
    x %>% es_element()
}

union_2arg <- function(x, y, ...)
{
    element <- union(es_element(x), es_element(y), ...)
    set <- union(es_set(x), es_set(y), ...)
    elementset <- union(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}

#' Set operations for BiocSets
#'
#' @rdname set_op
#'
#' @param x The first BiocSet to perform operations on
#' @param y The second BiocSet to perform operations on
#' @param ... other arguments passed on to methods
#' 
#' @return For union, a tibble with the results of the union of x and y
#' 
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
#' es2 <- BiocSet(set1 = letters[c(2:4)], set2 = LETTERS[c(2:4)])
#' union(es1, es2)
#' es3 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])
#' union(es3)
union.BiocSet <- function(x, y=NULL, ...)
{
    if (is.null(y))
        union_1arg(x, ...)
    else
	union_2arg(x, y, ...)
}

#' @rdname set_op
#'
#' 
#' @return For intersect, a tibble with the intersect of x and y 
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#' intersect(es1, es2)
#' es3 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])
#' set1 <- filter_elementset(es3, set == "set1")
#' set2 <- filter_elementset(es3, set == "set2")
#' intersect(set1, set2)
intersect.BiocSet <- function(x, y, ...)
{
    element <- intersect(es_element(x), es_element(y), ...)
    set <- intersect(es_set(x), es_set(y), ...)
    elementset <- intersect(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}

intersect_1arg <- function(x, ...)
{
    dup_list <- duplicated(es_element(x)$element)
    x %>% filter(element %in% unique_list) %>% es_element()
}

intersect_2arg <- function(x, y, ...)
{

}
