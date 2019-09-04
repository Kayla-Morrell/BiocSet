#' Functions applied to elements in a \code{BiocSet} object
#' @name element_funs
#' @rdname element_funs
#' @description All of the major methods applied to a \code{BiocSet} object can
#'     be explicitly applied to the element tibble. These functions bypass the
#'     need to use the \code{es_activate} function by indicating what function
#'     should be used on the element tibble.
#' @param .data A \code{BiocSet} object.
#' @param ... Additional arguments passed to the function.
#' @return A \code{BiocSet} object.
#' @export
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' filter_element(es, element == "a")
filter_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% filter(...)
    initialize(tbl, active = act)
}

#' @rdname element_funs
#'
#' @export
#'
#' @examples
#' 
#' es %>% select_element(element)
select_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% select(...)
    initialize(tbl, active = act)
}

#' @rdname element_funs
#'
#' @export
#'
#' @examples
#' 
#' es %>% mutate_element(pval = rnorm(1:52))
mutate_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname element_funs
#'
#' @export
#'
#' @examples
#'
#' es %>% summarise_element(n = n())
summarise_element <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "element") %>% summarise(...)
}

#' @rdname element_funs
#'
#' @export
#'
#' @examples
#'
#' es %>% arrange_element(desc(element))
arrange_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname element_funs
#'
#' @importFrom dplyr left_join
#'
#' @export
#'
#' @examples
#' 
#' tbl <- tibble(x = 1:5, y = letters[1:5])
#' es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
#' left_join_element(es, tbl, by = c(element = "y"))
left_join_element <- function(.data, ...)
{
    tbl <- es_element(.data) %>% left_join(...)
    initialize(.data, element = tbl)
}

#' @rdname element_funs
#' @param how Multiple entries will become a list.
#' @return For \code{tibble_from_element}, a tibble.
#' @export
#' @examples
#' 
#' tibble_from_element(es)
tibble_from_element <-
    function(.data, how = unlist)
{
    tibble_from_elementset(.data) %>%
        group_by(element) %>%
        summarise_all(list) %>%
        mutate_if(.test, how)
}

#' @rdname element_funs
#' @return For \code{data.frame_from_element}, a data.frame.
#' @export
#' @examples
#' 
#' data.frame_from_element(es)
data.frame_from_element <-
    function(.data, how = unlist)
{
    tbl <- tibble_from_element(.data, how)
    data.frame(tbl, row.names = "element")
}
