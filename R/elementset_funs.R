#' Functions applied to elementsets in a \code{BiocSet} object
#' @rdname elementset_funs
#' @name elementset_funs
#' @description All of the major methods applied to a \code{BiocSet} object can
#'     be explicitly applied to the elementset tibble. These functions bypass 
#'     the need to use the \code{es_activate} function by indicating what 
#'     function should be used on the elementset tibble.
#' @param .data A \code{BiocSet} object.
#' @param ... Additional arguments passed to the function.
#' @return A \code{BiocSet} object.
#' @export
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' filter_elementset(es, element == "a" | element == "A")
filter_elementset <- function(.data, ...) {
        act <- .active(.data)
        tbl <- es_activate(.data, "elementset") %>% filter(...)
        initialize(tbl, active = act)
}

#' @rdname elementset_funs
#' @export
#' @examples
#' 
#' es %>% select_elementset(element)
select_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% select(...)
    initialize(tbl, active = act)
}

#' @rdname elementset_funs
#' @export
#' @examples
#' 
#' es %>% mutate_elementset(pval = rnorm(1:52))
mutate_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname elementset_funs
#' @export
#' @examples
#' 
#' es %>% summarise_elementset(n = n())
summarise_elementset <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "elementset") %>% summarise(...)
}

#' @rdname elementset_funs
#' @export
#' @examples
#' 
#' es %>% arrange_elementset(desc(element))
arrange_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname elementset_funs
#' @export
#' @examples
#' 
#' tbl <- tibble(x = 5:6, y = c("set1", "set2"))
#' es %>% left_join_elementset(tbl, by = c(set = "y"))
left_join_elementset <- function(.data, ...)
{
    tbl <- es_elementset(.data) %>% left_join(...)
    initialize(.data, elementset = tbl)
}

#' @rdname elementset_funs
#' @return For \code{tibble_from_elementset}, a tibble.
#' @export
#' @examples
#' 
#' tibble_from_elementset(es)
tibble_from_elementset <-
    function(.data)
{
    stopifnot(is(.data, "BiocSet"))
    es_elementset(.data) %>%
        left_join(es_set(.data)) %>%
        left_join(es_element(.data))
}

#' @rdname elementset_funs
#' @return For \code{data.frame_from_elementset}, a data.frame.
#' @export
#' @examples
#' 
#' data.frame_from_elementset(es)
data.frame_from_elementset <-
    function(.data)
{
    tbl <- tibble_from_elementset(.data)
    data.frame(tbl)
}
