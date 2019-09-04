#' Functions applied to sets in a \code{BiocSet} object
#' @rdname set_funs
#' @name set_funs
#' @description All of the major methods applied to a \code{BiocSet} object can
#'     be explicitly applied to the set tibble. These functions bypass the need
#'     to use the \code{es_activate} function by indicating what function should
#'     be used on the element tibble.
#' @param .data A \code{BiocSet} object.
#' @param ... Additional argument passed to the function.
#' @return A \code{BiocSet} object.
#' @export
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' filter_set(es, set == "set1")
filter_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% filter(...)
    initialize(tbl, active = act)
}

#' @rdname set_funs
#' @name set_funs
#' @export
#' @examples
#' 
#' es %>% select_set(set)
select_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% select(...)
    initialize(tbl, active = act)
}

#' @rdname set_funs
#' @name set_funs
#' @export
#' @examples
#' 
#' es %>% mutate_set(pval = rnorm(1:2))
mutate_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname set_funs
#' @name set_funs
#' @export
#' @examples
#' 
#' es %>% summarise_set(n = n())
summarise_set <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "set") %>% summarise(...)
}

#' @rdname set_funs
#' @name set_funs
#' @export
#' @examples
#' 
#' es %>% arrange_set(desc(set))
arrange_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname set_funs
#' @name set_funs
#' @export
#' @examples
#' 
#' tbl <- tibble(x = 10:11, y = c("set1", "set2"))
#' es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
#' left_join_set(es, tbl, by = c(set = "y"))
left_join_set <- function(.data, ...)
{
    tbl <- es_set(.data) %>% left_join(...)
    initialize(.data, set = tbl)
}

#' @rdname set_funs
#' @name set_funs
#' @param how Multiple entries will become a list.
#' @return For \code{tibble_from_set}, a tibble.
#' @export
#' @examples
#' 
#' tibble_from_set(es)
tibble_from_set <-
    function(.data, how = unlist)
{
    tibble_from_elementset(.data) %>%
        group_by(set) %>%
        summarise_all(list) %>%
        mutate_if(.test, how)
}

#' @rdname set_funs
#' @name set_funs
#' @return For \code{data.frame_from_set}, a data.frame.
#' @export
#' @examples
#' 
#' data.frame_from_set(es)
data.frame_from_set <-
    function(.data, how = unlist)
{
    tbl <- tibble_from_set(.data, how)
    data.frame(tbl, row.names = "set")
}
