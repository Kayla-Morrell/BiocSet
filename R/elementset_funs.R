#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' filter_elementset(es, element == "a" | element == "A")
filter_elementset <- function(.data, ...) {
        act <- .active(.data)
        tbl <- es_activate(.data, "elementset") %>% filter(...)
        initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% select_elementset(element)
select_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% select(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% mutate_elementset(pval = rnorm(1:52))
mutate_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% summarise_elementset(n = n())
summarise_elementset <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "elementset") %>% summarise(...)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% arrange_elementset(desc(element))
arrange_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' tbl <- tibble(x = 5:6, y = c("set1", "set2"))
#' es %>% left_join_elementset(tbl, by = c(set = "y"))
left_join_elementset <- function(.data, ...)
{
    tbl <- es_elementset(.data) %>% left_join(...)
    initialize(.data, elementset = tbl)
}

#' @rdname biocset
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' tibble_by_elementset(es)
tibble_by_elementset <-
    function(es)
{
    stopifnot(is(es, "BiocSet"))
    es_elementset(es) %>%
        left_join(es_set(es)) %>%
        left_join(es_element(es))
}

#' @rdname biocset
#'
#' @return A data.frame
#'
#' @export
#'
#' @examples
#' data.frame_by_elementset(es)
data.frame_by_elementset <-
    function(es)
{
    tbl <- tibble_by_elementset(es)
    data.frame(tbl)
}
