#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' filter_set(es, set == "set1")
filter_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% filter(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% select_set(set)
select_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% select(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% mutate_set(pval = rnorm(1:2))
mutate_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% summarise_set(n = n())
summarise_set <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "set") %>% summarise(...)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% arrange_set(desc(set))
arrange_set <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "set") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' tbl <- tibble(x = 10:11, y = c("set1", "set2"))
#' es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
#' left_join_set(es, tbl, by = c(set = "y"))
left_join_set <- function(.data, ...)
{
    tbl <- es_set(.data) %>% left_join(...)
    initialize(.data, set = tbl)
}

#' @rdname biocset
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' tibble_by_set(es)
tibble_by_set <-
    function(es, how = unlist)
{
    tibble_by_elementset(es) %>%
        group_by(set) %>%
        summarise_all(list) %>%
        mutate_if(.test, how)
}

#' @rdname biocset
#'
#' @return A data.frame
#'
#' @export
#'
#' @examples
#' data.frame_by_set(es)
data.frame_by_set <-
    function(es, how = unlist)
{
    tbl <- tibble_by_set(es, how)
    data.frame(tbl, row.names = "set")
}
