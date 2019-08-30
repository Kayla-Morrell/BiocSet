#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' filter_element(es, element == "a")
filter_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% filter(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% select_element(element)
select_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% select(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% mutate_element(pval = rnorm(1:52))
mutate_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% summarise_element(n = n())
summarise_element <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "element") %>% summarise(...)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% arrange_element(desc(element))
arrange_element <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "element") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @importFrom dplyr left_join
#'
#' @export
#'
#' @examples
#' tbl <- tibble(x = 1:5, y = letters[1:5])
#' es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
#' left_join_element(es, tbl, by = c(element = "y"))
left_join_element <- function(.data, ...)
{
    tbl <- es_element(.data) %>% left_join(...)
    initialize(.data, element = tbl)
}

#' @rdname biocset
#'
#' @param how Multiple entries will become a list.
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' tibble_by_element(es)
tibble_by_element <-
    function(es, how = unlist)
{
    tibble_by_elementset(es) %>%
        group_by(element) %>%
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
#' data.frame_by_element(es)
data.frame_by_element <-
    function(es, how = unlist)
{
    tbl <- tibble_by_element(es, how)
    data.frame(tbl, row.names = "element")
}
