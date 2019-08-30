#' Major dplyr functions adapted into BiocSet
#'
#' @rdname major_func
#'
#' @param .data The 'BiocSet' tibble
#' @param ... Additional arguments passed to function.
#'
#' @return A 'BiocSet' object
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(element) %>% filter(element == "a")
filter.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- filter(sub, ...)
    .update(.data, tbl)
}

#' @rdname major_func
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% select(element)
select.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- select(sub, ...)
    .update(.data, tbl)
}

#' @rdname major_func
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(set) %>% mutate(pval = rnorm(1:2))
mutate.BiocSet <- function(.data, ...)
{
    stopifnot(!any(c("element", "set") %in% names(list(...))))

    sub <- .active_value(.data)
    tbl <- mutate(sub, ...)
    .update(.data, tbl)
}

#' @rdname major_func
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(set) %>% summarise(n = n())
summarise.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    summarise(sub, ...)
}

#' @rdname major_func
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(element) %>% arrange(desc(element))
arrange.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- arrange(sub, ...)
    .update(.data, tbl)
}

#' @rdname major_func
#'
#' @importFrom dplyr tbl_nongroup_vars
#'
#' @param x A BiocSet object.
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% mutate(pval = rnorm(1:52)) %>% es_elementset() %>% tbl_nongroup_vars()
tbl_nongroup_vars.BiocSet <- function(x)
{
    active <- .active(x)
    sub <- slot(x, active)
    tbl_nongroup_vars(sub)
}

#' @rdname major_func
#'
#' @importFrom dplyr group_by
#'
#' @param add logical, whether to add to the existing groups.
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% group_by(element, set)
group_by.BiocSet <- function(.data, ..., add = FALSE)
{
    sub <- .active_value(.data)
    group_by(sub, ..., add = FALSE)
}

#' @rdname major_func
#'
#' @importFrom dplyr left_join
#'
#' @param y A tibble to join
#' @param by A character vector of variables to join by
#' @param copy logical, allow syou to join tables across srcs
#' @param suffix Character vector of length 2, if there are non-joined duplicate
#'    variables in 'x' and 'y' these suffixes will be added to the output
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters[1:5], set2 = LETTERS[1:5])
#' tbl <- tibble(x = 1:10, y = c(letters[1:5], LETTERS[1:5]))
#' es %>% left_join(tbl, by = c(element = "y"))
left_join.BiocSet <- function(x, y, by, copy, suffix, ...)
{
    sub <- .active_value(x)
        tbl <- left_join(sub, y = y, by = by, ...)
    .update(x, tbl)
}

.as.list.BiocSet <- function(from)
{
    with(es_elementset(from), split(element, set))
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' library(org.Hs.eg.db)
#' es <- go_sets(org.Hs.eg.db, "ENSEMBL")
#' head(as.list(es))
as.list.BiocSet <- function(x, ...)
    .as.list.BiocSet(x)

setAs("BiocSet", "list", .as.list.BiocSet)

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
#' @return For `union()`, a BiocSet object with the results of the union of x
#'     and y.
#'
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#'
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
#' @importFrom dplyr intersect
#'
#' @return For `intersect()`, a BiocSet with the intersect of x and y.
#'
#' @export
#'
#' @examples
#'
#' intersect(es1, es2)
intersect.BiocSet <- function(x, y, ...)
{
    element <- intersect(es_element(x), es_element(y), ...)
    set <- intersect(es_set(x), es_set(y), ...)
    elementset <- intersect(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}
