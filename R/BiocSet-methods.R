#' BiocSet methods
#' @name BiocSet-methods
#' @rdname BiocSet-methods
#' @description \code{es_activate}: which of the three tibbles in the
#'     \code{BiocSet} object should be activated and have the chosen
#'     functionality applied to it.
#' @param .data The \code{BiocSet} object.
#' @param what Which of the three tibbles from \code{BiocSet} to activate.
#' @importFrom rlang quo_text enquo
#' @importFrom methods initialize
#' @return A \code{BiocSet} object.
#' @export
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es_activate(es, element)
es_activate <- function(.data, what)
{
    UseMethod("es_activate")
}

#' @export
#' @method es_activate BiocSet
es_activate.BiocSet <- function(.data, what)
{
    what <- quo_text(enquo(what))
    .active(.data) <- what
    .data
}

#' @rdname BiocSet-methods
#' @description \code{filter}: choose rows where conditions are true.
#' @param ... Additional arguments passed to function.
#' @export
#' @examples
#' 
#' es %>% es_activate(element) %>% filter(element == "a")
filter.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- filter(sub, ...)
    .update(.data, tbl)
}

#' @rdname BiocSet-methods
#' @description \code{select}: keep only the variables listed. 
#' @export
#' @examples
#' 
#' es %>% select(element)
select.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- select(sub, ...)
    .update(.data, tbl)
}

#' @rdname BiocSet-methods
#' @description \code{mutate}: add new variable and preserve the existing 
#'     variables.
#' @export
#' @examples
#'
#' es %>% es_activate(set) %>% mutate(pval = rnorm(1:2))
mutate.BiocSet <- function(.data, ...)
{
    stopifnot(!any(c("element", "set") %in% names(list(...))))

    sub <- .active_value(.data)
    tbl <- mutate(sub, ...)
    .update(.data, tbl)
}

#' @rdname BiocSet-methods
#' @description \code{summarise}: usually used with \code{group_by()}, output 
#'     will have one row for each group.
#' @export
#' @examples
#' 
#' es %>% es_activate(set) %>% summarise(n = n())
summarise.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    summarise(sub, ...)
}

#' @rdname BiocSet-methods
#' @description \code{arrange}: order rows by an expression involving its 
#'     variables.
#' @export
#' @examples
#' 
#' es %>% es_activate(element) %>% arrange(desc(element))
arrange.BiocSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- arrange(sub, ...)
    .update(.data, tbl)
}

#' @importFrom dplyr tbl_nongroup_vars
.tbl_nongroup_vars <- tbl_nongroup_vars

#' @rdname BiocSet-methods
#' @description \code{.tbl_nongroup_vars}: returns only non-grouping variables.
#' @param x For \code{.tbl_nongroup_vars} (internal), a \code{BiocSet}
#'     object. For \code{union} and \code{intersect} the first
#'     \code{BiocSet} object to perform the operations on.
#' @keywords internal
#' @examples
#' 
#' es %>% mutate(pval = rnorm(1:52)) %>% es_elementset() %>%
#'     BiocSet:::.tbl_nongroup_vars()
.tbl_nongroup_vars.BiocSet <- function(x)
{
    active <- .active(x)
    sub <- slot(x, active)
    .tbl_nongroup_vars(sub)
}

#' @rdname BiocSet-methods
#' @description \code{group_by}: converts an existing tbl into a grouped tbl. 
#' @importFrom dplyr group_by
#' @param add logical, whether to add to the existing groups.
#' @export
#' @examples
#' 
#' es %>% group_by(element, set)
group_by.BiocSet <- function(.data, ..., add = FALSE)
{
    sub <- .active_value(.data)
    group_by(sub, ..., add = FALSE)
}

#' @rdname BiocSet-methods
#' @description \code{left_join}: returns all rows from \code{x}, and all 
#'     columns from \code{x} and \code{y}. If no rows in \code{x} match with 
#'     \code{y} there will be \code{NA}s in the new column. If there are 
#'     multiple matches then all combinations are returned.
#' @importFrom dplyr left_join
#' @param y For \code{left_join}, a tibble to join. For \code{union} and 
#'     \code{intersect} the second \code{BiocSet} object used.
#' @param by A character vector of variables to join by.
#' @param copy logical, allows you to join tables across srcs.
#' @param suffix Character vector of length 2, if there are non-joined duplicate
#'    variables in 'x' and 'y' these suffixes will be added to the output.
#' @export
#' @examples
#' 
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

#' @rdname BiocSet-methods
#' @description \code{as.list}: coerces argument into a list.
#' @export
#' @examples
#' 
#' library(org.Hs.eg.db)
#' es <- go_sets(org.Hs.eg.db, "ENSEMBL")
#' head(as.list(es))
as.list.BiocSet <- function(x, ...)
    .as.list.BiocSet(x)

setAs("BiocSet", "list", .as.list.BiocSet)

#' @rdname BiocSet-methods
#' @description \code{union}: combines all rows from two \code{BiocSet} objects
#'     and removes duplicate records from the combined \code{BiocSet} object.
#' @importFrom dplyr union
#' @export
#' @examples
#' 
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#'
#' dplyr::union(es1, es2)
union.BiocSet <- function(x, y, ...)
{
    element <- union(es_element(x), es_element(y), ...)
    set <- union(es_set(x), es_set(y), ...)
    elementset <- union(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}

#' @rdname BiocSet-methods
#' @description \code{intersect}: combines all rows from two \code{BiocSet} 
#'     objects and returns rows that appear in both \code{BiocSet} objects.
#' @importFrom dplyr intersect
#' @export
#' @examples
#'
#' dplyr::intersect(es1, es2)
intersect.BiocSet <- function(x, y, ...)
{
    element <- intersect(es_element(x), es_element(y), ...)
    set <- intersect(es_set(x), es_set(y), ...)
    elementset <- intersect(es_elementset(x), es_elementset(y), ...)
    initialize(x, element = element, set = set, elementset = elementset)
}
