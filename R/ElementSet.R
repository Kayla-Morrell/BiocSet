#' An element set representation as a tripple tibble
#'
#' @rdname elementset
#'
#' @param ... For `ElementSet()`, named character() vectors of element
#'     sets. Each character vector is an element set. The name of the
#'     character vector is the name of the element set.
#' @param active A character to indicate which tibble is active.
#'
#' @return For `ElementSet()`, an S4 'ElementSet' object in a tripple
#'     tibble representation.
#'
#' @export
#'
#' @examples
#' ElementSet(set1 = letters, set2 = LETTERS)
ElementSet <- function(..., active = c("elementset", "element", "set"))
{
    active <- match.arg(active)
    elementset <- tbl_elementset(...)
    element <- tbl_element(elementset)
    set <- tbl_set(elementset)

    .ElementSet(element = element, set = set, elementset = elementset, active = active)
}

.element <- function(x) x@element

.set <- function(x) x@set

.elementset <- function(x) x@elementset

.active <- function(x) x@active

`.active<-` <- function(x, value)
{
    value <- gsub('"', '', value)
    stopifnot(value %in% c("element", "set", "elementset"))
    x@active <- value
    x
}

#' @importFrom methods slot
.active_value <-
    function(x)
{
    slot(x, .active(x))
}

setMethod(
    "show", "ElementSet",
    function(object)
    {
        active <- .active(object)
        cat("class: ", class(object), "\n", sep = "")
        cat("\nes_element()", if (active == "element") " <active>", ":\n", sep = "")
        print(.element(object), n = 3)
        cat("\nes_set()", if (active == "set") " <active>", ":\n", sep = "")
        print(.set(object), n = 3)
        cat("\nes_elementset()", if (active == "elementset") " <active>", ":\n", sep = "")
        print(.elementset(object), n = 3)
    })

#' @rdname elementset
#'
#' @export
es_activate <- function(.data, what)
{
    UseMethod("es_activate")
}

#' @rdname elementset
#'
#' @param .data The 'ElementSet' tibble.
#' @param what Which of the three tibbles to activate
#'
#' @importFrom rlang quo_text enquo
#' @importFrom methods initialize
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es_activate(es, element)
es_activate.ElementSet <- function(.data, what)
{
    what <- quo_text(enquo(what))
    .active(.data) <- what
    .data
}

setGeneric(
    ".update",
    function(x, value) standardGeneric(".update"),
    signature = "value"
)

setMethod(
    ".update", "tbl_element",
    function(x, value)
{
    stopifnot(all(value$element %in% .element(x)$element))
    elementset <- filter(.elementset(x), .elementset(x)$element %in% value$element)
    initialize(x, element = value, elementset = elementset)
})

setMethod(
    ".update", "tbl_set",
    function(x, value)
{
    stopifnot(all(value$set %in% .set(x)$set))
    elementset <- filter(.elementset(x), .elementset(x)$set %in% value$set)
    initialize(x, set = value, elementset = elementset)
})

setMethod(
    ".update", "tbl_elementset",
    function(x, value)
{
    stopifnot(
        all(value$element %in% .elementset(x)$element),
        all(value$set %in% .elementset(x)$set)
    )
    element <- filter(.element(x), .element(x)$element %in% value$element)
    set <- filter(.set(x), .set(x)$set %in% value$set)
    initialize(x, element = element, set = set, elementset = value)
})

#' @rdname elementset
#'
#' @export
update_es_element <- function(es, value)
    .update(es, value)

#' @rdname elementset
#'
#' @export
update_es_set <- function(es, value)
    .update(es, value)

#' @rdname elementset
#'
#' @export
update_es_elementset <- function(es, value)
    .update(es, value)

#' @rdname elementset
#'
#' @export
setGeneric("es_element", function(x) standardGeneric("es_element"))

#' @rdname elementset
#'
#' @exportMethod es_element
setMethod("es_element", "ElementSet", .element)

#' @rdname elementset
#'
#' @export
setGeneric("es_set", function(x) standardGeneric("es_set"))

#' @rdname elementset
#'
#' @exportMethod es_set
setMethod("es_set", "ElementSet", .set)

#' @rdname elementset
#'
#' @export
setGeneric("es_elementset", function(x) standardGeneric("es_elementset"))

#' @rdname elementset
#'
#' @exportMethod es_elementset
setMethod("es_elementset", "ElementSet", .elementset)

## es_set(es) <- es_set(es) %>% left_join(paths, by = c("set" = "name"))

#' @rdname elementset
#'
#' @export
`es_element<-` <- update_es_element

#' @rdname elementset
#'
#' @export
`es_set<-` <- update_es_set

#' @rdname elementset
#'
#' @export
`es_elementset<-` <- update_es_elementset

#' @rdname elementset
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(element) %>% filter(element == "a")
filter.ElementSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- filter(sub, ...)
    .update(.data, tbl)
}

#' @rdname elementset
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% select(element)
select.ElementSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- select(sub, ...)
    .update(.data, tbl)
}

#' @rdname elementset
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(set) %>% mutate(pval = rnorm(1:2))
mutate.ElementSet <- function(.data, ...)
{
    stopifnot(!any(c("element", "set") %in% names(list(...))))

    sub <- .active_value(.data)
    tbl <- mutate(sub, ...)
    .update(.data, tbl)
}

#' @rdname elementset
#'
#' @export
map_element <- function(.data, from, to) UseMethod("map_element")

#' @rdname elementset
#'
#' @param from a vector of the values to be replaced
#' @param to a vector of the replacement values
#'
#' @importFrom stats setNames
#'
#' @export
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% map_element(letters, LETTERS)
map_element.ElementSet <- function(.data, from, to)
{
    stopifnot(is.character(from), is.character(to), length(from) == length(to))

    element <- .element(.data)
    idx <- element$element %in% from
    element$element[idx] <- unname(setNames(to, from)[element$element[idx]])

    es <- .elementset(.data)
    idx <- es$element %in% from
    es$element[idx] <- unname(setNames(to, from)[es$element[idx]])

    initialize(.data, element = element, elementset = es)
}

#' @rdname elementset
#'
#' @export
map_set <- function(.data, from, to) UseMethod("map_set")

#' @rdname elementset
#'
#' @importFrom plyr mapvalues
#'
#' @export
#'
#' @examples
#' es <- ElementSet(a = letters, B = LETTERS)
#' es %>% map_set("set1", "foo")
map_set.ElementSet <- function(.data, from, to)
{
    stopifnot(is.character(from), is.character(to), length(from) == length(to))

    set <- .set(.data)
    idx <- set$set %in% from
    set$set <- mapvalues(set$set, from, to)

    es <- .elementset(.data)
    idx <- es$set %in% from
    es$set <- mapvalues(es$set, from, to)

    initialize(.data, set = set, elementset = es)
}

#' @rdname elementset
#'
#' @param add by default, `group_by()` will override existing groups. To add to
#' existing groups, add should be TRUE.
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% group_by(element, set)
group_by.ElementSet <- function(.data, ..., add = FALSE)
{
    sub <- .active_value(.data)
    group_by(sub, ..., add = FALSE)
}

## #' @rdname elementset
## #'
## #' @param x a ElementSet
## #'
## #' @export
## #'
## #' @examples
## #' es <- ElementSet(set1 = letters, set2 = LETTERS)
## #' es %>% group_by(set) %>% summarise(n = n()) %>% ungroup()
## ungroup.ElementSet <- function(x, ...)
## {
##     sub <- .active_value(x)
##     tbl <- ungroup(sub, ...)
##     .update(x, tbl)
## }

#' @rdname elementset
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(set) %>% summarise(n = n())
summarise.ElementSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    summarise(sub, ...)
}

#' @rdname elementset
#'
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% es_activate(element) %>% arrange(desc(element))
arrange.ElementSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- arrange(sub, ...)
    .update(.data, tbl)
}

#' @rdname elementset
#'
#' @importFrom dplyr tbl_vars
#' @export
#'
#' @examples
#' es <- ElementSet(set1 = letters, set2 = LETTERS)
#' es %>% mutate(pval = rnorm(1:52)) %>% tbl_vars()
tbl_vars.ElementSet <- function(x)
{
    active <- .active(x)
    sub <- slot(x, active)
    tbl_vars(sub)
}
