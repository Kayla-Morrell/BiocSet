#' An element set representation as a tripple tibble
#'
#' @rdname biocset
#'
#' @param ... For `BiocSet()`, named character() vectors of element
#'     sets. Each character vector is an element set. The name of the
#'     character vector is the name of the element set.
#' @param active A character to indicate which tibble is active.
#'
#' @return For `BiocSet()`, an S4 'BiocSet' object in a tripple
#'     tibble representation.
#'
#' @export
#'
#' @examples
#' BiocSet(set1 = letters, set2 = LETTERS)
BiocSet <- function(..., active = c("elementset", "element", "set"))
{
    active <- match.arg(active)
    elementset <- tbl_elementset(...)
    element <- tbl_element(elementset)
    set <- tbl_set(elementset)

    .BiocSet(element = element,
                set = set,
                elementset = elementset,
                active = active)
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
    "show", "BiocSet",
    function(object)
    {
        active <- .active(object)
        cat("class: ", class(object), "\n", sep = "")
        cat("\nes_element()", if (active == "element")
                                " <active>", ":\n", sep = ""
            )
        print(.element(object), n = 3)
        cat("\nes_set()", if (active == "set")
                            " <active>", ":\n", sep = ""
            )
        print(.set(object), n = 3)
        cat("\nes_elementset()", if (active == "elementset")
                                    " <active>", ":\n", sep = ""
            )
        print(.elementset(object), n = 3)
    })

#' @rdname biocset
#' 
#' @export
es_activate <- function(.data, what)
{
    UseMethod("es_activate")
}

#' @rdname biocset
#'
#' @param .data The 'BiocSet' tibble.
#' @param what Which of the three tibbles to activate
#'
#' @importFrom rlang quo_text enquo
#' @importFrom methods initialize
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es_activate(es, element)
es_activate.BiocSet <- function(.data, what)
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
    elementset <- filter(.elementset(x),
                        .elementset(x)$element %in% value$element)
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

#' @rdname biocset
#'
#' @param es The active tibble
#' @param value What it is being updated to 
#'
#' @export
update_es_element <- function(es, value)
    .update(es, value)

#' @rdname biocset
#'
#' @export
update_es_set <- function(es, value)
    .update(es, value)

#' @rdname biocset
#'
#' @export
update_es_elementset <- function(es, value)
    .update(es, value)

#' @rdname biocset
#'
#' @param x The active tibble
#'
#' @export
setGeneric("es_element", function(x) standardGeneric("es_element"))

#' @rdname biocset
#' @docType methods
#' @export
setMethod("es_element", "BiocSet", .element)

#' @rdname biocset
#'
#' @export
setGeneric("es_set", function(x) standardGeneric("es_set"))

#' @rdname biocset
#' @docType methods
#' @export
setMethod("es_set", "BiocSet", .set)

#' @rdname biocset
#'
#' @export
setGeneric("es_elementset", function(x) standardGeneric("es_elementset"))

#' @rdname biocset
#' @docType methods
#' @export
setMethod("es_elementset", "BiocSet", .elementset)

#' @rdname biocset
#'
#' @export
`es_element<-` <- update_es_element

#' @rdname biocset
#'
#' @export
`es_set<-` <- update_es_set

#' @rdname biocset
#'
#' @export
`es_elementset<-` <- update_es_elementset

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
#' filter_elementset(es, element == "a" | element == "A")
filter_elementset <- function(.data, ...) {
        act <- .active(.data)
        tbl <- es_activate(.data, "elementset") %>% filter(...)
        initialize(tbl, active = act)
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
#' es %>% select_elementset(element)
select_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% select(...)
    initialize(tbl, active = act)
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
#' es %>% mutate_elementset(pval = rnorm(1:52))
mutate_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% mutate(...)
    initialize(tbl, active = act)
}

#' @rdname biocset
#'
#' @export
map_element <- function(.data, from, to) UseMethod("map_element")

#' @rdname biocset
#'
#' @param from a vector of the values to be replaced
#' @param to a vector of the replacement values
#'
#' @importFrom stats setNames
#'
#' @export
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% map_element(letters, LETTERS)
map_element.BiocSet <- function(.data, from, to)
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

#' @rdname biocset
#'
#' @export
map_set <- function(.data, from, to) UseMethod("map_set")

#' @rdname biocset
#'
#' @importFrom plyr mapvalues
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% map_set("set1", "foo")
map_set.BiocSet <- function(.data, from, to)
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
#' es %>% summarise_elementset(n = n())
summarise_elementset <- function(.data, ...) {
    act <- .active(.data)
    es_activate(.data, "elementset") %>% summarise(...)
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
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% arrange_elementset(desc(element))
arrange_elementset <- function(.data, ...) {
    act <- .active(.data)
    tbl <- es_activate(.data, "elementset") %>% arrange(...)
    initialize(tbl, active = act)
}

#' @rdname major_func
#'
#' @importFrom dplyr tbl_nongroup_vars
#'
#' @param x A 'BiocSet' object.
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% mutate(pval = rnorm(1:52)) %>% tbl_nongroup_vars()
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

#' @rdname biocset
#'
#' @importFrom dplyr left_join
#'
#' @export
#'
#' @examples
#' tbl <- tibble(x = 1:5, y = letters[1:5])
#' es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
#' es %>% left_join_element(tbl, by = c(element = "y"))
left_join_element <- function(.data, ...)
{
    tbl <- es_element(.data) %>% left_join(...)
    initialize(.data, element = tbl)
}

#' @rdname biocset
#'
#' @export
#'
#' @examples
#' tbl <- tibble(x = 10:11, y = c("set1", "set2"))
#' es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
#' es %>% left_join_set(tbl, by = c(set = "y"))
left_join_set <- function(.data, ...)
{
    tbl <- es_set(.data) %>% left_join(...)
    initialize(.data, set = tbl)
}

#' @rdname biocset
#'
#' @param es A BiocSet object that should be split into a list of element sets
#'
#' @export
#'
#' @examples
#' es <- go_sets(org.Hs.eg.db, "ENSEMBL")
#' as_list(es)
as_list <- function(es)
{
    lens <- es_elementset(es) %>% group_by(set) %>% dplyr::count() %>% pull("n")
    elements <- unlist(es_elementset(es)$element)
    elements <- split(elements, rep(seq_along(lens), lens))
    names(elements) <- unique(es_elementset(es)$set)
    elements
}
