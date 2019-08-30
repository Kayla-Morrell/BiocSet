setOldClass(c("tbl_element", "tbl_set", "tbl_elementset"))

#' @rdname biocset
#'
#' @slot element The element tibble from `tbl_elementset`
#' @slot set The set tibble from `tbl_elementset`
#' @slot elementset The elementset tibble created from user input
#' @slot active Character, indicates which tibble is active
#'
#' @exportClass BiocSet

.BiocSet <- setClass(
    "BiocSet",
    slots = c(
        element = "tbl_element",
        set = "tbl_set",
        elementset = "tbl_elementset",
        active = "character"
    )
)

## Constructor
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

#' @rdname biocset
#'
#' @param elementset A tibble with element set information
#' @param element A tibble with element information
#' @param set A tibble with set information
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' element <-
#'    tibble(
#'        element = letters[1:10],
#'        v1 = sample(10),
#'        v2 = sample(10)
#'    )
#' set <-
#'    tibble(
#'        set = LETTERS[1:2],
#'        v1 = sample(2),
#'        v2 = sample(2)
#'    )
#' elementset <-
#'    tibble(
#'        element = letters[1:10],
#'        set = sample(LETTERS[1:2], 10, TRUE)
#'    )
#' BiocSet_from_elementset(elementset, element, set)
BiocSet_from_elementset <- function(elementset, element, set)
{
    if (missing(element))
        element <- tibble(element = character())
    if (missing(set))
        set <- tibble(set = character())
    stopifnot(
        "element" %in% names(elementset),
        is.character(elementset$element),
        "set" %in% names(elementset),
        "element" %in% names(element),
        is.character(element$element),
        "set" %in% names(set)
    )

    es <- do.call(BiocSet, split(elementset$element, elementset$set))
    es <- left_join_element(es, element)
    es <- left_join_set(es, set)
    es <- left_join_elementset(es, elementset)

    if (nrow(element) > nrow(es_element(es)))
        message("more elements in 'element' than in 'elementset'")
    if (nrow(set) > nrow(es_set(es)))
        message("more elements in 'set' than in 'elementset'")

    es
}
