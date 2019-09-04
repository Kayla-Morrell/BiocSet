#' @importFrom methods setOldClass
setOldClass(c("tbl_element", "tbl_set", "tbl_elementset"))

#' BiocSet class
#' @name BiocSet
#' @rdname BiocSet-class
#' @aliases BiocSet-class
#' @description NULL
#' @slot element The element tibble from `tbl_elementset`
#' @slot set The set tibble from `tbl_elementset`
#' @slot elementset The elementset tibble created from user input
#' @slot active Character, indicates which tibble is active
#' @exportClass BiocSet
NULL

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
#' @description The \code{BiocSet} constructor, the show method, the slot 
#'     accessors, and creating a \code{BiocSet} object from an element set 
#'     tibble rather than character vector(s).
#' @rdname BiocSet-class
#' @param ... Named character() vectors of element sets. Each character vector 
#'     is an element set. The name of the character vectors are the name of the 
#'     sets.
#' @param active A character to indicate which tibble is active. The default is
#'     "elementset".
#' @return An S4 \code{BiocSet} object shown as a tripple tibble, where each 
#'     slot is a tibble.
#' @export
#' @examples
#' BiocSet(set1 = letters, set2 = LETTERS)
BiocSet <- function(..., active = c("elementset", "element", "set"))
{
    active <- match.arg(active)
    elementset <- .tbl_elementset(...)
    element <- .tbl_element(elementset)
    set <- .tbl_set(elementset)

    .BiocSet(element = element,
                set = set,
                elementset = elementset,
                active = active)
}

#' @rdname BiocSet-class
#' @param object A \code{BiocSet} object.
#' @docType methods
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

update_es_element <- function(es, value)
    .update(es, value)

update_es_set <- function(es, value)
    .update(es, value)

update_es_elementset <- function(es, value)
    .update(es, value)

#' @rdname BiocSet-class
#' @param x A \code{BiocSet} object. 
#' @exportMethod es_element
setGeneric("es_element", function(x) standardGeneric("es_element"))

#' @rdname BiocSet-class
setMethod("es_element", "BiocSet", .element)

#' @rdname BiocSet-class
#' @exportMethod es_set
setGeneric("es_set", function(x) standardGeneric("es_set"))

#' @rdname BiocSet-class
setMethod("es_set", "BiocSet", .set)

#' @rdname BiocSet-class
#' @exportMethod es_elementset
setGeneric("es_elementset", function(x) standardGeneric("es_elementset"))

#' @rdname BiocSet-class
setMethod("es_elementset", "BiocSet", .elementset)

`es_element<-` <- update_es_element

`es_set<-` <- update_es_set

`es_elementset<-` <- update_es_elementset

#' @rdname BiocSet-class
#' @param elementset A tibble with element set information.
#' @param element A tibble with element information.
#' @param set A tibble with set information.
#' @export
#' @examples
#'
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
