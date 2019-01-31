#' A gene set representation as a tripple tibble
#'
#' @rdname geneset
#'
#' @param ... For `GeneSet()`, named character() vectors of gene
#'     sets. Each character vector is a gene set. The name of the
#'     character vector is the name of the gene set.
#' @param active A character to indicate which tibble is active.
#'
#' @return For `GeneSet()`, a S4 'GeneSet' object in a tripple
#'     tibble representation.
#'
#' @export
#'
#' @examples
#' GeneSet(set1 = letters, set2 = LETTERS)
GeneSet <- function(..., active = c("geneset", "gene", "set"))
{
    active <- match.arg(active)
    geneset <- tbl_geneset(...)
    gene <- tbl_gene(geneset)
    set <- tbl_set(geneset)

    .GeneSet(gene = gene, set = set, geneset = geneset, active = active)
}

.gene <- function(x) x@gene

.set <- function(x) x@set

.geneset <- function(x) x@geneset

.active <- function(x) x@active

`.active<-` <- function(x, value)
{
    value <- gsub('"', '', value)
    stopifnot(value %in% c("gene", "set", "geneset"))
    x@active <- value
    x
}

.active_value <-
    function(x)
{
    slot(x, .active(x))
}

setMethod(
    "show", "GeneSet",
    function(object)
    {
        active <- .active(object)
        cat("class: ", class(object), "\n", sep = "")
        cat("\ngene()", if (active == "gene") " <active>", ":\n", sep = "")
        print(.gene(object), n = 3)
        cat("\nset()", if (active == "set") " <active>", ":\n", sep = "")
        print(.set(object), n = 3)
        cat("\ngeneset()", if (active == "geneset") " <active>", ":\n", sep = "")
        print(.geneset(object), n = 3)
    })

gs_activate <- function(.data, ...)
{
    UseMethod("gs_activate")
}

#' @rdname geneset
#'
#' @param .data The 'GeneSet' tibble.
#' @param what Which of the three tibbles to activate
#'
#' @importFrom rlang quo_text enquo
#' 
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs_activate(gs, gene)
gs_activate.GeneSet <- function(.data, what)
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
    ".update", "tbl_gene",
    function(x, value)
{
    stopifnot(all(value$gene %in% .gene(x)$gene))
    geneset <- filter(.geneset(x), .geneset(x)$gene %in% value$gene)
    initialize(x, gene = value, geneset = geneset)
})

setMethod(
    ".update", "tbl_set",
    function(x, value)
{
    stopifnot(all(value$set %in% .set(x)$set))
    geneset <- filter(.geneset(x), .geneset(x)$set %in% value$set)
    initialize(x, set = value, geneset = geneset)
})

setMethod(
    ".update", "tbl_geneset",
    function(x, value)
{
    stopifnot(
        all(value$gene %in% .geneset(x)$gene),
        all(value$set %in% .geneset(x)$set)
    )
    gene <- filter(.gene(x), .gene(x)$gene %in% value$gene)
    set <- filter(.set(x), .set(x)$set %in% value$set)
    initialize(x, gene = gene, set = set, geneset = value)
})

#' @export
setGeneric("gene", function(x) standardGeneric("gene"))

#' @export
setMethod("gene", "GeneSet", .gene)

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(gene) %>% filter(gene == "a")
filter.GeneSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- filter(sub, ...)
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% select(gene)
select.GeneSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- select(sub, ...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(set) %>% mutate(pval = rnorm(1:2))
mutate.GeneSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- mutate(sub, ...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% group_by(gene, set)
group_by.GeneSet <- function(.data, ..., add = FALSE)
{
    sub <- .active_value(.data)
    tbl <- group_by(sub, ..., add = FALSE)
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% group_by(set) %>% summarise(n = n()) %>% ungroup()
ungroup.GeneSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- ungroup(sub, ...)
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(set) %>% summarise(n = n())
summarise.GeneSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    summarise(sub, ...)
}

#' @rdname geneset
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(gene) %>% arrange(desc(gene))
arrange.GeneSet <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- arrange(sub, ...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

#' @importFrom dplyr group_vars
#' @export
group_vars.GeneSet <- function(.data)
{
    sub <- .active_value(.data)
    group_vars(sub)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.GeneSet <- function(.data)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl_vars(sub)
}

## interface 2

#setMethod(
#   "show", "GeneSet",
#   function(object)
#{
#    ## FIXME: better implementation
#    tbl <- suppressMessages({
#        inner_join(.geneset(object), .gene(object)) %>%
#            inner_join(.set(object))
#    })
#    cat("class: ", class(object), "\n")
#    print(tbl)
#})

## implement common verbs, including implied constraints
##    gs %>% filter(p < 0.05)
