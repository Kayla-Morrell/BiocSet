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

#' @importFrom methods slot
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

#' @rdname geneset
#'
#' @export
gs_activate <- function(.data, what)
{
    UseMethod("gs_activate")
}

#' @rdname geneset
#'
#' @param .data The 'GeneSet' tibble.
#' @param what Which of the three tibbles to activate
#'
#' @importFrom rlang quo_text enquo
#' @importFrom methods initialize
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

#' @rdname geneset
#'
#' @export
update_gs_gene <- function(gs, value)
    .update(gs, value)

#' @rdname geneset
#'
#' @export
update_gs_set <- function(gs, value)
    .update(gs, value)

#' @rdname geneset
#'
#' @export
update_gs_geneset <- function(gs, value)
    .update(gs, value)

#' @rdname geneset
#'
#' @export
setGeneric("gs_gene", function(x) standardGeneric("gs_gene"))

#' @rdname geneset
#'
#' @exportMethod gs_gene
setMethod("gs_gene", "GeneSet", .gene)

#' @rdname geneset
#'
#' @export
setGeneric("gs_set", function(x) standardGeneric("gs_set"))

#' @rdname geneset
#'
#' @exportMethod gs_set
setMethod("gs_set", "GeneSet", .set)

#' @rdname geneset
#'
#' @export
setGeneric("gs_geneset", function(x) standardGeneric("gs_geneset"))

#' @rdname geneset
#'
#' @exportMethod gs_geneset
setMethod("gs_geneset", "GeneSet", .geneset)

## gs_set(gs) <- gs_set(gs) %>% left_join(paths, by = c("set" = "name"))

#' @rdname geneset
#'
#' @export
`gs_gene<-` <- update_gs_gene

#' @rdname geneset
#'
#' @export
`gs_set<-` <- update_gs_set

#' @rdname geneset
#'
#' @export
`gs_geneset<-` <- update_gs_geneset

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
    stopifnot(!any(c("gene", "set") %in% names(list(...))))

    sub <- .active_value(.data)
    tbl <- mutate(sub, ...)
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @export
map_gene <- function(.data, from, to) UseMethod("map_gene")

#' @rdname geneset
#'
#' @param from a vector of the values to be replaced
#' @param to a vector of the replacement values
#'
#' @importFrom stats setNames
#'
#' @export
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% map_gene(letters, LETTERS)
map_gene.GeneSet <- function(.data, from, to)
{
    stopifnot(is.character(from), is.character(to), length(from) == length(to))

    gene <- .gene(.data)
    idx <- gene$gene %in% from
    gene$gene[idx] <- unname(setNames(to, from)[gene$gene[idx]])

    gs <- .geneset(.data)
    idx <- gs$gene %in% from
    gs$gene[idx] <- unname(setNames(to, from)[gs$gene[idx]])

    initialize(.data, gene = gene, geneset = gs)
}

#' @rdname geneset
#'
#' @export
map_set <- function(.data, from, to) UseMethod("map_set")

#' @rdname geneset
#'
#' @importFrom plyr mapvalues
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(a = letters, B = LETTERS)
#' gs %>% map_set("set1", "foo")
map_set.GeneSet <- function(.data, from, to)
{
    stopifnot(is.character(from), is.character(to), length(from) == length(to))

    set <- .set(.data)
    idx <- set$set %in% from
    set$set <- mapvalues(set$set, from, to)

    gs <- .geneset(.data)
    idx <- gs$set %in% from
    gs$set <- mapvalues(gs$set, from, to)

    initialize(.data, set = set, geneset = gs)
}

#' @rdname geneset
#'
#' @param add by default, `group_by()` will override existing groups. To add to
#' existing groups, add should be TRUE.
#'
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% group_by(gene, set)
group_by.GeneSet <- function(.data, ..., add = FALSE)
{
    sub <- .active_value(.data)
    group_by(sub, ..., add = FALSE)
}

## #' @rdname geneset
## #'
## #' @param x a GeneSet
## #'
## #' @export
## #'
## #' @examples
## #' gs <- GeneSet(set1 = letters, set2 = LETTERS)
## #' gs %>% group_by(set) %>% summarise(n = n()) %>% ungroup()
## ungroup.GeneSet <- function(x, ...)
## {
##     sub <- .active_value(x)
##     tbl <- ungroup(sub, ...)
##     .update(x, tbl)
## }

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
    .update(.data, tbl)
}

#' @rdname geneset
#'
#' @importFrom dplyr tbl_vars
#' @export
#'
#' @examples
#' gs <- GeneSet(set1 = letters, set2 = LETTERS)
#' gs %>% mutate(pval = rnorm(1:52)) %>% tbl_vars()
tbl_vars.GeneSet <- function(x)
{
    active <- .active(x)
    sub <- slot(x, active)
    tbl_vars(sub)
}
