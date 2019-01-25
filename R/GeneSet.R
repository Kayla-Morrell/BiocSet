GeneSet <- function(...)
{
    geneset <- tbl_geneset(...)
    gene <- tbl_gene(geneset)
    set <- tbl_set(geneset)

    .GeneSet(gene = gene, set = set, geneset = geneset, active = "geneset")
}

.gene <- function(x) x@gene

`.gene<-` <- function(x, value)
{
    stopifnot(value[["gene"]] %in% x@gene[["gene"]])
    x@gene <- value
    x@geneset <- x@geneset[x@geneset[["gene"]] %in% value[["gene"]],]
    x

    ## should move to setMethod?
}

.set <- function(x) x@set

`.set<-` <- function(x, value)
{
    stopifnot(value[["set"]] %in% x@set[["set"]])
    x@set <- value
    x@geneset <- x@geneset[x@geneset[["set"]] %in% value[["set"]],]
    x

    ## should move to setMethod?
}

.geneset <- function(x) x@geneset

`.geneset<-` <- function(x, value)
{
    stopifnot(value[["gene"]] %in% x@geneset[["gene"]],
              value[["set"]] %in% x@geneset[["set"]])
    x@geneset <- value
    x@gene <- x@gene[x@gene[["gene"]] %in% value[["gene"]],]
    x@set <- x@set[x@set[["set"]] %in% value[["set"]],]
    x
    
    ## check with gene and set
    ## should move to setMethod?
}

.active <- function(x) x@active

`.active<-` <- function(x, value)
{
    value <- gsub('"', '', value)
    stopifnot(value %in% c("gene", "set", "geneset"))
    x@active <- value
    x
}

## interface 1

setMethod(
    "show", "GeneSet",
    function(object)
{
    cat(
        "class: ", class(object), "\n",
        "\ngene():\n",
        sep = ""
    )
    print(.gene(object), n = 3)
    cat("\nset():\n")
    print(.set(object), n = 3)
    cat("\ngeneset():\n")
    print(.geneset(object), n = 3)
})

## implement activate() (like tidygraph...) and common verbs
##
##        gs %>% activate(gene) %>% filter(p < 0.05)

gs_activate <- function(.data, ...)
{
    UseMethod("gs_activate")
}

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
        .gene(x) <- value
        x
})

setMethod(
    ".update", "tbl_set",
    function(x, value)
{
        .set(x) <- value
        x
})

setMethod(
    ".update", "tbl_geneset",
    function(x, value)
{
        .geneset(x) <- value
        x
})


filter.GeneSet <- function(.data, ...)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub  %>% filter(...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

select.GeneSet <- function(.data, ...)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub %>% select(...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

mutate.GeneSet <- function(.data, ...)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub %>% mutate(...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

group_by.GeneSet <- function(.data, ..., add = FALSE)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub %>% group_by(..., add = FALSE)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

ungroup.GeneSet <- function(.data, ...)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub %>% ungroup(...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

summarise.GeneSet <- function(.data, ...)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub %>% summarise(...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

arrange.GeneSet <- function(.data, ...)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl <- sub %>% arrange(...)
    class(tbl) <- class(sub)
    .update(.data, tbl)
}

## things might get ugly as the other functions update class...

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
