GeneSet <- function(...)
{
    ## like tbl_geneset
    geneset <- tbl_geneset(...)
    ## FIXME: implement tbl_*
    gene <- tbl_gene(geneset)
    set <- tbl_set(geneset)

    .GeneSet(gene = gene, set = set, geneset = geneset)
}

.gene <- function(x) x@gene

`.gene<-` <- function(x, value ) {
    x@gene <- value
    x
}

.set <- function(x) x@set

`.set<-` <- function(x, value) {
    x@set <- value
    x
}

.geneset <- function(x) x@geneset

## interface 1

setMethod(
    "show", "GeneSet",
    function(object)
{
    cat(
        "class: ", class(object), "\n",
        "gene():\n",
        sep = ""
    )
    print(.gene(object), n = 3)
    cat("set():\n")
    print(.set(object), n = 3)
    cat("geneset():\n")
    print(.geneset(object), n = 3)
})

## implement activate() (like tidygraph...) and common verbs
##
##        gs %>% activate(gene) %>% filter(p < 0.05)
##
## Probably another slot in GeneSet `activate` updated by activate()

## interface 2

setMethod(
    "show", "GeneSet",
    function(object)
{
    ## FIXME: better implementation
    tbl <- suppressMessages({
        inner_join(.geneset(gs), .gene(gs)) %>%
            inner_join(.set(gs))
    })
    cat("class: ", class(object), "\n")
    print(tbl)
})

## implement common verbs, including implied constraints
##    gs %>% filter(p < 0.05)
