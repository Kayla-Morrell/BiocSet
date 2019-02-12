#' A gene set representation as a tripple tibble
#'
#' @rdname geneset2
#'
#' @param ... For `GeneSet2()`, named character() vectors of gene
#'     sets. Each character vector is a gene set. The name of the
#'     character vector is the name of the gene set.
#' @param active A character to indicate which tibble is active.
#'
#' @return For `GeneSet2()`, a S4 'GeneSet' object in a tripple
#'     tibble representation.
#'
#' @export
#'
#' @examples
#' GeneSet2(set1 = letters, set2 = LETTERS)
GeneSet2 <- function(..., active = c("geneset", "gene", "set"))
{
    active <- match.arg(active)
    geneset <- tbl_geneset(...)
    gene <- tbl_gene(geneset)
    set <- tbl_set(geneset)

    .GeneSet2(gene = gene, set = set, geneset = geneset, active = active)
}

.gene <- function(x) x@gene

.set <- function(x) x@set

.geneset <- function(x) x@geneset

setMethod(
   "show", "GeneSet2",
   function(object)
{
    ## FIXME: better implementation
    tbl <- suppressMessages({
        inner_join(.geneset(object), .gene(object)) %>%
            inner_join(.set(object))
    })
    cat("class: ", class(object), "\n")
    print(tbl)
})


## implement common verbs, including implied constraints
##    gs %>% filter(p < 0.05)

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
setGeneric("gs_gene2", function(x) standardGeneric("gs_gene2"))

#' @export
setMethod("gs_gene2", "GeneSet2", .gene)

#' @export
setGeneric("gs_set2", function(x) standaradGeneric("gs_set2"))

#' @export
setMethod("gs_set2", "GeneSet2", .set)

#' @export
setGeneric("gs_geneset2", function(x) standardGeneric("gs_geneset2"))

#' @export
setMethod("gs_geneset2", "GeneSet2", .geneset)

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(gene) %>% filter(gene == "a")
filter.GeneSet2 <- function(.data, ...)
{
    tbl <- filter(.data, ...)
    .update(.data, tbl)
}

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% select(gene)
select.GeneSet2 <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- select(sub, ...)
    .update(.data, tbl)
}

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(set) %>% mutate(pval = rnorm(1:2))
mutate.GeneSet2 <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- mutate(sub, ...)
    .update(.data, tbl)
}

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% group_by(gene, set)
group_by.GeneSet2 <- function(.data, ..., add = FALSE)
{
    sub <- .active_value(.data)
    tbl <- group_by(sub, ..., add = FALSE)
    .update(.data, tbl)
}

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% group_by(set) %>% summarise(n = n()) %>% ungroup()
ungroup.GeneSet2 <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- ungroup(sub, ...)
    .update(.data, tbl)
}

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(set) %>% summarise(n = n())
summarise.GeneSet2 <- function(.data, ...)
{
    sub <- .active_value(.data)
    summarise(sub, ...)
}

#' @rdname geneset2
#'
#' @export
#'
#' @examples
#' gs <- GeneSet2(set1 = letters, set2 = LETTERS)
#' gs %>% gs_activate(gene) %>% arrange(desc(gene))
arrange.GeneSet2 <- function(.data, ...)
{
    sub <- .active_value(.data)
    tbl <- arrange(sub, ...)
    .update(.data, tbl)
}

#' @importFrom dplyr group_vars
#' @export
group_vars.GeneSet2 <- function(.data)
{
    sub <- .active_value(.data)
    group_vars(sub)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.GeneSet2 <- function(.data)
{
    active <- .active(.data)
    sub <- slot(.data, active)
    tbl_vars(sub)
}
