#' Functions for mapping elements and sets to different ids
#'
#' @rdname mappings
#'
#' @param org The AnnotationDbi object to identify keys/mappings from 
#' @param from A character to indicate which identifier to map from
#'
#' @importFrom AnnotationDbi keytypes mapIds
#'
#' @export
#'
#' @examples
#' library(org.Hs.eg.db)
#' go_sets(org.Hs.eg.db, "ENSEMBL")
go_sets <- function(org, from)
{
    stopifnot(from %in% keytypes(org))

    map <- AnnotationDbi::select(
        org, keys(org, from), c(from, "GO"), from
    )
    do.call(BiocSet, split(map[,names(map) == from], map$GO))
}

#' @rdname mappings
#'
#' @param es The BiocSet object to map the elements on
#' @param to A character to indicate which identifier to map to
#'
#' @importFrom AnnotationDbi mapIds
#' @importFrom tibble enframe
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = c("PRKACA", "TGFA", "MAP2K1"), set2 = c("CREB3", "FOS"))
#' es_map(es, org.Hs.eg.db, "SYMBOL", "ENTREZID") 
es_map <- function(es, org, from, to)
{
    stopifnot(
        from %in% keytypes(org),
        to %in% keytypes(org)
    )

    map <- mapIds(org, keys(org, from), to, from)
    tbl <- enframe(map, name = from, value = to)
    es %>% map_element(tbl$from, tbl$to)
}

#' @rdname mappings
#'
#' @param species Which species the pathways are from
#' @param pathways A character vector containing the pathways to map from
#'
#' @importFrom KEGGREST keggList keggGet
#'
#' @export
#'
#' @examples
#' pathways <- c("hsa05310", "hsa04110", "hsa05224", "hsa04970")
#' kegg_sets("hsa", pathways)
kegg_sets <- function(species, pathways) 
{
    paths <- enframe(keggList("pathway", species))
    paths <- mutate(
        paths,
        name = gsub("path:", "", name),
        value = gsub("\\-.*", "", value)
    )
    if (length(pathways) <= 10)
    {
        path <- keggGet(pathways)
        path[[1]]$GENE[c(TRUE, FALSE)]
    }
    else
    { 
        elements <- lapply(paths$name, function(x) {
            path <- keggGet(x)
            path[[1]]$GENE[c(TRUE, FALSE)]
        })
    }

    names(elements) <- paths$name
    elements <- elements[lengths(elements) != 0]

    do.call(BiocSet, elements)
}
