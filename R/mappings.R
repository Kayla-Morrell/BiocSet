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
    do.call(BiocSet, split(map[[from]], map$GO))
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
#' library(org.Hs.eg.db)
#' es <- BiocSet(set1 = c("PRKACA", "TGFA", "MAP2K1"), set2 = c("CREB3", "FOS"))
#' es_map(es, org.Hs.eg.db, "SYMBOL", "ENTREZID") 
es_map <- function(es, org, from, to)
{
    stopifnot(
        all(es_element(es)$element %in%
            keys(org, keytype = from)),
        from %in% keytypes(org),
        to %in% keytypes(org)
    )

    map <- mapIds(org, keys(org, from), to, from)
    tbl <- enframe(map, name = from, value = to)
    es %>% map_element(tbl[[from]], tbl[[to]])
}

#' @rdname mappings
#'
#' @param species Which species the pathways are from
#' @param pathways A character vector containing the pathways to map from
#'
#' @importFrom KEGGREST keggList keggGet
#'
#' @return A BiocSet object with Entrez IDs reported as elements (default from
#'        KEGGREST) and KEGG pathways as sets.
#'
#' @export
#'
#' @examples
#' pathways <- c("hsa05310", "hsa04110", "hsa05224", "hsa04970")
#' kegg_sets("hsa", pathways)
kegg_sets <- function(species, pathways) 
{
    stopifnot(species %in% keggList("organism")[,"organism"])

    if (length(pathways) <= 10)
    {
        path <- keggGet(pathways)
        elements <- lapply(seq_along(path), function(x) {
            path[[x]]$GENE[c(TRUE, FALSE)]
        })
    }
    else
    { 
        elements <- lapply(paths$name, function(x) {
            path <- keggGet(x)
            path[[1]]$GENE[c(TRUE, FALSE)]
        })
    }

    names(elements) <- pathways
    elements <- elements[lengths(elements) != 0]

    do.call(BiocSet, elements)
}
