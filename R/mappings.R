#' Functions for mapping elements and sets to different ids
#'
#' @rdname mappings
#'
#' @param org The AnnotationDbi object to identify keys/mappings from 
#' @param from A character to indicate which identifier to map from
#'
#' @importFrom AnnotationDbi keytypes keys
#'
#' @return A BiocSet object
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
#' @importFrom AnnotationDbi mapIds keytypes
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
#'
#' @importFrom KEGGREST keggList keggGet
#'
#' @return For kegg_sets(), a BiocSet object with Entrez IDs reported as 
#'        elements (default from KEGGREST) and KEGG pathways as sets.
#'
#' @export
#'
#' @examples
#' kegg_sets("hsa")
kegg_sets <- function(species) 
{
    stopifnot(species %in% keggList("organism")[,"organism"])

    name <- value <- NULL
    paths <- enframe(keggList("pathway", species))
    paths <- mutate(
        paths,
        name = gsub("path:", "", name),
        value = gsub("\\-.*", "", value)
        )

    if (length(paths$name) <= 10)
    {
        path <- keggGet(paths$name)
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

    names(elements) <- paths$name
    elements <- elements[lengths(elements) != 0]

    do.call(BiocSet, elements)
}

#' @rdname mappings
#' 
#' @param add The id to add to the `BiocSet` object
#'
#' @export
#'
#' @examples
#' library(org.Hs.eg.db)
#' es <- BiocSet(set1 = c("PRKACA", "TGFA", "MAP2K1"), set2 = c("FOS", "BRCA1"))
#' map <- map_add(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")
#' es %>% mutate_element(entrez = map)
map_add <- function(es, org, from, add) 
{
    stopifnot(from %in% keytypes(org),
        add %in% keytypes(org))
     
    if (from %in% c("ENTREZID", "ENSEMBL", "SYMBOL")) 
    {
        map <- mapIds(org, 
            keys = es_element(es)$element,
            column = add,
            keytype = from,
            multivals = "first"
        )
        unname(map)
    }
    else 
    {
        map <- mapIds(org,
            keys = as.character(es_set(es)$set),
            column = add,
            keytype = from,
            multivals = "first"
        )
        unname(map)
    }
} 
