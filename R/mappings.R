#' Functions for mapping elements and sets to different ids
#'
#' @rdname mappings
#'
#' @param org The AnnotationDbi object to identify keys/mappings from 
#' @param from A character to indicate which identifier to map from
#' @param go Character, the column name for the GO ids. Default is "GO".
#' @param evidence Character, the evidence codes for GO associations with 
#'    a gene of interest. Default is all possible evidence codes.
#' @param ontology Character, which Gene Ontology. Default is BP, CC, and MF
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
go_sets <- function(org, from, go = c("GO", "GOID"), evidence = NULL,
    ontology = NULL)
{
    stopifnot(from %in% keytypes(org))

    go <- match.arg(go)
    map <- AnnotationDbi::select(
        org, keys(org, from), c(from, go), from
    )
    map <- map[!is.na(map),]

    evidence_choices <- levels(as.factor(map$EVIDENCE))   
    evid <- match.arg(evidence, evidence_choices, several.ok = TRUE)    
    ontology_choices <- levels(as.factor(map$ONTOLOGY))    
    onto <- match.arg(ontology, ontology_choices, several.ok = TRUE)

    if (!is.null(evidence) && !is.null(ontology)) {
        stopifnot(all(evid %in% evidence_choices),
            all(onto %in% ontology_choices))

        map <- filter(map, map$EVIDENCE %in% evid, map$ONTOLOGY %in% onto)
        evidence_split <- split(map$EVIDENCE, map[[go]])
        ontology_split <- split(map$ONTOLOGY, map[[go]])

        es <- do.call(BiocSet, split(map[[from]], map[[go]]))
        es %>% mutate_set(
            evidence = evidence_split,
            ontology = ontology_split
        )
    }
    else if (!is.null(evidence) && is.null(ontology)) {
        stopifnot(all(evidence %in% evidence_choices))

        map <- filter(map, map$EVIDENCE %in% evid)
        evidence_split <- split(map$EVIDENCE, map[[go]])
        evidence_unique <- lapply(evidence_split, unique)

        es <- do.call(BiocSet, split(map[[from]], map[[go]]))
        es %>% mutate_set(evidence = evidence_unique)
    }
    else if (is.null(evidence) && !is.null(ontology)) {
        stopifnot(all(ontology %in% ontology_choices))

        map <- filter(map, map$ONTOLOGY %in% onto)
        ontology_split <- split(map$ONTOLOGY, map[[go]])
        ontology_unique <- lapply(ontology_split, unique)

        es <- do.call(BiocSet, split(map[[from]], map[[go]]))
        es %>% mutate_set(ontology = ontology_unique)
    }
    else {
        do.call(BiocSet, split(map[[from]], map[[go]]))
    } 
}

#' @importFrom AnnotationDbi mapIds keytypes
#' @importFrom tibble enframe
.es_map <- function(es, org, from, to, multi)
{
    stopifnot(
        all(es_element(es)$element %in%
            keys(org, keytype = from)),
        from %in% keytypes(org),
        to %in% keytypes(org)
    )

    keys <- es_element(es)$element
    map <- mapIds(org, keys, to, from, multiVals = multi)
    tbl <- enframe(map, name = from, value = to)
    es %>% map_element(tbl[[from]], tbl[[to]])
}

#' @rdname mappings
#'
#' @param es The BiocSet objec to map the elements on
#' @param to A character to indicate which identifier to map to
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = c("PRKACA", "TGFA", "MAP2K1"), set2 = c("CREB3", "FOS"))
#' es_map_unique(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")
es_map_unique <- function(es, org, from, to)
    .es_map(es, org, from, to, multi = "first")

#' @rdname mappings
#'
#' @param multi How should multiple values be returned? Options include:
#' list: This will just return a list object to the end user
#' filter: This will remove all elements that contain multiple matches and will
#' therefore return a shorter vector than what came in whenever some of the keys
#' match more than one value
#' asNA: This will return an NA value whenever there are multiple matches
#' CharacterList: This just returns a SimpleCharacterList object
#' FUN: You can also supply a function to the 'multiVals' argument for custom
#' behaviours.
#' 
#' @export
#'
#' @examples
#' library(EnsDb.Hsapiens.v86)
#' es <- BiocSet(set1 = c("BCL2", "BCL2L11"), set2 = c("7SK", "AAAS", "A1CF"))
#' es_map_multiple(es, EnsDb.Hsapiens.v86, "GENENAME", "TXID", "list")
es_map_multiple <- function(es, org, from, to, multi = 
    c('list', 'filter', 'asNA', 'CharacterList'))
{
    if(!is.function(multi))
        multi <- match.arg(multi)
    .es_map(es, org, from, to, multi)
}

#' @rdname mappings
#'
#' @param species Which species the pathways are from
#'
#' @importFrom KEGGREST keggList keggGet
#'
#' @return For kegg_sets(), a BiocSet object with Entrez IDs reported as 
#'     elements (default from KEGGREST) and KEGG pathways as sets.
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

    grp <- cumsum(seq_along(paths$name) %% 10L == 1L)
    path_names <- split(paths$name, grp)
    elements <- lapply(path_names, function(x) {
        paths <- keggGet(x)
        lapply(paths, function(path) path$GENE[c(TRUE, FALSE)])
    })

    paths_len <- unlist(lapply(
        seq_along(elements), function(x) lengths(elements[[x]])
    ))
    elements <- unlist(elements, use.names = FALSE)
    elements <- split(elements, rep(seq_along(paths_len), paths_len))
    names(elements) <- paths$name[which(paths_len != 0)]

    do.call(BiocSet, elements)
}

#' @rdname mappings
#' 
#' @param add The id to add to the `BiocSet` object
#'
#' @export
#' 
#' @examples
#' es <- BiocSet(set1 = c("PRKACA", "TGFA", "MAP2K1"), set2 = c("FOS", "BRCA1"))
#' map <- map_add_element(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")
#' es %>% mutate_element(entrez = map)
map_add_element <- function(es, org, from, add)
{
    stopifnot(from %in% keytypes(org),
        add %in% keytypes(org))

    map <- mapIds(org,
        keys = es_element(es)$element,
        column = add,
        keytype = from,
        multivals = "first"
    )
    unname(map)
}

#' @rdname mappings
#' 
#' @export
#'
#' @examples
#' library(GO.db)
#' go <- go_sets(org.Hs.eg.db, "ENSEMBL")
#' map <- map_add_set(go, GO.db, "GOID", "DEFINITION")
#' go %>% mutate_set(definition = map)
map_add_set <- function(es, org, from, add)
{
    stopifnot(from %in% keytypes(org),
        add %in% keytypes(org))

    map <- mapIds(org,
        keys = as.character(es_set(es)$set),
        column = add,
        keytype = from,
        multivals = "first"
    )
    unname(map)
}
