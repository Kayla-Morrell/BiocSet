#' Functions for mapping sets in the set tibble to different id types
#' @rdname mapping_set
#' @name mapping_set
#' @description Functions for creating \code{BiocSet} objects from GO sets and
#'     KEGG sets, and creating a new set mapping from a current \code{BiocSet} 
#'     object. \code{map_add_set} will add the mapping as a new column instead 
#'     of overwriting the current one used for the mapping.
#' @param org The AnnotationDbi object to identify keys/mappings from.
#' @param from A character to indicate which identifier to map from.
#' @param go A character to indicate the column name for the GO ids. 
#'     Default is "GO".
#' @param evidence A character to indicate the evidence codes for GO 
#'     associations with a gene of interest. Default is all possible evidence 
#'     codes.
#' @param ontology A character to indicate which Gene Ontology to use. 
#'     Default is BP, CC, and MF.
#' @importFrom AnnotationDbi keytypes keys
#' @return For \code{go_sets}, a \code{BiocSet} object with GO ids as the set 
#'     ids.
#' @export
#' @examples
#' library(org.Hs.eg.db)
#' go <- go_sets(org.Hs.eg.db, "ENSEMBL")
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

#' @rdname mapping_set
#' @name mapping_set
#' @param species Which species the pathways are from.
#' @importFrom KEGGREST keggList keggGet
#' @return For \code{kegg_sets}, a \code{BiocSet} object with Entrez IDs 
#'     reported as elements (default from KEGGREST) and KEGG pathways as sets.
#' @export
#' @examples
#'
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

#' @rdname mapping_set
#' @name mapping_set
#' @param .data The BiocSet object that contains the set tibble being mapped.
#' @param to A character to indicate which identifier to map to.
#' @return For \code{map_set}, a BiocSet object with the mapped set present in 
#'     the set tibble.
#' @export
#' @examples
#'
#' es <- BiocSet(set1 = letters, set2 = LETTERS)
#' es %>% map_set("set1", "foo") 
map_set <- function(.data, from, to) UseMethod("map_set")

#' @importFrom plyr mapvalues
#' @export
map_set.BiocSet <- function(.data, from, to)
{
    stopifnot(is.character(from), is.character(to), length(from) == length(to))

    set <- .set(.data)
    idx <- set$set %in% from
    set$set <- mapvalues(set$set, from, to)

    es <- .elementset(.data)
    idx <- es$set %in% from
    es$set <- mapvalues(es$set, from, to)

    initialize(.data, set = set, elementset = es)
}

#' @rdname mapping_set
#' @name mapping_set
#' @param add The id to add to the \code{BiocSet} object.
#' @return For \code{map_add_set}, a \code{BiocSet} object with a new column in
#'     the set tibble with the mapping of the new id type.
#' @export
#' @examples
#'
#' library(GO.db)
#' map <- map_add_set(go, GO.db, "GOID", "DEFINITION")
#' go %>% mutate_set(definition = map)
map_add_set <- function(.data, org, from, add)
{
    stopifnot(from %in% keytypes(org),
        add %in% keytypes(org))

    map <- mapIds(org,
        keys = as.character(es_set(.data)$set),
        column = add,
        keytype = from,
        multivals = "first"
    )
    unname(map)
}
