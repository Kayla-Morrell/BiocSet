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

.normalize_mapping <-
    function(from, to)
{
    stopifnot(
        length(from) == length(to),
        is.null(names(from)), is.null(names(to))
    )

    to <- rep(to, lengths(from))
    from <- unlist(from)

    from <- rep(from, lengths(to))
    to <- unlist(to)

    tibble(element = unname(from), to = unname(to))
}

map_element <- function(.data, from, to, keep_unmapped) UseMethod("map_element")

map_element.BiocSet <-
    function(.data, from, to, keep_unmapped = TRUE)
{
    stopifnot(is.character(from),
        is.character(to) || is.list(to) || is(to, "CharacterList"),
        length(from) == length(to)
    )

    es <- es_elementset(.data)

    ## mapping <- tibble(element = from, to)
    mapping <- .normalize_mapping(from, to)
    if (keep_unmapped) {
        aux <- as_tibble(es) %>%        # un-mapped elements
            select(element) %>%
            mutate(to = element) %>%
            filter(!element %in% from)
        mapping <- bind_rows(mapping, aux)
    }

    es <-                               # map
        left_join(mapping, es) %>%
        select(-element, element = to)
    es <- es %>%                        # de-duplicate
        group_by(element, set) %>%
        summarise_all(list) %>%
        mutate_if(.test, unlist)

    sets <- es_set(.data) %>%
        filter(set %in% es$set)

    elements <- es_element(.data)
    elements <-                         # map
        left_join(mapping, elements) %>%
        select(-element, element = to)
    elements <- elements %>%
        group_by(element) %>%
        summarise_all(list) %>%
        mutate_if(.test, unlist)

    BiocSet_from_elementset(es, elements, sets)
}

#' Functions for mapping elements in the element tibble to different id types
#' 
#' @rdname mapping_element
#'
#' @param es The BiocSet objec to map the elements on
#' @param org The AnnotationDbi object to identify keys/mappings from
#' @param from A character to indicate which identifier to map from
#' @param to A character to indicate which identifier to map to
#'
#' @return For `es_map_unique()`, a BiocSet object with unique elements
#'
#' @export
#'
#' @examples
#' library(org.Hs.eg.db)
#' es <- BiocSet(set1 = c("C5", "GANC"), set2 = c("AFM", "CGB1", "ADAM32"))
#' es_map_unique(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")
es_map_unique <- function(es, org, from, to)
    .es_map(es, org, from, to, multi = "first")


#' @rdname mapping_element
#'
#' @param multi How should multiple values be returned? 
#'     Options include:     
#'     \itemize{
#'     \item{list: This will just return a list object to the end user}
#'     \item{filter: This will remove all elements that contain multiple 
#'     matches and will therefore return a shorter vector than what came in 
#'     whenever some of the keys match more than one value}
#'     \item{asNA: This will return an NA value whenever there are multiple 
#'     matches}
#'     \item{CharacterList: This just returns a SimpleCharacterList object}
#'     \item{FUN: A function can be supplied to the 'multiVals' argument 
#'     for custom behaviors}
#'     }
#'
#' @export
#'
#' @return For `es_map_multiple()`, a BiocSet object with multiple mappings for
#'     certain elements
#'
#' @examples
#' 
#' es_map_multiple(es, org.Hs.eg.db, "SYMBOL", "ENSEMBLTRANS", "asNA")
es_map_multiple <- function(es, org, from, to, multi =
    c('list', 'filter', 'asNA', 'CharacterList'))
{
    if(!is.function(multi))
        multi <- match.arg(multi)
    .es_map(es, org, from, to, multi)
}


#' @rdname mapping_element
#'
#' @param add The id to add to the `BiocSet` object
#'
#' @return For `map_add_element()`, a BiocSet object with a new column in the 
#'     element tibble with the mapping of the new id type
#'
#' @export
#'
#' @examples
#'
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
