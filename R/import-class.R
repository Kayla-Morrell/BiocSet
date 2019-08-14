#' Importing and formating of element sets as an S3 class tibble
#' @rdname import
#'
#' @export
.GMTFile = setClass("GMTFile", contains = "RTLFile")

#' @rdname import
#'
#' @param resource For `GMTFile()`, the .gmt file that will be imported in.
#'
#' @return For `GMTFile()`, an object representing the path to a .gmt file.
#'
#' @export
GMTFile = function(resource, ...)
    .GMTFile(resource = resource)

#' @rdname import
#'
#' @param path For `import.gmt()`, a file name or URL containing element sets.
#'
#' @importFrom rtracklayer import export resource
#' @importFrom methods new
#' @importFrom utils write.table
#'
#' @export
#'
#' @examples
#' gmtFile <- system.file(package = "BiocSet", "extdata",
#'     "hallmark.gene.symbol.gmt")
#' import(gmtFile)

import.gmt <- function(path) {
    sets <- readLines(path)
    sets <- strsplit(sets, "\t")
    names <- vapply(sets, function(set) set[[1]], character(1))
    elements <- lapply(sets, function(set) set[-(1:2)])
    names(elements) <- names

    source <- vapply(sets, function(set) set[[2]], character(1))
    source[source=="NA" | !nzchar(source)] <- NA
    tbl <- do.call(BiocSet, elements)
    tbl <- tbl %>% mutate_set(source = source)
    tbl
}


#' @rdname import
#'
#' @param con For `import()`, the file name or URL the element set is
#'     loaded from. For `export()`, the file name or URL the element set
#'     is to be saved to.
#' @param format For `import()`, the format of the output.
#' @param text For `import()`, if con is missing this is a character
#'     vector directly providing the element set that should be imported.
#' @param ... Parameters to pass to the format-specific method
#'
#' @return For `import()`, tbl_elementset
#'
#' @export
setMethod(
    "import", c("GMTFile", "ANY", "ANY"),
    function(con, format, text, ...)
{
    import.gmt(resource(con))
})

#' @rdname import
#'
#' @param tbl For `export.BiocSet()`, a BiocSet that
#'     should be exported to a gmt file.
#'
#' @importFrom rlang .data
#'
#' @export

export.BiocSet <- function(tbl, path = tempfile(fileext = ".gmt")) {
    stopifnot(is_tbl_elementset(es_elementset(tbl)))
    
    if(!"source" %in% names(es_set(tbl)))
        tbl <- mutate_set(tbl, source = rep(NA_character_, nrow(es_set(tbl))))

    es <- es_elementset(tbl)
    ## bug in dplyr
    if(nrow(es)==0L){
        sets <- tibble(set = character(0), element = character(0))
    } else {
        sets <- group_by(es, .data$set) %>%
            summarise(element = paste(.data$element, collapse = "\t"))
    }
    
    tbl <- left_join_set(tbl, sets, by = c(set = "set"))

    write.table(es_set(tbl), path, sep = "\t",
                col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#' @rdname import
#'
#' @param object For `export()`, the object to be exported.
#'
#' @return For `export()`, a GMTFile object representing the location
#'     where the element set was written
#'
#' @export
setMethod(
    "export", c("BiocSet", "GMTFile", "ANY"),
    function(object, con, format, ...)
{
    export.BiocSet(object, resource(con))
    con
})
