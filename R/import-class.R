.GMTFile = setClass("GMTFile", contains = "RTLFile")

GMTFile = function(resource, ...)
    .GMTFile(resource = resource)

#' @importFrom rtracklayer import export resource
#' @importFrom methods new
#' @importFrom utils write.table
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


#' Importing/exporting
#' @rdname import
#' @name import
#' @description Importing/exporting and formating of element sets as a 
#'     \code{BiocSet} object.
#' @param con For \code{import}, the file name or URL the element set is
#'     loaded from. For \code{export}, the file name or URL the element set is
#'     written to.
#' @param format For \code{import}, the format of the input. For 
#'     \code{export}, the format of the output.
#' @param text If con is missing this is a character vector directly providing 
#'     the element set that should be imported.
#' @param ... Parameters to pass to the format-specific method
#' @return For `import()`, a BiocSet object
#' @aliases import,GMTFile,ANY,ANY-method
#' @export
#' @examples
#' gmtFile <- system.file(package = "BiocSet", "extdata",
#'     "hallmark.gene.symbol.gmt")
#' tbl <- import(gmtFile)
setMethod(
    "import", c("GMTFile", "ANY", "ANY"),
    function(con, format, text, ...)
{
    import.gmt(resource(con))
})

#' @importFrom rlang .data
export.BiocSet <- function(tbl, path = tempfile(fileext = ".gmt")) {
    stopifnot(.is_tbl_elementset(es_elementset(tbl)))
    
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
#'     where the BiocSet object was written to
#'
#' @export
#'
#' @examples
#' 
#' tbl2 <- BiocSet(set1 = letters, set2 = LETTERS)
#' fl <- tempfile(fileext = ".gmt")
#' gmt <- export(tbl2, fl)
setMethod(
    "export", c("BiocSet", "GMTFile", "ANY"),
    function(object, con, format, ...)
{
    export.BiocSet(object, resource(con))
    con
})
