context("import-class")

test_that("'import.gmt()' works", {
    gmtFile <- system.file(package = "BiocSet", "extdata",
                           "hallmark.gene.symbol.gmt")
    es <- import.gmt(gmtFile)
    expect_true(is_tbl_elementset(.elementset(es)))
    expect_identical(dim(.elementset(es)), c(7324L, 3L))
    expect_type(.elementset(es)$source, "character")
})

test_that("'import()' works", {
    gmtFile <- system.file(package = "BiocSet", "extdata",
                           "hallmark.gene.symbol.gmt")
    es <- import.gmt(gmtFile)
    fl <- tempfile(fileext = ".gmt")
    export(es, fl)
    expect_identical(es, import(fl))

    es <- BiocSet() %>% mutate(source = character(0))
    fl <- tempfile(fileext = ".gmt")
    export(es, fl)
    expect_identical(es, import(fl))

    es <- BiocSet(a = letters, b = LETTERS)
    fl <- tempfile(fileext = ".gmt")
    export(es, fl)
    expect_identical(es %>% mutate(source = NA_character_), import(fl))
})


