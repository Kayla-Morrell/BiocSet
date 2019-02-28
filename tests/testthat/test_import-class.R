context("import-class")

test_that("'import.gmt()' works", {
    gmtFile <- system.file(package = "GeneSet", "extdata",
                           "hallmark.gene.symbol.gmt")
    gs <- import.gmt(gmtFile)
    expect_true(is_tbl_geneset(.geneset(gs)))
    expect_identical(dim(.geneset(gs)), c(7324L, 3L))
    expect_type(.geneset(gs)$source, "character")
})

test_that("'import()' works", {
    gmtFile <- system.file(package = "GeneSet", "extdata",
                           "hallmark.gene.symbol.gmt")
    gs <- import.gmt(gmtFile)
    fl <- tempfile(fileext = ".gmt")
    export(gs, fl)
    expect_identical(gs, import(fl))

    gs <- GeneSet() %>% mutate(source = character(0))
    fl <- tempfile(fileext = ".gmt")
    export(gs, fl)
    expect_identical(gs, import(fl))

    gs <- GeneSet(a = letters, b = LETTERS)
    fl <- tempfile(fileext = ".gmt")
    export(gs, fl)
    expect_identical(gs %>% mutate(source = NA_character_), import(fl))
})


