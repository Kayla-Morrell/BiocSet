context("import-class")

test_that("'import.gmt()' works", {
    gmtFile <- system.file(package = "GeneSet", "extdata", "hallmark.gene.symbol.gmt")
    gs <- import.gmt(gmtFile)
    expect_true(is_tbl_geneset(gs))
    expect_identical(dim(gs), c(7324L, 3L))
    expect_type(gs$source, "character")
})

test_that("'import()' works", {
    gmtFile <- system.file(package = "GeneSet", "extdata", "hallmark.gene.symbol.gmt")
    gs <- import.gmt(gmtFile)
    fl <- tempfile(fileext = ".gmt")
    export(gs, fl)
    expect_identical(gs, import(fl))

    gs <- tbl_geneset() %>% mutate(source = character(0))
    export(gs, fl)
    expect_identical(gs, import(fl))

    gs <- tbl_geneset(a = letters, b = LETTERS)
    export(gs, fl)
    expect_identical(gs %>% mutate(source = NA_character_), import(fl))

    fl <- tempfile(fileext = ".gmt")
    file.create(fl)
    expect_error(export(gs, fl))
})
