context("GeneSet")

test_that("'GeneSet()' works",
{
    gs <- GeneSet(a = letters, b = LETTERS)
    expect_s4_class(gs, "GeneSet")
    expect_identical(gs@active, "geneset")
})
