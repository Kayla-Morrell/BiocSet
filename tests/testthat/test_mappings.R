context("mapping")

test_that("'go_sets()' works",
{
    library(org.Hs.eg.db)
    es <- go_sets(org.Hs.eg.db, "ENSEMBL")

    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_element(es)), c(22206L, 1L))
    expect_identical(dim(es_set(es)), c(17495L, 1L))
    expect_identical(dim(es_elementset(es)), c(282353L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es)))
    
    expect_match(es_element(es), keys(org.Hs.eg.db, keytype = "ENSEMBL"))
})

test_that("'es_map()' works",
{

})

test_that("'kegg_sets()' works",
{

})
