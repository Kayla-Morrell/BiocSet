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

    expect_true(all(es_element(es)$element %in% 
        keys(org.Hs.eg.db, keytype="ENSEMBL")))
    expect_true(all(es_set(es)$set %in%
        keys(org.Hs.eg.db, keytype="GO")))

    expect_error(es <- go_sets(org.Hs.eg.db))
    expect_error(es <- go_sets(org.Hs.eg.db, ENSEMBL))
    expect_error(es <- go_sets(org.Hs.eg.db, "IDS"))
})

test_that("'es_map()' works",
{
    library(org.Hs.eg.db)
    es <- BiocSet(
        set1 = c("BRCA1", "BRCA2", "TGFA", "ERCC2"),
        set2 = c("PRNP", "FMR1", "PAX3")
    )
    es1 <- es %>% es_map(org.Hs.eg.db, "SYMBOL", "ENTREZID")

    expect_s4_class(es1, "BiocSet")
    expect_identical(dim(es_element(es1)), c(7L, 1L))
    expect_identical(dim(es_set(es1)), c(2L, 1L))
    expect_identical(dim(es_elementset(es1)), c(7L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es1)))

    expect_true(all(es_element(es1)$element %in%
        keys(org.Hs.eg.db, keytype="ENTREZID")))
    
    expect_error(es_map(es, org.Hs.eg.db))
    expect_error(es_map(es, org.Hs.eg.db, "SYMBOL"))
    expect_error(es_map(es, org.Hs.eg.db, SYMBOL, "ENTREZID"))
    expect_error(es_map(es, org.Hs.eg.db, "SYMBOL", 10))
    expect_error(es_map(es, org.Hs.eg.db, "SYMBOL", "IDS"))
})

test_that("'kegg_sets()' works",
{
    pathways <- c("hsa05310", "hsa05224", "hsa04110")
    es <- kegg_sets("hsa", pathways)

    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_element(es)), c(288L, 1L))
    expect_identical(dim(es_set(es)), c(3L, 1L))
    expect_equal(dim(es_set(es))[1], length(pathways))
    expect_identical(dim(es_elementset(es)), c(302L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es)))

    expect_error(kegg_sets(hsa, pathways))
    expect_error(kegg_sets("hsa"))
    expect_error(kegg_sets("hsa", "pathways"))
    expect_error(kegg_sets("hsa", 1:10))     
})
