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

    expect_error(go_sets(org.Hs.eg.db))
    expect_error(go_sets(org.Hs.eg.db, ENSEMBL))
    expect_error(go_sets(org.Hs.eg.db, "IDS"))
    expect_error(go_sets(species, "ENSEMBL"))
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
    expect_error(es_map(es, species, "SYMBOL", "ENTREZID"))
})

test_that("'kegg_sets()' works",
{
    pathways <- c("hsa05310", "hsa05224", "hsa04110")
    es <- kegg_sets("hsa", pathways)

    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_element(es)), c(288L, 1L))
    expect_identical(dim(es_set(es)), c(3L, 1L))
    expect_identical(dim(es_elementset(es)), c(302L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es)))

    pathways2 <- c("hsa00010", "hsa00020", "hsa00030", "hsa00040", "hsa00051", "hsa00052", "hsa00053", "hsa00061", "hsa00062", "hsa00071", "hsa00072", "hsa00100", "hsa00120")
    es2 <- kegg_sets("hsa", pathways2)

    expect_identical(dim(es_element(es2)), c(281L, 1L))
    expect_identical(dim(es_set(es2)), c(13L, 1L))
    expect_identical(dim(es_elementset(es2)), c(388L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es2)))

    expect_error(kegg_sets(hsa, pathways))
    expect_error(kegg_sets("hsa"))
    expect_error(kegg_sets("hsa", "pathways"))
    expect_error(kegg_sets("hsa", 1:10))     
})
