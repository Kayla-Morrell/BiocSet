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

    es1 <- go_sets(org.Hs.eg.db, "SYMBOL", evidence = c("IDA", "IMP", "HDA"))

    expect_s4_class(es1, "BiocSet")
    expect_identical(dim(es_element(es1)), c(13138L, 1L))
    expect_identical(dim(es_set(es1)), c(11077L, 2L))
    expect_identical(dim(es_elementset(es1)), c(87444L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es1)))
    
    es2 <- go_sets(org.Hs.eg.db, "SYMBOL", ontology = "MF")

    expect_s4_class(es2, "BiocSet")
    expect_identical(dim(es_element(es2)), c(16969L, 1L))
    expect_identical(dim(es_set(es2)), c(3903L, 2L))
    expect_identical(dim(es_elementset(es2)), c(56051L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es2)))

    es3 <- go_sets(org.Hs.eg.db, "ENSEMBL", evidence = c("IEP", "HEP"),
        ontology = c("CC", "MF"))

    expect_s4_class(es3, "BiocSet")
    expect_identical(dim(es_element(es3)), c(0L, 1L))
    expect_identical(dim(es_set(es3)), c(0L, 3L))
    expect_identical(dim(es_elementset(es3)), c(0L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es3)))

    expect_error(go_sets(org.Hs.eg.db))
    expect_error(go_sets(org.Hs.eg.db, ENSEMBL))
    expect_error(go_sets(org.Hs.eg.db, "IDS"))
    expect_error(go_sets(species, "ENSEMBL"))
    expect_error(go_sets(org.Hs.eg.db, "ENSEMBL", c("IDA", "IMP")))
    expect_error(go_sets(org.Hs.eg.db, "ENSEMBL", c("IDA", "IMP"), "CC"))
    expect_error(go_sets(org.Hs.eg.db, "ENSEMBL", "CC"))
    expect_error(go_sets(org.Hs.eg.db, "ENSEMBL", 1:2))
    expect_error(go_sets(org.Hs.eg.db, "SYMBOL", 1:2, 1:10))
    expect_error(go_sets(org.Hs.eg.db, "ENSEMBL", go))
    expect_error(go_sets())
})

## Need to change these tests to include es_map_unique and es_map_multiple ##
#test_that("'es_map()' works",
#{
#    es <- BiocSet(
#        set1 = c("BRCA1", "BRCA2", "TGFA", "ERCC2"),
#        set2 = c("PRNP", "FMR1", "PAX3")
#    )
#    es1 <- es %>% es_map(org.Hs.eg.db, "SYMBOL", "ENTREZID")
#
#    expect_s4_class(es1, "BiocSet")
#    expect_identical(dim(es_element(es1)), c(7L, 1L))
#    expect_identical(dim(es_set(es1)), c(2L, 1L))
#    expect_identical(dim(es_elementset(es1)), c(7L, 2L))
#    expect_true(is_tbl_elementset(es_elementset(es1)))
#
#    expect_true(all(es_element(es1)$element %in%
#        keys(org.Hs.eg.db, keytype="ENTREZID")))
#    
#    expect_error(es_map(es, org.Hs.eg.db))
#    expect_error(es_map(es, org.Hs.eg.db, "SYMBOL"))
#    expect_error(es_map(es, org.Hs.eg.db, SYMBOL, "ENTREZID"))
#    expect_error(es_map(es, org.Hs.eg.db, "SYMBOL", 10))
#    expect_error(es_map(es, org.Hs.eg.db, "SYMBOL", "IDS"))
#    expect_error(es_map(es, species, "SYMBOL", "ENTREZID"))
#})

test_that("'kegg_sets()' works",
{
    es <- kegg_sets("hsa")

    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_element(es)), c(7878L, 1L))
    expect_identical(dim(es_set(es)), c(326L, 1L))
    expect_identical(dim(es_elementset(es)), c(29145L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es)))

    expect_error(kegg_sets(hsa))
    expect_error(kegg_sets())
    expect_error(kegg_sets(1:2))
})

test_that("'map_add_element()' works",
{
    es <- BiocSet(set1 = c("PRKACA", "TGFA", "MAP2K1"), set2 = c("FOS", "BRCA1"))
    map <- map_add_element(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")

    expect_identical(length(map), 5L)
    expect_identical(class(map), "character")
    expect_identical(map, c("5566", "7039", "5604", "2353", "672"))

    expect_error(map_add_element(org.Hs.eg.db, "SYMBOL", "ENTREZID"))
})

test_that("'map_add_set()' works",
{
    library(GO.db)
    go <- go_sets(org.Hs.eg.db, "ENSEMBL")
    map <- map_add_set(go, GO.db, "GOID", "DEFINITION")

    expect_identical(length(map), 17495L)
    expect_identical(class(map), "character")

    expect_error(map_add_set(GO.db, "GOID", "DEFINITION"))
})
