context("mapping_set")

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

test_that("'kegg_sets()' works",
{
    es <- kegg_sets("hsa")

    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_element(es)), c(7896L, 1L))
    expect_identical(dim(es_set(es)), c(326L, 1L))
    expect_identical(dim(es_elementset(es)), c(29296L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es)))

    expect_error(kegg_sets(hsa))
    expect_error(kegg_sets())
    expect_error(kegg_sets(1:2))
})

test_that("'map_set.BiocSet()' works", {
    es <- BiocSet(a = letters, B = LETTERS)

    es1 <- es %>% map_set("a", "A")
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L,2L))
    expect_identical(levels(es_elementset(es1)$set), levels(es_set(es1)$set))

    expect_error(es %>% map_set())
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
