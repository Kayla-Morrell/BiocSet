context("url_ref")

test_that("'url_ref_element()' works",
{
    es <- BiocSet(set1 = c("TP53", "TNF", "EGFR"), set2 = c("ENSG00000136244", 
        "ENSG00000112715"), set3 = c("291", "1890", "4205"))
    es1 <- url_ref_element(es)

    expect_s4_class(es1, "BiocSet")
    expect_identical(dim(es_element(es1)), c(8L, 2L))
    expect_identical(dim(es_set(es1)), c(3L, 1L))
    expect_identical(dim(es_elementset(es1)), c(8L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es1)))

    expect_error(url_ref_element(1))
    expect_error(url_ref_element())
})

test_that("'url_ref_set()' works",
{
    es <- BiocSet("GO:0000002" = c("abc", "ABC"), "GO:0000010" = c("def", "ghi",
        "DEF", "GHI"), "hsa00010" = c("jkl", "JKL", "mno", "MNO"), "hsa00020" =
        c("pqr", "PQR"))
    es1 <- url_ref_set(es)

    expect_s4_class(es1, "BiocSet")
    expect_identical(dim(es_element(es1)), c(12L, 1L))
    expect_identical(dim(es_set(es1)), c(4L, 2L))
    expect_identical(dim(es_elementset(es1)), c(12L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es1)))

    expect_error(url_ref_set(3))
    expect_error(url_ref_set())
})

test_that("'url_ref()' works",
{
    es <- BiocSet("GO:0000002" = c("abc", "ABC"), "GO:0000010" = c("def", "ghi",
        "DEF", "GHI"), "hsa00010" = c("jkl", "JKL", "mno", "MNO"), "hsa00020" = 
        c("pqr", "PQR"))
    es1 <- url_ref(es)

    expect_s4_class(es1, "BiocSet")
    expect_identical(dim(es_element(es1)), c(12L, 2L))
    expect_identical(dim(es_set(es1)), c(4L, 2L))
    expect_identical(dim(es_elementset(es1)), c(12L, 2L))
    expect_true(is_tbl_elementset(es_elementset(es1)))

    expect_error(url_ref(3))
    expect_error(url_ref())
    expect_error(url_ref("abc"))
})
