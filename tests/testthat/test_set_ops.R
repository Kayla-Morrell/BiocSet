context("set_ops")

test_that("'union.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    expect_s4_class(es1, "BiocSet")
    expect_s4_class(es2, "BiocSet")

    tbl <- union(es1, es2)
    expect_s3_class(tbl, "tbl_elementset")
    expect_identical(class(tbl), c("tbl_elementset", "tbl_elementset_base",
                                    "tbl_df", "tbl", "data.frame"))
    expect_identical(dim(tbl), c(12L, 2L))
    expect_true(is_tbl_elementset(tbl))

    tbl2 <- union(es_activate(es1, "element"), es_activate(es2, "element"))
    expect_s3_class(tbl2, "tbl_element")
    expect_identical(class(tbl2), c("tbl_element", "tbl_elementset_base", 
                                     "tbl_df", "tbl", "data.frame"))
    expect_identical(dim(tbl2), c(12L, 1L))
    expect_false(is_tbl_elementset(tbl2))

    tbl3 <- union(es_activate(es1, "set"), es_activate(es2, "set"))
    expect_s3_class(tbl3, "tbl_set")
    expect_identical(class(tbl3), c("tbl_set", "tbl_elementset_base",
                                     "tbl_df", "tbl", "data.frame"))
    expect_identical(dim(tbl3), c(2L, 1L))
    expect_false(is_tbl_elementset(tbl3))
})

test_that("'intersect.BiocSet()' works", {
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    expect_s4_class(es1, "BiocSet")
    expect_s4_class(es2, "BiocSet")

    tbl <- intersect(es1, es2)
    expect_s3_class(tbl, "tbl_elementset")
    expect_identical(class(tbl), c("tbl_elementset", "tbl_elementset_base",
                                    "tbl_df", "tbl", "data.frame"))
    expect_identical(dim(tbl), c(4L, 2L))
    expect_true(is_tbl_elementset(tbl))

    tbl2 <- intersect(es_activate(es1, "element"), es_activate(es2, "element"))
    expect_s3_class(tbl2, "tbl_element")
    expect_identical(class(tbl2), c("tbl_element", "tbl_elementset_base",
                                     "tbl_df", "tbl", "data.frame"))
    expect_identical(dim(tbl2), c(4L, 1L))
    expect_false(is_tbl_elementset(tbl2))

    tbl3 <- intersect(es_activate(es1, "set"), es_activate(es2, "set"))
    expect_s3_class(tbl3, "tbl_set")
    expect_identical(class(tbl3), c("tbl_set", "tbl_elementset_base",
                                     "tbl_df", "tbl", "data.frame"))
    expect_identical(dim(tbl3), c(2L, 1L))
    expect_false(is_tbl_elementset(tbl3))
})
