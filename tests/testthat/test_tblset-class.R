context("tblset-class")

test_that("'tbl_set()' works", {
    es <- tbl_elementset(set1 = letters, set2 = LETTERS)

    es1 <- tbl_set(es)
    expect_s3_class(es1, "tbl_set")
    expect_identical(class(es1), c("tbl_set", "tbl_elementset_base", "tbl_df",
                                   "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(2L,1L))
    expect_identical(levels(es1$set), levels(es$set))
    expect_false(is_tbl_elementset(es1))
})
