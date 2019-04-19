context("tblelement-class")

test_that("'tbl_element()' works", {
    es <- tbl_elementset(set1 = letters, set2 = LETTERS)

    es1 <- tbl_element(es)
    expect_s3_class(es1, "tbl_element")
    expect_identical(class(es1), c("tbl_element",
                                   "tbl_elementset_base",
                                   "tbl_df",
                                   "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(es1$element, es$element)
    expect_identical(dim(es1), c(52L,1L))
    expect_false(is_tbl_elementset(es1))
})
