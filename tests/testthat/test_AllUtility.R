context("AllUtility")

test_that("'.element()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% es_element()
    expect_s3_class(es1, "tbl_element")
    expect_identical(class(es1), c("tbl_element", "tbl_elementset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(52L,1L))
})

test_that("'.set()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% es_set()
    expect_s3_class(es1, "tbl_set")
    expect_identical(class(es1), c("tbl_set", "tbl_elementset_base",
                                  "tbl_df", "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(2L,1L))
})

test_that("'.elementset()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% es_elementset()
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", "tbl_elementset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(52L,2L))
})

test_that("'.active()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% .active()
    expect_is(es1, "character")
    expect_length(es1, 1)
})
