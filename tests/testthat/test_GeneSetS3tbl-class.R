context("GeneSetS3tbl-class")

test_that("'GeneSetS3tbl()' works", {
    gs <- GeneSetS3tbl(a = letters, B = LETTERS)
    expect_s3_class(gs, c("GeneSetS3tbl","tbl_df","tbl","data.frame"))
    expect_s3_class(GeneSetS3tbl(),
        c("GeneSetS3tbl","tbl_df","tbl","data.frame"))

    expect_error(GeneSetS3tbl(B = letters, A = 1:5))
    expect_error(GeneSetS3tbl(A = 1:5))
    expect_error(GeneSetS3tbl(letters))     # unnamed
    expect_error(GeneSetS3tbl(a=letters, LETTERS))
})
