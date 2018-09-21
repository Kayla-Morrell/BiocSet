context("GeneSetS3-class")

test_that("'GeneSetS3()' works", {
    gs <- GeneSetS3(a = letters, B = LETTERS)
    expect_s3_class(gs, "GeneSetS3")
    expect_s3_class(GeneSetS3(), "GeneSetS3")

    expect_error(GeneSetS3(B = letters, A = 1:5))
    expect_error(GeneSetS3(A = 1:5))
    expect_error(GeneSetS3(letters))     # unnamed
    expect_error(GeneSetS3(a=letters, LETTERS))
})