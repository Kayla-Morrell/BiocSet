context("GeneSet")

test_that("'GeneSet()' works",
{
    gs <- GeneSet(a = letters, b = LETTERS)
    expect_s4_class(gs, "GeneSet")
    expect_identical(dim(gs@gene), c(52L,1L))
    expect_identical(dim(gs@set), c(2L,1L))
    expect_identical(dim(gs@geneset), c(52L,2L))
    expect_true(is_tbl_geneset(gs@geneset))
    expect_identical(gs@active, "geneset")
    expect_identical(length(gs@active), 1L)

    gs <- GeneSet()
    expect_s4_class(gs, "GeneSet")
    expect_identical(dim(gs@geneset), c(0L,2L))
    expect_true(is_tbl_geneset(gs@geneset))

    gs <- GeneSet(set1 = character(), set2 = LETTERS)
    expect_s4_class(gs, "GeneSet")
    expect_identical(dim(gs@geneset), c(26L,2L))
    expect_true(is_tbl_geneset(gs@geneset))
    expect_identical(levels(gs@geneset$set), c("set1", "set2"))

    expect_error(GeneSet(set1 = 1:10, set2 = LETTERS))
    expect_error(GeneSet(set1 = 1:10))
    expect_error(GeneSet(LETTERS))
    expect_error(GeneSet(set1 = letters, LETTERS))
    expect_error(GeneSet(set1 = letters, set2 = 1:10))
})

test_that("'.gene()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% .gene()
    expect_s3_class(gs1, "tbl_gene")
    expect_identical(class(gs1), c("tbl_gene", "tbl_geneset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(dim(gs1), c(52L,1L))
})

test_that("'.set()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% .set()
    expect_s3_class(gs1, "tbl_set")
    expect_identical(class(gs1), c("tbl_set", "tbl_geneset_base",
                                  "tbl_df", "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(dim(gs1), c(2L,1L))
})

test_that("'.geneset()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% .geneset()
    expect_s3_class(gs1, "tbl_geneset")
    expect_identical(class(gs1), c("tbl_geneset", "tbl_geneset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(dim(gs1), c(52L,2L))
})

test_that("'.active()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% .active()
    expect_is(gs1, "character")
    expect_length(gs1, 1)
})


#test_that("'`.active()`' works", {
#
#})

#test_that("'.active_value()' works", {
#
#})

#test_that("'gs_activate()' works", {
#
#})

#test_that("'gs_activate.GeneSet()' works", {
#
#})

test_that("'filter.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)
    expect_equivalent(filter(gs), gs)

    gs1 <- filter(gs, set == "set1")
    expect_true(is_tbl_geneset(.geneset(gs1)))
    expect_identical(dim(.set(gs1)), c(1L,1L))
    expect_identical(dim(.geneset(gs1)), c(26L,2L))
    expect_identical(.geneset(gs1)$gene, letters)

    foo <- "k"
    gs2 <- filter(gs, gene == foo)
    expect_true(is_tbl_geneset(.geneset(gs2)))
    expect_identical(dim(.gene(gs2)), c(1L,1L))
    expect_identical(.geneset(gs2)$gene, foo)
})

test_that("'select.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- select(gs, c(gene, set))

})

#test_that("'mutate.GeneSet()' works", {
#
#})

#test_that("'group_by.GeneSet()' works", {
#         
#})

#test_that("'ungroup.GeneSet()' works", {
#
#})

#test_that("'summarise.GeneSet()' works", {
#
#})

#test_that("'arrange.GeneSet()' works", {
#
#})

#test_that("'group_vars.GeneSet()' works", {
#
#})

#test_that("'tbl_vars.GeneSet()' works", {
#
#})
