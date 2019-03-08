context("GeneSet")

test_that("'GeneSet()' works",
{
    gs <- GeneSet(a = letters, b = LETTERS)
    expect_s4_class(gs, "GeneSet")
    expect_identical(dim(gs_gene(gs)), c(52L,1L))
    expect_identical(dim(gs_set(gs)), c(2L,1L))
    expect_identical(dim(gs_geneset(gs)), c(52L,2L))
    expect_true(is_tbl_geneset(gs_geneset(gs)))
    expect_identical(.active(gs), "geneset")
    expect_length(.active(gs), 1L)

    gs <- GeneSet()
    expect_s4_class(gs, "GeneSet")
    expect_identical(dim(gs_geneset(gs)), c(0L,2L))
    expect_true(is_tbl_geneset(gs_geneset(gs)))

    gs <- GeneSet(set1 = character(), set2 = LETTERS)
    expect_s4_class(gs, "GeneSet")
    expect_identical(dim(gs_geneset(gs)), c(26L,2L))
    expect_true(is_tbl_geneset(gs_geneset(gs)))
    expect_identical(levels(gs_geneset(gs)$set), c("set1", "set2"))

    expect_error(GeneSet(set1 = 1:10, set2 = LETTERS))
    expect_error(GeneSet(set1 = 1:10))
    expect_error(GeneSet(LETTERS))
    expect_error(GeneSet(set1 = letters, LETTERS))
    expect_error(GeneSet(set1 = letters, set2 = 1:10))
})

test_that("'.gene()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% gs_gene()
    expect_s3_class(gs1, "tbl_gene")
    expect_identical(class(gs1), c("tbl_gene", "tbl_geneset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(dim(gs1), c(52L,1L))
})

test_that("'.set()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% gs_set()
    expect_s3_class(gs1, "tbl_set")
    expect_identical(class(gs1), c("tbl_set", "tbl_geneset_base",
                                  "tbl_df", "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(dim(gs1), c(2L,1L))
})

test_that("'.geneset()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% gs_geneset()
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

test_that("'gs_activate.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    expect_identical(.active(gs), "geneset")

    gs1 <- gs %>% gs_activate(gene)
    expect_identical(.active(gs1), "gene")

    gs2 <- gs %>% gs_activate(set)
    expect_identical(.active(gs2), "set")

})

test_that("'filter.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)
    expect_equivalent(filter(gs), gs)

    gs1 <- gs %>% filter(set == "set1")
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(dim(gs_set(gs1)), c(1L,1L))
    expect_identical(dim(gs_geneset(gs1)), c(26L,2L))
    expect_identical(gs_geneset(gs1)$gene, letters)

    foo <- "k"
    gs2 <- gs %>% filter(gene == foo)
    expect_true(is_tbl_geneset(gs_geneset(gs2)))
    expect_identical(dim(gs_gene(gs2)), c(1L,1L))
    expect_identical(gs_geneset(gs2)$gene, foo)
})

test_that("'select.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% select(c(gene, set))
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(dim(gs_geneset(gs1)), c(52L,2L))
    expect_identical(gs1, gs)

    gs2 <- gs %>% select(gene)
    expect_true(is_tbl_geneset(gs_geneset(gs2)))
    expect_identical(dim(gs_geneset(gs2)), c(52L,2L))
    expect_identical(gs_gene(gs2), gs_gene(gs))

    gs3 <- gs %>% select(set)
    expect_true(is_tbl_geneset(gs_geneset(gs3)))
    expect_identical(dim(gs_geneset(gs3)), c(52L, 2L))
    expect_identical(gs_set(gs3), gs_set(gs))
})

test_that("'mutate.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% mutate(pval = rnorm(1:52))
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(dim(gs_geneset(gs1)), c(52L,3L))

    expect_error(gs %>% mutate(z = 1:2))
})

test_that("'map_gene.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% map_gene(letters, LETTERS)
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(dim(gs_geneset(gs1)), c(52L,2L))
    expect_identical(gs_geneset(gs1)$gene, gs_gene(gs1)$gene)

    expect_error(gs %>% mutate(gene = 1:52))
})

test_that("'map_set.GeneSet()' works", {
    gs <- GeneSet(a = letters, B = LETTERS)

    gs1 <- gs %>% map_set("a", "A")
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(dim(gs_geneset(gs1)), c(52L,2L))
    expect_identical(levels(gs_geneset(gs1)$set), levels(gs_set(gs1)$set))

    expect_error(gs %>% mutate(set = 1:52))
})

test_that("'group_by.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% group_by(gene, set)
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(dim(gs_geneset(gs1)), c(52L,2L))

    gs2 <- gs %>% group_by(gene)
    expect_true(is_tbl_geneset(gs_geneset(gs2)))
    expect_identical(dim(gs_geneset(gs2)), c(52L,2L))

    gs3 <- gs %>% group_by(set)
    expect_true(is_tbl_geneset(gs_geneset(gs3)))
    expect_identical(dim(gs_geneset(gs3)), c(52L,2L))

    expect_error(gs %>% group_by(Genes))
})

test_that("'ungroup.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% group_by(gene) %>% ungroup
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_identical(gs1, gs)
    expect_identical(dim(gs_geneset(gs1)), c(52L,2L))
})

test_that("'summarise.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% summarise(n = n())
    expect_false(is_tbl_geneset(gs1))
    expect_identical(dim(gs1), c(1L,1L))
    expect_equal(gs1$n, 52L)

    gs2 <- gs %>% group_by(gene) %>% summarise(n = n())
    expect_false(is_tbl_geneset(gs2))
    expect_identical(dim(gs2), c(52L,2L))
    expect_equal(gs2$n, c(rep(1L, 52)))

    gs3 <- gs %>% group_by(set) %>% summarise(n = n())
    expect_false(is_tbl_geneset(gs3))
    expect_identical(dim(gs3), c(2L,2L))
    expect_equal(gs3$n, c(26L, 26L))
})

test_that("'arrange.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% arrange()
    expect_true(is_tbl_geneset(gs_geneset(gs1)))
    expect_equal(gs1, gs)

    gs2 <- gs %>% arrange(gene)
    expect_true(is_tbl_geneset(gs_geneset(gs2)))
    expect_equal(gs2, gs)
    expect_equal(gs_geneset(gs2)$gene, sort(gs_geneset(gs)$gene))

    gs3 <- gs %>% arrange(set)
    expect_true(is_tbl_geneset(gs_geneset(gs3)))
    expect_equal(gs3, gs)
    expect_equal(gs_geneset(gs3)$set, sort(gs_geneset(gs)$set))

    gs4 <- gs %>% arrange(gene == "k")
    expect_true(is_tbl_geneset(gs_geneset(gs4)))
    expect_identical(dim(gs_geneset(gs4)), c(52L,2L))

    gs5 <- gs %>% arrange(set == "set1")
    expect_true(is_tbl_geneset(gs_geneset(gs5)))
    expect_identical(dim(gs_geneset(gs5)), c(52L,2L))
})

test_that("'group_vars.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% group_by(gene) %>% group_vars()
    expect_identical(gs1, "gene")
    expect_length(gs1, 1L)
    expect_identical(class(gs1), "character")

    gs2 <- gs %>% group_by(gene, set) %>% group_vars()
    expect_identical(gs2, c("gene", "set"))
    expect_length(gs2, 2L)
    expect_identical(class(gs2), "character")
})

test_that("'tbl_vars.GeneSet()' works", {
    gs <- GeneSet(set1 = letters, set2 = LETTERS)

    gs1 <- gs %>% tbl_vars()
    expect_identical(gs1, c("gene", "set"))
    expect_length(gs1, 2L)
    expect_identical(class(gs1), "character")

    gs2 <- gs %>% gs_activate(gene) %>% tbl_vars()
    expect_identical(gs2, "gene")
    expect_length(gs2, 1L)
    expect_identical(class(gs2), "character")
})
