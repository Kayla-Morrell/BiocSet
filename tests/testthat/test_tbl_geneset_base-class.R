context("tbl_geneset_base-class")

test_that("'subclass_tbl_geneset_base()' works", {
    tbl <- c("tbl_geneset_base", "tbl_df", "tbl", "data.frame")
    
    gs1 <- tbl_geneset(set1 = letters, set2 = LETTERS)
    expect_s3_class(gs1, "tbl_geneset")
    expect_identical(class(gs1), c("tbl_geneset", tbl))
    expect_length(class(gs1), 5)

    gs2 <- tbl_gene(gs1)
    expect_s3_class(gs2, "tbl_gene")
    expect_identical(class(gs2), c("tbl_gene", tbl))
    expect_length(class(gs2), 5)
    
    gs3 <- tbl_set(gs1)
    expect_s3_class(gs3, "tbl_set")
    expect_identical(class(gs3), c("tbl_set", tbl))
    expect_length(class(gs3), 5)
})

test_that("'filter.tbl_geneset_base()' works", {
    gs <- tbl_geneset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_geneset_base", "tbl_df", "tbl", "data.frame")
    
    gs1 <- gs %>% filter(gene == "a")
    expect_s3_class(gs1, "tbl_geneset")
    expect_identical(class(gs1), c("tbl_geneset", tbl))
    expect_length(class(gs1), 5)
    

    gs2 <- tbl_gene(gs) %>% filter(gene == "a" | gene == "A")
    expect_s3_class(gs2, "tbl_gene")
    expect_identical(class(gs2), c("tbl_gene", tbl))
    expect_length(class(gs2), 5)

    gs3 <- tbl_set(gs) %>% filter(set == "set1")
    expect_s3_class(gs3, "tbl_set")
    expect_identical(class(gs3), c("tbl_set", tbl))
    expect_length(class(gs3), 5)

    gs_not <- gs %>% filter(gene == "1")
    expect_identical(dim(gs_not), c(0L, 2L))
})

test_that("'select.tbl_geneset_base()' works", {
    gs <- tbl_geneset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_geneset_base", "tbl_df", "tbl", "data.frame")

    gs1 <- gs %>% select(gene)
    expect_s3_class(gs1, "tbl_geneset")
    expect_identical(class(gs1), c("tbl_geneset", tbl))
    expect_length(class(gs1), 5)
    expect_error(
        gs %>% select(Genes),
        "object 'Genes' not found"
    )

    gs2 <- tbl_gene(gs) %>% select(gene)
    expect_s3_class(gs2, "tbl_gene")
    expect_identical(class(gs2), c("tbl_gene", tbl))
    expect_length(class(gs2), 5)
    expect_error(
        tbl_gene(gs) %>% select(set),
        "object 'set' not found"
    )

    gs3 <- tbl_set(gs) %>% select(set)
    expect_s3_class(gs3, "tbl_set")
    expect_identical(class(gs3), c("tbl_set", tbl))
    expect_length(class(gs3), 5)
    expect_error(
        tbl_set(gs) %>% select(gene),
        "object 'gene' not found"
    )
})

test_that("'mutate.tbl_geneset_base()' works", {
    gs <- tbl_geneset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_geneset_base", "tbl_df", "tbl", "data.frame")

    gs1 <- gs %>% mutate(pval = rnorm(1:52))
    expect_s3_class(gs1, "tbl_geneset")
    expect_identical(class(gs1), c("tbl_geneset", tbl))
    expect_length(class(gs1), 5)
    expect_error(
        gs %>% mutate(pval = rnorm(1:10)),
        "Column `pval` must be length 52 \\(the number of rows\\) or one, not 10"
    )

    gs2 <- tbl_gene(gs) %>% mutate(pval = rnorm(1:52))
    expect_s3_class(gs2, "tbl_gene")
    expect_identical(class(gs2), c("tbl_gene", tbl))
    expect_length(class(gs2), 5)
    expect_error(
        tbl_gene(gs) %>% mutate(pval = rnorm(1:2)),
        "Column `pval` must be length 52 \\(the number of rows\\) or one, not 2"
    )

    gs3 <- tbl_set(gs) %>% mutate(pval = rnorm(1:2))
    expect_s3_class(gs3, "tbl_set")
    expect_identical(class(gs3), c("tbl_set", tbl))
    expect_length(class(gs3), 5)
    expect_error(
        tbl_set(gs) %>% mutate(pval = rnorm(1:23)),
        "Column `pval` must be length 2 \\(the number of rows\\) or one, not 23"
    )
})

test_that("'group_by.tbl_geneset_base()' works", {
    gs <- tbl_geneset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_geneset_base", "grouped_df", "tbl_df", "tbl", "data.frame")

    gs1 <- gs %>% group_by(gene)
    expect_s3_class(gs1, "tbl_geneset")
    expect_identical(class(gs1), c("tbl_geneset", tbl))
    expect_length(class(gs1), 6)
    expect_error(
        gs %>% group_by(Genes),
        "Column `Genes` is unknown"
    )

    gs2 <- tbl_gene(gs) %>% group_by(gene)
    expect_s3_class(gs2, "tbl_gene")
    expect_identical(class(gs2), c("tbl_gene", tbl))
    expect_length(class(gs2), 6)
    expect_error(
        tbl_gene(gs) %>% group_by(set),
        "Column `set` is unknown"
    )

    gs3 <- tbl_set(gs) %>% group_by(set)
    expect_s3_class(gs3, "tbl_set")
    expect_identical(class(gs3), c("tbl_set", tbl))
    expect_length(class(gs3), 6)
    expect_error(
        tbl_set(gs) %>% group_by(gene),
        "Column `gene` is unknown"
    )
})
