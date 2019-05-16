context("url_ref")

test_that("'url_ref()' works",
{
    foo <- url_ref("GO:0000002", browser = "false")
    expect_null(foo)

    foo2 <- url_ref("ENSG00000025708", browser = "false")
    expect_null(foo2)

    foo3 <- url_ref("5566", browser = "false")
    expect_null(foo3)

    foo4 <- url_ref("map05310", browser = "false")
    expect_null(foo4)

    foo5 <- url_ref("BRCA1", browser = "false")
    expect_null(foo5)

    expect_error(url_ref())
    expect_error(url_ref(GO))
})
