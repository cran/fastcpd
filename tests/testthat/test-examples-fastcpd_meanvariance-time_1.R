testthat::test_that(
  "examples/fastcpd_meanvariance-time_1.txt", {
    testthat::skip_on_cran()

    examples_time <- readLines("examples/fastcpd_meanvariance-time_1.txt")
    source(textConnection(paste(
      examples_time[seq_len(length(examples_time) - 2) + 1],
      collapse = "\n"
    )))

    testthat::expect_equal(result@cp_set, c(1985, 4000))
    testthat::expect_lt(result_time[3], 60)
  }
)
