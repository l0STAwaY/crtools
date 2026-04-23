test_that("get_ct_family returns correct ct family when fitting using normal model", {
  
  library(pscl)
  

  set.seed(123)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    y = rpois(n, lambda = 2)
  )
  
# pois
m_pois <- glm(y ~ x, data = df, family = poisson())
testthat::expect_equal(get_ct_family(m_pois), "poisson")

# quasai
m_qpois <- glm(y ~ x, data = df, family = quasipoisson())
testthat::expect_equal(get_ct_family(m_qpois), "qpoisson")

# negbinomial
m_nb <- MASS::glm.nb(y ~ x, data = df)
testthat::expect_equal(get_ct_family(m_nb), "negbin")

# z poisson
m_zip <- pscl::zeroinfl(y ~ x, data = df, dist = "poisson")
testthat::expect_equal(get_ct_family(m_zip), "zip")

# z negbi
m_zinb <- pscl::zeroinfl(y ~ x, data = df, dist = "negbin")
testthat::expect_equal(get_ct_family(m_zinb), "zinb")
  
})

testthat::test_that("get_ct_family returns correct ct family when fitting using fit_ct", {
  
library(pscl)
library(MASS)


set.seed(123)
df <- data.frame(
  x = rnorm(100),
  y = rpois(100, lambda = 2)

# pois
m_pois <- fit_ct(y ~ x, data = df, family = "poisson")
testthat::expect_equal(get_ct_family(m_pois), "poisson")

# quasai
m_qpois <- fit_ct(y ~ x, data = df, family = "qpoisson")
testthat::expect_equal(get_ct_family(m_qpois), "qpoisson")

# negbinomial
m_nb <- fit_ct(y ~ x, data = df, family = "negbin")
testthat::expect_equal(get_ct_family(m_nb), "negbin")

# z poisson
m_zip <- fit_ct(y ~ x, data = df, family = "zip")
testthat::expect_equal(get_ct_family(m_zip), "zip")

# z negbi
m_zinb <- fit_ct(y ~ x, data = df, family = "zinb")
testthat::expect_equal(get_ct_family(m_zinb), "zinb")
  
})