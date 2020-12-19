testthat::context("remote covrpage")

testthat::skip_on_ci()
testthat::skip_on_cran()

testthat::describe("run covrpage snapshot", {

  it('run on covrwarn repo',{
    testthat::expect_true(covrpage_snapshot('yonicd/covrwarn',preview = FALSE))
  })
  
})

testthat::describe("run map_testthat snapshot", {

  it('run on covrwarn repo',{
    testthat::expect_s3_class(map_testthat_snapshot('yonicd/covrwarn'),'data.frame')
  })
  
})
