context('shard generation')
test_that("shard_a_total", {
	expect_equal(shard_a_total(8000,8), c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000))
	expect_equal(shard_a_total(799098,8), c(99889, 99887, 99887, 99887, 99887, 99887, 99887, 99887))
	expect_error(shard_a_total(1,8))
	expect_equal(shard_a_total(8,8), rep(1,8))
})