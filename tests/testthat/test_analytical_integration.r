test_that('we can analytically extract the distribution',{
	# this was too slow
	st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rds")
	nonred_constr <- attr(st_res,"constraints_and_tasks")$nonredundant_constr
	vertices <- findVertices(nonred_constr)
	})