test_that('we can analytically extract the distribution',{
	st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda")
	nonred_constr <- attr(st_res,"constraints_and_tasks")$nonredundant_constr
	vertices <- findVertices(nonred_constr)
	})