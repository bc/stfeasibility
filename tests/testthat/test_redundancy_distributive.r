context('redundancy distributive property')
# see thread at https://math.stackexchange.com/questions/2839859/does-redundancy-removal-in-linear-programming-follow-a-distributive-property
test_that("redundancy does not follow distributive property",{
	constr_a <- rbind(
		c(-1,-0.25),
		c(-1,-4),
		c(1,0)) %>% set_colnames(c("x1", "x2"))
	rhs_a <- c(-1,-4,-0.7)
	a <- list(constr=constr_a, dir=rep("<=",3), rhs=rhs_a)

	constr_b <- rbind(
		c(-1,-0.5),
		c(-1,0))%>% set_colnames(c("x1", "x2"))
	rhs_b <- c(-1.2,0)
	b <- list(constr=constr_b, dir=rep("<=",2), rhs=rhs_b)

	print_constraint(a)
	print_constraint(b)
	res_a <- a %>% eliminate_redundant
	res_b <- b %>% eliminate_redundant
	ab <- merge_constraints(a,b)

	lp("min",res_a$constr,res_a$dir,res_a$rhs, objective.in=c(1,1))
	lp("min",res_b$constr,res_b$dir,res_b$rhs, objective.in=c(1,1))
	lp("min",ab$constr,ab$dir,ab$rhs, objective.in=c(1,1))
	

	print_constraint(res_a)
	print_constraint(res_b)

	a$constr %*% c(.8,.8)
	b$constr %*% c(.8,.8)
	ab$constr %*% c(.8,.8)
	expect_equal(a$constr %*% c(.8,.8)%>% as.numeric, a$rhs)
	expect_equal(b$constr %*% c(.8,.8)%>% as.numeric, b$rhs)

})