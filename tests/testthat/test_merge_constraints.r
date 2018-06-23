context('Functions for merging constraints')

test_that("two constraints can be combined without affecting one another's individual outputs", {
	# fx_constraint
	first_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
	# fy_constraint
	second_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(0,1,0,0), bounds_tuple_of_numeric) 	
	merged_constraint <- diagonal_merge_constraints(first_constraint, second_constraint, "2")
	#via https://www.r-bloggers.com/creating-an-image-of-a-matrix-in-r-using-image/
	rotate <- function(x) t(apply(x, 2, rev))
	mat <- merged_constraint$constr%>%as.matrix%>%melt

	library(reshape2)
	library(ggplot2)
	longData<-mat[mat$value!=0,]
	longData$Var1 <- factor(longData$Var1, levels=rev(levels(longData$Var1)))
	# colnames(longData) <- c("x_timepoint","constraint")
	# longData$Var2 <- factor(longData$Var2, levels=rev(levels(longData$Var2)))
	p<- ggplot(longData, aes(x = Var2, y = Var1)) + 
 	 geom_raster(aes(fill=value)) + 
 	 scale_fill_gradient(low="white", high="red", na.value="darkgrey", trans = "sqrt") +
 	 labs(x="input variable per timestep", y="constraint",title="Matrix") +
 	 theme_classic() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))



	# image(mat%>%rotate%>%sqrt, col=brewer.pal(12, "PRGn"))
 	# p + 
show(p)
	browser()
	# expect_equal(minimizing on left constraint result, minimizing on combined constraint[left part])
})


append_with_underscore <- function(s,appendix) paste0(s,"_",appendix)