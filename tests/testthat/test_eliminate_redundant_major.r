test_that("major performance", {
    structures <- pblapply(3:4, function(i){
        print("Just starting %s" %--% i)
        return(create_nonredundant_structures(i))
    })
    use_data(structures)
})
