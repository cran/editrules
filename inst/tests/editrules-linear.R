
context("violatedEdits")

test_that("Numerical edit checks",{
    is_equivalent_to( 
        violatedEdits(
            editmatrix(c( "x+3*y==2*z", "x==z")),
            data.frame( 
                x = c(0,2,1),
                y = c(0,0,1),
                z = c(0,1,1)
            )),
            matrix(c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE),nrow=3)
    )
})

test_that("Categorical edit checks work",{

is_equivalent_to(
    violatedEdits(
            editarray(c(
                "gender %in% c('male','female')",
                "pregnant %in% c(TRUE, FALSE)",
                "if( gender == 'male' ) !pregnant"
            )),
            data.frame(
                gender=c('male','male','female'), 
                pregnant=c(TRUE,FALSE,TRUE)
            )),
        c(TRUE,FALSE,TRUE)   
    )
})



