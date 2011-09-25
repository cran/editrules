### R code from vignette source 'editrules-linear.Snw'

###################################################
### code chunk number 1: editrules-linear.Snw:70-71
###################################################
library(editrules)


###################################################
### code chunk number 2: editrules-linear.Snw:200-211
###################################################
E <- editmatrix(c(
"t  == ct + p" ,
"ct == ch + cp",
"p  <= 0.6*t",
"ct <= 0.3*t",
"cp <= 0.3*t",
"t  >  0",
"ch >  0",
"cp >  0",
"ct >  0"), normalize=TRUE)
E


###################################################
### code chunk number 3: editrules-linear.Snw:232-235
###################################################
data(edits)
edits
editmatrix(edits)


###################################################
### code chunk number 4: editrules-linear.Snw:308-317
###################################################
# define two records in a data.frame
dat <- data.frame(
  t = c(1000, 1200),
 ct = c(400,  200),
 ch = c(100,  350),
 cp = c(200,  575),
 p =  c(500,  652 ))
# check for violated edits
violatedEdits(E,dat)


###################################################
### code chunk number 5: editrules-linear.Snw:450-451
###################################################
substValue(E,"t",10)


###################################################
### code chunk number 6: editrules-linear.Snw:496-501
###################################################
(E2 <- editmatrix(c("2*x1 + x2 -x3 == 8",
                  "2*x3 + 11 == 3*x1 + x2", 
                  "x2 + 2*x3 + 3 == 2*x1")
                ))
echelon(E2)


###################################################
### code chunk number 7: editrules-linear.Snw:643-647
###################################################
eliminate(E,"t")
F <- E
for ( var in c("t","cp","p") ) F <- eliminate(F,var)
F


###################################################
### code chunk number 8: editrules-linear.Snw:959-969
###################################################
E1 = editmatrix(c(
    "y >  x - 1",
    "y > -x + 3",
    "y <  x + 1",
    "y < -x + 5"))
bt <- errorLocalizer(E1, c(x=2,y=-1))
bt$searchNext()
bt$duration
bt$maxdurationExceeded
bt$searchNext()


###################################################
### code chunk number 9: editrules-linear.Snw:1007-1010
###################################################
E <- editmatrix(c("p + c == t"))
r <- c(p=755, c=125, t=200)
bt <- errorLocalizer(E, r)


###################################################
### code chunk number 10: editrules-linear.Snw:1014-1017
###################################################
bt$searchNext()$adapt
bt$searchNext()$adapt
bt$searchNext()$adapt


###################################################
### code chunk number 11: editrules-linear.Snw:1022-1026
###################################################
bt <- errorLocalizer(E, r, weight=c(1,1,2))
bt$searchNext()$adapt
bt$searchNext()$adapt
bt$searchNext()$adapt


###################################################
### code chunk number 12: editrules-linear.Snw:1036-1043
###################################################
E <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0"))
bt <- errorLocalizer(E, r)
bt$searchNext()$adapt
bt$searchNext()$adapt
bt$searchNext()$adapt


###################################################
### code chunk number 13: editrules-linear.Snw:1052-1065
###################################################
# An example with missing data.
E <- editmatrix(c(
    "p + c1 + c2 == t",
    "c1 - 0.3*t >= 0",
    "p > 0",
    "c1 > 0",
    "c2 > 0",
    "t > 0"))
x <- c(p=755, c1=50, c2=NA,t=200)
bt <- errorLocalizer(E,x)
bt$searchNext()$adapt
bt$searchNext()$adapt
(s <- bt$searchNext()$adapt)


###################################################
### code chunk number 14: editrules-linear.Snw:1070-1071
###################################################
substValue(E,names(x)[!s],x[!s])


###################################################
### code chunk number 15: editrules-linear.Snw:1265-1298
###################################################
bt <- backtracker(
    isSolution = {  # check for solution or pruning
        w <- sum(weight[adapt])
        if ( isObviouslyInfeasible(.E) || w > wsol ) return(FALSE)
        if (length(.totreat) == 0){
            wsol <<- w
            adapt <- adapt 
            return(TRUE)
        }
    },
    choiceLeft = {  # things to do in the left node
        .var <- .totreat[1]
        .E <- substValue(.E, .var , r[.var])
        .totreat <- .totreat[-1]
                              
        adapt[.var] <- FALSE
    },
    choiceRight = { # things to do in the right node
        .var <- .totreat[1]
        .E <- eliminate(.E, .var)
        .totreat <- .totreat[-1]
                              
        adapt[.var] <- TRUE
    },
    # Initialize variables in root node
    .E = editmatrix(c("y > x-1 ","y > -x+3")),
    .totreat = c("x","y"),
    r = c(x=2,y=-1),
    adapt = c(x=FALSE, y=FALSE),
    weight = c(1,1),        
    wsol = 2                
)
bt$searchNext()


