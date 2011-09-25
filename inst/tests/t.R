
for ( d in dir("../../R",full.names=TRUE)) dmp <- source(d)
    v <- expression(
        A %in% letters[1:2],
        B %in% letters[3:4],
        if ( A == 'a' ) x > 0
    )
    E <- editset(v)
getArr(E$mixcat)
array(c(F,T,T,F,T,T),dim=c(1,6))











