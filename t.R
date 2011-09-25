

f <- dir('~/Projecten/editrules/pkg/R',full.names=TRUE)
for ( x in f ) dmp <- source(x)

ageGrps=rep(1,5)
genderIndex=rep(1,5)
icdIndex=rep(103,5)
diagnosisIndex=c('TOT', 'A00', 'A02', 'C53','C54')
 
 
e.diag=editarray(c(
  "ageGrps %in% c(1,2,3,4)",
  "genderIndex %in% c(0,1,2)",
  "icdIndex %in% c(103)",
  "diagnosisIndex %in% c('TOT', 'A00', 'A02', 'C53','C54')", 
  "if(genderIndex=='1' & icdIndex=='103') diagnosisIndex %in% c('C53','C54')"
))
dat.diag=data.frame(ageGrps,genderIndex,icdIndex,diagnosisIndex)


el <- errorLocalizer(e.diag,t(dat.diag)[,1])

err=localizeErrors(e.diag,dat.diag)
 


