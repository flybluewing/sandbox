# FB가 존재해야 함.

pDP <- DP.defineDPrelative()
pDP$flag <- DP.getFlag( pDP )
pDP$predr <- DP.getPredr( pDP )

DPdiff7			<- DP.defineDPdiff( pMax=7 )
DPdiff7$flag	<- DP.getFlag( DPdiff7 )
DPdiff7$predr	<- DP.getPredr( DPdiff7 )

dnaMtx <- matrix( 0 ,nrow=4 ,ncol=GZoid$dnaSize )
dnaMtx[1,]	<- sort(sample(GZoid$dnaCode,GZoid$dnaSize))
dnaMtx[2,]	<- sort(sample(GZoid$dnaCode,GZoid$dnaSize))
dnaMtx[3,]	<- sort(sample(GZoid$dnaCode,GZoid$dnaSize))
dnaMtx[4,]	<- sort(sample(GZoid$dnaCode,GZoid$dnaSize))

anaObj <- DP.analyze( DPdiff7, dnaMtx )

DP.predr
