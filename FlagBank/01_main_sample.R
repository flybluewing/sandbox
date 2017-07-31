# 실행용 코드가 아님!!

# 실행 예시를 모아두기 위한 코드

source("hCommon.R")

pSAF <- list()
pSAF$def	<- getDefSAF_multiple(1,3)
class(pSAF)	<- class(pSAF$def)
pSAF$flag	<- getFlag( pSAF )
class(pSAF)	<- class(pSAF$flag)
pSAF$simul	<- getMlpSimul( pSAF ,pSimulNum=2 ,pSimulSetNum=3 )
pSAF$predr	<- getMlpPredr( pSAF )

tSample	<- pSAF$flag$rMtx[1,]
pred	<- k.predict( pSAF ,pXVal=tSample[pSAF$flag$col.x] )

