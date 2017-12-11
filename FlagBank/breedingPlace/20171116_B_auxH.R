# 20171116_B_auxH.R 임시

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
myObj <- load( "Obj_deskObj.save" )
#   deskObj
#    - zhF testSpan allZoidMtx 
#    - indices.pool indices.h indices.all indices.flag indices.zoidMtx


zhF <- deskObj$zhF
allZoidMtx <- deskObj$allZoidMtx

flag.z <- allZoidMtx[,2] %% 5

flag <- zhF[,2] %% 5
seqObj <- k.seq(flag)
seqCntMtx <- seqObj$seqCntMtx
seqIdx <- which(seqCntMtx[,"cnt"]>1)

resLst <- list()
for( hIdx in 500:length(flag) ){
	seqObj <- k.seq( flag[1:(hIdx-1)] )
	resObj <- list( hIdx=hIdx )
	resObj$hit <- flag[hIdx]!=flag[hIdx-1]
	resObj$rmCnt <- sum(flag.z == flag[hIdx-1])
	resLst[[1+length(resLst)]] <- resObj
}
resLst.base <- resLst

resLst <- list()
for( hIdx in 500:length(flag) ){
	seqObj <- k.seq( flag[1:(hIdx-1)] )
	resObj <- list( hIdx=hIdx )
	resObj$hit <- flag[hIdx]!=flag[hIdx-1]
	resObj$rmCnt <- sum(flag.z == flag[hIdx-1])
	resLst[[1+length(resLst)]] <- resObj
}



#	연속 발생은 없다.
#		- 다음 숫자는 지금 숫자와는 다르다는 전제
#	연속 발생 후, 바로 다른 연속발생이 발생치는 않는다.
#		- 2,2,2,3 으로 끝난 상태에서의 다음 숫자는 3이 아니다
#	연속 발생 종료 후 발생하는 숫자는 이전 연속발생 종료다음 숫자와 다르다.
#		- 현재 연속발생 숫자와 이전 연속발생 종료 다음 숫자를 제외하자.
#	연속 발생 길이는 이전 연속발생 길이와 다르다.
#		- 해당 길이 상태에서 연속발생 가능성 낮다. 그 다음다음 연속은 필히 발생.







