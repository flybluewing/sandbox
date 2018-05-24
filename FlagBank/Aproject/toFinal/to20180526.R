# to20180526.R 최종접근
source("./toFinal/to20180526_H.R")

# allIdx <- allIdxLst$allZoid.idx1
finalCut <- function( gEnv ,allIdx ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.

    allIdxF <- allIdx
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- gEnv$zhF[nrow(gEnv$zhF)-1,] - lastZoid

	# 참고 자료 --------------------------------------------------------------------
    rebCnt <- sapply( 2:nrow(gEnv$zhF) ,function(idx){
                    cnt <- sum( gEnv$zhF[idx-1,] %in% gEnv$zhF[idx,] )
                    return(cnt)
                })
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,2 ,function(p){sort(unique(p))})
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 	799 12 17 23 34 42 45
	# 	800  1  4 10 12 28 45
	# 	801 17 25 28 37 43 44
	# 	802 10 11 12 18 24 42
	# 	803  5  9 14 26 30 43
	# 	804  1 10 13 26 32 36
	#   805  3 12 13 18 31 32
	#   806 14,20,23,31,37,38
	#   807  6,10,18,25,34,35

	# <recycle> 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% lastZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 같은 자리 똑같은 값은 최근 두번이나..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( all(aZoid!=lastZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# zoid width : 44  27  32  38  35  29  24  29 ... 24,29 회피.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !((aZoid[6]-aZoid[1]) %in% c(24,49)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[2] : 10 단위가 너무 많았다.
    flag <- (gEnv$allZoidMtx[allIdxF,2]%%10) != 0	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep[1:2] 4,8 -> 2배율 관계 제거.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cStep <- aZoid[2:3]-aZoid[1:2]
					return( (cStep[1]*2) != cStep[2] )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep 4,8 배율 존재 제거
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cStep <- aZoid[2:6]-aZoid[1:5]
					return( 0==chkHaveSeq(cStep,c(4,8)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cStep 1이 또 나오려나?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cStep <- aZoid[2:6]-aZoid[1:5]
					return( all(cStep!=1) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# fStep 동일값이 연이어 존재? 37,38 -> 34,35
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					fStep <- lastZoid - aZoid
					return( all(fStep[2:6]!=fStep[1:5]) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# zoid[c(4,6)] Q5,qx,Q5 패턴 재발 피하자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					rem <- aZoid[c(4,6)]%%10
					return( !all(rem==c(5,5)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	

	# <recycle> zoid[1] 은 9 이내로 집중
    flag <- gEnv$allZoidMtx[allIdxF,1] <= 9	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	

	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	# colPtnLst[[1]]$val  13  4  8  4
	# colPtnLst[[2]]$val  13 36
	# colPtnLst[[3]]$val  14 27 33
	# colPtnLst[[4]]$val  35 26 27 33 33
	# colPtnLst[[5]]$val  27 42 32 42
	# colPtnLst[[6]]$val  

	# <recycle>
	banVal <- sapply( colPtnLst ,function(p){return( if(0<length(p$val)) p$val[1] else 0 )})
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					chkFlag <- aZoid==banVal
					return( 2>sum(chkFlag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					chkFlag <- rep(F,30)
					chkFlag[1] <- aZoid[1]==4
					chkFlag[5] <- aZoid[5]==42
					chkFlag[30] <- 0<sum(aZoid==banVal)
					return( 2>sum(chkFlag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] -  5, 6
	#		cvSeqNextLst[[2]] zoid[2:3] -  3, 4 (5,6)
	#		cvSeqNextLst[[3]] zoid[3:4] -  2,10
	#		cvSeqNextLst[[4]] zoid[4:5] - 27,45
	#		cvSeqNextLst[[5]] zoid[5:6] - 20,31

	# <recycle> 둘 중 하나라도 일치하는 게 총 3개 이상.
	matLst <- lapply( cvSeqNextLst ,function(p){
					return( if(0<nrow(p$fndMtx)) p$fndMtx[1,] else c(0,0) )
				})
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( cIdx in 1:5 ){
						cnt <- cnt + sum(aZoid[cIdx+0:1]==matLst[[cIdx]])
					}
					return( 3>cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> 이전 쌍과 매치되는 게 2개 이상.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( cIdx in 1:5 ){
						for( rIdx in seq_len(nrow(cvSeqNextLst[[cIdx]]$fndMtx)) ){
							if( all(aZoid[cIdx+0:1]==cvSeqNextLst[[cIdx]]$fndMtx[rIdx,]) ){
								cnt <- cnt + 1
								break
							}
						}
					}
					return( 2>cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:3] - 3 19 22 
	#                                     4  7 16

	#  4,3,2(?) zoid[1] 에서 2 회피
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( all(aZoid[1:3]!=c(3,19,22)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# qqe working	0:		1:205,780

	rebCnt <- sapply( 1:nrow(gEnv$zhF) ,function(hIdx){
					if(hIdx==1) return(0)

					cnt <- sum( gEnv$zhF[hIdx,]%in%gEnv$zhF[(hIdx-1),] )
					return(cnt)
				} )

	#-------------------------------------------------------------------------------
	# rebCnt == 0
	zoidMtx <- gEnv$zhF[rebCnt==0,,drop=F]
	uAnaObj <- getUnitAnalyzer( zoidMtx ,pECol=NULL )
	rptUnitAnalyze( uAnaObj ,pTitle="rebound count zero" ,pRptFile="./report/rptUA_rebCnt0" )

	#-------------------------------------------------------------------------------
	# rebCnt == 1

	# ==============================================================================
	# 


	# loose.ban.colValSeqNext() 0.05%
	cvSeqNextObj <- loose.ban.colValSeqNext( gEnv$zhF ,gEnv$allZoidMtx[allIdxF,] ,pLevel=2 )
	allIdxF <- allIdxF[-cvSeqNextObj$filtedIdx]






	allIdxF <- cutByWidth( gEnv ,allIdxF )

	azColValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))} )
	# ==============================================================================
	# cutByWidth()
	allIdxF <- cutByWidth( gEnv ,allIdxF )
	allIdxF <- cutByColVal.col1( gEnv ,allIdxF )	# 2:1454    4:1088    5:1006    7:661
	allIdxF <- cutByColVal.col2( gEnv ,allIdxF )
	allIdxF <- cutByColVal.col3( gEnv ,allIdxF )
	allIdxF <- cutByColVal.col4( gEnv ,allIdxF )
	allIdxF <- cutByColVal.col5( gEnv ,allIdxF )
	allIdxF <- cutByColVal.col6( gEnv ,allIdxF )



    

	# length(allZoidF) 26257
	allIdxF.4 <- finalCut.first4( gEnv ,allIdxF )
	allIdxF.7 <- finalCut.first7( gEnv ,allIdxF )

	rObj <- list( allIdxF=allIdxF ,allIdxF.4=allIdxF.4 ,allIdxF.7=allIdxF.7 )

    # allIdx <- allIdxF
    
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})
    stdColValLst <- apply( gEnv$zhF ,2 ,function(p){sort(unique(p))})

    rebCnt <- sapply( 2:nrow(localHisMtx) ,function(idx){
                        sum( localHisMtx[(idx-1),] %in% localHisMtx[idx,] )
                    })
    rebMtxLst <- lapply( which(rebCnt>1) ,function( idx ){
                        return( localHisMtx[idx+0:1,] )
                    })

    return( rObj )

} # finalCut()




