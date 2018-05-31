# to20180526.R 최종접근
source("./toFinal/to20180602_H.R")

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
	#   808 15 21 31 32 41 43

	# <recycle> 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% lastZoid) )
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
	
	# zoid%%10 Q1 이 너무 많았다.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					rem <- aZoid%%10
					return( 1>=sum(rem==1) )
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
	# colPtnLst[[1]]$val   6 11 18 17  7
	# colPtnLst[[2]]$val  22 14 13
	# colPtnLst[[3]]$val  25 16 17 11 24 34 11
	# colPtnLst[[4]]$val  33 36
	# colPtnLst[[5]]$val  41 34
	# colPtnLst[[6]]$val  45 39 30

	# <recycle> 마지막 값이 2개 이상 일치 배제
	banVal <- sapply( colPtnLst ,function(p){return( if(0<length(p$val)) p$val[1] else 0 )})
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					chkFlag <- aZoid==banVal
					return( 2>sum(chkFlag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] -  7 18
	#		cvSeqNextLst[[2]] zoid[2:3] - 28 34
	#		cvSeqNextLst[[3]] zoid[3:4] - 37 38
	#		cvSeqNextLst[[4]] zoid[4:5] - 31 44
	#		cvSeqNextLst[[5]] zoid[5:6] - 34 44

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
	#		cvSeqNextLst[[2]] zoid[2:4] - 18 32 40
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( all(aZoid[2:4]!=c(18,32,40)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF.bak <- allIdxF

	tStmp <- Sys.time()
	uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=TRUE )
	allIdxF <-uAnaLstGrp$allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	tDiff <- Sys.time() - tStmp

	tStmp <- Sys.time()
	fltCntSum <- assInterUAnaGrp( gEnv ,allIdxF ,uAnaLstGrp )
	allIdxF <-uAnaLstGrp$allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	tDiff <- Sys.time() - tStmp


	# QQE working
	# qqe working	0:		1:205,780

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




