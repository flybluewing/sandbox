# to20180616.R 최종접근
source("./toFinal/to20180616_H.R")

# allIdx <- allIdxLst$allZoid.idx0
finalCut <- function( gEnv ,allIdx ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.

    allIdxF <- allIdx
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - gEnv$zhF[nrow(gEnv$zhF)-1,]

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
	#	809  6 11 15 17 23 40
	#   810  5 10 13 21 39 43

	# 3,6 : 3,x,6,x,6,x,3(?)
	# 5   : 15,10,5 (?) 
	flag <- !(gEnv$allZoidMtx[allIdxF,1] %in% c(3,5,6))	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flag <- !(gEnv$allZoidMtx[allIdxF,2] %in% c(9,12))	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 20,10 이 너무 많은 듯.
	flag <- ( gEnv$allZoidMtx[allIdxF,2]%%10 != 0 )	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flag <- !(gEnv$allZoidMtx[allIdxF,4] %in% c(38))	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flag <- !(gEnv$allZoidMtx[allIdxF,6] %in% c(40,43,45))	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# <recycle> 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% lastZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> zoid[1] 은 9 이내로 집중
    flag <- gEnv$allZoidMtx[allIdxF,1] <= 9	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> cStep 반복은 2개 이하.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 2 > cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# <recycle> fStep 반복은 2개 이하.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( fStep==(aZoid-lastZoid) )
					return( 2 > cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # rem 동일 3 이상
    zRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( zRem==(aZoid%%10) )
					return( cnt<3 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # quotient 동일 4이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- max( table(aZoid%/%10) )
					return( cnt<4 )
				})	;kIdx<-head(which(!flag))
    fltCnt[!flag] <- fltCnt[!flag] + 1


	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=F )
	# colPtnLst[[1]]$val   8  9  2 20
	# colPtnLst[[2]]$val  19 17 17
	# colPtnLst[[3]]$val   7 35 10
	# colPtnLst[[4]]$val  23
	# colPtnLst[[5]]$val  28 23 38
	# colPtnLst[[6]]$val  42 36 24 43 45 38 32

	# <recycle> 마지막 값이 2개 이상 일치 배제
	banVal <- sapply( colPtnLst ,function(p){return( if(0<length(p$val)) p$val[1] else 0 )})
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					chkFlag <- aZoid==banVal
					return( 2>sum(chkFlag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(aZoid==banVal)
					cnt <- cnt + sum( aZoid[1:2]==c(7,17) )
					return( 2>cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] -  2 11 (14,19  x,x 15,22)
	#		cvSeqNextLst[[2]] zoid[2:3] - 12 13
	#		cvSeqNextLst[[3]] zoid[3:4] -  8 16
	#		cvSeqNextLst[[4]] zoid[4:5] - 40 43
	#		cvSeqNextLst[[5]] zoid[5:6] - 21 29

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
	#		cvSeqNextLst[[1]] zoid[1:3] -  2 11 17
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !all(aZoid[1:3]==c( 2,11,17)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

 	fltCnt <- finalFlt.Recycle( gEnv ,allIdxF )$fltCnt	# 0이 존재한다면 0도 제외
	flag <- fltCnt<3	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF.bak <- allIdxF

	tStmp <- Sys.time()
	uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=F ) # 한번 Cut한 후 레포트 생성하자.
	rstObj <- cutUAna_byGrp( uAnaLstGrp ,gEnv ,allIdxF ,thldName="allZoid.idx0" )
	allIdxF <- rstObj$allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	rstObj <- cutUAna_Inter( uAnaLstGrp ,gEnv ,allIdxF )
	allIdxF <- rstObj$allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	tDiff <- Sys.time() - tStmp

	# uAnaLstGrp에서 uAnaCutData 수집.
	#		- uAnaCutData.cust
	uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=TRUE )
	uAnaCutDataLst.c <- list()	# uAnaCutDataLst custom
	for( nIdx in attributes(uAnaLstGrp)$names ){	# nIdx <- "uAnaLst.rebCnt"
		uAnaLst <- uAnaLstGrp[[nIdx]]
		cutDataLst <- list()
		for( uIdx in 1:length(uAnaLst) ){
			cutData <- list( )
			cutData$colVal		<- uAnaLst[[uIdx]]$uAnaCutData$colVal
			cutData$colVal.f	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.f
			cutData$colVal.c	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.c
			cutDataLst[[uAnaLst[[uIdx]]$idStr]] <- cutData
		} # uIdx
		uAnaCutDataLst.c[[nIdx]] <- cutDataLst
	} # nIdx

	# 상황에 맞게 제외조건 추가. to2018nnnn_H.R에서 정의
	customizeCutData <- function( uAnaCutDataLst.c ){ return(uAnaCutDataLst.c) }
	uAnaCutDataLst.c <- customizeCutData( uAnaCutDataLst.c )


	#	uAnaCutData 에 대해 임의 추가
	#	단일 uAnaCutData 내에서의 중복 제거대상 적용.
	#	다른 uAnaCutDate 에서도 중복 제거대상 되는 것 제거.




	tStmp <- Sys.time()
	uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=F ) # 한번 Cut한 후 레포트 생성하자.
	cat(sprintf("uAnaLstGrp$allIdxF %d\n",length(uAnaLstGrp$allIdxF)))
	# fltCntSum <- assInterUAnaGrp( gEnv ,allIdxF ,uAnaLstGrp )
	# table(fltCntSum)
	tDiff <- Sys.time() - tStmp

	# save
	# saveObj <- list( allIdxF=allIdxF ,fltCntSum=fltCntSum )
	# save( saveObj ,file="Obj_saveObj.save")

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




