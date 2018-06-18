# to20180623.R 최종접근
source("./toFinal/to20180623_H.R")

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
	#	811  8 11 19 21 36 45

	# 19 = 8 + 11
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				for( idx in 1:4 ){
					if( aZoid[idx+2] == (aZoid[idx]+aZoid[idx+1]) ){
						return( FALSE )
					}
				}
				return( TRUE )
			})	;kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 11,10,11,10(?)
    flag <- gEnv$allZoidMtx[allIdxF,2] != c(10)	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 11,10,11,9(?)
    flag <- gEnv$allZoidMtx[allIdxF,2] != c(9)	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 41 ,x ,39 ,x ,38(?)
    flag <- gEnv$allZoidMtx[allIdxF,5] != c(38)	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 43 ,x ,43 ,x ,43(?)
    flag <- gEnv$allZoidMtx[allIdxF,6] != c(43)	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 21, 21... 또 연속 발생치는 않겠지.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( all(aZoid!=lastZoid) )
				})	;kIdx<-head(which(!flag))
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

    # <recycle> rem 동일 3 이상
    zRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( zRem==(aZoid%%10) )
					return( cnt<3 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # <recycle> quotient 동일 4이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- max( table(aZoid%/%10) )
					return( cnt<4 )
				})	;kIdx<-head(which(!flag))
    fltCnt[!flag] <- fltCnt[!flag] + 1


	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=F )
	# colPtnLst[[1]]$val  12 15 15  7  4 19  1
	# colPtnLst[[2]]$val  15  7  7 17
	# colPtnLst[[3]]$val  20  4
	# colPtnLst[[4]]$val  36 31 18
	# colPtnLst[[5]]$val  37 23 24 34 42 30 28
	# colPtnLst[[6]]$val  41 40 42 35 43 45 42 42 44 42 42

	# <recycle> 마지막 값이 2개 이상 일치 배제
	banVal <- sapply( colPtnLst ,function(p){return( if(0<length(p$val)) p$val[1] else 0 )})
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					chkFlag <- aZoid==banVal
					return( 2>sum(chkFlag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- ifelse( aZoid[1]==12 ,1 ,0 )
					cnt <- cnt + ifelse( aZoid[2]==15 ,1 ,0 )
					cnt <- cnt + ifelse( aZoid[6]==42 ,1 ,0 )
					return( 2>cnt )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] - 13 21 ( 8,17  21,25 )
	#		cvSeqNextLst[[2]] zoid[2:3] -  7 12 (11,20   9,27 )
	#		cvSeqNextLst[[3]] zoid[3:4] - 18 19 (18,28  15,18)
	#		cvSeqNextLst[[4]] zoid[4:5] - 18 24
	#		cvSeqNextLst[[5]] zoid[5:6] - 18 22 (33,44)

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

	# <recycle> 폭이 똑같은 게 2개 이상 제거.
	step <- sapply( cvSeqNextLst ,function(p){ ifelse(0<nrow(p$fndMtx),p$fndMtx[,2]-p$fndMtx[,1],-1) })
    cnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( idx in 1:5 ){
						if( step[idx] == (aZoid[idx+1]-aZoid[idx]) ){
							cnt <- cnt+1
						}
					}
					return( cnt )
				})	
	flag <- cnt<2 ;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> rem 패턴이 똑같은 거 2개 이상 제거.
	banValLst <- lapply( cvSeqNextLst ,function(p){ if(0<nrow(p$fndMtx)) p$fndMtx[1,]%%10 else c(-1,-1)  })
    cnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( idx in 1:5 ){
						if( all(banValLst[[idx]]==(aZoid[idx:(idx+1)]%%10)) ){
							cnt <- cnt+1
						}
					}
					return( cnt )
				})	
	flag <- cnt<2 ;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# ---------------------------------------------------
	# 이번 타임에서는 과감하게 해도 될 거 같다. fStep, cStep 모두 제각각임.

	# cvSeqNextLst -> fStep
	banValLst <- lapply( cvSeqNextLst ,function( p ){
					if( 2 > nrow(p$fndMtx) ) {
						return( c(-1,-1) )
					} else {
						return( p$fndMtx[1,] + (p$fndMtx[1,]-p$fndMtx[2,]) )
					}
				})
	#	fStep match by Pattern
	cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cnt <- 0
					for( idx in 1:5 ){
						if( all(aZoid[idx:(idx+1)] == banValLst[[idx]]) ){
							cnt <- cnt + 1
						}
					}
					return( cnt )
				})
	flag <- cnt<1 ;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	fStep match by value
	cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cnt <- 0
					for( idx in 1:5 ){
						cnt <- cnt + sum(aZoid[idx:(idx+1)] == banValLst[[idx]])
					}
					return( cnt )
				})
	flag <- cnt<2 ;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	hold point
	#		cvSeqNextLst[[2]] zoid[2:4] -  20 33 35

	# <recycle> 이전 쌍과 매치되는 게 있으면 제거.
	matLst <- lapply( cvSeqNextLst ,function(p){ if( 0==nrow(p$fndMtx) ) c(-1,-1,-1) else p$fndMtx[1,] } )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( idx in 1:4 ){
						if( all(matLst[[idx]]==aZoid[idx:(idx+2)]) ){
							return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# <recycle> finalFlt.Recycle()
 	fltCnt <- finalFlt.Recycle( gEnv ,allIdxF )$fltCnt	# 0이 존재한다면 0도 제외
	flag <- fltCnt<3	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

 	fltCnt.recycle <- finalFlt.Recycle( gEnv ,allIdxF )$fltCnt
	fltCnt.cust <- finalFlt.cust( gEnv ,allIdxF )$fltCnt
	flag <- 3 > ( fltCnt.recycle + fltCnt.cust )	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF.bak <- allIdxF


	tStmp <- Sys.time()
	uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=F ) # 한번 Cut한 후 레포트 생성하자.
	rstObj <- cutUAna_byGrp( uAnaLstGrp ,gEnv ,allIdxF ,thldName="allZoid.idx0" )
	allIdxF <- rstObj$allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	rstObj <- cutUAna_Inter( uAnaLstGrp ,gEnv ,allIdxF )	# assInterUAnaGrp() 사용
	allIdxF <- rstObj$allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	tDiff <- Sys.time() - tStmp

	save( allIdxF ,file="Obj_allIdxF.save" )

	
	# Report 생성.
	uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=TRUE )

	# uAnaLstGrp에서 uAnaCutData 수집.
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




