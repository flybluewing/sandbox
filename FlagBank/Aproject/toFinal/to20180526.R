# to20180526.R 최종접근
source("./toFinal/to20180519_H.R")

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

    # zoid[3] : 13,13,23,q3?
    flag <- gEnv$allZoidMtx[allIdxF,3]%%10 != 3	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[3] : 14,xx,13,xx,12?
    flag <- gEnv$allZoidMtx[allIdxF,3] != 12	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[5] : 30,XX,31,XX,32? no
    flag <- gEnv$allZoidMtx[allIdxF,5] != 32	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 재발이 너무 많았다.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( lastZoid %in% aZoid )
					return( cnt<1 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # cStep 1도 너무 많았다.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any( 1==(aZoid[2:6]-aZoid[1:5]) ) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[3] q4,xx,q3,xx,q2? and zoid[6] q4,xx,q3,xx,q2,xx,q1?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					rem <-(aZoid%%10)[c(3,6)]
					return( !all( rem==c(2,1) ) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid %/% 10 이 4개 이상인 것은 제외하자.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( 3>=max(table(aZoid%/%10)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# qqe working	0:91,693		1:167,098





	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	# colPtnLst[[1]]$val   7  4  7  2 13  1  1  6  8 14  1 22  6  6
	# colPtnLst[[2]]$val  12 29 13 11
	# colPtnLst[[3]]$val  28 20  9
	# colPtnLst[[4]]$val  22 23 16 28
	# colPtnLst[[5]]$val  12 24 27 29
	# colPtnLst[[6]]$val  22 42 17 42 37

    # zoid[1] 이 2,4,7일 때 나머지 컬럼에서의 colPtnVal 재발은 없겠지.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( !(aZoid[1] %in% c(2,4,7)) ){
						return( TRUE )
					}
					return( !any(aZoid[2:6]==c(12,28,22,12,22)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[1] 이 2,4,7일 때 zoid[4]에서 colPtnVal 21은 나오기 어렵겠지.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( !(aZoid[1] %in% c(2,4,7)) ){
						return( TRUE )
					}
					return( !any(aZoid[4]==c(21)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] - 8,16
	#			zoid[1] != 9 : 6,7,8,?
	#			zoid[2] !=16 : 9,9,16,?
	#			zoid[2]/zoid[1] != 2
	#		cvSeqNextLst[[2]] zoid[2:3] - 31,35
	#			zoid[3] 10 : 27,26,35,?	- n4가 나오기엔..
	#		cvSeqNextLst[[3]] zoid[3:4] - 5,11
	#			zoid[3] : 9,5,4,5,?	- 4,6,9가 나오기엔..
	#		cvSeqNextLst[[3]] zoid[4:5] - 15,16
	#			zoid[4]-zoid[2] : 1 차이 반복될까?
	#		cvSeqNextLst[[3]] zoid[5:6] - 14,22

	# loose.ban.colValSeqNext() 0.05%
	cvSeqNextObj <- loose.ban.colValSeqNext( gEnv$zhF ,gEnv$allZoidMtx[allIdxF,] ,pLevel=2 )
	allIdxF <- allIdxF[-cvSeqNextObj$filtedIdx]

	# cvSeqNextLst[[1]]$fndMtx[,1] : 6,x,8,6,x,8,? 6이 다시 재발하기엔 너무 규칙적이겠지?
	flag <- gEnv$allZoidMtx[allIdxF,1]!=6	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[1]이 7,4,2일 때(colPtnLst[[1]]$val) zoid[2] 값이 2배인 경우는 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( !(aZoid[1] %in% c(2,4,7)) ){
						return( TRUE )
					}
					return( (2*aZoid[1]) != aZoid[2] )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:3] - 8 13 26
	#		cvSeqNextLst[[2]] zoid[2:4] - 
	#		cvSeqNextLst[[3]] zoid[3:5] - 9 14 16
	#		cvSeqNextLst[[3]] zoid[4:6] -22 31 42

	# cvSeqNextLst[[1]] fndMtx[,1] 3->8이 너무 많다.
	flag <- gEnv$allZoidMtx[allIdxF,1]!=8	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cvSeqNextLst[[3]] zoid[3:5] - c( 9,14,16) 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !all(aZoid[3:5]==c(9,14,16)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cvSeqNextLst[[3]] zoid[4:6] - c(22,31,42) 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !all(aZoid[4:6]==c(22,31,42)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# ==============================================================================
	# zoid[1] 9 미만으로 제한
	flag <- gEnv$allZoidMtx[allIdxF,1]<9	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 10 이나 11 배수는 2개 이상 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 1<sum(aZoid%%10==0) ) return( FALSE )
					if( 1<sum(aZoid%%11==0) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[3]은 10 이상으로 보자.
	flag <- gEnv$allZoidMtx[allIdxF,3]>=10	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[5] 는 33 이상이겠지.
	flag <- gEnv$allZoidMtx[allIdxF,5]>=33	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# col(2,3)과 col(5,6)의 간격이 똑같게 반복될까?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( (aZoid[3]-aZoid[2])==(aZoid[6]-aZoid[5]) ){
						return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 인접 값 끼리 배수관계 존재 피하자.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					rem <- aZoid[2:6] %% aZoid[1:5]
					if( any(rem==0) ){
						return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

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




