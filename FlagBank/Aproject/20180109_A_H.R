# 20180109_A_H.R 마지막 시도가 되길..


filt_A0010 <- function( pEnv ){
	#	42/789 5%
	
	filtId="A0010";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
	stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
	flag <- stepM<=2

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0010()

filt_A0010.hard <- function( pEnv ) {
	#	동일간격 4개 이상이거나 3개가 연달아 놓이는 경우.
	#	15/789 2%
	
	filtId="A0010.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	allCodeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
	flag <- rep( TRUE ,nrow(allCodeMtx) )
	for( aIdx in 1:nrow(allCodeMtx) ){
		curCode <- allCodeMtx[aIdx,]
		cnt <- max(table(curCode))
		if( 3 > cnt ){
			next
		} else if(4<=cnt){
			flag[aIdx] <- FALSE
			next
		}
		
		seqCnt <- 0
		for( idx in 1:(length(curCode)-1) ){
			if( curCode[idx]==curCode[idx+1] ){
				seqCnt <- seqCnt + 1
			} else { seqCnt<-0 }
			if(2<=seqCnt){ # 즉 3연속 발생.
				flag[aIdx] <- FALSE
			}
		}
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0010.hard()


filt_A0020 <- function( pEnv ){	# 7599240
	
	filtId="A0020";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 2
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- (cnt>0)&(cnt<6)

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0020()

filt_A0030 <- function( pEnv ){	# 7041489
	
	filtId="A0030";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 3
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- cnt>0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0030()

#----------------------------------------------------------------
#  A0100 : lastZoid를 기준으로 잘라낸다.
filt_A0100.A <- function( pEnv ){	# 7740330
	
	filtId="A0100.A";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allCodeMtx) )
	for( nextJump in 1:8 ){
		filtGrp <- getPtnRebGrp( stdCodeMtx ,pNextJump=nextJump )
		filtRst <- filtGrp$filt( allCodeMtx )
		rstFlag <- sapply(filtRst ,function(p){p$survive})
		flag <- flag & rstFlag
		# allCodeMtx <- allCodeMtx[flag,] 속도 단축 시도는 나중에 하자.
		tDiff <- Sys.time() - tStmp
		pEnv$logStr(sprintf("nextJump:%d  past:%.1f%s remove:%d"
						,nextJump,tDiff,units(tDiff),sum(!rstFlag) )
				)
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0100.A()

#-[A0110.A]------------------------------------------------------
filt_A0110.A <- function( pEnv ){
	# 117/389 (30%)

	filtId="A0110.A";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allCodeMtx) )
	for( nextJump in 1:2 ){
		filtGrp <- getPtnRebGrp2( stdCodeMtx ,pNextJump=nextJump )
		filtRst <- filtGrp$filt( allCodeMtx )
		rstFlag <- sapply(filtRst ,function(p){p$survive})
		flag <- flag & rstFlag
		tDiff <- Sys.time() - tStmp
		pEnv$logStr(sprintf("nextJump:%d  past:%.1f%s remove:%d"
						,nextJump,tDiff,units(tDiff),sum(!flag) )
				)
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0110.A()

filt_A0110.A.hard <- function( pEnv ){
	# pSurviveLimit=1	 1/389
	# pSurviveLimit=2	11/389

	filtId="A0110.A.hard";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allCodeMtx) )
	for( nextJump in 1:2 ){
		filtGrp <- getPtnRebGrp2( stdCodeMtx ,pNextJump=nextJump )
		filtRst <- filtGrp$filt( allCodeMtx ,pSurviveLimit=1 )
		rstFlag <- sapply(filtRst ,function(p){p$survive})
		flag <- flag & rstFlag
		tDiff <- Sys.time() - tStmp
		pEnv$logStr(sprintf("nextJump:%d  past:%.1f%s remove:%d"
						,nextJump,tDiff,units(tDiff),sum(!flag) )
				)
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0110.A.hard()


#-[AJ000.A]------------------------------------------------------
#	rebLen <- getRebLen( 1:45 ,zhF )
filt_AJ000.A <- function( pEnv ){	#
	# 기존 발생과 5개 동일한 것은 딱 두 번 있었다.
	# 사실 시간만 오래 걸리고 제거가능성은 그리 높지 않을 거 같은데....

	filtId="AJ000.A";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	zhF <- pEnv$zhF
	allZoidMtx <- pEnv$allZoidMtx

	testSpan <- 100:nrow(zhF)
	stdCodeMtx <- matrix( NA ,nrow=length(testSpan) ,ncol=ncol(zhF) )
	for( idx in 1:length(testSpan) ){
		tIdx <- testSpan[idx]
		rebLen <- getRebLen( 1:45 ,zhF[1:(tIdx-1),] )
		stdCodeMtx[idx,] <- rebLen[zhF[tIdx,]]
	}

	curRebLen <- rep( NA ,ncol(allZoidMtx) )
	flagIdx <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		curRebLen <- rebLen[allZoidMtx[aIdx,]]
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum(curRebLen==stdCodeMtx[sIdx,])
			if( cnt >=5 ){
				flagIdx[aIdx] <- testSpan[sIdx]
				break
			}
		}
	}
	flag <- flagIdx == 0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AJ000.A()

#-[AJ000.B]------------------------------------------------------
#	rebLen <- getRebLen( 1:45 ,zhF )
#		filt_AJ000.A 에서 5개가 아닌 4개 일치 체크(최근 h개는 비슷한 패턴 없..)
#			총 689개 내에서 20H이내는 4쌍, 5%는 154H이내.
#			차리리 AJ000.B를 정식버전으로 하고 filt_AJ000.A를 Hard 버전으로 할까...
filt_AJ000.B <- function( pEnv ){	#

	filtId="AJ000.B";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	zhF <- pEnv$zhF
	allZoidMtx <- pEnv$allZoidMtx

	testSpan <- 100:nrow(zhF)
	stdCodeMtx <- matrix( NA ,nrow=length(testSpan) ,ncol=ncol(zhF) )
	for( idx in 1:length(testSpan) ){
		tIdx <- testSpan[idx]
		rebLen <- getRebLen( 1:45 ,zhF[1:(tIdx-1),] )
		stdCodeMtx[idx,] <- rebLen[zhF[tIdx,]]
	}

	testSpan <- testSpan[(nrow(stdCodeMtx)-159):nrow(stdCodeMtx)]
	stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-159):nrow(stdCodeMtx),]

	curRebLen <- rep( NA ,ncol(allZoidMtx) )
	flagIdx <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		curRebLen <- rebLen[allZoidMtx[aIdx,]]
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum(curRebLen==stdCodeMtx[sIdx,])
			if( cnt >=4 ){
				flagIdx[aIdx] <- testSpan[sIdx]
				break
			}
		}
	}
	flag <- flagIdx == 0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AJ000.B()

#-[AJ000.C]------------------------------------------------------
#	rebLen <- getRebLen( 1:45 ,zhF )
#		제일 작은 rebLen 3개의 값과 갯수.
#			1H 에서 동일매치 6쌍, 7H에서 동일매치 45쌍
filt_AJ000.C <- function( pEnv ){	#

	filtId="AJ000.C";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	zhF <- pEnv$zhF
	allZoidMtx <- pEnv$allZoidMtx

	testSpan <- 100:nrow(zhF)
	stdCodeMtx <- matrix( NA ,nrow=length(testSpan) ,ncol=ncol(zhF) )
	for( idx in 1:length(testSpan) ){
		tIdx <- testSpan[idx]
		rebLen <- getRebLen( 1:45 ,zhF[1:(tIdx-1),] )
		stdCodeMtx[idx,] <- rebLen[zhF[tIdx,]]
	}
	testSpan <- testSpan[ (nrow(stdCodeMtx)-7+1):nrow(stdCodeMtx) ]
	stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-7+1):nrow(stdCodeMtx), ]
	stdCodeMtx <- minFreqCnt( stdCodeMtx ,pSize=3 )

	allCodeMtx <- allZoidMtx
	for( aIdx in 1:nrow(allCodeMtx) ){
		allCodeMtx[aIdx,] <- rebLen[ allZoidMtx[aIdx,] ]
	}
	allCodeMtx <- minFreqCnt( allCodeMtx ,pSize=3 )

	flagIdx <- rep(0,nrow(allCodeMtx))
	for( aIdx in 1:nrow(allCodeMtx) ){
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum( allCodeMtx[aIdx,]==stdCodeMtx[sIdx,] )
			if( cnt >= 6 ){
				flagIdx[aIdx] <- sIdx # testSpan[sIdx] 디버깅이 더 어려워진다...
				break
			}
		}
	}
	flag <- flagIdx == 0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AJ000.C()





#-[AK000.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
filt_AK000.A <- function( pEnv ){	#
	
	filtId="AK000.A";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- apply( allCodeMtx ,1 ,function(p){p[6]-p[1]})
	flag <- flag>20

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.A()

#-[AK000.B]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 다음에도 반복될 가능성.. 5% 미만.
filt_AK000.B <- function( pEnv ){	#
	
	filtId="AK000.B";	tStmp <- Sys.time()
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	flag <- (allCodeMtx[,6]-allCodeMtx[,1])!=(lastZoid[6]-lastZoid[1])

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.B()

#-[AK000.C]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이전 패턴의 반복. 3% 미만.
filt_AK000.C <- function( pEnv ){	#
	
	filtId="AK000.C";	tStmp <- Sys.time()
	stdCode <- pEnv$zhF[,6]-pEnv$zhF[,1]
	allZoidMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allZoidMtx) )
	for( dIdx in 5:1 ){
		ptn <- getPastPtn( stdCode ,pDepth=dIdx )
		if( !is.null(ptn) ){
			flag <- (allZoidMtx[,6]-allZoidMtx[,1])!=ptn$nextVal
			break
		}
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.C()

#-[AK000.D]------------------------------------------------------
#	zhF[,6]-zhF[,1]	: 이전과 같은 간격으로 건너뛸 가능성. 55/785
filt_AK000.D <- function( pEnv ){	#
	
	filtId="AK000.D";	tStmp <- Sys.time()
	stdCode <- pEnv$zhF[,6]-pEnv$zhF[,1]
	allCodeMtx <- pEnv$allZoidMtx

	remVal <- c( stdCode[length(stdCode)-1] , 
					stdCode[length(stdCode)] + 
						abs(stdCode[length(stdCode)]-stdCode[length(stdCode)-1])
				)
	flag <- apply( allZoidMtx ,1 ,function(p){ !( (p[6]-p[1])%in%remVal ) })

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.D()

#-[AL000.A]------------------------------------------------------
#	이전에 발생한 DNA의 첫번째가 다음에도 발생할 가능성. 53/785
filt_AL000.A <- function( pEnv ){	#
	
	filtId="AL000.A";	tStmp <- Sys.time()
	zhF <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allCodeMtx) )
	comVal <- intersect(zhF[nrow(zhF),],zhF[nrow(zhF)-1,])
	if( 0 < length(comVal) ){
		flag <- apply( allCodeMtx ,1 ,function(p){ !(comVal[1]%in%p) } )
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AL000.A()

#-[AP000.A]------------------------------------------------------
#	zhF %/% 10 Quoatient.  이전 H와 1개 이내로 틀리는 것 59/787
filt_AP000.A <- function( pEnv ){	#
	
	filtId="AP000.A";	tStmp <- Sys.time()
	allCodeMtx <- pEnv$allZoidMtx
	stdCode <- pEnv$zhF[nrow(pEnv$zhF),] %/% 10

	flag <- apply( allZoidMtx %/% 10 ,1 ,function(p){ sum(p!=stdCode) })
	flag <- flag > 1

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.A()

#-[AP000.B]------------------------------------------------------
#	zhF %/% 10 Quoatient.  동일한 패턴의 재발간격. 7 H 이내 재발 57/787
filt_AP000.B <- function( pEnv ){	#
	
	filtId="AP000.B";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF %/% 10
	stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-6):nrow(stdCodeMtx),]
	allCodeMtx <- pEnv$allZoidMtx %/% 10

	flag <- rep( 0 ,nrow(allCodeMtx) )
	for( aIdx in 1:nrow(allCodeMtx) ){
		for( sIdx in 1:nrow(stdCodeMtx) ){
			if( all(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,]) ){
				flag[aIdx] <- sIdx
				break
			}
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.B()

#-[AP000.C]------------------------------------------------------
#	zhF %/% 10 Quoatient.  한가지 Quoatient가 4개 이상인 경우 49/787
filt_AP000.C <- function( pEnv ){	#
	
	filtId="AP000.C";	tStmp <- Sys.time()
	allCodeMtx <- pEnv$allZoidMtx %/% 10

	flagLst <- apply(allCodeMtx ,1 ,table)
	flag <- sapply(flagLst,max)
	flag <- flag < 4

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.C()

#-[AP000.D]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient패턴 Next 값.	5/288
filt_AP000.D <- function( pEnv ){	#
	
	filtId="AP000.D";	tStmp <- Sys.time()
	stdCodeMtx <- getTblCnt( pEnv$zhF %/% 10 )
	allCodeMtx <- getTblCnt( pEnv$allZoidMtx %/% 10 )

	flag <- rep( 0 ,nrow(allCodeMtx) )
	ptn <- getPtnReb( stdCodeMtx )
	if( !is.null(ptn) ){
		for( aIdx in 1:nrow(allCodeMtx) ){
			if( all(allCodeMtx[aIdx,]==ptn$nextRow) ){
				flag[aIdx] <- 1
			}
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.D()

#-[AP000.E]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient그룹이 다음에도 반복.
filt_AP000.E <- function( pEnv ,pStepSize=1000000 ){	#
	
	filtId="AP000.E";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx
	stdGrpLst <- apply( pEnv$zhF ,1 ,function(p){ getQGrp(p,0:4) } )
	for( gIdx in 1:length(stdGrpLst) ){
		# 1~9, 40번 대역에서 1개 나오는 경우는 너무 흔하니,
		#	아예 없는 것으로 제외시킴.
		if( 1==stdGrpLst[[gIdx]]$grpCnt[1] ){
			stdGrpLst[[gIdx]]$grpCnt[1] <- 0
			stdGrpLst[[gIdx]]$grpLst[[1]] <- integer(0)
		}
		if( 1==stdGrpLst[[gIdx]]$grpCnt[5] ){
			stdGrpLst[[gIdx]]$grpCnt[5] <- 0
			stdGrpLst[[gIdx]]$grpLst[[5]] <- integer(0)
		}
	}

	backMtx <- matrix( c(1,2) ,ncol=2 ,nrow=1 ) # grp 크기 ,검색 H 범위
	backMtx <- rbind( backMtx ,c(2,5) )
	backMtx <- rbind( backMtx ,c(3,200) )
	#	4이상은 아예 존재치를 않았다. AP000.C에서 제거됨.

	flag <- rep( TRUE ,nrow(allZoidMtx) )
	stepEadge <- nrow(allZoidMtx)	# 메모리 용량한계땜시 부분부분 처리하자.
	stepSize <- pStepSize
	for( stepIdx in 0:(stepEadge %/% stepSize) ){
		stepSpan <- (1:stepSize) + stepIdx*stepSize
		stepSpan <- stepSpan[stepSpan<=stepEadge]

		allGrpLst <- apply( allZoidMtx[stepSpan,] ,1 ,function(p){ getQGrp(p,0:4) } )
		for( bIdx in 1:nrow(backMtx) ){	
			scanSpan <- (length(stdGrpLst)-backMtx[bIdx,2]+1):length(stdGrpLst)
			for( sIdx in scanSpan ){
				rstFlag <- stdGrpLst[[sIdx]]$filt( allGrpLst ,pMin=backMtx[bIdx,1] )
				flag[stepSpan] <- (rstFlag==0) & flag[stepSpan]
			}
		}
		allGrpLst <- NULL;	gc()
	}

	# allGrpLst <- apply( allZoidMtx ,1 ,function(p){ getQGrp(p,0:4) } )

	# flag <- rep( TRUE ,length(allGrpLst) )
	# for( bIdx in 1:nrow(backMtx) ){	
	# 	scanSpan <- (length(stdGrpLst)-backMtx[bIdx,2]+1):length(stdGrpLst)
	# 	for( sIdx in scanSpan ){
	# 		rstFlag <- stdGrpLst[[sIdx]]$filt( allGrpLst ,pMin=backMtx[bIdx,1] )
	# 		flag <- (rstFlag==0) & flag
	# 	}
	# }

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.E()

filt_AP000.E.hard <- function( pEnv ,pStepSize=1000000 ){	#

	filtId="AP000.E.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx
	stdGrpLst <- apply( pEnv$zhF ,1 ,function(p){ getQGrp(p,0:4) } )
	# grp 1짜리는 아예 backMtx에서 취급 안하기로 한다.

	# grp 크기 ,검색 H 범위. 
	# AP000.C등의 필터를 거치지 않을 상태도 포함될 수 있으므로 6개까지 모두 체크한다.
	backMtx <- matrix( c(2,1) ,ncol=2 ,nrow=1 ) # 6 / 788
	backMtx <- rbind( backMtx ,c(3,40) )	# 4 / 788
	backMtx <- rbind( backMtx ,c(4,length(stdGrpLst)) )	# 0 / 788
	backMtx <- rbind( backMtx ,c(5,length(stdGrpLst)) )	# 0 / 788
	backMtx <- rbind( backMtx ,c(6,length(stdGrpLst)) )	# 0 / 788

	flag <- rep( TRUE ,nrow(allZoidMtx) )
	stepEadge <- nrow(allZoidMtx)	# 메모리 용량한계땜시 부분부분 처리하자.
	stepSize <- pStepSize
	for( stepIdx in 0:(stepEadge %/% stepSize) ){
		stepSpan <- (1:stepSize) + stepIdx*stepSize
		stepSpan <- stepSpan[stepSpan<=stepEadge]

		allGrpLst <- apply( allZoidMtx[stepSpan,] ,1 ,function(p){ getQGrp(p,0:4) } )
		for( bIdx in 1:nrow(backMtx) ){	
			scanSpan <- (length(stdGrpLst)-backMtx[bIdx,2]+1):length(stdGrpLst)
			for( sIdx in scanSpan ){
				rstFlag <- stdGrpLst[[sIdx]]$filt( allGrpLst ,pMin=backMtx[bIdx,1] )
				flag[stepSpan] <- (rstFlag==0) & flag[stepSpan]
			}
		}
		allGrpLst <- NULL;	gc()
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.E.hard()


#-[AQ000.A]------------------------------------------------------
#	DNA 코드가 다음 H에서 몇 개나 재발되는지. (3개 이상 20/787)
filt_AQ000.A <- function( pEnv ){	#
	
	filtId="AQ000.A";	tStmp <- Sys.time()
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	flag <- apply( allZoidMtx ,1 ,function(p){ sum(lastZoid%in%p) } )
	flag <- flag<3

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AQ000.A()

#-[AR000.A]------------------------------------------------------
#	remainder 근거리 재발. (3H 이내 재발 31/787)
filt_AR000.A <- function( pEnv ){	#
	
	filtId="AR000.A";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF[(nrow(pEnv$zhF)-2):nrow(pEnv$zhF),] %% 2
	allCodeMtx <- pEnv$allZoidMtx %% 2

	flag <- rep( 0 ,nrow(allCodeMtx) )
	for( aIdx in 1:nrow(allCodeMtx) ){
		for( sIdx in 1:nrow(stdCodeMtx) ){
			if( all(allCodeMtx[aIdx,]==stdCodeMtx[sIdx,]) ){
				flag[aIdx] <- sIdx
				break
			}
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AR000.A()

#-[AR000.B]------------------------------------------------------
#	remainder 패턴 재발 (nextVal)	17% 탈락.
filt_AR000.B <- function( pEnv ){	#
	
	filtId="AR000.B";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF %% 10
	allZoidMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allZoidMtx) )
	ptn <- getPtnRebGrp( stdCodeMtx ,pNextJump=1 )
	if( !is.null(ptn) ){
		flag <- sapply( ptn$filt( allZoidMtx %% 10 )
						,function(p){p$survive} 
					)
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AR000.B()

#-[AS000.A]------------------------------------------------------
#	연이은 DNA코드가 다음에도 연이어서 재발 38/787
filt_AS000.A <- function( pEnv ){	#
	
	filtId="AS000.A";	tStmp <- Sys.time()
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		rebIdx.1 <- which( lastZoid %in% allZoidMtx[aIdx,] )
		if( 2!=length(rebIdx.1) ){
			next	# 3개 이상발생은 AQ000.A에서 제거되었다.
		}
		flag[aIdx] <- 2
		rebIdx.2 <- which( allZoidMtx[aIdx,] %in% lastZoid )
		if( (rebIdx.1[2]-rebIdx.1[1])==(rebIdx.2[2]-rebIdx.2[1]) ){
			flag[aIdx] <- rebIdx.1[1]
		}
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AS000.A()



#-[C0000.A]------------------------------------------------------
#	zhF[,2:6]-zhF[,1:5] : 똑같은 경우는 없었고, 1개 틀린 경우는 32/787
#		백만개 당 23분 정도 소요.
filt_C0000.A <- function( pEnv ){	#
	
	filtId="C0000.A";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF[,2:6] - pEnv$zhF[,1:5]
	allCodeMtx <- pEnv$allZoidMtx[,2:6] - pEnv$allZoidMtx[,1:5]

	flag <- rep(0,nrow(allZoidMtx))
	for( aIdx in 1:nrow(allCodeMtx) ){
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum(stdCodeMtx[sIdx,]!=allCodeMtx[aIdx,])
			if( 1 >= cnt ){
				flag[aIdx] <- sIdx
				break
			}	
		}
	}
	flag <- flag == 0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_C0000.A()

#-[C1000.A]------------------------------------------------------
#	abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
#		2개 이하로 틀린 경우는 65개 정도
filt_C1000.A <- function( pEnv ){	#
	
	filtId="C1000.A";	tStmp <- Sys.time()
	zhF <- pEnv$zhF
	stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		chkVal <- abs(allZoidMtx[aIdx,]-lastZoid)
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum(chkVal==stdCodeMtx[sIdx,])
			if( 4<=cnt ){
				flag[aIdx] <- sIdx
				break
			}
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_C1000.A()





tempBlk <- function(){

}

filt_XXX <- function( pEnv ){	#
	
	filtId="XXX";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	# flag <- rep( TRUE ,nrow(allCodeMtx) )

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_XXX()


filt_XXX <- function( pEnv ) {
	
	filtId="XXX";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	# codeMtx <- allZoidMtx[,2:6]-allZoidMtx[,1:5]
	# stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
	# flag <- stepM<=2

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_XXX()
