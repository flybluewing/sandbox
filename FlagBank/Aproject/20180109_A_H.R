# 20180109_A_H.R 마지막 시도가 되길..

getFiltLst.base <- function( ){

	filtFuncLst <- list()
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0010
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0020
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0030
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0040
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0100.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0110.A

	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.B
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.C

	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.B
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.C
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.D

	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AL000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.B
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.C
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.D
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.E
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.B
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AS000.A

	filtFuncLst[[1+length(filtFuncLst)]] <- filt_C0000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_C1000.A

	return( filtFuncLst )

} # getFiltLst.base()

getFiltLst.hard <- function( ){

	filtFuncLst <- list()
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0010.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0020.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0030.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0040.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0050.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0060.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0100.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0110.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.B.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.C.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.B.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.B.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.C.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.D.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.E.hard

	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.A
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.B
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.C

	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.B.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.C.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AS000.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_C0000.A.hard
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_C1000.A.hard

	return( filtFuncLst )

} # getFiltLst.hard()

getFiltLst.hard4RebCnt <- function( ){
	# 전체 zhF가 아닌 특정 조건을 유지(rebCnt==0 등)하는 zhF만 골라내서 
	#	새로운 lastZoid를 기준으로 적용하기 위한 함수들.
	# 	전체 34%, filt_AK000.B.hard() 제외시 23% 손실.
	filtFuncLst <- list()
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0020.hard		# 2/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0100.A.hard	# 4/104 
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0110.A.hard	# 3/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.A.hard	# 0/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.B.hard	# 3/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AJ000.C.hard	# 1/104
	# filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.B.hard	#13/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AK000.C		# 7/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.B.hard	# 1/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.D		# 0/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AP000.E.hard	# 1/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.A		# 2/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.B		# 0/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.C		# 0/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.A.hard	# 0/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.B.hard	# 1/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.C.hard	# 1/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AS000.A.hard	# 2/104
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_C1000.A.hard	# 1/104

	return( filtFuncLst )

} # getFiltLst..hard4RebCnt()


# getFiltLst.base() 에 적용대기상태 함수들.
#	일단 나중에 걸러주기로 사용함.(20180109_A_ref.R)
getFiltLst.bench <- function(){
	
	filtFuncLst <- list()
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0020.hard	# .hard관계가 뒤바뀐상태
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_A0050	#
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AR000.C #
	filtFuncLst[[1+length(filtFuncLst)]] <- filt_AQ000.B #

	return( filtFuncLst )

} # getFiltLst.bench()

# allZoidMtx[,2:6]-allZoidMtx[,1:5]
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


filt_A0020 <- function( pEnv ){
	
	filtId="A0020";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 2
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- (cnt>0)&(cnt<6)

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0020()

filt_A0020.hard <- function( pEnv ){ # allZoidMtx 기준 3.8% 제거

	filtId="A0020.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	# 0 이거나 6 이거나.
	# 	21/789 2.5%. 그냥 hard 기준삼자.
	codeMtx <- allZoidMtx %% 2
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- (cnt>0)&(cnt<6) 

	# lastZoid와 위치까지 일치 (최대 5% 수준)
	# 	1,5 전부일치/불일치 없음
	# 	2,4 전부일치 6 불일치 5 (11/381 3%)
	# 	3   전부일치13 불일치 1 (14/278 5%)
	stdCode <- zhF[nrow(zhF),] %% 2
	mFlag <- apply( codeMtx ,1 ,function(p){!all(p==stdCode)})
	flag <- flag & mFlag

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0020.hard()


filt_A0030 <- function( pEnv ){	# 7041489
	# 8 / 789

	filtId="A0030";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 3
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- cnt < 5

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0030()

filt_A0030.hard <- function( pEnv ){	# 7041489
	# 2 / 789

	filtId="A0030.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 3
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- cnt < 6

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0030.hard()

filt_A0040 <- function( pEnv ) {
	# 6 / 789

	filtId="A0040";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 5
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- cnt < 4

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0030()

filt_A0040.hard <- function( pEnv ) {
	# 0 / 789

	filtId="A0040.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	codeMtx <- allZoidMtx %% 5
	cnt <- apply( codeMtx ,1 ,function(p){sum(p==0)})
	flag <- cnt < 5

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0040()

# 40 영역대가 3개 이상. (20/789 2.5% )
filt_A0050 <- function( pEnv ) {

	filtId="A0050";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	allCodeMtx <- allZoidMtx %/% 10
	flag <- apply( allCodeMtx ,1 ,function(p){sum(p==4)})
	flag <- flag < 3

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0050()

filt_A0050.hard <- function( pEnv ) {
	# 2.5% 급이라.. 그냥 예외없이 모두 hard 단계에 넣는다.
	filtId="A0050.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	allCodeMtx <- allZoidMtx %/% 10
	flag <- apply( allCodeMtx ,1 ,function(p){sum(p==4)})
	flag <- flag < 3

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_A0050.hard()

# zoid[1] 값이 20 이상 (18/790 2% )
filt_A0060 <- function( pEnv ) {

	filtId="A0060";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	flag <- apply( allZoidMtx ,1 ,function(p){(p[1]<20)})

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)
}	# filt_A0060()

filt_A0060.hard <- function( pEnv ) {
	# 2% 수준이라 그냥 동일하게 hard()함수로 사용.
	filtId="A0060.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	flag <- apply( allZoidMtx ,1 ,function(p){(p[1]<20)})

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)
}	# filt_A0060.hard()


#----------------------------------------------------------------
#  A0100 : lastZoid를 기준으로 잘라낸다.
filt_A0100.A <- function( pEnv ,pJumpSpan=1:8 ){
	
	filtId="A0100.A";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allCodeMtx) )
	for( nextJump in pJumpSpan ){
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

filt_A0100.A.hard <- function( pEnv ,pJumpSpan=1:2 ){
	#	pJumpSpan 기본값만 바꿨는데, 뭔가 더 좋은 방안 있을 듯.
	
	filtId="A0100.A.hard";	tStmp <- Sys.time()
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allCodeMtx) )
	for( nextJump in pJumpSpan ){
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

} # filt_A0100.A.hard()



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
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

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

filt_AJ000.A.hard <- function( pEnv ){	#
	# filt_AJ000.A() 자체가 발생 가능성 낮다. 그냥 hard

	filtId="AJ000.A.hard";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

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

} # filt_AJ000.A.hard()


#-[AJ000.B]------------------------------------------------------
#	rebLen <- getRebLen( 1:45 ,zhF )
#		filt_AJ000.A 에서 5개가 아닌 4개 일치 체크(최근 h개는 비슷한 패턴 없..)
#			총 689개 내에서 20H이내는 4쌍, 5%는 154H이내.
#			차리리 AJ000.B를 정식버전으로 하고 filt_AJ000.A를 Hard 버전으로 할까...
filt_AJ000.B <- function( pEnv ){	#

	filtId="AJ000.B";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

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

filt_AJ000.B.hard <- function( pEnv ){	#
	# 최근 80H 이내 동일한 rebLen은 제외. (20H이내는 4쌍.)

	filtId="AJ000.B.hard";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	testSpan <- 100:nrow(zhF)
	stdCodeMtx <- matrix( NA ,nrow=length(testSpan) ,ncol=ncol(zhF) )
	for( idx in 1:length(testSpan) ){
		tIdx <- testSpan[idx]
		rebLen <- getRebLen( 1:45 ,zhF[1:(tIdx-1),] )
		stdCodeMtx[idx,] <- rebLen[zhF[tIdx,]]
	}

	testSpan <- testSpan[(nrow(stdCodeMtx)-79):nrow(stdCodeMtx)]
	stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-79):nrow(stdCodeMtx),]

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

} # filt_AJ000.B.hard()


#-[AJ000.C]------------------------------------------------------
#	rebLen <- getRebLen( 1:45 ,zhF )
#		제일 작은 rebLen 3개의 값과 갯수.
#			1H 에서 동일매치 6쌍, 7H에서 동일매치 45쌍
filt_AJ000.C <- function( pEnv ){	#

	filtId="AJ000.C";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

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

filt_AJ000.C.hard <- function( pEnv ){	#
	# 2H로 범위 좁힌다.

	filtId="AJ000.C.hard";	tStmp <- Sys.time()
	rebLen <- getRebLen( 1:45 ,pEnv$zhF )
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	testSpan <- 100:nrow(zhF)
	stdCodeMtx <- matrix( NA ,nrow=length(testSpan) ,ncol=ncol(zhF) )
	for( idx in 1:length(testSpan) ){
		tIdx <- testSpan[idx]
		rebLen <- getRebLen( 1:45 ,zhF[1:(tIdx-1),] )
		stdCodeMtx[idx,] <- rebLen[zhF[tIdx,]]
	}
	testSpan <- testSpan[ (nrow(stdCodeMtx)-1):nrow(stdCodeMtx) ]
	stdCodeMtx <- stdCodeMtx[(nrow(stdCodeMtx)-1):nrow(stdCodeMtx), ]
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

} # filt_AJ000.C.hard()


#-[AK000.A]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 20 이하인 경우는 전체 5.6% 정도.. 자르자!!
filt_AK000.A <- function( pEnv ){	#
	
	filtId="AK000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- apply( allCodeMtx ,1 ,function(p){p[6]-p[1]})
	flag <- flag>20

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.A()

filt_AK000.A.hard <- function( pEnv ){	#
	# 4 / 789
	
	filtId="AK000.A.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx

	flag <- apply( allCodeMtx ,1 ,function(p){p[6]-p[1]})
	flag <- flag>15

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.A.hard()


#-[AK000.B]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이 다음에도 반복될 가능성.. 5% 미만.
filt_AK000.B <- function( pEnv ){	#
	
	filtId="AK000.B";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	flag <- (allCodeMtx[,6]-allCodeMtx[,1])!=(lastZoid[6]-lastZoid[1])

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.B()

filt_AK000.B.hard <- function( pEnv ){	#
	
	filtId="AK000.B.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	stdCode.width <- lastZoid[6]-lastZoid[1]
	stdCode.bin <- lastZoid%%2
	
	allCode.width <- pEnv$allZoidMtx[,6]-pEnv$allZoidMtx[,1]
	allCodeMtx.bin <- pEnv$allZoidMtx%%2

	flag <- rep( TRUE ,length(allCode.width) )
	for( aIdx in 1:length(allCode.width) ){
		widthAbs <- abs(stdCode.width-allCode.width[aIdx])
		matCnt <- sum( allCodeMtx.bin[aIdx,]==stdCode.bin )
		if( 0==widthAbs ){
			if( (1>=matCnt) || (matCnt<=5) ){ # (7+2)/789
				flag[aIdx] <- FALSE
			}
		} else if( 1==widthAbs ){
			if( (1>=matCnt) || (matCnt<=5) ){ # (4+2)/789
				flag[aIdx] <- FALSE
			}
		}
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.B.hard()


#-[AK000.C]------------------------------------------------------
#	zhF[,6]-zhF[,1] 이전 패턴의 반복. 4% 미만.
filt_AK000.C <- function( pEnv ){	#
	
	filtId="AK000.C";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCode <- pEnv$zhF[,6]-pEnv$zhF[,1]

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
filt_AK000.D <- function( pEnv ){
	
	filtId="AK000.D";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCode <- pEnv$zhF[,6]-pEnv$zhF[,1]

	stepDiff <- abs(stdCode[length(stdCode)]-stdCode[length(stdCode)-1])
	remVal <- c( stdCode[length(stdCode)]-stepDiff , 
					stdCode[length(stdCode)] + stepDiff
				)
	flag <- apply( allZoidMtx ,1 ,function(p){ !( (p[6]-p[1])%in%remVal ) })

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AK000.D()

#-[AL000.A]------------------------------------------------------
#	이전에 발생한 DNA의 첫번째가 다음에도 발생할 가능성. (31/391 8%)
filt_AL000.A <- function( pEnv ){	#
	
	filtId="AL000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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
	# AP000.A 의 hard 버전은 AP000.B.hard 이다.
	filtId="AP000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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

filt_AP000.B.hard <- function( pEnv ){	#
	# 9 / 789 # 바로 다음에도 똑같은 패턴 발생.
	filtId="AP000.B.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF %/% 10
	stdCodeMtx <- stdCodeMtx[nrow(stdCodeMtx),,drop=F]
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

} # filt_AP000.B.hard()


#-[AP000.C]------------------------------------------------------
#	zhF %/% 10 Quoatient.  한가지 Quoatient가 4개 이상인 경우 10/391
filt_AP000.C <- function( pEnv ){	#
	
	filtId="AP000.C";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx %/% 10

	flagLst <- apply(allCodeMtx ,1 ,table)
	flag <- sapply(flagLst,max)
	flag <- flag < 4

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.C()

filt_AP000.C.hard <- function( pEnv ){	#
	# 기본형과 똑같이 한다. 그냥 filt_AP000.C() 는 허용대상에서 제외..
	filtId="AP000.C.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	allCodeMtx <- pEnv$allZoidMtx %/% 10

	flagLst <- apply(allCodeMtx ,1 ,table)
	flag <- sapply(flagLst,max)
	flag <- flag < 4

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AP000.C.hard()


#-[AP000.D]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient패턴 Next 값.	5/288
filt_AP000.D <- function( pEnv ){	#
	
	filtId="AP000.D";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- getTblCnt( pEnv$zhF%/%10 ,pTblVal=0:4 )
	allCodeMtx <- getTblCnt( pEnv$allZoidMtx%/%10 ,pTblVal=0:4 )

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

filt_AP000.D.hard <- function( pEnv ){	#
	# 원본과 동일. 
	filtId="AP000.D.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- getTblCnt( pEnv$zhF%/%10 ,pTblVal=0:4 )
	allCodeMtx <- getTblCnt( pEnv$allZoidMtx%/%10 ,pTblVal=0:4 )

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

} # filt_AP000.D.hard()


#-[AP000.E]------------------------------------------------------
#	zhF %/% 10 Quoatient.  Quoatient그룹이 다음에도 반복.
filt_AP000.E <- function( pEnv ,pStepSize=1000000 ){
	# 20% 수준

	filtId="AP000.E";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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

		allGrpLst <- apply( allZoidMtx[stepSpan,,drop=F] ,1 ,function(p){ getQGrp(p,0:4) } )
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
	# 6/391
	filtId="AP000.E.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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

		allGrpLst <- apply( allZoidMtx[stepSpan,,drop=F] ,1 ,function(p){ getQGrp(p,0:4) } )
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
filt_AQ000.A <- function( pEnv ){	# 전체기준 2.3%제거.
	
	filtId="AQ000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]

	flag <- apply( allZoidMtx ,1 ,function(p){ sum(lastZoid%in%p) } )
	flag <- flag<3

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AQ000.A()

#-[AQ000.B]------------------------------------------------------
#	2개 재발 DNA가 나중에도 2개로 재발될 가능성 (10/790 1.2%)
filt_AQ000.B <- function( pEnv ){	#
	# 1.2% 수준이라 이것 자체로도 hard 사용가능성은 있으나..
	filtId="AQ000.B";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

	rebLst <- list()
	for( hIdx in 2:nrow(zhF) ){
		comDna <- intersect( zhF[hIdx,] ,zhF[(hIdx-1),] )
		if( 2==length(comDna) ){
			rebLst[[1+length(rebLst)]] <- comDna
		}
	}
	stdCodeMtx <- do.call( rbind ,rebLst )
	matMtx <- scanSameRow(stdCodeMtx)
	stdCodeMtx <- stdCodeMtx[-matMtx[,2],]

	lastZoid <- zhF[nrow(zhF),]
	# lastZoid 내에 있는 조합만 골라내자.
	stdCodeMtx <- stdCodeMtx[
						apply( stdCodeMtx ,1 ,function(p){ 2==sum(p%in%lastZoid) } ) 
						, ,drop=F
					]

	flag <- rep( TRUE ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		for( sIdx in seq_len(nrow(stdCodeMtx)) ){
			cnt <- sum( stdCodeMtx[sIdx,] %in% allZoidMtx[aIdx,] )
			if( 2==cnt ){
				flag[aIdx] <- FALSE
				break
			}
		}
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AQ000.B()

#-[AQ000.C]------------------------------------------------------
#	최근 H 개 내 재발값 좌표일치. 
filt_AQ000.C <- function( pEnv ){
	# 측정치 2.5% 수준. 그대로 .hard로 사용해도 될 듯.
	
	filtId="AQ000.C";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- zhF[nrow(zhF),]

	stdCodeLst <- list()
	for( hIdx in 2:nrow(zhF) ){
		codeObj <- list( hIdx=hIdx )
		rebVal <- intersect( zhF[hIdx-1,] ,zhF[hIdx,] )
		if( 1==length(rebVal) ){
			rebValQuo <- rebVal %/% 10
			codeObj$info <- c( rebVal
								,which(zhF[hIdx-1,]==rebVal)
								,which(zhF[hIdx  ,]==rebVal)
								,sum( (zhF[hIdx-1,]%/%10)==rebValQuo )
								,sum( (zhF[hIdx  ,]%/%10)==rebValQuo )
							)
			codeObj$nextQuo <- zhF[hIdx,(zhF[hIdx,]%/%10)==rebValQuo] # .hard 함수용
			stdCodeLst[[1+length(stdCodeLst)]] <- codeObj
		}
	}
	stdCodeMtx <- do.call( rbind ,lapply(stdCodeLst,function(p){p$info}) )

	sel.hIdx <- (9<stdCodeMtx[,1]) & (stdCodeMtx[,1]<40) # 10~39까지만 사용.
	stdCodeMtx <- stdCodeMtx[sel.hIdx,,drop=F]
	stdCode.hIdx <- do.call( c ,lapply(stdCodeLst[sel.hIdx],function(p){p$hIdx}) )
	rownames(stdCodeMtx) <- stdCode.hIdx
	colnames(stdCodeMtx) <- c("val","pos.p","pos.c","quo.p","quo.c") 
		# 값, 이전/이후 위치&그룹크기

	# 동작확인.
	lastZoid.Quo <- lastZoid %/% 10
	keyColName <- c("val","pos.p","quo.p")
	chkVal <- c(0,0,0)	;names(chkVal)<- keyColName
	chkLst <- list()
	for( cIdx in 1:length(lastZoid) ){
		chkVal[] <- c( lastZoid[cIdx] ,cIdx ,sum(lastZoid.Quo==lastZoid.Quo[cIdx]) )
		cFlag <- apply(stdCodeMtx[,keyColName] ,1 ,function(p){all(p==chkVal)} )
		chkLst[[1+length(chkLst)]] <- stdCodeMtx[cFlag,,drop=F] # 없으면 nrow()==0
	}

	flag <- rep( 0 ,nrow(allZoidMtx) ) # 일치 발견 시, lastZoid의 포지션 값.(pos.c)
	for( aIdx in 1:nrow(allZoidMtx) ){
		val <- intersect(lastZoid,allZoidMtx[aIdx,])
		if( 1 != length(val) ){
			next
		}

		pos.p <- which(lastZoid==val)
		if( 0==nrow(chkLst[[pos.p]]) ){
			next
		}
		
		compVal <- c( which(allZoidMtx[aIdx,]==val) 
						,sum((allZoidMtx[aIdx,]%/%10)==lastZoid.Quo[pos.p]) 
					)
		for( rIdx in 1:nrow(chkLst[[pos.p]]) ){
			if( all(chkLst[[pos.p]][rIdx,c("pos.c","quo.c")]==compVal) ){
				flag[aIdx] <- pos.p
			}
		}
	}
	flag <- flag==0
	

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AQ000.C()



#-[AR000.A]------------------------------------------------------
#	remainder 근거리 재발. (3H 이내 재발 31/787)
filt_AR000.A <- function( pEnv ){	#
	
	filtId="AR000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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

filt_AR000.A.hard <- function( pEnv ){	#
	#	2 / 789
	filtId="AR000.A.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF %% 10
	allCodeMtx <- pEnv$allZoidMtx %% 10

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

} # filt_AR000.A.hard()


#-[AR000.B]------------------------------------------------------
#	remainder(%%10) 패턴 재발 (nextVal)	17% 탈락.
filt_AR000.B <- function( pEnv ){	#
	
	filtId="AR000.B";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
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

filt_AR000.B.hard <- function( pEnv ){	#
	
	filtId="AR000.B.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF %% 10
	allZoidMtx <- pEnv$allZoidMtx

	flag <- rep( TRUE ,nrow(allZoidMtx) )
	ptn <- getPtnRebGrp( stdCodeMtx ,pNextJump=1 )
	if( !is.null(ptn) ){
		flag <- sapply( ptn$filt( allZoidMtx %% 10 ,pLevel=4 )
						,function(p){p$survive} 
					)
	}

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AR000.B.hard()

#-[AR000.C]------------------------------------------------------
#	remainder(%%10) 패턴이 바로 다음에도 재발.
filt_AR000.C <- function( pEnv ){	#
	# 3개 동일이 16/789(2%), 4개 이상은 없음. .hard 함수 전용으로 쓰자.
	filtId="AR000.C";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCode <- zhF[nrow(zhF),] %% 10

	flag <- apply( allZoidMtx %% 10 ,1 ,function(p){sum(p==stdCode)} )
	flag <- flag < 3

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AR000.C()

filt_AR000.C.hard <- function( pEnv ){	#
	# 3개 동일이 16/789(2%), 4개 이상은 없음.
	filtId="AR000.C.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCode <- zhF[nrow(zhF),] %% 10

	flag <- apply( allZoidMtx %% 10 ,1 ,function(p){sum(p==stdCode)} )
	flag <- flag < 3

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AR000.C.hard()


#-[AS000.A]------------------------------------------------------
#	연이은 DNA코드가 다음에도 같은 간격으로 재발 38/787
filt_AS000.A <- function( pEnv ){	
	# 29/389  7.5%	
	filtId="AS000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( 0 ,nrow(allCodeMtx) )
	for( aIdx in 1:nrow(allCodeMtx) ){
		rebIdx.1 <- which( lastZoid %in% allCodeMtx[aIdx,] )
		if( 2>length(rebIdx.1) ){
			next
		}
		rebIdx.2 <- which( allCodeMtx[aIdx,] %in% lastZoid )
		rebLen <- length(rebIdx.1)
		widthF <- ( rebIdx.1[2:rebLen]-rebIdx.1[1:(rebLen-1)] ) ==
					( rebIdx.2[2:rebLen]-rebIdx.2[1:(rebLen-1)] )
		if( all(widthF) ){ # all()이 더 엄하긴 한데...
			flag[aIdx] <- rebIdx.1[1]
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AS000.A()

filt_AS000.A.hard <- function( pEnv ){	#
	# 7/389  2%
	filtId="AS000.A.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	allCodeMtx <- pEnv$allZoidMtx

	flag <- rep( 0 ,nrow(allCodeMtx) )
	for( aIdx in 1:nrow(allCodeMtx) ){
		rebIdx.1 <- which( lastZoid %in% allCodeMtx[aIdx,] )
		if( 3>length(rebIdx.1) ){
			next	# 3개 이상발생은 AQ000.A에서 제거되었다.
		}
		# flag[aIdx] <- 2
		rebIdx.2 <- which( allCodeMtx[aIdx,] %in% lastZoid )
		if( (rebIdx.1[2]-rebIdx.1[1])==(rebIdx.2[2]-rebIdx.2[1]) ){
			flag[aIdx] <- rebIdx.1[1]
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_AS000.A.hard()

#-[C0000.A]------------------------------------------------------
#	zhF[,2:6]-zhF[,1:5] 
filt_C0000.A <- function( pEnv ){	#
	#	22 / 389
	#	백만개 당 23분 정도 소요.

	filtId="C0000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF[,2:6] - pEnv$zhF[,1:5]
	allCodeMtx <- pEnv$allZoidMtx[,2:6,drop=F] - pEnv$allZoidMtx[,1:5,drop=F]

	flag <- rep(0,nrow(allCodeMtx))
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

filt_C0000.A.hard <- function( pEnv ){	#
	#	0 / 389

	filtId="C0000.A.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCodeMtx <- pEnv$zhF[,2:6] - pEnv$zhF[,1:5]
	allCodeMtx <- pEnv$allZoidMtx[,2:6,drop=F] - pEnv$allZoidMtx[,1:5,drop=F]

	flag <- rep(0,nrow(allCodeMtx))
	for( aIdx in 1:nrow(allCodeMtx) ){
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum(stdCodeMtx[sIdx,]!=allCodeMtx[aIdx,])
			if( 0 >= cnt ){
				flag[aIdx] <- sIdx
				break
			}	
		}
	}
	flag <- flag == 0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_C0000.A.hard()


#-[C1000.A]------------------------------------------------------
#	abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])
filt_C1000.A <- function( pEnv ){
	
	filtId="C1000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- zhF[nrow(zhF),]
	stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])

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

filt_C1000.A.hard <- function( pEnv ){
	#	1개 이하로 틀린 경우
	
	filtId="C1000.A.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	lastZoid <- zhF[nrow(zhF),]
	stdCodeMtx <- abs(zhF[1:(nrow(zhF)-1),] - zhF[2:nrow(zhF),])

	flag <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		chkVal <- abs(allZoidMtx[aIdx,]-lastZoid)
		for( sIdx in 1:nrow(stdCodeMtx) ){
			cnt <- sum(chkVal==stdCodeMtx[sIdx,])
			if( 5<=cnt ){
				flag[aIdx] <- sIdx
				break
			}
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_C1000.A.hard()


#-[D0000.A]------------------------------------------------------
#	rebMtx (7*6)
filt_D0000.A <- function( pEnv ,pHeight=7 ){

	filtId="D0000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCode <- zhF[nrow(zhF),] %% 10

	rowSpan <- (nrow(zhF)-pHeight+1):nrow(zhF)
	stdMtx <- zhF[rowSpan-10,]	# 바로 이전 맵을 체크하는 것은 너무 유사성이 많아서.
	allMtx <- zhF[rowSpan   ,]
	lastZoid <- zhF[nrow(zhF),]

	remValLst <- lapply( lastZoid ,function(p){integer(0)} )
	for( rIdx in 1:pHeight ){
		for( cIdx in 1:length(lastZoid) ){
			lzIdx <- which(lastZoid==stdMtx[rIdx,cIdx])	# Last zoid col index
			if( 0<length(lzIdx) ){
				remValLst[[lzIdx]] <- c( remValLst[[lzIdx]] ,allMtx[rIdx,cIdx] )
			}
		}
	}
	scanCol <- which( sapply( remValLst ,length ) > 0 )	# 모든 컬럼이 아닌, 배제대상 있는 것만.

	flag <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		for( cIdx in scanCol ){
			#	하나만 일치해도 예외대상.
			if( allZoidMtx[aIdx,cIdx] %in% remValLst[[cIdx]] ){
				flag[aIdx] <- cIdx
			}
		}
	}
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_D0000.A()

filt_D0000.A.hard <- function( pEnv ,pHeight=7 ,pThld=2 ){

	filtId="D0000.A.hard";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF
	stdCode <- zhF[nrow(zhF),] %% 10

	rowSpan <- (nrow(zhF)-pHeight+1):nrow(zhF)
	stdMtx <- zhF[rowSpan-10,]	# 바로 이전 맵을 체크하는 것은 너무 유사성이 많아서.
	allMtx <- zhF[rowSpan   ,]
	lastZoid <- zhF[nrow(zhF),]

	remValLst <- lapply( lastZoid ,function(p){integer(0)} )
	for( rIdx in 1:pHeight ){
		for( cIdx in 1:length(lastZoid) ){
			lzIdx <- which(lastZoid==stdMtx[rIdx,cIdx])	# Last zoid col index
			if( 0<length(lzIdx) ){
				remValLst[[lzIdx]] <- c( remValLst[[lzIdx]] ,allMtx[rIdx,cIdx] )
			}
		}
	}
	scanCol <- which( sapply( remValLst ,length ) > 0 )	# 모든 컬럼이 아닌, 배제대상 있는 것만.
	
	chkFlag <- rep( 0 ,length(lastZoid) )

	flag <- rep( 0 ,nrow(allZoidMtx) )
	for( aIdx in 1:nrow(allZoidMtx) ){
		chkFlag[] <- 0
		for( cIdx in scanCol ){
			if( allZoidMtx[aIdx,cIdx] %in% remValLst[[cIdx]] ){
				chkFlag[cIdx] <- cIdx
			}
		}
		flag[aIdx] <- sum(chkFlag>1)	# 매치 갯수.
	}
	flag <- flag<pThld	# pThld보다 작은 수의 매치만 허용.

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_D0000.A.hard()


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
