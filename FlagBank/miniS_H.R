source("gE_H.R")	# 유전 알고리즘을 위한 Generic Function 선언 받아오기.

getMiniS <- function( pCd.h ,pCd.std ,fltLst ){
		# fltObj 기초정보 생성
		# fltObj별 예측자료 생성
		# 각 Cd별 ScoreMtx
		# 총합 및 순위측정.
	}

# Generic Function
mS.predr <- function( pObj ,... ){	UseMethod("mS.predr")	}
mS.analyze <- function( pObj ,... ){	UseMethod("mS.analyze")	}
mS.score <- function( pObj ,... ){	UseMethod("mS.score")	}





# ====================================================================
#	mSfltS04 : 현재값이 과거(N)에 발생한 횟수.
mS.getFltDefS04 <- function( pN=3 ,pFlagId="std" ,pUseSeqPCum=F ){
		rObj <- list( idStr=sprintf("mSfltS04f%s_%d_%s",pFlagId,pN,ifelse(pUseSeqPCum,"cp","np")) )
		class(rObj) <- "mSfltS04"
		rObj$pN <- pN
		rObj$flagId <- pFlagId
		rObj$useSeqPCum = pUseSeqPCum
		return( rObj )
	} # mS.getFltDefS04( pBase )

mS.predr.mSfltS04 <- function( pObj ,pPbg ){

		pb <- pPbg[[pObj$flagId]]
		sRange <- (pObj$pN+1):length(pb$cd.h)
		mtx <- mapply( function(p){return(pb$cd.h[(p-pObj$pN):p])} ,sRange )
		rebCnt <- apply( mtx ,2 ,function(p){return(sum(p[1:pObj$pN]==p[pObj$pN+1]))} )

		sObj <- k.seq( rebCnt )
		seq.max <- max(sObj$hSeqCntMtx[,"seqCnt"])
		seqPMtx <- matrix( NA ,ncol=length(sObj$cdRange) ,nrow=seq.max )
		colnames(seqPMtx) <- sObj$cdRange
        if( pObj$useSeqPCum ){
            seqPMtx[,colnames(sObj$seqPMtx.cum)] <- sObj$seqPMtx.cum
        } else {
            seqPMtx[,colnames(sObj$seqPMtx)] <- sObj$seqPMtx
        }

		seq.last <- sObj$seqCntMtx[nrow(sObj$seqCntMtx),]
		
		rObj <- list( seqPMtx=seqPMtx )
		rObj$lastCd <- pb$cd.h[ (length(pb$cd.h)-pObj$pN+1):length(pb$cd.h) ]
		rObj$seq.val <- seq.last["val"]
		rObj$seq.cnt <- seq.last["cnt"] # 같은 값이 나타났을 때의 cnt

		rObj$seq.prob <- ifelse( seq.last["cnt"]>nrow(seqPMtx) ,0 # 기존 연속발생 최대치 넘으면 확률 0
										,seqPMtx[seq.last["cnt"],as.character(seq.last["val"])] 
									)
		return( rObj )

	} # mS.predr.mSfltS04

mS.analyze.mSfltS04 <- function( pObj ,pCd.x ){

		flag <- pPbg[[pObj$flagId]]$nextCd( pCd.x )
		cnt <- mapply( function(p){sum(pObj$predr$lastCd==p)} ,flag )
		cd.a <- ifelse( pObj$predr$seq.val==cnt ,pObj$predr$seq.prob ,(100-pObj$predr$seq.prob) )
		names(cd.a) <- as.character(pCd.x)

		rObj <- list( cd.a=cd.a )
		return( rObj )
		
	} # mS.analyze.mSfltS04

mS.score.mSfltS04 <- function( pObj ,pAnaObj ){
		# 확률값을 그대로 score로 처리.
		cd.s <- pAnaObj$cd.a
		cd.s[is.na(cd.s)] <- 0	# NA는 0% 처리

		# cd.order <- order( cd.s ,decreasing=T ) # 동일수준 값에 대한 처리문제로 인해 폐기.
		
		cd.order <- rank( cd.s ,na.last=F ,ties.method="average" )
		cd.order <- length(cd.s) - cd.order +1
		cd.order[cd.order>length(cd.s)] <- length(cd.s) 
			# 동일수준 값을 가진 cd들의 석차가 평균으로 계산하기 때문에
			# cd.order가 최대값을 넘을 수 있는 가능성에 대한 처리.
		
		cd.order <- cd.order*100/length(cd.order) # 상위 몇 % 기준.
		cd.order[is.na(pAnaObj$cd.a)] <- NA
		names( cd.order ) <- names( cd.s )

		rObj <- list( cd.s=cd.s )
		rObj$cd.order <- cd.order
		return( rObj )
	} # mS.score.mSfltS04


# ====================================================================
#	mSfltS03 : 배수의 연속
mS.getFltDefS03 <- function( pBase=3 ,pFlagId="std" ,pUseSeqPCum=F ){
		rObj <- list( idStr=sprintf("mSfltS03f%s_%d_%s",pFlagId,pBase,ifelse(pUseSeqPCum,"cp","np")) )
   		class(rObj) <- "mSfltS03"
		rObj$pBase <- pBase
		rObj$flagId <- pFlagId
		rObj$useSeqPCum = pUseSeqPCum
		return( rObj )
	} # mS.getFltDefS03( pBase )

mS.predr.mSfltS03 <- function( pObj ,pPbg ){

		pb <- pPbg[[pObj$flagId]]
		sObj <- k.seq( pb$cd.h %/% pObj$pBase )
		seq.max <- max(sObj$hSeqCntMtx[,"seqCnt"])
		seqPMtx <- matrix( NA ,ncol=length(sObj$cdRange) ,nrow=seq.max )
		colnames(seqPMtx) <- sObj$cdRange
        if( pObj$useSeqPCum ){
            seqPMtx[,colnames(sObj$seqPMtx.cum)] <- sObj$seqPMtx.cum
        } else {
            seqPMtx[,colnames(sObj$seqPMtx)] <- sObj$seqPMtx
        }

		# 마지막 코드값과 연속 수.
		nr <- nrow(sObj$hSeqCntMtx)
		lastSeq <- sObj$hSeqCntMtx[nr,]

		rObj <- list( seqPMtx=seqPMtx )
		rObj$lastCd <- lastSeq["val"]
		rObj$lastSeq <- lastSeq["seqCnt"]	# 마지막 코드의 연속발생상태
											# 즉, 추가코드가 마지막 코드와 동일하다면
											# rObj$lastSeq+1 값을 가지고
											# seqPMtx 테이블 row값과 비교해야 한다.
		rObj$lastSeq.prob <- ifelse( (lastSeq["seqCnt"]+1)>nrow(seqPMtx) ,0 
										,seqPMtx[lastSeq["seqCnt"]+1,as.character(lastSeq["val"])] 
									)
				# 추가 코드가 마지막 코드와 동일할 경우의 확률.
		return( rObj )
	} # mS.predr.mSfltS03

mS.analyze.mSfltS03 <- function( pObj ,pCd.x ){

		flag <- pPbg[[pObj$flagId]]$nextCd( pCd.x )
		cd.a <- ifelse( pObj$predr$lastCd==(flag%/%pObj$pBase) 
						,pObj$predr$lastSeq.prob ,(100-pObj$predr$lastSeq.prob) 
					)
		names(cd.a) <- as.character(pCd.x)

		rObj <- list( cd.a=cd.a )
		return( rObj )
		
	} # mS.analyze.mSfltS03

mS.score.mSfltS03 <- function( pObj ,pAnaObj ){
		# 확률값을 그대로 score로 처리.
		cd.s <- pAnaObj$cd.a
		cd.s[is.na(cd.s)] <- 0	# NA는 0% 처리

		# cd.order <- order( cd.s ,decreasing=T ) # 동일수준 값에 대한 처리문제로 인해 폐기.
		
		cd.order <- rank( cd.s ,na.last=F ,ties.method="average" )
		cd.order <- length(cd.s) - cd.order +1
		cd.order[cd.order>length(cd.s)] <- length(cd.s) 
			# 동일수준 값에 석차가 평균으로 계산하기 때문에
			# 윗 계산에서 최대값을 넘을 수 있는 가능성에 대한 처리.
		
		cd.order <- cd.order*100/length(cd.order) # 상위 몇 % 기준.
		cd.order[is.na(pAnaObj$cd.a)] <- NA
		names( cd.order ) <- names( cd.s )

		rObj <- list( cd.s=cd.s )
		rObj$cd.order <- cd.order
		return( rObj )
	} # mS.score.mSfltS03

	
# ====================================================================
#	mSfltS02 : 나머지(%%)의 연속
mS.getFltDefS02 <- function( pBase=3 ,pFlagId="std" ,pUseSeqPCum=F ){
		rObj <- list( idStr=sprintf("mSfltS02f%s_%d_%s",pFlagId,pBase,ifelse(pUseSeqPCum,"cp","np")) )
		class(rObj) <- "mSfltS02"
		rObj$pBase <- pBase
		rObj$cdRange <- 0:(pBase-1)
		rObj$flagId <- pFlagId
		rObj$useSeqPCum = pUseSeqPCum
		return( rObj )
	} # mS.getFltDefS02( pBase )

mS.predr.mSfltS02 <- function( pObj ,pPbg ){

		pb <- pPbg[[pObj$flagId]]
		sObj <- k.seq( pb$cd.h %% pObj$pBase )
		seq.max <- max(sObj$hSeqCntMtx[,"seqCnt"])
		seqPMtx <- matrix( NA ,ncol=length(pObj$cdRange) ,nrow=seq.max )
		colnames(seqPMtx) <- pObj$cdRange
        if( pObj$useSeqPCum ){
            seqPMtx[,colnames(sObj$seqPMtx.cum)] <- sObj$seqPMtx.cum
        } else {
            seqPMtx[,colnames(sObj$seqPMtx)] <- sObj$seqPMtx
        }

		# 마지막 코드값과 연속 수.
		nr <- nrow(sObj$hSeqCntMtx)
		lastSeq <- sObj$hSeqCntMtx[nr,]

		rObj <- list( seqPMtx=seqPMtx )
		rObj$lastCd <- lastSeq["val"]
		rObj$lastSeq <- lastSeq["seqCnt"]	# 마지막 코드의 연속발생상태
											# 즉, 추가코드가 마지막 코드와 동일하다면
											# rObj$lastSeq+1 값을 가지고
											# seqPMtx 테이블 row값과 비교해야 한다.
		rObj$lastSeq.prob <- ifelse( (lastSeq["seqCnt"]+1)>nrow(seqPMtx) ,0 # 기존 연속발생 최대치 넘으면 확률 0
										,seqPMtx[lastSeq["seqCnt"]+1,as.character(lastSeq["val"])] 
									)
				# 추가 코드가 마지막 코드와 동일할 경우의 확률.
		return( rObj )
	} # mS.predr.mSfltS02

mS.analyze.mSfltS02 <- function( pObj ,pCd.x ){

		flag <- pPbg[[pObj$flagId]]$nextCd( pCd.x )
		cd.a <- ifelse( pObj$predr$lastCd==(flag%%pObj$pBase) ,pObj$predr$lastSeq.prob ,(100-pObj$predr$lastSeq.prob) )
		names(cd.a) <- as.character(pCd.x)

		rObj <- list( cd.a=cd.a )
		return( rObj )
		
	} # mS.analyze.mSfltS02

mS.score.mSfltS02 <- function( pObj ,pAnaObj ){
		# 확률값을 그대로 score로 처리.
		cd.s <- pAnaObj$cd.a
		cd.s[is.na(cd.s)] <- 0	# NA는 0% 처리

		# cd.order <- order( cd.s ,decreasing=T ) # 동일수준 값에 대한 처리문제로 인해 폐기.
		
		cd.order <- rank( cd.s ,na.last=F ,ties.method="average" )
		cd.order <- length(cd.s) - cd.order +1
		cd.order[cd.order>length(cd.s)] <- length(cd.s) 
			# 동일수준 값에 석차가 평균으로 계산하기 때문에
			# 윗 계산에서 최대값을 넘을 수 있는 가능성에 대한 처리.
		
		cd.order <- cd.order*100/length(cd.order) # 상위 몇 % 기준.
		cd.order[is.na(pAnaObj$cd.a)] <- NA
		names( cd.order ) <- names( cd.s )

		rObj <- list( cd.s=cd.s )
		rObj$cd.order <- cd.order
		return( rObj )
	} # mS.score.mSfltS02



# ====================================================================
#	mSfltS01 : 동일 코드의 연속
mS.getFltDefS01 <- function( pFlagId="std" ,pUseSeqPCum=F ){
		rObj <- list( idStr=sprintf("mSfltS01f%s_%s",pFlagId,ifelse(pUseSeqPCum,"cp","np")) )
		class(rObj) <- "mSfltS01"
		rObj$flagId <- pFlagId
		rObj$useSeqPCum = pUseSeqPCum
		return( rObj )
	} # mS.getFltDefS01( pPbAttrName )

mS.predr.mSfltS01 <- function( pObj ,pPbg ){

		pb <- pPbg[[pObj$flagId]]
		sObj <- k.seq( pb$cd.h )
		seq.max <- max(sObj$hSeqCntMtx[,"seqCnt"])
		seqPMtx <- matrix( NA ,ncol=length(pb$cdRange) ,nrow=seq.max )
			# row : 연속 수. 1,1,1 이면 2연속이 발생한 상황.
		colnames(seqPMtx) <- pb$cdRange
        if( pObj$useSeqPCum ){
            seqPMtx[,colnames(sObj$seqPMtx.cum)] <- sObj$seqPMtx.cum
        } else {
            seqPMtx[,colnames(sObj$seqPMtx)] <- sObj$seqPMtx
        }

		# 마지막 코드값과 연속 수.
		nr <- nrow(sObj$hSeqCntMtx)
		lastSeq <- sObj$hSeqCntMtx[nr,]

		rObj <- list( seqPMtx=seqPMtx )
		rObj$lastCd <- lastSeq["val"]
		rObj$lastSeq <- lastSeq["seqCnt"]	# 마지막 코드의 연속발생상태
											# 즉, 추가코드가 마지막 코드와 동일하다면
											# rObj$lastSeq+1 값을 가지고
											# seqPMtx 테이블 row값과 비교해야 한다.
		rObj$lastSeq.prob <- ifelse( (lastSeq["seqCnt"]+1)>nrow(seqPMtx) ,0 
										,seqPMtx[lastSeq["seqCnt"]+1,as.character(lastSeq["val"])]
									)
				# 추가 코드가 마지막 코드와 동일할 경우의 확률.
		return( rObj )
	} # mS.predr.mSfltS01

mS.analyze.mSfltS01 <- function( pObj ,pCd.x ){

		flag <- pPbg[[pObj$flagId]]$nextCd( pCd.x )
		cd.a <- ifelse( pObj$predr$lastCd==flag ,pObj$predr$lastSeq.prob ,(100-pObj$predr$lastSeq.prob) )
		names(cd.a) <- as.character(pCd.x)

		rObj <- list( cd.a=cd.a )
		return( rObj )
		
	}

mS.score.mSfltS01 <- function( pObj ,pAnaObj ){
		# 확률값을 그대로 score로 처리.
		cd.s <- pAnaObj$cd.a
		cd.s[is.na(cd.s)] <- 0	# NA는 0% 처리

		# cd.order <- order( cd.s ,decreasing=T ) # 동일수준 값에 대한 처리문제로 인해 폐기.
		
		cd.order <- rank( cd.s ,na.last=F ,ties.method="average" )
		cd.order <- length(cd.s) - cd.order +1
		cd.order[cd.order>length(cd.s)] <- length(cd.s) 
			# 동일수준 값에 석차가 평균으로 계산하기 때문에
			# 윗 계산에서 최대값을 넘을 수 있는 가능성에 대한 처리.
		
		cd.order <- cd.order*100/length(cd.order) # 상위 몇 % 기준.
		cd.order[is.na(pAnaObj$cd.a)] <- NA
		names( cd.order ) <- names( cd.s )

		rObj <- list( cd.s=cd.s )
		rObj$cd.order <- cd.order
		return( rObj )
	} # mS.score.mSfltS01

# ====================================================================
#	mSflt001
mS.getFltDef001 <- function( pFlagId="std" ){
		rObj <- list( idStr=sprintf("mSflt001f%s",pFlagId) )
		class(rObj) <- "mSflt001"
		rObj$flagId <- pFlagId
		return( rObj )
	} # mS.getFltDef001( )

#	- pPb : PredrBase
mS.predr.mSflt001 <- function( pObj ,pPbg ){

		pb <- pPbg[[pObj$flagId]]
		fq <- table(pb$cd.h)
		fq.std <- k.standardize( fq )
		cdProb <- rep( 0 ,length(pb$cdRange) )
		names(cdProb) <- pb$cdRange
		cdProb[names(fq.std)] <- fq.std
		
		rObj <- list( cd=pb$cdRange )
		rObj$cdProb		<- cdProb
		rObj$cdOrder 	<- order(cdProb,decreasing=T)*100/length(cdProb)
		return( rObj )
	} # mS.predr.mSflt001

#	
mS.analyze.mSflt001 <- function( pObj ,pCd.x ){

		pb <- pPbg[[pObj$flagId]]
		cdProb.name	<- names(pObj$predr$cdProb)
		cd.a <- mapply(function(p){
								# 기존에 발생한 적 없다면 0% 처리.
								return(ifelse(p %in% cdProb.name,pObj$predr$cdProb[p],0))
							}
						,as.character( pb$nextCd(pCd.x) )
					)
		names(cd.a) <- as.character(pCd.x)

		rObj <- list( cd.a=cd.a )
		return( rObj )
	} # mS.analyze.mSflt001

mS.score.mSflt001 <- function( pObj ,pAnaObj ){
		# 확률값을 그대로 score로 처리.
		cd.s <- pAnaObj$cd.a

		# cd.order <- order( cd.s ,decreasing=T ) # 동일수준 값에 대한 처리문제로 인해 폐기.
		cd.order <- rank( cd.s ,na.last=F ,ties.method="average" )
		cd.order <- length(cd.s) - cd.order +1
		cd.order[cd.order>length(cd.s)] <- length(cd.s) 
			# 동일수준 값에 석차가 평균으로 계산하기 때문에
			# 윗 계산에서 최대값을 넘을 수 있는 가능성에 대한 처리.
		
		cd.order <- cd.order*100/length(cd.order) # 상위 몇 % 기준.
		cd.order[is.na(pAnaObj$cd.a)] <- NA
		names( cd.order ) <- names( pAnaObj$cd.a )

		rObj <- list( cd.s=cd.s )
		rObj$cd.order <- cd.order
		return( rObj )
	} # mS.score.mSflt001


# =====================================================================
# mS.getPredrBase
#
mS.getPredrBaseGrp <- function( pCd.h ){
		rObj <- list()
		
		pb <- mS.getPredrBase(pCd.h)
		rObj[[pb$idStr]] <- pb
		pb <- mS.getPredrBase.diff(pCd.h)
		rObj[[pb$idStr]] <- pb
		pb <- mS.getPredrBase.div(pCd.h)
		rObj[[pb$idStr]] <- pb
		pb <- mS.getPredrBase.left(pCd.h)
		rObj[[pb$idStr]] <- pb

		return( rObj )
	}

# ---------------------------------------------------------------------------
# mS.getPredrBase 의 자식요소.
mS.getPredrBase <- function( pCd.h ){
		
		lnth <- length( pCd.h )
		r <- range(pCd.h)
		cdRange <- seq( r[1] ,r[2] ,1 )
		names(cdRange) <- cdRange
		h.l <- length( pCd.h )
		f.l <- length( pCd.h )
		
		rObj <- list( idStr="std" ,cd.h=pCd.h )
		rObj$cdRange <- cdRange
		rObj$hLen <- lnth
		rObj$lastCd <- pCd.h[lnth]
		rObj$nextCd <- function( p.x ){
				return( p.x )
			}
		
		return( rObj )
	}

# ---------------------------------------------------------------------------
# mS.getPredrBase 의 자식요소.
# 	diff : 이전 history와의 차이 절대값.
mS.getPredrBase.diff <- function( pCd.h ){

		lnth <- length( pCd.h )
		flag <- abs( pCd.h[1:(lnth-1)] - pCd.h[2:lnth] )
		names(flag) <- names(pCd.h[2:lnth])
		
		r <- range(flag)
		cdRange <- seq( r[1] ,r[2] ,1 )
		names(cdRange) <- cdRange
		
		rObj <- list( idStr="diff" ,cd.h=flag )
		rObj$cdRange <- cdRange
		rObj$hLen <- lnth
		rObj$lastCd <- pCd.h[lnth]
		rObj$nextCd <- function( p.x ){
				return( abs(p.x-rObj$lastCd) )
			}

		return( rObj )
	}

# ---------------------------------------------------------------------------
# mS.getPredrBase 의 자식요소.
# 	div : 이전 history를 현재 값으로 나누기.
mS.getPredrBase.div <- function( pCd.h ){

		lnth <- length( pCd.h )
		flag <- pCd.h[1:(lnth-1)] %/% pCd.h[2:lnth]
		names(flag) <- names(pCd.h[2:lnth])
		
		r <- range(flag)
		cdRange <- seq( r[1] ,r[2] ,1 )
		names(cdRange) <- cdRange
		
		rObj <- list( idStr="div" ,cd.h=flag )
		rObj$cdRange <- cdRange
		rObj$hLen <- lnth
		rObj$lastCd <- pCd.h[lnth]
		rObj$nextCd <- function( p.x ){
				return( abs(rObj$lastCd %/% ifelse(p.x==0,NA,p.x) ) )
			}

		return( rObj )
	}

# ---------------------------------------------------------------------------
# mS.getPredrBase 의 자식요소.
# 	left : 이전 history를 현재 값으로 나누기.
mS.getPredrBase.left <- function( pCd.h ){

		lnth <- length( pCd.h )
		flag <- pCd.h[1:(lnth-1)] %% pCd.h[2:lnth]
		names(flag) <- names(pCd.h[2:lnth])

		r <- range(flag)
		cdRange <- seq( r[1] ,r[2] ,1 )
		names(cdRange) <- cdRange
		
		rObj <- list( idStr="left" ,cd.h=flag )
		rObj$cdRange <- cdRange
		rObj$hLen <- lnth
		rObj$lastCd <- pCd.h[lnth]
		rObj$nextCd <- function( p.x ){
				return( abs(rObj$lastCd %% ifelse(p.x==0,NA,p.x)) )
			}

		return( rObj )
	}

# ---------------------------------------------------------------------------
# mS.getPredrBase 의 자식요소.
# 	freq : 발생 빈도.
mS.getPredrBase.freq <- function( pCd.h ){

		lnth <- length( pCd.h )
		r <- range(pCd.h)
		flag <- seq( r[1] ,r[2] ,1 )
		names(flag) <- flag
		flag[] <- 0
		
		tbl <- table(pCd.h)
		flag[names(tbl)] <- tbl
		flag[] <- round(rank(max(flag) - flag))

		cdRange <- seq( r[1] ,r[2] ,1 ) # 1~꼴찌까지의 순위이므로..
		names(cdRange) <- cdRange
		
		rObj <- list( idStr="freq" ,cd.h=flag )
		rObj$cdRange <- cdRange
		rObj$hLen <- lnth
		rObj$lastCd <- pCd.h[lnth]
		rObj$rank.end <- cdRange[length(cdRange)] # 가장 꼴찌 순위.
		rObj$nextCd <- function( p.x ){
				p.x.name <- as.character(p.x)
				return(	ifelse( p.x.name %in% names(flag) 
							,flag[p.x.name]
							,rObj$rank.end
						)
					)
			}

		return( rObj )
	}

