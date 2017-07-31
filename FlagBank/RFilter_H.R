RF.filt <- function( pObj ,... ){
		UseMethod("RF.filt")
	}

# A,B,C,D 로직 구현에 있어서
# 상세정보의 비교대상과 비교방법들이 달라질 것이다.
# 이 부분에 따라 별도 클래스가 구성되게 될 듯.

# -----------------------------------------------
#	
RF.defineRelative_a <- function( pDP ){
		# pDP : DPrelative 객체(Flag가 생성된 상태)
		defObj <- list( DPdiff = pDPdiff )
		
		rObj <- list( def=defObj );	class(rObj) <- "RFRelative_a"
		return( rObj )		
	}


# -----------------------------------------------
#	pDP : DPdiff 클래스 객체
RF.defineDiff_a <- function( pDPdiff ){
		defObj <- list( DPdiff = pDPdiff )
		
		rObj <- list( def=defObj );	class(rObj) <- "RFDiff_a"
		return( rObj )
	}

# -----------------------------------------------
#	비정형 필터들.


# -----------------------------------------------
# k.filt.RFdiff_a
#	- 
k.filt.RFdiff_a <- function( pZhMtx ,pZoidMtx ,pMaxDiff=10 ,pLog=F ,pAna=F ){
		
		if(pLog){	k.FLogStr("k.filt.RFdiff_a start") }
		filtCls <- rep( NA ,nrow(pZoidMtx) )
		rObj <- list( filtCls=filtCls )

		oObj <- k.diff( pZhMtx ,pMax=pMaxDiff )
		if( pAna ){
			if(pLog){	k.FLogStr("    Analyze start") }
			b 
			nR <- length(oObj$diffLst)
			cName <- c("all","pMaxCnt","pMax")
			sameMtx <- matrix( NA ,ncol=length(cName) ,nrow=nR );	colnames(sameMtx)<-cName
			for( idx in 2:nR ){
				sameMtx[idx,"pMax"] <- length(intersect(oObj$diffLst[[idx-1]], oObj$diffLst[[idx]]))
				sameMtx[idx,"pMaxCnt"] <- length(oObj$diffLst[[idx]])
				sameMtx[idx,"all"] <- length(intersect(oObj$diffMtx[idx-1,], oObj$diffMtx[idx,]))
			}
		}

		lastDiff <- oObj$diffMtx[nrow(oObj$diffMtx),]
		lastDiffLst <- oObj$diffLst[[nrow(oObj$diffMtx)]]
		
		nObj <- k.diff( pZoidMtx ,pMax=pMaxDiff )
		nR <- length(nObj$diffLst)
		cName <- c("all","pMaxCnt","pMax")
		sameMtx <- matrix( 0 ,ncol=length(cName) ,nrow=nR );	colnames(sameMtx)<-cName
		for( idx in 2:nR ){
			sameMtx[idx,"pMax"] <- length(intersect(lastDiffLst, nObj$diffLst[[idx]]))
			sameMtx[idx,"pMaxCnt"] <- length(nObj$diffLst[[idx]])
			sameMtx[idx,"all"] <- length(intersect(lastDiff, nObj$diffMtx[idx,]))
		}

		if(pLog){	k.FLogStr("    Leveling start") }
		filtCls <- rep( NA ,nrow(pZoidMtx) )
		survive	<- is.na(filtCls)
		filtLev <- 1

		# Filt level 1
		sFlag <- sameMtx[,"all"] >= 6
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }
		
		# Filt level 2
		sFlag <- sameMtx[,"pMax"] >= 3
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }
		
		# Filt level 3
		sFlag.idx <- which(sameMtx[,"all"] == 3)
		
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }

		
		
		if(pLog){	k.FLogStr("k.filt.RFdiff_a finish") }
		rObj$oObj <- oObj;	rObj$nObj <- nObj
		return( rObj )
	}

# -----------------------------------------------
# k.filt.RFRelative_a
#	- 지금까지의 hZoid와 dna가 너무 같은 것은 제외.
k.filt.RFhZoidOnly_a <- function( pZhMtx ,pZoidMtx ,pLog=F ){
		
		if(pLog){	k.FLogStr("k.filt.RFhZoidOnly_a start") }
		# pZhMtx <- as.matrix(pDP$zh)

		rMtx <- k.compareZoid( pZoidMtx ,pZhMtx ,pSim=5 ,pLog=pLog )

		filtCls <- rep( NA ,nrow(pZoidMtx) )
		survive	<- is.na(filtCls)
		filtLev <- 1

		# Filt level 1.
		sFlag <- rep( F ,nrow(pZoidMtx) )
		sFlag[rMtx[rMtx[,3]==6,1]] <- T
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }

		# Filt level 5.
		sFlag <- rep( F ,nrow(pZoidMtx) )
		sFlag[rMtx[rMtx[,3]==5,1]] <- T
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }

		if(pLog){	k.FLogStr("k.filt.RFhZoidOnly_a finish") }
		rObj <- list( filtCls=filtCls )
		return( rObj )

	}

# -----------------------------------------------
# k.filt.RFRelative_a
#	- 주로 동일한 위치에 DNA 중복보유하는 것을 필터링.
k.filt.RFRelative_a <- function( pZhMtx ,pZoidMtx ,pLog=F ){

	if(pLog){	k.FLogStr("RF.filt.RFRelative_a start") }
	
	# pZhMtx <- as.matrix(pDP$zh)
	lastZ	<- pZhMtx[nrow(pZhMtx),]
	hzMtx	<- pZhMtx[1:(nrow(pZhMtx)-1),]
	
	hObj	<- k.relativeSearch( lastZ ,hzMtx )
	if(pLog){	k.FLogStr("    hObj is made.") }		
	nObj	<- k.relativeSearch( lastZ ,pZoidMtx )
	if(pLog){	k.FLogStr("    nObj is made.") }
	
	filtCls <- rep( NA ,nrow(pZoidMtx) )
	survive	<- is.na(filtCls)
	filtLev <- 1
	
	# Filt level 1.
	hCnt <- sapply( hObj$rebCode ,length )
	nCnt <- sapply( nObj$rebCode ,length )
	filtFlag <- survive & nCnt>3
	filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
	survive	<- is.na(filtCls)
	if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }
	
	# Filt level 2.
	designated <- which(hCnt==3)
	if( 0 < length(designated) ){
		rebPos <- hObj$rebPos[[designated[length(designated)]]]
		sFlag <- sapply( nObj$rebPos ,function(p){return(identical(p,rebPos))} )
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }
	}
	
	# Filt level 3.
	designated <- which(hCnt==2)
	if( 0 < length(designated) ){
		rebPos <- hObj$rebPos[[designated[length(designated)]]]
		sFlag <- sapply( nObj$rebPos ,function(p){return(identical(p,rebPos))} )
		filtFlag <- survive & sFlag
		filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
		survive	<- is.na(filtCls)
		if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }
	}
	
	
	lastDna <- as.vector(pZhMtx[(nrow(pZhMtx)-5):nrow(pZhMtx),])
	cnt <- apply( pZoidMtx ,1 ,function(p){return(sum(p%in%lastDna))} )
	
	# Filt Level 4
	filtFlag <- survive & (cnt<1)
	filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
	survive	<- is.na(filtCls)
	if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }

	# Filt Level 5
	filtFlag <- survive & (cnt>5)
	filtCls[filtFlag] <- filtLev;	filtLev <- filtLev+1
	survive	<- is.na(filtCls)
	if(pLog){	k.FLogStr(sprintf("    Filt %d : %d",filtLev,sum(filtFlag))) }

	
	if(pLog){	k.FLogStr("RF.filt.RFRelative_a finish") }
	
	rObj <- list( filtCls )
	
	return( rObj )
}