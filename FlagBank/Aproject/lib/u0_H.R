# u0_H.R unit model zero

zMtx <- tail(gEnv$allZoidMtx)	;rownames(zMtx) <- NULL

pVal <- zMtx[,1]	#  39 39 39 39 39 40
pCordLst <- lapply( 1:6 ,function(idx){ c(idx,1) })

pVal <- c( 1,7,5,1,7,3 )


# 1,1,1 / 1,2,3 / 2,4,6
u0.srchStep_std <- function( pVal ,pCordLst=NULL ){

    idxFlagLst <- u0.getChkIdx_std( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
    # same
    for( fIdx in seq_len(idxFlagLst) ){
        srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 2>srcVal.len ) next

        matCnt <- 0 # 연속이 몇 번 발생중인지.
        for( idx in 2:srcVal.len ){
            if( srcVal[idx]!=srcVal[1] ) break

            matCnt <- matCnt+1
        }
        if( matCnt==0 ) next

        banObj <- list( banVal=srcVal[1] ,certSize=matCnt )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_same( banObj ,pVal ,idxFlagLst[[fIdx]] )
        banLst[[1+length(banLst)]] <- banObj

        if( (matCnt+1)<srcVal.len ){   # 끝자락 값은 대칭 방지를 위해 추가...
            banObj <- list( banVal=srcVal[matCnt+2] ,certSize=matCnt )
            banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1+1)]
            banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1+1)]
            banObj$descript <- u0.getDescript_sameEnd( banObj ,pVal ,idxFlagLst[[fIdx]] )
            banLst[[1+length(banLst)]] <- banObj
        }
    } # same for()
    
    # desc1
    for( fIdx in seq_len(idxFlagLst) ){
		srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 2>srcVal.len ) next

		srcDiff <- srcVal[2:srcVal.len] - srcVal[1:(srcVal.len-1)]
		if( 1!=abs(srcDiff[1]) ) next
        matCnt <- 0 # 연속이 몇 번 발생중인지.
		for( idx in 1:length(srcDiff) ){
			if(srcDiff[idx]!=srcDiff[1]) break

			matCnt <- matCnt + 1
		}

        banObj <- list( banVal=(srcVal[1]-srcDiff[1]) ,certSize=matCnt )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_desc1( banObj ,pVal ,idxFlagLst[[fIdx]] )
		banLst[[1+length(banLst)]] <- banObj
    } # desc1 for()

    # descN
    for( fIdx in seq_len(idxFlagLst) ){
		srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 3>srcVal.len ) next	# desc N 이면 최소한 3연속어야 의미있음.

		srcDiff <- srcVal[2:srcVal.len] - srcVal[1:(srcVal.len-1)]
		if( 2>abs(srcDiff[1]) ) next	# desc에서의 N 값도 제한을 두는 게 좋을지...
        matCnt <- 0 # 연속이 몇 번 발생중인지.
		for( idx in 1:length(srcDiff) ){
			if(srcDiff[idx]!=srcDiff[1]) break

			matCnt <- matCnt + 1
		}
		if( matCnt<2 ) next

        banObj <- list( banVal=(srcVal[1]-srcDiff[1]) ,certSize=matCnt ,srcDiff=srcDiff[1] )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_descN( banObj ,pVal ,idxFlagLst[[fIdx]] )
		banLst[[1+length(banLst)]] <- banObj
    } # descN for()
	
    return( banLst )

} # u0.srchStep_std

# 1,2,3,2,1 / 1,2,2,1
u0.srchStep_symm <- function( pVal ){

    idxFlagLst <- u0.getChkIdx_symm( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
	for( fIdx in seq_len(idxFlagLst) ){
		chkIdx <- idxFlagLst[[fIdx]]
		if( !all(pVal[chkIdx$chk1]==pVal[chkIdx$chk2]) ) next
		if( all(pVal[chkIdx$coverArea[1]]==pVal[chkIdx$coverArea]) ) next # 한가지 값으로만 도배되어있다면..
		
		coverArea <- c( chkIdx$coverArea ,chkIdx$ban )

        banObj <- list( banVal=pVal[chkIdx$ban] ,coverArea=coverArea )
        banObj$cordLst=pCordLst[ coverArea ]
        banObj$cordStr=cordStr[  coverArea ]
        banObj$descript <- u0.getDescript_symm( banObj ,pVal )
		banLst[[1+length(banLst)]] <- banObj
		
	} # descN for()

	return( banLst )
} # u0.srchStep_std

# 1,2,7,2,1,...
u0.srchStep_ptnReb <- function( pVal ){

    idxFlagLst <- u0.getChkIdx_ptn( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
	for( fIdx in seq_len(idxFlagLst) ){
		chkIdx <- idxFlagLst[[fIdx]]
		if( !all(pVal[chkIdx$chk1]==pVal[chkIdx$chk2]) ) next
		if( all(pVal[chkIdx$coverArea[1]]==pVal[chkIdx$coverArea]) ) next # 한가지 값으로만 도배되어있다면..

        banObj <- list( banVal=pVal[chkIdx$ban] ,coverArea=chkIdx$coverArea )
        banObj$cordLst=pCordLst[ coverArea ]
        banObj$cordStr=cordStr[  coverArea ]
        banObj$descript <- u0.getDescript_ptnReb( banObj ,pVal )
		banLst[[1+length(banLst)]] <- banObj
		
	} # descN for()

	return( banLst )

} # u0.srchStep_std


u0.getChkIdx_std <- function( pMaxLen=6 ){

    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    tempFlag <- c( T ,T ,T ,T ,T ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[1]] <- tempFlag

    tempFlag <- c( F ,T ,F ,T ,F ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[2]] <- tempFlag

    tempFlag <- c( F ,F ,T ,F ,F ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[3]] <- tempFlag

    return(idxFlagLst)
} # u0.getChkIdx4std()

u0.getChkIdx_symm <- function( pMaxLen=6 ){

    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    if( 4<=maxLen ){ # 1,2,1,3
        idxFlagLst[[1]] <- list( chk1=1 ,chk2=3 ,ban=4 ,coverArea=1:3 )
    }
    if( 5<=maxLen ){ # 1,2,2,1,3
        idxFlagLst[[2]] <- list( chk1=1:2 ,chk2=4:3 ,ban=5 ,coverArea=1:4 )
    }
    if( 6<=maxLen ){ # 1,2,4,2,1,3
        idxFlagLst[[3]] <- list( chk1=1:2 ,chk2=5:4 ,ban=6 ,coverArea=1:5 )
    }

    return(idxFlagLst)

} # u0.getChkIdx_symm()

u0.getChkIdx_ptn <- function( pMaxLen=6 ){
    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    if( 4<=maxLen ){
        idxFlagLst[[1]] <- list( chk1=1:2 ,ban=3 ,chk2=4:5 ,coverArea=1:5 )
    }

    return(idxFlagLst)
} # u0.getChkIdx_ptn()


# rawVal<-pVal	;idxFlag<-idxFlagLst[[fIdx]]
u0.getDescript_same <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+1)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[same    ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_sameEnd <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+2)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[sameEnd ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_desc1 <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+1)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[desc1   ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_descN <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+1)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[desc(%2d) ] %2d(?),%s",banObj$srcDiff,banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_symm <- function( banObj ,rawVal ){
	valStr <- sprintf("%2d",rawVal[banObj$coverArea])
	valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[symm    ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_ptnReb <- function( banObj ,rawVal ){
	valStr <- sprintf("%2d",rawVal[banObj$coverArea])
	valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[ptnReb   ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}



