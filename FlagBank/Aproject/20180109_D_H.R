# 20180109_D_H.R final approach

anaFlagFnd <- function( flag ,report=TRUE ){
    kIdx <- head(which(flag))
    logStr <- kLog.getPerStr( sum(flag) ,length(flag) ,pLong=T )
    if( report ) cat(sprintf("    filted    %s\n",logStr))
    return( kIdx )
} # anaFlagFnd( )

anaFltCnt <- function( fltCnt ,dbgThld=1 ,report=TRUE ){
    kIdx <- head(which(fltCnt>=dbgThld))
    tbl <- table(fltCnt)    ;names(tbl) <- sprintf("cnt %s ",names(tbl))
    logStr <- kLog.getPerStr( tbl ,length(fltCnt) ,pLong=T )
    if( report ) cat(sprintf("    filted    %s\n",paste(logStr,collapse="   ")))
    return( kIdx )
} # anaFlagFnd( )

anaQuoTbl <- function( zMtx ){

    rObj <- list()
    rObj$tblLst <- apply(zMtx%/%10 ,1 ,table )
    if( 2>nrow(zMtx) ) return( rObj )

    fltCnt <- rep( 0 ,length(rObj$tblLst) )
    for( hIdx in 2:length(rObj$tblLst) ){
        tblPast <-rObj$tblLst[[hIdx-1]]
        tblNow <- rObj$tblLst[[hIdx]]
        if( length(tblPast)!=length(tblNow) ){
            next
        }

        logStr <- ""
        if( all(tblPast==tblNow) ){
            fltCnt[hIdx] <- 1
            logStr <- sprintf("same tbl - hIdx:%d %s",hIdx ,paste(tblPast,collapse=",") )
            if( all(names(tblPast)==names(tblNow)) ){
                fltCnt[hIdx] <- 2
                logStr <- sprintf("%s perfact",logStr)
            }
        }
    }

    fltCntTbl=table(fltCnt)
    names( fltCntTbl ) <- c("none","match","perfact match")[1:length(fltCntTbl)]
    perStr <- kLog.getPerStr( fltCntTbl ,sum(fltCntTbl) ,pLong=T )
    cat(sprintf("       Quo10 pattern rebind table \n"))
    cat(sprintf("       %s \n",paste(perStr,collapse="   ")))

    rObj$fltCntTbl <- fltCntTbl
    return( rObj )

} # anaQuoTbl( )

anaMtx <- function( zMtx ,stdZoid=NULL ){
    # rObj <- anaMtx( stdMI$rawTail ,stdZoid )
    cat("    Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl \n")
    dummy <- sapply( seq_len(nrow(zMtx)) ,function( zIdx ){
                    valStr <- paste(sprintf("%2d",zMtx[zIdx,]) ,collapse=" " )
                    rebCnt <- 0
                    if( zIdx>1 ){
                        rebCnt <- sum(zMtx[zIdx,] %in% zMtx[(zIdx-1),])
                    }

                    cStepStr <- paste(sprintf("%2d",zMtx[zIdx,2:6]-zMtx[zIdx,1:5]) 
                                    ,collapse=" " )
                    fStepStr <- if( zIdx>1 ){
                                    paste(sprintf("%3d" ,zMtx[zIdx,]-zMtx[(zIdx-1),])
                                        ,collapse=" " )
                                } else { "                       " }

                    quoObj <- fCutU.getQuoObj( zMtx[zIdx,]  )
                    quoTblStr <- paste( quoObj$tbl ,collapse=" " )
                    quoSizeStr <- paste( quoObj$size ,collapse=" " )

                    cat(sprintf("    %s%s |%s |%s |%s |%s\n"
                            ,valStr
                            ,ifelse(rebCnt>0,sprintf("(%d)",rebCnt),"   ")
                            ,cStepStr ,fStepStr ,quoSizeStr ,quoTblStr
                        ))
                })
    if( !is.null(stdZoid) ){
        cat("-<standard zoid>---------------------------------------------------------------------\n")
                    lastZoid <- zMtx[nrow(zMtx),]
                    valStr <- paste(sprintf("%2d",stdZoid) ,collapse=" " )
                    rebCnt <- sum(stdZoid %in% lastZoid)

                    cStepStr <- paste(sprintf("%2d",stdZoid[2:6]-stdZoid[1:5]) ,collapse=" " )
                    fStepStr <- paste(sprintf("%3d" ,stdZoid-lastZoid) ,collapse=" " )

                    quoObj <- fCutU.getQuoObj( stdZoid )
                    quoTblStr <- paste( quoObj$tbl ,collapse=" " )
                    quoSizeStr <- paste( quoObj$size ,collapse=" " )

                    cat(sprintf("    %s%s |%s |%s |%s |%s\n"
                            ,valStr
                            ,ifelse(rebCnt>0,sprintf("(%d)",rebCnt),"   ")
                            ,cStepStr ,fStepStr ,quoSizeStr ,quoTblStr
                        ))
    }

    dupValCnt <- table(as.vector(zMtx))
    dupValCnt <- dupValCnt[dupValCnt>1]
    dupValStr <- paste( names(dupValCnt) ,dupValCnt ,sep=":" )
    cat(sprintf("  dup number  %s\n",paste(dupValStr,collapse="   ") ))
    
    zw <- zMtx[,6]-zMtx[,1]
    cat(sprintf("  zoid width  ... %s and ?\n",paste(zw,collapse="   ") ))

    rObj <- list( quoTbl=anaQuoTbl(zMtx) )
    return(NULL)
} # anaMtx()

anaMtx.freqVal <- function( rawTail ){
	# rawTail <- stdMI$rawTail
	if( 2>nrow(rawTail) )	return("")
	
    vals <- sort(unique(as.vector(rawTail)))
    freqVals <- vals[ table(rawTail)>1 ]

    for( fVal in freqVals ){
		zoid1st <- NULL
		zoid2nd <- NULL
		for( rIdx in nrow(rawTail):1 ){
			if( !any(rawTail[rIdx,]==fVal) ) next
			
			if( is.null(zoid2nd) ){
				zoid2nd <- rawTail[rIdx,]
			} else {
				zoid1st <- rawTail[rIdx,]
				break
			}
		}	# rIdx

		posDiff <- which( zoid2nd==fVal ) - which( zoid1st==fVal )
		if( 0==posDiff ){
			zoid1st.code <- zoid1st
			zoid2nd.code <- zoid2nd
		} else {
			zoid1st.code <- if(posDiff>0) zoid1st[1:(6-posDiff)] else zoid1st[(1-posDiff):6]
			zoid2nd.code <- if(posDiff>0) zoid2nd[(1+posDiff):6] else zoid2nd[1:(6+posDiff)]
		}
		zoidBan <- zoid2nd.code + (zoid2nd.code-zoid1st.code)
		diffEvt <- 2>abs(zoid2nd.code-zoid1st.code)	# diff event
		zoidBan.str <- sprintf("%3d",zoidBan)
		zoidBan.str[diffEvt] <- sprintf("%s!",zoidBan.str[diffEvt])
		zoidBan.str[!diffEvt] <- sprintf("%s ",zoidBan.str[!diffEvt])

		baseIdx <- which(zoid1st.code==fVal)
		zoidBan.str[baseIdx] <- sprintf("%3d*",fVal)
		zoidBan.str[zoidBan<1 | zoidBan>45] <- " NA "
		if( 1<baseIdx ){
			idxSpan <- 1:(baseIdx-1)
			zoidBan.str[idxSpan] <- ifelse( zoidBan[idxSpan]>=fVal ," NA " ,zoidBan.str[idxSpan] )
		}
		if( baseIdx<length(zoidBan) ){
			idxSpan <- (baseIdx+1):length(zoidBan)
			zoidBan.str[idxSpan] <- ifelse( zoidBan[idxSpan]<=fVal ," NA " ,zoidBan.str[idxSpan] )
		}

		zoid1st.str <- paste( sprintf("%3d ",zoid1st.code) ,collapse="," )
		zoid2nd.str <- paste( sprintf("%3d ",zoid2nd.code) ,collapse="," )
		cat(sprintf("    <%3d> %3d    %s\n",fVal,posDiff,zoid1st.str))
		cat(sprintf("                 %s\n",zoid2nd.str))
		cat(sprintf("             --> %s\n",paste(zoidBan.str,collapse=",") ))
    }

	return("")
} # anaMtx.freqVal()

anaMtx_ColVal <- function( zMtx ){
    # idx<-3	;zMtx <-cvSeqNextLst[[idx]]$fndMtx
    getRowStr <- function( rowIdx ,mtxLst ){
        rowStr <- sapply( mtxLst ,function( mtx ){
                        if( rowIdx>nrow(mtx) ){
                            return( paste(rep("  ",ncol(mtx)),collapse=" ") )
                        } else {
                            return( paste(sprintf("%2d",mtx[rowIdx,]),collapse=" ") )
                        }
                    })
        return( paste(rowStr,collapse="    ") )
    } # getRowStr()

    for( cIdx in 1:ncol(zMtx)){
        cat(sprintf("col:%d \n",cIdx))
        colValTbl <- table(zMtx[,cIdx])
        colValSpan <- as.integer( names(colValTbl[colValTbl>1]) )
        if( 0==length(colValSpan) ){
            next
        }
        colValLst <- list()
        for( colVal in colValSpan ){
            colValLst[[as.character(colVal)]] <- zMtx[zMtx[,cIdx]==colVal ,]
        }
        maxRow <- max( sapply(colValLst,nrow) )
        for( rIdx in 1:maxRow ){
            cat( sprintf("    %s\n",getRowStr(rIdx,colValLst)) )
        }
    } # cIdx

} # anaMtx_ColVal()

