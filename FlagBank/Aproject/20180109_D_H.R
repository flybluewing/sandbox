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

anaMtx <- function( zMtx ){
    cat("    Raw value(reb)         cStep            QuoSize     QuoTbl \n")
    rObj <- sapply( seq_len(nrow(zMtx)) ,function( zIdx ){
                    valStr <- paste(sprintf("%2d",zMtx[zIdx,]) ,collapse=" " )
                    rebCnt <- 0
                    if( zIdx>1 ){
                        rebCnt <- sum(zMtx[zIdx,] %in% zMtx[(zIdx-1),])
                    }

                    cStepStr <- paste(sprintf("%2d",zMtx[zIdx,2:6]-zMtx[zIdx,1:5]) 
                                    ,collapse=" " )

                    quoObj <- fCutU.getQuoObj( zMtx[zIdx,]  )
                    quoTblStr <- paste( quoObj$tbl ,collapse=" " )
                    quoSizeStr <- paste( quoObj$size ,collapse=" " )

                    cat(sprintf("    %s%s   %s   %s   %s\n"
                            ,valStr
                            ,ifelse(rebCnt>0,sprintf("(%d)",rebCnt),"   ")
                            ,cStepStr ,quoSizeStr ,quoTblStr
                        ))
                })

    dupValCnt <- table(as.vector(zMtx))
    dupValCnt <- dupValCnt[dupValCnt>1]
    dupValStr <- paste( names(dupValCnt) ,dupValCnt ,sep=":" )
    cat(sprintf("  dup number  %s\n",paste(dupValStr,collapse="   ") ))
    
    zw <- zMtx[,6]-zMtx[,1]
    cat(sprintf("  zoid width  %s\n",paste(zw,collapse="   ") ))

    rObj <- list( quoTbl=anaQuoTbl(zMtx) )
    return(NULL)
} # anaMtx()



