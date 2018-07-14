# 20180109_D_H.R final approach

anaFlagFnd <- function( flag ,report=TRUE ){
    kIdx <- head(which(flag))
    logStr <- kLog.getPerStr( sum(flag) ,length(flag) ,pLong=T )
    if( report ) cat(sprintf("    filted    %s\n",logStr))
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


