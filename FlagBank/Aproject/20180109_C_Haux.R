# 20180109_C_H.R 교차모델

#   ban.throughH() 함수 설정값 결정을 위한 테스트
#       20180109_C_ReadMe.txt 파일내용 참고.
ban.throughH.test <-function( pBanObj ){
    #   초기 시작점부터 바로 이전까지 n포인트가 연속으로 동일할 때, 
    #   다음 z에서까지 n포인트가 모두 동일하긴 어렵겟지?
    #       --> n개는 depth에 따라 최저 수치 존재. 

    depthIdx <- 5

    surObjLst <- list()
    for( nIdx in pBanObj$cfNames ){
        surObj <- list( idStr=nIdx )
        encValLst <- pBanObj$encValLst[[nIdx]]
        matFlag <- rep( TRUE ,length(encValLst[[1]]) )

        encSpan <- (depthIdx+1):length(encValLst)
        surviveCntLst <- list()
        for( eIdx in encSpan ){
            baseCode <- encValLst[[eIdx-depthIdx]]
            matFlag[] <- TRUE
            for( sIdx in (eIdx-(depthIdx-1):1) ){
                matFlag <- (baseCode==encValLst[[sIdx]]) & matFlag
            }
            survive.last    <- sum(matFlag)
            survive.tgt     <- sum( baseCode[matFlag]==encValLst[[eIdx]][matFlag] )
            
            surviveCntLst[[1+length(surviveCntLst)]] <- c(survive.last,survive.tgt)
        }
        surObj$cntMtx <- do.call( rbind ,surviveCntLst )
        colnames(surObj$cntMtx) <- c("survive.last","survive.tgt")
        surObjLst[[nIdx]] <- surObj
    } # for(nIdx)

#    for( surIdx in 1:length(surObjLst) ){
    for( surIdx in 1:6 ){
        surObj <- surObjLst[[surIdx]]
        cat(sprintf("[%s]\n",surObj$idStr))
        tbl <- table(surObj$cntMtx[,1])
        tblStr <- paste( names(tbl) ,tbl ,sep=":")
        cat(sprintf("  freq %s \n",paste(tblStr,collapse=" ")))

        failFlag <- surObj$cntMtx[,1]==surObj$cntMtx[,2]        
        codeLen <- length(pBanObj$encValLst[[surObj$idStr]][[1]])
        for( thld in (codeLen-1:6) ){
            failCnt <- sum(surObj$cntMtx[failFlag,1]>=thld)
            rptMsg <- sprintf("   thld %d depth %d fail %.1f%% (%d of %d) \n" 
                        ,thld,depthIdx
                        ,failCnt*100/nrow(surObj$cntMtx),failCnt,nrow(surObj$cntMtx))
            cat( rptMsg )
        }
    }

    

} # ban.multiDim

