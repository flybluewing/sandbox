# bFMtx와 bSMtx 영역 모두를 커버할 수 있다.

bHCRMtxLst <- list()

crScrH <- crScrHTool$getData()

mName <- "HCRsz_bf01Ph"
if( FALSE ){
    workMName <- c("score1","score2","sScore01","sScore02","fake")
    wMLst= HCR.getMName( workMName ,crScrH ,warn=T )
    bHCRMtxLst[[mName]] <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=wMLst ,szColName="ph" )
}


