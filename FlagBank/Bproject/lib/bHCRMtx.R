# bFMtx와 bSMtx 영역 모두를 커버할 수 있다.

bHCRMtxLst <- list()

crScrH <- crScrHTool$getData()  # bHCRMtxLst 정의가 끝난 후, 마지막에 NULL 처리됨을 주의.

mName <- "HCRsz_bf01Ph"
if( TRUE ){
    # scMtx.sz            r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #                 rebCnt    0      0           0    0      0           0
    #                 rebDup    0      0           0    0      0           0

    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 phase를 비교하는 것은 아님을 유의.
    workMName <- c("score1","score2","sScore01","sScore02")
    wMLst= HCR.getMName( workMName ,crScrH ,warn=F )
    bHCRMtxLst[[mName]] <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
}

mName <- "HCRsz_bf01fCol"
if( TRUE ){
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 fCol을 비교하는 것은 아님을 유의.
    workMName <- c("score1","score2","sScore01","sScore02")
    wMLst= HCR.getMName( workMName ,crScrH ,warn=F )
    bHCRMtxLst[[mName]] <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
}

crScrH <- NULL