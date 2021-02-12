# bFMtx와 bSMtx 영역 모두를 커버할 수 있다.

bHCRMtxLst <- list()

crScrH <- crScrHTool$getData()  # bHCRMtxLst 정의가 끝난 후, 마지막에 NULL 처리됨을 주의.

mName <- "HCRsz_bf01Ph"
if( TRUE ){
    # scMtx.sz            r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #                 rebCnt    0      0           0    0      0           0
    #                 rebDup    0      0           0    0      0           0
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 phase를 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5","score8","score9")   # score 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,crScrH,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}

mName <- "HCRsz_bf01fCol"
if( TRUE ){
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 fCol을 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5","score8","score9")   # score 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,crScrH,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}

mName <- "HCRreb_szC01R"    # rebCnt
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score5")
        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,crScrH,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}

mName <- "HCRreb_szC01E"

mName <- "HCRreb_szD01R"    # rebDup
mName <- "HCRreb_szD01E"


# crScrH <- NULL