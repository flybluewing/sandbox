# bFMtx와 bSMtx 영역 모두를 커버할 수 있다.

#   Todo : scoreFV 적용 추가필요.

bHCRMtxLst <- list()

crScrH <- crScrHTool$getData()  # bHCRMtxLst 정의가 끝난 후, 마지막에 NULL 처리됨을 주의.

# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx (Ph,fCol)               HCR.MtxTmpl_szReb()
# ------------------------------------------------------------------------------------------
mName <- "HCRsz_bf01Ph"
if( TRUE ){
    # scMtx.sz            r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #                 rebCnt    0      0           0    0      0           0
    #                 rebDup    0      0           0    0      0           0
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 phase를 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5","score8","score9")   # score 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bf2APh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfavPh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfAZPh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
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

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bf2AfCol"
if( TRUE ){
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 fCol을 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfavfCol"
if( TRUE ){
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 fCol을 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfAZfCol"
if( TRUE ){
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 fCol을 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}


# ------------------------------------------------------------------------------------------
#   szMtx for bSMtx (Ph,fCol)               HCR.MtxTmpl_szReb()
# ------------------------------------------------------------------------------------------
mName <- "HCRsz_bS01Ph"
if( TRUE ){
    # scMtx.sz            r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #                 rebCnt    0      0           0    0      0           0
    #                 rebDup    0      0           0    0      0           0
    #   mName 별 중복 카운트 수만 비교할 뿐, 중복 발생 phase를 비교하는 것은 아님을 유의.

    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRsz_bS01AVefCol"    # QQE:Todo  (왜 ph에는 없었고 fCol에만 있었지?)
mName <- "HCRsz_bS2APh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore04","sScore06","sScore07")
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bSavPh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bSAZPh"     # mName 수가 너무 적음. av도 포함시킬까?
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw")

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCRsz_bS1avPh"    # bS01Ph + bSavPh
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore01","sScore02","sScore03","sScore05","sScore08","sScore09" )   # sScore 4,6,7 빠짐.
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCRsz_bS01AVefCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRsz_bS2AfCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore01","sScore02","sScore03" )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bSavfCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bSAZfCol"     # mName 수가 너무 적음. av도 포함시킬까?
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRsz_bS1avPh"    # QQE:Todo  (왜 ph에만 있고 fCol에는 없었지?)


# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx + bSMtx (Ph,fCol)               HCR.MtxTmpl_szReb()
# ------------------------------------------------------------------------------------------
mName <- "HCRsz_bfS01Ph"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5","score8","score9")   # score 4,6,7 빠짐.
        workMName <- c( workMName ,"sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfS2APh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        # workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07")
        # workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfSAVePh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- NULL
        workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfSavPh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfSAZPh"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        workMName <- c( workMName ,"sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.ph" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCRsz_bfS01fCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5","score8","score9")   # score 4,6,7 빠짐.
        workMName <- c( workMName ,"sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRsz_bfS2AfCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        # workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07")
        # workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRsz_bfSAVefCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- NULL
        workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRsz_bfSavfCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRsz_bfSAZfCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        workMName <- c( workMName ,"sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_szReb( mName=mName ,wMLst=rObj$wMLst ,szColName="r.fCol" ,szRowName="rebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}




# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx/bSMtx reb (r.ph,r.fCol,r.dblHpnFlg)
# ------------------------------------------------------------------------------------------
# HCR.MtxTmpl_rebSz(),HCR.MtxTmpl_rebRaw()
#   Todo : HCRreb_rawCxxx_a ,HCRreb_rawCxxx_b를 합쳐야 할 지도..
mName <- "HCRreb_szC01R"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score5")
        workMName <- c( workMName ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRreb_szC02R"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score4","score6","score7","score8","score9")
        workMName <- c( workMName ,"scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRreb_szC03R"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}


mName <- "HCRreb_szS01R"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24")

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_szS02R"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_szS03R"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$szCol <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebSz( mName=mName ,wMLst=rObj$wMLst ,crScrH ,szCol=rObj$szCol )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


mName <- "HCRreb_rawC01R_a"     # c( "ph","fCol","phReb" )
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "score1","score2","score3","score5" )
        workMName <- c( workMName ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "ph","fCol","phReb" )   # all ph fCol phReb xyCnt.fCol xyCnt.phase

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawC01R_b"     # c( "all","phReb","xyCnt.fCol","xyCnt.phase" )
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "score1","score2","score3","score5" )
        workMName <- c( workMName ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "all","phReb","xyCnt.fCol","xyCnt.phase" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawC02R_a"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "score4","score6","score7","score8","score9")
        workMName <- c( workMName ,"scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "ph","fCol","phReb" )   # all ph fCol phReb xyCnt.fCol xyCnt.phase

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawC02R_b"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "score4","score6","score7","score8","score9")
        workMName <- c( workMName ,"scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "all","phReb","xyCnt.fCol","xyCnt.phase" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawC03R_a"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "ph","fCol","phReb" )   # all ph fCol phReb xyCnt.fCol xyCnt.phase

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawC03R_b"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "all","phReb","xyCnt.fCol","xyCnt.phase" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


mName <- "HCRreb_rawS01R_a"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
        workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "ph","fCol","phReb" )   # all ph fCol phReb xyCnt.fCol xyCnt.phase

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawS01R_b"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
        workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "all","phReb","xyCnt.fCol","xyCnt.phase" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}
mName <- "HCRreb_rawS02R_a"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "ph","fCol","phReb" )   # all ph fCol phReb xyCnt.fCol xyCnt.phase

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawS02R_b"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c( "sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "all","phReb","xyCnt.fCol","xyCnt.phase" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawS03R_a" # (mName 수가 너무 적음. av도 포함시킬까?)
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "ph","fCol","phReb" )   # all ph fCol phReb xyCnt.fCol xyCnt.phase

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_rawS03R_b" # (mName 수가 너무 적음. av도 포함시킬까?)
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$compCol <- c( "all","phReb","xyCnt.fCol","xyCnt.phase" )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_rebRaw( mName=mName ,wMLst=rObj$wMLst ,crScrH ,compCol=rObj$compCol ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx/bSMtx reb (e.ph,e.fCol,e.dblHpnFlg)
# ------------------------------------------------------------------------------------------
#   evt 들에 대한 체크버전.
mName <- "HCRreb_szC01E"
mName <- "HCRreb_szC02E"
mName <- "HCRreb_szCAavE"
mName <- "HCRreb_szCavE"
mName <- "HCRreb_szS01E"
mName <- "HCRreb_szS02E"
mName <- "HCRreb_szSAavE"
mName <- "HCRreb_szSavE"

mName <- "HCRreb_rawC01E" # work
mName <- "HCRreb_rawC02E" # work
mName <- "HCRreb_rawCAavE" # work
mName <- "HCRreb_rawCavE" # work
mName <- "HCRreb_rawS01E" # work
mName <- "HCRreb_rawS02E" # work
mName <- "HCRreb_rawSAavE" # work
mName <- "HCRreb_rawSavE" # work



# ------------------------------------------------------------------------------------------
#   ph reb by mNames for bfMtx/bSMtx sz     HCR.MtxTmpl_phRebCnt_sz()
# ------------------------------------------------------------------------------------------
#   주의!! pName 이 이들의 fColName이다.
mName <- "HCRreb_phSzF01"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "score1","score2","score3","score4","score5","score6","score7","score8","score9" )
        workMName <- c( workMName ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF","scoreFV")
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
        workMName <- c( workMName ,"scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phRebCnt_sz( mName ,wMName=rObj$wMLst$bf ,crScrH ,mGrp="std.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_phSzF02"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
        workMName <- c( workMName ,"scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phRebCnt_sz( mName ,wMName=rObj$wMLst$bf ,crScrH ,mGrp="std.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCRreb_phSzS01"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
        workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phRebCnt_sz( mName ,wMName=rObj$wMLst$bS ,crScrH ,mGrp="bS.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_phSzS02"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )
        workMName <- c( workMName ,"sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phRebCnt_sz( mName ,wMName=rObj$wMLst$bS ,crScrH ,mGrp="bS.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCRreb_phSzFS02"  # QQE:Todo  hpn 갯수를 2~3으로 제한 시도.
mName <- "HCRreb_phSzFS02"  # QQE:Todo  hpn 갯수를 2~3으로 제한 시도.

# ------------------------------------------------------------------------------------------
#   ph reb by mNames for bfMtx/bSMtx raw      HCR.MtxTmpl_phRebCnt_raw()
# ------------------------------------------------------------------------------------------
#   suspend. sz 쪽이 더 성능 좋을 듯 하다.
mName <- "HCRreb_phRawF01"    # suspend
if( FALSE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score4")
        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phRebCnt_raw( mName ,wMName=rObj$wMLst$bf ,crScrH ,mGrp="std.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRreb_phRawS01"    # suspend
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore04")
        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phRebCnt_raw( mName ,wMName=rObj$wMLst$bS ,crScrH ,mGrp="bS.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}





# ------------------------------------------------------------------------------------------
#   summMtx for bFMtx (all ph fCol phReb xyCnt.fCol xyCnt.phase)    HCR.MtxTmpl_rawReb()
# ------------------------------------------------------------------------------------------
#   HCR.MtxTmpl_szReb() 의 raw sum버전. 
#       - phReb xyCnt.fCol xyCnt.phase 에 대한 reb 체크를 위해 사용.
#       - bFMtx + bSMtx 버전은 필요 없을 듯. bF,bS에 대한 재발금지 기준이 높아서 의미 없을 듯.
mName <- "HCRraw_bf01Sum01"
if( TRUE ){
    #             all ph fCol phReb xyCnt.fCol xyCnt.phase
    #         raw   0  0    0     0          0           0
    #         evt   0  0    0     0          0           0
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5")   # score 4,6,7 빠짐.
        workMName <- c( workMName ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF","scoreFV")
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName=c("phReb","xyCnt.fCol","xyCnt.phase") ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRraw_bf02Sum01"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","score8","score9")
        workMName <- c( workMName ,"scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
        workMName <- c( workMName ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        workMName <- c( workMName ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName=c("phReb","xyCnt.fCol","xyCnt.phase") ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRraw_bf03Sum01"     # QQE:Todo
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        workMName <- c( workMName ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName=c("phReb","xyCnt.fCol","xyCnt.phase") ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCRraw_bS01Sum01"
if( TRUE ){
    #             all ph fCol phReb xyCnt.fCol xyCnt.phase
    #         raw   0  0    0     0          0           0
    #         evt   0  0    0     0          0           0
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore01","sScore02","sScore03","sScore05")   # score 4,6,7 빠짐.
        workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName=c("phReb","xyCnt.fCol","xyCnt.phase") ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRraw_bS02Sum01"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore04","sScore06","sScore07","sScore08","sScore09")
        workMName <- c( workMName ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
        workMName <- c( workMName ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        workMName <- c( workMName ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName=c("phReb","xyCnt.fCol","xyCnt.phase") ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCRraw_bS03Sum01" # QQE:Todo    (mName 수가 너무 적음. av도 포함시킬까?)
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName=c("phReb","xyCnt.fCol","xyCnt.phase") ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


# ------------------------------------------------------------------------------------------
#   Reb for bCMtx.R ,bSMtx_Multi_C.R
# ------------------------------------------------------------------------------------------
#   개발 테스트 코드 파일       ReadMe\ZDev_HCR_crScr.R
if( FALSE ){    # sample code

    mName <- "HCR_crScrNnR"
    if( FALSE ){
        fMaker <- function( mName ,crScrH ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

            wMGrp <- list()
            wMGrp[["F"]] <- c("score1","score3","score8"      )
            wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

            rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
            rObj$getFilter <- function( crScrH=NULL ){
                fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="all" )   # raw용(rawF=T)
                return( fObj )
            }

            return( rObj )
        }
        bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
    }
    mName <- "HCR_crScrNnE"
    if( FALSE ){
        fMaker <- function( mName ,crScrH ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

            wMGrp <- list()
            wMGrp[["F"]] <- c("score1","score3","score8"      )
            wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

            rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
            rObj$getFilter <- function( crScrH=NULL ){
                fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="all" ) # evt용(rawF=F)
                return( fObj )
            }

            return( rObj )
        }
        bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
    }
    mName <- "HCR_crScrNnPhEvt"
    if( FALSE ){
        fMaker <- function( mName ,crScrH ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

            wMGrp <- list()
            wMGrp[["F"]] <- c("score1","score3","score8"      )
            wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

            rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
            rObj$getFilter <- function( crScrH=NULL ){
                fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="all" )
                return( fObj )
            }

            return( rObj )
        }
        bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
    }
    mName <- "HCR_crScrNnSum"
    if( FALSE ){
        fMaker <- function( mName ,crScrH ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

            wMGrp <- list()
            wMGrp[["F"]] <- c("score1","score3","score8"      )
            wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

            rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
            rObj$getFilter <- function( crScrH=NULL ){
                fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="all" )
                return( fObj )
            }

            return( rObj )
        }
        bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
    }
    mName <- "HCR_crScrNnSumClM"
    if( FALSE ){
        fMaker <- function( mName ,crScrH ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

            wMGrp <- list()
            wMGrp[["F"]] <- c("score1","score3","score8"      )
            wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

            rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
            rObj$getFilter <- function( crScrH=NULL ){
                fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="all" )
                return( fObj )
            }

            return( rObj )
        }
        bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
    }

    k <- c( "HCR_crScrN01R" ,"HCR_crScrN01E" ,"HCR_crScrN01PhEvt" ,"HCR_crScrN01Sum" ,"HCR_crScrN01SumClM" )
    if( FALSE ){
        workMName <- c("score1","score3","score8")              # "HCR_crScrN01R" "HCR_crScrN01E"
        workMName <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")    # "HCR_crScrN02R"

        workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")                               # "HCR_crScrN03R"
        workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )

        workMName <- NULL                                                                                   # "HCR_crScrN04R"
        workMName <- c( workMName ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
        workMName <- c( workMName ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
        workMName <- c( workMName ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )

        workMName <- c( "scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )   # "HCR_crScrN05R"
    }

    k <- c("HCR_bSMScr02R" ,"HCR_bSMScr02E" ,"HCR_bSMScr02PhEvt" ,"HCR_bSMScr02Sum" ,"HCR_bSMScr02SumClM" )
    if( FALSE ){

        workMName <- c( "sScore01","sScore02","sScore03" )              # "HCR_crScrN01R" "HCR_crScrN01E"
        workMName <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08") # "HCR_bSMScr02R"

        workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )             # "HCR_bSMScr03R"

        workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")                             # "HCR_bSMScr04R"
        workMName <- c( workMName ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
        workMName <- c( workMName ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )

        workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw") # "HCR_bSMScr05R"
    }

}


#- HCR.MtxTmpl_crScrNnx -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_RszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="szPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_EszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="szPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_RcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="commPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_EcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="commPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_Rrares"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["_"]] <- c("score1","score3","score8"   ,"sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="rares" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCR_crScrN02_RszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="szPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN02_EszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="szPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN02_RcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="commPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN02_EcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="commPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN02_Rrares"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["_"]] <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="rares" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


mName <- "HCR_crScrN03_RszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        wMGrp[["S"]] <- c( "sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="szPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN03_EszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        wMGrp[["S"]] <- c( "sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="szPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN03_RcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        wMGrp[["S"]] <- c( "sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="commPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN03_EcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        wMGrp[["S"]] <- c( "sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="commPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN03_Rrares"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["_"]] <- c( "scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="rares" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


mName <- "HCR_crScrN04_RszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        wMGrp[["S"]] <- c( "sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="szPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN04_EszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        wMGrp[["S"]] <- c( "sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="szPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN04_RcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        wMGrp[["S"]] <- c( "sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="commPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN04_EcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        wMGrp[["F"]] <- c( wMGrp[["F"]] ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        wMGrp[["S"]] <- c( "sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        wMGrp[["S"]] <- c( wMGrp[["S"]] ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="commPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN04_Rrares"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["_"]] <- c( "scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24" )

        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24" )


        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="rares" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


mName <- "HCR_crScrN05_RszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        wMGrp[["S"]] <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="szPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN05_EszPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        wMGrp[["S"]] <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="szPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN05_RcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        wMGrp[["S"]] <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="commPhFCol" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN05_EcommPhFCol"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c( "scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        wMGrp[["S"]] <- c( "sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=F ,colOpt="commPhFCol" )   # evt용(rawF=F)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN05_Rrares"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["_"]] <- c( "scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )
        wMGrp[["_"]] <- c( wMGrp[["_"]] ,"sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw" )

        rObj <- list( mName=mName ,wMGrp=wMGrp )
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMGrp=rObj$wMGrp ,rawF=T ,colOpt="rares" )   # raw용(rawF=T)
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

#- HCR.MtxTmpl_crScrNnPhEvt -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_PhEvtmax"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="max" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_PhEvtcnt"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="cnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_PhEvtrebEvtMax"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="rebEvtMax" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_PhEvtrebEvtMCnt"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="rebEvtMCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


mName <- "HCR_crScrN02_PhEvtmax"
mName <- "HCR_crScrN02_PhEvtcnt"
mName <- "HCR_crScrN02_PhEvtrebEvtMax"
mName <- "HCR_crScrN02_PhEvtrebEvtMCnt"

mName <- "HCR_crScrN03_PhEvtmax"
mName <- "HCR_crScrN03_PhEvtcnt"
mName <- "HCR_crScrN03_PhEvtrebEvtMax"
mName <- "HCR_crScrN03_PhEvtrebEvtMCnt"

mName <- "HCR_crScrN04_PhEvtmax"
mName <- "HCR_crScrN04_PhEvtcnt"
mName <- "HCR_crScrN04_PhEvtrebEvtMax"
mName <- "HCR_crScrN04_PhEvtrebEvtMCnt"

mName <- "HCR_crScrN05_PhEvtmax"
mName <- "HCR_crScrN05_PhEvtcnt"
mName <- "HCR_crScrN05_PhEvtrebEvtMax"
mName <- "HCR_crScrN05_PhEvtrebEvtMCnt"


#- HCR.MtxTmpl_crScrNnPhEvt -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_PhEvtmax"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="max" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_PhEvtcnt"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="cnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_PhEvtrebEvtMax"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="rebEvtMax" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_PhEvtrebEvtMCnt"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="rebEvtMCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCR_crScrN02_PhEvtmax"
mName <- "HCR_crScrN02_PhEvtcnt"
mName <- "HCR_crScrN02_PhEvtrebEvtMax"
mName <- "HCR_crScrN02_PhEvtrebEvtMCnt"

mName <- "HCR_crScrN03_PhEvtmax"
mName <- "HCR_crScrN03_PhEvtcnt"
mName <- "HCR_crScrN03_PhEvtrebEvtMax"
mName <- "HCR_crScrN03_PhEvtrebEvtMCnt"

mName <- "HCR_crScrN04_PhEvtmax"
mName <- "HCR_crScrN04_PhEvtcnt"
mName <- "HCR_crScrN04_PhEvtrebEvtMax"
mName <- "HCR_crScrN04_PhEvtrebEvtMCnt"

mName <- "HCR_crScrN05_PhEvtmax"
mName <- "HCR_crScrN05_PhEvtcnt"
mName <- "HCR_crScrN05_PhEvtrebEvtMax"
mName <- "HCR_crScrN05_PhEvtrebEvtMCnt"



#- HCR.MtxTmpl_crScrNnSum -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_sumsumRaw"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="sumRaw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_sumsumEvt"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="sumEvt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_sumszSumRebCnt"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="szSumRebCnt" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_sumszSumRebDup"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="szSumRebDup" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCR_crScrN02_sumsumRaw"
mName <- "HCR_crScrN02_sumsumEvt"
mName <- "HCR_crScrN02_sumszSumRebCnt"
mName <- "HCR_crScrN02_sumszSumRebDup"

mName <- "HCR_crScrN03_sumsumRaw"
mName <- "HCR_crScrN03_sumsumEvt"
mName <- "HCR_crScrN03_sumszSumRebCnt"
mName <- "HCR_crScrN03_sumszSumRebDup"

mName <- "HCR_crScrN04_sumsumRaw"
mName <- "HCR_crScrN04_sumsumEvt"
mName <- "HCR_crScrN04_sumszSumRebCnt"
mName <- "HCR_crScrN04_sumszSumRebDup"

mName <- "HCR_crScrN05_sumsumRaw"
mName <- "HCR_crScrN05_sumsumEvt"
mName <- "HCR_crScrN05_sumszSumRebCnt"
mName <- "HCR_crScrN05_sumszSumRebDup"


#- HCR.MtxTmpl_crScrNnSumClM -----------------------------------------------------------------------------------------------
mName <- "HCR_crScrN01_sumClMsumTotX"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="sumTotX" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}
mName <- "HCR_crScrN01_sumClMsumTotHpn"
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.

        wMGrp <- list()
        wMGrp[["F"]] <- c("score1","score3","score8"      )
        wMGrp[["S"]] <- c("sScore01","sScore02","sScore03")

        rObj <- list( mName=mName ,wMGrp=wMGrp )       # wMLst=HCR.getMName(workMName,warn=F)
        rObj$getFilter <- function( crScrH=NULL ){
            fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMGrp=rObj$wMGrp ,colOpt="sumTotHpn" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

mName <- "HCR_crScrN02_sumClMsumTotX"
mName <- "HCR_crScrN02_sumClMsumTotHpn"

mName <- "HCR_crScrN03_sumClMsumTotX"
mName <- "HCR_crScrN03_sumClMsumTotHpn"

mName <- "HCR_crScrN04_sumClMsumTotX"
mName <- "HCR_crScrN04_sumClMsumTotHpn"

mName <- "HCR_crScrN05_sumClMsumTotX"
mName <- "HCR_crScrN05_sumClMsumTotHpn"


if( FALSE ){    # deprecated
        mName <- "HCR_crScrN01R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score3","score8")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN01E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score3","score8")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN01PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score3","score8")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN01Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score3","score8")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN01SumClM"   # bCMtx에는 없지만 Reb체크를 위해..
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score3","score8")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }

        mName <- "HCR_crScrN02R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN02E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN02PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN02Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN02SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("score1","score2","score3","score4","score5","score6","score7","score8","score9")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }


        mName <- "HCR_crScrN03R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN03E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN03PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN03Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN03SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }

        mName <- "HCR_crScrN04R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
                workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
                workMName <- c( workMName ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
                workMName <- c( workMName ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN04E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
                workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
                workMName <- c( workMName ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
                workMName <- c( workMName ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN04PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
                workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
                workMName <- c( workMName ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
                workMName <- c( workMName ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN04Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
                workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
                workMName <- c( workMName ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
                workMName <- c( workMName ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN04SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
                workMName <- c( workMName ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
                workMName <- c( workMName ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
                workMName <- c( workMName ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }

        mName <- "HCR_crScrN05R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName  ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
                workMName <- c( workMName  ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN05E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName  ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
                workMName <- c( workMName  ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN05PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName  ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
                workMName <- c( workMName  ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN05Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName  ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
                workMName <- c( workMName  ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_crScrN05SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- NULL
                workMName <- c( workMName  ,"scoreA","scoreB","scoreC","scoreD","scoreE","scoreF" )
                workMName <- c( workMName  ,"scoreFV"  ,"scoreGS" ,"scoreGSh2" ,"scoreGS3" ,"scorePSh" ,"scorePSrp" ,"scorePSrpRaw" )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }



        mName <- "HCR_bSMScr02R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr02E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr02PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr02Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr02SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }

        mName <- "HCR_bSMScr04R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
                workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
                workMName <- c( workMName ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
                workMName <- c( workMName ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr04E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
                workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
                workMName <- c( workMName ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
                workMName <- c( workMName ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr04PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
                workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
                workMName <- c( workMName ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
                workMName <- c( workMName ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr04Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
                workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
                workMName <- c( workMName ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
                workMName <- c( workMName ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr04SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
                workMName <- c( workMName ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
                workMName <- c( workMName ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
                workMName <- c( workMName ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }

        mName <- "HCR_bSMScr05R"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=T )   # raw용(rawF=T)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr05E"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnx( mName=mName ,wMLst=rObj$wMLst ,rawF=F ) # evt용(rawF=F)
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr05PhEvt"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnPhEvt( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr05Sum"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSum( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }
        mName <- "HCR_bSMScr05SumClM"
        if( F ){
            fMaker <- function( mName ,crScrH ){
                # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
                workMName <- c("sScore0GS" ,"sScore0GSh2" ,"sScore0GS3" ,"sScore0PSh" ,"sScore0PSrp" ,"sScore0PSrpRaw")

                rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
                rObj$getFilter <- function( crScrH=NULL ){
                    fObj <- HCR.MtxTmpl_crScrNnSumClM( mName=mName ,wMLst=rObj$wMLst )
                    return( fObj )
                }

                return( rObj )
            }
            bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
        }

}



# crScrH <- NULL

