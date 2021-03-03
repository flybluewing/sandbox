# bFMtx와 bSMtx 영역 모두를 커버할 수 있다.

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

mName <- "HCRsz_bS01fCol"    # WORK
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.

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
mName <- "HCRsz_bS2AfCol"    # WORK
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore04","sScore06","sScore07")
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
mName <- "HCRsz_bSavfCol"    # WORK
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
mName <- "HCRsz_bfS2APh"    # WORK
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07")
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
mName <- "HCRsz_bfSavPh"    # WORK
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

mName <- "HCRsz_bfS01fCol"    # WORK
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
mName <- "HCRsz_bfS2AfCol"    # WORK
if( TRUE ){
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score4","score6","score7","scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
        workMName <- c( workMName ,"scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24" )
        workMName <- c( workMName ,"sScore04","sScore06","sScore07")
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
mName <- "HCRsz_bfSavfCol"    # WORK
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





# ------------------------------------------------------------------------------------------
#   szMtx for bFMtx/bSMtx reb (r.ph,r.fCol,r.dblHpnFlg)
# ------------------------------------------------------------------------------------------
# HCR.MtxTmpl_rebSz(),HCR.MtxTmpl_rebRaw()
#   Todo : HCRreb_rawCxxx_a ,HCRreb_rawCxxx_b를 합쳐야 할 지도..
mName <- "HCRreb_szC01R"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score5")
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
mName <- "HCRreb_szCAavR"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
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
mName <- "HCRreb_szCavR"
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
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

mName <- "HCRreb_szS01R"    # WORK
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
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
mName <- "HCRreb_szS02R"    # WORK
if( TRUE ){

    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

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
mName <- "HCRreb_szSAavR"    # WORK
if( TRUE ){

    fMaker <- function( mName ,crScrH ){

        workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
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
mName <- "HCRreb_szSavR"    # WORK
if( TRUE ){

    fMaker <- function( mName ,crScrH ){

        workMName <- c( "sScore01","sScore02","sScore03","sScore05","sScore08","sScore09" )   # sScore 4,6,7 빠짐.
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

mName <- "HCRreb_rawC01R_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score5")
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
mName <- "HCRreb_rawC01R_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score5")
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
mName <- "HCRreb_rawC02R_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score4","score6","score7","score8","score9")
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
mName <- "HCRreb_rawC02R_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("score4","score6","score7","score8","score9")
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
mName <- "HCRreb_rawCAavR_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
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
mName <- "HCRreb_rawCAavR_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("scoreA","scoreB","scoreC","scoreD","scoreE","scoreF")
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
mName <- "HCRreb_rawCavR_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
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
mName <- "HCRreb_rawCavR_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
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

mName <- "HCRreb_rawS01R_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
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
mName <- "HCRreb_rawS01R_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore05","sScore08","sScore09")   # sScore 4,6,7 빠짐.
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
mName <- "HCRreb_rawS02R_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c( "sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

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
mName <- "HCRreb_rawS02R_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c( "sScore04","sScore06","sScore07" )
        workMName <- c( workMName ,"sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24" )

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
mName <- "HCRreb_rawSAavR_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
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
mName <- "HCRreb_rawSAavR_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
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
mName <- "HCRreb_rawSavR_a" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c( "sScore01","sScore02","sScore03","sScore05","sScore08","sScore09" )   # sScore 4,6,7 빠짐.
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
mName <- "HCRreb_rawSavR_b" # work
if( TRUE ){

    fMaker <- function( mName ,crScrH ){
        workMName <- c( "sScore01","sScore02","sScore03","sScore05","sScore08","sScore09" )   # sScore 4,6,7 빠짐.
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
#   ph reb by mNames for bfMtx raw      HCR.MtxTmpl_phReb_raw()
# ------------------------------------------------------------------------------------------
mName <- "HCRreb_phRawF01"    # WORK
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c("score1","score2","score3","score4")
        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phReb_raw( mName ,wMName=rObj$wMLst$bf ,crScrH ,mGrp="std.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}

#   work New
# ------------------------------------------------------------------------------------------
#   ph reb by mNames for bSMtx raw
# ------------------------------------------------------------------------------------------
mName <- "HCRreb_phRawS01"    # WORK
if( TRUE ){
    fMaker <- function( mName ,crScrH ){
        workMName <- c("sScore01","sScore02","sScore03","sScore04")
        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )

        rObj$getFilter <- function( crScrH ){
            fObj <- HCR.MtxTmpl_phReb_raw( mName ,wMName=rObj$wMLst$bS ,crScrH ,mGrp="bS.grp" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )
}


#   work New
# ------------------------------------------------------------------------------------------
#   ph reb by mNames for fMtx sz
# ------------------------------------------------------------------------------------------




# ------------------------------------------------------------------------------------------
#   summMtx for bFMtx (all ph fCol phReb xyCnt.fCol xyCnt.phase)
# ------------------------------------------------------------------------------------------
#   고려사항 : colName 을 한 개 컬럼씩만 하면 너무 방대하니, col들의 sum으로 할까?
mName <- "HCRraw_bf01Ph"
if( TRUE ){
    #             all ph fCol phReb xyCnt.fCol xyCnt.phase
    #         raw   0  0    0     0          0           0
    #         evt   0  0    0     0          0           0
    fMaker <- function( mName ,crScrH ){

        workMName <- c("score1","score2","score3","score5","score8","score9")   # score 4,6,7 빠짐.

        rObj <- list( mName=mName ,wMLst=HCR.getMName(workMName,warn=F) )
        rObj$getFilter <- function( crScrH=NULL ){
            # crScrH 사실 필요 치 않음. 단지 다른 filter 생성자와 파라미터 맞추기 위함.
            fObj <- HCR.MtxTmpl_rawReb( mName=mName ,wMLst=rObj$wMLst ,colName="ph" ,rowName="raw" )
            return( fObj )
        }

        return( rObj )
    }
    bHCRMtxLst[[mName]] <- fMaker( mName ,crScrH )

}

mName <- "HCRraw_bf2APh"
mName <- "HCRraw_bfavPh"
mName <- "HCRraw_bf01fCol"
mName <- "HCRraw_bf2AfCol"
mName <- "HCRraw_bfavfCol"

# ------------------------------------------------------------------------------------------
#   summMtx for bSMtx (all ph fCol phReb xyCnt.fCol xyCnt.phase)
# ------------------------------------------------------------------------------------------
mName <- "HCRraw_bS01Ph"
mName <- "HCRraw_bS2APh"
mName <- "HCRraw_bSavPh"
mName <- "HCRraw_bS1avPh"    # bS01Ph + bSavPh
mName <- "HCRraw_bS01fCol"    # WORK
mName <- "HCRraw_bS2AfCol"    # WORK
mName <- "HCRraw_bSavfCol"    # WORK

# ------------------------------------------------------------------------------------------
#   summMtx for bFMtx + bSMtx (all ph fCol phReb xyCnt.fCol xyCnt.phase)
# ------------------------------------------------------------------------------------------
mName <- "HCRraw_bfS01Ph"
mName <- "HCRraw_bfS2APh"    # WORK
mName <- "HCRraw_bfSavPh"    # WORK
mName <- "HCRraw_bfS01fCol"    # WORK
mName <- "HCRraw_bfS2AfCol"    # WORK
mName <- "HCRraw_bfSavfCol"    # WORK




# crScrH <- NULL

