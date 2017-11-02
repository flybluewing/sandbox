# 파일명 : 20171029_A_H_seqAnaFun.R
#   의존 : 20170917_A_H.R

# 
seqAnaFun.default <- function( pFlag ,pPredObj ,pInitNA ,pCodeVal ){

    flag <- if(is.na(pInitNA)){ pFlag 
                } else pFlag[-(1:pInitNA)]
    codeVal <- if(is.null(pCodeVal)){ sort(unique(flag),na.last=T) 
                    } else pCodeVal
    codeVal.naIdx <- if( any(is.na(codeVal)) ) { which(is.na(codeVal)) 
                        } else NULL
            # 즉, codeVal.naIdx 값이 NULL임을 보고 flag 내에 NA가 존재 치 않음을 확인하도록 한다.

    seqNumObj   <- k.seqNum( flag	,pCodeVal=codeVal )
    probObj     <- pPredObj$predict( seqNumObj )

    return( probObj )

} # seqAnaFun.default()

