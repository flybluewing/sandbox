# 20180109_C_HUnit.R 교차모델

#   pECol : 예외대상 컬럼. Exception column
getUnitAnalyzer <- function( pMtx ,pECol=NULL ){

    dSize <- nrow(pMtx) ;dWidth <- ncol(pMtx)
    dLast <- pMtx[dSize,]
    dCStep <- dLast[2:dWidth] - dLast[1:(dWidth-1)]
    dFStep <- if( dSize>1 ) pMtx[dSize,]-pMtx[(dSize-1),] else NULL

    dInfo <- list( dLast=dLast ,dCStep=dCStep ,dFStep=dFStep ,dSize=dSize ,dWidth=dWidth )
    dInfo$eCol <- pECol

    funcLst.1st <- list()
    funcLst.1st[[1+length(funcLst.1st)]] <- get.UA0001



    funcLst.2nd <- list()

    uAnaObj <- list( funcLst.1st=funcLst.1st ,funcLst.2nd=funcLst.2nd )
    uAnaObj$dInfo <- dInfo

    return( uAnaObj )

} # getUnitAnalyzer()

#   2,3,4,5(?) 순차증가/감소
uaUtil.decline <- function( val ){

    val.len <- length(val)
    if( 3>val.len ){
        return( NULL )
    }

    fDiff <- val[ val.len + 0:-1 ] - val[ val.len + -1:-2 ]
    if( fDiff[1]!=fDiff[2] ){
        return( NULL )
    }

    banVal <- val[val.len] + fDiff[1]
    return(banVal)

} # uaUtil.decline()

#   2,2,3,3(?) 연속의 재발생
uaUtil.rebAgain <- function( val ){
    val.len <- length(val)
    if( 3>val.len ){
        return( NULL )
    }
    if( val[val.len-1]!=val[val.len-2] ){
        return( NULL )
    }
    banVal <- val[val.len]
    return(banVal)
} # uaUtil.decline()

#   2,3,4,2,3,4(?) 동일 패턴 재발
#       비슷한 값들로 인해 rebPtn2,rebPtn3,rebPtn4 중첩발생 가능
uaUtil.rebPtn1 <- function( val ){
    val.len <- length(val)
    if( 3>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0]==val[val.len-2]) ){
        return(val[val.len-1])
    }
    return( NULL )
} # uaUtil.decline1()
uaUtil.rebPtn2 <- function( val ){
    val.len <- length(val)
    if( 5>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0:1]==val[val.len-3:4]) ){
        return(val[val.len-2])
    }
    return( NULL )
} # uaUtil.decline2()
uaUtil.rebPtn3 <- function( val ){  
    val.len <- length(val)
    if( 7>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0:2]==val[val.len-4:6]) ){
        return(val[val.len-3])
    }
    return( NULL )
} # uaUtil.decline3()
uaUtil.rebPtn4 <- function( val ){
    val.len <- length(val)
    if( 9>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0:3]==val[val.len-5:8]) ){
        return(val[val.len-4])
    }
    return( NULL )
} # uaUtil.decline4()

#   1,2,3,2,1(?)
uaUtil.symm1 <- function( val ){
    val.len <- length(val)
    if( 3<=val.len ){ # 1,2,2,(1?)
        if( val[val.len]==val[val.len-1] ){
            return(val[val.len-2])
        }
    } else if( 4<=val.len ){ # 1,2,3,2,(1?)
        if( val[val.len]==val[val.len-2] ){
            return(val[val.len-3])
        }
    }
    return( NULL )
} # uaUtil.symm1()
uaUtil.symm2 <- function( val ){
    val.len <- length(val)
    if( 5<=val.len ){ # 1,2,3,3,2,(1?)
        if( val[val.len-0:1]==val[val.len-3:2] ){
            return(val[val.len-4])
        }
    } else if( 6<=val.len ){ # 1,2,3,4,3,2(1?)
        if( val[val.len-0:1]==val[val.len-4:3] ){
            return(val[val.len-5])
        }
    }
    return( NULL )
} # uaUtil.symm2()
uaUtil.symm3 <- function( val ){
    val.len <- length(val)
    if( 7<=val.len ){ # 1,2,3,4,4,3,2,(1?)
        if( val[val.len-0:2]==val[val.len-5:3] ){
            return(val[val.len-6])
        }
    } else if( 8<=val.len ){ # 1,2,3,4,5,4,3,2,(1?)
        if( val[val.len-0:2]==val[val.len-6:4] ){
            return(val[val.len-7])
        }
    }
    return( NULL )
} # uaUtil.symm3()

#   동일 패턴이 연속 발생.
uaUtil.seqReb <- function( pCodeMtx ,pECol=NULL ,pFirstOnly=T ){
    #  A,e,C,e,e
    #  e,A,e,C,e
    #  e,e,A,e,C
    code.len <- nrow(pCodeMtx)
    col.len <- ncol(pCodeMtx)
    if( 1>=code.len ){
        return( NULL )
    }

    fndLst <- list()
    for( cIdx in 0:(col.len-1) ){
        flag <- pCodeMtx[code.len ,1:(col.len-cIdx)]==pCodeMtx[code.len-1 ,(1+cIdx):col.len]
        if( !is.null(pECol) ){
            flag[pECol] <- FALSE
        }
        if( 1<sum(flag) ){
            val <- pCodeMtx[code.len ,1:(col.len-cIdx)]
            val[!flag] <- NA
            flagIdx <- which(flag)
            val <- val[ flagIdx[1]:flagIdx[length(flagIdx)] ]
            fndLst[[1+length(fndLst)]] <- val
            if(pFirstOnly){ 
                break 
            }
        }
    }

    return( NULL )
} # uaUtil.seqReb()

pCodeMtx <- c(1,2,3,2,2,0)
pCodeMtx <- rbind( pCodeMtx ,c(2,1,2,3,2,0) )
pCodeMtx <- rbind( pCodeMtx ,c(2,2,1,2,3,0) )





# 같은 컬럼에서 같은 값 반복.
get.UA0001 <- function( pMtx ,dInfo ){
    
    fObj <- list( idStr="UA0001" ,dInfo=dInfo )]
    
    #   반환값 0이면 필터링 결과 없음. 1이면 교차검증 대기. 2 이상이면 단독 필터링 가능.
    fObj$filt <- function( aZoid ){
        flag <- dInfo$dLast==aZoid
        if( !is.null(dInfo$eCol) ) flag <- flag[-dInfo$eCol]
        return( sum(flag) )
    } # fObj$filt()

    return( fObj )
} # ua.a0001()




