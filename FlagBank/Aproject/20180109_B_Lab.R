# 20180109_B_Lab.R 실험실.
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_B_H.R")


stdMtx <- zhF %% 4
flagThread <- stdMtx[,1]
measureSpan <- 1:200
# flagThread <- c(1:4,1:4,1:4)

eleSet  <- sort(unique(flagThread))  # element set 발생가능 경우들.
eleMean <- table(flagThread[measureSpan])/length(measureSpan)   # 발생 평균. 1/n에 해당하는 값이기도 하다.
eleMTTH    <- 1/eleMean    # mean time to haunt. (의외로 그리 많이 쓰이지는 않을 듯.)

eleStatLst <- list()
for( idx in 1:length(eleSet) ){
    eleObj <- list( val=eleSet[idx] ,salary=eleMean[idx] )
    eleObj$energy <- eleObj$salary  # 기본급. 즉 최소 발생확률은 항상 유지되어야 한다.
    eleObj$bank <- 0
    eleStatLst[[sprintf("ele%d",eleSet[idx])]] <- eleObj
}


flagThread <- c( 1 ,1 ,1 ,4 ,1    ,1 ,4 ,1 ,1 ,1
                ,1 ,4 ,4 ,1 ,1    ,4 ,4 ,1 ,1 ,1
                )

eleObj <- eleStatLst[[4]]
for( hIdx in 1:length(flagThread) ){
    # 어떤 것이 발생 했는지의 여부

    QQE:working
    
    # 발생/비발생에 따른 다음 발생 에너지 계산.
    #   다음 H에서의(hIdx+1) 발생 에너지를 가늠한다.
    if( 4==flagThread[hIdx] ){
        eleObj <- bank.haunt( eleObj )
    } else{
        eleObj <- bank.quiet( eleObj )
    }
    k.FLogStr(sprintf("[%s]hIdx:%2d energy:%.2f bank:%.2f    salary:%.2f" 
                ,ifelse( 4==flagThread[hIdx] ,"T" ,"F" )
                ,hIdx   ,eleObj$energy ,eleObj$bank ,eleObj$salary
            ))
}



bank.haunt <- function( pEleObj ) {     # 발생 시 뱅킹처리.
    rEleObj <- pEleObj
    
    rEleObj$energy <- (rEleObj$energy+rEleObj$bank) - 1.0
    rEleObj$bank <- 0.0                 # 계좌 밸런스 정리.
    if( 1.00001 < rEleObj$energy ){     # 빼고나서도 에너지가 남으면 저축.
        rEleObj$bank <- rEleObj$energy <- 1.0
        rEleObj$energy <- 1.0
    } else if( 0 > rEleObj$energy ){    # 발생 에너지 부족하면 대출 추가.
        rEleObj$bank <- rEleObj$energy
        rEleObj$energy <- 0.0
    }

    rEleObj$energy <- rEleObj$energy + rEleObj$salary   # 발생 시엔 월급 없다.

    return( rEleObj )
} # bank.haunt()

bank.quiet <- function( pEleObj ) {     # 미발생 시 뱅킹처리.
    rEleObj <- pEleObj
    rEleObj$energy <- (rEleObj$energy+rEleObj$bank) + rEleObj$salary
    rEleObj$bank <- 0.0                 # 계좌 밸런스 정리.
    if( 1.00001 < rEleObj$energy ) {    # 발생 에너지가 1.0 넘으면 저축.
        rEleObj$bank <- rEleObj$energy - 1.0
        rEleObj$energy <- 1.0
    } else if( 0 > rEleObj$energy ) {   # 아직도 갚아야 할 대출금이 남았다.
        rEleObj$bank    <- rEleObj$energy - rEleObj$salary  # 이번 월급만 유지하고 나머지는 대출금.
        rEleObj$energy  <- rEleObj$salary
    }

    return( rEleObj )
} # bank.haunt()

