# 20180109_B_Lab.R 실험실.

aux.FLogStr <- function( pStr ){
    k.FLogStr( pStr ,pFile="./log/B_Lab_Aux.log" )
} # aut.FLogStr()

rptRstLogLst <- function( pRstLogLst ,pEleSet ) {

    aux.FLogStr("= rptRstLogLst =================================================")

    hauntVal <- sapply( pRstLogLst ,function(p){p$hauntVal} )

    for( eIdx in seq_len(length(pEleSet)) ){ # 갯수만 갖고 얘기하자.
        hEnergy <- sapply( pRstLogLst ,function(p){
                             p$eleStatLst[[eIdx]]$energy
                        })
        energy.haunt <- hEnergy[ hauntVal==pEleSet[eIdx] ]
        energy.quiet <- hEnergy[ hauntVal!=pEleSet[eIdx] ]

        logStr <- sprintf( "hauntVal:%d  haunt E:%.3f  quiet E:%.3f" 
                            ,pEleSet[eIdx] 
                            ,mean(energy.haunt) ,mean(energy.quiet)
                    )
        aux.FLogStr( logStr )
    }

    aux.FLogStr("----------------------------------------------------------------")

} # rptRstLogLst() 

