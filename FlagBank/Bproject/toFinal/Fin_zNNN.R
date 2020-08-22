# B.rptStdMI.grp( stdMI.grp )

Fin.custCutLst <- list()

pName <- "basic"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextZW"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextQuo10"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextBin"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextRebNum"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextCStepBin"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextFStepBin"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextColVal_1"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextColVal_2"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextColVal_3"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextColVal_4"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextColVal_5"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


pName <- "nextColVal_6"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )
    if( 0==stdMI$mtxLen ){
        cutRst <- list( surFlag=surFlag )
        if( anaOnly ){
            cutRst$cutInfoLst <- cutInfoLst
        }
        return( cutRst )
    }


    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}


