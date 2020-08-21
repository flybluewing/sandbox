# B.rptStdMI.grp( stdMI.grp )

Fin.custCutLst <- list()

pName <- "basic"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      1 15 19 23 28 42    |14  4  4  5 14 |                        |1 2 2 0 1 |1 2 2 1
    #     19 22 30 34 39 44(1) | 3  8  4  5  5 | 18   7  11  11  11   2 |0 1 1 3 1 |1 1 3 1
    #      5 16 21 26 34 42(1) |11  5  5  8  8 |-14  -6  -9  -8  -5  -2 |1 1 2 1 1 |1 1 2 1 1
    #      5 17 18 22 23 43(1) |12  1  4  1 20 |  0   1  -3  -4 -11   1 |1 2 2 0 1 |1 2 2 1
    #      2  6 11 16 25 31    | 4  5  5  9  6 | -3 -11  -7  -6   2 -12 |2 2 1 1 0 |2 2 1 1
    #      1  4 10 14 15 35    | 3  6  4  1 20 | -1  -2  -1  -2 -10   4 |2 3 0 1 0 |2 3 1
    #   dup number  1:2   5:2   15:2   16:2   19:2   22:2   23:2   34:2   42:2
    #   zoid width  ... 41   25   37   38   29   34 and ?

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
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - lastZoid

        if( TRUE ){ # aZoid/aRem
            cutId <- "aRem[3:4]==c(4,4)"
            if( all(aRem[3:4]==c(4,4)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid test"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
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
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      1 15 19 23 28 42    |14  4  4  5 14 |                        |1 2 2 0 1 |1 2 2 1
    #     19 22 30 34 39 44(1) | 3  8  4  5  5 | 18   7  11  11  11   2 |0 1 1 3 1 |1 1 3 1
    #      5 16 21 26 34 42(1) |11  5  5  8  8 |-14  -6  -9  -8  -5  -2 |1 1 2 1 1 |1 1 2 1 1
    #      5 17 18 22 23 43(1) |12  1  4  1 20 |  0   1  -3  -4 -11   1 |1 2 2 0 1 |1 2 2 1
    #      2  6 11 16 25 31    | 4  5  5  9  6 | -3 -11  -7  -6   2 -12 |2 2 1 1 0 |2 2 1 1
    #      1  4 10 14 15 35    | 3  6  4  1 20 | -1  -2  -1  -2 -10   4 |2 3 0 1 0 |2 3 1
    #   dup number  1:2   5:2   15:2   16:2   19:2   22:2   23:2   34:2   42:2
    #   zoid width  ... 41   25   37   38   29   34 and ?

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
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - lastZoid

        if( TRUE ){ # aZoid/aRem
            cutId <- "aZoid test"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }

    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )

}



    # "basic" "nextZW" "nextQuo10" "nextBin" "nextRebNum" "nextCStepBin" "nextFStepBin" "nextColVal_1" "nextColVal_2" "nextColVal_3" "nextColVal_4" "nextColVal_5" "nextColVal_6"

