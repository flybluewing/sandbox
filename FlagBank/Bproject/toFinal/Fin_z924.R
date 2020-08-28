# B.rptStdMI.grp( stdMI.grp )

Fin.custCutLst <- list()

pName <- "basic"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      9 14 17 18 42 44    | 5  3  1 24  2 |                        |1 3 0 0 2 |1 3 2
    #      2  3 26 33 34 43    | 1 23  7  1  9 | -7 -11   9  15  -8  -1 |2 0 1 2 1 |2 1 2 1
    #      5  7 12 22 28 41    | 2  5 10  6 13 |  3   4 -14 -11  -6  -2 |2 1 2 0 1 |2 1 2 1
    #      2  6 13 17 27 43    | 4  7  4 10 16 | -3  -1   1  -5  -1   2 |2 2 1 0 1 |2 2 1 1
    #      3 17 18 23 36 41(1) |14  1  5 13  5 |  1  11   5   6   9  -2 |1 2 1 1 1 |1 2 1 1 1
    #      3 11 34 42 43 44(1) | 8 23  8  1  1 |  0  -6  16  19   7   3 |1 1 0 1 3 |1 1 1 3
    #   dup number  2:2   3:3   17:3   18:2   34:2   41:2   42:2   43:3   44:2
    #   zoid width  ... 35   41   36   41   38   41 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
        aZw <- aZoid[6]-aZoid[1]

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid[5]==41"
            if( aZoid[5]==41 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[5:6]==c(34,43)"
            if( all(aZoid[5:6]==c(34,43)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[1:2]==c( 3,11)"
            if( all(aZoid[1:2]==c( 3,11)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[2:3]==c(.N,N.)" # (23,36)   (34,42)   (.N,N.)
            if( aRem[2]==aZoid[3]%/%10 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[4:6] Q3 seq"
            qVal <- aZoid[4:6]%/%10
            if( all(qVal[1]==qVal[2:3]) && all(aCStep[4:5]==1) ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }

            cutId <- "aZw==36 && all(aZoid[c(1,4,6)]==c( 5,22,41))" #  5  7 12 22 28 41 
            if( aZw==36 && all(aZoid[c(1,4,6)]==c( 5,22,41)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aCStep

            cutId <- "aCStep[4:5]==c(1,1)"
            if( all(aCStep[4:5]==c(1,1)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[c(3,5)]==c(9,9)"   # cStep[4,c(1,3)] / cStep[5,c(3,5)]
            if( all(aCStep[c(3,5)]==c(9,9)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==36 && matCnt>2"
            matCnt <- sum( aCStep==c( 2 ,5,10 ,6,13) )  # stdMI$cStepTail[3,]
            if( aZw==36 && matCnt>2 ){
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
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      3  5 12 13 33 39    | 2  7  1 20  6 |                        |2 2 0 2 0 |2 2 2
    #     19 22 30 34 39 44(1) | 3  8  4  5  5 | 16  17  18  21   6   5 |0 1 1 3 1 |1 1 3 1
    #      1  3 24 27 39 45(1) | 2 21  3 12  6 |-18 -19  -6  -7   0   1 |2 0 2 1 1 |2 2 1 1
    #      7 24 29 30 34 35(1) |17  5  1  4  1 |  6  21   5   3  -5 -10 |1 0 2 3 0 |1 2 3
    #      5  7 12 22 28 41(1) | 2  5 10  6 13 | -2 -17 -17  -8  -6   6 |2 1 2 0 1 |2 1 2 1
    #      3 17 18 23 36 41(1) |14  1  5 13  5 | -2  10   6   1   8   0 |1 2 1 1 1 |1 2 1 1 1
    #   dup number  3:3   5:2   7:2   12:2   22:2   24:2   30:2   34:2   39:3   41:2
    #   zoid width  ... 36   25   44   28   36   38 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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

            cutId <- "aZoid[5:6]==c(36,41)" # 41,41,?
            if( all(aZoid[5:6]==c(36,41)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[5:6]==c(.N,N.)" # (12,22)   (23,36)   (.N,N.)
            if( aRem[5]==aZoid[6]%/%10 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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

            cutId <- "aFStep[1:2]==c(-2,10)"
            if( all(aFStep[1:2]==c(-2,10)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aFStep[1:2]==c( 6,21)"
            if( all(aFStep[1:2]==c( 6,21)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      1  7 16 18 34 38    | 6  9  2 16  4 |                        |2 2 0 2 0 |2 2 2
    #      1 28 35 41 43 44(1) |27  7  6  2  1 |  0  21  19  23   9   6 |1 0 1 1 3 |1 1 1 3
    #     12 14 21 30 39 43(1) | 2  7  9  9  4 | 11 -14 -14 -11  -4  -1 |0 2 1 2 1 |2 1 2 1
    #     13 16 24 25 33 36    | 3  8  1  8  3 |  1   2   3  -5  -6  -7 |0 2 2 2 0 |2 2 2
    #     12 16 26 28 30 42(1) | 4 10  2  2 12 | -1   0   2   3  -3   6 |0 2 2 1 1 |2 2 1 1
    #     14 17 19 22 24 40    | 3  2  3  2 16 |  2   1  -7  -6  -6  -2 |0 3 2 0 1 |3 2 1
    #   dup number  1:2   12:2   14:2   16:3   24:2   28:2   30:2   43:2
    #   zoid width  ... 37   43   31   23   30   26 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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

            cutId <- "aCStep[c(1,3,4)] (6,n,n)"
            if( (aCStep[1]==6) && (aCStep[3]==aCStep[4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[c(1,2,4)] (3,n,n)"
            if( (aCStep[1]==3) && (aCStep[2]==aCStep[4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[3:5]==c( 3, 2,16)" # CStep¿¡¼­ 16Àº ÈçÄ¡ ¾ÊÀÝ³ª?
            if( all(aCStep[3:5]==c( 3, 2,16)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      2 11 12 15 23 37    | 9  1  3  8 14 |                        |1 3 1 1 0 |1 3 1 1
    #      5  7  9 11 32 35(1) | 2  2  2 21  3 |  3  -4  -3  -4   9  -2 |3 1 0 2 0 |3 1 2
    #     10 14 19 39 40 43    | 4  5 20  1  3 |  5   7  10  28   8   8 |0 3 0 1 2 |3 1 2
    #      3 10 13 22 31 32(1) | 7  3  9  9  1 | -7  -4  -6 -17  -9 -11 |1 2 1 2 0 |1 2 1 2
    #     14 26 32 36 39 42(1) |12  6  4  3  3 | 11  16  19  14   8  10 |0 1 1 3 1 |1 1 3 1
    #      5 18 20 23 30 34    |13  2  3  7  4 | -9  -8 -12 -13  -9  -8 |1 1 2 2 0 |1 1 2 2
    #   dup number  5:2   10:2   11:2   14:2   23:2   32:3   39:2
    #   zoid width  ... 35   30   33   29   28   29 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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

            cutId <- "aRem[c(3,5)]==c(0,0) && (aRem[4]+1)==aRem[6]"
            qVal <- aZoid %/% 10
            if( all(aRem[c(3,5)]==c(0,0)) && ((aRem[4]+1)==aRem[6]) && all(qVal[c(3,5)]==qVal[c(4,6)]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==28 && aRem[c(3,5)]==c(0,0)"
            if( aZw==28 && aRem[c(3,5)]==c(0,0) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep[c(1,3,5)]==c(14,2,5)"
            if( all(aCStep[c(1,3,5)]==c(14,2,5)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep[c(2,4,6)] aFStep[c(1,3,5)]"
            fCStep <- aFStep[c(2,4,6)] - aFStep[c(1,3,5)]
            if( all(fCStep==1) ){
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


pName <- "nextRebNum"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){
    # *** nextRebNum ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #     15 18 21 32 35 44    | 3  3 11  3  9 |                        |0 2 1 2 1 |2 1 2 1
    #     13 19 28 37 38 43    | 6  9  9  1  5 | -2   1   7   5   3  -1 |0 2 1 2 1 |2 1 2 1
    #      3 11 14 15 32 36    | 8  3  1 17  4 |-10  -8 -14 -22  -6  -7 |1 3 0 2 0 |1 3 2
    #      6 16 37 38 41 45    |10 21  1  3  4 |  3   5  23  23   9   9 |1 1 0 2 2 |1 1 2 2
    #      2 10 11 19 35 39    | 8  1  8 16  4 | -4  -6 -26 -19  -6  -6 |1 3 0 2 0 |1 3 2
    #     12 18 19 29 31 39(2) | 6  1 10  2  8 | 10   8   8  10  -4   0 |0 3 1 2 0 |3 1 2
    #   dup number  11:2   15:2   18:2   19:3   32:2   35:2   37:2   38:2   39:2
    #   zoid width  ... 29   30   33   39   37   27 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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

            cutId <- "aZoid[5:6]==c(31,39)"
            if( all(aZoid[5:6]==c(31,39)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aRem[c(3,4,6)]==9"
            if( all(aRem[c(3,4,6)]==9) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aRem[c(1,4,6)]==c(2,9,9)"
            if( all(aRem[c(1,4,6)]==c(2,9,9)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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
            cutId <- "all(aCStep[c(2,5)]==c(1,8)) && any(aCStep[c(1,3)]==c(4,12))"
            if( all(aCStep[c(2,5)]==c(1,8)) && any(aCStep[c(1,3)]==c(4,12)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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
    # *** nextCStepBin ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #     12 15 16 20 24 30    | 3  1  4  4  6 |                        |0 3 2 1 0 |3 2 1
    #     14 26 32 36 39 42    |12  6  4  3  3 |  2  11  16  16  15  12 |0 1 1 3 1 |1 1 3 1
    #      9 18 32 33 37 44(1) | 9 14  1  4  7 | -5  -8   0  -3  -2   2 |1 1 0 3 1 |1 1 3 1
    #      3  7 12 31 34 38    | 4  5 19  3  4 | -6 -11 -20  -2  -3  -6 |2 1 0 3 0 |2 1 3
    #      5 18 20 23 30 34(1) |13  2  3  7  4 |  2  11   8  -8  -4  -4 |1 1 2 2 0 |1 1 2 2
    #      6 21 22 32 35 36    |15  1 10  3  1 |  1   3   2   9   5   2 |1 0 2 3 0 |1 2 3
    #   dup number  12:2   18:2   20:2   30:2   32:3   34:2   36:2
    #   zoid width  ... 18   28   35   35   29   30 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
            cutId <- "all(aCStep[c(2,5)]==1) && (aCStep[1]==sum(aCStep[2:5]))"
            if( all(aCStep[c(2,5)]==1) && (aCStep[1]==sum(aCStep[2:5])) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "all(aFStep[c(3,6)]==2) && (aFStep[4]==sum(aFStep[c(3,5,6)]))"
            if( all(aFStep[c(3,6)]==2) && (aFStep[4]==sum(aFStep[c(3,5,6)])) ){
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


pName <- "nextFStepBin"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){
    # *** nextFStepBin ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      4 13 18 31 33 45    | 9  5 13  2 12 |                        |1 2 0 2 1 |1 2 2 1
    #     14 21 29 31 32 37(1) | 7  8  2  1  5 | 10   8  11   0  -1  -8 |0 1 2 3 0 |1 2 3
    #      3  4  9 24 25 33    | 1  5 15  1  8 |-11 -17 -20  -7  -7  -4 |3 0 2 1 0 |3 2 1
    #     12 14 21 30 39 43    | 2  7  9  9  4 |  9  10  12   6  14  10 |0 2 1 2 1 |2 1 2 1
    #      2  8 33 35 37 41    | 6 25  2  2  4 |-10  -6  12   5  -2  -2 |2 0 0 3 1 |2 3 1
    #     10 24 40 41 43 44(1) |14 16  1  2  1 |  8  16   7   6   6   3 |0 1 1 0 4 |1 1 4
    #   dup number  4:2   14:2   21:2   24:2   31:2   33:3   37:2   41:2   43:2
    #   zoid width  ... 41   23   30   31   39   34 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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

            cutId <- "quo4 reb"
            tbl <- table(aZoid%/%10)
            if( any(tbl==4) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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
            cutId <- "all(aCStep[3:5]==c(1,2,1)) && all(aCStep[1:2]>13)"
            if( all(aCStep[3:5]==c(1,2,1)) && all(aCStep[1:2]>13) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep"
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
    # *** nextColVal_1 ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      1 15 19 23 28 42    |14  4  4  5 14 |                        |1 2 2 0 1 |1 2 2 1
    #      3 13 29 38 39 42(1) |10 16  9  1  3 |  2  -2  10  15  11   0 |1 1 1 2 1 |1 1 1 2 1
    #      1  4 14 18 29 37(1) | 3 10  4 11  8 | -2  -9 -15 -20 -10  -5 |2 2 1 1 0 |2 2 1 1
    #      2  5 14 28 31 32(1) | 3  9 14  3  1 |  1   1   0  10   2  -5 |2 1 1 2 0 |2 1 1 2
    #      7 24 29 30 34 35    |17  5  1  4  1 |  5  19  15   2   3   3 |1 0 2 3 0 |1 2 3
    #      3 11 34 42 43 44(1) | 8 23  8  1  1 | -4 -13   5  12   9   9 |1 1 0 1 3 |1 1 1 3
    #   dup number  1:2   3:2   14:2   28:2   29:3   34:2   42:3
    #   zoid width  ... 41   39   36   30   28   41 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
        aZw <- aZoid[6]-aZoid[1]

        if( TRUE ){ # aZoid/aRem

            rem11 <- aZoid %% 11
            cutId <- "all(rem11[c(2,6)]==0) && (aCStep[4]==aCStep[5])"
            if( all(rem11[c(2,6)]==0) && (aCStep[4]==aCStep[5]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==39 && aZoid[6]==44"
            if( all(rem11[c(2,6)]==0) && (aCStep[4]==aCStep[5]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aCStep
            cutId <- "aZw==39 && aCStep[1]==aCStep[3]"
            if( aZw==39 && aCStep[1]==aCStep[3] ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==39 && aCStep[2]==23"
            if( aZw==39 && aCStep[1]==aCStep[3] ){
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
    # *** nextColVal_2 ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      5 10 13 21 39 43    | 5  3  8 18  4 |                        |1 2 1 1 1 |1 2 1 1 1
    #      1  3 12 14 16 43(1) | 2  9  2  2 27 | -4  -7  -1  -7 -23   0 |2 3 0 0 1 |2 3 1
    #      4  7 13 29 31 39    | 3  6 16  2  8 |  3   4   1  15  15  -4 |2 1 1 2 0 |2 1 1 2
    #     14 26 32 36 39 42(1) |12  6  4  3  3 | 10  19  19   7   8   3 |0 1 1 3 1 |1 1 3 1
    #      4  5 12 14 32 42(3) | 1  7  2 18 10 |-10 -21 -20 -22  -7   0 |2 2 0 1 1 |2 2 1 1
    #      9 14 17 18 42 44(2) | 5  3  1 24  2 |  5   9   5   4  10   2 |1 3 0 0 2 |1 3 2
    #   dup number  4:2   5:2   12:2   13:2   14:4   32:2   39:3   42:3   43:2
    #   zoid width  ... 38   42   35   28   38   35 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
        aZw <- aZoid[6]-aZoid[1]

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid[5]==44 && all(aRem[3:4]==c(4,2))"
            if( aZoid[5]==44 && all(aRem[3:4]==c(4,2)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep[4]==24 && (rebNum>=2)"
            rebNum <- sum(aCStep==c( 5, 3, 1,24, 2))
            if( aCStep[4]==24 && (rebNum>=2) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[4:5]==c(3,3) && (aZw==28)"
            if( aCStep[4:5]==c(3,3) && (aZw==28) ){
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
    # *** nextColVal_3 ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      3  4  9 11 22 42    | 1  5  2 11 20 |                        |3 1 1 0 1 |3 1 1 1
    #     15 17 19 34 38 41    | 2  2 15  4  3 | 12  13  10  23  16  -1 |0 3 0 2 1 |3 2 1
    #      5 14 27 30 39 43    | 9 13  3  9  4 |-10  -3   8  -4   1   2 |1 1 1 2 1 |1 1 1 2 1
    #      6  8 14 21 30 37(2) | 2  6  7  9  7 |  1  -6 -13  -9  -9  -6 |2 1 1 2 0 |2 1 1 2
    #      5 15 20 31 34 42    |10  5 11  3  8 | -1   7   6  10   4   5 |1 1 1 2 1 |1 1 1 2 1
    #      2 21 28 38 42 45(1) |19  7 10  4  3 | -3   6   8   7   8   3 |1 0 2 1 2 |1 2 1 2
    #   dup number  5:2   14:2   15:2   21:2   30:2   34:2   38:2   42:3
    #   zoid width  ... 39   26   38   31   37   43 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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

            cutId <- "aRem[3:5]==c(8,8,5)"
            if( all(aRem[3:5]==c(8,8,5)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep[1]==19 && all(aCStep[3:4]==c(9,5))"
            if( aCStep[1]==19 && all(aCStep[3:4]==c(9,5)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep"
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
    # *** nextColVal_4 ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      4 15 17 26 36 37    |11  2  9 10  1 |                        |1 2 1 2 0 |1 2 1 2
    #      7  9 15 26 27 42(2) | 2  6 11  1 15 |  3  -6  -2   0  -9   5 |2 1 2 0 1 |2 1 2 1
    #     11 13 15 17 25 34(1) | 2  2  2  8  9 |  4   4   0  -9  -2  -8 |0 4 1 1 0 |4 1 1
    #   dup number  15:3   17:2   26:2
    #   zoid width  ... 33   35   23 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
            cutId <- "all(aCStep[1]==aCStep[2:3]) && ((aCStep[4]+1)==aCStep[5])"
            if( all(aCStep[1]==aCStep[2:3]) && ((aCStep[4]+1)==aCStep[5]) ){
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
    # *** nextColVal_5 ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      5  6 16 18 37 38    | 1 10  2 19  1 |                        |2 2 0 2 0 |2 2 2
    #     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  10  10  10  -7   4 |0 2 2 1 1 |2 2 1 1
    #     10 24 40 41 43 44    |14 16  1  2  1 | -2   8  14  13  13   2 |0 1 1 0 4 |1 1 4
    #      6 10 16 28 34 38(1) | 4  6 12  6  4 | -4 -14 -24 -13  -9  -6 |1 2 1 2 0 |1 2 1 2
    #      2  6 20 27 37 39(1) | 4 14  7 10  2 | -4  -4   4  -1   3   1 |2 0 2 2 0 |2 2 2
    #      3  4 16 27 38 40(1) | 1 12 11 11  2 |  1  -2  -4   0   1   1 |2 1 1 1 1 |2 1 1 1 1
    #   dup number  6:3   10:2   16:4   27:2   28:2   37:2   38:3   40:2
    #   zoid width  ... 33   30   34   32   37   37 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
        aZW <- aZoid[6] - aZoid[1]

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid[c(1,3,4)]==c( 4,27,39)"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[3]==27 && aZw==37"
            if( aZoid[3]==27 && aZw==37 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aCStep
            cutId <- "all(aCStep[3:4]>10) && (aCStep[3]==aCStep[4])"
            if( all(aCStep[3:4]>10) && (aCStep[3]==aCStep[4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "(aCStep[3]==aCStep[4]) && all(aCStep[c(1,5)]==c(1,2))"
            if( (aCStep[3]==aCStep[4]) && all(aCStep[c(1,5)]==c(1,2)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==37 && all(aCStep[c(1,5)]==c(1,2))"
            if( aZw==37 && all(aCStep[c(1,5)]==c(1,2)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==37 && (aCStep[3]==aCStep[4])"
            if( aZw==37 && (aCStep[3]==aCStep[4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "all(cRem[1]==cRem[3:4]) && (cRem[2]==cRem[5])"
            cRem <- aCStep %/% 10
            if( all(cRem[1]==cRem[3:4]) && (cRem[2]==cRem[5]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep[4:6]==c(-4, 1, 1)"
            if( all(aFStep[4:6]==c(-4, 1, 1)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "all(aFStep[4:5]==c(-4, 1)) && aZW==37"
            if( all(aFStep[4:6]==c(-4, 1, 1)) ){
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


pName <- "nextColVal_6"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){
    # *** nextColVal_6 ***
    #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
    #      5 16 21 26 34 42    |11  5  5  8  8 |                        |1 1 2 1 1 |1 1 2 1 1
    #      4 14 23 28 37 45    |10  9  5  9  8 | -1  -2   2   2   3   3 |1 1 2 1 1 |1 1 2 1 1
    #      3 16 21 22 23 44(1) |13  5  1  1 21 | -1   2  -2  -6 -14  -1 |1 1 3 0 1 |1 1 3 1
    #      7 24 29 30 34 35    |17  5  1  4  1 |  4   8   8   8  11  -9 |1 0 2 3 0 |1 2 3
    #      2  6 11 13 22 37    | 4  5  2  9 15 | -5 -18 -18 -17 -12   2 |2 2 1 1 0 |2 2 1 1
    #      2  3 26 33 34 43(1) | 1 23  7  1  9 |  0  -3  15  20  12   6 |2 0 1 2 1 |2 1 2 1
    #   dup number  2:2   3:2   16:2   21:2   22:2   23:2   26:2   34:3   37:2
    #   zoid width  ... 37   41   41   28   35   41 and ?

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){ surFlag <- rep( T ,aLen )   }

    cutInfoLst <- lapply( seq_len(aLen) ,function(p){return(list())} )
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
        aZw <- aZoid[6] - aZoid[1]

        if( TRUE ){ # aZoid/aRem

            rem11 <- aZoid %% 11


            cutId <- "(aZoid[1]==2) && all(rem11[c(3,5)]==0)"
            if( (aZoid[1]==2) && all(rem11[c(3,5)]==0) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

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
            cutId <- "aCStep[2]>20 && (aCStep[1]==aCStep[4])"
            if( aCStep[2]>20 && (aCStep[1]==aCStep[4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[2]==23 && aCStep[5]==1"
            if( TRUE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && all(aCStep[2:4]==c(5,1,1))"    #   stdMI$cStepTail[3]
            if( aZw==41 && all(aCStep[2:4]==c(5,1,1)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && aCStep[2]==23"
            if( aZw==41 && aCStep[2]==23 ){
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


