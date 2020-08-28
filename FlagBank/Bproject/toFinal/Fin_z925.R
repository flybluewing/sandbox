# B.rptStdMI.grp( stdMI.grp )

Fin.custCutLst <- list()

pName <- "basic"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** basic ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      2  3 26 33 34 43    | 1 23  7  1  9 |                        |2 0 1 2 1 |2 1 2 1
        #      5  7 12 22 28 41    | 2  5 10  6 13 |  3   4 -14 -11  -6  -2 |2 1 2 0 1 |2 1 2 1
        #      2  6 13 17 27 43    | 4  7  4 10 16 | -3  -1   1  -5  -1   2 |2 2 1 0 1 |2 2 1 1
        #      3 17 18 23 36 41(1) |14  1  5 13  5 |  1  11   5   6   9  -2 |1 2 1 1 1 |1 2 1 1 1
        #      3 11 34 42 43 44(1) | 8 23  8  1  1 |  0  -6  16  19   7   3 |1 1 0 1 3 |1 1 1 3
        #     13 24 32 34 39 42(2) |11  8  2  5  3 | 10  13  -2  -8  -4  -2 |0 1 1 3 1 |1 1 3 1
        #   dup number  2:2   3:3   13:2   17:2   34:3   41:2   42:2   43:3
        #   zoid width  ... 41   36   41   38   41   29 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid[1]==13 && any(aZw==c(29,41))" # aZoid[1] 만으론 약해서 ZW로 보강
            if( aZoid[1]==13 && any(aZw==c(29,41)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && all(aZoid[c(1,6)]==c( 3,44))"
            if( aZw==41 && all(aZoid[c(1,6)]==c( 3,44)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && all(aZoid[c(1,6)]==c( 4,45))"
            if( aZw==41 && all(aZoid[c(1,6)]==c( 3,44)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aRem[1:3]==c(4,2,5)"
            if( all(aRem[1:3]==c(4,2,5)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aRem[1:2]==c(4,2) &&  any(aZw==c(29,41))"
            if( all(aRem[1:2]==c(4,2)) &&  any(aZw==c(29,41)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && all(rem11[c(2,6)]==0)"
            rem11 <- aZoid%%11
            if( aZw==41 && all(rem11[c(2,6)]==0) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aZw==41 && (aCStep[1]==aCStep[3])"
            if( aZw==41 && (aCStep[1]==aCStep[3]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && (aCStep[4]==aCStep[5]) && aCStep[2]==23"
            if( aZw==41 && (aCStep[4]==aCStep[5]) && aCStep[2]==23 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[c(1,3)]==12 && aZw==29"
            if( all(aCStep[c(1,3)]==12) && aZw==29 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw%in%c(41,29) && aCStep[4]==sum35 && aCStep[2]==sum45"
            sum35 <- aCStep[3]+aCStep[5]
            sum25 <- aCStep[4]+aCStep[5]
            if( "aZw%in%c(41,29) && aCStep[4]==sum35 && aCStep[2]==sum45" ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "all(aFStep[4:6]==rat3)"
            rat3 <- aFStep[3]*c(4,2,1)
            if( all(aFStep[4:6]==rat3) ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextZW ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #     14 20 23 31 37 38    | 6  3  8  6  1 |                        |0 1 2 3 0 |1 2 3
        #     15 21 31 32 41 43(1) | 6 10  1  9  2 |  1   1   8   1   4   5 |0 1 1 2 2 |1 1 2 2
        #     10 21 22 30 35 42(1) |11  1  8  5  7 | -5   0  -9  -2  -6  -1 |0 1 2 2 1 |1 2 2 1
        #      3  9 11 12 13 19    | 6  2  1  1  6 | -7 -12 -11 -18 -22 -23 |2 4 0 0 0 |2 4
        #      1  4 10 14 15 35    | 3  6  4  1 20 | -2  -5  -1   2   2  16 |2 3 0 1 0 |2 3 1
        #      7 19 23 24 36 39    |12  4  1 12  3 |  6  15  13  10  21   4 |1 1 2 2 0 |1 1 2 2
        #   dup number  10:2   14:2   15:2   19:2   21:2   23:2   31:2   35:2
        #   zoid width  ... 24   28   32   16   34   32 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "all(aCStep[c(1,3)]>10) && aCStep[1]==aCStep[3]"
            if( all(aCStep[c(1,3)]>10) && aCStep[1]==aCStep[3] ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( FALSE ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1]     ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextQuo10 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      4  8 18 25 27 32    | 4 10  7  2  5 |                        |2 1 2 1 0 |2 1 2 1
        #      5 16 21 26 34 42    |11  5  5  8  8 |  1   8   3   1   7  10 |1 1 2 1 1 |1 1 2 1 1
        #     18 34 39 43 44 45(1) |16  5  4  1  1 | 13  18  18  17  10   3 |0 1 0 2 3 |1 2 3
        #      4 14 23 28 37 45(1) |10  9  5  9  8 |-14 -20 -16 -15  -7   0 |1 1 2 1 1 |1 1 2 1 1
        #      5 12 25 26 38 45(1) | 7 13  1 12  7 |  1  -2   2  -2   1   0 |1 1 2 1 1 |1 1 2 1 1
        #      7 24 29 30 34 35    |17  5  1  4  1 |  2  12   4   4  -4 -10 |1 0 2 3 0 |1 2 3
        #   dup number  4:2   5:2   18:2   25:2   26:2   34:3   45:3
        #   zoid width  ... 28   37   27   41   40   28 and ?
        zwFlag <- aZw==c(28,37) ;names(zwFlag)=c("last","next")
        qFlag2 <- all(stdMI$quoTail[2,]==aQuo$size)
        qFlag4 <- all(stdMI$quoTail[4,]==aQuo$size)
        qFlag5 <- all(stdMI$quoTail[5,]==aQuo$size)
        qFlag6 <- all(stdMI$quoTail[6,]==aQuo$size)
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZw==37 && aZoid[6]==35"
            if( aZw==37 && aZoid[6]==35 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "quo10Flag6 && any(aZw==c(28,37))"
            quo10Flag6 <- all(stdMI$quoTail[6,]==aQuo$size)
            if( quo10Flag6 && any(aZw==c(28,37)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "quo10Flag5 && any(aZw==c(28,37))"
            quo10Flag5 <- all(stdMI$quoTail[5,]==aQuo$size)
            if( quo10Flag5 && any(aZw==c(28,37)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "quo10Flag5 && all(aZoid[c(1,4)]==c( 5,26))"
            quo10Flag5 <- all(stdMI$quoTail[5,]==aQuo$size)
            if( quo10Flag5 && any(aZw==c(28,37)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag4 && all(aZoid[c(1,4)]==c( 4,28)) && aZw==37"
            qFlag4 <- all(stdMI$quoTail[4,]==aQuo$size)
            if( qFlag4 && all(aZoid[c(1,4)]==c( 4,28)) && aZw==37 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep

            cutId <- "qFlag6 && all(aCStep[c(3,5)]==1)"
            if( qFlag6 && all(aCStep[c(3,5)]==1) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag6 && zwFlag[\"last\"]"
            if( qFlag6 && zwFlag["last"] ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && qFlag5 && all(aCStep[c(1,5)]==7)"
            if( any(zwFlag) && qFlag5 && all(aCStep[c(1,5)]==7) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && qFlag4 && all(aCStep[c(2,4)]==9)"
            if( any(zwFlag) && qFlag4 && all(aCStep[c(2,4)]==9) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && qFlag4 && all(aCStep[c(3,5)]==c(5,8))"
            if( any(zwFlag) && qFlag4 && all(aCStep[c(3,5)]==c(5,8)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && qFlag5 && all(aCStep[c(3,5)]==c(1,7))"
            if( any(zwFlag) && qFlag5 && all(aCStep[c(3,5)]==c(1,7)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && qFlag5 && all(aCStep[c(2,4)]==c(13,12))"
            if( any(zwFlag) && qFlag5 && all(aCStep[c(3,5)]==c(1,7)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && qFlag2 && all(aCStep[c(2,4)]==aCStep[c(3,5)])"
            if( any(zwFlag) && qFlag2 && all(aCStep[c(2,4)]==aCStep[c(3,5)]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep

            cutId <- "any(zwFlag) && qFlag5 && all(aFStep[1:2]==aFStep[5:4])"
            if( FALSE ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aFStep test"
            if( FALSE ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextBin ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      9 15 16 21 28 34    | 6  1  5  7  6 |                        |1 2 2 1 0 |1 2 2 1
        #      6 15 22 23 25 32(1) | 9  7  1  2  7 | -3   0   6   2  -3  -2 |1 1 3 1 0 |1 1 3 1
        #      3 13 15 40 41 44(1) |10  2 25  1  3 | -3  -2  -7  17  16  12 |1 2 0 0 3 |1 2 3
        #     12 18 24 26 39 40(1) | 6  6  2 13  1 |  9   5   9 -14  -2  -4 |0 2 2 1 1 |2 2 1 1
        #      3 10 16 19 31 39(1) | 7  6  3 12  8 | -9  -8  -8  -7  -8  -1 |1 3 0 2 0 |1 3 2
        #      5 16 21 26 34 42(1) |11  5  5  8  8 |  2   6   5   7   3   3 |1 1 2 1 1 |1 1 2 1 1
        #   dup number  3:2   15:3   16:3   21:2   26:2   34:2   39:2   40:2
        #   zoid width  ... 25   26   41   28   36   37 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "all(aZoid[c(1,3)]==c( 3,17)) && aZw==38"
            if( all(aZoid[c(1,3)]==c( 3,17)) && aZw==38 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "all(aRem[c(2,4)]==1) && any(aRem[c(1,5)]==c(3,1))"
            if( all(aRem[c(2,4)]==1) && aRem[5]==1 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "all(aRem[c(2,4)]==1) && aZw==38"
            if( all(aRem[c(2,4)]==1) && aZw==38 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "all(aCStep[c(2,4)]==aCStep[c(3,5)])"
            if( all(aCStep[c(2,4)]==aCStep[c(3,5)]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[1]==11 && aZw==38 && aCStep[2]==aCStep[3]"
            if( aCStep[1]==11 && aZw==38 && aCStep[2]==aCStep[3] ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[1]==11 && aZw==38 && aCStep[4]==aCStep[5]"
            if( aCStep[1]==11 && aZw==38 && aCStep[4]==aCStep[5] ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep[5]==aFStep[6] && aZw==38"
            if( aFStep[5]==aFStep[6] && aZw==38 ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1]     ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextRebNum ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      3 22 25 29 32 44    |19  3  4  3 12 |                        |1 0 3 1 1 |1 3 1 1
        #     12 13 17 22 25 33(2) | 1  4  5  3  8 |  9  -9  -8  -7  -7 -11 |0 3 2 1 0 |3 2 1
        #      2  7 26 29 40 43    | 5 19  3 11  3 |-10  -6   9   7  15  10 |2 0 2 0 2 |2 2 2
        #      5  6 19 26 41 45(1) | 1 13  7 15  4 |  3  -1  -7  -3   1   2 |2 1 1 0 2 |2 1 1 2
        #      2  5  6 13 28 44(2) | 3  1  7 15 16 | -3  -1 -13 -13 -13  -1 |3 1 1 0 1 |3 1 1 1
        #      2 12 19 24 39 44(2) |10  7  5 15  5 |  0   7  13  11  11   0 |1 2 1 1 1 |1 2 1 1 1
        #   dup number  2:3   5:2   6:2   12:2   13:2   19:2   22:2   25:2   26:2   29:2   44:3
        #   zoid width  ... 41   21   41   40   42   42 and ?
        zwFlag <- aZw==c(42,40) ;names(zwFlag)=c("last","symm")
        qFlag <- apply(stdMI$quoTail ,1 ,function(qDat){all(qDat==aQuo$size)})

        if( TRUE ){ # aZoid/aRem

            aRemPair <- sum( aRem[c(1,3,4)]==aRem[c(2,5,6)] )
            cutId <- "any(zwFlag) && aRemPair>1"
            if( any(zwFlag) && aRemPair>1 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[c(1,6)]==c(2,45) && aRemPair>1"
            if( any(zwFlag) && aRemPair>1 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }            

            cutId <- "any(zwFlag) && any(aZoid[c(1,6)]==c(2,44))"
            if( any(zwFlag) && any(aZoid[c(1,6)]==c(2,44)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "all(aZoid[c(1,6)]==c(5,45)) && aRem[2]==aRem[4]"  # zwFlag["symm"]
            if( all(aZoid[c(1,6)]==c(5,45)) && aRem[2]==aRem[4] ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }


            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            aRatio <- sum( aRem[c(1,3,4)]==(aRem[5]*c(2,1,3)) )

            cutId <- "aRatio==3"
            if( aRatio==3 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "all(aCStep[c(1,2,4)]==c(10, 7,15))"
            if( all(aCStep[c(1,2,4)]==c(10, 7,15)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && aCStep[4]==15"
            if( any(zwFlag) && aCStep[4]==15 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && all(aCStep[c(3,5)]==5)"
            if( any(zwFlag) && all(aCStep[c(3,5)]==5) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && aRatio>1"
            if( any(zwFlag) && aRatio>1 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "any(zwFlag) && all(aCStep[c(1,4)]==c(10,15))"
            if( any(zwFlag) && all(aCStep[c(1,4)]==c(10,15)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "zwFlag[\"last\"] && all(aCStep[3:4]==c( 7,15))"
            if( zwFlag["last"] && all(aCStep[3:4]==c( 7,15)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "zwFlag[\"last\"] && all(aCStep[4:5]==c(15,16))"
            if( zwFlag["last"] && all(aCStep[4:5]==c(15,16)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "zwFlag[\"last\"] && all(aFStep[3]==aFStep[4:5])"
            if( zwFlag["last"] && all(aFStep[3]==aFStep[4:5]) ){
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


pName <- "nextCStepBin"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

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

    #   nextBin 과 중복되고 있음. skip !
    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextCStepBin ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      2  4  5 17 27 32    | 2  1 12 10  5 |                        |3 1 1 1 0 |3 1 1 1
        #      6  7 18 19 30 38    | 1 11  1 11  8 |  4   3  13   2   3   6 |2 2 0 2 0 |2 2 2
        #      1  3 12 14 16 43    | 2  9  2  2 27 | -5  -4  -6  -5 -14   5 |2 3 0 0 1 |2 3 1
        #     12 18 24 26 39 40(1) | 6  6  2 13  1 | 11  15  12  12  23  -3 |0 2 2 1 1 |2 2 1 1
        #      3 10 16 19 31 39(1) | 7  6  3 12  8 | -9  -8  -8  -7  -8  -1 |1 3 0 2 0 |1 3 2
        #      5 16 21 26 34 42(1) |11  5  5  8  8 |  2   6   5   7   3   3 |1 1 2 1 1 |1 1 2 1 1
        #   dup number  3:2   5:2   12:2   16:3   18:2   19:2   26:2   39:2
        #   zoid width  ... 30   32   42   28   36   37 and ?

        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( FALSE ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextFStepBin ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      6  8 13 30 35 40    | 2  5 17  5  5 |                        |2 1 0 2 1 |2 1 2 1
        #     13 14 26 33 40 43(2) | 1 12  7  7  3 |  7   6  13   3   5   3 |0 2 1 1 2 |2 1 1 2
        #      2  5 15 18 19 23    | 3 10  3  1  4 |-11  -9 -11 -15 -21 -20 |2 3 1 0 0 |2 3 1
        #      1 11 21 23 34 44(1) |10 10  2 11 10 | -1   6   6   5  15  21 |1 1 2 1 1 |1 1 2 1 1
        #     13 14 26 28 30 36    | 1 12  2  2  6 | 12   3   5   5  -4  -8 |0 2 2 2 0 |2 2 2
        #      4  7 13 29 31 39(1) | 3  6 16  2  8 | -9  -7 -13   1   1   3 |2 1 1 2 0 |2 1 1 2
        #   dup number  13:4   14:2   23:2   26:2   30:2   40:2
        #   zoid width  ... 34   30   21   43   23   35 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aZw==35 && all(aCStep[c(3,5)]==c(16, 8))"
            if( aZw==35 && all(aCStep[c(3,5)]==c(16, 8)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==35 && all(aCStep[4:5]==c(2,8))"
            if( aZw==35 && all(aCStep[4:5]==c(2,8)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==35 && all(aCStep[3:4]==c(16, 2))"
            if( aZw==35 && all(aCStep[3:4]==c(16, 2)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==23 && all(aCStep[c(3:4)]==c(2,2))"
            if( aZw==23 && all(aCStep[c(3:4)]==c(2,2)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aZw==35 && all(aFStep[5:6]==4)"   # 6,6  5,5   1,1   4,4 (6-5,5-1)
            if( aZw==35 && all(aFStep[5:6]==4) ){
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


pName <- "nextColVal_1"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextColVal_1 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      2  9 22 25 31 45    | 7 13  3  6 14 |                        |2 0 2 1 1 |2 2 1 1
        #      3 21 22 33 41 42(1) |18  1 11  8  1 |  1  12   0   8  10  -3 |1 0 2 1 2 |1 2 1 2
        #     23 27 28 38 42 43(1) | 4  1 10  4  1 | 20   6   6   5   1   1 |0 0 3 1 2 |3 1 2
        #     10 14 16 18 27 28(2) | 4  2  2  9  1 |-13 -13 -12 -20 -15 -15 |0 4 2 0 0 |4 2
        #      5 11 12 29 33 44    | 6  1 17  4 11 | -5  -3  -4  11   6  16 |1 2 1 1 1 |1 2 1 1 1
        #     12 18 30 39 41 42(1) | 6 12  9  2  1 |  7   7  18  10   8  -2 |0 2 0 2 2 |2 2 2
        #   dup number  12:2   18:2   22:2   27:2   28:2   33:2   41:2   42:3
        #   zoid width  ... 43   39   20   18   39   30 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep

            cutId <- "aZw==30 && aCStep[2]==12"
            if( aZw==30 && aCStep[2]==12 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==30 && all(aCStep[c(1,5)]==c(6,11))"
            if( aZw==30 && all(aCStep[c(1,5)]==c(6,11)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( FALSE ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1]     ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextColVal_2 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      3  4 12 14 25 43    | 1  8  2 11 18 |                        |2 2 1 0 1 |2 2 1 1
        #      5 13 17 23 28 36    | 8  4  6  5  8 |  2   9   5   9   3  -7 |1 2 2 1 0 |1 2 2 1
        #     17 23 27 35 38 43(2) | 6  4  8  3  5 | 12  10  10  12  10   7 |0 1 2 2 1 |1 2 2 1
        #      6 16 37 38 41 45(1) |10 21  1  3  4 |-11  -7  10   3   3   2 |1 1 0 2 2 |1 1 2 2
        #      6 10 16 28 34 38(3) | 4  6 12  6  4 |  0  -6 -21 -10  -7  -7 |1 2 1 2 0 |1 2 1 2
        #      1 11 17 27 35 39    |10  6 10  8  4 | -5   1   1  -1   1   1 |1 2 1 2 0 |1 2 1 2
        #   dup number  6:2   16:2   17:3   23:2   27:2   28:2   35:2   38:3   43:2
        #   zoid width  ... 40   31   26   39   32   38 and ?
        qFlag <- apply(stdMI$quoTail,1,function(qDat){all(qDat==aQuo$size)})
        if( TRUE ){ # aZoid/aRem

            cutId <- "qFlag[6] && any(aRem[c(1,2)]==aRem[c(3,4)])"
            if( qFlag[6] && any(aRem[c(1,2)]==aRem[c(3,4)]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag[4] && all(aRem[1:2]==6)"
            if( qFlag[4] && all(aRem[1:2]==6) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag[5] && any(remPtnFlag)"
            remPtnFlag <- c( all(aRem[c(1,3)]==6) ,all(aRem[c(4,6)]==8) )
            if( qFlag[5] && any(remPtnFlag) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "qFlag[6] && all(aCStep[c(2,5)]==c(6,4))"
            if( qFlag[6] && all(aCStep[c(2,5)]==c(6,4)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag[6] && any(aCStep[c(2,5)]==c(6,4)) && all(aCStep[c(1,3)]==10)"
            if( qFlag[6] && any(aCStep[c(2,5)]==c(6,4)) && all(aCStep[c(1,3)]==10) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag[5] && all(aCStep[1:2]==aCStep[5:4])"
            if( qFlag[5] && all(aCStep[1:2]==aCStep[5:4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==32 && all(aCStep[1:2]==aCStep[5:4])"
            if( qFlag[5] && all(aCStep[1:2]==aCStep[5:4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==32 && qFlag[5] && any(sameFlag)"
            sameFlag <- c( all(aCStep[c(1,5)]==4) ,all(aCStep[c(2,4)]==6) )
            if( aZw==32 && qFlag[5] && any(sameFlag) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "qFlag[4] && all(aCStep[1:2]==c(10,21))"
            if( qFlag[4] && all(aCStep[1:2]==c(10,21)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==39 && all(aCStep[1:2]==c(10,21))"
            if( aZw==39 && all(aCStep[1:2]==c(10,21)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( FALSE ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextColVal_3 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      5 27 31 34 35 43    |22  4  3  1  8 |                        |1 0 1 3 1 |1 1 3 1
        #      3  7 14 23 26 42    | 4  7  9  3 16 | -2 -20 -17 -11  -9  -1 |2 1 2 0 1 |2 1 2 1
        #      6 16 37 38 41 45    |10 21  1  3  4 |  3   9  23  15  15   3 |1 1 0 2 2 |1 1 2 2
        #     19 21 30 33 34 42    | 2  9  3  1  8 | 13   5  -7  -5  -7  -3 |0 1 1 3 1 |1 1 3 1
        #      8 22 35 38 39 41    |14 13  3  1  2 |-11   1   5   5   5  -1 |1 0 1 3 1 |1 1 3 1
        #      4 14 23 28 37 45    |10  9  5  9  8 | -4  -8 -12 -10  -2   4 |1 1 2 1 1 |1 1 2 1 1
        #   dup number  14:2   23:2   34:2   35:2   37:2   38:2   41:2   42:2   45:2
        #   zoid width  ... 38   39   39   23   33   41 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aZw==33 && all(aCStep[1:2]==c(14,13))"
            if( aZw==33 && all(aCStep[1:2]==c(14,13)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && (aCStep[1]==10) && (aCStep[2]==aCStep[4])"
            if( aZw==41 && (aCStep[1]==10) && (aCStep[2]==aCStep[4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==33 && any(aCStep[1:2]==c(14,13)) && all(aCStep[3:4]==c(3,1))"
            if( aZw==33 && any(aCStep[1:2]==c(14,13)) && all(aCStep[3:4]==c(3,1)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==41 && (aCStep[1]==10) && all(aCStep[3:4]==c(5,9))"
            if( aZw==41 && (aCStep[1]==10) && all(aCStep[3:4]==c(5,9)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aFStep
            cutId <- "aZw==33 && all(aFStep[3:5]==5)"
            if( aZw==33 && all(aFStep[3:5]==5) ){
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


pName <- "nextColVal_4"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextColVal_4 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      8 10 13 36 37 40    | 2  3 23  1  3 |                        |1 2 0 2 1 |1 2 2 1
        #     10 15 18 21 34 41(1) | 5  3  3 13  7 |  2   5   5 -15  -3   1 |0 3 1 1 1 |3 1 1 1
        #     14 15 16 17 38 45(1) | 1  1  1 21  7 |  4   0  -2  -4   4   4 |0 4 0 1 1 |4 1 1
        #      1  4 10 12 28 45(1) | 3  6  2 16 17 |-13 -11  -6  -5 -10   0 |2 2 1 0 1 |2 2 1 1
        #     14 17 19 22 24 40    | 3  2  3  2 16 | 13  13   9  10  -4  -5 |0 3 2 0 1 |3 2 1
        #      5 16 21 26 34 42    |11  5  5  8  8 | -9  -1   2   4  10   2 |1 1 2 1 1 |1 1 2 1 1
        #   dup number  10:3   14:2   15:2   16:2   17:2   21:2   34:2   40:2   45:2
        #   zoid width  ... 32   31   31   44   26   37 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "all(aZoid[c(1,6)]==c(1,45)) && aRem[5]==8"
            if( all(aZoid[c(1,6)]==c(1,45)) && aRem[5]==8 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "aCStep[5]==16 && all(aCStep[1:2]==aCStep[3:4]) && aZw==26"
            if( aCStep[5]==16 && all(aCStep[1:2]==aCStep[3:4]) && aZw==26 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }
        if( TRUE ){ # aFStep
            cutId <- "all(aFStep[3:5]==(c(1,2,5)*aFStep[6])) && aZw==45"
            if( all(aFStep[3:5]==(c(1,2,5)*aFStep[6])) && aZw==45 ){
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


pName <- "nextColVal_5"
Fin.custCutLst[[pName]] <- function( aZoidMtx ,surFlag=NULL ,stdMI ,anaOnly=F ){

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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextColVal_5 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #      3  7 10 13 25 36    | 4  3  3 12 11 |                        |2 2 1 1 0 |2 2 1 1
        #      5 16 21 26 34 42    |11  5  5  8  8 |  2   9  11  13   9   6 |1 1 2 1 1 |1 1 2 1 1
        #     19 23 28 37 42 45(1) | 4  5  9  5  3 | 14   7   7  11   8   3 |0 1 2 1 2 |1 2 1 2
        #      1  4 14 18 29 37(1) | 3 10  4 11  8 |-18 -19 -14 -19 -13  -8 |2 2 1 1 0 |2 2 1 1
        #      4  9 17 18 26 42(2) | 5  8  1  8 16 |  3   5   3   0  -3   5 |2 2 1 0 1 |2 2 1 1
        #      5 12 25 26 38 45(1) | 7 13  1 12  7 |  1   3   8   8  12   3 |1 1 2 1 1 |1 1 2 1 1
        #   dup number  4:2   5:2   18:2   25:2   26:3   37:2   42:3   45:2
        #   zoid width  ... 33   37   26   36   38   40 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZoid test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }
        }
        if( TRUE ){ # aCStep
            cutId <- "all( aCStep[c(2,4)]==c(13,12) )"
            if( all( aCStep[c(2,4)]==c(13,12) ) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aCStep[3]==1 && aCStep[1]==aCStep[5] && aZw=40"
            if( aCStep[3]==1 && aCStep[1]==aCStep[5] && aZw=40 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aFStep
            cutId <- "all(aFStep[c(2,3)]==aFStep[c(6,4)]) && aZw==40"
            if( all(aFStep[c(2,3)]==aFStep[c(6,4)]) && aZw==40 ){
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

        aZoid <- aZoidMtx[aIdx,]    ;aZw<-aZoid[6]-aZoid[1] # ;aQuo<-fCutU.getQuoObj(aZoid,valSet=T)
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - stdMI$lastZoid

        # *** nextColVal_6 ***
        #     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
        #     19 22 30 34 39 44    | 3  8  4  5  5 |                        |0 1 1 3 1 |1 1 3 1
        #      5 17 18 22 23 43(1) |12  1  4  1 20 |-14  -5 -12 -12 -16  -1 |1 2 2 0 1 |1 2 2 1
        #      1  4 14 18 29 37(1) | 3 10  4 11  8 | -4 -13  -4  -4   6  -6 |2 2 1 1 0 |2 2 1 1
        #      1 15 17 23 25 41(1) |14  2  6  2 16 |  0  11   3   5  -4   4 |1 2 2 0 1 |1 2 2 1
        #      8 19 20 21 33 39    |11  1  1 12  6 |  7   4   3  -2   8  -2 |1 1 2 2 0 |1 1 2 2
        #      5  8 18 21 22 38(2) | 3 10  3  1 16 | -3 -11  -2   0 -11  -1 |2 1 2 1 0 |2 1 2 1
        #   dup number  1:2   5:2   8:2   17:2   18:3   19:2   21:2   22:3   23:2   39:2
        #   zoid width  ... 25   38   36   40   31   33 and ?
        if( TRUE ){ # aZoid/aRem

            cutId <- "aZw==33 && all(aZoid[3:4]==c( 8,21))"
            if( aZw==33 && all(aZoid[3:4]==c( 8,21)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==33 && aZoid[4]==21 && (0==aZoid[5]%%11)"
            if( aZw==33 && aZoid[4]==21 && (0==aZoid[5]%%11) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

        }
        if( TRUE ){ # aCStep
            cutId <- "aZw==33 && aCStep[1]==aCStep[3] && any(aCStep[c(2,5)]==c(10,16))"
            if( aZw==33 && aCStep[1]==aCStep[3] && any(aCStep[c(2,5)]==c(10,16)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==33 && all(aCStep[c(2,5)]==c(10,16))"
            if( aZw==33 && all(aCStep[c(2,5)]==c(10,16)) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZw==33 && aCStep test"
            if( FALSE ){
                # surFlag[aIdx] <- F
                # if( !anaOnly ){ next
                # } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="custCut" ,pName=pName ,info=cutId )
                #     cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                # }
            }

        }
        if( TRUE ){ # aFStep
            cutId <- "aFStep test"
            if( FALSE ){
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


