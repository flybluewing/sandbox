# to20180428.R 최종접근

finalCut.first4 <- function( gEnv ,allIdxF ){

    # ===================================================================================
    firstCode <- c(4)
    flag <- gEnv$allZoidMtx[allIdxF,1] %in% c(firstCode) # zoid[1]은 1:5 사이에서 가장 오래 안 나온 값.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
    if( 0==length(allIdxF) ){
        cat("None of them....\n")
    }

    # zoid[1]을 4로 정하면 5->4가 된다. 다른 컬럼에서까지 이런 1간격 변경은 없겠지.
    banCode <- gEnv$zhF[nrow(gEnv$zhF),][2:6]-1
    flag <- apply( gEnv$allZoidMtx[allIdxF,2:6,drop=F] ,1 ,function( aCode ){
                return( !any(aCode==banCode) )
            })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[1]을 4로 정하면 5->4가 된다. 전체 코드에서 1 변경값이 3개이상 있지는 않겠지.
    lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                    oneLess <- intersect( aZoid ,(lastZoid-1) )
                    oneMore <- intersect( aZoid ,(lastZoid+1) )
                    cnt <- length(unique( oneLess ,oneMore ))
                    return( cnt<3 )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


    localHisMtx <- gEnv$zhF[gEnv$zhF[,1]%in%firstCode ,]
    #         740  4  8  9 16 17 19
    #         752  4 16 20 33 40 43
    #         761  4  7 11 24 42 45
    #         785  4  6 15 25 26 33

    # aZoid4[1:2]에 대해 (4,7) (4,6) ... and (4,5)? no
    flag <- gEnv$allZoidMtx[allIdxF,2]!=5
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4 %%10 에서 5가 동일 연속 거부.
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                    idx <- which( 5==(aZoid%%10) )
                    if( 2<length(idx) ) return( FALSE )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )
                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4 %/% 10 값이 같은 거 거부.
    lastQuo <- localHisMtx[nrow(localHisMtx),] %/% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                return( !all(aZoid%/%10==lastQuo) )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zhF[730,]  4 10 14 15 18 22.. (10 많음)
    #   (10,14) (14,15) (15,18) 패턴은 피하자. lastZoid에서도 10 많음.
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){

                    idx <- which( aZoid %in% c(10,14) )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )

                    idx <- which( aZoid %in% c(14,15) )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )

                    idx <- which( aZoid %in% c(15,18) )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )

                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4와 연속으로 3개 똑같은 발생은 피하자.(3개 초과는 아예 스킵.)
    lastLH <- localHisMtx[nrow(localHisMtx),]
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                    idx <- which( aZoid %in% lastLH )
                    if( 3<length(idx) ) return( FALSE )
                    if( 3==length(idx) && 2==(idx[3]-idx[1]) ) return( FALSE )

                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid[n,6] 이 43 . . 43 . . ? 2번씩 건너뛰어서 43이 또 나올까?
    #   게다가 lastZoid[6]은 33임. 3이 너무 많다. 33, 43 둘 다 빼자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                return( !any( aZoid[6] %in% c(33,43) ) )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )

} # finalCut.first4( )

finalCut.first7 <- function( gEnv ,allIdxF ){

	return( allIdxF )

} # finalCut.first7( )

# allIdx <- allZoid.idx1
finalCut <- function( gEnv ,allIdx ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.

    allIdxF <- allIdx
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,2 ,function(p){sort(unique(p))})
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

#     798  2 10 14 22 32 36
#     799 12 17 23 34 42 45
#     800  1  4 10 12 28 45
#     801 17 25 28 37 43 44
#     802 10 11 12 18 24 42
#     803  5  9 14 26 30 43
    rebCnt <- sapply( 2:nrow(gEnv$zhF) ,function(idx){
                    cnt <- sum( gEnv$zhF[idx-1,] %in% gEnv$zhF[idx,] )
                    return(cnt)
                })

    # zoid[6] : 42,43.. 44? no
    flag <- gEnv$allZoidMtx[allIdxF,6] != 44
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 2개 재발은 떼자.
    lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
            cnt <- sum( lastZoid %in% aZoid )
            return( cnt<2 )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[3:6] %/% 10 이 (1,1,2,4) 패턴이 두번이나 반복되었다..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F]%/%10 ,1 ,function( aZoid ){
            if( all(aZoid[3:4]==c(1,1)) ) return( FALSE )
            if( all(aZoid[4:5]==c(1,2)) ) return( FALSE )
            if( all(aZoid[5:6]==c(2,4)) ) return( FALSE )
            return( TRUE )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[2:6]-zoid[1:5] 4, 5,12, 4,13 : 4가 두개씩이나? 다음엔 없겠지.
    flag <- apply( gEnv$allZoidMtx[allIdxF,2:6,drop=F] - gEnv$allZoidMtx[allIdxF,1:5,drop=F]
            ,1 
            ,function( aCStep ){
                return( !any(aCStep==4) )
            })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 12,17이 너무 자주 나왔다. (12,17 이 연달아 붙어 나오긴 힘들겠지?)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    idx <- which( aZoid %in% c(12,17) )
                    if( 2!=length(idx) ) return( TRUE )
                    if( 1==(idx[2]-idx[1]) ) return( FALSE )
                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 42,45도 최근에 나왔으니, (42,45) 연달아 붙어 나오긴 힘들겠지?
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    idx <- which( aZoid %in% c(42,45) )
                    if( 2!=length(idx) ) return( TRUE )
                    if( 1==(idx[2]-idx[1]) ) return( FALSE )
                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 10,11,12 이 최근에 나왔다. CStep 1이 2번연속되는 건 아직 없겠지..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cStep2 <- aZoid[3:6] - aZoid[1:4]
                    return( !any(cStep2==2) )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid %% 10 에서 한 개 quo가 4개 이상 차지하는 것은 피하자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    maxCnt <- max(table(aZoid%/%10))
                    return( maxCnt<4 )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF.4 <- finalCut.first4( gEnv ,allIdxF )
	allIdxF.7 <- finalCut.first4( gEnv ,allIdxF )
	
	rObj <- list( allIdxF.4=allIdxF.4 ,allIdxF.7=allIdxF.7 )

    # allIdx <- allIdxF
    
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})
    stdColValLst <- apply( gEnv$zhF ,2 ,function(p){sort(unique(p))})

    rebCnt <- sapply( 2:nrow(localHisMtx) ,function(idx){
                        sum( localHisMtx[(idx-1),] %in% localHisMtx[idx,] )
                    })
    rebMtxLst <- lapply( which(rebCnt>1) ,function( idx ){
                        return( localHisMtx[idx+0:1,] )
                    })

    return( rObj )

} # finalCut()


