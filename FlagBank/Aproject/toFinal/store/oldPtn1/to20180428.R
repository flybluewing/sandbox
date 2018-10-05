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
	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(4==(aZoid%%10))
					return( 2>=cnt )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# localHisMtx[,4]%%10 : 5,6,3,4,5,?  6이 나오긴 어렵겠지?
	flag <- (gEnv$allZoidMtx[allIdxF,4]%%10) != 6
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# lastZoid에서도 3 두개 뿐(30,43)이고, lastLH에서도 33 뿐이다.
	#	33혼자만 나오고 나머지엔 3이 없기는 힘들겠지?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( any(aZoid==33) ){
						cnt <- sum(aZoid%%10) + sum(aZoid%/%10)
						if( cnt==2 ){
							return(FALSE)
						}
					}
					return( TRUE )
				})
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# %%10 에서 5나 6이 2개인 것.(4도 2개 이상인 상태에서..)
	flag <- apply( gEnv$allZoidMtx[allIdxF,]%%10 ,1 ,function(p){
					if( 1<sum(p==4) ){
						if( (sum(p==5)==2)||(sum(p==6)==2) ){
							return( FALSE )
						}
					}
					return( TRUE )
				})
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =============================================================================
	# secondCode
	#	16 17 18 19 20 21 22 23 
	#	15 12  2 18 24 31 34  9 
	
	# 	sCode 21 : 4,21,22,34,37,38
	#	sCode 22 : 4,22,27,28,38,40
	#		--> 4,21,22,?,?,c(38,40) 은 제외시키자.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:3]==c(4,21,22)) ){
					if( aZoid[6] %in% c(38,40) ){
						return(FALSE)
					}
				}
				return( TRUE )
			})
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:3]==c(4,22,27)) ){
					if( any(aZoid[4:6] %in% c(28,38)) ){
						return(FALSE)
					}
				}
				return( TRUE )
			})
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
		
	#	sCode 20 : 4 20 26 28 35 40
	#		4,20,?,?,28,?,40 을 제외시키자. (28과 40이 꽤 많이 보이네.)
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:2]==c(4,20)) ){
					if( any(aZoid[4]==28 || aZoid[6]==40) ){
						return(FALSE)
					}
				}
				return( TRUE )
			})
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	#	sCode 19
	#	172  4 19 21 24 26 41
	#	224  4 19 26 27 30 42
	#	242  4 19 20 21 32 34
	#	455  4 19 20 26 30 35
	#		 4,19,?,?,?,(30,40),? 을 제외시키자. (lastZoid : 5  9 14 26 30 43 )
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:2]==c(4,19)) ){
					if( aZoid[5]%in%c(30,40) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	sCode 16 : 752  4 16 20 33 40 43
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:2]==c(4,16)) ){
					if( aZoid[4]%in%c(33) || aZoid[5]%in%c(40) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# aZoid[4] 에서 31, 32는 제외하자.
	#	611  2 22 27 33 36 37
	#	739  7 22 29 33 34 35
	#	760 10 22 27 31 42 43
	#	764  7 22 24 31 34 36
	#	797  5 22 31 32 39 45
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:2]==c(4,22)) ){
					if( aZoid[4]%in%c(31) || aZoid[5]%in%c(42) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# aZoid[4] 에서 31, 32는 제외하자.
	#	778  6 21 35 36 37 41
	#	796  1 21 26 36 40 41	
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:2]==c(4,21)) ){
					if( aZoid[4]%in%c(26,36) || aZoid[5]%in%c(30,40) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	#	557  4 20 26 28 35 40
	#	664 10 20 33 36 41 44
	#	712 17 20 30 31 33 45
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[1:2]==c(4,20)) ){
					if( aZoid[3]%in%c(30,33) || aZoid[5]%in%c(33) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	secondCode <- 16
	localHisMtx <- gEnv$zhF[ (gEnv$zhF[,2]==secondCode) ,]
	# localHisMtx <- gEnv$zhF[ (gEnv$zhF[,1]==firstCode)&(gEnv$zhF[,2]==secondCode) ,]

	# =============================================================================
	# thirdCode
	#	23 24 25 27
	#	23 17 19 26
	thirdCode <- 25
	localHisMtx <- gEnv$zhF[ (gEnv$zhF[,1]==firstCode)&(gEnv$zhF[,3]==thirdCode) ,]
	localHisMtx <- gEnv$zhF[ (gEnv$zhF[,3]==thirdCode) ,]

	allZoidMtx <- gEnv$allZoidMtx[allIdxF,]
	allZoidMtx[ (allZoidMtx[,1]==firstCode)&(allZoidMtx[,3]==thirdCode) ,]
	
	#	124  4 16 23 25 29 42
	#	306  4 18 23 30 34 41
	#	360  4 16 23 25 35 40
	#	660  4  9 23 33 39 44
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[c(1,3)]==c(4,23)) ){
					if( aZoid[2]%in%c(16) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	487  4  8 25 27 37 41
	#	545  4 24 25 27 34 35
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[c(1,3)]==c(4,25)) ){
					if( aZoid[4]%in%c(27) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	#	520  4 22 27 28 38 40
	#	609  4  8 27 34 39 40
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[c(1,3)]==c(4,27)) ){
					if( aZoid[6]%in%c(40) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# =============================================================================
	# forthCode
	#	29 30 31
	#	26 15 23
	forthCode <- 31
	tail(gEnv$zhF[ (gEnv$zhF[,1]==firstCode)&(gEnv$zhF[,4]==forthCode) ,])
	tail(gEnv$zhF[ (gEnv$zhF[,4]==forthCode) ,])

	allZoidMtx <- gEnv$allZoidMtx[allIdxF,]
	allZoidMtx[ (allZoidMtx[,1]==firstCode)&(allZoidMtx[,4]==forthCode) ,]
	
	gEnv$allZoidMtx[allIdxF[kIdx],]


	#	767  5 15 20 31 34 42
	#	791  2 10 12 31 33 42
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[c(1,4)]==c(4,31)) ){
					if( aZoid[5]%in%c(32,33) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	return( allIdxF )

} # finalCut.first4( )

finalCut.first7 <- function( gEnv ,allIdxF ){

	# ===================================================================================
	fObj <- list( idx=1 ,val=7 )
	fObj$lastMtx	<- tail(gEnv$zhF)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.1	<- tail(gEnv$zhF[gEnv$zhF[,fObj$idx]==fObj$val,])
	fObj$lastZoid.1	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]
	
	flag <- gEnv$allZoidMtx[allIdxF,fObj$idx] %in% fObj$val
	allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
    if( 0==length(allIdxF) ){
        cat("None of them....\n")
    }

	# fObj$lastMtx.1[,5] : 33 ,.. ,34 ,.. ,34 ,.. ,? 34
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[fObj$idx]==c(fObj$val)) ){
					if( aZoid[5]%in%c(33,34) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# fObj$lastMtx.1[,2] : 22 ,.. ,22 ,.. ,? 22 밴
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[fObj$idx]==c(fObj$val)) ){
					if( aZoid[2]%in%c(22) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# lastMtx.1 %/% 10 패턴이 반복적.. 0 ,2 ,2 ,3 ,3 ,3
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[fObj$idx]==c(fObj$val)) ){
					if( all( (aZoid%/%10)==c(0,2,2,3,3,3) ) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# lastMtx.1[,c(2,3)] 둘 다 똑같이 5씩 증가했다. 다음에도 그럴까?
	#		32,34..
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				if( all(aZoid[fObj$idx]==c(fObj$val)) ){
					if( all( aZoid[2:3]==c(32,34) ) ){
						return(FALSE)
					}
				}
				return( TRUE )
			});	kIdx <- head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# suspenct
	#	aZoid[2] : 33
	#	aZoid[6] : 37
	
	
	table(flag)
	head(gEnv$allZoidMtx[allIdxF[kIdx],])
	allZoidMtx <- gEnv$allZoidMtx[allIdxF,]
	apply( allZoidMtx ,2 ,function(p){sort(unique(p))} )
	
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


	# zoid[3:5]는 변화량 절대값이 2로 3번 연속이다. 재현 어렵겠지?
	#	802 10 11 12 18 24 42
	#	803  5  9 14 26 30 43
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    idx <- which( 2==abs(aZoid-lastZoid) )					
					fndIdx <- haveSeq( idx ,c(2,2,2) )
					return( is.null(fndIdx) )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))	

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(0==(aZoid%%10))
					return( 1>=cnt )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(0==(aZoid%%11))
					return( 1>=cnt )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	allIdxF.4 <- finalCut.first4( gEnv ,allIdxF )
	allIdxF.7 <- finalCut.first7( gEnv ,allIdxF )

	rObj <- list( allIdxF=allIdxF ,allIdxF.4=allIdxF.4 ,allIdxF.7=allIdxF.7 )

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


