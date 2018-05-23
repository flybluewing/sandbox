# to20180526_H.R 최종접근
finalCut.first2 <- function( gEnv ,allIdxF ){

	# ===================================================================================
	fObj <- list( idx=1 ,val=2 )
	fObj$lastMtx	<- tail(gEnv$zhF)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(gEnv$zhF[gEnv$zhF[,fObj$idx]==fObj$val,])
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]
	fObj$localZhF	<- gEnv$zhF[gEnv$zhF[,1]==fObj$val ,]
	fObj$tailZhF	<- tail(fObj$localZhF)

	flag <- gEnv$allZoidMtx[allIdxF,fObj$idx] %in% fObj$val
	allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
    if( 0==length(allIdxF) ){
        cat("None of them....\n")
    }

	# aZoid[2] : 8,10,6,10,7,10,? 8?
	flag <- gEnv$allZoidMtx[allIdxF,2] != 8	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# aZoid[2] : 8,10,6,10,7,10,? 10?
	flag <- gEnv$allZoidMtx[allIdxF,2] != 10	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	azColValLst <- apply( gEnv$allZoidMtx[allIdxF,] ,2 ,function(p){sort(unique(p))})

} # finalCut.first2()

finalCut.first4 <- function( gEnv ,allIdxF ){

	# ===================================================================================
	fObj <- list( idx=1 ,val=4 )
	fObj$lastMtx	<- tail(gEnv$zhF)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.1	<- tail(gEnv$zhF[gEnv$zhF[,fObj$idx]==fObj$val,])
	fObj$lastZoid.1	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]
	fObj$localZhF	<- gEnv$zhF[gEnv$zhF[,1]==fObj$val ,]
	fObj$tailZhF	<- tail(fObj$localZhF)

	flag <- gEnv$allZoidMtx[allIdxF,fObj$idx] %in% fObj$val
	allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
    if( 0==length(allIdxF) ){
        cat("None of them....\n")
    }

    # zoid[1]을 4로 정하면 1->4가 된다. lastZoid[2:3] 처럼 +x,-x 형태 변화는 없겠지.
	#		즉 aZoid[2] 는 7(10-3) 제외하자.
    flag <- gEnv$allZoidMtx[allIdxF,2] != 7
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

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
    lastQuo <- fObj$lastZoid.1 %/% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                return( !all(aZoid%/%10==lastQuo) )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4와 연속으로 3개 똑같은 발생은 피하자.(3개 초과는 아예 스킵.)
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                    idx <- which( aZoid %in% fObj$lastZoid.1 )
                    if( 3<length(idx) ) return( FALSE )
                    if( 3==length(idx) && 2==(idx[3]-idx[1]) ) return( FALSE )

                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid[n,6] 이 43 . . 43 . . ? 2번씩 건너뛰어서 43이 또 나올까?
	#			43,33 모두 빼자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                return( !any( aZoid[6] %in% c(33,43) ) )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# localHisMtx[,4]%%10 : 5,6,3,4,5,?  6이 나오긴 어렵겠지?
	flag <- (gEnv$allZoidMtx[allIdxF,4]%%10) != 6
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# fObj$lastZoid, fObj$lastZoid.l 모두 26나왔다. 또 나오긴..
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				return( !(26 %in% aZoid) )
			} )
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# lZoid[2] 에 6이 올 수 있을까?
	#	8,X,7,X,? 에다가 fObj$lastZoid.l[2] 도 6임
	flag <- gEnv$allZoidMtx[allIdxF,2] != 6
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 11 배수가 연달아 3번째다. 다음에도 나오긴 어렵겠지? (33,11,33..)
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
				return( !any(aZoid%%11==0) )
			} )
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

	# length(allIdxF)	4377

	lastIdx <- tail(which(gEnv$zhF[,1]==fObj$val))
	gEnv$zhF[sort(c(lastIdx,lastIdx-1)) ,]

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


cutByWidth <- function( gEnv ,allIdxF ){

	# width ==========================================================================
	#	28,30,31,32,33,34,35,36
	widthIdx.28 <- which( 28 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.28,])	# 781 11 16 18 19 24 39
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 28!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(11,16,18,19,24,39)
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[1] <- aZoid[1]==2
					flag[2] <- any(aZoid==c(11,16,18,19,24,39))

					cStep <- aZoid[2:6]-aZoid[1:5]
					flag[3] <- any( cStep[2:5]==cStep[1:4] )
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	widthIdx.30 <- which( 30 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.30,])	# 780 15 17 19 21 27 45
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 30!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(15,17,19,21,27,45)
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[1] <- any(aZoid==c(15,17,19,21,27,45))
					flag[2] <- aZoid[3] %in% c(17,20)
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	widthIdx.31 <- which( 31 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.31,])	# 783 14 15 16 17 38 45
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 31!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(14,15,16,17,38,45)
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[1] <- any(aZoid==c(14,15,16,17,38,45))
					cStep <- aZoid[2:6]-aZoid[1:5]
					flag[2] <- any( cStep[2:5]==cStep[1:4] )
					flag[3] <- aZoid[2] %in% c(14)
					flag[4] <- aZoid[3] %in% c(15)
					flag[5] <- aZoid[4] %in% c(21)
					flag[6] <- aZoid[5] %in% c(34)
					flag[7] <- any(cStep==1)
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	widthIdx.32 <- which( 32 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.32,])	# 802 10 11 12 18 24 42
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 32!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(10,11,12,18,24,42)
					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- lastH-aZoid
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[1] <- aZoid[1] %in% c(14)
					flag[2] <- aZoid[2] %in% c(15)
					flag[4] <- aZoid[4] %in% c(17)
					flag[7] <- any(aZoid==lastH)
					flag[8] <- any(cStep==1)
					flag[9] <- any(fStep[2:6]==fStep[1:5])
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	widthIdx.33 <- which( 33 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.33,])	# 799 12 17 23 34 42 45
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 33!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(12,17,23,34,42,45)
					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- lastH-aZoid
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[1] <- aZoid[1] %in% c(11)
					flag[2] <- aZoid[2] %in% c(16,18)
					flag[4] <- aZoid[4] %in% c(33)
					flag[7] <- any(aZoid==lastH)
					flag[9] <- any(fStep[2:6]==fStep[1:5])
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	widthIdx.34 <- which( 34 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.34,])	# 798  2 10 14 22 32 36
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 34!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c( 2 ,10 ,14 ,22 ,32 ,36 )
					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- lastH-aZoid
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[1] <- aZoid[1] %in% c(9)
					flag[4] <- aZoid[4] %in% c(24)
					flag[7] <- any(aZoid==lastH)
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	widthIdx.35 <- which( 35 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.35,])	# 804  1 10 13 26 32 36
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 35!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(  1 ,10 ,13 ,26 ,32 ,36 )
					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- lastH-aZoid
					rCode <- lastH%%10

					flag <- rep( F ,20 )
					flag[2] <- aZoid[2] %in% c(12)
					flag[3] <- aZoid[3] %in% c(19)
					flag[4] <- aZoid[4] %in% c(24)
					flag[7] <- any(aZoid==lastH)
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	widthIdx.36 <- which( 36 == (gEnv$zhF[,6]-gEnv$zhF[,1]) )
	# tail(gEnv$zhF[widthIdx.36,])	# 784  3 10 23 24 31 39
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 36!=(aZoid[6]-aZoid[1]) ) return(TRUE)

					lastH <- c(  3 ,10 ,23 ,24 ,31 ,39 )
					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- lastH-aZoid

					flag <- rep( F ,20 )
					flag[7] <- any(aZoid==lastH)
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( allIdxF )
} # cutByWidth()

cutByColVal.col1 <- function( gEnv ,allIdxF ){
	# 2:1454    4:1088    5:1006    7:661
	stdPosIdx <- 1

	stdFixVal <- 2	;curIdx<-which(gEnv$zhF[,1]==stdFixVal)
	zhF <- gEnv$zhF[curIdx,]
	lastH <- zhF[nrow(zhF),]
	cStep <- lastH[2:6] - lastH[1:5]
	fStep <- zhF[nrow(zhF)-1,] - zhF[nrow(zhF),]
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[1]!=stdFixVal ) return(TRUE)

					flag <- rep( F ,20 )
					flag[2] <- aZoid[2] %in% c(8)
					flag[5] <- (aZoid[5]%%10) == 9
					flag[7] <- any(aZoid[-stdPosIdx]==lastH[-stdPosIdx])
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	stdFixVal <- 4	;curIdx<-which(gEnv$zhF[,1]==stdFixVal)
	zhF <- gEnv$zhF[curIdx,]
	lastH <- zhF[nrow(zhF),]
	cStep <- lastH[2:6] - lastH[1:5]
	fStep <- zhF[nrow(zhF)-1,] - zhF[nrow(zhF),]
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[1]!=stdFixVal ) return(TRUE)

					flag <- rep( F ,20 )
					flag[2] <- aZoid[2] %in% c(6)	# 추가적인 거부 근거이므로..
					flag[7] <- any(aZoid[-stdPosIdx]==lastH[-stdPosIdx])
					flag[20] <- all(aZoid%/%10 == lastH%/%10)

					return( 1>=sum(flag) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	return( allIdxF )
} # cutByColVal.col1()
cutByColVal.col2 <- function( gEnv ,allIdxF ){
	return( allIdxF )
} # cutByColVal.col2()
cutByColVal.col3 <- function( gEnv ,allIdxF ){
	return( allIdxF )
} # cutByColVal.col3()
cutByColVal.col4 <- function( gEnv ,allIdxF ){
	return( allIdxF )
} # cutByColVal.col4()
cutByColVal.col5 <- function( gEnv ,allIdxF ){
	return( allIdxF )
} # cutByColVal.col5()
cutByColVal.col6 <- function( gEnv ,allIdxF ){
	return( allIdxF )
} # cutByColVal.col6()


