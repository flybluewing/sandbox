# to20180512.R 최종접근

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

# allIdx <- allIdxLst$allZoid.idx1
finalCut <- function( gEnv ,allIdx ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.

    allIdxF <- allIdx
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

	# 참고 자료 --------------------------------------------------------------------
    rebCnt <- sapply( 2:nrow(gEnv$zhF) ,function(idx){
                    cnt <- sum( gEnv$zhF[idx-1,] %in% gEnv$zhF[idx,] )
                    return(cnt)
                })
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,2 ,function(p){sort(unique(p))})
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 	799 12 17 23 34 42 45
	# 	800  1  4 10 12 28 45
	# 	801 17 25 28 37 43 44
	# 	802 10 11 12 18 24 42
	# 	803  5  9 14 26 30 43
	# 	804  1 10 13 26 32 36
	#   805  3 12 13 18 31 32

    # zoid[5] : 30,32,31,? 30? no
    flag <- gEnv$allZoidMtx[allIdxF,5] != 30
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[4] : 37,18,26,26,18,? 37? no.
    flag <- gEnv$allZoidMtx[allIdxF,4] != 37
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[3] : 14,13,13,? 14? no
    flag <- gEnv$allZoidMtx[allIdxF,3] != 14
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# 26->26 ,13->13 재현될까?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !any( aZoid==lastZoid ) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[1:2]는 똑같이 2 씩 증가했다... 반복될까?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cDiff <- aZoid-lastZoid
					return( !any(cDiff[2:6]==cDiff[1:5]) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# lastZoid %% 10 에서 2 두개, 3두개.. 반복될까?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					rem <- aZoid %% 10
					cnt2 <- sum(rem==2)
					cnt3 <- sum(rem==3)
					return( !( 2==cnt2 && 2==cnt3 ) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	
	# cStep 1이 2개나 발생... 다음에도 또 있을 수 있을까?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cStep <- aZoid[2:6] - aZoid[1:5]
					return( !any(cStep==1) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 2개 재발은 떼자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( lastZoid %in% aZoid )
					return( cnt<2 )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid %/% 10 이 4개 이상인 것은 제외하자.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( 3>=max(aZoid%/%10) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 12,13,18은 최근에 너무 많이 나온 듯. 빼자.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !any(c(12,13,18) %in% aZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 26이 또 나올까? 너무 많지않나?
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !any(c(26) %in% aZoid) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))


	# lastZoid %/% 10 이 그대로 반복되긴 어렵겠지?
	lastCode <- lastZoid %/% 10
	flag <- apply( gEnv$allZoidMtx[allIdxF,]%/%10 ,1 ,function(aCode){
					cnt1 <- sum(aCode==1)
					cnt3 <- sum(aCode==3)
					return( !( 3==cnt1 && 2==cnt3 ) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	# colPtnLst[[1]]$val   7  4  7  2 13  1  1  6  8 14  1 22  6  6
	# colPtnLst[[2]]$val  12 29 13 11
	# colPtnLst[[3]]$val  28 20  9
	# colPtnLst[[4]]$val  22 23 16 28
	# colPtnLst[[5]]$val  12 24 27 29
	# colPtnLst[[6]]$val  22 42 17 42 37

    # zoid[1] 이 2,4,7일 때 나머지 컬럼에서의 colPtnVal 재발은 없겠지.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( !(aZoid[1] %in% c(2,4,7)) ){
						return( TRUE )
					}
					return( !any(aZoid[2:6]==c(12,28,22,12,22)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[1] 이 2,4,7일 때 zoid[4]에서 colPtnVal 21은 나오기 어렵겠지.
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( !(aZoid[1] %in% c(2,4,7)) ){
						return( TRUE )
					}
					return( !any(aZoid[4]==c(21)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] - 8,16
	#			zoid[1] != 9 : 6,7,8,?
	#			zoid[2] !=16 : 9,9,16,?
	#			zoid[2]/zoid[1] != 2
	#		cvSeqNextLst[[2]] zoid[2:3] - 31,35
	#			zoid[3] 10 : 27,26,35,?	- n4가 나오기엔..
	#		cvSeqNextLst[[3]] zoid[3:4] - 5,11
	#			zoid[3] : 9,5,4,5,?	- 4,6,9가 나오기엔..
	#		cvSeqNextLst[[3]] zoid[4:5] - 15,16
	#			zoid[4]-zoid[2] : 1 차이 반복될까?
	#		cvSeqNextLst[[3]] zoid[5:6] - 14,22

	# cvSeqNextLst[[1]]$fndMtx[,1] : 6,x,8,6,x,8,? 6이 다시 재발하기엔 너무 규칙적이겠지?
	flag <- gEnv$allZoidMtx[allIdxF,1]!=6	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# zoid[1]이 7,4,2일 때(colPtnLst[[1]]$val) zoid[2] 값이 2배인 경우는 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( !(aZoid[1] %in% c(2,4,7)) ){
						return( TRUE )
					}
					return( (2*aZoid[1]) != aZoid[2] )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:3] - 8 13 26
	#		cvSeqNextLst[[2]] zoid[2:4] - 
	#		cvSeqNextLst[[3]] zoid[3:5] - 9 14 16
	#		cvSeqNextLst[[3]] zoid[4:6] -22 31 42

	# cvSeqNextLst[[1]] fndMtx[,1] 3->8이 너무 많다.
	flag <- gEnv$allZoidMtx[allIdxF,1]!=8	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cvSeqNextLst[[3]] zoid[3:5] - c( 9,14,16) 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !all(aZoid[3:5]==c(9,14,16)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

	# cvSeqNextLst[[3]] zoid[4:6] - c(22,31,42) 제외
	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					return( !all(aZoid[4:6]==c(22,31,42)) )
				})	;kIdx<-head(which(!flag))
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))




	# length(allZoidF) 70834
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




