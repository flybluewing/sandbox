# 파일명 : 20171029_A_H_seqAnaFun.R
#   의존 : 20170917_A_H.R

# 
seqAnaFun.default <- function( pFlag ,pPredObj ,pInitNA ,pCodeVal ){

    flag <- if(is.na(pInitNA)){ pFlag 
                } else pFlag[-(1:pInitNA)]
    codeVal <- if(is.null(pCodeVal)){ sort(unique(flag),na.last=T) 
                    } else pCodeVal
    codeVal.naIdx <- if( any(is.na(codeVal)) ) { which(is.na(codeVal)) 
                        } else NULL
            # 즉, codeVal.naIdx 값이 NULL임을 보고 flag 내에 NA가 존재 치 않음을 확인하도록 한다.

    seqNumObj   <- k.seqNum( flag	,pCodeVal=codeVal )
    probObj     <- pPredObj$predict( seqNumObj )

    return( probObj )

} # seqAnaFun.default()


#  seqAnaFun.default()$predict() 결과에 대한 해석기
# 	    ( 20171029_ReadMe.txt의 "[확률 계산 구조]"" 항목 확인 )
#	- pProbBase : NULL	모두 똑같은 확률로 취급.
#				"mean"	평균 발생률을 기준하여 계산
#				"prob"	연속발생을 고려한 발생확률을 기준하여 계산
#				(ProbAnaObj의 probMtx 참고)
#	- pIsChanging: NULL	고려안함. 대신 pProb가 NULL이면 안됨.
#				"P"	비발생이 발생인 경우만 적용.(isChanging==1)
#				"N"	연속 발생이 불가한 경우만 적용.(isChanging==-1)
#				"A"	P,N 모두 적용.
#	- pCreFunId : c( fun$idStr ,fun$fGIdStr ) 다루는 데이터의 creFun 특성 확인용.
#	- pEleCord	: c( elementIdx ,columnIdx )
#	pProbBase="mean" ;pIsChanging="A" ;pEleCord=c(eIdx,cIdx) ;pStandardize=F ;pCreFunId=NULL	;pUseNA=F
getAnaTranslators <- function( pProbBase="mean" ,pIsChanging="A" ,pEleCord ,pStandardize=F ,pUseNA=F ,pCreFunId=NULL ){

		# trObj <- getAnaTranslators( pEleCord=c(eIdx,cIdx) )
		# trRst <- trObj$translate( eleSet ,pProbAnaObj )

	rObj <- list( probBase=pProbBase ,isChanging=pIsChanging ,standardize=pStandardize ,useNA=pUseNA )
	rObj$creFunId = pCreFunId	;if(!is.null(pCreFunId)) names(rObj$creFunId) <- c("ele","col") # getIoAddr() 참고.
	rObj$eleCord = pEleCord		;if(!is.null(pEleCord)) names(rObj$eleCord) <- c("ele","col")

	rObj$translate <- function( pEleSet ,pProbAnaObj ){

				trObj <- list( eleCord=rObj$eleCord )
				trObj$creFunId <- rObj$creFunId
				trObj$codeVal <- pEleSet$funCodeValLst[[ rObj$eleCord["ele"] ]][[ rObj$eleCord["col"] ]]
				trObj$codeValNA.idx <- pEleSet$funCodeValNAidxLst[[ rObj$eleCord["ele"] ]][[ rObj$eleCord["col"] ]]
				prob <-	if( is.null(rObj$probBase) ){	prob<-rep( 0.5,length(trObj$codeVal) )
							names(prob)<-colnames(pProbAnaObj$probMtx)
							prob
						} else pProbAnaObj$probMtx[ rObj$probBase , ]
				isChanging <- pProbAnaObj$probMtx[ "isChanging" ,]

				if( !rObj$useNA ){
					prob <- prob[ -trObj$codeValNA.idx ]
					isChanging <- isChanging[ -trObj$codeValNA.idx ]
				}

				if( rObj$standardize ){
					prob <- k.standardize( prob ,pPer=F )
				}

				if( !is.null(rObj$isChanging) ){
					if( "P"==rObj$isChanging ){
						prob[ isChanging== 1 ] = 1
					} else if( "N"==rObj$isChanging ) {
						prob[ isChanging==-1 ] = 0
					} else { # "A"
						prob[ isChanging== 1 ] = 1
						prob[ isChanging==-1 ] = 0
					}
				}

				trObj$prob <- prob
				trObj$isChanging <- isChanging

				return( trObj )
			}

	return( rObj )

} # getAnaTranslators()
