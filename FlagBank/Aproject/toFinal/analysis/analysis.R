#	rpt.analyLst( analyLst[["toZ821"]] )
rpt.analyLst <- function( analy ){
	#	analy <- analyLst[["toZ821"]]
	cat( sprintf( "* stdFltCnt:%d \n" ,analy$stdFltCnt )  )
	cat( sprintf( "    colValSeqNext:%d \n" ,analy$fCutCnt.colValSeqNext )  )
	cat( sprintf( "    colValSeqNext.cStep:%d \n",analy$fCutCnt.colValSeqNext.cStep	) )

	phaseNm <- attributes(analy)$names
	phaseNm <- phaseNm[-grep("colValSeqNext",phaseNm)]
	phaseNm <- phaseNm[-grep("^stdFltCnt$",phaseNm)]
	scoreMtx <- do.call( rbind, 
							lapply(phaseNm,function(nm){
										do.call( c ,analy[[nm]] )
									})
						)
	rownames(scoreMtx) <- gsub("^fCutCnt\\.","",phaseNm)
	rptStr <- sprintf("    %s",capture.output(scoreMtx))
	cat( sprintf("%s \n",paste(rptStr,collapse="\n")) )
	return( scoreMtx )
} # rpt.analyLst()


# 	mtxLst <- lapply( c("toZ819","toZ820","toZ821") 
# 						,function(nm){ rpt.analyLst( analyLst[[nm]] ) }
# 					)
# 	for( nIdx in colnames( mtxLst[[1]] ) ){
# 		cat( sprintf("name : %s\n",nIdx) )
# 		mtx <- do.call( rbind ,lapply( mtxLst ,function(mtx){mtx[,nIdx]}) )
# 		rptStr <- sprintf("  %s",capture.output(mtx))
# 		cat( sprintf("%s \n",paste(rptStr,collapse="\n")) )
# 	}
# 	fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx ,rpt=T )


analyLst <- list()

analyLst[["toZ809"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=1	# rebCol
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=1	# reb
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# spanM
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=1	,auxQuo=0
					,raw =2	,rawFV =1	# fCutU.hasPtn(c(15,NA,23),aZoid)
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1	# spanM
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =2
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=2	# reb quoPtn
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=1	# spanM
					,auxZW=1	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# quoPtn
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# zw
					,auxZW=0	,auxQuo=1
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# zw
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# quoPtn
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
) # analyLst[["toZ809"]]

analyLst[["toZ814"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=1	# reb
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=2	#  reb spanM
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0	# fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# reb
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=1	#  reb
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=2	# nbor spanM
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	#  reb
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
) # analyLst[["toZ814"]]

analyLst[["toZ816"]] <- list(	stdFltCnt = 0
	,fCutCnt.basic = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=NA	# reb2 rebCol
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=NA	# rebCol
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=NA	# reb2
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=NA	# rebCol
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA	# 2* fCutU.hasPtn(c(18,19),aZoid)
										# 2  fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=NA	# reb2 rebCol
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=NA	# reb2 rebCol
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=NA	# reb2 rebCol
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
) # analyLst[["toZ816"]]

analyLst[["toZ819"]] <- list(	stdFltCnt =2
	,fCutCnt.basic = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.colValSeqNext =2
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
) # analyLst[["toZ819"]]

analyLst[["toZ820"]] <- list(	stdFltCnt = 2
	,fCutCnt.basic = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.colValSeqNext = 0
	,fCutCnt.colValSeqNext.cStep = 0
	,fCutCnt.nextZW = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
) # analyLst[["toZ820"]]

analyLst[["toZ821"]] <- list(	stdFltCnt = 3
	,fCutCnt.basic = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.colValSeqNext =4
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=NA
					,auxZW=NA	,auxQuo=NA
					,raw =NA	,rawFV =NA
					,rem =NA	,cStep =NA	,fStep =NA
				)
) # analyLst[["toZ821"]]


