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


# 	mtxLst <- lapply( c("toZ819","toZ820","toZ821","toZ822") 
# 						,function(nm){ rpt.analyLst( analyLst[[nm]] ) }
# 					)
# 	for( nIdx in colnames( mtxLst[[1]] ) ){
# 		cat( sprintf("name : %s\n",nIdx) )
# 		mtx <- do.call( rbind ,lapply( mtxLst ,function(mtx){mtx[,nIdx]}) )
# 		rptStr <- sprintf("  %s",capture.output(mtx))
# 		cat( sprintf("%s \n",paste(rptStr,collapse="\n")) )
# 	}

# 	fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx ,rpt=T )

# 	stdMI$rawTail
# 	fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
# 	auxCntMtx
# 	cntMtx


analyLst <- list()
# [gold] ---------------------------------------
#	toZ800	: 0		toZ801	: 0		toZ806	: 1
analyLst[["toZ809"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=0	# reb / / c23
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb	/	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# 	/ spanM	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	#	/	/
					,auxZW=1	,auxQuo=0
					,raw =2	,rawFV =1	# fCutU.hasPtn(c(15,NA,23),aZoid)
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1	# 	/ spanM /
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =2
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# reb / quoPtn /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=1	# 	/ spanM	/ c23
					,auxZW=1	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# 	/ zw	/
					,auxZW=0	,auxQuo=1
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# 	/ zw	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# 	/ quoPtn /
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#	/	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
) # analyLst[["toZ809"]]  check

analyLst[["toZ814"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0	# reb / 	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=1	# reb / spanM / c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1	# fStep(1/2)
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	# reb /	/
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
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	# reb /	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=2	# 	/ nbor spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# 	/ nbor	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	#	/	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0	# reb /	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	#	/	/
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
) # analyLst[["toZ814"]]

analyLst[["toZ816"]] <- list(	stdFltCnt = 0
	,fCutCnt.basic = list(	commonCutCnt=0	#	/	/	
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=0	#	/	/
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0	#	/	/ 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0	# reb /	/ c23
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1	# reb / spanM /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =2	# 2 * fCutU.hasPtn(c(18,19))
					,rem =1	,cStep =3	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
) # analyLst[["toZ816"]]

# [late] ---------------------------------------
analyLst[["toZ819"]] <- list(	stdFltCnt =2
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =2
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =2	,rawFV =0
					,rem =0	,cStep =0	,fStep =1
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# reb / spanM / c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0	# reb /	/ c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# reb / spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=2	# 	/ zw / c25 max2
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0	#	/	/ c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# reb / spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =3	,fStep =1
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =2	,fStep =0
				)
) # analyLst[["toZ819"]] check

analyLst[["toZ820"]] <- list(	stdFltCnt = 1	# (A0110.A 필터 제외)
	,fCutCnt.basic = list(	commonCutCnt=1	# 	/ cStep2 /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext = 0
	,fCutCnt.colValSeqNext.cStep = 0
	,fCutCnt.nextZW = list(	commonCutCnt=1	# reb / quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=2	# / zw cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=2	# reb / zw remH1 /
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1	# fStep(1/2)
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
) # analyLst[["toZ820"]]

analyLst[["toZ821"]] <- list(	stdFltCnt = 3
	,fCutCnt.basic = list(	commonCutCnt=3	# 	/ quoAll cStep2 cStep3 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =4
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0	#	/	/ c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1	# 	/ quoPtn / c31 c21 c22 c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1	#	/ quoAll / c21
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# reb / quoPtn /
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0	# reb /	/ c22
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1	# 	/ quoPtn / 
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# 	/ zw / c21 c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =3	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
) # analyLst[["toZ821"]]

analyLst[["toZ822"]] <- list(	stdFltCnt = 3
	,fCutCnt.basic = list(	commonCutCnt=0	# reb /	/ c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =1
	,fCutCnt.colValSeqNext.cStep =3
	,fCutCnt.nextZW = list(	commonCutCnt=2	# reb / spanM quoPtn  / c34 c25
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1	# 	/ quoPtn / c24
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0	#	/	/ c33
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =2,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =2
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=1	# 	/	/ cStep2
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
) # analyLst[["toZ822"]]

analyLst[["toZ823"]] <- list(	stdFltCnt =1
	,fCutCnt.default =0
	,fCutCnt.basic = list(	commonCutCnt=0	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =0
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=2	# 	/ nbor quoAll / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =1	# fCutU.hasPtn(c(12,NA,24),aZoid)
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=1	# 	/ spanM / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0	#	/	/ c25
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =3	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1	# reb /	/ 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1	# 	/ cStep2 / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1	# 	/ quoAll / 
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =1
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0	# reb /	/	
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =1
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1	# reb /	spanM / c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=1	# reb / spanM / c21
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
) # analyLst[["toZ823"]]

