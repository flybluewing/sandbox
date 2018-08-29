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

} # rpt.analyLst()


analyLst <- list()

analyLst[["toZ809"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =1
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =1
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =1
					,rem =2	,cStep =0	,fStep =2
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=4
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=1	,auxQuo=0
					,raw =0	,rawFV =4
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1
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
	,fCutCnt.basic = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =3
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =5
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =2
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=3
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

analyLst[["toZ816"]] <- list(	stdFltCnt =0
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =NA
	,fCutCnt.colValSeqNext.cStep =NA
	,fCutCnt.nextZW = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=1
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextFStepBin = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=2
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =4
					,rem =1	,cStep =3	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
) # analyLst[["toZ816"]]

analyLst[["toZ820"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.colValSeqNext = 0
	,fCutCnt.colValSeqNext.cStep = 0
	,fCutCnt.nextZW = list(	commonCutCnt=2
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =2	,cStep =0	,fStep =0
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
	,fCutCnt.nextFStepBin = list(	commonCutCnt=2
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_1 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=5
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextColVal_6 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =1	,fStep =0
				)
) # analyLst[["toZ820"]]

analyLst[["toZ821"]] <- list(	stdFltCnt = 1
	,fCutCnt.basic = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =0	,fStep =0
				)
	,fCutCnt.colValSeqNext =4
	,fCutCnt.colValSeqNext.cStep =0
	,fCutCnt.nextZW = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextBin = list(	commonCutCnt=0
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextQuo10 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =1	,cStep =0	,fStep =1
				)
	,fCutCnt.nextRebNum = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
				)
	,fCutCnt.nextCStepBin = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =1	,cStep =0	,fStep =0
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
	,fCutCnt.nextColVal_2 = list(	commonCutCnt=3
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =0	,cStep =2	,fStep =0
				)
	,fCutCnt.nextColVal_3 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =0	,rawFV =0
					,rem =0	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_4 = list(	commonCutCnt=1
					,auxZW=0	,auxQuo=0
					,raw =1	,rawFV =0
					,rem =2	,cStep =1	,fStep =0
				)
	,fCutCnt.nextColVal_5 = list(	commonCutCnt=1
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


