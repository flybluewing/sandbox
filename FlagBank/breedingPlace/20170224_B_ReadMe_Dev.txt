
[*] tSample.winDef06()
	<1>	getFieldSet.dummy.ptn() 구성이 다음과 같을 때,
				ptn <- c(   1,  0,  0,  1,  1,  0
						 ,  0,  1,  1,  0,  0,  0
						 ,  1,  0,  0,  1,  0,  0
						)
				ptn <- c(   1,  0,  0,  0,  0,  0
						 ,  0,  1,  1,  0,  0,  0
						 ,  0,  0,  0,  1,  0,  0
						)
				ptn <- c(   0,  0,  0,  1,  1,  0
						 ,  0,  1,  1,  0,  0,  0
						 ,  1,  0,  0,  0,  0,  0
						)
				ptn <- c(   0,  0,  0,  1,  1,  0
						 ,  0,  0,  0,  0,  0,  0
						 ,  0,  0,  0,  0,  0,  0
						)
				ptn <- c(   0,  0,  0,  0,  0,  0
						 ,  0,  1,  1,  0,  0,  0
						 ,  0,  0,  0,  0,  0,  0
						)

		* scanRawPattern( ) 결과 패턴
			단, pFiltF=function(pMatPtn,pIdxMtx){return(1>=nrow(pIdxMtx))}
			
				idx:1 
					 1  .  .  1  .  .
					 .  .  .  .  .  .
					 .  .  .  .  .  . 
					compPair 3 
				idx:2 
					 1  .  .  .  .  .
					 .  1  1  .  .  .
					 .  .  .  1  .  . 
					compPair 10,20,30 
				idx:3 
					 .  .  .  1  1  .
					 .  1  1  .  .  .
					 1  .  .  .  .  . 
					compPair 110,120,130,140 
				idx:4 
					 .  .  .  1  1  .
					 .  .  .  .  .  .
					 .  .  .  .  .  . 
					compPair 210,220 
				idx:5 
					 .  .  .  .  .  .
					 .  1  1  .  .  .
					 .  .  .  .  .  . 
					compPair 310 
				
		* findOverlap( ) 1차 패턴
				idx:1 
					 .  .  .  .  .  .
					 .  1  1  .  .  .
					 .  .  .  .  .  . 
				  idxMtx 
					 3,5 2,5 2,3 
				idx:2 
					 .  .  .  1  1  .
					 .  .  .  .  .  .
					 .  .  .  .  .  . 
				  idxMtx 
					 3,4 
				idx:3 
					 .  .  .  1  .  .
					 .  .  .  .  .  .
					 .  .  .  .  .  . 
				  idxMtx 
					 1,4 1,3 
				idx:4 
					 1  .  .  .  .  .
					 .  .  .  .  .  .
					 .  .  .  .  .  . 
				  idxMtx 
					 1,2 

		* findOverlap( ) 2차 패턴
				idx:1 
					 .  .  .  1  .  .
					 .  .  .  .  .  .
					 .  .  .  .  .  . 
				  idxMtx 
					 2,3 
