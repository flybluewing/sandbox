
set excId="1107"

set anaId=0
START R CMD BATCH --no-save --no-restore "--args anaId=%anaId% excId=%excId% " seqAnaFun.R seqAnaFun_%anaId%.log

set anaId=1
START R CMD BATCH --no-save --no-restore "--args anaId=%anaId% excId=%excId% " seqAnaFun.R seqAnaFun_%anaId%.log


pause()