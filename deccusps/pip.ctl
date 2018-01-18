;JOB%1(20) TO MAKE PIP.SHR FROM PIP.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=PIP/RESTART:1
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LINK.SHR
;	LNK???.SHR
;	JOBDAT.REL
;	CREF.SHR
;	SETSRC.SHR
;[SELF]	PIP.MAC
;	PIP.HLP
;
;OUTPUT FILE:
;	PIP.SHR
;OUTPUT LISTINGS:
;	PIP  MAP
;	PIP  CREF LISTING
;	PIP.LOG
;
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF (ERROR)	;TOO BAD
;
;SET LIB TO [10,7]
.RUN DSK:SETSRC[10,7]
*LIB[10,7]
;
.RUN DSK:DIRECT
*TTY:/CHECKSUM=PIP???.*+DIRECT.SHR,COMPIL.SHR,MACRO.SHR,LINK.SHR,LNK???.SHR,CREF.SHR,JOBDAT.REL
;
.ASSIGN DSK: SYS:
;COMPILE, LOAD, AND SAVE; PRODUCING MAP AND CREF FILE
.LOAD /MAP:LPT:PIP /CREF /COMPILE PIP.MAC %"CONTENT:ALL"
.SSAVE DSK:PIP
.VERSION
.IF (ERROR) .E 137
;
;TRY IT JUST TO MAKE SURE IT WORKS
.RUN DSK:PIP
*/Q
;
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=PIP.SHR
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.PLEASE PIP SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DELETE PIP.REL
.IF (ERROR) ;DON'T CARE IF FAILED
