Include "Skynet++.bb"

SeedRnd MilliSecs()

SE_Init()


;	Load script

;SE_CompileScript("Misc.txt", "Misc.sky")
;If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE
;
;Local Script.SE_Script=SE_LoadScriptExec("Misc.sky")

Local Script.SE_Script=SE_LoadScriptText("Misc.txt")
If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE



;	Output warnings
DebugLog "WARNINGS:"
For W.SE_Warning=Each SE_Warning
	DebugLog W\Message
Next
DebugLog "--------------------------"



;	Invoke function "_main"
Local F.SE_FuncPtr=Script\FirstFuncPtr
SE_InvokeUserFunction(F)