;	Script instance example

Include "Skynet++.bb"

SeedRnd MilliSecs()

SE_Init()


;	Load script
;SE_CompileScript("Instance.txt", "Instance.sky")
;If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE
;
;Local Script.SE_Script=SE_LoadScriptExec("Instance.sky")

Local Script.SE_Script=SE_LoadScriptText("Instance.txt")
If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE



;	Output warnings
DebugLog "WARNINGS:"
For W.SE_Warning=Each SE_Warning
	DebugLog W\Message
Next
DebugLog "--------------------------"




;	Create instance
Local I.SE_Instance=SE_CreateInstance(Script)

SE_SetValue(I\FirstPublic\Value, "Hello world!", SE_STRING)
SE_SetInstance(I)



;	Invoke function "_main"
Local F.SE_FuncPtr=Script\FirstFuncPtr
SE_InvokeUserFunction(F)

SE_UnsetInstance(I)