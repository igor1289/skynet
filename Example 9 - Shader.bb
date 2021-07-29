Include "Skynet++.bb"

SeedRnd MilliSecs()

SE_Init()


;	Load script

;SE_CompileScript("Shader.txt", "Shader.sky")
;If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE
;
;Local Script.SE_Script=SE_LoadScriptExec("Shader.sky")

Local Script.SE_Script=SE_LoadScriptText("Shader.txt")
If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE



;	Output warnings
DebugLog "WARNINGS:"
For W.SE_Warning=Each SE_Warning
	DebugLog W\Message
Next
DebugLog "--------------------------"


Graphics3D 1024, 768, 32, 2
SetBuffer BackBuffer()

Camera=CreateCamera()
PositionEntity Camera, 0, 0, -10
CameraClsColor Camera, 100, 120, 180

CreateLight()

;	Invoke function "init"
Local F.SE_FuncPtr

F=SE_FindFunc(Script, "init")
If F<>Null Then SE_InvokeUserFunction(F)

F=SE_FindFunc(Script, "_main")
SE_InvokeUserFunction(F)

While Not KeyHit(1)
	SE_InvokeUserFunction(F)
	UpdateWorld
	RenderWorld
	Flip
Wend

;~IDEal Editor Parameters:
;~C#Blitz3D