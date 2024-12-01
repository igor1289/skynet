;#1
;// ========================================== all publics and functions in mp_functions.inc
;// *Tip: YOU CAN CREATE GLOBALS IN FUNCTIONS
;// *Tip: NEVER USE () IN KEYWORDS!
;// *Tip:
;//		Never use double functions in using function, example: SetInfo(variable, Function(1, 2), 1) // if you do this, then you can get MAV or something else (Skynet++ Bug)
;//		Use:
;//			local result = Function(1, 2)
;//			SetInfo(variable, result, 1)
;// ========================================================================================================================

;#2
;IS_ENEMY = true


Include "Skynet++.bb"

SeedRnd MilliSecs()

SE_Init()

Local Script.SE_Script=SE_LoadScriptText("Debug.txt")
If SE_ERROR Then RuntimeError SE_ERROR_MESSAGE


;	Output warnings
DebugLog "WARNINGS:"
For W.SE_Warning=Each SE_Warning
	DebugLog W\Message
Next
DebugLog "--------------------------"

;	Instructions
Dim InstructionNames$(38)

InstructionNames(SE_MOV)="MOV"
InstructionNames(SE_ADD)="ADD"
InstructionNames(SE_SUB)="SUB"
InstructionNames(SE_MUL)="MUL"
InstructionNames(SE_DIV)="DIV"
InstructionNames(SE_POW)="POW"
InstructionNames(SE_MOD)="MOD"
InstructionNames(SE_NEG)="NEG"
InstructionNames(SE_INC)="INC"
InstructionNames(SE_DEC)="DEC"
InstructionNames(SE_CE)="CE"
InstructionNames(SE_CNE)="CNE"
InstructionNames(SE_CG)="CG"
InstructionNames(SE_CL)="CL"
InstructionNames(SE_CGE)="CGE"
InstructionNames(SE_CLE)="CLE"
InstructionNames(SE_AND)="AND"
InstructionNames(SE_XOR)="XOR"
InstructionNames(SE_OR)="OR"
InstructionNames(SE_BN)="BN"
InstructionNames(SE_SHL)="SHL"
InstructionNames(SE_SHR)="SHR"
InstructionNames(SE_NOT)="NOT"
InstructionNames(SE_JMP)="JMP"
InstructionNames(SE_JIT)="JIT"
InstructionNames(SE_JIF)="JIF"
InstructionNames(SE_MTA)="MTA"
InstructionNames(SE_INVU)="INVU"
InstructionNames(SE_INVG)="INVG"
InstructionNames(SE_RET)="RET"
InstructionNames(SE_END)="END"
InstructionNames(SE_GP)="GP"
InstructionNames(SE_VT)="VT"
InstructionNames(SE_ARGS)="ARGS"
InstructionNames(SE_CA)="CA"
InstructionNames(SE_ACC)="ACC"
InstructionNames(SE_LEN)="LEN"

;	Instruction value types

Dim ValueTypes$(10)

ValueTypes(SE_NULL)="NULL"
ValueTypes(SE_INT)="INT"
ValueTypes(SE_FLOAT)="FLOAT"
ValueTypes(SE_STRING)="STRING"
ValueTypes(SE_TRANSIENT)="TRANSIENT"
ValueTypes(SE_POINTER)="POINTER"
ValueTypes(SE_ACCESSOR)="ACCESSOR"
ValueTypes(SE_ARRAY)="ARRAY"
ValueTypes(SE_LABEL)="LABEL"
ValueTypes(SE_FUNC_PTR)="FUNC_PTR"

Function OutputInstuctions(Script.SE_Script)
	Local F.SE_FuncPtr = Script\FirstFuncPtr

	Local N = 1

	While F <> Null
		Local I.SE_Inst = F\FirstInst

		DebugLog "[" + F\Name + "]"

		While I <> Null And I <> F\LastInst
			Local L$ = N + ") " + InstructionNames(I\Code) + ": "

			If I\A <> Null Then
				L = L + OutputValue$(I\A)
			End If

			If I\B <> Null Then
				L = L + ", " + OutputValue$(I\B)
			End If


			If I\C <> Null Then
				L = L + ", " + OutputValue$(I\C)
			End If

			DebugLog L$

			I = I\NextInst

			N = N + 1
		Wend

		F = F\NextFuncPtr
	Wend
End Function

Function OutputValue$(V.SE_Value)
	Local Val$ = ""

	If V\ValueType = SE_TRANSIENT Then
		Val = V\IntValue
	Else If V\ValueType = SE_FUNC_PTR Then
		Val = V\FuncPtr\Name
	Else
		Val = SE_ValueToString(V)
	End If

	Return ValueTypes(V\ValueType) + "(" + Val + ")"
End Function

OutputInstuctions(Script)


;Dim Debug_ValueTypes$(10)
;
;Debug_ValueTypes(SE_NULL)="NULL"
;Debug_ValueTypes(SE_INT)="INT"
;Debug_ValueTypes(SE_FLOAT)="FLOAT"
;Debug_ValueTypes(SE_STRING)="STRING"
;Debug_ValueTypes(SE_TRANSIENT)="TRANSIENT"
;Debug_ValueTypes(SE_POINTER)="POINTER"
;Debug_ValueTypes(SE_ACCESSOR)="ACCESSOR"
;Debug_ValueTypes(SE_ARRAY)="ARRAY"
;Debug_ValueTypes(SE_LABEL)="LABEL"
;Debug_ValueTypes(SE_FUNC_PTR)="FUNC_PTR"
;
;Function Debug_OutputValue$(V.SE_Value)
;	Local Val$ = ""
;
;	If V\ValueType = SE_TRANSIENT Then
;		Val = V\IntValue
;	Else If V\ValueType = SE_FUNC_PTR Then
;		Val = V\FuncPtr\Name
;	Else
;		Val = SE_ValueToString(V)
;	End If
;
;	Return Debug_ValueTypes(V\ValueType) + "(" + Val + ")"
;End Function
;
;Function Debug_ArgumentsStack()
;	Local Result$ = ""
;
;	For I = 0 To SE_ARGUMENTS_STACK_LEVEL - 1
;		Local ValueText$ = Debug_OutputValue$(SE_ARGUMENTS_STACK(I))
;
;		If I = SE_ARGUMENTS_STACK_LEVEL - SE_ARGUMENTS_NUMBER - 1 Then
;			ValueText = "|" + ValueText
;		End If
;
;		Result = Result + ValueText + "; "
;	Next
;
;	DebugLog Result
;End Function

;	Invoke function "_main"
Local F.SE_FuncPtr=Script\FirstFuncPtr
SE_InvokeUserFunction(F)
WaitKey()
;~IDEal Editor Parameters:
;~C#Blitz3D