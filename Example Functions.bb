Select FunctionName

	Case "print"
		Print SE_ToStringArg(0)
		
	Case "debuglog"
		DebugLog SE_ToStringArg(0)
	
	Case "error"
		RuntimeError SE_ToStringArg(0)
		
	Case "input_str"
		SE_ReturnString(Input(SE_ToStringArg(0)))
	
	Case "input_int"
		SE_ReturnInt(Input(SE_ToStringArg(0)))
	
	Case "input_float"
		SE_ReturnFloat(Input(SE_ToStringArg(0)))
		
	Case "rand"
		SE_ReturnInt(Rand(SE_ToIntArg(0), SE_ToIntArg(1)))

	Case "rnd"
		SE_ReturnFloat(Rnd(SE_ToFloatArg(0), SE_ToFloatArg(1)))

	Case "cos"
		SE_ReturnFloat(Cos(SE_ToFloatArg(0)))

	Case "sin"
		SE_ReturnFloat(Sin(SE_ToFloatArg(0)))

	Case "create_cube"
		SE_ReturnInt(CreateCube())

	Case "turn"
		TurnEntity SE_ToIntArg(0), SE_ToFloatArg(1), SE_ToFloatArg(2), SE_ToFloatArg(3)

	Case "load_texture"
		SE_ReturnInt(LoadTexture(SE_ToStringArg(0), SE_ToIntArg(1)))

	Case "entity_texture"
		EntityTexture SE_ToIntArg(0), SE_ToIntArg(1)

	Case "position_texture"
		PositionTexture SE_ToIntArg(0), SE_ToFloatArg(1), SE_ToFloatArg(2)
End Select