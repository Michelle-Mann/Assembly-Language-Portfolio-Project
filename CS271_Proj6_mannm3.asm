TITLE Project6: String Primitives & Macros     (Proj6_mannm3.asm)

; Author: Michelle Mann
; Last Modified: 11/27/2022
; OSU email address: mannm3@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 12/4/2022
; Description: This program asks the user to input 10 signed integers (as strings) that will fit inside a 32-bit register. Each of these 
; strings are validated (making sure they do not contain anything other than the signs "-","+" , or numerical values) before 
; translating them to the appropriate decimal value. Once complete, these values are summed together to display their truncated value.
; On the programmers side of the world, this program translates forward and backwards decimal to string values and read and writes those 
; values to appropriate arrays. This program also uses macros for GetString and DisplayString applications.


INCLUDE Irvine32.inc


; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Prints a prompt to the user that includes the line count and the current total sum.
; Asks the user for a string input within 20 bytes (21 with null-terminator)
; Stores string for decimal calculation, validation and byte count of input.
;
; Preconditions: EDX is the offset address of any string prompts. ReadString requires EDX to be pointed
; to address of written string, and ECX to be the string buffer. EDI is used to write to the address of new string.
;
; Postconditions: EDX becomes the address location of our newly input string.
;  EAX holds the length (in bytes) of the string that was input. All registers (EAX - EDX) including EDI
; are used and restored. 
;
; Receives:
; prompt1		= totalPrompt
; prompt2		= inputPrompt
; countNum		= the line count
; subtotal		= the current running total of all inputs.
; stringOutput	= the offset address of the string that the user input.
; stringBuffer	= the max number of bytes allowed for input
; numCharacters	= that number of characters that was ipnut as a result of ReadString
; lowStringADD	= OFFSET of lowString global
; tensArrADD	= OFFSET of tensArray global
;
; returns: a user-input string, and the number of bytes of said string.
; ---------------------------------------------------------------------------------

mGetString		MACRO prompt1, prompt2, countNum, subtotal, stringOutput, numCharacters, tensArrADD, lowStringADD
	PUSHAD
	
	; input for user will look like: "0. Your current subtotal is: ??? ---- Please enter a signed integer: "
	MOV		EAX, countNum
	MOV		EBX, [EAX]
	PUSH	lowStringADD
	PUSH	tensArrADD
	PUSH	EBX
	CALL	WriteVal									; Writes the lineCount as string
	MOV		EDX, prompt1
	CALL	WriteString									; Writes prompt2
	MOV		EAX, subtotal
	MOV		EBX, [EAX]
	PUSH	lowStringADD
	PUSH	tensArrADD
	PUSH	EBX
	CALL	WriteVal									; Writes subtotal as string
	MOV		EDX, prompt2
	CALL	WriteString									; Writes prompt2

	; Prep for Reading String
	MOV		EDX, stringOutput
	MOV		ECX, BUFFERSIZE
	CALL	ReadString

	; Stores EAX to ByteCount
	MOV		EBX, numCharacters							
	MOV		[EBX], EAX

	POPAD
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Prints a string after moving the offset address reference to EDX.
;
; Preconditions: OFFSET address of string should be passed. String should be null-terminated. 
; 
; Postconditions: EDX is modified and restored. 
;
; Receives:
; stringToPrint = the OFFSET starting address of the string in which to print.
;
; returns: a printed string for the user.
; ---------------------------------------------------------------------------------

mDisplayString	MACRO stringToPrint:REQ
	PUSH	EDX
	MOV		EDX, stringToPrint
	CALL	WriteString
	POP		EDX
ENDM


; (insert constant definitions here)
POSITIVE	= 43						; The decimal value of the "+" sign
NEGATIVE	= 45						; The decimal value of the "-" sign
ZERO		= 48						; The decimal value of "0"

TOGSTATE_0	= 0							; For NegToggle, FirstDigit or ValidToggle = 0, neutral state
TOGSTATE_1	= 1							; For NegToggle, FirstDigit or ValidToggle = 1, triggered state
TOGSTATE_2	= 2							; For NegToggle, FirstDigit or ValidToggle = 2, alt triggered state

LOWEST		= -2147483648				; The lowest decimal

BUFFERSIZE	= 21						; Buffersize.

TENSARRAYL	= LENGTHOF tensArray		; The length of tensArray (an array of constant values)
TENSARRAYS	= SIZEOF tensArray			; The size of tensArray (an array of constant values)
TENSARRAYT	= TYPE tensArray			; The type of all values in tensArray (an array of constant values)

NUMARRAYL	= LENGTHOF numArray			; The length of numArray (an array of constant values)
NUMARRAYS	= SIZEOF numArray			; The size of numArray (an array of constant values)
NUMARRAYT	= TYPE numArray				; The type of all values in numArray (an array of constant values)

INPUTARRL	= LENGTHOF inputArray		; The length of inputArray
INPUTARRT	= TYPE inputArray			; The type of all values of inputArray

.data

	intro1		BYTE	"Project #6: To String?...Or Not to String?...Macro's the Question!			Written By: Michelle Mann",0

	instruct1	BYTE	"Welcome, Gumshoe! Your task -=should you chose to accept it=- is to enter 10 signed decimal integers.",0
	instruct2	BYTE	 "Each integer needs to be small (or large) enough to fit in a 32-bit register. Once we've entered 10,",0
	instruct3	BYTE	"this program will display that list of integers, sum them, and return for you their average!",0
	instruct4	BYTE	"0. All inputs can accept up to 20 characters! Let's get to it!",0

	ec1			BYTE	"EC#1: This program will print line #s 1-10 based on number of inputs and our total sum after each input.",0

	totalPrompt	BYTE	". Your current total is : ",0
	prompt1		BYTE	" ---- Please enter a signed integer: ",0

	error1		BYTE	"ERROR: Your number was either too large or contained invalid characters. Please try again.",0

	printOut1	BYTE	"You entered the following numbers: ",0
	commaSpace	BYTE	", ",0
	printOut2	BYTE	"The sum of these numbers is: ",0
	printOut3	BYTE	"The truncated average is: ",0

	goodbye		BYTE	"Great work, Gumshoe!",0

	lowString	BYTE	"-2147483648",0

	; Instead of writing 20-separate constants here, I'm opting to use 2 arrays full of constant values 
	; These are referenced by address.
	tensArray	DWORD	1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1
	numArray	DWORD	48,49,50,51,52,53,54,55,56,57

	inputArray	SDWORD	10 DUP(?)
	lineCount	DWORD	0
	intSum		SDWORD	0

	byteCount	SDWORD	?
	tempDec		SDWORD	?
	avgValue	SDWORD	?

.code
main PROC

	_introduction:
	; Prints an introduction for the user.
		PUSH	OFFSET instruct4
		PUSH	OFFSET ec1
		PUSH	OFFSET instruct3
		PUSH	OFFSET instruct2
		PUSH	OFFSET instruct1
		PUSH	OFFSET intro1
		CALL	introduction

	 ; Set Loop Counter
		MOV		ECX, INPUTARRL
		MOV		EDI, OFFSET inputArray

	_getVals:
	; Obtains 10 user input strings and writes the decimal equivalences to inputArray
		INC		lineCount
		PUSH	OFFSET lowString
		PUSH	OFFSET numArray
		PUSH	OFFSET tensArray
		PUSH	OFFSET error1
		PUSH	OFFSET totalPrompt
		PUSH	OFFSET prompt1
		PUSH	OFFSET tempDec
		PUSH	OFFSET byteCount
		PUSH	OFFSET inputArray
		PUSH	OFFSET intSum
		PUSH	OFFSET lineCount
		CALL	ReadVal

		MOV		EAX, tempDec
		ADD		intSum, EAX
		
		LOOP	_getVals

	_calculateAverage:
	; Calculates the average of the 10 numbers.
		MOV		EAX, intSum
		MOV		EBX, lineCount
		CDQ
		IDIV	EBX

		MOV		avgValue, EAX

	_printCalculations:
	; Prints all the calcultations for the user.
		PUSH	OFFSET lowString
		PUSH	OFFSET tensArray
		PUSH	OFFSET goodbye
		PUSH	OFFSET commaSpace
		PUSH	OFFSET printOut3
		PUSH	OFFSET printOut2
		PUSH	OFFSET printOut1
		PUSH	OFFSET inputArray
		PUSH	intSum
		PUSH	avgValue
		CALL	printThings

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; ---------------------------------------------------------------------------------
; Name: introduction
;
; Displays introduction print statements for user which includes title, instructions and ec posts.
;
; Preconditions: OFFSET address of string should be passed. String should be null-terminated. 
;
; Postconditions: EDX is manipulated multipled times and is restored after exit.
;
; Receives:
;	[EBP+8]		= OFFSET intro1
;	[EBP+12]	= OFFSET instruct1
;	[EBP+16]	= OFFSET instruct2
;	[EBP+20]	= OFFSET instruct3
;	[EBP+24]	= OFFSET ec1
;	[EBP+28]	= OFFSET instruct4
; returns: 
;	printed string.
; ---------------------------------------------------------------------------------

introduction PROC
	PUSH	EBP
	MOV		EBP, ESP

	MOV		EDX, [EBP+8]					; OFFSETS intro1
	mDisplayString EDX
	CALL	CrLf
	CALL	CrLf
	MOV		EDX, [EBP+12]					; OFFSETS instruct1
	mDisplayString EDX
	CALL	CrLf
	MOV		EDX, [EBP+16]					; OFFSETS instruct2
	mDisplayString EDX
	CALL	CrLf
	MOV		EDX, [EBP+20]					; OFFSETS instruct3
	mDisplayString EDX
	CALL	CrLf
	CALL	CrLf
	MOV		EDX, [EBP+24]					; OFFSETS ec1
	mDisplayString EDX
	CALL	CrLf
	CALL	CrLf
	MOV		EDX, [EBP+28]					; OFFSETS instruct 4
	mDisplayString EDX
	CALL	CrLf
	CaLL	CrLf

	POP	EBP
	RET 20
introduction ENDP


; ---------------------------------------------------------------------------------
; Name: WriteVal
;
; Takes a user-input decimal integer and returns the string ascii representation of that integer.
; The only numbers that will need to ever be printed in this direction are those already validated, So we can 
; assume they will always been valid, SDWORD sized integers.
;
; Preconditions: decVal must be able to fit inside of a 32-bit register. Any other values will be truncated to fit.
;	
; Postconditions: All registers are manipulated and restored after usage (EAX, EBX, ECX, EDX, ESI, EDI)
;
; Receives:
;	[EBP+8]		= decVal				; The decimal value that will ultimately be printed as a string.
;	[EBP+12]	= OFFSET tensArray
;	[EBP+16]	= OFFSET lowString
;
; returns: 
;	printed string.
; ---------------------------------------------------------------------------------

WriteVal PROC
	 LOCAl	lTempString[12]:BYTE			; LOCAL address of our string that will ultimately be printed. 
	 LOCAL	lHoldNum:SDWORD					; copy of the decimal number we are manipulating; so we don't change the original
	 LOCAL	lDigitToStore:DWORD				; the digit that will be written to a "string" in each stringarray
	 LOCAL	lFirstDigit:DWORD				; a toggle that addresses when we've reached our first non-0 or symbolic character

	 PUSHAD

	 ; Count is initialized to 0, EDI is initialized to the start of our tempString
	 MOV	lFirstDigit, TOGSTATE_0
	 LEA	EDI, lTempString
	 MOV	ESI, [EBP+12]
	 
	 ; Copies decVal to holdNum so we don't manipulate decVal
	 MOV	EAX, [EBP+8]
	 MOV	lHoldNum, EAX

	 ; If this is the LOWEST decimal that fits in a 32-bit register, negating results in overflow (in which we lose "-".)
	 ; Handle this case specially.
	 CMP	lHoldNum, LOWEST
	 JE		_lowestAvailNeg

	 ; Compare input SDWORD to 0. If less than, we know our first character will be a "-"
	 CMP	lHoldNum, 0 
	 JL		_negativeNum
	 JG		_positiveNumPrep
	 JE		_isZero

	 _negativeNum:
	 ; If number is negative, the lowest number it can possibly be is -2,147,483,648 (11 digits total)
		
		; First step is negating that number (I want to deal with the positive at this point.)
		; Then write the "-" to our tempString and continue onward.
		NEG		lHoldNum
		MOV		EAX, 0
		MOV		AL, NEGATIVE
		STOSB	
		JMP		_positiveNumPrep

	 _positiveNumPrep:
	 ; At this point all we're doing is setting our loop counter to max digits the value can hold. 
	 ; In this case, the billions place.
		MOV		ECX, TENSARRAYL

	_testLoop:
	; The meat and potatoes. This divides each digit our integer to write by the nearest 0's place (i.e. a value of 275 to 100).
	; EAX after division will be the digit that needs to be written. We then multiply that number by the correct place value 
	; (i.e. 2 x 100 = 200) and subtract that number from our holdNum and progress to the next 0's place.
		LEA		EDX, lDigitToStore
		LEA		EAX, lHoldNum
		PUSH	EDX
		PUSH	EAX
		PUSH	[ESI]
		CALL	Division

		; If the value that is populated to EAX is 0, then either write 0 to string or ignore it (in the case of a leading zero).
		MOV		EAX, 0
		CMP		lDigitToStore, EAX
		JE		_valIsZero
		JG		_valNotZero

	_valIsZero:
	; Prints a 0 to string (48d)

		; Checks if there are any other non-zero digits (to remove leading zeroes). If not, continues past this digit.
		; Otherwise, writes the zero, increments ESI to the next 0s place. and loops again.
		CMP		lFirstDigit, TOGSTATE_0
		JE		_nextDigit
		MOV		EAX, ZERO
		STOSB
		JMP		_nextDigit

	_valNotZero:
	; Prints a non-zero digit to string (48d plus valueToStore)
		
		; Toggles firstDigit to reflect 1 instead of 0 from this point on.
		MOV		lFirstDigit, TOGSTATE_1

		; Adds 48 to valueToStore (48d is 0 in ascii). Writes value to string.
		MOV		EAX, lDigitToStore
		ADD		EAX, 48
		STOSB

		; Preps for the subtraction of our holdNum.
		LEA		EAX, lDigitToStore
		LEA		EDX, lHoldNum
		PUSH	EDX
		PUSH	EAX
		PUSH	[ESI]
		CALL	SubAndTotal

		JMP		_nextDigit

	_nextDigit:
	; Starts the loop again. Increments ESI and runs loop.
		ADD		ESI, TENSARRAYT
		LOOP	_testLoop

		JMP		_terminateString

	_isZero:
	; When value is actually just 0. Writes the zero and terminates. Then exits.
		
		MOV		EAX, ZERO
		STOSB
		JMP		_terminateString

	_terminateString:
	; Writes the null zero to the end of the string.
	; Finally Writes the string stored in tempString via mDisplayString.
		MOV		EAX, 0
		STOSB
		LEA		EAX, lTempString
		mDisplayString EAX
		JMP		_exits

	_lowestAvailNeg:
	; if the lowest negative, this decimal can't fit in 32-bit reg when negated. So just print the string.
		MOV		EAX, [EBP+16]
		mDisplayString EAX

	_exits:
		POPAD
		RET 12
WriteVal ENDP


; ---------------------------------------------------------------------------------
; Name: Divison
;
; Takes a positive integer and divides it by a subsequent value. Returns the value in EAX as 
; valueToStore. Returns remainder in EDX (although we are not technically using it here.)
;
; Preconditions: [EBP+8] should not be 0. Cannot divide by 0.
;
; Postconditions: EAX, EBX, EDX are all manipulated and restored. 
;
; Receives:
;	[EBP+8]		= value in TENSARRAY			; The number that will be divided by
;	[EBP+12]	= holdNum						; The number that will be divided
;	[EBP+16]	= offset lValuetoStore			; the EAX value after division.
;
; returns: 
;	[EBP+16]	= EAX value after divison.
; ---------------------------------------------------------------------------------

Division PROC
	PUSH	EBP
	MOV		EBP, ESP

	; Temp stores all EAX, EBX, and EDX values on the stack to recall after procedure completes.
	PUSH	EAX
	PUSH	EBX
	PUSH	EDX

	; Sets [EBP+8] to dividend and [EBP+12] as divisor. Sign extends and divides.
	MOV		EDX, [EBP+12]
	MOV		EAX, [EDX]
	MOV		EBX, [EBP+8]
	CDQ
	DIV		EBX

	; The return value must be temporarily stored in EDX and called from there.
	MOV		EDX, [EBP+16]
	MOV		[EDX], EAX

	; Restores values.
	POP EDX
	POP	EBX
	POP EAX

	; Exits.
	POP EBP
	RET	12
Division ENDP


; ---------------------------------------------------------------------------------
; Name: SubAndTotal
;
; Creates the value that should be subtracted from holdNum and does the calculation.
;
; Preconditions: None
;
; Postconditions: EAX, EBX, EDX are all manipulated and restored. 
;
; Receives:
; Receives:
;	[EBP+8]		= [ESI]						; the current value from TENSARRAY
;	[EBP+12]	= OFFSET valueToStore		; Essentially, the EAX value after holdNum is divided by TENSARRAY
;	[EBP+16]	= OFFSET holdNum			; Our decimal value
;
; returns: 
;	[EBP+16]	= an Updated holdNum
; ---------------------------------------------------------------------------------

SubAndTotal PROC
	PUSH	EBP
	MOV		EBP, ESP

	; Temp stores all EAX, EBX values on the stack to recall after procedure completes.
	PUSH	EAX
	PUSH	EBX

	; Moves values to registers for calculations. EAX = valueToStore, EBX = ESI [value in TENSARRAY]
	MOV		EBX, [EBP+8]
	MOV		EDX, [EBP+12]
	MOV		EAX, [EDX]

	MUL		EBX			

	; Subtracts valueToStore * ESI from holdNum for new holdNum
	MOV		EBX, [EBP+16]
	MOV		EDX, [EBX]
	SUB		EDX, EAX

	MOV		[EBX], EDX

	; Restores all.
	POP		EBX
	POP		EAX
	
	; Exits.
	POP		EBP
	RET		12
SubAndTotal ENDP


; --------------------------------------------------------------------------------
; Name: ReadVal
;
; Takes a users input (as a string), validates if it is an appropriate 32-bit decimal value, 
; then deposits that decimal value into an array of input integers for later calculations.
; In the event of an inproper input, displays error message and attemps input again.
;
; Preconditions: User input will be a string. 
;
; Postconditions: All register values (EAX, EBX, ECX, EDX, ESI and EDI) are all manipulated and returned.
;
; Receives:
;	[EBP+48]	= OFFSET lowString
;	[EBP+44]	= OFFSET numArray
;	[EBP+40]	= OFFSET tensArray
;	[EBP+36]	= OFFSET error1
;	[EBP+32]	= OFFSET totalPrompt
;	[EBP+28]	= OFFSET prompt1
;	[EBP+24]	= OFFSET tempDec
;	[EBP+20]	= OFFSET byteCount
;	[EBP+16]	= OFFSET inputArray
;	[EBP+12]	= OFFSET intSum
;	[EBP+8]		= OFFSET lineCount
;
; returns: 
;	[EBP+8]		= updated lineCount
;	[EBP+12]	= updated intSum
;	An array item (in the form of decimal value)
;	Can alternatively return an error before prompting to user to try again.
; ---------------------------------------------------------------------------------

ReadVal	PROC

	LOCAL	lInputString[21]:BYTE					; a temp location for storing input string
	LOCAL	lNegToggle:DWORD						; a toggle for Negative [0 = non-negative, 1 = negative, 2 = positive w/ +]
	LOCAL	lValidToggle:DWORD						; a toggle for Valid input [0 = valid, 1 = invalid]
	
	PUSHAD
	_getInput:
		; get user input

		LEA		EDX, lInputString
		MOV		EDI, EDX

		; in call: [prompt1], [prompt2], [AddlineCount], [AddintSum], [AddInputString], [AddbyteCount]
		mGetString [EBP+32], [EBP+28], [EBP+8], [EBP+12], EDI, [EBP+20], [EBP+40], [EBP+48]
		
	
	_validatesPrep:
	; Prep for validation.

		; initiates toggle values appropriately every time procedure is called. 
		MOV		lNegToggle, TOGSTATE_0
		MOV		lValidToggle, TOGSTATE_0

		; prep for validation call including loop counter set and pointing ESI appropriately.
		LEA		ESI, lInputString
		MOV		EAX, [EBP+20]
		MOV		ECX, [EAX]

		; Decrements ECX, we don't actually want to hit the null terminator here.
		; Unless this is a single character -- with which we'll handle specially.
		DEC		ECX

		; The first char is the only char allowed to be - or +. Check this first.
		MOV		EAX, 0
		LODSB

		CMP		EAX, POSITIVE
		JE		_togglePos

		CMP		EAX, NEGATIVE
		JE		_toggleNeg
		JMP		_validateChar

	_togglePos:
	; Toggles Pos of first character.
		MOV		lNegToggle, TOGSTATE_2
		JMP		_loadNewChar

	_toggleNeg:
	; Toggles Neg of first character.
		MOV		lNegToggle, TOGSTATE_1
		JMP		_loadNewChar

	_loadNewChar:
	; Loads a new character.
		MOV		EAX, 0
		LODSB
	
	_validateChar:
	; A loop that validates each character of a string. Must be 0:9 at this point.

		; Resets EAX value before LODSB
		LEA		EBX, lNegToggle
		LEA		EDX, lValidToggle
		PUSH	[EBP+44]
		PUSH	EDX
		PUSH	EBX
		PUSH	EAX
		CALL	ValidChar

		; Checks if validToggle has been flipped. If so, string is invalid. If not, go to next character.
		CMP		lValidToggle, TOGSTATE_1
		JE		_invalidString

		; If this was a single-character decimal, ECX would zero out before trying to LOOP. Catches this case.
		CMP		ECX, 0
		JE		_processDigits

		; Otherwise, decrements ECX and runs again. Or, if ECX = 0 after decrement, moves towards processing.
		LOOP	_loadNewChar

		; If we're done with validating each character, on to the next step
		JMP		_processDigits

	_processDigits:
	; exchanges string for decimal value by calling CalculateDec procedure.
		LEA		EAX, lInputString
		LEA		EDX, lValidToggle
		LEA		ECX, lNegToggle
		PUSH	[EBP+40]						; ADD tensArray
		PUSH	ECX								; ADD lNegToggle
		PUSH	EDX								; ADD lValidToggle
		PUSH	[EBP+24]						; ADD tempDec
		PUSH	EAX								; lInputString
		PUSH	[EBP+20]						; ADD byteCount
		CALL	CalculateDec

		; Checks if invalid toggle has been thrown at this point. If so, throws error. If not, continues.
		MOV		EAX, TOGSTATE_1
		CMP		lValidToggle, EAX
		JE		_invalidString

		JMP		_validDec
	
	_toggleInvalid:
	; Check for invalid toggle. If so, proceed with error message.
		MOV		EBX, TOGSTATE_1
		MOV		lValidToggle, EBX	
		JMP		_invalidString

	_invalidString:
	; If deemed invalid, prints error message on new line, then attempts input again. Nothing else changes. 
		
		; Moves offset address of error message to EDX for mDisplayString.
		CALL	CrLf
		MOV		EDX, [EBP+36]
		mDisplayString EDX
		CALL	CrLf

		; Jumps to input attempt again.
		JMP		_getInput

	_validDec:
	; When confirmed validation happens.

		; if still valid at this point, write this value to the array at ((count-1) * 4) + EDI
		; EAX holds current TempDec, then LineCount and multiiplies them together.
		MOV		EAX, [EBP+24]				; Current TempDec
		MOV		EBX, [EAX]
		MOV		EDX, [EBP+8]				; Current LineCount
		MOV		EAX, [EDX]
		DEC		EAX
		MOV		EDX, INPUTARRT
		MUL		EDX
		MOV		EDI, [EBP+16]				; inputArray
		ADD		EDI, EAX
		MOV		[EDI], EBX

		JMP		_exit

	_exit:
	POPAD
	RET 44
ReadVal ENDP


; ---------------------------------------------------------------------------------
; Name: ValidChar
;
; Loops through string to confirm all values are valid (i.e. +, -, 0:9)
;
; Preconditions: None.
;
; Postconditions: All EAX, EBX, ECX, EDX and ESI values are adjusted and restored.
;
; Receives:
;	[EBP+8]		= character							
;	[EBP+12]	= toggles for negative.
;	[EBP+16]	= toggle for invalid character.
;	[EBP+20]	= OFFSET address for numArray
;
; returns: 
;	[EBP+12]	= updated toggles for negative.
;	[EBP+16]	= updated toggle for invalid character
; ---------------------------------------------------------------------------------

ValidChar PROC

	PUSH	EBP
	MOV		EBP, ESP
	PUSH	ESI
	PUSHAD

	MOV		ESI, [EBP+20]
	MOV		EAX, [EBP+8]

	MOV		ECX, NUMARRAYL
	
	_isValidCharTest:
	; Is this value between 0 - 9. If so and confirmed, exit, if not, toggle validity.
	; Loops through numArray and makes sure character is valid (i.e. exists in array)

		MOV		EBX, [ESI]
		CMP		EAX, EBX

		JE		_NextChar
		JNE		_notValidCharYet

	_notValidCharYet:
	; If not valid, move ESI to next valid character and check again.
	; IF end of loop is reached, toggle not valid.

		ADD		ESI, NUMARRAYT
		LOOP	_isValidCharTest

		JMP		_toggleValidity

	_toggleValidity:
	; If end of loop is reached, character is invalid, which means invalid input. Toggle validity and return for error processing.
		MOV		EBX, 1
		MOV		EDX, [EBP+16]
		MOV		[EDX], EBX
		
		JMP		_NextChar

	_NextChar:
	; Returns for next char.
		POPAD
		POP ESI
		POP EBP
		RET 16
ValidChar ENDP


; ---------------------------------------------------------------------------------
; Name: CalculateDec
;
; Sums a decimal value that is input as a string.
;
; Preconditions: [EBP+16] is a string of anscii characters that we will translate to dec. TENSARRAY is
; an array of constant values from 1 to 1,000,000,000 and dictates the tens place of what we are calculating.
;
; Postconditions: All EAX, EBX, ECX, EDX and ESI values are adjusted and restored.
;
; Receives:
;	[EBP+8]		= OFFSET address of lByteCount							
;	[EBP+12]	= OFFSET address of lInputString
;	[EBP+16]	= OFFSET address of ltempDec
;	[EBP+20]	= OFFSET address of lValidToggle
;	[EBP+24]	= OFFSET address of lNegToggle
;	[EBP+28]	= OFFSET tensArray
;
; returns: 
;	[EBP+12]	= Potential value for lTempDec
;	[EBP+20]	= updated toggle for invalid character
; ---------------------------------------------------------------------------------

CalculateDec PROC
	PUSH	EBP
	MOV		EBP, ESP
	PUSHAD

	; Points ESI to our InputString
	; Points EBX to our lByteLength (I/O)
	MOV		ESI, [EBP+12]
	MOV		EBX, [EBP+8]
	MOV		ECX, [EBX]

	_firstCharacter:
	; if first character is -, +, or 0 pass it. Otherwise, this is our first valid digit.
		MOV		EAX, 0
		LODSB	
		
		CMP		EAX, POSITIVE
		JE		_nextCharacter

		CMP		EAX, NEGATIVE
		JE		_nextCharacter

		CMP		EAX, ZERO	
		JE		_leadingZeroes	

		JMP		_checkCharacters

	_nextCharacter:
	; Decrement tempByteCount [ECX] and load next character.
		DEC		ECX
		MOV		EAX, 0
		LODSB

		CMP		EAX, ZERO
		JE		_leadingZeroes
		JMP		_checkCharacters

	_leadingZeroes:
	; Removes any leading zeroes.
		
		; If the only digit is a zero.
		MOV		EBX, 1						; ECX is byteCount
		CMP		EBX, ECX
		JE		_calculationPrep
		
		DEC		ECX
		MOV		EAX, 0
		LODSB

		CMP		EAX, ZERO
		JE		_leadingZeroes
		JMP		_checkCharacters

	_checkCharacters:
	; This our first non-zero character. Now we check the number of bytes and make sure we exist in the proper number of places.
		
		; The above value in EBX is how many digit places we're dealing with. 
		; Let's check if it's inside 10 digits (1-billions place)
		
		MOV		EDX, 10
		CMP		ECX, EDX

		JLE		_calculationPrep
		JG		_toggleInvalid

	_calculationPrep:
	; Prep for calculation

		;This is our final byteCount that we'll be references for arithmetic purposes. Store in byteCount
		MOV		EBX, [EBP+8]
		MOV		[EBX], ECX

		; Initalizes value of TempDec
		MOV		EDX, [EBP+16]
		MOV		EBX, 0
		MOV		[EDX], EBX

	_startCalculations:
	; starts calculations.

		; Stores our ESI and EAX from above. The address and value of our first non-zero character.
		PUSH	ESI
		PUSH	EAX
		
		; ESI is now going to point to TENSARRAY. We subtract our bytecount from EAX and multiply by type for the index of digits
		; place in TENSARRAY. (i.e. the 10's place is 10-2 = 8 * 4 = ESI + 32.) ECX is still our tempBytecount
		MOV		EAX, TENSARRAYL
		SUB		EAX, ECX
		MOV		EBX, TENSARRAYT
		IMUL	EBX

		MOV		ESI, [EBP+28]
		ADD		ESI, EAX

		; This value is our decimal place (the equivalence of 10 ^ n * digit)
		MOV		EBX, [ESI]
		
		; This is our current character value (as decimal). We subtract this value -- which has been validated --
		; by 48 (the first viable character that isn't - or +) and that is our digit (i.e. 48 - 48 = 0)
		; we then multiply this value by our EBX (5 * 10^1 = 50)
		POP		EAX
		SUB		EAX, 48
		IMUL	EBX

		; We're going to test here for overflow. If so, it's definitely invalid. Otherwise, test for neg.
		JO		_toggleInvalid

		MOV		EDX, [EBP+24]
		MOV		EBX, [EDX]
		CMP		EBX, TOGSTATE_1

		JE		_negative
		JNE		_positiveOrAdd

	_negative:
	; If negative, negate this value ('-' + '-' is still '-')
		NEG		EAX
		JMP		_positiveOrAdd

	_positiveOrAdd:
	; Adds our new value to [EBX].
		MOV		EBX, [EBP+16]
		ADD		[EBX], EAX

		; we are going to test again for overflow after this addition. Otherwise, chose next character.
		JO		_toggleInvalid

		POP		ESI
		MOV		EAX, 0
		LODSB

		LOOP	_startCalculations

		JMP		_exit

	_toggleInvalid:
	; If invalid, we toggle the lValidToggle
		POP		ESI
		
		MOV		EAX, [EBP+20]
		MOV		EBX, TOGSTATE_1
		MOV		[EAX], EBX	
		JMP		_exit

	_exit:
	; Exits.
		POPAD
		POP	EBP
		RET	24
CalculateDec ENDP

; ---------------------------------------------------------------------------------
; Name: printThings
;
; Prints all relevant information that has been calculated at this time for the user, including: all input decimal numbers
; their sum, and the truncated average of those numbers. Finally prints goodbye prompt at the end.
;
; Preconditions: avgValue, intSum and all inputs from inputArray are all 32-bit variables (DWORDS and SDWORDS.)
; printed statements are all null-terminated strings.
;
; Postconditions: EAX, ECX, EDX and ESI are moved and restored upon exit.
;
; Receives:
;	[EBP+8]		= avgValue						
;	[EBP+12]	= intSum
;	[EBP+16]	= OFFSET inputArray
;	[EBP+20]	= OFFSET printOut1
;	[EBP+24]	= OFFSET printOut2
;	[EBP+28]	= OFFSET printOut3
;	[EBP+32]	= OFFSET commaSpace
;	[EBP+36]	= OFFSET goodbye
;	[EBP+40]	= OFFSET tensArray
;	[EBP+44]	= OFFSET lowString
;
; returns: 
;	Series of printed statements using mReadString
; ---------------------------------------------------------------------------------

printThings PROC
	PUSH	EBP
	MOV		EBP, ESP
	PUSHAD

	; Prints the lead-up prompt to our printed numbers. 
	CALL	CrLf
	MOV		EDX, [EBP+20]
	mDisplayString EDX
	CALL	CrLf

	; Writes first value.
	MOV		ESI, [EBP+16]					; ESI moved to inputArray address
	MOV		EAX, [ESI]
	PUSH	[EBP+44]
	PUSH	[EBP+40]
	PUSH	EAX
	CALL	WriteVal

	MOV		ECX, INPUTARRL
	DEC		ECX

	_LoopOfPrintedVals:
	; Loops through inputArray LENGTHOF inputArray - 1 times for prints with commas between each print.
		MOV		EDX, [EBP+32]				; variable commaSpace offset here and printed.
		mDisplayString EDX
		ADD		ESI, INPUTARRT
		MOV		EAX, [ESI]
		PUSH	[EBP+44]
		PUSH	[EBP+40]
		PUSH	EAX
		CALL	WriteVal

		LOOP	_LoopOfPrintedVals
		CALL	CrLf

	_sumAndAverage:
	; Prints the sum and average of these numbers.
		MOV		EDX, [EBP+24]				; Loads printOut2
		mDisplayString EDX
		MOV		EAX, [EBP+12]				; Loads intSum
		PUSH	[EBP+44]
		PUSH	[EBP+40]
		PUSH	EAX
		CALL	WriteVal
		CALL	CrLf
		MOV		EDX, [EBP+28]				; Loads printOut3
		mDisplayString EDX
		MOV		EAX, [EBP+8]				; Loads avgValue
		PUSH	[EBP+44]
		PUSH	[EBP+40]
		PUSH	EAX
		CALL	WriteVal
		CALL	CrLf
		CALL	CrLf
		MOV		EDX, [EBP+36]				; Loads goodbye
		mDisplayString EDX
		CALL	CrLf
		CALL	CrLf
		
	POPAD
	POP		EBP
	RET 40
printThings ENDP

END main
