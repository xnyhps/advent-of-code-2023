.global _main

.align 4

.text

_main:
	SUB SP, SP, 0x10

	# X21: running total
	MOV X21, #0

	ADR X0, path
	ADR X1, mode

	BL _fopen

	CBZ X0, _bail

	# X25: FILE pointer
	MOV X25, X0

_readloop:
	MOV X3, X25
	MOV X2, #1
	MOV X1, #1
	ADD X0, SP, #8
	BL _fread

	CBZ X0, _done

	LDRB W24, [SP, #8]

	# X24: current character
	MOV X0, X24
	BL _isdigit

	CBZ X0, _readloop

	# X23 first digit
	SUB X23, X24, #0x30
	MOV X22, X23

_readloop2:
	MOV X3, X25
	MOV X2, #1
	MOV X1, #1
	ADD X0, SP, #8
	BL _fread

	MOV X20, X0

	CBZ X0, _end

	LDRB W24, [SP, #8]

	MOV X0, X24
	BL _isdigit

	CBZ X0, _notdigit

	# X22 last digit
	SUB X22, X24, #0x30

_notdigit:
	CMP X24, '\n'
	BNE _readloop2

_end:
	STR X23, [SP]
	STR X22, [SP, #8]
	ADR X0, digits
	BL _printf

	MOV X0, #10
	MUL X23, X23, X0

	ADD X21, X21, X23
	ADD X21, X21, X22

	CBNZ X20, _readloop

_done:
	STR X21, [SP]
	ADR X0, result
	BL _printf

	MOV X0, #0

	BL _exit

_bail:
	STP x29, x30, [SP, -0x10]!
	SUB SP, SP, 0x10

	STR X0, [SP]
	ADR X0, err
	BL _printf

	BL _abort

path: .asciz "1.input"
mode: .asciz "r"
err: .asciz "Error\n"
number: .asciz "%d\n"
digits: .asciz "Digits: %d %d\n"
result: .asciz "Result: %d\n"