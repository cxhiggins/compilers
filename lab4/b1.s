@ picoPascal compiler output
	.include "fixup.s"
	.global pmain

@ proc StringLength(var s: tempstring): integer;
@ Initial code:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@   while s[i] <> ENDSTR do i := i+1 end;
@ <LABEL L147>
@ <JNEQ L148,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>,
@   <CONST 0>>
@ <JUMP L149>
@ <LABEL L148>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L147>
@ <LABEL L149>
@   return i
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L146>
@ <LABEL L146>

@ After simplification:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L147>
@   while s[i] <> ENDSTR do i := i+1 end;
@ <JEQ L149,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <CONST 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L147>
@ <LABEL L149>
@   return i
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L147>
@   while s[i] <> ENDSTR do i := i+1 end;
@ <JEQ L149,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <CONST 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L147>
@ <LABEL L149>
@   return i
@ <RESULTW, <LOADW, <REGVAR 0>>>

	.text
_StringLength:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <LABEL L147>
.L147:
@   while s[i] <> ENDSTR do i := i+1 end;
@ <JEQ L149,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <CONST 0>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r4
	ldrb r0, [r0]
	cmp r0, #0
	beq .L149
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L147>
	b .L147
@ <LABEL L149>
.L149:
@   return i
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc SaveString(var s: tempstring): permstring;
@ Initial code:
@   if charptr + StringLength(s) + 1 > MAXCHARS then
@ <JGT L151,
@   <PLUS,
@     <PLUS,
@       <LOADW, <GLOBAL _charptr>>,
@       <CALL 1,
@         <GLOBAL _StringLength>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>>,
@     <CONST 1>>,
@   <CONST 2048>>
@ <JUMP L152>
@ <LABEL L151>
@     newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g1>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g2>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L153>
@ <LABEL L152>
@ <LABEL L153>
@   p := charptr; i := 0;
@ <STOREW, <LOADW, <GLOBAL _charptr>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@   repeat
@ <LABEL L154>
@     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
@ <STOREC,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>,
@   <OFFSET,
@     <GLOBAL _charbuf>,
@     <TIMES, <LOADW, <GLOBAL _charptr>>, <CONST 1>>>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _charptr>>, <CONST 1>>,
@   <GLOBAL _charptr>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JEQ L155,
@   <LOADC,
@     <OFFSET,
@       <GLOBAL _charbuf>,
@       <TIMES,
@         <MINUS, <LOADW, <GLOBAL _charptr>>, <CONST 1>>,
@         <CONST 1>>>>,
@   <CONST 0>>
@ <JUMP L154>
@ <LABEL L155>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L150>
@ <LABEL L150>

@ After simplification:
@   if charptr + StringLength(s) + 1 > MAXCHARS then
@ <JLEQ L153,
@   <PLUS,
@     <PLUS,
@       <LOADW, <GLOBAL _charptr>>,
@       <CALL 1, <GLOBAL _StringLength>, <ARG 0, <LOADW, <LOCAL 40>>>>>,
@     <CONST 1>>,
@   <CONST 2048>>
@     newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g1>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g2>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L153>
@   p := charptr; i := 0;
@ <STOREW, <LOADW, <GLOBAL _charptr>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <LABEL L154>
@     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>,
@   <OFFSET, <GLOBAL _charbuf>, <LOADW, <GLOBAL _charptr>>>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _charptr>>, <CONST 1>>,
@   <GLOBAL _charptr>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JNEQ L154,
@   <LOADC,
@     <OFFSET,
@       <OFFSET, <GLOBAL _charbuf>, <LOADW, <GLOBAL _charptr>>>,
@       <CONST -1>>>,
@   <CONST 0>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   if charptr + StringLength(s) + 1 > MAXCHARS then
@ <DEFTEMP 1,
@   <CALL 1, <GLOBAL _StringLength>, <ARG 0, <LOADW, <LOCAL 40>>>>>
@ <JLEQ L153,
@   <PLUS, <PLUS, <LOADW, <GLOBAL _charptr>>, <TEMP 1>>, <CONST 1>>,
@   <CONST 2048>>
@     newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g1>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g2>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L153>
@   p := charptr; i := 0;
@ <STOREW, <LOADW, <GLOBAL _charptr>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <LABEL L154>
@     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
@ <DEFTEMP 2, <GLOBAL _charbuf>>
@ <DEFTEMP 3, <GLOBAL _charptr>>
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>,
@   <OFFSET, <TEMP 2>, <LOADW, <TEMP 3>>>>
@ <DEFTEMP 4, <PLUS, <LOADW, <TEMP 3>>, <CONST 1>>>
@ <STOREW, <TEMP 4>, <TEMP 3>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JNEQ L154,
@   <LOADC, <OFFSET, <OFFSET, <TEMP 2>, <TEMP 4>>, <CONST -1>>>,
@   <CONST 0>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

_SaveString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if charptr + StringLength(s) + 1 > MAXCHARS then
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 1, <GLOBAL _StringLength>>>
	bl _StringLength
@ <JLEQ L153,
@   <PLUS, <PLUS, <LOADW, <GLOBAL _charptr>>, <TEMP 1>>, <CONST 1>>,
@   <CONST 2048>>
	set r1, _charptr
	ldr r1, [r1]
	add r0, r1, r0
	add r0, r0, #1
	cmp r0, #2048
	ble .L153
@     newline(); print_string("Panic: "); print_string("out of string space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g1>>
	set r0, g1
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 19>>
	set r1, #19
@ <ARG 0, <GLOBAL g2>>
	set r0, g2
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L153>
.L153:
@   p := charptr; i := 0;
@ <STOREW, <LOADW, <GLOBAL _charptr>>, <REGVAR 0>>
	set r0, _charptr
	ldr r4, [r0]
@ <STOREW, <CONST 0>, <REGVAR 1>>
	set r5, #0
@ <LABEL L154>
.L154:
@     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
@ <DEFTEMP 2, <GLOBAL _charbuf>>
	set r6, _charbuf
@ <DEFTEMP 3, <GLOBAL _charptr>>
	set r7, _charptr
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>,
@   <OFFSET, <TEMP 2>, <LOADW, <TEMP 3>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r5
	ldrb r0, [r0]
	ldr r1, [r7]
	add r1, r6, r1
	strb r0, [r1]
@ <DEFTEMP 4, <PLUS, <LOADW, <TEMP 3>>, <CONST 1>>>
	ldr r0, [r7]
	add r8, r0, #1
@ <STOREW, <TEMP 4>, <TEMP 3>>
	str r8, [r7]
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JNEQ L154,
@   <LOADC, <OFFSET, <OFFSET, <TEMP 2>, <TEMP 4>>, <CONST -1>>>,
@   <CONST 0>>
	add r0, r6, r8
	set r1, #-1
	add r0, r0, r1
	ldrb r0, [r0]
	cmp r0, #0
	bne .L154
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc StringEqual(var s1: tempstring; s2: permstring): boolean;
@ Initial code:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@   while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
@ <LABEL L157>
@ <JNEQ L160,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>,
@   <CONST 0>>
@ <JUMP L159>
@ <LABEL L160>
@ <JEQ L158,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>,
@   <LOADC,
@     <OFFSET,
@       <GLOBAL _charbuf>,
@       <TIMES,
@         <PLUS,
@           <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@           <LOADW, <REGVAR 0>>>,
@         <CONST 1>>>>>
@ <JUMP L159>
@ <LABEL L158>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L157>
@ <LABEL L159>
@   return (s1[i] = charbuf[s2+i])
@ <RESULTW,
@   <EQ,
@     <LOADC,
@       <OFFSET,
@         <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>,
@     <LOADC,
@       <OFFSET,
@         <GLOBAL _charbuf>,
@         <TIMES,
@           <PLUS,
@             <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@             <LOADW, <REGVAR 0>>>,
@           <CONST 1>>>>>>
@ <JUMP L156>
@ <LABEL L156>

@ After simplification:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L157>
@   while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
@ <JEQ L159,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <CONST 0>>
@ <JNEQ L159,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <LOADC,
@     <OFFSET,
@       <OFFSET, <GLOBAL _charbuf>, <LOADW, <LOCAL 44>>>,
@       <LOADW, <REGVAR 0>>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L157>
@ <LABEL L159>
@   return (s1[i] = charbuf[s2+i])
@ <RESULTW,
@   <EQ,
@     <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@     <LOADC,
@       <OFFSET,
@         <OFFSET, <GLOBAL _charbuf>, <LOADW, <LOCAL 44>>>,
@         <LOADW, <REGVAR 0>>>>>>

@ After sharing:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L157>
@   while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
@ <DEFTEMP 1,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>>
@ <JEQ L159, <TEMP 1>, <CONST 0>>
@ <JNEQ L159,
@   <TEMP 1>,
@   <LOADC,
@     <OFFSET,
@       <OFFSET, <GLOBAL _charbuf>, <LOADW, <LOCAL 44>>>,
@       <LOADW, <REGVAR 0>>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L157>
@ <LABEL L159>
@   return (s1[i] = charbuf[s2+i])
@ <RESULTW,
@   <EQ,
@     <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@     <LOADC,
@       <OFFSET,
@         <OFFSET, <GLOBAL _charbuf>, <LOADW, <LOCAL 44>>>,
@         <LOADW, <REGVAR 0>>>>>>

_StringEqual:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <LABEL L157>
.L157:
@   while (s1[i] <> ENDSTR) and (s1[i] = charbuf[s2+i]) do i := i+1 end;
@ <DEFTEMP 1,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r4
	ldrb r5, [r0]
@ <JEQ L159, <TEMP 1>, <CONST 0>>
	cmp r5, #0
	beq .L159
@ <JNEQ L159,
@   <TEMP 1>,
@   <LOADC,
@     <OFFSET,
@       <OFFSET, <GLOBAL _charbuf>, <LOADW, <LOCAL 44>>>,
@       <LOADW, <REGVAR 0>>>>>
	set r0, _charbuf
	set ip, #44
	add r1, fp, ip
	ldr r1, [r1]
	add r0, r0, r1
	add r0, r0, r4
	ldrb r0, [r0]
	cmp r5, r0
	bne .L159
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L157>
	b .L157
@ <LABEL L159>
.L159:
@   return (s1[i] = charbuf[s2+i])
@ <RESULTW,
@   <EQ,
@     <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@     <LOADC,
@       <OFFSET,
@         <OFFSET, <GLOBAL _charbuf>, <LOADW, <LOCAL 44>>>,
@         <LOADW, <REGVAR 0>>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r4
	ldrb r0, [r0]
	set r1, _charbuf
	set ip, #44
	add r2, fp, ip
	ldr r2, [r2]
	add r1, r1, r2
	add r1, r1, r4
	ldrb r1, [r1]
	cmp r0, r1
	mov r0, #0
	moveq r0, #1
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc WriteString(s: permstring);
@ Initial code:
@   i := s;
@ <STOREW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <REGVAR 0>>
@   while charbuf[i] <> ENDSTR do
@ <LABEL L162>
@ <JNEQ L163,
@   <LOADC,
@     <OFFSET,
@       <GLOBAL _charbuf>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>,
@   <CONST 0>>
@ <JUMP L164>
@ <LABEL L163>
@     print_char(charbuf[i]); i := i+1
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADC,
@       <OFFSET,
@         <GLOBAL _charbuf>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L162>
@ <LABEL L164>
@ <LABEL L161>

@ After simplification:
@   i := s;
@ <STOREW, <LOADW, <LOCAL 40>>, <REGVAR 0>>
@ <LABEL L162>
@   while charbuf[i] <> ENDSTR do
@ <JEQ L161,
@   <LOADC, <OFFSET, <GLOBAL _charbuf>, <LOADW, <REGVAR 0>>>>,
@   <CONST 0>>
@     print_char(charbuf[i]); i := i+1
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0, <LOADC, <OFFSET, <GLOBAL _charbuf>, <LOADW, <REGVAR 0>>>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L162>
@ <LABEL L161>

@ After sharing:
@   i := s;
@ <STOREW, <LOADW, <LOCAL 40>>, <REGVAR 0>>
@ <LABEL L162>
@   while charbuf[i] <> ENDSTR do
@ <DEFTEMP 1, <LOADC, <OFFSET, <GLOBAL _charbuf>, <LOADW, <REGVAR 0>>>>>
@ <JEQ L161, <TEMP 1>, <CONST 0>>
@     print_char(charbuf[i]); i := i+1
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <TEMP 1>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L162>
@ <LABEL L161>

_WriteString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := s;
@ <STOREW, <LOADW, <LOCAL 40>>, <REGVAR 0>>
	set ip, #40
	add r0, fp, ip
	ldr r4, [r0]
@ <LABEL L162>
.L162:
@   while charbuf[i] <> ENDSTR do
@ <DEFTEMP 1, <LOADC, <OFFSET, <GLOBAL _charbuf>, <LOADW, <REGVAR 0>>>>>
	set r0, _charbuf
	add r0, r0, r4
	ldrb r5, [r0]
@ <JEQ L161, <TEMP 1>, <CONST 0>>
	cmp r5, #0
	beq .L161
@     print_char(charbuf[i]); i := i+1
@ <ARG 0, <TEMP 1>>
	mov r0, r5
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L162>
	b .L162
@ <LABEL L161>
.L161:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc LocAlloc(size: integer): ptr;
@ Initial code:
@   if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
@ <JGEQ L166,
@   <PLUS,
@     <LOADW, <GLOBAL _lsp>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <LOADW, <GLOBAL _gsp>>>
@ <JUMP L167>
@ <LABEL L166>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g3>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g4>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L168>
@ <LABEL L167>
@ <LABEL L168>
@   p := lsp + 1; lsp := lsp + size; return p
@ <STOREW, <PLUS, <LOADW, <GLOBAL _lsp>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <PLUS,
@     <LOADW, <GLOBAL _lsp>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <GLOBAL _lsp>>
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L165>
@ <LABEL L165>

@ After simplification:
@   if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
@ <JLT L168,
@   <PLUS, <LOADW, <GLOBAL _lsp>>, <LOADW, <LOCAL 40>>>,
@   <LOADW, <GLOBAL _gsp>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g3>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g4>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L168>
@   p := lsp + 1; lsp := lsp + size; return p
@ <STOREW, <PLUS, <LOADW, <GLOBAL _lsp>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _lsp>>, <LOADW, <LOCAL 40>>>,
@   <GLOBAL _lsp>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
@ <JLT L168,
@   <PLUS, <LOADW, <GLOBAL _lsp>>, <LOADW, <LOCAL 40>>>,
@   <LOADW, <GLOBAL _gsp>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g3>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g4>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L168>
@   p := lsp + 1; lsp := lsp + size; return p
@ <DEFTEMP 1, <GLOBAL _lsp>>
@ <DEFTEMP 2, <LOADW, <TEMP 1>>>
@ <STOREW, <PLUS, <TEMP 2>, <CONST 1>>, <REGVAR 0>>
@ <STOREW, <PLUS, <TEMP 2>, <LOADW, <LOCAL 40>>>, <TEMP 1>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

_LocAlloc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if lsp + size >= gsp then newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2) end;
@ <JLT L168,
@   <PLUS, <LOADW, <GLOBAL _lsp>>, <LOADW, <LOCAL 40>>>,
@   <LOADW, <GLOBAL _gsp>>>
	set r0, _lsp
	ldr r0, [r0]
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	add r0, r0, r1
	set r1, _gsp
	ldr r1, [r1]
	cmp r0, r1
	blt .L168
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g3>>
	set r0, g3
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 18>>
	set r1, #18
@ <ARG 0, <GLOBAL g4>>
	set r0, g4
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L168>
.L168:
@   p := lsp + 1; lsp := lsp + size; return p
@ <DEFTEMP 1, <GLOBAL _lsp>>
	set r5, _lsp
@ <DEFTEMP 2, <LOADW, <TEMP 1>>>
	ldr r6, [r5]
@ <STOREW, <PLUS, <TEMP 2>, <CONST 1>>, <REGVAR 0>>
	add r4, r6, #1
@ <STOREW, <PLUS, <TEMP 2>, <LOADW, <LOCAL 40>>>, <TEMP 1>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r6, r0
	str r0, [r5]
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc GloAlloc(kind, size: integer): ptr;
@ Initial code:
@   if gsp - size <= lsp then
@ <JLEQ L170,
@   <MINUS,
@     <LOADW, <GLOBAL _gsp>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <LOADW, <GLOBAL _lsp>>>
@ <JUMP L171>
@ <LABEL L170>
@     newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g5>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g6>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L172>
@ <LABEL L171>
@ <LABEL L172>
@   gsp := gsp - size; p := gsp;
@ <STOREW,
@   <MINUS,
@     <LOADW, <GLOBAL _gsp>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <GLOBAL _gsp>>
@ <STOREW, <LOADW, <GLOBAL _gsp>>, <REGVAR 0>>
@   mem[p] := lsl(kind, 8) + size;
@ <STOREW,
@   <PLUS,
@     <LSL, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 8>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L169>
@ <LABEL L169>

@ After simplification:
@   if gsp - size <= lsp then
@ <JGT L172,
@   <MINUS, <LOADW, <GLOBAL _gsp>>, <LOADW, <LOCAL 44>>>,
@   <LOADW, <GLOBAL _lsp>>>
@     newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g5>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g6>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L172>
@   gsp := gsp - size; p := gsp;
@ <STOREW,
@   <MINUS, <LOADW, <GLOBAL _gsp>>, <LOADW, <LOCAL 44>>>,
@   <GLOBAL _gsp>>
@ <STOREW, <LOADW, <GLOBAL _gsp>>, <REGVAR 0>>
@   mem[p] := lsl(kind, 8) + size;
@ <STOREW,
@   <PLUS, <LSL, <LOADW, <LOCAL 40>>, <CONST 8>>, <LOADW, <LOCAL 44>>>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   if gsp - size <= lsp then
@ <JGT L172,
@   <MINUS, <LOADW, <GLOBAL _gsp>>, <LOADW, <LOCAL 44>>>,
@   <LOADW, <GLOBAL _lsp>>>
@     newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g5>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g6>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L172>
@   gsp := gsp - size; p := gsp;
@ <DEFTEMP 1, <GLOBAL _gsp>>
@ <DEFTEMP 2, <LOADW, <LOCAL 44>>>
@ <DEFTEMP 3, <MINUS, <LOADW, <TEMP 1>>, <TEMP 2>>>
@ <STOREW, <TEMP 3>, <TEMP 1>>
@ <STOREW, <TEMP 3>, <REGVAR 0>>
@   mem[p] := lsl(kind, 8) + size;
@ <STOREW,
@   <PLUS, <LSL, <LOADW, <LOCAL 40>>, <CONST 8>>, <TEMP 2>>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

_GloAlloc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if gsp - size <= lsp then
@ <JGT L172,
@   <MINUS, <LOADW, <GLOBAL _gsp>>, <LOADW, <LOCAL 44>>>,
@   <LOADW, <GLOBAL _lsp>>>
	set r0, _gsp
	ldr r0, [r0]
	set ip, #44
	add r1, fp, ip
	ldr r1, [r1]
	sub r0, r0, r1
	set r1, _lsp
	ldr r1, [r1]
	cmp r0, r1
	bgt .L172
@     newline(); print_string("Panic: "); print_string("out of stack space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g5>>
	set r0, g5
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 18>>
	set r1, #18
@ <ARG 0, <GLOBAL g6>>
	set r0, g6
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L172>
.L172:
@   gsp := gsp - size; p := gsp;
@ <DEFTEMP 1, <GLOBAL _gsp>>
	set r5, _gsp
@ <DEFTEMP 2, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r6, [r0]
@ <DEFTEMP 3, <MINUS, <LOADW, <TEMP 1>>, <TEMP 2>>>
	ldr r0, [r5]
	sub r7, r0, r6
@ <STOREW, <TEMP 3>, <TEMP 1>>
	str r7, [r5]
@ <STOREW, <TEMP 3>, <REGVAR 0>>
	mov r4, r7
@   mem[p] := lsl(kind, 8) + size;
@ <STOREW,
@   <PLUS, <LSL, <LOADW, <LOCAL 40>>, <CONST 8>>, <TEMP 2>>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #8
	add r0, r0, r6
	set r1, _mem
	lsl r2, r4, #2
	add r1, r1, r2
	str r0, [r1]
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc HeapAlloc(size: integer): ptr;
@ Initial code:
@   if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
@ <JGT L174,
@   <PLUS,
@     <LOADW, <GLOBAL _hp>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <CONST 25000>>
@ <JUMP L175>
@ <LABEL L174>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g7>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g8>>,
@   <ARG 1, <CONST 17>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L176>
@ <LABEL L175>
@ <LABEL L176>
@   p := hp + 1; hp := hp + size; return p
@ <STOREW, <PLUS, <LOADW, <GLOBAL _hp>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <PLUS,
@     <LOADW, <GLOBAL _hp>>,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <GLOBAL _hp>>
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L173>
@ <LABEL L173>

@ After simplification:
@   if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
@ <JLEQ L176,
@   <PLUS, <LOADW, <GLOBAL _hp>>, <LOADW, <LOCAL 40>>>,
@   <CONST 25000>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g7>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g8>>,
@   <ARG 1, <CONST 17>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L176>
@   p := hp + 1; hp := hp + size; return p
@ <STOREW, <PLUS, <LOADW, <GLOBAL _hp>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _hp>>, <LOADW, <LOCAL 40>>>,
@   <GLOBAL _hp>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
@ <JLEQ L176,
@   <PLUS, <LOADW, <GLOBAL _hp>>, <LOADW, <LOCAL 40>>>,
@   <CONST 25000>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g7>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g8>>,
@   <ARG 1, <CONST 17>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L176>
@   p := hp + 1; hp := hp + size; return p
@ <DEFTEMP 1, <GLOBAL _hp>>
@ <DEFTEMP 2, <LOADW, <TEMP 1>>>
@ <STOREW, <PLUS, <TEMP 2>, <CONST 1>>, <REGVAR 0>>
@ <STOREW, <PLUS, <TEMP 2>, <LOADW, <LOCAL 40>>>, <TEMP 1>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

_HeapAlloc:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if hp + size > MEMSIZE then newline(); print_string("Panic: "); print_string("out of heap space"); newline(); exit(2) end;
@ <JLEQ L176,
@   <PLUS, <LOADW, <GLOBAL _hp>>, <LOADW, <LOCAL 40>>>,
@   <CONST 25000>>
	set r0, _hp
	ldr r0, [r0]
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	add r0, r0, r1
	set r1, #25000
	cmp r0, r1
	ble .L176
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g7>>
	set r0, g7
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 17>>
	set r1, #17
@ <ARG 0, <GLOBAL g8>>
	set r0, g8
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L176>
.L176:
@   p := hp + 1; hp := hp + size; return p
@ <DEFTEMP 1, <GLOBAL _hp>>
	set r5, _hp
@ <DEFTEMP 2, <LOADW, <TEMP 1>>>
	ldr r6, [r5]
@ <STOREW, <PLUS, <TEMP 2>, <CONST 1>>, <REGVAR 0>>
	add r4, r6, #1
@ <STOREW, <PLUS, <TEMP 2>, <LOADW, <LOCAL 40>>>, <TEMP 1>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r6, r0
	str r0, [r5]
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc prog(line: array 60 of char);
@ Initial code:
@   for i := 0 to 59 do
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW, <CONST 59>, <REGVAR 1>>
@ <LABEL L178>
@ <JGT L179, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
@     infile[pin] := line[i]; pin := pin+1
@ <STOREC,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 1>>>>,
@   <OFFSET,
@     <GLOBAL _infile>,
@     <TIMES, <LOADW, <GLOBAL _pin>>, <CONST 1>>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _pin>>, <CONST 1>>, <GLOBAL _pin>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L178>
@ <LABEL L179>
@   infile[pin] := ENDLINE; pin := pin+1
@ <STOREC,
@   <CONST 10>,
@   <OFFSET,
@     <GLOBAL _infile>,
@     <TIMES, <LOADW, <GLOBAL _pin>>, <CONST 1>>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _pin>>, <CONST 1>>, <GLOBAL _pin>>
@ <LABEL L177>

@ After simplification:
@   for i := 0 to 59 do
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW, <CONST 59>, <REGVAR 1>>
@ <LABEL L178>
@ <JGT L179, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
@     infile[pin] := line[i]; pin := pin+1
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <OFFSET, <GLOBAL _infile>, <LOADW, <GLOBAL _pin>>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _pin>>, <CONST 1>>, <GLOBAL _pin>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L178>
@ <LABEL L179>
@   infile[pin] := ENDLINE; pin := pin+1
@ <STOREC,
@   <CONST 10>,
@   <OFFSET, <GLOBAL _infile>, <LOADW, <GLOBAL _pin>>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _pin>>, <CONST 1>>, <GLOBAL _pin>>

@ After sharing:
@   for i := 0 to 59 do
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW, <CONST 59>, <REGVAR 1>>
@ <LABEL L178>
@ <JGT L179, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
@     infile[pin] := line[i]; pin := pin+1
@ <DEFTEMP 1, <GLOBAL _pin>>
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <OFFSET, <GLOBAL _infile>, <LOADW, <TEMP 1>>>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L178>
@ <LABEL L179>
@   infile[pin] := ENDLINE; pin := pin+1
@ <DEFTEMP 2, <GLOBAL _pin>>
@ <STOREC, <CONST 10>, <OFFSET, <GLOBAL _infile>, <LOADW, <TEMP 2>>>>
@ <STOREW, <PLUS, <LOADW, <TEMP 2>>, <CONST 1>>, <TEMP 2>>

_prog:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   for i := 0 to 59 do
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <STOREW, <CONST 59>, <REGVAR 1>>
	set r5, #59
@ <LABEL L178>
.L178:
@ <JGT L179, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
	cmp r4, r5
	bgt .L179
@     infile[pin] := line[i]; pin := pin+1
@ <DEFTEMP 1, <GLOBAL _pin>>
	set r6, _pin
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>>,
@   <OFFSET, <GLOBAL _infile>, <LOADW, <TEMP 1>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r4
	ldrb r0, [r0]
	set r1, _infile
	ldr r2, [r6]
	add r1, r1, r2
	strb r0, [r1]
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L178>
	b .L178
@ <LABEL L179>
.L179:
@   infile[pin] := ENDLINE; pin := pin+1
@ <DEFTEMP 2, <GLOBAL _pin>>
	set r6, _pin
@ <STOREC, <CONST 10>, <OFFSET, <GLOBAL _infile>, <LOADW, <TEMP 2>>>>
	set r0, #10
	set r1, _infile
	ldr r2, [r6]
	add r1, r1, r2
	strb r0, [r1]
@ <STOREW, <PLUS, <LOADW, <TEMP 2>>, <CONST 1>>, <TEMP 2>>
	ldr r0, [r6]
	add r0, r0, #1
	str r0, [r6]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc rdchar(var ch: char);
@ Initial code:
@   if pout >= pin then
@ <JGEQ L181, <LOADW, <GLOBAL _pout>>, <LOADW, <GLOBAL _pin>>>
@ <JUMP L182>
@ <LABEL L181>
@     ch := ENDFILE
@ <STOREC, <CONST 127>, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <JUMP L183>
@ <LABEL L182>
@     ch := infile[pout]; pout := pout+1
@ <STOREC,
@   <LOADC,
@     <OFFSET,
@       <GLOBAL _infile>,
@       <TIMES, <LOADW, <GLOBAL _pout>>, <CONST 1>>>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _pout>>, <CONST 1>>, <GLOBAL _pout>>
@ <LABEL L183>
@ <LABEL L180>

@ After simplification:
@   if pout >= pin then
@ <JLT L182, <LOADW, <GLOBAL _pout>>, <LOADW, <GLOBAL _pin>>>
@     ch := ENDFILE
@ <STOREC, <CONST 127>, <LOADW, <LOCAL 40>>>
@ <JUMP L180>
@ <LABEL L182>
@     ch := infile[pout]; pout := pout+1
@ <STOREC,
@   <LOADC, <OFFSET, <GLOBAL _infile>, <LOADW, <GLOBAL _pout>>>>,
@   <LOADW, <LOCAL 40>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _pout>>, <CONST 1>>, <GLOBAL _pout>>
@ <LABEL L180>

@ After sharing:
@   if pout >= pin then
@ <JLT L182, <LOADW, <GLOBAL _pout>>, <LOADW, <GLOBAL _pin>>>
@     ch := ENDFILE
@ <STOREC, <CONST 127>, <LOADW, <LOCAL 40>>>
@ <JUMP L180>
@ <LABEL L182>
@     ch := infile[pout]; pout := pout+1
@ <DEFTEMP 1, <GLOBAL _pout>>
@ <STOREC,
@   <LOADC, <OFFSET, <GLOBAL _infile>, <LOADW, <TEMP 1>>>>,
@   <LOADW, <LOCAL 40>>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@ <LABEL L180>

_rdchar:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if pout >= pin then
@ <JLT L182, <LOADW, <GLOBAL _pout>>, <LOADW, <GLOBAL _pin>>>
	set r0, _pout
	ldr r0, [r0]
	set r1, _pin
	ldr r1, [r1]
	cmp r0, r1
	blt .L182
@     ch := ENDFILE
@ <STOREC, <CONST 127>, <LOADW, <LOCAL 40>>>
	set r0, #127
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	strb r0, [r1]
@ <JUMP L180>
	b .L180
@ <LABEL L182>
.L182:
@     ch := infile[pout]; pout := pout+1
@ <DEFTEMP 1, <GLOBAL _pout>>
	set r4, _pout
@ <STOREC,
@   <LOADC, <OFFSET, <GLOBAL _infile>, <LOADW, <TEMP 1>>>>,
@   <LOADW, <LOCAL 40>>>
	set r0, _infile
	ldr r1, [r4]
	add r0, r0, r1
	ldrb r0, [r0]
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	strb r0, [r1]
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@ <LABEL L180>
.L180:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc GetChar(): char;
@ Initial code:
@   if pbchar <> ENDFILE then
@ <JNEQ L185, <LOADC, <GLOBAL _pbchar>>, <CONST 127>>
@ <JUMP L186>
@ <LABEL L185>
@     ch := pbchar; pbchar := ENDFILE
@ <STOREC, <LOADC, <GLOBAL _pbchar>>, <OFFSET, <LOCAL 0>, <CONST -1>>>
@ <STOREC, <CONST 127>, <GLOBAL _pbchar>>
@ <JUMP L187>
@ <LABEL L186>
@     rdchar(ch);
@ <CALL 1,
@   <GLOBAL _rdchar>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <OFFSET, <LOCAL 0>, <CONST -1>>>>
@     if ch = ENDLINE then lineno := lineno+1 end
@ <JEQ L188, <LOADC, <OFFSET, <LOCAL 0>, <CONST -1>>>, <CONST 10>>
@ <JUMP L189>
@ <LABEL L188>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _lineno>>, <CONST 1>>,
@   <GLOBAL _lineno>>
@ <JUMP L190>
@ <LABEL L189>
@ <LABEL L190>
@ <LABEL L187>
@   return ch
@ <RESULTW, <LOADC, <OFFSET, <LOCAL 0>, <CONST -1>>>>
@ <JUMP L184>
@ <LABEL L184>

@ After simplification:
@   if pbchar <> ENDFILE then
@ <JEQ L186, <LOADC, <GLOBAL _pbchar>>, <CONST 127>>
@     ch := pbchar; pbchar := ENDFILE
@ <STOREC, <LOADC, <GLOBAL _pbchar>>, <LOCAL -1>>
@ <STOREC, <CONST 127>, <GLOBAL _pbchar>>
@ <JUMP L187>
@ <LABEL L186>
@     rdchar(ch);
@ <CALL 1, <GLOBAL _rdchar>, <ARG 0, <LOCAL -1>>>
@     if ch = ENDLINE then lineno := lineno+1 end
@ <JNEQ L187, <LOADC, <LOCAL -1>>, <CONST 10>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _lineno>>, <CONST 1>>,
@   <GLOBAL _lineno>>
@ <LABEL L187>
@   return ch
@ <RESULTW, <LOADC, <LOCAL -1>>>

@ After sharing:
@   if pbchar <> ENDFILE then
@ <DEFTEMP 1, <GLOBAL _pbchar>>
@ <DEFTEMP 2, <LOADC, <TEMP 1>>>
@ <JEQ L186, <TEMP 2>, <CONST 127>>
@     ch := pbchar; pbchar := ENDFILE
@ <STOREC, <TEMP 2>, <LOCAL -1>>
@ <STOREC, <CONST 127>, <TEMP 1>>
@ <JUMP L187>
@ <LABEL L186>
@     rdchar(ch);
@ <CALL 1, <GLOBAL _rdchar>, <ARG 0, <LOCAL -1>>>
@     if ch = ENDLINE then lineno := lineno+1 end
@ <JNEQ L187, <LOADC, <LOCAL -1>>, <CONST 10>>
@ <DEFTEMP 3, <GLOBAL _lineno>>
@ <STOREW, <PLUS, <LOADW, <TEMP 3>>, <CONST 1>>, <TEMP 3>>
@ <LABEL L187>
@   return ch
@ <RESULTW, <LOADC, <LOCAL -1>>>

_GetChar:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   if pbchar <> ENDFILE then
@ <DEFTEMP 1, <GLOBAL _pbchar>>
	set r4, _pbchar
@ <DEFTEMP 2, <LOADC, <TEMP 1>>>
	ldrb r5, [r4]
@ <JEQ L186, <TEMP 2>, <CONST 127>>
	cmp r5, #127
	beq .L186
@     ch := pbchar; pbchar := ENDFILE
@ <STOREC, <TEMP 2>, <LOCAL -1>>
	set ip, #-1
	add r0, fp, ip
	strb r5, [r0]
@ <STOREC, <CONST 127>, <TEMP 1>>
	set r0, #127
	strb r0, [r4]
@ <JUMP L187>
	b .L187
@ <LABEL L186>
.L186:
@     rdchar(ch);
@ <ARG 0, <LOCAL -1>>
	set ip, #-1
	add r0, fp, ip
@ <CALL 1, <GLOBAL _rdchar>>
	bl _rdchar
@     if ch = ENDLINE then lineno := lineno+1 end
@ <JNEQ L187, <LOADC, <LOCAL -1>>, <CONST 10>>
	set ip, #-1
	add r0, fp, ip
	ldrb r0, [r0]
	cmp r0, #10
	bne .L187
@ <DEFTEMP 3, <GLOBAL _lineno>>
	set r4, _lineno
@ <STOREW, <PLUS, <LOADW, <TEMP 3>>, <CONST 1>>, <TEMP 3>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@ <LABEL L187>
.L187:
@   return ch
@ <RESULTW, <LOADC, <LOCAL -1>>>
	set ip, #-1
	add r0, fp, ip
	ldrb r0, [r0]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PushBack(ch: char);
@ Initial code:
@   pbchar := ch
@ <STOREC, <LOADC, <OFFSET, <LOCAL 0>, <CONST 40>>>, <GLOBAL _pbchar>>
@ <LABEL L191>

@ After simplification:
@   pbchar := ch
@ <STOREC, <LOADC, <LOCAL 40>>, <GLOBAL _pbchar>>

@ After sharing:
@   pbchar := ch
@ <STOREC, <LOADC, <LOCAL 40>>, <GLOBAL _pbchar>>

_PushBack:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   pbchar := ch
@ <STOREC, <LOADC, <LOCAL 40>>, <GLOBAL _pbchar>>
	set ip, #40
	add r0, fp, ip
	ldrb r0, [r0]
	set r1, _pbchar
	strb r0, [r1]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Deref(t: term; e: frame): term;
@ Initial code:
@   if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
@ <JEQ L193, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 0>>
@ <JUMP L194>
@ <LABEL L193>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g9>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g10>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L195>
@ <LABEL L194>
@ <LABEL L195>
@   if (lsr(mem[t], 8) = REF) and (e <> NULL) then
@ <JEQ L199,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 5>>
@ <JUMP L197>
@ <LABEL L199>
@ <JNEQ L196, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>, <CONST 0>>
@ <JUMP L197>
@ <LABEL L196>
@     t := (e+7+(mem[t+1]-1)*TERM_SIZE)
@ <STOREW,
@   <PLUS,
@     <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>, <CONST 7>>,
@     <TIMES,
@       <MINUS,
@         <LOADW,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <TIMES,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 1>>,
@               <CONST 4>>>>,
@         <CONST 1>>,
@       <CONST 2>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@ <JUMP L198>
@ <LABEL L197>
@ <LABEL L198>
@   while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
@ <LABEL L200>
@ <JEQ L203,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JUMP L202>
@ <LABEL L203>
@ <JNEQ L201,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L202>
@ <LABEL L201>
@     t := mem[t+1]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@ <JUMP L200>
@ <LABEL L202>
@   return t
@ <RESULTW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <JUMP L192>
@ <LABEL L192>

@ After simplification:
@   if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
@ <JNEQ L195, <LOADW, <LOCAL 40>>, <CONST 0>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g9>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g10>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L195>
@   if (lsr(mem[t], 8) = REF) and (e <> NULL) then
@ <JNEQ L200,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 5>>
@ <JEQ L200, <LOADW, <LOCAL 44>>, <CONST 0>>
@     t := (e+7+(mem[t+1]-1)*TERM_SIZE)
@ <STOREW,
@   <PLUS,
@     <PLUS, <LOADW, <LOCAL 44>>, <CONST 7>>,
@     <MINUS,
@       <LSL,
@         <LOADW,
@           <OFFSET,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@             <CONST 4>>>,
@         <CONST 1>>,
@       <CONST 2>>>,
@   <LOCAL 40>>
@ <LABEL L200>
@   while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
@ <JNEQ L202,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JEQ L202,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@     t := mem[t+1]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOCAL 40>>
@ <JUMP L200>
@ <LABEL L202>
@   return t
@ <RESULTW, <LOADW, <LOCAL 40>>>

@ After sharing:
@   if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
@ <JNEQ L195, <LOADW, <LOCAL 40>>, <CONST 0>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g9>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g10>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L195>
@   if (lsr(mem[t], 8) = REF) and (e <> NULL) then
@ <DEFTEMP 1,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <JNEQ L200, <LSR, <LOADW, <TEMP 1>>, <CONST 8>>, <CONST 5>>
@ <DEFTEMP 2, <LOADW, <LOCAL 44>>>
@ <JEQ L200, <TEMP 2>, <CONST 0>>
@     t := (e+7+(mem[t+1]-1)*TERM_SIZE)
@ <STOREW,
@   <PLUS,
@     <PLUS, <TEMP 2>, <CONST 7>>,
@     <MINUS,
@       <LSL, <LOADW, <OFFSET, <TEMP 1>, <CONST 4>>>, <CONST 1>>,
@       <CONST 2>>>,
@   <LOCAL 40>>
@ <LABEL L200>
@   while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <JNEQ L202, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 4>>
@ <DEFTEMP 4, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
@ <JEQ L202, <TEMP 4>, <CONST 0>>
@     t := mem[t+1]
@ <STOREW, <TEMP 4>, <LOCAL 40>>
@ <JUMP L200>
@ <LABEL L202>
@   return t
@ <RESULTW, <LOADW, <LOCAL 40>>>

_Deref:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if t = NULL then newline(); print_string("Panic: "); print_string("Deref"); newline(); exit(2) end;
@ <JNEQ L195, <LOADW, <LOCAL 40>>, <CONST 0>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #0
	bne .L195
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g9>>
	set r0, g9
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 5>>
	set r1, #5
@ <ARG 0, <GLOBAL g10>>
	set r0, g10
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L195>
.L195:
@   if (lsr(mem[t], 8) = REF) and (e <> NULL) then
@ <DEFTEMP 1,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r4, r0, r1
@ <JNEQ L200, <LSR, <LOADW, <TEMP 1>>, <CONST 8>>, <CONST 5>>
	ldr r0, [r4]
	lsr r0, r0, #8
	cmp r0, #5
	bne .L200
@ <DEFTEMP 2, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r5, [r0]
@ <JEQ L200, <TEMP 2>, <CONST 0>>
	cmp r5, #0
	beq .L200
@     t := (e+7+(mem[t+1]-1)*TERM_SIZE)
@ <STOREW,
@   <PLUS,
@     <PLUS, <TEMP 2>, <CONST 7>>,
@     <MINUS,
@       <LSL, <LOADW, <OFFSET, <TEMP 1>, <CONST 4>>>, <CONST 1>>,
@       <CONST 2>>>,
@   <LOCAL 40>>
	add r0, r5, #7
	add r1, r4, #4
	ldr r1, [r1]
	lsl r1, r1, #1
	sub r1, r1, #2
	add r0, r0, r1
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L200>
.L200:
@   while (lsr(mem[t], 8) = CELL) and (mem[t+1] <> NULL) do
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r4, r0, r1
@ <JNEQ L202, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 4>>
	ldr r0, [r4]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L202
@ <DEFTEMP 4, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
	add r0, r4, #4
	ldr r4, [r0]
@ <JEQ L202, <TEMP 4>, <CONST 0>>
	cmp r4, #0
	beq .L202
@     t := mem[t+1]
@ <STOREW, <TEMP 4>, <LOCAL 40>>
	set ip, #40
	add r0, fp, ip
	str r4, [r0]
@ <JUMP L200>
	b .L200
@ <LABEL L202>
.L202:
@   return t
@ <RESULTW, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Lookup(var name: tempstring): symbol;
@ Initial code:
@   h := 0; i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@   while name[i] <> ENDSTR do
@ <LABEL L205>
@ <JNEQ L206,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>,
@   <CONST 0>>
@ <JUMP L207>
@ <LABEL L206>
@     h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
@ <STOREW,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0,
@       <PLUS,
@         <TIMES, <CONST 5>, <LOADW, <REGVAR 0>>>,
@         <LOADC,
@           <OFFSET,
@             <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@             <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>>>,
@     <ARG 1, <CONST 511>>>,
@   <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L205>
@ <LABEL L207>
@   p := h+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 2>>
@   while symtab[p].name <> -1 do
@ <LABEL L208>
@ <JNEQ L209,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 2>>, <CONST 16>>>,
@       <CONST 0>>>,
@   <CONST -1>>
@ <JUMP L210>
@ <LABEL L209>
@     if StringEqual(name, symtab[p].name) then return p end;
@ <JNEQ L211,
@   <CALL 2,
@     <GLOBAL _StringEqual>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _symtab>,
@             <TIMES, <LOADW, <REGVAR 2>>, <CONST 16>>>,
@           <CONST 0>>>>>,
@   <CONST 0>>
@ <JUMP L212>
@ <LABEL L211>
@ <RESULTW, <LOADW, <REGVAR 2>>>
@ <JUMP L204>
@ <JUMP L213>
@ <LABEL L212>
@ <LABEL L213>
@     p := p-1;
@ <STOREW, <MINUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@     if p = 0 then p := MAXSYMBOLS end
@ <JEQ L214, <LOADW, <REGVAR 2>>, <CONST 0>>
@ <JUMP L215>
@ <LABEL L214>
@ <STOREW, <CONST 511>, <REGVAR 2>>
@ <JUMP L216>
@ <LABEL L215>
@ <LABEL L216>
@ <JUMP L208>
@ <LABEL L210>
@   if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
@ <JGEQ L217, <LOADW, <GLOBAL _nsymbols>>, <CONST 459>>
@ <JUMP L218>
@ <LABEL L217>
@     newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g11>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g12>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L219>
@ <LABEL L218>
@ <LABEL L219>
@   symtab[p].name := SaveString(name);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _SaveString>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 2>>, <CONST 16>>>,
@     <CONST 0>>>
@   symtab[p].arity := -1;
@ <STOREW,
@   <CONST -1>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 2>>, <CONST 16>>>,
@     <CONST 4>>>
@   symtab[p].action := 0; symtab[p].prok := NULL;
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 2>>, <CONST 16>>>,
@     <CONST 8>>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 2>>, <CONST 16>>>,
@     <CONST 12>>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 2>>>
@ <JUMP L204>
@ <LABEL L204>

@ After simplification:
@   h := 0; i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <LABEL L205>
@   while name[i] <> ENDSTR do
@ <JEQ L207,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>,
@   <CONST 0>>
@     h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
@ <STOREW,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0,
@       <PLUS,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 5>>,
@         <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>>,
@     <ARG 1, <CONST 511>>>,
@   <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L205>
@ <LABEL L207>
@   p := h+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 2>>
@ <LABEL L208>
@   while symtab[p].name <> -1 do
@ <JEQ L210,
@   <LOADW,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>,
@   <CONST -1>>
@     if StringEqual(name, symtab[p].name) then return p end;
@ <JEQ L213,
@   <CALL 2,
@     <GLOBAL _StringEqual>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>>>,
@   <CONST 0>>
@ <RESULTW, <LOADW, <REGVAR 2>>>
@ <JUMP L204>
@ <LABEL L213>
@     p := p-1;
@ <STOREW, <MINUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@     if p = 0 then p := MAXSYMBOLS end
@ <JNEQ L208, <LOADW, <REGVAR 2>>, <CONST 0>>
@ <STOREW, <CONST 511>, <REGVAR 2>>
@ <JUMP L208>
@ <LABEL L210>
@   if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
@ <JLT L219, <LOADW, <GLOBAL _nsymbols>>, <CONST 459>>
@     newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g11>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g12>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L219>
@   symtab[p].name := SaveString(name);
@ <STOREW,
@   <CALL 1, <GLOBAL _SaveString>, <ARG 0, <LOADW, <LOCAL 40>>>>,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>
@   symtab[p].arity := -1;
@ <STOREW,
@   <CONST -1>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>,
@     <CONST 4>>>
@   symtab[p].action := 0; symtab[p].prok := NULL;
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>,
@     <CONST 8>>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>,
@     <CONST 12>>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 2>>>
@ <LABEL L204>

@ After sharing:
@   h := 0; i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <LABEL L205>
@   while name[i] <> ENDSTR do
@ <DEFTEMP 1,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>
@ <JEQ L207, <TEMP 1>, <CONST 0>>
@     h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
@ <DEFTEMP 2,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0, <PLUS, <TIMES, <LOADW, <REGVAR 0>>, <CONST 5>>, <TEMP 1>>>,
@     <ARG 1, <CONST 511>>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L205>
@ <LABEL L207>
@   p := h+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 2>>
@ <LABEL L208>
@   while symtab[p].name <> -1 do
@ <DEFTEMP 3,
@   <LOADW,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>>
@ <JEQ L210, <TEMP 3>, <CONST -1>>
@     if StringEqual(name, symtab[p].name) then return p end;
@ <DEFTEMP 4,
@   <CALL 2,
@     <GLOBAL _StringEqual>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <TEMP 3>>>>
@ <JEQ L213, <TEMP 4>, <CONST 0>>
@ <RESULTW, <LOADW, <REGVAR 2>>>
@ <JUMP L204>
@ <LABEL L213>
@     p := p-1;
@ <STOREW, <MINUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@     if p = 0 then p := MAXSYMBOLS end
@ <JNEQ L208, <LOADW, <REGVAR 2>>, <CONST 0>>
@ <STOREW, <CONST 511>, <REGVAR 2>>
@ <JUMP L208>
@ <LABEL L210>
@   if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
@ <JLT L219, <LOADW, <GLOBAL _nsymbols>>, <CONST 459>>
@     newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g11>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g12>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L219>
@   symtab[p].name := SaveString(name);
@ <DEFTEMP 5,
@   <CALL 1, <GLOBAL _SaveString>, <ARG 0, <LOADW, <LOCAL 40>>>>>
@ <DEFTEMP 6,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>
@ <STOREW, <TEMP 5>, <TEMP 6>>
@   symtab[p].arity := -1;
@ <STOREW, <CONST -1>, <OFFSET, <TEMP 6>, <CONST 4>>>
@   symtab[p].action := 0; symtab[p].prok := NULL;
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 6>, <CONST 8>>>
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 6>, <CONST 12>>>
@   return p
@ <RESULTW, <LOADW, <REGVAR 2>>>
@ <LABEL L204>

_Lookup:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   h := 0; i := 0;
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <STOREW, <CONST 0>, <REGVAR 1>>
	set r5, #0
@ <LABEL L205>
.L205:
@   while name[i] <> ENDSTR do
@ <DEFTEMP 1,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r5
	ldrb r7, [r0]
@ <JEQ L207, <TEMP 1>, <CONST 0>>
	cmp r7, #0
	beq .L207
@     h := (5 * h + ord(name[i])) mod MAXSYMBOLS; i := i+1 
@ <ARG 1, <CONST 511>>
	set r1, #511
@ <ARG 0, <PLUS, <TIMES, <LOADW, <REGVAR 0>>, <CONST 5>>, <TEMP 1>>>
	set r0, #5
	mul r0, r4, r0
	add r0, r0, r7
@ <DEFTEMP 2, <CALL 2, <GLOBAL int_mod>>>
	bl int_mod
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L205>
	b .L205
@ <LABEL L207>
.L207:
@   p := h+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 2>>
	add r6, r4, #1
@ <LABEL L208>
.L208:
@   while symtab[p].name <> -1 do
@ <DEFTEMP 3,
@   <LOADW,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>>
	set r0, _symtab
	lsl r1, r6, #4
	add r0, r0, r1
	ldr r7, [r0]
@ <JEQ L210, <TEMP 3>, <CONST -1>>
	set r0, #-1
	cmp r7, r0
	beq .L210
@     if StringEqual(name, symtab[p].name) then return p end;
@ <ARG 1, <TEMP 3>>
	mov r1, r7
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 2, <GLOBAL _StringEqual>>>
	bl _StringEqual
@ <JEQ L213, <TEMP 4>, <CONST 0>>
	cmp r0, #0
	beq .L213
@ <RESULTW, <LOADW, <REGVAR 2>>>
	mov r0, r6
@ <JUMP L204>
	b .L204
@ <LABEL L213>
.L213:
@     p := p-1;
@ <STOREW, <MINUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
	sub r6, r6, #1
@     if p = 0 then p := MAXSYMBOLS end
@ <JNEQ L208, <LOADW, <REGVAR 2>>, <CONST 0>>
	cmp r6, #0
	bne .L208
@ <STOREW, <CONST 511>, <REGVAR 2>>
	set r6, #511
@ <JUMP L208>
	b .L208
@ <LABEL L210>
.L210:
@   if nsymbols >= (MAXSYMBOLS div 10) * (HASHFACTOR div 10) then
@ <JLT L219, <LOADW, <GLOBAL _nsymbols>>, <CONST 459>>
	set r0, _nsymbols
	ldr r0, [r0]
	set r1, #459
	cmp r0, r1
	blt .L219
@     newline(); print_string("Panic: "); print_string("out of symbol space"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g11>>
	set r0, g11
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 19>>
	set r1, #19
@ <ARG 0, <GLOBAL g12>>
	set r0, g12
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L219>
.L219:
@   symtab[p].name := SaveString(name);
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 5, <CALL 1, <GLOBAL _SaveString>>>
	bl _SaveString
@ <DEFTEMP 6,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 2>>, <CONST 4>>>>
	set r1, _symtab
	lsl r2, r6, #4
	add r7, r1, r2
@ <STOREW, <TEMP 5>, <TEMP 6>>
	str r0, [r7]
@   symtab[p].arity := -1;
@ <STOREW, <CONST -1>, <OFFSET, <TEMP 6>, <CONST 4>>>
	set r0, #-1
	add r1, r7, #4
	str r0, [r1]
@   symtab[p].action := 0; symtab[p].prok := NULL;
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 6>, <CONST 8>>>
	set r0, #0
	add r1, r7, #8
	str r0, [r1]
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 6>, <CONST 12>>>
	set r0, #0
	add r1, r7, #12
	str r0, [r1]
@   return p
@ <RESULTW, <LOADW, <REGVAR 2>>>
	mov r0, r6
@ <LABEL L204>
.L204:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Enter(name: keyword; arity: integer; action: integer): symbol;
@ Initial code:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 1>>
@   while name[i] <> ' ' do
@ <LABEL L221>
@ <JNEQ L222,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>,
@   <CONST 32>>
@ <JUMP L223>
@ <LABEL L222>
@     temp[i] := name[i]; i := i+1 
@ <STOREC,
@   <LOADC,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@       <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -128>>,
@     <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L221>
@ <LABEL L223>
@   temp[i] := ENDSTR; s := Lookup(temp);
@ <STOREC,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -128>>,
@     <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _Lookup>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <OFFSET, <LOCAL 0>, <CONST -128>>>>,
@   <REGVAR 0>>
@   symtab[s].arity := arity; symtab[s].action := action;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@     <CONST 4>>>
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@     <CONST 8>>>
@   return s
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L220>
@ <LABEL L220>

@ After simplification:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <LABEL L221>
@   while name[i] <> ' ' do
@ <JEQ L223,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>,
@   <CONST 32>>
@     temp[i] := name[i]; i := i+1 
@ <STOREC,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>,
@   <OFFSET, <LOCAL -128>, <LOADW, <REGVAR 1>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L221>
@ <LABEL L223>
@   temp[i] := ENDSTR; s := Lookup(temp);
@ <STOREC, <CONST 0>, <OFFSET, <LOCAL -128>, <LOADW, <REGVAR 1>>>>
@ <STOREW, <CALL 1, <GLOBAL _Lookup>, <ARG 0, <LOCAL -128>>>, <REGVAR 0>>
@   symtab[s].arity := arity; symtab[s].action := action;
@ <STOREW,
@   <LOADW, <LOCAL 44>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 4>>>
@ <STOREW,
@   <LOADW, <LOCAL 48>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 8>>>
@   return s
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <LABEL L221>
@   while name[i] <> ' ' do
@ <DEFTEMP 1,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>
@ <JEQ L223, <TEMP 1>, <CONST 32>>
@     temp[i] := name[i]; i := i+1 
@ <STOREC, <TEMP 1>, <OFFSET, <LOCAL -128>, <LOADW, <REGVAR 1>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L221>
@ <LABEL L223>
@   temp[i] := ENDSTR; s := Lookup(temp);
@ <STOREC, <CONST 0>, <OFFSET, <LOCAL -128>, <LOADW, <REGVAR 1>>>>
@ <DEFTEMP 2, <CALL 1, <GLOBAL _Lookup>, <ARG 0, <LOCAL -128>>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@   symtab[s].arity := arity; symtab[s].action := action;
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW, <LOADW, <LOCAL 44>>, <OFFSET, <TEMP 3>, <CONST 4>>>
@ <STOREW, <LOADW, <LOCAL 48>>, <OFFSET, <TEMP 3>, <CONST 8>>>
@   return s
@ <RESULTW, <LOADW, <REGVAR 0>>>

_Enter:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #128
@   i := 0;
@ <STOREW, <CONST 0>, <REGVAR 1>>
	set r5, #0
@ <LABEL L221>
.L221:
@   while name[i] <> ' ' do
@ <DEFTEMP 1,
@   <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r5
	ldrb r6, [r0]
@ <JEQ L223, <TEMP 1>, <CONST 32>>
	cmp r6, #32
	beq .L223
@     temp[i] := name[i]; i := i+1 
@ <STOREC, <TEMP 1>, <OFFSET, <LOCAL -128>, <LOADW, <REGVAR 1>>>>
	set ip, #-128
	add r0, fp, ip
	add r0, r0, r5
	strb r6, [r0]
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L221>
	b .L221
@ <LABEL L223>
.L223:
@   temp[i] := ENDSTR; s := Lookup(temp);
@ <STOREC, <CONST 0>, <OFFSET, <LOCAL -128>, <LOADW, <REGVAR 1>>>>
	set r0, #0
	set ip, #-128
	add r1, fp, ip
	add r1, r1, r5
	strb r0, [r1]
@ <ARG 0, <LOCAL -128>>
	set ip, #-128
	add r0, fp, ip
@ <DEFTEMP 2, <CALL 1, <GLOBAL _Lookup>>>
	bl _Lookup
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@   symtab[s].arity := arity; symtab[s].action := action;
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>
	set r0, _symtab
	lsl r1, r4, #4
	add r6, r0, r1
@ <STOREW, <LOADW, <LOCAL 44>>, <OFFSET, <TEMP 3>, <CONST 4>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r6, #4
	str r0, [r1]
@ <STOREW, <LOADW, <LOCAL 48>>, <OFFSET, <TEMP 3>, <CONST 8>>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r6, #8
	str r0, [r1]
@   return s
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc InitSymbols();
@ Initial code:
@   nsymbols := 0;
@ <STOREW, <CONST 0>, <GLOBAL _nsymbols>>
@   for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <CONST 511>, <REGVAR 2>>
@ <LABEL L225>
@ <JGT L226, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
@ <STOREW,
@   <CONST -1>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@     <CONST 0>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L225>
@ <LABEL L226>
@   cons   := Enter(":       ", 2, 0);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g13>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 0>>>,
@   <GLOBAL _cons>>
@   cutsym := Enter("!       ", 0, CUT);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g14>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 1>>>,
@   <GLOBAL _cutsym>>
@   eqsym  := Enter("=       ", 2, EQUALITY);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g15>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 8>>>,
@   <GLOBAL _eqsym>>
@   nilsym := Enter("nil     ", 0, 0);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g16>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>,
@   <GLOBAL _nilsym>>
@   notsym := Enter("not     ", 1, NAFF);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g17>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 7>>>,
@   <GLOBAL _notsym>>
@   node   := Enter("node    ", 2, 0);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g18>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 0>>>,
@   <GLOBAL _node>>
@   dummy  := Enter("call    ", 1, CALL);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g19>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 2>>>,
@   <REGVAR 1>>
@   dummy  := Enter("plus    ", 3, PLUS);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g20>>,
@     <ARG 1, <CONST 3>>,
@     <ARG 2, <CONST 3>>>,
@   <REGVAR 1>>
@   dummy  := Enter("times   ", 3, TIMES);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g21>>,
@     <ARG 1, <CONST 3>>,
@     <ARG 2, <CONST 4>>>,
@   <REGVAR 1>>
@   dummy  := Enter("integer ", 1, ISINT);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g22>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 5>>>,
@   <REGVAR 1>>
@   dummy  := Enter("char    ", 1, ISCHAR);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g23>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 6>>>,
@   <REGVAR 1>>
@   dummy  := Enter("false   ", 0, FAIL);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g24>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 9>>>,
@   <REGVAR 1>>
@   dummy  := Enter("print   ", 1, PRINT);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g25>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 10>>>,
@   <REGVAR 1>>
@   dummy  := Enter("nl      ", 0, NL)
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL g26>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 11>>>,
@   <REGVAR 1>>
@ <LABEL L224>

@ After simplification:
@   nsymbols := 0;
@ <STOREW, <CONST 0>, <GLOBAL _nsymbols>>
@   for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <CONST 511>, <REGVAR 2>>
@ <LABEL L225>
@ <JGT L226, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
@ <STOREW,
@   <CONST -1>,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L225>
@ <LABEL L226>
@   cons   := Enter(":       ", 2, 0);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g13>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 0>>>,
@   <GLOBAL _cons>>
@   cutsym := Enter("!       ", 0, CUT);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g14>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 1>>>,
@   <GLOBAL _cutsym>>
@   eqsym  := Enter("=       ", 2, EQUALITY);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g15>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 8>>>,
@   <GLOBAL _eqsym>>
@   nilsym := Enter("nil     ", 0, 0);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g16>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>,
@   <GLOBAL _nilsym>>
@   notsym := Enter("not     ", 1, NAFF);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g17>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 7>>>,
@   <GLOBAL _notsym>>
@   node   := Enter("node    ", 2, 0);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g18>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 0>>>,
@   <GLOBAL _node>>
@   dummy  := Enter("call    ", 1, CALL);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g19>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 2>>>,
@   <REGVAR 1>>
@   dummy  := Enter("plus    ", 3, PLUS);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g20>>,
@     <ARG 1, <CONST 3>>,
@     <ARG 2, <CONST 3>>>,
@   <REGVAR 1>>
@   dummy  := Enter("times   ", 3, TIMES);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g21>>,
@     <ARG 1, <CONST 3>>,
@     <ARG 2, <CONST 4>>>,
@   <REGVAR 1>>
@   dummy  := Enter("integer ", 1, ISINT);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g22>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 5>>>,
@   <REGVAR 1>>
@   dummy  := Enter("char    ", 1, ISCHAR);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g23>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 6>>>,
@   <REGVAR 1>>
@   dummy  := Enter("false   ", 0, FAIL);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g24>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 9>>>,
@   <REGVAR 1>>
@   dummy  := Enter("print   ", 1, PRINT);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g25>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 10>>>,
@   <REGVAR 1>>
@   dummy  := Enter("nl      ", 0, NL)
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g26>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 11>>>,
@   <REGVAR 1>>

@ After sharing:
@   nsymbols := 0;
@ <STOREW, <CONST 0>, <GLOBAL _nsymbols>>
@   for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <CONST 511>, <REGVAR 2>>
@ <LABEL L225>
@ <JGT L226, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
@ <STOREW,
@   <CONST -1>,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L225>
@ <LABEL L226>
@   cons   := Enter(":       ", 2, 0);
@ <DEFTEMP 1,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g13>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 0>>>>
@ <STOREW, <TEMP 1>, <GLOBAL _cons>>
@   cutsym := Enter("!       ", 0, CUT);
@ <DEFTEMP 2,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g14>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 1>>>>
@ <STOREW, <TEMP 2>, <GLOBAL _cutsym>>
@   eqsym  := Enter("=       ", 2, EQUALITY);
@ <DEFTEMP 3,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g15>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 8>>>>
@ <STOREW, <TEMP 3>, <GLOBAL _eqsym>>
@   nilsym := Enter("nil     ", 0, 0);
@ <DEFTEMP 4,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g16>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>>
@ <STOREW, <TEMP 4>, <GLOBAL _nilsym>>
@   notsym := Enter("not     ", 1, NAFF);
@ <DEFTEMP 5,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g17>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 7>>>>
@ <STOREW, <TEMP 5>, <GLOBAL _notsym>>
@   node   := Enter("node    ", 2, 0);
@ <DEFTEMP 6,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g18>>,
@     <ARG 1, <CONST 2>>,
@     <ARG 2, <CONST 0>>>>
@ <STOREW, <TEMP 6>, <GLOBAL _node>>
@   dummy  := Enter("call    ", 1, CALL);
@ <DEFTEMP 7,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g19>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 2>>>>
@ <STOREW, <TEMP 7>, <REGVAR 1>>
@   dummy  := Enter("plus    ", 3, PLUS);
@ <DEFTEMP 8,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g20>>,
@     <ARG 1, <CONST 3>>,
@     <ARG 2, <CONST 3>>>>
@ <STOREW, <TEMP 8>, <REGVAR 1>>
@   dummy  := Enter("times   ", 3, TIMES);
@ <DEFTEMP 9,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g21>>,
@     <ARG 1, <CONST 3>>,
@     <ARG 2, <CONST 4>>>>
@ <STOREW, <TEMP 9>, <REGVAR 1>>
@   dummy  := Enter("integer ", 1, ISINT);
@ <DEFTEMP 10,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g22>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 5>>>>
@ <STOREW, <TEMP 10>, <REGVAR 1>>
@   dummy  := Enter("char    ", 1, ISCHAR);
@ <DEFTEMP 11,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g23>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 6>>>>
@ <STOREW, <TEMP 11>, <REGVAR 1>>
@   dummy  := Enter("false   ", 0, FAIL);
@ <DEFTEMP 12,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g24>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 9>>>>
@ <STOREW, <TEMP 12>, <REGVAR 1>>
@   dummy  := Enter("print   ", 1, PRINT);
@ <DEFTEMP 13,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g25>>,
@     <ARG 1, <CONST 1>>,
@     <ARG 2, <CONST 10>>>>
@ <STOREW, <TEMP 13>, <REGVAR 1>>
@   dummy  := Enter("nl      ", 0, NL)
@ <DEFTEMP 14,
@   <CALL 3,
@     <GLOBAL _Enter>,
@     <ARG 0, <GLOBAL g26>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 11>>>>
@ <STOREW, <TEMP 14>, <REGVAR 1>>

_InitSymbols:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   nsymbols := 0;
@ <STOREW, <CONST 0>, <GLOBAL _nsymbols>>
	set r0, #0
	set r1, _nsymbols
	str r0, [r1]
@   for i := 1 to MAXSYMBOLS do symtab[i].name := -1 end;
@ <STOREW, <CONST 1>, <REGVAR 0>>
	set r4, #1
@ <STOREW, <CONST 511>, <REGVAR 2>>
	set r6, #511
@ <LABEL L225>
.L225:
@ <JGT L226, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
	cmp r4, r6
	bgt .L226
@ <STOREW,
@   <CONST -1>,
@   <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>
	set r0, #-1
	set r1, _symtab
	lsl r2, r4, #4
	add r1, r1, r2
	str r0, [r1]
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L225>
	b .L225
@ <LABEL L226>
.L226:
@   cons   := Enter(":       ", 2, 0);
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g13>>
	set r0, g13
@ <DEFTEMP 1, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 1>, <GLOBAL _cons>>
	set r1, _cons
	str r0, [r1]
@   cutsym := Enter("!       ", 0, CUT);
@ <ARG 2, <CONST 1>>
	set r2, #1
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <GLOBAL g14>>
	set r0, g14
@ <DEFTEMP 2, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 2>, <GLOBAL _cutsym>>
	set r1, _cutsym
	str r0, [r1]
@   eqsym  := Enter("=       ", 2, EQUALITY);
@ <ARG 2, <CONST 8>>
	set r2, #8
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g15>>
	set r0, g15
@ <DEFTEMP 3, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 3>, <GLOBAL _eqsym>>
	set r1, _eqsym
	str r0, [r1]
@   nilsym := Enter("nil     ", 0, 0);
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <GLOBAL g16>>
	set r0, g16
@ <DEFTEMP 4, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 4>, <GLOBAL _nilsym>>
	set r1, _nilsym
	str r0, [r1]
@   notsym := Enter("not     ", 1, NAFF);
@ <ARG 2, <CONST 7>>
	set r2, #7
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g17>>
	set r0, g17
@ <DEFTEMP 5, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 5>, <GLOBAL _notsym>>
	set r1, _notsym
	str r0, [r1]
@   node   := Enter("node    ", 2, 0);
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g18>>
	set r0, g18
@ <DEFTEMP 6, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 6>, <GLOBAL _node>>
	set r1, _node
	str r0, [r1]
@   dummy  := Enter("call    ", 1, CALL);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g19>>
	set r0, g19
@ <DEFTEMP 7, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 7>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("plus    ", 3, PLUS);
@ <ARG 2, <CONST 3>>
	set r2, #3
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g20>>
	set r0, g20
@ <DEFTEMP 8, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 8>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("times   ", 3, TIMES);
@ <ARG 2, <CONST 4>>
	set r2, #4
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g21>>
	set r0, g21
@ <DEFTEMP 9, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 9>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("integer ", 1, ISINT);
@ <ARG 2, <CONST 5>>
	set r2, #5
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g22>>
	set r0, g22
@ <DEFTEMP 10, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 10>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("char    ", 1, ISCHAR);
@ <ARG 2, <CONST 6>>
	set r2, #6
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g23>>
	set r0, g23
@ <DEFTEMP 11, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 11>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("false   ", 0, FAIL);
@ <ARG 2, <CONST 9>>
	set r2, #9
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <GLOBAL g24>>
	set r0, g24
@ <DEFTEMP 12, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 12>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("print   ", 1, PRINT);
@ <ARG 2, <CONST 10>>
	set r2, #10
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g25>>
	set r0, g25
@ <DEFTEMP 13, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 13>, <REGVAR 1>>
	mov r5, r0
@   dummy  := Enter("nl      ", 0, NL)
@ <ARG 2, <CONST 11>>
	set r2, #11
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <GLOBAL g26>>
	set r0, g26
@ <DEFTEMP 14, <CALL 3, <GLOBAL _Enter>>>
	bl _Enter
@ <STOREW, <TEMP 14>, <REGVAR 1>>
	mov r5, r0
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc AddClause(c: clause);
@ Initial code:
@   s := mem[mem[c+3]+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS,
@                   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                   <CONST 3>>,
@                 <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <REGVAR 0>>
@   if symtab[s].action <> 0 then
@ <JNEQ L228,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@ <JUMP L229>
@ <LABEL L228>
@     newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g27>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g28>>,
@   <ARG 1, <CONST 40>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     WriteString(symtab[s].name)
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@         <CONST 0>>>>>
@ <JUMP L230>
@ <LABEL L229>
@   elsif symtab[s].prok = NULL then
@ <JEQ L231,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@ <JUMP L232>
@ <LABEL L231>
@     symtab[s].prok := c
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@     <CONST 12>>>
@ <JUMP L233>
@ <LABEL L232>
@     p := symtab[s].prok;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 12>>>,
@   <REGVAR 1>>
@     while mem[p+2] <> NULL do p := mem[p+2] end;
@ <LABEL L234>
@ <JNEQ L235,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 1>>, <CONST 2>>, <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L236>
@ <LABEL L235>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 1>>, <CONST 2>>, <CONST 4>>>>,
@   <REGVAR 1>>
@ <JUMP L234>
@ <LABEL L236>
@     mem[p+2] := c
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 1>>, <CONST 2>>, <CONST 4>>>>
@ <LABEL L233>
@ <LABEL L230>
@ <LABEL L227>

@ After simplification:
@   s := mem[mem[c+3]+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 12>>>,
@           <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
@   if symtab[s].action <> 0 then
@ <JEQ L229,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@     newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g27>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g28>>,
@   <ARG 1, <CONST 40>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     WriteString(symtab[s].name)
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>>>
@ <JUMP L227>
@ <LABEL L229>
@   elsif symtab[s].prok = NULL then
@ <JNEQ L232,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@     symtab[s].prok := c
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 12>>>
@ <JUMP L227>
@ <LABEL L232>
@     p := symtab[s].prok;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 12>>>,
@   <REGVAR 1>>
@ <LABEL L234>
@     while mem[p+2] <> NULL do p := mem[p+2] end;
@ <JEQ L236,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <REGVAR 1>>
@ <JUMP L234>
@ <LABEL L236>
@     mem[p+2] := c
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@     <CONST 8>>>
@ <LABEL L227>

@ After sharing:
@   s := mem[mem[c+3]+1];
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 1>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 12>>>,
@           <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
@   if symtab[s].action <> 0 then
@ <DEFTEMP 2, <GLOBAL _symtab>>
@ <JEQ L229,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 2>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@     newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g27>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g28>>,
@   <ARG 1, <CONST 40>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     WriteString(symtab[s].name)
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW, <OFFSET, <TEMP 2>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>>>
@ <JUMP L227>
@ <LABEL L229>
@   elsif symtab[s].prok = NULL then
@ <DEFTEMP 3,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 12>>>
@ <JNEQ L232, <LOADW, <TEMP 3>>, <CONST 0>>
@     symtab[s].prok := c
@ <STOREW, <LOADW, <LOCAL 40>>, <TEMP 3>>
@ <JUMP L227>
@ <LABEL L232>
@     p := symtab[s].prok;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 12>>>,
@   <REGVAR 1>>
@ <LABEL L234>
@     while mem[p+2] <> NULL do p := mem[p+2] end;
@ <DEFTEMP 4,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@       <CONST 8>>>>
@ <JEQ L236, <TEMP 4>, <CONST 0>>
@ <STOREW, <TEMP 4>, <REGVAR 1>>
@ <JUMP L234>
@ <LABEL L236>
@     mem[p+2] := c
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@     <CONST 8>>>
@ <LABEL L227>

_AddClause:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   s := mem[mem[c+3]+1];
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r6, _mem
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 1>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 12>>>,
@           <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #12
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r4, [r0]
@   if symtab[s].action <> 0 then
@ <DEFTEMP 2, <GLOBAL _symtab>>
	set r6, _symtab
@ <JEQ L229,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 2>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 8>>>,
@   <CONST 0>>
	lsl r0, r4, #4
	add r0, r6, r0
	add r0, r0, #8
	ldr r0, [r0]
	cmp r0, #0
	beq .L229
@     newline(); print_string("Error: "); print_string("cannot add clauses to built-in relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g27>>
	set r0, g27
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 40>>
	set r1, #40
@ <ARG 0, <GLOBAL g28>>
	set r0, g28
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <STOREC, <CONST 0>, <GLOBAL _run>>
	set r0, #0
	set r1, _run
	strb r0, [r1]
@     WriteString(symtab[s].name)
@ <ARG 0,
@   <LOADW, <OFFSET, <TEMP 2>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>>
	lsl r0, r4, #4
	add r0, r6, r0
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _WriteString>>
	bl _WriteString
@ <JUMP L227>
	b .L227
@ <LABEL L229>
.L229:
@   elsif symtab[s].prok = NULL then
@ <DEFTEMP 3,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 12>>>
	set r0, _symtab
	lsl r1, r4, #4
	add r0, r0, r1
	add r6, r0, #12
@ <JNEQ L232, <LOADW, <TEMP 3>>, <CONST 0>>
	ldr r0, [r6]
	cmp r0, #0
	bne .L232
@     symtab[s].prok := c
@ <STOREW, <LOADW, <LOCAL 40>>, <TEMP 3>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	str r0, [r6]
@ <JUMP L227>
	b .L227
@ <LABEL L232>
.L232:
@     p := symtab[s].prok;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 12>>>,
@   <REGVAR 1>>
	set r0, _symtab
	lsl r1, r4, #4
	add r0, r0, r1
	add r0, r0, #12
	ldr r5, [r0]
@ <LABEL L234>
.L234:
@     while mem[p+2] <> NULL do p := mem[p+2] end;
@ <DEFTEMP 4,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@       <CONST 8>>>>
	set r0, _mem
	lsl r1, r5, #2
	add r0, r0, r1
	add r0, r0, #8
	ldr r6, [r0]
@ <JEQ L236, <TEMP 4>, <CONST 0>>
	cmp r6, #0
	beq .L236
@ <STOREW, <TEMP 4>, <REGVAR 1>>
	mov r5, r6
@ <JUMP L234>
	b .L234
@ <LABEL L236>
.L236:
@     mem[p+2] := c
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@     <CONST 8>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _mem
	lsl r2, r5, #2
	add r1, r1, r2
	add r1, r1, #8
	str r0, [r1]
@ <LABEL L227>
.L227:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeCompound(fun: symbol; var arg: argbuf): term;
@ Initial code:
@   n := symtab[fun].arity;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 16>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@   p := HeapAlloc(TERM_SIZE+n);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <PLUS, <CONST 2>, <LOADW, <REGVAR 2>>>>>,
@   <REGVAR 0>>
@   mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
@ <STOREW,
@   <PLUS,
@     <PLUS, <LSL, <CONST 1>, <CONST 8>>, <CONST 2>>,
@     <LOADW, <REGVAR 2>>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@   mem[p+1] := fun;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@   for i := 1 to n do mem[p+i+1] := arg[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <REGVAR 2>>, <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <LABEL L238>
@ <JGT L239,
@   <LOADW, <REGVAR 1>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@       <TIMES, <LOADW, <REGVAR 1>>, <CONST 4>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L238>
@ <LABEL L239>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L237>
@ <LABEL L237>

@ After simplification:
@   n := symtab[fun].arity;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <LOCAL 40>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@   p := HeapAlloc(TERM_SIZE+n);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <ARG 0, <PLUS, <LOADW, <REGVAR 2>>, <CONST 2>>>>,
@   <REGVAR 0>>
@   mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
@ <STOREW,
@   <PLUS, <LOADW, <REGVAR 2>>, <CONST 258>>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@   mem[p+1] := fun;
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@   for i := 1 to n do mem[p+i+1] := arg[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <REGVAR 2>>, <LOCAL -4>>
@ <LABEL L238>
@ <JGT L239, <LOADW, <REGVAR 1>>, <LOADW, <LOCAL -4>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <LOCAL 44>>,
@       <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L238>
@ <LABEL L239>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   n := symtab[fun].arity;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <LOCAL 40>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@   p := HeapAlloc(TERM_SIZE+n);
@ <DEFTEMP 1,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <ARG 0, <PLUS, <LOADW, <REGVAR 2>>, <CONST 2>>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 258>>, <TEMP 2>>
@   mem[p+1] := fun;
@ <STOREW, <LOADW, <LOCAL 40>>, <OFFSET, <TEMP 2>, <CONST 4>>>
@   for i := 1 to n do mem[p+i+1] := arg[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <REGVAR 2>>, <LOCAL -4>>
@ <LABEL L238>
@ <JGT L239, <LOADW, <REGVAR 1>>, <LOADW, <LOCAL -4>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <LOCAL 44>>,
@       <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L238>
@ <LABEL L239>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

_MakeCompound:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   n := symtab[fun].arity;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <LOCAL 40>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
	set r0, _symtab
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #4
	ldr r6, [r0]
@   p := HeapAlloc(TERM_SIZE+n);
@ <ARG 0, <PLUS, <LOADW, <REGVAR 2>>, <CONST 2>>>
	add r0, r6, #2
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>>>
	bl _HeapAlloc
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   mem[p] := lsl(FUNC, 8) + TERM_SIZE+n;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r7, r0, r1
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 258>>, <TEMP 2>>
	set r0, #258
	add r0, r6, r0
	str r0, [r7]
@   mem[p+1] := fun;
@ <STOREW, <LOADW, <LOCAL 40>>, <OFFSET, <TEMP 2>, <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r7, #4
	str r0, [r1]
@   for i := 1 to n do mem[p+i+1] := arg[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
	set r5, #1
@ <STOREW, <LOADW, <REGVAR 2>>, <LOCAL -4>>
	set ip, #-4
	add r0, fp, ip
	str r6, [r0]
@ <LABEL L238>
.L238:
@ <JGT L239, <LOADW, <REGVAR 1>>, <LOADW, <LOCAL -4>>>
	set ip, #-4
	add r0, fp, ip
	ldr r0, [r0]
	cmp r5, r0
	bgt .L239
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <LOCAL 44>>,
@       <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	lsl r1, r5, #2
	add r0, r0, r1
	ldr r0, [r0]
	set r1, _mem
	add r2, r4, r5
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	str r0, [r1]
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L238>
	b .L238
@ <LABEL L239>
.L239:
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeNode(fun: symbol; a1, a2: term): term;
@ Initial code:
@   arg[1] := a1; arg[2] := a2;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -256>>,
@     <TIMES, <CONST 1>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -256>>,
@     <TIMES, <CONST 2>, <CONST 4>>>>
@   return MakeCompound(fun, arg)
@ <RESULTW,
@   <CALL 2,
@     <GLOBAL _MakeCompound>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <OFFSET, <LOCAL 0>, <CONST -256>>>>>
@ <JUMP L240>
@ <LABEL L240>

@ After simplification:
@   arg[1] := a1; arg[2] := a2;
@ <STOREW, <LOADW, <LOCAL 44>>, <LOCAL -252>>
@ <STOREW, <LOADW, <LOCAL 48>>, <LOCAL -248>>
@   return MakeCompound(fun, arg)
@ <RESULTW,
@   <CALL 2,
@     <GLOBAL _MakeCompound>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOCAL -256>>>>

@ After sharing:
@   arg[1] := a1; arg[2] := a2;
@ <STOREW, <LOADW, <LOCAL 44>>, <LOCAL -252>>
@ <STOREW, <LOADW, <LOCAL 48>>, <LOCAL -248>>
@   return MakeCompound(fun, arg)
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _MakeCompound>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOCAL -256>>>>
@ <RESULTW, <TEMP 1>>

_MakeNode:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #256
@   arg[1] := a1; arg[2] := a2;
@ <STOREW, <LOADW, <LOCAL 44>>, <LOCAL -252>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	set ip, #-252
	add r1, fp, ip
	str r0, [r1]
@ <STOREW, <LOADW, <LOCAL 48>>, <LOCAL -248>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	set ip, #-248
	add r1, fp, ip
	str r0, [r1]
@   return MakeCompound(fun, arg)
@ <ARG 1, <LOCAL -256>>
	set ip, #-256
	add r1, fp, ip
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _MakeCompound>>>
	bl _MakeCompound
@ <RESULTW, <TEMP 1>>
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeRef(offset: integer): term;
@ Initial code:
@   return refnode[offset]
@ <RESULTW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _refnode>,
@       <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>>
@ <JUMP L241>
@ <LABEL L241>

@ After simplification:
@   return refnode[offset]
@ <RESULTW,
@   <LOADW,
@     <OFFSET, <GLOBAL _refnode>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>>

@ After sharing:
@   return refnode[offset]
@ <RESULTW,
@   <LOADW,
@     <OFFSET, <GLOBAL _refnode>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>>

_MakeRef:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   return refnode[offset]
@ <RESULTW,
@   <LOADW,
@     <OFFSET, <GLOBAL _refnode>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>>
	set r0, _refnode
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeInt(i: integer): term;
@ Initial code:
@   p := HeapAlloc(TERM_SIZE);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 2>>>,
@   <REGVAR 0>>
@   mem[p] := lsl(INT, 8) + TERM_SIZE;
@ <STOREW,
@   <PLUS, <LSL, <CONST 2>, <CONST 8>>, <CONST 2>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@   mem[p+1] := i; return p
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L242>
@ <LABEL L242>

@ After simplification:
@   p := HeapAlloc(TERM_SIZE);
@ <STOREW, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>, <REGVAR 0>>
@   mem[p] := lsl(INT, 8) + TERM_SIZE;
@ <STOREW,
@   <CONST 514>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@   mem[p+1] := i; return p
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   p := HeapAlloc(TERM_SIZE);
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   mem[p] := lsl(INT, 8) + TERM_SIZE;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <CONST 514>, <TEMP 2>>
@   mem[p+1] := i; return p
@ <STOREW, <LOADW, <LOCAL 40>>, <OFFSET, <TEMP 2>, <CONST 4>>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

_MakeInt:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   p := HeapAlloc(TERM_SIZE);
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>>>
	bl _HeapAlloc
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   mem[p] := lsl(INT, 8) + TERM_SIZE;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r5, r0, r1
@ <STOREW, <CONST 514>, <TEMP 2>>
	set r0, #514
	str r0, [r5]
@   mem[p+1] := i; return p
@ <STOREW, <LOADW, <LOCAL 40>>, <OFFSET, <TEMP 2>, <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r5, #4
	str r0, [r1]
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeChar(c: char): term;
@ Initial code:
@   p := HeapAlloc(TERM_SIZE);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 2>>>,
@   <REGVAR 0>>
@   mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
@ <STOREW,
@   <PLUS, <LSL, <CONST 3>, <CONST 8>>, <CONST 2>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@   mem[p+1] := ord(c); return p
@ <STOREW,
@   <LOADC, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L243>
@ <LABEL L243>

@ After simplification:
@   p := HeapAlloc(TERM_SIZE);
@ <STOREW, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>, <REGVAR 0>>
@   mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
@ <STOREW,
@   <CONST 770>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@   mem[p+1] := ord(c); return p
@ <STOREW,
@   <LOADC, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   p := HeapAlloc(TERM_SIZE);
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <CONST 770>, <TEMP 2>>
@   mem[p+1] := ord(c); return p
@ <STOREW, <LOADC, <LOCAL 40>>, <OFFSET, <TEMP 2>, <CONST 4>>>
@ <RESULTW, <LOADW, <REGVAR 0>>>

_MakeChar:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   p := HeapAlloc(TERM_SIZE);
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>>>
	bl _HeapAlloc
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   mem[p] := lsl(CHRCTR, 8) + TERM_SIZE;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r5, r0, r1
@ <STOREW, <CONST 770>, <TEMP 2>>
	set r0, #770
	str r0, [r5]
@   mem[p+1] := ord(c); return p
@ <STOREW, <LOADC, <LOCAL 40>>, <OFFSET, <TEMP 2>, <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldrb r0, [r0]
	add r1, r5, #4
	str r0, [r1]
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeString(var s: tempstring): term;
@ Initial code:
@   i := StringLength(s);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _StringLength>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>,
@   <REGVAR 1>>
@   p := MakeNode(nilsym, NULL, NULL);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _nilsym>>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>,
@   <REGVAR 0>>
@   while i > 0 do
@ <LABEL L245>
@ <JGT L246, <LOADW, <REGVAR 1>>, <CONST 0>>
@ <JUMP L247>
@ <LABEL L246>
@     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
@ <STOREW, <MINUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1,
@       <CALL 1,
@         <GLOBAL _MakeChar>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <LOADC,
@             <OFFSET,
@               <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@               <TIMES, <LOADW, <REGVAR 1>>, <CONST 1>>>>>>>,
@     <ARG 2, <LOADW, <REGVAR 0>>>>,
@   <REGVAR 0>>
@ <JUMP L245>
@ <LABEL L247>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L244>
@ <LABEL L244>

@ After simplification:
@   i := StringLength(s);
@ <STOREW,
@   <CALL 1, <GLOBAL _StringLength>, <ARG 0, <LOADW, <LOCAL 40>>>>,
@   <REGVAR 1>>
@   p := MakeNode(nilsym, NULL, NULL);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _nilsym>>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>,
@   <REGVAR 0>>
@ <LABEL L245>
@   while i > 0 do
@ <JLEQ L247, <LOADW, <REGVAR 1>>, <CONST 0>>
@     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
@ <STOREW, <MINUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1,
@       <CALL 1,
@         <GLOBAL _MakeChar>,
@         <ARG 0,
@           <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>>>,
@     <ARG 2, <LOADW, <REGVAR 0>>>>,
@   <REGVAR 0>>
@ <JUMP L245>
@ <LABEL L247>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   i := StringLength(s);
@ <DEFTEMP 1,
@   <CALL 1, <GLOBAL _StringLength>, <ARG 0, <LOADW, <LOCAL 40>>>>>
@ <STOREW, <TEMP 1>, <REGVAR 1>>
@   p := MakeNode(nilsym, NULL, NULL);
@ <DEFTEMP 2,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _nilsym>>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@ <LABEL L245>
@   while i > 0 do
@ <JLEQ L247, <LOADW, <REGVAR 1>>, <CONST 0>>
@     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
@ <STOREW, <MINUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <DEFTEMP 3,
@   <CALL 1,
@     <GLOBAL _MakeChar>,
@     <ARG 0,
@       <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>>>
@ <DEFTEMP 4,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <TEMP 3>>,
@     <ARG 2, <LOADW, <REGVAR 0>>>>>
@ <STOREW, <TEMP 4>, <REGVAR 0>>
@ <JUMP L245>
@ <LABEL L247>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

_MakeString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := StringLength(s);
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 1, <GLOBAL _StringLength>>>
	bl _StringLength
@ <STOREW, <TEMP 1>, <REGVAR 1>>
	mov r5, r0
@   p := MakeNode(nilsym, NULL, NULL);
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <LOADW, <GLOBAL _nilsym>>>
	set r0, _nilsym
	ldr r0, [r0]
@ <DEFTEMP 2, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@ <LABEL L245>
.L245:
@   while i > 0 do
@ <JLEQ L247, <LOADW, <REGVAR 1>>, <CONST 0>>
	cmp r5, #0
	ble .L247
@     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
@ <STOREW, <MINUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	sub r5, r5, #1
@ <ARG 0, <LOADC, <OFFSET, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r5
	ldrb r0, [r0]
@ <DEFTEMP 3, <CALL 1, <GLOBAL _MakeChar>>>
	bl _MakeChar
@ <ARG 2, <LOADW, <REGVAR 0>>>
	mov r2, r4
@ <ARG 1, <TEMP 3>>
	mov r1, r0
@ <ARG 0, <LOADW, <GLOBAL _cons>>>
	set r0, _cons
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <STOREW, <TEMP 4>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L245>
	b .L245
@ <LABEL L247>
.L247:
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc MakeClause(nvars: integer; head: term;
@ Initial code:
@   p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <PLUS,
@         <PLUS, <CONST 4>, <LOADW, <OFFSET, <LOCAL 0>, <CONST 52>>>>,
@         <CONST 1>>>>,
@   <REGVAR 0>>
@   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 2>>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 3>>, <CONST 4>>>>
@   for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 52>>>, <REGVAR 2>>
@ <LABEL L249>
@ <JGT L250, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>,
@       <TIMES, <LOADW, <REGVAR 1>>, <CONST 4>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <MINUS,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@           <LOADW, <REGVAR 1>>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L249>
@ <LABEL L250>
@   mem[(p+4)+nbody+1-1] := NULL;
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <MINUS,
@         <PLUS,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@             <LOADW, <OFFSET, <LOCAL 0>, <CONST 52>>>>,
@           <CONST 1>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@   if head = NULL then 
@ <JEQ L251, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>, <CONST 0>>
@ <JUMP L252>
@ <LABEL L251>
@     mem[p+1] := 0
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <JUMP L253>
@ <LABEL L252>
@     mem[p+1] := Key(head, NULL)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Key>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@     <ARG 1, <CONST 0>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <LABEL L253>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L248>
@ <LABEL L248>

@ After simplification:
@   p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <ARG 0, <PLUS, <PLUS, <LOADW, <LOCAL 52>>, <CONST 4>>, <CONST 1>>>>,
@   <REGVAR 0>>
@   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 8>>>
@ <STOREW,
@   <LOADW, <LOCAL 44>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 12>>>
@   for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <LOCAL 52>>, <REGVAR 2>>
@ <LABEL L249>
@ <JGT L250, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <LOCAL 48>>,
@       <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@           <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST -4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L249>
@ <LABEL L250>
@   mem[(p+4)+nbody+1-1] := NULL;
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@         <LOADW, <LOCAL 52>>>,
@       <CONST 2>>>>
@   if head = NULL then 
@ <JNEQ L252, <LOADW, <LOCAL 44>>, <CONST 0>>
@     mem[p+1] := 0
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <JUMP L253>
@ <LABEL L252>
@     mem[p+1] := Key(head, NULL)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Key>,
@     <ARG 0, <LOADW, <LOCAL 44>>>,
@     <ARG 1, <CONST 0>>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L253>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
@ <DEFTEMP 1,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <ARG 0, <PLUS, <PLUS, <LOADW, <LOCAL 52>>, <CONST 4>>, <CONST 1>>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <LOADW, <LOCAL 40>>, <TEMP 2>>
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 2>, <CONST 8>>>
@ <STOREW, <LOADW, <LOCAL 44>>, <OFFSET, <TEMP 2>, <CONST 12>>>
@   for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <LOCAL 52>>, <REGVAR 2>>
@ <LABEL L249>
@ <JGT L250, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <LOCAL 48>>,
@       <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@           <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST -4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L249>
@ <LABEL L250>
@   mem[(p+4)+nbody+1-1] := NULL;
@ <DEFTEMP 3, <GLOBAL _mem>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <TEMP 3>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@         <LOADW, <LOCAL 52>>>,
@       <CONST 2>>>>
@   if head = NULL then 
@ <JNEQ L252, <LOADW, <LOCAL 44>>, <CONST 0>>
@     mem[p+1] := 0
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <JUMP L253>
@ <LABEL L252>
@     mem[p+1] := Key(head, NULL)
@ <DEFTEMP 4,
@   <CALL 2,
@     <GLOBAL _Key>,
@     <ARG 0, <LOADW, <LOCAL 44>>>,
@     <ARG 1, <CONST 0>>>>
@ <STOREW,
@   <TEMP 4>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L253>
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>

_MakeClause:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   p := HeapAlloc(CLAUSE_SIZE + nbody + 1);
@ <ARG 0, <PLUS, <PLUS, <LOADW, <LOCAL 52>>, <CONST 4>>, <CONST 1>>>
	set ip, #52
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, #4
	add r0, r0, #1
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>>>
	bl _HeapAlloc
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r7, r0, r1
@ <STOREW, <LOADW, <LOCAL 40>>, <TEMP 2>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	str r0, [r7]
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 2>, <CONST 8>>>
	set r0, #0
	add r1, r7, #8
	str r0, [r1]
@ <STOREW, <LOADW, <LOCAL 44>>, <OFFSET, <TEMP 2>, <CONST 12>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r7, #12
	str r0, [r1]
@   for i := 1 to nbody do mem[(p+4)+i-1] := body[i] end;
@ <STOREW, <CONST 1>, <REGVAR 1>>
	set r5, #1
@ <STOREW, <LOADW, <LOCAL 52>>, <REGVAR 2>>
	set ip, #52
	add r0, fp, ip
	ldr r6, [r0]
@ <LABEL L249>
.L249:
@ <JGT L250, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
	cmp r5, r6
	bgt .L250
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <LOADW, <LOCAL 48>>,
@       <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@           <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST -4>>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	lsl r1, r5, #2
	add r0, r0, r1
	ldr r0, [r0]
	set r1, _mem
	add r2, r4, #4
	add r2, r2, r5
	lsl r2, r2, #2
	add r1, r1, r2
	set r2, #-4
	add r1, r1, r2
	str r0, [r1]
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L249>
	b .L249
@ <LABEL L250>
.L250:
@   mem[(p+4)+nbody+1-1] := NULL;
@ <DEFTEMP 3, <GLOBAL _mem>>
	set r7, _mem
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <TEMP 3>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>,
@         <LOADW, <LOCAL 52>>>,
@       <CONST 2>>>>
	set r0, #0
	add r1, r4, #4
	set ip, #52
	add r2, fp, ip
	ldr r2, [r2]
	add r1, r1, r2
	lsl r1, r1, #2
	add r1, r7, r1
	str r0, [r1]
@   if head = NULL then 
@ <JNEQ L252, <LOADW, <LOCAL 44>>, <CONST 0>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #0
	bne .L252
@     mem[p+1] := 0
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	set r0, #0
	lsl r1, r4, #2
	add r1, r7, r1
	add r1, r1, #4
	str r0, [r1]
@ <JUMP L253>
	b .L253
@ <LABEL L252>
.L252:
@     mem[p+1] := Key(head, NULL)
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 2, <GLOBAL _Key>>>
	bl _Key
@ <STOREW,
@   <TEMP 4>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	set r1, _mem
	lsl r2, r4, #2
	add r1, r1, r2
	add r1, r1, #4
	str r0, [r1]
@ <LABEL L253>
.L253:
@   return p
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc IsString(t: term; e: frame): boolean;
@ Initial code:
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@   while i < limit do
@ <LABEL L255>
@ <JLT L256, <LOADW, <REGVAR 0>>, <CONST 128>>
@ <JUMP L257>
@ <LABEL L256>
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <JNEQ L258,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JUMP L264>
@ <LABEL L264>
@ <JNEQ L258,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW, <GLOBAL _cons>>>
@ <JUMP L259>
@ <LABEL L258>
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <RESULTW,
@   <AND,
@     <EQ,
@       <LSR,
@         <LOADW,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <TIMES,
@               <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@               <CONST 4>>>>,
@         <CONST 8>>,
@       <CONST 1>>,
@     <EQ,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 4>>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
@ <JUMP L254>
@ <JUMP L260>
@ <LABEL L259>
@     elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
@ <JNEQ L261,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <CALL 2,
@             <GLOBAL _Deref>,
@             <STATLINK, <CONST 0>>,
@             <ARG 0,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS,
@                       <PLUS,
@                         <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                         <CONST 1>>,
@                       <CONST 1>>,
@                     <CONST 4>>>>>,
@             <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 3>>
@ <JUMP L262>
@ <LABEL L261>
@       return false
@ <RESULTW, <CONST 0>>
@ <JUMP L254>
@ <JUMP L263>
@ <LABEL L262>
@       i := i+1; t := Deref(mem[t+2+1], e) 
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 2>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@ <LABEL L263>
@ <LABEL L260>
@ <JUMP L255>
@ <LABEL L257>
@   return false
@ <RESULTW, <CONST 0>>
@ <JUMP L254>
@ <LABEL L254>

@ After simplification:
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@ <LABEL L255>
@   while i < limit do
@ <JGEQ L257, <LOADW, <REGVAR 0>>, <CONST 128>>
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <JNEQ L258,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JEQ L259,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _cons>>>
@ <LABEL L258>
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <RESULTW,
@   <AND,
@     <EQ,
@       <LSR,
@         <LOADW,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@         <CONST 8>>,
@       <CONST 1>>,
@     <EQ,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 4>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
@ <JUMP L254>
@ <LABEL L259>
@     elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
@ <JEQ L262,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <CALL 2,
@             <GLOBAL _Deref>,
@             <ARG 0,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@                   <CONST 8>>>>,
@             <ARG 1, <LOADW, <LOCAL 44>>>>,
@           <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 3>>
@       return false
@ <RESULTW, <CONST 0>>
@ <JUMP L254>
@ <LABEL L262>
@       i := i+1; t := Deref(mem[t+2+1], e) 
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@ <JUMP L255>
@ <LABEL L257>
@   return false
@ <RESULTW, <CONST 0>>
@ <LABEL L254>

@ After sharing:
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <LOCAL 40>>
@ <LABEL L255>
@   while i < limit do
@ <JGEQ L257, <LOADW, <REGVAR 0>>, <CONST 128>>
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <JNEQ L258, <LSR, <LOADW, <TEMP 2>>, <CONST 8>>, <CONST 1>>
@ <JEQ L259,
@   <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>,
@   <LOADW, <GLOBAL _cons>>>
@ <LABEL L258>
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <RESULTW,
@   <AND,
@     <EQ, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 1>>,
@     <EQ,
@       <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
@ <JUMP L254>
@ <LABEL L259>
@     elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
@ <DEFTEMP 4, <GLOBAL _mem>>
@ <DEFTEMP 5,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 8>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <JEQ L262,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 4>, <LSL, <TEMP 5>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 3>>
@       return false
@ <RESULTW, <CONST 0>>
@ <JUMP L254>
@ <LABEL L262>
@       i := i+1; t := Deref(mem[t+2+1], e) 
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <DEFTEMP 6,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 6>, <LOCAL 40>>
@ <JUMP L255>
@ <LABEL L257>
@   return false
@ <RESULTW, <CONST 0>>
@ <LABEL L254>

_IsString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 1>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L255>
.L255:
@   while i < limit do
@ <JGEQ L257, <LOADW, <REGVAR 0>>, <CONST 128>>
	cmp r4, #128
	bge .L257
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r5, r0, r1
@ <JNEQ L258, <LSR, <LOADW, <TEMP 2>>, <CONST 8>>, <CONST 1>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #1
	bne .L258
@ <JEQ L259,
@   <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>,
@   <LOADW, <GLOBAL _cons>>>
	add r0, r5, #4
	ldr r0, [r0]
	set r1, _cons
	ldr r1, [r1]
	cmp r0, r1
	beq .L259
@ <LABEL L258>
.L258:
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r5, r0, r1
@ <RESULTW,
@   <AND,
@     <EQ, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 1>>,
@     <EQ,
@       <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #1
	mov r0, #0
	moveq r0, #1
	add r1, r5, #4
	ldr r1, [r1]
	set r2, _nilsym
	ldr r2, [r2]
	cmp r1, r2
	mov r1, #0
	moveq r1, #1
	and r0, r0, r1
@ <JUMP L254>
	b .L254
@ <LABEL L259>
.L259:
@     elsif lsr(mem[Deref(mem[t+1+1], e)], 8) <> CHRCTR then
@ <DEFTEMP 4, <GLOBAL _mem>>
	set r5, _mem
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #8
	ldr r0, [r0]
@ <DEFTEMP 5, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <JEQ L262,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 4>, <LSL, <TEMP 5>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 3>>
	lsl r0, r0, #2
	add r0, r5, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #3
	beq .L262
@       return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L254>
	b .L254
@ <LABEL L262>
.L262:
@       i := i+1; t := Deref(mem[t+2+1], e) 
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set r0, _mem
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r0, r0, r2
	add r0, r0, #12
	ldr r0, [r0]
@ <DEFTEMP 6, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 6>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L255>
	b .L255
@ <LABEL L257>
.L257:
@   return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <LABEL L254>
.L254:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc IsList(t: term; e: frame): boolean;
@ Initial code:
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@   while i < limit do
@ <LABEL L266>
@ <JLT L267, <LOADW, <REGVAR 0>>, <CONST 128>>
@ <JUMP L268>
@ <LABEL L267>
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <JNEQ L269,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JUMP L272>
@ <LABEL L272>
@ <JNEQ L269,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW, <GLOBAL _cons>>>
@ <JUMP L270>
@ <LABEL L269>
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <RESULTW,
@   <AND,
@     <EQ,
@       <LSR,
@         <LOADW,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <TIMES,
@               <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@               <CONST 4>>>>,
@         <CONST 8>>,
@       <CONST 1>>,
@     <EQ,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 4>>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
@ <JUMP L265>
@ <JUMP L271>
@ <LABEL L270>
@       i := i+1; t := Deref(mem[t+2+1], e)
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 2>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@ <LABEL L271>
@ <JUMP L266>
@ <LABEL L268>
@   return false
@ <RESULTW, <CONST 0>>
@ <JUMP L265>
@ <LABEL L265>

@ After simplification:
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@ <LABEL L266>
@   while i < limit do
@ <JGEQ L268, <LOADW, <REGVAR 0>>, <CONST 128>>
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <JNEQ L269,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JEQ L270,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _cons>>>
@ <LABEL L269>
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <RESULTW,
@   <AND,
@     <EQ,
@       <LSR,
@         <LOADW,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@         <CONST 8>>,
@       <CONST 1>>,
@     <EQ,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 4>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
@ <JUMP L265>
@ <LABEL L270>
@       i := i+1; t := Deref(mem[t+2+1], e)
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@ <JUMP L266>
@ <LABEL L268>
@   return false
@ <RESULTW, <CONST 0>>
@ <LABEL L265>

@ After sharing:
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <LOCAL 40>>
@ <LABEL L266>
@   while i < limit do
@ <JGEQ L268, <LOADW, <REGVAR 0>>, <CONST 128>>
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <JNEQ L269, <LSR, <LOADW, <TEMP 2>>, <CONST 8>>, <CONST 1>>
@ <JEQ L270,
@   <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>,
@   <LOADW, <GLOBAL _cons>>>
@ <LABEL L269>
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <RESULTW,
@   <AND,
@     <EQ, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 1>>,
@     <EQ,
@       <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
@ <JUMP L265>
@ <LABEL L270>
@       i := i+1; t := Deref(mem[t+2+1], e)
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <DEFTEMP 4,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 4>, <LOCAL 40>>
@ <JUMP L266>
@ <LABEL L268>
@   return false
@ <RESULTW, <CONST 0>>
@ <LABEL L265>

_IsList:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   i := 0; t := Deref(t, e);
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 1>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L266>
.L266:
@   while i < limit do
@ <JGEQ L268, <LOADW, <REGVAR 0>>, <CONST 128>>
	cmp r4, #128
	bge .L268
@     if (lsr(mem[t], 8) <> FUNC) or (mem[t+1] <> cons) then
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r5, r0, r1
@ <JNEQ L269, <LSR, <LOADW, <TEMP 2>>, <CONST 8>>, <CONST 1>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #1
	bne .L269
@ <JEQ L270,
@   <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>,
@   <LOADW, <GLOBAL _cons>>>
	add r0, r5, #4
	ldr r0, [r0]
	set r1, _cons
	ldr r1, [r1]
	cmp r0, r1
	beq .L270
@ <LABEL L269>
.L269:
@       return (lsr(mem[t], 8) = FUNC) and (mem[t+1] = nilsym)
@ <DEFTEMP 3,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r5, r0, r1
@ <RESULTW,
@   <AND,
@     <EQ, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 1>>,
@     <EQ,
@       <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@       <LOADW, <GLOBAL _nilsym>>>>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #1
	mov r0, #0
	moveq r0, #1
	add r1, r5, #4
	ldr r1, [r1]
	set r2, _nilsym
	ldr r2, [r2]
	cmp r1, r2
	mov r1, #0
	moveq r1, #1
	and r0, r0, r1
@ <JUMP L265>
	b .L265
@ <LABEL L270>
.L270:
@       i := i+1; t := Deref(mem[t+2+1], e)
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set r0, _mem
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r0, r0, r2
	add r0, r0, #12
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 4>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L266>
	b .L266
@ <LABEL L268>
.L268:
@   return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <LABEL L265>
.L265:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ShowString(t: term; e: frame);
@ Initial code:
@   t := Deref(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@   print_char('"');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 34>>>
@   while mem[t+1] <> nilsym do
@ <LABEL L274>
@ <JNEQ L275,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW, <GLOBAL _nilsym>>>
@ <JUMP L276>
@ <LABEL L275>
@     print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <CALL 2,
@               <GLOBAL _Deref>,
@               <STATLINK, <CONST 0>>,
@               <ARG 0,
@                 <LOADW,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <TIMES,
@                       <PLUS,
@                         <PLUS,
@                           <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                           <CONST 1>>,
@                         <CONST 1>>,
@                       <CONST 4>>>>>,
@               <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@             <CONST 1>>,
@           <CONST 4>>>>>>
@     t := Deref(mem[t+2+1], e)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 2>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@ <JUMP L274>
@ <LABEL L276>
@   print_char('"')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 34>>>
@ <LABEL L273>

@ After simplification:
@   t := Deref(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@   print_char('"');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 34>>>
@ <LABEL L274>
@   while mem[t+1] <> nilsym do
@ <JEQ L276,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _nilsym>>>
@     print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL,
@             <CALL 2,
@               <GLOBAL _Deref>,
@               <ARG 0,
@                 <LOADW,
@                   <OFFSET,
@                     <OFFSET,
@                       <GLOBAL _mem>,
@                       <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@                     <CONST 8>>>>,
@               <ARG 1, <LOADW, <LOCAL 44>>>>,
@             <CONST 2>>>,
@         <CONST 4>>>>>
@     t := Deref(mem[t+2+1], e)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@ <JUMP L274>
@ <LABEL L276>
@   print_char('"')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 34>>>

@ After sharing:
@   t := Deref(t, e);
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <LOCAL 40>>
@   print_char('"');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 34>>>
@ <LABEL L274>
@   while mem[t+1] <> nilsym do
@ <DEFTEMP 2, <GLOBAL _mem>>
@ <DEFTEMP 3, <OFFSET, <TEMP 2>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
@ <JEQ L276,
@   <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@   <LOADW, <GLOBAL _nilsym>>>
@     print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
@ <DEFTEMP 4,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 3>, <CONST 8>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 2>, <LSL, <TEMP 4>, <CONST 2>>>,
@         <CONST 4>>>>>
@     t := Deref(mem[t+2+1], e)
@ <DEFTEMP 5,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 2>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 5>, <LOCAL 40>>
@ <JUMP L274>
@ <LABEL L276>
@   print_char('"')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 34>>>

_ShowString:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t := Deref(t, e);
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 1>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@   print_char('"');
@ <ARG 0, <CONST 34>>
	set r0, #34
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <LABEL L274>
.L274:
@   while mem[t+1] <> nilsym do
@ <DEFTEMP 2, <GLOBAL _mem>>
	set r4, _mem
@ <DEFTEMP 3, <OFFSET, <TEMP 2>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r5, r4, r0
@ <JEQ L276,
@   <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@   <LOADW, <GLOBAL _nilsym>>>
	add r0, r5, #4
	ldr r0, [r0]
	set r1, _nilsym
	ldr r1, [r1]
	cmp r0, r1
	beq .L276
@     print_char(chr(mem[Deref(mem[t+1+1], e)+1]));
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 3>, <CONST 8>>>>
	add r0, r5, #8
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <ARG 0,
@   <LOADW,
@     <OFFSET, <OFFSET, <TEMP 2>, <LSL, <TEMP 4>, <CONST 2>>>, <CONST 4>>>>
	mov r5, r0
	lsl r0, r5, #2
	add r0, r4, r0
	add r0, r0, #4
	ldr r0, [r0]
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@     t := Deref(mem[t+2+1], e)
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 2>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r4, r0
	add r0, r0, #12
	ldr r0, [r0]
@ <DEFTEMP 5, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 5>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L274>
	b .L274
@ <LABEL L276>
.L276:
@   print_char('"')
@ <ARG 0, <CONST 34>>
	set r0, #34
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PrintCompound(t: term; e: frame; prio: integer);
@ Initial code:
@   f := mem[t+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <REGVAR 0>>
@   if f = cons then
@ <JEQ L278, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _cons>>>
@ <JUMP L279>
@ <LABEL L278>
@     if IsString(t, e) then
@ <JNEQ L302,
@   <CALL 2,
@     <GLOBAL _IsString>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <CONST 0>>
@ <JUMP L303>
@ <LABEL L302>
@       ShowString(t, e)
@ <CALL 2,
@   <GLOBAL _ShowString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>
@ <JUMP L304>
@ <LABEL L303>
@       if prio < CONSPRIO then print_char('(') end;
@ <JLT L305, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>
@ <JUMP L306>
@ <LABEL L305>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@ <JUMP L307>
@ <LABEL L306>
@ <LABEL L307>
@       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 0>>>
@       print_char(':');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 58>>>
@       PrintTerm(mem[t+2+1], e, CONSPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 2>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 1>>>
@       if prio < CONSPRIO then print_char(')') end
@ <JLT L308, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>
@ <JUMP L309>
@ <LABEL L308>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L310>
@ <LABEL L309>
@ <LABEL L310>
@ <LABEL L304>
@ <JUMP L280>
@ <LABEL L279>
@   elsif f = eqsym then
@ <JEQ L281, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _eqsym>>>
@ <JUMP L282>
@ <LABEL L281>
@     if prio < EQPRIO then print_char('(') end;
@ <JLT L296, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 2>>
@ <JUMP L297>
@ <LABEL L296>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@ <JUMP L298>
@ <LABEL L297>
@ <LABEL L298>
@     PrintTerm(mem[t+1+1], e, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 1>>>
@     print_string(" = ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g29>>,
@   <ARG 1, <CONST 3>>>
@     PrintTerm(mem[t+2+1], e, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 2>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 1>>>
@     if prio < EQPRIO then print_char(')') end
@ <JLT L299, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 2>>
@ <JUMP L300>
@ <LABEL L299>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L301>
@ <LABEL L300>
@ <LABEL L301>
@ <JUMP L283>
@ <LABEL L282>
@   elsif f = notsym then
@ <JEQ L284, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _notsym>>>
@ <JUMP L285>
@ <LABEL L284>
@     print_string("not ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g30>>,
@   <ARG 1, <CONST 4>>>
@     PrintTerm(mem[t+1+1], e, MAXPRIO)
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 2>>>
@ <JUMP L286>
@ <LABEL L285>
@   elsif (f = node) and IsList(mem[t+2+1], e) then
@ <JEQ L295, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _node>>>
@ <JUMP L288>
@ <LABEL L295>
@ <JNEQ L287,
@   <CALL 2,
@     <GLOBAL _IsList>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 2>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <CONST 0>>
@ <JUMP L288>
@ <LABEL L287>
@     PrintNode(t, e)
@ <CALL 2,
@   <GLOBAL _PrintNode>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>
@ <JUMP L289>
@ <LABEL L288>
@     WriteString(symtab[f].name);
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@         <CONST 0>>>>>
@     if symtab[f].arity > 0 then
@ <JGT L290,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <JUMP L291>
@ <LABEL L290>
@       print_char('(');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@       PrintTerm(mem[t+1+1], e, ARGPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 2>>>
@       for i := 2 to symtab[f].arity do
@ <STOREW, <CONST 2>, <REGVAR 1>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@ <LABEL L293>
@ <JGT L294, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@         print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g31>>,
@   <ARG 1, <CONST 2>>>
@         PrintTerm(mem[t+i+1], e, ARGPRIO)
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS,
@               <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@               <LOADW, <REGVAR 1>>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 2>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L293>
@ <LABEL L294>
@       print_char(')')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L292>
@ <LABEL L291>
@ <LABEL L292>
@ <LABEL L289>
@ <LABEL L286>
@ <LABEL L283>
@ <LABEL L280>
@ <LABEL L277>

@ After simplification:
@   f := mem[t+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
@   if f = cons then
@ <JNEQ L279, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _cons>>>
@     if IsString(t, e) then
@ <JEQ L303,
@   <CALL 2,
@     <GLOBAL _IsString>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <CONST 0>>
@       ShowString(t, e)
@ <CALL 2,
@   <GLOBAL _ShowString>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>>
@ <JUMP L277>
@ <LABEL L303>
@       if prio < CONSPRIO then print_char('(') end;
@ <JGEQ L307, <LOADW, <LOCAL 48>>, <CONST 1>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@ <LABEL L307>
@       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 0>>>
@       print_char(':');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 58>>>
@       PrintTerm(mem[t+2+1], e, CONSPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 12>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 1>>>
@       if prio < CONSPRIO then print_char(')') end
@ <JGEQ L277, <LOADW, <LOCAL 48>>, <CONST 1>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L277>
@ <LABEL L279>
@   elsif f = eqsym then
@ <JNEQ L282, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _eqsym>>>
@     if prio < EQPRIO then print_char('(') end;
@ <JGEQ L298, <LOADW, <LOCAL 48>>, <CONST 2>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@ <LABEL L298>
@     PrintTerm(mem[t+1+1], e, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 1>>>
@     print_string(" = ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g29>>,
@   <ARG 1, <CONST 3>>>
@     PrintTerm(mem[t+2+1], e, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 12>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 1>>>
@     if prio < EQPRIO then print_char(')') end
@ <JGEQ L277, <LOADW, <LOCAL 48>>, <CONST 2>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L277>
@ <LABEL L282>
@   elsif f = notsym then
@ <JNEQ L285, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _notsym>>>
@     print_string("not ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g30>>,
@   <ARG 1, <CONST 4>>>
@     PrintTerm(mem[t+1+1], e, MAXPRIO)
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@ <JUMP L277>
@ <LABEL L285>
@   elsif (f = node) and IsList(mem[t+2+1], e) then
@ <JNEQ L288, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _node>>>
@ <JEQ L288,
@   <CALL 2,
@     <GLOBAL _IsList>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <CONST 0>>
@     PrintNode(t, e)
@ <CALL 2,
@   <GLOBAL _PrintNode>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>>
@ <JUMP L277>
@ <LABEL L288>
@     WriteString(symtab[f].name);
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>>>
@     if symtab[f].arity > 0 then
@ <JLEQ L277,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@       print_char('(');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@       PrintTerm(mem[t+1+1], e, ARGPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@       for i := 2 to symtab[f].arity do
@ <STOREW, <CONST 2>, <REGVAR 1>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@ <LABEL L293>
@ <JGT L294, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@         print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g31>>,
@   <ARG 1, <CONST 2>>>
@         PrintTerm(mem[t+i+1], e, ARGPRIO)
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL,
@             <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>,
@             <CONST 2>>>,
@         <CONST 4>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L293>
@ <LABEL L294>
@       print_char(')')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <LABEL L277>

@ After sharing:
@   f := mem[t+1];
@ <DEFTEMP 1, <LOADW, <LOCAL 40>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 1>, <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
@   if f = cons then
@ <JNEQ L279, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _cons>>>
@     if IsString(t, e) then
@ <DEFTEMP 2,
@   <CALL 2,
@     <GLOBAL _IsString>,
@     <ARG 0, <TEMP 1>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <JEQ L303, <TEMP 2>, <CONST 0>>
@       ShowString(t, e)
@ <CALL 2,
@   <GLOBAL _ShowString>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>>
@ <JUMP L277>
@ <LABEL L303>
@       if prio < CONSPRIO then print_char('(') end;
@ <JGEQ L307, <LOADW, <LOCAL 48>>, <CONST 1>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@ <LABEL L307>
@       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
@ <DEFTEMP 3, <GLOBAL _mem>>
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 3>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 0>>>
@       print_char(':');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 58>>>
@       PrintTerm(mem[t+2+1], e, CONSPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 3>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 12>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 1>>>
@       if prio < CONSPRIO then print_char(')') end
@ <JGEQ L277, <LOADW, <LOCAL 48>>, <CONST 1>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L277>
@ <LABEL L279>
@   elsif f = eqsym then
@ <JNEQ L282, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _eqsym>>>
@     if prio < EQPRIO then print_char('(') end;
@ <JGEQ L298, <LOADW, <LOCAL 48>>, <CONST 2>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@ <LABEL L298>
@     PrintTerm(mem[t+1+1], e, EQPRIO-1);
@ <DEFTEMP 4, <GLOBAL _mem>>
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 1>>>
@     print_string(" = ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g29>>,
@   <ARG 1, <CONST 3>>>
@     PrintTerm(mem[t+2+1], e, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 12>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 1>>>
@     if prio < EQPRIO then print_char(')') end
@ <JGEQ L277, <LOADW, <LOCAL 48>>, <CONST 2>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <JUMP L277>
@ <LABEL L282>
@   elsif f = notsym then
@ <JNEQ L285, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _notsym>>>
@     print_string("not ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g30>>,
@   <ARG 1, <CONST 4>>>
@     PrintTerm(mem[t+1+1], e, MAXPRIO)
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@ <JUMP L277>
@ <LABEL L285>
@   elsif (f = node) and IsList(mem[t+2+1], e) then
@ <JNEQ L288, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _node>>>
@ <DEFTEMP 5,
@   <CALL 2,
@     <GLOBAL _IsList>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <JEQ L288, <TEMP 5>, <CONST 0>>
@     PrintNode(t, e)
@ <CALL 2,
@   <GLOBAL _PrintNode>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>>
@ <JUMP L277>
@ <LABEL L288>
@     WriteString(symtab[f].name);
@ <DEFTEMP 6, <GLOBAL _symtab>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW, <OFFSET, <TEMP 6>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>>>
@     if symtab[f].arity > 0 then
@ <JLEQ L277,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 6>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@       print_char('(');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 40>>>
@       PrintTerm(mem[t+1+1], e, ARGPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@       for i := 2 to symtab[f].arity do
@ <STOREW, <CONST 2>, <REGVAR 1>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 6>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@ <LABEL L293>
@ <JGT L294, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@         print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g31>>,
@   <ARG 1, <CONST 2>>>
@         PrintTerm(mem[t+i+1], e, ARGPRIO)
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL,
@             <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>,
@             <CONST 2>>>,
@         <CONST 4>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L293>
@ <LABEL L294>
@       print_char(')')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 41>>>
@ <LABEL L277>

_PrintCompound:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   f := mem[t+1];
@ <DEFTEMP 1, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r7, [r0]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 1>, <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
	set r0, _mem
	lsl r1, r7, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r4, [r0]
@   if f = cons then
@ <JNEQ L279, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _cons>>>
	set r0, _cons
	ldr r0, [r0]
	cmp r4, r0
	bne .L279
@     if IsString(t, e) then
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <TEMP 1>>
	mov r0, r7
@ <DEFTEMP 2, <CALL 2, <GLOBAL _IsString>>>
	bl _IsString
@ <JEQ L303, <TEMP 2>, <CONST 0>>
	cmp r0, #0
	beq .L303
@       ShowString(t, e)
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <CALL 2, <GLOBAL _ShowString>>
	bl _ShowString
@ <JUMP L277>
	b .L277
@ <LABEL L303>
.L303:
@       if prio < CONSPRIO then print_char('(') end;
@ <JGEQ L307, <LOADW, <LOCAL 48>>, <CONST 1>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #1
	bge .L307
@ <ARG 0, <CONST 40>>
	set r0, #40
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <LABEL L307>
.L307:
@       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
@ <DEFTEMP 3, <GLOBAL _mem>>
	set r7, _mem
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 3>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #8
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@       print_char(':');
@ <ARG 0, <CONST 58>>
	set r0, #58
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@       PrintTerm(mem[t+2+1], e, CONSPRIO);
@ <ARG 2, <CONST 1>>
	set r2, #1
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 3>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #12
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@       if prio < CONSPRIO then print_char(')') end
@ <JGEQ L277, <LOADW, <LOCAL 48>>, <CONST 1>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #1
	bge .L277
@ <ARG 0, <CONST 41>>
	set r0, #41
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <JUMP L277>
	b .L277
@ <LABEL L279>
.L279:
@   elsif f = eqsym then
@ <JNEQ L282, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _eqsym>>>
	set r0, _eqsym
	ldr r0, [r0]
	cmp r4, r0
	bne .L282
@     if prio < EQPRIO then print_char('(') end;
@ <JGEQ L298, <LOADW, <LOCAL 48>>, <CONST 2>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #2
	bge .L298
@ <ARG 0, <CONST 40>>
	set r0, #40
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <LABEL L298>
.L298:
@     PrintTerm(mem[t+1+1], e, EQPRIO-1);
@ <DEFTEMP 4, <GLOBAL _mem>>
	set r7, _mem
@ <ARG 2, <CONST 1>>
	set r2, #1
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #8
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@     print_string(" = ");
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g29>>
	set r0, g29
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     PrintTerm(mem[t+2+1], e, EQPRIO-1);
@ <ARG 2, <CONST 1>>
	set r2, #1
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #12
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@     if prio < EQPRIO then print_char(')') end
@ <JGEQ L277, <LOADW, <LOCAL 48>>, <CONST 2>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #2
	bge .L277
@ <ARG 0, <CONST 41>>
	set r0, #41
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <JUMP L277>
	b .L277
@ <LABEL L282>
.L282:
@   elsif f = notsym then
@ <JNEQ L285, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _notsym>>>
	set r0, _notsym
	ldr r0, [r0]
	cmp r4, r0
	bne .L285
@     print_string("not ");
@ <ARG 1, <CONST 4>>
	set r1, #4
@ <ARG 0, <GLOBAL g30>>
	set r0, g30
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     PrintTerm(mem[t+1+1], e, MAXPRIO)
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set r0, _mem
	set ip, #40
	add r3, fp, ip
	ldr r3, [r3]
	lsl r3, r3, #2
	add r0, r0, r3
	add r0, r0, #8
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@ <JUMP L277>
	b .L277
@ <LABEL L285>
.L285:
@   elsif (f = node) and IsList(mem[t+2+1], e) then
@ <JNEQ L288, <LOADW, <REGVAR 0>>, <LOADW, <GLOBAL _node>>>
	set r0, _node
	ldr r0, [r0]
	cmp r4, r0
	bne .L288
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set r0, _mem
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r0, r0, r2
	add r0, r0, #12
	ldr r0, [r0]
@ <DEFTEMP 5, <CALL 2, <GLOBAL _IsList>>>
	bl _IsList
@ <JEQ L288, <TEMP 5>, <CONST 0>>
	cmp r0, #0
	beq .L288
@     PrintNode(t, e)
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <CALL 2, <GLOBAL _PrintNode>>
	bl _PrintNode
@ <JUMP L277>
	b .L277
@ <LABEL L288>
.L288:
@     WriteString(symtab[f].name);
@ <DEFTEMP 6, <GLOBAL _symtab>>
	set r7, _symtab
@ <ARG 0,
@   <LOADW, <OFFSET, <TEMP 6>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>>>
	lsl r0, r4, #4
	add r0, r7, r0
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _WriteString>>
	bl _WriteString
@     if symtab[f].arity > 0 then
@ <JLEQ L277,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 6>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST 0>>
	lsl r0, r4, #4
	add r0, r7, r0
	add r0, r0, #4
	ldr r0, [r0]
	cmp r0, #0
	ble .L277
@       print_char('(');
@ <ARG 0, <CONST 40>>
	set r0, #40
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@       PrintTerm(mem[t+1+1], e, ARGPRIO);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set r0, _mem
	set ip, #40
	add r3, fp, ip
	ldr r3, [r3]
	lsl r3, r3, #2
	add r0, r0, r3
	add r0, r0, #8
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@       for i := 2 to symtab[f].arity do
@ <STOREW, <CONST 2>, <REGVAR 1>>
	set r5, #2
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 6>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
	lsl r0, r4, #4
	add r0, r7, r0
	add r0, r0, #4
	ldr r6, [r0]
@ <LABEL L293>
.L293:
@ <JGT L294, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
	cmp r5, r6
	bgt .L294
@         print_string(", ");
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g31>>
	set r0, g31
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@         PrintTerm(mem[t+i+1], e, ARGPRIO)
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>,
@           <CONST 2>>>,
@       <CONST 4>>>>
	set r0, _mem
	set ip, #40
	add r3, fp, ip
	ldr r3, [r3]
	add r3, r3, r5
	lsl r3, r3, #2
	add r0, r0, r3
	add r0, r0, #4
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L293>
	b .L293
@ <LABEL L294>
.L294:
@       print_char(')')
@ <ARG 0, <CONST 41>>
	set r0, #41
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <LABEL L277>
.L277:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PrintNode(t: term; e: frame);
@ Initial code:
@   print_char('<');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 60>>>
@   PrintTerm(mem[t+1+1], e, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 2>>>
@   u := Deref(mem[t+2+1], e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 2>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <REGVAR 0>>
@   while mem[u+1] <> nilsym do
@ <LABEL L312>
@ <JNEQ L313,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>,
@   <LOADW, <GLOBAL _nilsym>>>
@ <JUMP L314>
@ <LABEL L313>
@     print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g32>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(mem[u+1+1], e, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <CONST 2>>>
@     u := Deref(mem[u+2+1], e)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <PLUS, <LOADW, <REGVAR 0>>, <CONST 2>>, <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <REGVAR 0>>
@ <JUMP L312>
@ <LABEL L314>
@   print_char('>');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 62>>>
@ end;
@ <LABEL L311>

@ After simplification:
@   print_char('<');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 60>>>
@   PrintTerm(mem[t+1+1], e, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@   u := Deref(mem[t+2+1], e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <REGVAR 0>>
@ <LABEL L312>
@   while mem[u+1] <> nilsym do
@ <JEQ L314,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _nilsym>>>
@     print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g32>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(mem[u+1+1], e, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@     u := Deref(mem[u+2+1], e)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <REGVAR 0>>
@ <JUMP L312>
@ <LABEL L314>
@   print_char('>');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 62>>>

@ After sharing:
@   print_char('<');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 60>>>
@   PrintTerm(mem[t+1+1], e, MAXPRIO);
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@   u := Deref(mem[t+2+1], e);
@ <DEFTEMP 2,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@ <LABEL L312>
@   while mem[u+1] <> nilsym do
@ <DEFTEMP 3, <GLOBAL _mem>>
@ <JEQ L314,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _nilsym>>>
@     print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g32>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(mem[u+1+1], e, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 8>>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <CONST 2>>>
@     u := Deref(mem[u+2+1], e)
@ <DEFTEMP 4,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 4>, <REGVAR 0>>
@ <JUMP L312>
@ <LABEL L314>
@   print_char('>');
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 62>>>

_PrintNode:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   print_char('<');
@ <ARG 0, <CONST 60>>
	set r0, #60
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@   PrintTerm(mem[t+1+1], e, MAXPRIO);
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r5, _mem
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #8
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@   u := Deref(mem[t+2+1], e);
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #12
	ldr r0, [r0]
@ <DEFTEMP 2, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@ <LABEL L312>
.L312:
@   while mem[u+1] <> nilsym do
@ <DEFTEMP 3, <GLOBAL _mem>>
	set r5, _mem
@ <JEQ L314,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _nilsym>>>
	lsl r0, r4, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r0, [r0]
	set r1, _nilsym
	ldr r1, [r1]
	cmp r0, r1
	beq .L314
@     print_string(", ");
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g32>>
	set r0, g32
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     PrintTerm(mem[u+1+1], e, MAXPRIO);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 8>>>>
	lsl r0, r4, #2
	add r0, r5, r0
	add r0, r0, #8
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@     u := Deref(mem[u+2+1], e)
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 3>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 12>>>>
	lsl r0, r4, #2
	add r0, r5, r0
	add r0, r0, #12
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 4>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L312>
	b .L312
@ <LABEL L314>
.L314:
@   print_char('>');
@ <ARG 0, <CONST 62>>
	set r0, #62
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PrintTerm(t: term; e: frame; prio: integer);
@ Initial code:
@   t := Deref(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@   if t = NULL then
@ <JEQ L316, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 0>>
@ <JUMP L317>
@ <LABEL L316>
@     print_string("*null-term*")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g33>>,
@   <ARG 1, <CONST 11>>>
@ <JUMP L318>
@ <LABEL L317>
@     case lsr(mem[t], 8) of
@ <JCASE 5 L319,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L321>
@         PrintCompound(t, e, prio)
@ <CALL 3,
@   <GLOBAL _PrintCompound>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@   <ARG 2, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>>
@ <JUMP L320>
@ <LABEL L322>
@         print_num(mem[t+1])
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@           <CONST 4>>>>>>
@ <JUMP L320>
@ <LABEL L323>
@         print_char(''''); print_char(chr(mem[t+1])); print_char('''')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 39>>>
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@           <CONST 4>>>>>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 39>>>
@ <JUMP L320>
@ <LABEL L324>
@         if (t >= gsp) then
@ <JGEQ L326,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW, <GLOBAL _gsp>>>
@ <JUMP L327>
@ <LABEL L326>
@           print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 71>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <CALL 2,
@       <GLOBAL int_div>,
@       <ARG 0,
@         <MINUS,
@           <CONST 25000>,
@           <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>,
@       <ARG 1, <CONST 2>>>>>
@ <JUMP L328>
@ <LABEL L327>
@           print_char('L'); print_num((t - hp) div TERM_SIZE)
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 76>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <CALL 2,
@       <GLOBAL int_div>,
@       <ARG 0,
@         <MINUS,
@           <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@           <LOADW, <GLOBAL _hp>>>>,
@       <ARG 1, <CONST 2>>>>>
@ <LABEL L328>
@ <JUMP L320>
@ <LABEL L325>
@         print_char('@'); print_num(mem[t+1])
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 64>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@           <CONST 4>>>>>>
@ <JUMP L320>
@ <LABEL L319>
@       print_string("*unknown-term(tag="); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g34>>,
@   <ARG 1, <CONST 18>>>
@       print_num(lsr(mem[t], 8)); print_string(")*")
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@       <CONST 8>>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g35>>,
@   <ARG 1, <CONST 2>>>
@ <LABEL L320>
@ <LABEL L318>
@ <LABEL L315>

@ After simplification:
@   t := Deref(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@   if t = NULL then
@ <JNEQ L317, <LOADW, <LOCAL 40>>, <CONST 0>>
@     print_string("*null-term*")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g33>>,
@   <ARG 1, <CONST 11>>>
@ <JUMP L315>
@ <LABEL L317>
@     case lsr(mem[t], 8) of
@ <JCASE 5 L319,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L321>
@         PrintCompound(t, e, prio)
@ <CALL 3,
@   <GLOBAL _PrintCompound>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <LOADW, <LOCAL 48>>>>
@ <JUMP L315>
@ <LABEL L322>
@         print_num(mem[t+1])
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L315>
@ <LABEL L323>
@         print_char(''''); print_char(chr(mem[t+1])); print_char('''')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 39>>>
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 39>>>
@ <JUMP L315>
@ <LABEL L324>
@         if (t >= gsp) then
@ <JLT L327, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _gsp>>>
@           print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 71>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <CALL 2,
@       <GLOBAL int_div>,
@       <ARG 0, <MINUS, <CONST 25000>, <LOADW, <LOCAL 40>>>>,
@       <ARG 1, <CONST 2>>>>>
@ <JUMP L315>
@ <LABEL L327>
@           print_char('L'); print_num((t - hp) div TERM_SIZE)
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 76>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <CALL 2,
@       <GLOBAL int_div>,
@       <ARG 0, <MINUS, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _hp>>>>,
@       <ARG 1, <CONST 2>>>>>
@ <JUMP L315>
@ <LABEL L325>
@         print_char('@'); print_num(mem[t+1])
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 64>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L315>
@ <LABEL L319>
@       print_string("*unknown-term(tag="); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g34>>,
@   <ARG 1, <CONST 18>>>
@       print_num(lsr(mem[t], 8)); print_string(")*")
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g35>>,
@   <ARG 1, <CONST 2>>>
@ <LABEL L315>

@ After sharing:
@   t := Deref(t, e);
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <LOCAL 40>>
@   if t = NULL then
@ <JNEQ L317, <TEMP 1>, <CONST 0>>
@     print_string("*null-term*")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g33>>,
@   <ARG 1, <CONST 11>>>
@ <JUMP L315>
@ <LABEL L317>
@     case lsr(mem[t], 8) of
@ <JCASE 5 L319,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L321>
@         PrintCompound(t, e, prio)
@ <CALL 3,
@   <GLOBAL _PrintCompound>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 44>>>,
@   <ARG 2, <LOADW, <LOCAL 48>>>>
@ <JUMP L315>
@ <LABEL L322>
@         print_num(mem[t+1])
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L315>
@ <LABEL L323>
@         print_char(''''); print_char(chr(mem[t+1])); print_char('''')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 39>>>
@ <CALL 1,
@   <GLOBAL print_char>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 39>>>
@ <JUMP L315>
@ <LABEL L324>
@         if (t >= gsp) then
@ <JLT L327, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _gsp>>>
@           print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 71>>>
@ <DEFTEMP 2,
@   <CALL 2,
@     <GLOBAL int_div>,
@     <ARG 0, <MINUS, <CONST 25000>, <LOADW, <LOCAL 40>>>>,
@     <ARG 1, <CONST 2>>>>
@ <CALL 1, <GLOBAL print_num>, <ARG 0, <TEMP 2>>>
@ <JUMP L315>
@ <LABEL L327>
@           print_char('L'); print_num((t - hp) div TERM_SIZE)
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 76>>>
@ <DEFTEMP 3,
@   <CALL 2,
@     <GLOBAL int_div>,
@     <ARG 0, <MINUS, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _hp>>>>,
@     <ARG 1, <CONST 2>>>>
@ <CALL 1, <GLOBAL print_num>, <ARG 0, <TEMP 3>>>
@ <JUMP L315>
@ <LABEL L325>
@         print_char('@'); print_num(mem[t+1])
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 64>>>
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L315>
@ <LABEL L319>
@       print_string("*unknown-term(tag="); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g34>>,
@   <ARG 1, <CONST 18>>>
@       print_num(lsr(mem[t], 8)); print_string(")*")
@ <CALL 1,
@   <GLOBAL print_num>,
@   <ARG 0,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g35>>,
@   <ARG 1, <CONST 2>>>
@ <LABEL L315>

_PrintTerm:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t := Deref(t, e);
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 1>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@   if t = NULL then
@ <JNEQ L317, <TEMP 1>, <CONST 0>>
	cmp r0, #0
	bne .L317
@     print_string("*null-term*")
@ <ARG 1, <CONST 11>>
	set r1, #11
@ <ARG 0, <GLOBAL g33>>
	set r0, g33
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L315>
	b .L315
@ <LABEL L317>
.L317:
@     case lsr(mem[t], 8) of
@ <JCASE 5 L319,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #5
	ldrlo pc, [pc, r0, LSL #2]
	b .L319
	.word .L321
	.word .L322
	.word .L323
	.word .L324
	.word .L325
@ <LABEL L321>
.L321:
@         PrintCompound(t, e, prio)
@ <ARG 2, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r2, [r0]
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintCompound>>
	bl _PrintCompound
@ <JUMP L315>
	b .L315
@ <LABEL L322>
.L322:
@         print_num(mem[t+1])
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
@ <CALL 1, <GLOBAL print_num>>
	bl print_num
@ <JUMP L315>
	b .L315
@ <LABEL L323>
.L323:
@         print_char(''''); print_char(chr(mem[t+1])); print_char('''')
@ <ARG 0, <CONST 39>>
	set r0, #39
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <ARG 0, <CONST 39>>
	set r0, #39
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <JUMP L315>
	b .L315
@ <LABEL L324>
.L324:
@         if (t >= gsp) then
@ <JLT L327, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _gsp>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _gsp
	ldr r1, [r1]
	cmp r0, r1
	blt .L327
@           print_char('G'); print_num((MEMSIZE - t) div TERM_SIZE)
@ <ARG 0, <CONST 71>>
	set r0, #71
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <MINUS, <CONST 25000>, <LOADW, <LOCAL 40>>>>
	set r0, #25000
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	sub r0, r0, r2
@ <DEFTEMP 2, <CALL 2, <GLOBAL int_div>>>
	bl int_div
@ <ARG 0, <TEMP 2>>
@ <CALL 1, <GLOBAL print_num>>
	bl print_num
@ <JUMP L315>
	b .L315
@ <LABEL L327>
.L327:
@           print_char('L'); print_num((t - hp) div TERM_SIZE)
@ <ARG 0, <CONST 76>>
	set r0, #76
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <MINUS, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _hp>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r2, _hp
	ldr r2, [r2]
	sub r0, r0, r2
@ <DEFTEMP 3, <CALL 2, <GLOBAL int_div>>>
	bl int_div
@ <ARG 0, <TEMP 3>>
@ <CALL 1, <GLOBAL print_num>>
	bl print_num
@ <JUMP L315>
	b .L315
@ <LABEL L325>
.L325:
@         print_char('@'); print_num(mem[t+1])
@ <ARG 0, <CONST 64>>
	set r0, #64
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
@ <CALL 1, <GLOBAL print_num>>
	bl print_num
@ <JUMP L315>
	b .L315
@ <LABEL L319>
.L319:
@       print_string("*unknown-term(tag="); 
@ <ARG 1, <CONST 18>>
	set r1, #18
@ <ARG 0, <GLOBAL g34>>
	set r0, g34
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@       print_num(lsr(mem[t], 8)); print_string(")*")
@ <ARG 0,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
@ <CALL 1, <GLOBAL print_num>>
	bl print_num
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g35>>
	set r0, g35
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <LABEL L315>
.L315:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PrintClause(c: clause);
@ Initial code:
@   if c = NULL then
@ <JEQ L330, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 0>>
@ <JUMP L331>
@ <LABEL L330>
@     print_string("*null-clause*"); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g36>>,
@   <ARG 1, <CONST 13>>>
@ <CALL 0, <GLOBAL newline>>
@   else
@ <JUMP L332>
@ <LABEL L331>
@     if mem[c+3] <> NULL then
@ <JNEQ L333,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 3>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L334>
@ <LABEL L333>
@       PrintTerm(mem[c+3], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 3>>,
@           <CONST 4>>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@       print_char(' ')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 32>>>
@ <JUMP L335>
@ <LABEL L334>
@ <LABEL L335>
@     print_string(":- ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g37>>,
@   <ARG 1, <CONST 3>>>
@     if mem[(c+4)+1-1] <> NULL then
@ <JNEQ L336,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <MINUS,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>,
@             <CONST 1>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L337>
@ <LABEL L336>
@       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <MINUS,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 4>>,
@               <CONST 1>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@       i := 2;
@ <STOREW, <CONST 2>, <REGVAR 0>>
@       while mem[(c+4)+i-1] <> NULL do
@ <LABEL L339>
@ <JNEQ L340,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <MINUS,
@           <PLUS,
@             <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>,
@             <LOADW, <REGVAR 0>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L341>
@ <LABEL L340>
@ 	print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g38>>,
@   <ARG 1, <CONST 2>>>
@ 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <MINUS,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 4>>,
@               <LOADW, <REGVAR 0>>>,
@             <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@ 	i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L339>
@ <LABEL L341>
@ <JUMP L338>
@ <LABEL L337>
@ <LABEL L338>
@     print_char('.'); newline()
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 46>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L332>
@ <LABEL L329>

@ After simplification:
@   if c = NULL then
@ <JNEQ L331, <LOADW, <LOCAL 40>>, <CONST 0>>
@     print_string("*null-clause*"); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g36>>,
@   <ARG 1, <CONST 13>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L329>
@ <LABEL L331>
@     if mem[c+3] <> NULL then
@ <JEQ L335,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@       PrintTerm(mem[c+3], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 12>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@       print_char(' ')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 32>>>
@ <LABEL L335>
@     print_string(":- ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g37>>,
@   <ARG 1, <CONST 3>>>
@     if mem[(c+4)+1-1] <> NULL then
@ <JEQ L338,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 16>>>,
@   <CONST 0>>
@       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 16>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@       i := 2;
@ <STOREW, <CONST 2>, <REGVAR 0>>
@ <LABEL L339>
@       while mem[(c+4)+i-1] <> NULL do
@ <JEQ L338,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>,
@             <LOADW, <REGVAR 0>>>,
@           <CONST 2>>>,
@       <CONST -4>>>,
@   <CONST 0>>
@ 	print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g38>>,
@   <ARG 1, <CONST 2>>>
@ 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL,
@             <PLUS,
@               <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>,
@               <LOADW, <REGVAR 0>>>,
@             <CONST 2>>>,
@         <CONST -4>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@ 	i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L339>
@ <LABEL L338>
@     print_char('.'); newline()
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 46>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L329>

@ After sharing:
@   if c = NULL then
@ <JNEQ L331, <LOADW, <LOCAL 40>>, <CONST 0>>
@     print_string("*null-clause*"); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g36>>,
@   <ARG 1, <CONST 13>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L329>
@ <LABEL L331>
@     if mem[c+3] <> NULL then
@ <DEFTEMP 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
@ <JEQ L335, <TEMP 1>, <CONST 0>>
@       PrintTerm(mem[c+3], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <TEMP 1>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@       print_char(' ')
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 32>>>
@ <LABEL L335>
@     print_string(":- ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g37>>,
@   <ARG 1, <CONST 3>>>
@     if mem[(c+4)+1-1] <> NULL then
@ <DEFTEMP 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 16>>>>
@ <JEQ L338, <TEMP 2>, <CONST 0>>
@       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <TEMP 2>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@       i := 2;
@ <STOREW, <CONST 2>, <REGVAR 0>>
@ <LABEL L339>
@       while mem[(c+4)+i-1] <> NULL do
@ <DEFTEMP 3, <GLOBAL _mem>>
@ <JEQ L338,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 3>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>,
@             <LOADW, <REGVAR 0>>>,
@           <CONST 2>>>,
@       <CONST -4>>>,
@   <CONST 0>>
@ 	print_string(", ");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g38>>,
@   <ARG 1, <CONST 2>>>
@ 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <TEMP 3>,
@           <LSL,
@             <PLUS,
@               <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>,
@               <LOADW, <REGVAR 0>>>,
@             <CONST 2>>>,
@         <CONST -4>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 2>>>
@ 	i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L339>
@ <LABEL L338>
@     print_char('.'); newline()
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 46>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L329>

_PrintClause:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if c = NULL then
@ <JNEQ L331, <LOADW, <LOCAL 40>>, <CONST 0>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #0
	bne .L331
@     print_string("*null-clause*"); newline();
@ <ARG 1, <CONST 13>>
	set r1, #13
@ <ARG 0, <GLOBAL g36>>
	set r0, g36
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <JUMP L329>
	b .L329
@ <LABEL L331>
.L331:
@     if mem[c+3] <> NULL then
@ <DEFTEMP 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 12>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #12
	ldr r5, [r0]
@ <JEQ L335, <TEMP 1>, <CONST 0>>
	cmp r5, #0
	beq .L335
@       PrintTerm(mem[c+3], NULL, MAXPRIO);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <TEMP 1>>
	mov r0, r5
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@       print_char(' ')
@ <ARG 0, <CONST 32>>
	set r0, #32
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <LABEL L335>
.L335:
@     print_string(":- ");
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g37>>
	set r0, g37
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     if mem[(c+4)+1-1] <> NULL then
@ <DEFTEMP 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 16>>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #16
	ldr r5, [r0]
@ <JEQ L338, <TEMP 2>, <CONST 0>>
	cmp r5, #0
	beq .L338
@       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <TEMP 2>>
	mov r0, r5
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@       i := 2;
@ <STOREW, <CONST 2>, <REGVAR 0>>
	set r4, #2
@ <LABEL L339>
.L339:
@       while mem[(c+4)+i-1] <> NULL do
@ <DEFTEMP 3, <GLOBAL _mem>>
	set r5, _mem
@ <JEQ L338,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 3>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>,
@             <LOADW, <REGVAR 0>>>,
@           <CONST 2>>>,
@       <CONST -4>>>,
@   <CONST 0>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, #4
	add r0, r0, r4
	lsl r0, r0, #2
	add r0, r5, r0
	set r1, #-4
	add r0, r0, r1
	ldr r0, [r0]
	cmp r0, #0
	beq .L338
@ 	print_string(", ");
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g38>>
	set r0, g38
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 3>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>,
@             <LOADW, <REGVAR 0>>>,
@           <CONST 2>>>,
@       <CONST -4>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, #4
	add r0, r0, r4
	lsl r0, r0, #2
	add r0, r5, r0
	set r3, #-4
	add r0, r0, r3
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@ 	i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L339>
	b .L339
@ <LABEL L338>
.L338:
@     print_char('.'); newline()
@ <ARG 0, <CONST 46>>
	set r0, #46
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <LABEL L329>
.L329:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ShowError();
@ Initial code:
@   errflag := true; errcount := errcount+1;
@ <STOREC, <CONST 1>, <GLOBAL _errflag>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _errcount>>, <CONST 1>>,
@   <GLOBAL _errcount>>
@   print_string("Line "); print_num(lineno); print_char(' ');
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g39>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 1, <GLOBAL print_num>, <ARG 0, <LOADW, <GLOBAL _lineno>>>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 32>>>
@   print_string("Syntax error - ")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g40>>,
@   <ARG 1, <CONST 15>>>
@ <LABEL L342>

@ After simplification:
@   errflag := true; errcount := errcount+1;
@ <STOREC, <CONST 1>, <GLOBAL _errflag>>
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _errcount>>, <CONST 1>>,
@   <GLOBAL _errcount>>
@   print_string("Line "); print_num(lineno); print_char(' ');
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g39>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 1, <GLOBAL print_num>, <ARG 0, <LOADW, <GLOBAL _lineno>>>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 32>>>
@   print_string("Syntax error - ")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g40>>,
@   <ARG 1, <CONST 15>>>

@ After sharing:
@   errflag := true; errcount := errcount+1;
@ <STOREC, <CONST 1>, <GLOBAL _errflag>>
@ <DEFTEMP 1, <GLOBAL _errcount>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@   print_string("Line "); print_num(lineno); print_char(' ');
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g39>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 1, <GLOBAL print_num>, <ARG 0, <LOADW, <GLOBAL _lineno>>>>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <CONST 32>>>
@   print_string("Syntax error - ")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g40>>,
@   <ARG 1, <CONST 15>>>

_ShowError:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   errflag := true; errcount := errcount+1;
@ <STOREC, <CONST 1>, <GLOBAL _errflag>>
	set r0, #1
	set r1, _errflag
	strb r0, [r1]
@ <DEFTEMP 1, <GLOBAL _errcount>>
	set r4, _errcount
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   print_string("Line "); print_num(lineno); print_char(' ');
@ <ARG 1, <CONST 5>>
	set r1, #5
@ <ARG 0, <GLOBAL g39>>
	set r0, g39
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 0, <LOADW, <GLOBAL _lineno>>>
	set r0, _lineno
	ldr r0, [r0]
@ <CALL 1, <GLOBAL print_num>>
	bl print_num
@ <ARG 0, <CONST 32>>
	set r0, #32
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@   print_string("Syntax error - ")
@ <ARG 1, <CONST 15>>
	set r1, #15
@ <ARG 0, <GLOBAL g40>>
	set r0, g40
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Recover();
@ Initial code:
@   if errcount >= 20 then
@ <JGEQ L344, <LOADW, <GLOBAL _errcount>>, <CONST 20>>
@ <JUMP L345>
@ <LABEL L344>
@     print_string("Too many errors: I am giving up"); newline(); exit(2) 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g41>>,
@   <ARG 1, <CONST 31>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L346>
@ <LABEL L345>
@ <LABEL L346>
@   if token <> DOT then
@ <JNEQ L347, <LOADW, <GLOBAL _token>>, <CONST 10>>
@ <JUMP L348>
@ <LABEL L347>
@     repeat
@ <LABEL L350>
@       ch := GetChar()
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <JEQ L351, <LOADC, <REGVAR 0>>, <CONST 46>>
@ <JUMP L352>
@ <LABEL L352>
@ <JEQ L351, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <JUMP L350>
@ <LABEL L351>
@     token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
@ <JUMP L349>
@ <LABEL L348>
@ <LABEL L349>
@ <LABEL L343>

@ After simplification:
@   if errcount >= 20 then
@ <JLT L346, <LOADW, <GLOBAL _errcount>>, <CONST 20>>
@     print_string("Too many errors: I am giving up"); newline(); exit(2) 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g41>>,
@   <ARG 1, <CONST 31>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L346>
@   if token <> DOT then
@ <JEQ L343, <LOADW, <GLOBAL _token>>, <CONST 10>>
@ <LABEL L350>
@       ch := GetChar()
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <JEQ L351, <LOADC, <REGVAR 0>>, <CONST 46>>
@ <JNEQ L350, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <LABEL L351>
@     token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
@ <LABEL L343>

@ After sharing:
@   if errcount >= 20 then
@ <JLT L346, <LOADW, <GLOBAL _errcount>>, <CONST 20>>
@     print_string("Too many errors: I am giving up"); newline(); exit(2) 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g41>>,
@   <ARG 1, <CONST 31>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L346>
@   if token <> DOT then
@ <JEQ L343, <LOADW, <GLOBAL _token>>, <CONST 10>>
@ <LABEL L350>
@       ch := GetChar()
@ <DEFTEMP 1, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 1>, <REGVAR 0>>
@ <JEQ L351, <LOADC, <REGVAR 0>>, <CONST 46>>
@ <JNEQ L350, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <LABEL L351>
@     token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
@ <LABEL L343>

_Recover:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if errcount >= 20 then
@ <JLT L346, <LOADW, <GLOBAL _errcount>>, <CONST 20>>
	set r0, _errcount
	ldr r0, [r0]
	cmp r0, #20
	blt .L346
@     print_string("Too many errors: I am giving up"); newline(); exit(2) 
@ <ARG 1, <CONST 31>>
	set r1, #31
@ <ARG 0, <GLOBAL g41>>
	set r0, g41
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L346>
.L346:
@   if token <> DOT then
@ <JEQ L343, <LOADW, <GLOBAL _token>>, <CONST 10>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #10
	beq .L343
@ <LABEL L350>
.L350:
@       ch := GetChar()
@ <DEFTEMP 1, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@ <JEQ L351, <LOADC, <REGVAR 0>>, <CONST 46>>
	cmp r4, #46
	beq .L351
@ <JNEQ L350, <LOADC, <REGVAR 0>>, <CONST 127>>
	cmp r4, #127
	bne .L350
@ <LABEL L351>
.L351:
@     token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
	set r0, #10
	set r1, _token
	str r0, [r1]
@ <LABEL L343>
.L343:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Scan();
@ Initial code:
@   ch := GetChar(); token := 0;
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <STOREW, <CONST 0>, <GLOBAL _token>>
@   while token = 0 do
@ <LABEL L354>
@ <JEQ L355, <LOADW, <GLOBAL _token>>, <CONST 0>>
@ <JUMP L356>
@ <LABEL L355>
@     if ch = ENDFILE then
@ <JEQ L357, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <JUMP L358>
@ <LABEL L357>
@       token := EOFTOK
@ <STOREW, <CONST 14>, <GLOBAL _token>>
@ <JUMP L359>
@ <LABEL L358>
@     elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 32>>
@ <JUMP L453>
@ <LABEL L453>
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 9>>
@ <JUMP L452>
@ <LABEL L452>
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 10>>
@ <JUMP L361>
@ <LABEL L360>
@       ch := GetChar()
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L362>
@ <LABEL L361>
@     elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
@ <JGEQ L451, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JUMP L450>
@ <LABEL L451>
@ <JLEQ L363, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <JUMP L450>
@ <LABEL L450>
@ <JEQ L363, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JUMP L448>
@ <LABEL L448>
@ <JGEQ L449, <LOADC, <REGVAR 0>>, <CONST 97>>
@ <JUMP L364>
@ <LABEL L449>
@ <JLEQ L363, <LOADC, <REGVAR 0>>, <CONST 122>>
@ <JUMP L364>
@ <LABEL L363>
@       if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
@ <JGEQ L432, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JUMP L431>
@ <LABEL L432>
@ <JLEQ L428, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <JUMP L431>
@ <LABEL L431>
@ <JEQ L428, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JUMP L429>
@ <LABEL L428>
@ 	 token := VARIABLE
@ <STOREW, <CONST 2>, <GLOBAL _token>>
@ <JUMP L430>
@ <LABEL L429>
@ 	 token := IDENT
@ <STOREW, <CONST 1>, <GLOBAL _token>>
@ <LABEL L430>
@       i := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
@       while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
@ <LABEL L433>
@ <JGEQ L444, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JUMP L443>
@ <LABEL L444>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <JUMP L443>
@ <LABEL L443>
@ <JEQ L434, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JUMP L441>
@ <LABEL L441>
@ <JGEQ L442, <LOADC, <REGVAR 0>>, <CONST 97>>
@ <JUMP L439>
@ <LABEL L442>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 122>>
@ <JUMP L439>
@ <LABEL L439>
@ <JGEQ L440, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JUMP L435>
@ <LABEL L440>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 57>>
@ <JUMP L435>
@ <LABEL L434>
@         if i > MAXSTRING then
@ <JGT L436, <LOADW, <REGVAR 2>>, <CONST 128>>
@ <JUMP L437>
@ <LABEL L436>
@           newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g42>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g43>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L438>
@ <LABEL L437>
@ <LABEL L438>
@         toksval[i] := ch; ch := GetChar(); i := i+1
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <TIMES, <LOADW, <REGVAR 2>>, <CONST 1>>>>
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <JUMP L433>
@ <LABEL L435>
@       PushBack(ch);
@ <CALL 1,
@   <GLOBAL _PushBack>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADC, <REGVAR 0>>>>
@       toksval[i] := ENDSTR; tokval := Lookup(toksval);
@ <STOREC,
@   <CONST 0>,
@   <OFFSET, <GLOBAL _toksval>, <TIMES, <LOADW, <REGVAR 2>>, <CONST 1>>>>
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _Lookup>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL _toksval>>>,
@   <GLOBAL _tokval>>
@       if tokval = notsym then token := NEGATE end
@ <JEQ L445, <LOADW, <GLOBAL _tokval>>, <LOADW, <GLOBAL _notsym>>>
@ <JUMP L446>
@ <LABEL L445>
@ <STOREW, <CONST 13>, <GLOBAL _token>>
@ <JUMP L447>
@ <LABEL L446>
@ <LABEL L447>
@ <JUMP L365>
@ <LABEL L364>
@     elsif ((ch >= '0') and (ch <= '9')) then
@ <JGEQ L427, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JUMP L367>
@ <LABEL L427>
@ <JLEQ L366, <LOADC, <REGVAR 0>>, <CONST 57>>
@ <JUMP L367>
@ <LABEL L366>
@       token := NUMBER; tokival := 0;
@ <STOREW, <CONST 3>, <GLOBAL _token>>
@ <STOREW, <CONST 0>, <GLOBAL _tokival>>
@       while ((ch >= '0') and (ch <= '9')) do
@ <LABEL L423>
@ <JGEQ L426, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JUMP L425>
@ <LABEL L426>
@ <JLEQ L424, <LOADC, <REGVAR 0>>, <CONST 57>>
@ <JUMP L425>
@ <LABEL L424>
@         tokival := 10 * tokival + (ord(ch) - ord('0'));
@ <STOREW,
@   <PLUS,
@     <TIMES, <CONST 10>, <LOADW, <GLOBAL _tokival>>>,
@     <MINUS, <LOADC, <REGVAR 0>>, <CONST 48>>>,
@   <GLOBAL _tokival>>
@         ch := GetChar()
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L423>
@ <LABEL L425>
@       PushBack(ch)
@ <CALL 1,
@   <GLOBAL _PushBack>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADC, <REGVAR 0>>>>
@ <JUMP L368>
@ <LABEL L367>
@       case ch of
@ <JCASE 30 L369, <MINUS, <LOADC, <REGVAR 0>>, <CONST 33>>>
@ <LABEL L371>
@         '(': token := LPAR
@ <STOREW, <CONST 7>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L372>
@       | ')': token := RPAR
@ <STOREW, <CONST 8>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L373>
@       | ',': token := COMMA
@ <STOREW, <CONST 9>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L374>
@       | '.': token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L375>
@       | '=': token := EQUAL
@ <STOREW, <CONST 12>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L376>
@       | '<': token := LANGLE
@ <STOREW, <CONST 15>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L377>
@       | '>': token := RANGLE
@ <STOREW, <CONST 16>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L378>
@       | '#': token := HASH
@ <STOREW, <CONST 17>, <GLOBAL _token>>
@ <JUMP L370>
@ <LABEL L379>
@       | '!': token := IDENT; tokval := cutsym
@ <STOREW, <CONST 1>, <GLOBAL _token>>
@ <STOREW, <LOADW, <GLOBAL _cutsym>>, <GLOBAL _tokval>>
@ <JUMP L370>
@ <LABEL L380>
@ 	  ch := GetChar();
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ 	  if ch <> '*' then
@ <JNEQ L387, <LOADC, <REGVAR 0>>, <CONST 42>>
@ <JUMP L388>
@ <LABEL L387>
@ 	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
@ <JNEQ L402, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L401>
@ <LABEL L401>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g44>>,
@   <ARG 1, <CONST 11>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L403>
@ <LABEL L402>
@ <LABEL L403>
@ <JUMP L389>
@ <LABEL L388>
@ 	    ch2 := ' '; ch := GetChar();
@ <STOREC, <CONST 32>, <REGVAR 1>>
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ 	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
@ <LABEL L390>
@ <JNEQ L393, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <JUMP L392>
@ <LABEL L393>
@ <JEQ L394, <LOADC, <REGVAR 1>>, <CONST 42>>
@ <JUMP L391>
@ <LABEL L394>
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 47>>
@ <JUMP L391>
@ <LABEL L391>
@ 	      ch2 := ch; ch := GetChar() 
@ <STOREC, <LOADC, <REGVAR 0>>, <REGVAR 1>>
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L390>
@ <LABEL L392>
@ 	    if ch = ENDFILE then
@ <JEQ L395, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <JUMP L396>
@ <LABEL L395>
@ 	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
@ <JNEQ L399, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L398>
@ <LABEL L398>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g45>>,
@   <ARG 1, <CONST 22>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L400>
@ <LABEL L399>
@ <LABEL L400>
@ <JUMP L397>
@ <LABEL L396>
@ 	      ch := GetChar()
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <LABEL L397>
@ <LABEL L389>
@ <JUMP L370>
@ <LABEL L381>
@ 	  ch := GetChar();
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ 	  if ch = '-' then
@ <JEQ L404, <LOADC, <REGVAR 0>>, <CONST 45>>
@ <JUMP L405>
@ <LABEL L404>
@ 	    token := ARROW
@ <STOREW, <CONST 6>, <GLOBAL _token>>
@ <JUMP L406>
@ <LABEL L405>
@ 	    PushBack(ch); token := COLON 
@ <CALL 1,
@   <GLOBAL _PushBack>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADC, <REGVAR 0>>>>
@ <STOREW, <CONST 11>, <GLOBAL _token>>
@ <LABEL L406>
@ <JUMP L370>
@ <LABEL L382>
@ 	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
@ <STOREW, <CONST 4>, <GLOBAL _token>>
@ <STOREW,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <GLOBAL _tokival>>
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ 	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
@ <JNEQ L407, <LOADC, <REGVAR 0>>, <CONST 39>>
@ <JUMP L408>
@ <LABEL L407>
@ <JNEQ L411, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L410>
@ <LABEL L410>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g46>>,
@   <ARG 1, <CONST 13>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L412>
@ <LABEL L411>
@ <LABEL L412>
@ <JUMP L409>
@ <LABEL L408>
@ <LABEL L409>
@ <JUMP L370>
@ <LABEL L383>
@ 	  token := STRCON; i := 0; ch := GetChar();
@ <STOREW, <CONST 5>, <GLOBAL _token>>
@ <STOREW, <CONST 0>, <REGVAR 2>>
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ 	  while (ch <> '"') and (ch <> ENDLINE) do
@ <LABEL L413>
@ <JNEQ L416, <LOADC, <REGVAR 0>>, <CONST 34>>
@ <JUMP L415>
@ <LABEL L416>
@ <JNEQ L414, <LOADC, <REGVAR 0>>, <CONST 10>>
@ <JUMP L415>
@ <LABEL L414>
@ 	    toksval[i] := ch; ch := GetChar(); i := i+1 
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <TIMES, <LOADW, <REGVAR 2>>, <CONST 1>>>>
@ <STOREC,
@   <CALL 0, <GLOBAL _GetChar>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <JUMP L413>
@ <LABEL L415>
@ 	  toksval[i] := ENDSTR;
@ <STOREC,
@   <CONST 0>,
@   <OFFSET, <GLOBAL _toksval>, <TIMES, <LOADW, <REGVAR 2>>, <CONST 1>>>>
@ 	  if ch = ENDLINE then
@ <JEQ L417, <LOADC, <REGVAR 0>>, <CONST 10>>
@ <JUMP L418>
@ <LABEL L417>
@ 	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
@ <JNEQ L421, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L420>
@ <LABEL L420>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g47>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L422>
@ <LABEL L421>
@ <LABEL L422>
@ 	    PushBack(ch)
@ <CALL 1,
@   <GLOBAL _PushBack>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADC, <REGVAR 0>>>>
@ <JUMP L419>
@ <LABEL L418>
@ <LABEL L419>
@ <JUMP L370>
@ <LABEL L369>
@ 	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
@ <JNEQ L385, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L384>
@ <LABEL L384>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g48>>,
@   <ARG 1, <CONST 17>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L386>
@ <LABEL L385>
@ <LABEL L386>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L370>
@ <LABEL L368>
@ <LABEL L365>
@ <LABEL L362>
@ <LABEL L359>
@ <JUMP L354>
@ <LABEL L356>
@ <LABEL L353>

@ After simplification:
@   ch := GetChar(); token := 0;
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <GLOBAL _token>>
@ <LABEL L354>
@   while token = 0 do
@ <JNEQ L353, <LOADW, <GLOBAL _token>>, <CONST 0>>
@     if ch = ENDFILE then
@ <JNEQ L358, <LOADC, <REGVAR 0>>, <CONST 127>>
@       token := EOFTOK
@ <STOREW, <CONST 14>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L358>
@     elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 32>>
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 9>>
@ <JNEQ L361, <LOADC, <REGVAR 0>>, <CONST 10>>
@ <LABEL L360>
@       ch := GetChar()
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <JUMP L354>
@ <LABEL L361>
@     elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
@ <JLT L450, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JLEQ L363, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <LABEL L450>
@ <JEQ L363, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JLT L364, <LOADC, <REGVAR 0>>, <CONST 97>>
@ <JGT L364, <LOADC, <REGVAR 0>>, <CONST 122>>
@ <LABEL L363>
@       if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
@ <JLT L431, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JLEQ L428, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <LABEL L431>
@ <JNEQ L429, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <LABEL L428>
@ 	 token := VARIABLE
@ <STOREW, <CONST 2>, <GLOBAL _token>>
@ <JUMP L430>
@ <LABEL L429>
@ 	 token := IDENT
@ <STOREW, <CONST 1>, <GLOBAL _token>>
@ <LABEL L430>
@       i := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
@ <LABEL L433>
@       while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
@ <JLT L443, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <LABEL L443>
@ <JEQ L434, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JLT L439, <LOADC, <REGVAR 0>>, <CONST 97>>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 122>>
@ <LABEL L439>
@ <JLT L435, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JGT L435, <LOADC, <REGVAR 0>>, <CONST 57>>
@ <LABEL L434>
@         if i > MAXSTRING then
@ <JLEQ L438, <LOADW, <REGVAR 2>>, <CONST 128>>
@           newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g42>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g43>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L438>
@         toksval[i] := ch; ch := GetChar(); i := i+1
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <JUMP L433>
@ <LABEL L435>
@       PushBack(ch);
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@       toksval[i] := ENDSTR; tokval := Lookup(toksval);
@ <STOREC, <CONST 0>, <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ <STOREW,
@   <CALL 1, <GLOBAL _Lookup>, <ARG 0, <GLOBAL _toksval>>>,
@   <GLOBAL _tokval>>
@       if tokval = notsym then token := NEGATE end
@ <JNEQ L354, <LOADW, <GLOBAL _tokval>>, <LOADW, <GLOBAL _notsym>>>
@ <STOREW, <CONST 13>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L364>
@     elsif ((ch >= '0') and (ch <= '9')) then
@ <JLT L367, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JGT L367, <LOADC, <REGVAR 0>>, <CONST 57>>
@       token := NUMBER; tokival := 0;
@ <STOREW, <CONST 3>, <GLOBAL _token>>
@ <STOREW, <CONST 0>, <GLOBAL _tokival>>
@ <LABEL L423>
@       while ((ch >= '0') and (ch <= '9')) do
@ <JLT L425, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JGT L425, <LOADC, <REGVAR 0>>, <CONST 57>>
@         tokival := 10 * tokival + (ord(ch) - ord('0'));
@ <STOREW,
@   <PLUS,
@     <TIMES, <LOADW, <GLOBAL _tokival>>, <CONST 10>>,
@     <MINUS, <LOADC, <REGVAR 0>>, <CONST 48>>>,
@   <GLOBAL _tokival>>
@         ch := GetChar()
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <JUMP L423>
@ <LABEL L425>
@       PushBack(ch)
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <JUMP L354>
@ <LABEL L367>
@       case ch of
@ <JCASE 30 L369, <MINUS, <LOADC, <REGVAR 0>>, <CONST 33>>>
@ <LABEL L371>
@         '(': token := LPAR
@ <STOREW, <CONST 7>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L372>
@       | ')': token := RPAR
@ <STOREW, <CONST 8>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L373>
@       | ',': token := COMMA
@ <STOREW, <CONST 9>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L374>
@       | '.': token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L375>
@       | '=': token := EQUAL
@ <STOREW, <CONST 12>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L376>
@       | '<': token := LANGLE
@ <STOREW, <CONST 15>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L377>
@       | '>': token := RANGLE
@ <STOREW, <CONST 16>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L378>
@       | '#': token := HASH
@ <STOREW, <CONST 17>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L379>
@       | '!': token := IDENT; tokval := cutsym
@ <STOREW, <CONST 1>, <GLOBAL _token>>
@ <STOREW, <LOADW, <GLOBAL _cutsym>>, <GLOBAL _tokval>>
@ <JUMP L354>
@ <LABEL L380>
@ 	  ch := GetChar();
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ 	  if ch <> '*' then
@ <JEQ L388, <LOADC, <REGVAR 0>>, <CONST 42>>
@ 	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g44>>,
@   <ARG 1, <CONST 11>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <JUMP L354>
@ <LABEL L388>
@ 	    ch2 := ' '; ch := GetChar();
@ <STOREC, <CONST 32>, <REGVAR 1>>
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <LABEL L390>
@ 	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <JNEQ L391, <LOADC, <REGVAR 1>>, <CONST 42>>
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 47>>
@ <LABEL L391>
@ 	      ch2 := ch; ch := GetChar() 
@ <STOREC, <LOADC, <REGVAR 0>>, <REGVAR 1>>
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <JUMP L390>
@ <LABEL L392>
@ 	    if ch = ENDFILE then
@ <JNEQ L396, <LOADC, <REGVAR 0>>, <CONST 127>>
@ 	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g45>>,
@   <ARG 1, <CONST 22>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <JUMP L354>
@ <LABEL L396>
@ 	      ch := GetChar()
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <JUMP L354>
@ <LABEL L381>
@ 	  ch := GetChar();
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ 	  if ch = '-' then
@ <JNEQ L405, <LOADC, <REGVAR 0>>, <CONST 45>>
@ 	    token := ARROW
@ <STOREW, <CONST 6>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L405>
@ 	    PushBack(ch); token := COLON 
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <STOREW, <CONST 11>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L382>
@ 	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
@ <STOREW, <CONST 4>, <GLOBAL _token>>
@ <STOREW, <CALL 0, <GLOBAL _GetChar>>, <GLOBAL _tokival>>
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ 	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
@ <JEQ L354, <LOADC, <REGVAR 0>>, <CONST 39>>
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g46>>,
@   <ARG 1, <CONST 13>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <JUMP L354>
@ <LABEL L383>
@ 	  token := STRCON; i := 0; ch := GetChar();
@ <STOREW, <CONST 5>, <GLOBAL _token>>
@ <STOREW, <CONST 0>, <REGVAR 2>>
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <LABEL L413>
@ 	  while (ch <> '"') and (ch <> ENDLINE) do
@ <JEQ L415, <LOADC, <REGVAR 0>>, <CONST 34>>
@ <JEQ L415, <LOADC, <REGVAR 0>>, <CONST 10>>
@ 	    toksval[i] := ch; ch := GetChar(); i := i+1 
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ <STOREC, <CALL 0, <GLOBAL _GetChar>>, <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <JUMP L413>
@ <LABEL L415>
@ 	  toksval[i] := ENDSTR;
@ <STOREC, <CONST 0>, <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ 	  if ch = ENDLINE then
@ <JNEQ L354, <LOADC, <REGVAR 0>>, <CONST 10>>
@ 	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
@ <JNEQ L422, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g47>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L422>
@ 	    PushBack(ch)
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <JUMP L354>
@ <LABEL L369>
@ 	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
@ <JNEQ L386, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g48>>,
@   <ARG 1, <CONST 17>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L386>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L354>
@ <LABEL L353>

@ After sharing:
@   ch := GetChar(); token := 0;
@ <DEFTEMP 1, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 1>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <GLOBAL _token>>
@ <LABEL L354>
@   while token = 0 do
@ <DEFTEMP 2, <GLOBAL _token>>
@ <JNEQ L353, <LOADW, <TEMP 2>>, <CONST 0>>
@     if ch = ENDFILE then
@ <JNEQ L358, <LOADC, <REGVAR 0>>, <CONST 127>>
@       token := EOFTOK
@ <STOREW, <CONST 14>, <TEMP 2>>
@ <JUMP L354>
@ <LABEL L358>
@     elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 32>>
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 9>>
@ <JNEQ L361, <LOADC, <REGVAR 0>>, <CONST 10>>
@ <LABEL L360>
@       ch := GetChar()
@ <DEFTEMP 3, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 3>, <REGVAR 0>>
@ <JUMP L354>
@ <LABEL L361>
@     elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
@ <JLT L450, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JLEQ L363, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <LABEL L450>
@ <JEQ L363, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JLT L364, <LOADC, <REGVAR 0>>, <CONST 97>>
@ <JGT L364, <LOADC, <REGVAR 0>>, <CONST 122>>
@ <LABEL L363>
@       if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
@ <JLT L431, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JLEQ L428, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <LABEL L431>
@ <JNEQ L429, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <LABEL L428>
@ 	 token := VARIABLE
@ <STOREW, <CONST 2>, <GLOBAL _token>>
@ <JUMP L430>
@ <LABEL L429>
@ 	 token := IDENT
@ <STOREW, <CONST 1>, <GLOBAL _token>>
@ <LABEL L430>
@       i := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
@ <LABEL L433>
@       while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
@ <JLT L443, <LOADC, <REGVAR 0>>, <CONST 65>>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 90>>
@ <LABEL L443>
@ <JEQ L434, <LOADC, <REGVAR 0>>, <CONST 95>>
@ <JLT L439, <LOADC, <REGVAR 0>>, <CONST 97>>
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 122>>
@ <LABEL L439>
@ <JLT L435, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JGT L435, <LOADC, <REGVAR 0>>, <CONST 57>>
@ <LABEL L434>
@         if i > MAXSTRING then
@ <JLEQ L438, <LOADW, <REGVAR 2>>, <CONST 128>>
@           newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g42>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g43>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L438>
@         toksval[i] := ch; ch := GetChar(); i := i+1
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ <DEFTEMP 4, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 4>, <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <JUMP L433>
@ <LABEL L435>
@       PushBack(ch);
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@       toksval[i] := ENDSTR; tokval := Lookup(toksval);
@ <DEFTEMP 5, <GLOBAL _toksval>>
@ <STOREC, <CONST 0>, <OFFSET, <TEMP 5>, <LOADW, <REGVAR 2>>>>
@ <DEFTEMP 6, <CALL 1, <GLOBAL _Lookup>, <ARG 0, <TEMP 5>>>>
@ <STOREW, <TEMP 6>, <GLOBAL _tokval>>
@       if tokval = notsym then token := NEGATE end
@ <JNEQ L354, <TEMP 6>, <LOADW, <GLOBAL _notsym>>>
@ <STOREW, <CONST 13>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L364>
@     elsif ((ch >= '0') and (ch <= '9')) then
@ <JLT L367, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JGT L367, <LOADC, <REGVAR 0>>, <CONST 57>>
@       token := NUMBER; tokival := 0;
@ <STOREW, <CONST 3>, <GLOBAL _token>>
@ <STOREW, <CONST 0>, <GLOBAL _tokival>>
@ <LABEL L423>
@       while ((ch >= '0') and (ch <= '9')) do
@ <JLT L425, <LOADC, <REGVAR 0>>, <CONST 48>>
@ <JGT L425, <LOADC, <REGVAR 0>>, <CONST 57>>
@         tokival := 10 * tokival + (ord(ch) - ord('0'));
@ <DEFTEMP 7, <GLOBAL _tokival>>
@ <STOREW,
@   <PLUS,
@     <TIMES, <LOADW, <TEMP 7>>, <CONST 10>>,
@     <MINUS, <LOADC, <REGVAR 0>>, <CONST 48>>>,
@   <TEMP 7>>
@         ch := GetChar()
@ <DEFTEMP 8, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 8>, <REGVAR 0>>
@ <JUMP L423>
@ <LABEL L425>
@       PushBack(ch)
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <JUMP L354>
@ <LABEL L367>
@       case ch of
@ <JCASE 30 L369, <MINUS, <LOADC, <REGVAR 0>>, <CONST 33>>>
@ <LABEL L371>
@         '(': token := LPAR
@ <STOREW, <CONST 7>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L372>
@       | ')': token := RPAR
@ <STOREW, <CONST 8>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L373>
@       | ',': token := COMMA
@ <STOREW, <CONST 9>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L374>
@       | '.': token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L375>
@       | '=': token := EQUAL
@ <STOREW, <CONST 12>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L376>
@       | '<': token := LANGLE
@ <STOREW, <CONST 15>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L377>
@       | '>': token := RANGLE
@ <STOREW, <CONST 16>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L378>
@       | '#': token := HASH
@ <STOREW, <CONST 17>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L379>
@       | '!': token := IDENT; tokval := cutsym
@ <STOREW, <CONST 1>, <GLOBAL _token>>
@ <STOREW, <LOADW, <GLOBAL _cutsym>>, <GLOBAL _tokval>>
@ <JUMP L354>
@ <LABEL L380>
@ 	  ch := GetChar();
@ <DEFTEMP 9, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 9>, <REGVAR 0>>
@ 	  if ch <> '*' then
@ <JEQ L388, <LOADC, <REGVAR 0>>, <CONST 42>>
@ 	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g44>>,
@   <ARG 1, <CONST 11>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <JUMP L354>
@ <LABEL L388>
@ 	    ch2 := ' '; ch := GetChar();
@ <STOREC, <CONST 32>, <REGVAR 1>>
@ <DEFTEMP 10, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 10>, <REGVAR 0>>
@ <LABEL L390>
@ 	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 127>>
@ <JNEQ L391, <LOADC, <REGVAR 1>>, <CONST 42>>
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 47>>
@ <LABEL L391>
@ 	      ch2 := ch; ch := GetChar() 
@ <STOREC, <LOADC, <REGVAR 0>>, <REGVAR 1>>
@ <DEFTEMP 11, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 11>, <REGVAR 0>>
@ <JUMP L390>
@ <LABEL L392>
@ 	    if ch = ENDFILE then
@ <JNEQ L396, <LOADC, <REGVAR 0>>, <CONST 127>>
@ 	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g45>>,
@   <ARG 1, <CONST 22>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <JUMP L354>
@ <LABEL L396>
@ 	      ch := GetChar()
@ <DEFTEMP 12, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 12>, <REGVAR 0>>
@ <JUMP L354>
@ <LABEL L381>
@ 	  ch := GetChar();
@ <DEFTEMP 13, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 13>, <REGVAR 0>>
@ 	  if ch = '-' then
@ <JNEQ L405, <LOADC, <REGVAR 0>>, <CONST 45>>
@ 	    token := ARROW
@ <STOREW, <CONST 6>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L405>
@ 	    PushBack(ch); token := COLON 
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <STOREW, <CONST 11>, <GLOBAL _token>>
@ <JUMP L354>
@ <LABEL L382>
@ 	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
@ <STOREW, <CONST 4>, <GLOBAL _token>>
@ <DEFTEMP 14, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREW, <TEMP 14>, <GLOBAL _tokival>>
@ <DEFTEMP 15, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 15>, <REGVAR 0>>
@ 	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
@ <JEQ L354, <LOADC, <REGVAR 0>>, <CONST 39>>
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g46>>,
@   <ARG 1, <CONST 13>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <JUMP L354>
@ <LABEL L383>
@ 	  token := STRCON; i := 0; ch := GetChar();
@ <STOREW, <CONST 5>, <GLOBAL _token>>
@ <STOREW, <CONST 0>, <REGVAR 2>>
@ <DEFTEMP 16, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 16>, <REGVAR 0>>
@ <LABEL L413>
@ 	  while (ch <> '"') and (ch <> ENDLINE) do
@ <JEQ L415, <LOADC, <REGVAR 0>>, <CONST 34>>
@ <JEQ L415, <LOADC, <REGVAR 0>>, <CONST 10>>
@ 	    toksval[i] := ch; ch := GetChar(); i := i+1 
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ <DEFTEMP 17, <CALL 0, <GLOBAL _GetChar>>>
@ <STOREC, <TEMP 17>, <REGVAR 0>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <JUMP L413>
@ <LABEL L415>
@ 	  toksval[i] := ENDSTR;
@ <STOREC, <CONST 0>, <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
@ 	  if ch = ENDLINE then
@ <JNEQ L354, <LOADC, <REGVAR 0>>, <CONST 10>>
@ 	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
@ <JNEQ L422, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g47>>,
@   <ARG 1, <CONST 19>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L422>
@ 	    PushBack(ch)
@ <CALL 1, <GLOBAL _PushBack>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <JUMP L354>
@ <LABEL L369>
@ 	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
@ <JNEQ L386, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g48>>,
@   <ARG 1, <CONST 17>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L386>
@ <CALL 1, <GLOBAL print_char>, <ARG 0, <LOADC, <REGVAR 0>>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L354>
@ <LABEL L353>

_Scan:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   ch := GetChar(); token := 0;
@ <DEFTEMP 1, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@ <STOREW, <CONST 0>, <GLOBAL _token>>
	set r0, #0
	set r1, _token
	str r0, [r1]
@ <LABEL L354>
.L354:
@   while token = 0 do
@ <DEFTEMP 2, <GLOBAL _token>>
	set r7, _token
@ <JNEQ L353, <LOADW, <TEMP 2>>, <CONST 0>>
	ldr r0, [r7]
	cmp r0, #0
	bne .L353
@     if ch = ENDFILE then
@ <JNEQ L358, <LOADC, <REGVAR 0>>, <CONST 127>>
	cmp r4, #127
	bne .L358
@       token := EOFTOK
@ <STOREW, <CONST 14>, <TEMP 2>>
	set r0, #14
	str r0, [r7]
@ <JUMP L354>
	b .L354
@ <LABEL L358>
.L358:
@     elsif (ch = ' ') or (ch = TAB) or (ch = ENDLINE) then
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 32>>
	cmp r4, #32
	beq .L360
@ <JEQ L360, <LOADC, <REGVAR 0>>, <CONST 9>>
	cmp r4, #9
	beq .L360
@ <JNEQ L361, <LOADC, <REGVAR 0>>, <CONST 10>>
	cmp r4, #10
	bne .L361
@ <LABEL L360>
.L360:
@       ch := GetChar()
@ <DEFTEMP 3, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 3>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L354>
	b .L354
@ <LABEL L361>
.L361:
@     elsif ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) then
@ <JLT L450, <LOADC, <REGVAR 0>>, <CONST 65>>
	cmp r4, #65
	blt .L450
@ <JLEQ L363, <LOADC, <REGVAR 0>>, <CONST 90>>
	cmp r4, #90
	ble .L363
@ <LABEL L450>
.L450:
@ <JEQ L363, <LOADC, <REGVAR 0>>, <CONST 95>>
	cmp r4, #95
	beq .L363
@ <JLT L364, <LOADC, <REGVAR 0>>, <CONST 97>>
	cmp r4, #97
	blt .L364
@ <JGT L364, <LOADC, <REGVAR 0>>, <CONST 122>>
	cmp r4, #122
	bgt .L364
@ <LABEL L363>
.L363:
@       if (((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) then 
@ <JLT L431, <LOADC, <REGVAR 0>>, <CONST 65>>
	cmp r4, #65
	blt .L431
@ <JLEQ L428, <LOADC, <REGVAR 0>>, <CONST 90>>
	cmp r4, #90
	ble .L428
@ <LABEL L431>
.L431:
@ <JNEQ L429, <LOADC, <REGVAR 0>>, <CONST 95>>
	cmp r4, #95
	bne .L429
@ <LABEL L428>
.L428:
@ 	 token := VARIABLE
@ <STOREW, <CONST 2>, <GLOBAL _token>>
	set r0, #2
	set r1, _token
	str r0, [r1]
@ <JUMP L430>
	b .L430
@ <LABEL L429>
.L429:
@ 	 token := IDENT
@ <STOREW, <CONST 1>, <GLOBAL _token>>
	set r0, #1
	set r1, _token
	str r0, [r1]
@ <LABEL L430>
.L430:
@       i := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
	set r6, #0
@ <LABEL L433>
.L433:
@       while ((((ch >= 'A') and (ch <= 'Z')) or (ch = '_')) or ((ch >= 'a') and (ch <= 'z'))) or ((ch >= '0') and (ch <= '9')) do
@ <JLT L443, <LOADC, <REGVAR 0>>, <CONST 65>>
	cmp r4, #65
	blt .L443
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 90>>
	cmp r4, #90
	ble .L434
@ <LABEL L443>
.L443:
@ <JEQ L434, <LOADC, <REGVAR 0>>, <CONST 95>>
	cmp r4, #95
	beq .L434
@ <JLT L439, <LOADC, <REGVAR 0>>, <CONST 97>>
	cmp r4, #97
	blt .L439
@ <JLEQ L434, <LOADC, <REGVAR 0>>, <CONST 122>>
	cmp r4, #122
	ble .L434
@ <LABEL L439>
.L439:
@ <JLT L435, <LOADC, <REGVAR 0>>, <CONST 48>>
	cmp r4, #48
	blt .L435
@ <JGT L435, <LOADC, <REGVAR 0>>, <CONST 57>>
	cmp r4, #57
	bgt .L435
@ <LABEL L434>
.L434:
@         if i > MAXSTRING then
@ <JLEQ L438, <LOADW, <REGVAR 2>>, <CONST 128>>
	cmp r6, #128
	ble .L438
@           newline(); print_string("Panic: "); print_string("identifier too long"); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g42>>
	set r0, g42
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 19>>
	set r1, #19
@ <ARG 0, <GLOBAL g43>>
	set r0, g43
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L438>
.L438:
@         toksval[i] := ch; ch := GetChar(); i := i+1
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
	set r0, _toksval
	add r0, r0, r6
	strb r4, [r0]
@ <DEFTEMP 4, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 4>, <REGVAR 0>>
	mov r4, r0
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
	add r6, r6, #1
@ <JUMP L433>
	b .L433
@ <LABEL L435>
.L435:
@       PushBack(ch);
@ <ARG 0, <LOADC, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _PushBack>>
	bl _PushBack
@       toksval[i] := ENDSTR; tokval := Lookup(toksval);
@ <DEFTEMP 5, <GLOBAL _toksval>>
	set r7, _toksval
@ <STOREC, <CONST 0>, <OFFSET, <TEMP 5>, <LOADW, <REGVAR 2>>>>
	set r0, #0
	add r1, r7, r6
	strb r0, [r1]
@ <ARG 0, <TEMP 5>>
	mov r0, r7
@ <DEFTEMP 6, <CALL 1, <GLOBAL _Lookup>>>
	bl _Lookup
@ <STOREW, <TEMP 6>, <GLOBAL _tokval>>
	set r1, _tokval
	str r0, [r1]
@       if tokval = notsym then token := NEGATE end
@ <JNEQ L354, <TEMP 6>, <LOADW, <GLOBAL _notsym>>>
	set r1, _notsym
	ldr r1, [r1]
	cmp r0, r1
	bne .L354
@ <STOREW, <CONST 13>, <GLOBAL _token>>
	set r0, #13
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L364>
.L364:
@     elsif ((ch >= '0') and (ch <= '9')) then
@ <JLT L367, <LOADC, <REGVAR 0>>, <CONST 48>>
	cmp r4, #48
	blt .L367
@ <JGT L367, <LOADC, <REGVAR 0>>, <CONST 57>>
	cmp r4, #57
	bgt .L367
@       token := NUMBER; tokival := 0;
@ <STOREW, <CONST 3>, <GLOBAL _token>>
	set r0, #3
	set r1, _token
	str r0, [r1]
@ <STOREW, <CONST 0>, <GLOBAL _tokival>>
	set r0, #0
	set r1, _tokival
	str r0, [r1]
@ <LABEL L423>
.L423:
@       while ((ch >= '0') and (ch <= '9')) do
@ <JLT L425, <LOADC, <REGVAR 0>>, <CONST 48>>
	cmp r4, #48
	blt .L425
@ <JGT L425, <LOADC, <REGVAR 0>>, <CONST 57>>
	cmp r4, #57
	bgt .L425
@         tokival := 10 * tokival + (ord(ch) - ord('0'));
@ <DEFTEMP 7, <GLOBAL _tokival>>
	set r7, _tokival
@ <STOREW,
@   <PLUS,
@     <TIMES, <LOADW, <TEMP 7>>, <CONST 10>>,
@     <MINUS, <LOADC, <REGVAR 0>>, <CONST 48>>>,
@   <TEMP 7>>
	ldr r0, [r7]
	set r1, #10
	mul r0, r0, r1
	sub r1, r4, #48
	add r0, r0, r1
	str r0, [r7]
@         ch := GetChar()
@ <DEFTEMP 8, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 8>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L423>
	b .L423
@ <LABEL L425>
.L425:
@       PushBack(ch)
@ <ARG 0, <LOADC, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _PushBack>>
	bl _PushBack
@ <JUMP L354>
	b .L354
@ <LABEL L367>
.L367:
@       case ch of
@ <JCASE 30 L369, <MINUS, <LOADC, <REGVAR 0>>, <CONST 33>>>
	sub r0, r4, #33
	cmp r0, #30
	ldrlo pc, [pc, r0, LSL #2]
	b .L369
	.word .L379
	.word .L383
	.word .L378
	.word .L369
	.word .L369
	.word .L369
	.word .L382
	.word .L371
	.word .L372
	.word .L369
	.word .L369
	.word .L373
	.word .L369
	.word .L374
	.word .L380
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L369
	.word .L381
	.word .L369
	.word .L376
	.word .L375
	.word .L377
@ <LABEL L371>
.L371:
@         '(': token := LPAR
@ <STOREW, <CONST 7>, <GLOBAL _token>>
	set r0, #7
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L372>
.L372:
@       | ')': token := RPAR
@ <STOREW, <CONST 8>, <GLOBAL _token>>
	set r0, #8
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L373>
.L373:
@       | ',': token := COMMA
@ <STOREW, <CONST 9>, <GLOBAL _token>>
	set r0, #9
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L374>
.L374:
@       | '.': token := DOT
@ <STOREW, <CONST 10>, <GLOBAL _token>>
	set r0, #10
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L375>
.L375:
@       | '=': token := EQUAL
@ <STOREW, <CONST 12>, <GLOBAL _token>>
	set r0, #12
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L376>
.L376:
@       | '<': token := LANGLE
@ <STOREW, <CONST 15>, <GLOBAL _token>>
	set r0, #15
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L377>
.L377:
@       | '>': token := RANGLE
@ <STOREW, <CONST 16>, <GLOBAL _token>>
	set r0, #16
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L378>
.L378:
@       | '#': token := HASH
@ <STOREW, <CONST 17>, <GLOBAL _token>>
	set r0, #17
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L379>
.L379:
@       | '!': token := IDENT; tokval := cutsym
@ <STOREW, <CONST 1>, <GLOBAL _token>>
	set r0, #1
	set r1, _token
	str r0, [r1]
@ <STOREW, <LOADW, <GLOBAL _cutsym>>, <GLOBAL _tokval>>
	set r0, _cutsym
	ldr r0, [r0]
	set r1, _tokval
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L380>
.L380:
@ 	  ch := GetChar();
@ <DEFTEMP 9, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 9>, <REGVAR 0>>
	mov r4, r0
@ 	  if ch <> '*' then
@ <JEQ L388, <LOADC, <REGVAR 0>>, <CONST 42>>
	cmp r4, #42
	beq .L388
@ 	    if not errflag then ShowError(); print_string("bad token /"); newline(); Recover() end
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L354
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 11>>
	set r1, #11
@ <ARG 0, <GLOBAL g44>>
	set r0, g44
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <JUMP L354>
	b .L354
@ <LABEL L388>
.L388:
@ 	    ch2 := ' '; ch := GetChar();
@ <STOREC, <CONST 32>, <REGVAR 1>>
	set r5, #32
@ <DEFTEMP 10, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 10>, <REGVAR 0>>
	mov r4, r0
@ <LABEL L390>
.L390:
@ 	    while (ch <> ENDFILE) and not ((ch2 = '*') and (ch = '/')) do
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 127>>
	cmp r4, #127
	beq .L392
@ <JNEQ L391, <LOADC, <REGVAR 1>>, <CONST 42>>
	cmp r5, #42
	bne .L391
@ <JEQ L392, <LOADC, <REGVAR 0>>, <CONST 47>>
	cmp r4, #47
	beq .L392
@ <LABEL L391>
.L391:
@ 	      ch2 := ch; ch := GetChar() 
@ <STOREC, <LOADC, <REGVAR 0>>, <REGVAR 1>>
	mov r5, r4
@ <DEFTEMP 11, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 11>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L390>
	b .L390
@ <LABEL L392>
.L392:
@ 	    if ch = ENDFILE then
@ <JNEQ L396, <LOADC, <REGVAR 0>>, <CONST 127>>
	cmp r4, #127
	bne .L396
@ 	      if not errflag then ShowError(); print_string("end of file in comment"); newline(); Recover() end
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L354
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 22>>
	set r1, #22
@ <ARG 0, <GLOBAL g45>>
	set r0, g45
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <JUMP L354>
	b .L354
@ <LABEL L396>
.L396:
@ 	      ch := GetChar()
@ <DEFTEMP 12, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 12>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L354>
	b .L354
@ <LABEL L381>
.L381:
@ 	  ch := GetChar();
@ <DEFTEMP 13, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 13>, <REGVAR 0>>
	mov r4, r0
@ 	  if ch = '-' then
@ <JNEQ L405, <LOADC, <REGVAR 0>>, <CONST 45>>
	cmp r4, #45
	bne .L405
@ 	    token := ARROW
@ <STOREW, <CONST 6>, <GLOBAL _token>>
	set r0, #6
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L405>
.L405:
@ 	    PushBack(ch); token := COLON 
@ <ARG 0, <LOADC, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _PushBack>>
	bl _PushBack
@ <STOREW, <CONST 11>, <GLOBAL _token>>
	set r0, #11
	set r1, _token
	str r0, [r1]
@ <JUMP L354>
	b .L354
@ <LABEL L382>
.L382:
@ 	  token := CHCON; tokival := ord(GetChar()); ch := GetChar();
@ <STOREW, <CONST 4>, <GLOBAL _token>>
	set r0, #4
	set r1, _token
	str r0, [r1]
@ <DEFTEMP 14, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREW, <TEMP 14>, <GLOBAL _tokival>>
	set r1, _tokival
	str r0, [r1]
@ <DEFTEMP 15, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 15>, <REGVAR 0>>
	mov r4, r0
@ 	  if ch <> '''' then if not errflag then ShowError(); print_string("missing quote"); newline(); Recover() end end
@ <JEQ L354, <LOADC, <REGVAR 0>>, <CONST 39>>
	cmp r4, #39
	beq .L354
@ <JNEQ L354, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L354
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 13>>
	set r1, #13
@ <ARG 0, <GLOBAL g46>>
	set r0, g46
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <JUMP L354>
	b .L354
@ <LABEL L383>
.L383:
@ 	  token := STRCON; i := 0; ch := GetChar();
@ <STOREW, <CONST 5>, <GLOBAL _token>>
	set r0, #5
	set r1, _token
	str r0, [r1]
@ <STOREW, <CONST 0>, <REGVAR 2>>
	set r6, #0
@ <DEFTEMP 16, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 16>, <REGVAR 0>>
	mov r4, r0
@ <LABEL L413>
.L413:
@ 	  while (ch <> '"') and (ch <> ENDLINE) do
@ <JEQ L415, <LOADC, <REGVAR 0>>, <CONST 34>>
	cmp r4, #34
	beq .L415
@ <JEQ L415, <LOADC, <REGVAR 0>>, <CONST 10>>
	cmp r4, #10
	beq .L415
@ 	    toksval[i] := ch; ch := GetChar(); i := i+1 
@ <STOREC,
@   <LOADC, <REGVAR 0>>,
@   <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
	set r0, _toksval
	add r0, r0, r6
	strb r4, [r0]
@ <DEFTEMP 17, <CALL 0, <GLOBAL _GetChar>>>
	bl _GetChar
@ <STOREC, <TEMP 17>, <REGVAR 0>>
	mov r4, r0
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
	add r6, r6, #1
@ <JUMP L413>
	b .L413
@ <LABEL L415>
.L415:
@ 	  toksval[i] := ENDSTR;
@ <STOREC, <CONST 0>, <OFFSET, <GLOBAL _toksval>, <LOADW, <REGVAR 2>>>>
	set r0, #0
	set r1, _toksval
	add r1, r1, r6
	strb r0, [r1]
@ 	  if ch = ENDLINE then
@ <JNEQ L354, <LOADC, <REGVAR 0>>, <CONST 10>>
	cmp r4, #10
	bne .L354
@ 	    if not errflag then ShowError(); print_string("unterminated string"); newline(); Recover() end;
@ <JNEQ L422, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L422
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 19>>
	set r1, #19
@ <ARG 0, <GLOBAL g47>>
	set r0, g47
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <LABEL L422>
.L422:
@ 	    PushBack(ch)
@ <ARG 0, <LOADC, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _PushBack>>
	bl _PushBack
@ <JUMP L354>
	b .L354
@ <LABEL L369>
.L369:
@ 	if not errflag then ShowError(); print_string("illegal character"); newline(); Recover() end; print_char(ch); newline()
@ <JNEQ L386, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L386
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 17>>
	set r1, #17
@ <ARG 0, <GLOBAL g48>>
	set r0, g48
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <LABEL L386>
.L386:
@ <ARG 0, <LOADC, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL print_char>>
	bl print_char
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <JUMP L354>
	b .L354
@ <LABEL L353>
.L353:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PrintToken(t: integer);
@ Initial code:
@   case t of
@ <JCASE 17 L455,
@   <MINUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>>
@ <LABEL L457>
@       print_string("identifier "); WriteString(symtab[tokval].name)
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g49>>,
@   <ARG 1, <CONST 11>>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <TIMES, <LOADW, <GLOBAL _tokval>>, <CONST 16>>>,
@         <CONST 0>>>>>
@ <JUMP L456>
@ <LABEL L458>
@       print_string("variable "); WriteString(symtab[tokval].name)
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g50>>,
@   <ARG 1, <CONST 9>>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <TIMES, <LOADW, <GLOBAL _tokval>>, <CONST 16>>>,
@         <CONST 0>>>>>
@ <JUMP L456>
@ <LABEL L459>
@   | NUMBER: print_string("number");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g51>>,
@   <ARG 1, <CONST 6>>>
@   | CHCON:  print_string("char constant");
@ <JUMP L456>
@ <LABEL L460>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g52>>,
@   <ARG 1, <CONST 13>>>
@   | ARROW:  print_string(":-");
@ <JUMP L456>
@ <LABEL L461>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g53>>,
@   <ARG 1, <CONST 2>>>
@   | LPAR:   print_string("(");
@ <JUMP L456>
@ <LABEL L462>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g54>>,
@   <ARG 1, <CONST 1>>>
@   | RPAR:   print_string(")");
@ <JUMP L456>
@ <LABEL L463>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g55>>,
@   <ARG 1, <CONST 1>>>
@   | COMMA:  print_string(",");
@ <JUMP L456>
@ <LABEL L464>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g56>>,
@   <ARG 1, <CONST 1>>>
@   | DOT:    print_string(".");
@ <JUMP L456>
@ <LABEL L465>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g57>>,
@   <ARG 1, <CONST 1>>>
@   | COLON:  print_string(":");
@ <JUMP L456>
@ <LABEL L466>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g58>>,
@   <ARG 1, <CONST 1>>>
@   | EQUAL:  print_string("=");
@ <JUMP L456>
@ <LABEL L467>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g59>>,
@   <ARG 1, <CONST 1>>>
@   | STRCON: print_string("string constant")
@ <JUMP L456>
@ <LABEL L468>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g60>>,
@   <ARG 1, <CONST 15>>>
@ <JUMP L456>
@ <LABEL L469>
@   | LANGLE: print_string("<")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g61>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L456>
@ <LABEL L470>
@   | RANGLE: print_string(">")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g62>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L456>
@ <LABEL L471>
@   | HASH:   print_string("#")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g63>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L456>
@ <LABEL L455>
@     print_string("unknown token")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g64>>,
@   <ARG 1, <CONST 13>>>
@ <LABEL L456>
@ <LABEL L454>

@ After simplification:
@   case t of
@ <JCASE 17 L455, <MINUS, <LOADW, <LOCAL 40>>, <CONST 1>>>
@ <LABEL L457>
@       print_string("identifier "); WriteString(symtab[tokval].name)
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g49>>,
@   <ARG 1, <CONST 11>>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL, <LOADW, <GLOBAL _tokval>>, <CONST 4>>>>>>
@ <JUMP L454>
@ <LABEL L458>
@       print_string("variable "); WriteString(symtab[tokval].name)
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g50>>,
@   <ARG 1, <CONST 9>>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL, <LOADW, <GLOBAL _tokval>>, <CONST 4>>>>>>
@ <JUMP L454>
@ <LABEL L459>
@   | NUMBER: print_string("number");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g51>>,
@   <ARG 1, <CONST 6>>>
@ <JUMP L454>
@ <LABEL L460>
@   | CHCON:  print_string("char constant");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g52>>,
@   <ARG 1, <CONST 13>>>
@ <JUMP L454>
@ <LABEL L461>
@   | ARROW:  print_string(":-");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g53>>,
@   <ARG 1, <CONST 2>>>
@ <JUMP L454>
@ <LABEL L462>
@   | LPAR:   print_string("(");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g54>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L463>
@   | RPAR:   print_string(")");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g55>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L464>
@   | COMMA:  print_string(",");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g56>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L465>
@   | DOT:    print_string(".");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g57>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L466>
@   | COLON:  print_string(":");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g58>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L467>
@   | EQUAL:  print_string("=");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g59>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L468>
@   | STRCON: print_string("string constant")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g60>>,
@   <ARG 1, <CONST 15>>>
@ <JUMP L454>
@ <LABEL L469>
@   | LANGLE: print_string("<")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g61>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L470>
@   | RANGLE: print_string(">")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g62>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L471>
@   | HASH:   print_string("#")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g63>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L455>
@     print_string("unknown token")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g64>>,
@   <ARG 1, <CONST 13>>>
@ <LABEL L454>

@ After sharing:
@   case t of
@ <JCASE 17 L455, <MINUS, <LOADW, <LOCAL 40>>, <CONST 1>>>
@ <LABEL L457>
@       print_string("identifier "); WriteString(symtab[tokval].name)
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g49>>,
@   <ARG 1, <CONST 11>>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL, <LOADW, <GLOBAL _tokval>>, <CONST 4>>>>>>
@ <JUMP L454>
@ <LABEL L458>
@       print_string("variable "); WriteString(symtab[tokval].name)
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g50>>,
@   <ARG 1, <CONST 9>>>
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL, <LOADW, <GLOBAL _tokval>>, <CONST 4>>>>>>
@ <JUMP L454>
@ <LABEL L459>
@   | NUMBER: print_string("number");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g51>>,
@   <ARG 1, <CONST 6>>>
@ <JUMP L454>
@ <LABEL L460>
@   | CHCON:  print_string("char constant");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g52>>,
@   <ARG 1, <CONST 13>>>
@ <JUMP L454>
@ <LABEL L461>
@   | ARROW:  print_string(":-");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g53>>,
@   <ARG 1, <CONST 2>>>
@ <JUMP L454>
@ <LABEL L462>
@   | LPAR:   print_string("(");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g54>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L463>
@   | RPAR:   print_string(")");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g55>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L464>
@   | COMMA:  print_string(",");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g56>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L465>
@   | DOT:    print_string(".");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g57>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L466>
@   | COLON:  print_string(":");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g58>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L467>
@   | EQUAL:  print_string("=");
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g59>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L468>
@   | STRCON: print_string("string constant")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g60>>,
@   <ARG 1, <CONST 15>>>
@ <JUMP L454>
@ <LABEL L469>
@   | LANGLE: print_string("<")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g61>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L470>
@   | RANGLE: print_string(">")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g62>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L471>
@   | HASH:   print_string("#")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g63>>,
@   <ARG 1, <CONST 1>>>
@ <JUMP L454>
@ <LABEL L455>
@     print_string("unknown token")
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g64>>,
@   <ARG 1, <CONST 13>>>
@ <LABEL L454>

_PrintToken:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   case t of
@ <JCASE 17 L455, <MINUS, <LOADW, <LOCAL 40>>, <CONST 1>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	sub r0, r0, #1
	cmp r0, #17
	ldrlo pc, [pc, r0, LSL #2]
	b .L455
	.word .L457
	.word .L458
	.word .L459
	.word .L460
	.word .L468
	.word .L461
	.word .L462
	.word .L463
	.word .L464
	.word .L465
	.word .L466
	.word .L467
	.word .L455
	.word .L455
	.word .L469
	.word .L470
	.word .L471
@ <LABEL L457>
.L457:
@       print_string("identifier "); WriteString(symtab[tokval].name)
@ <ARG 1, <CONST 11>>
	set r1, #11
@ <ARG 0, <GLOBAL g49>>
	set r0, g49
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <LSL, <LOADW, <GLOBAL _tokval>>, <CONST 4>>>>>
	set r0, _symtab
	set r1, _tokval
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _WriteString>>
	bl _WriteString
@ <JUMP L454>
	b .L454
@ <LABEL L458>
.L458:
@       print_string("variable "); WriteString(symtab[tokval].name)
@ <ARG 1, <CONST 9>>
	set r1, #9
@ <ARG 0, <GLOBAL g50>>
	set r0, g50
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <LSL, <LOADW, <GLOBAL _tokval>>, <CONST 4>>>>>
	set r0, _symtab
	set r1, _tokval
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _WriteString>>
	bl _WriteString
@ <JUMP L454>
	b .L454
@ <LABEL L459>
.L459:
@   | NUMBER: print_string("number");
@ <ARG 1, <CONST 6>>
	set r1, #6
@ <ARG 0, <GLOBAL g51>>
	set r0, g51
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L460>
.L460:
@   | CHCON:  print_string("char constant");
@ <ARG 1, <CONST 13>>
	set r1, #13
@ <ARG 0, <GLOBAL g52>>
	set r0, g52
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L461>
.L461:
@   | ARROW:  print_string(":-");
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g53>>
	set r0, g53
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L462>
.L462:
@   | LPAR:   print_string("(");
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g54>>
	set r0, g54
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L463>
.L463:
@   | RPAR:   print_string(")");
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g55>>
	set r0, g55
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L464>
.L464:
@   | COMMA:  print_string(",");
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g56>>
	set r0, g56
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L465>
.L465:
@   | DOT:    print_string(".");
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g57>>
	set r0, g57
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L466>
.L466:
@   | COLON:  print_string(":");
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g58>>
	set r0, g58
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L467>
.L467:
@   | EQUAL:  print_string("=");
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g59>>
	set r0, g59
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L468>
.L468:
@   | STRCON: print_string("string constant")
@ <ARG 1, <CONST 15>>
	set r1, #15
@ <ARG 0, <GLOBAL g60>>
	set r0, g60
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L469>
.L469:
@   | LANGLE: print_string("<")
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g61>>
	set r0, g61
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L470>
.L470:
@   | RANGLE: print_string(">")
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g62>>
	set r0, g62
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L471>
.L471:
@   | HASH:   print_string("#")
@ <ARG 1, <CONST 1>>
	set r1, #1
@ <ARG 0, <GLOBAL g63>>
	set r0, g63
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <JUMP L454>
	b .L454
@ <LABEL L455>
.L455:
@     print_string("unknown token")
@ <ARG 1, <CONST 13>>
	set r1, #13
@ <ARG 0, <GLOBAL g64>>
	set r0, g64
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <LABEL L454>
.L454:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc VarRep(name: symbol): term;
@ Initial code:
@   if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
@ <JEQ L473, <LOADW, <GLOBAL _nvars>>, <CONST 63>>
@ <JUMP L474>
@ <LABEL L473>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g65>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g66>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L475>
@ <LABEL L474>
@ <LABEL L475>
@   i := 1; vartable[nvars+1] := name;  (* sentinel *)
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _vartable>,
@     <TIMES, <PLUS, <LOADW, <GLOBAL _nvars>>, <CONST 1>>, <CONST 4>>>>
@   while name <> vartable[i] do i := i+1 end;
@ <LABEL L476>
@ <JNEQ L477,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _vartable>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>>
@ <JUMP L478>
@ <LABEL L477>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L476>
@ <LABEL L478>
@   if i = nvars+1 then nvars := nvars+1 end;
@ <JEQ L479,
@   <LOADW, <REGVAR 0>>,
@   <PLUS, <LOADW, <GLOBAL _nvars>>, <CONST 1>>>
@ <JUMP L480>
@ <LABEL L479>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _nvars>>, <CONST 1>>, <GLOBAL _nvars>>
@ <JUMP L481>
@ <LABEL L480>
@ <LABEL L481>
@   return MakeRef(i)
@ <RESULTW,
@   <CALL 1,
@     <GLOBAL _MakeRef>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <REGVAR 0>>>>>
@ <JUMP L472>
@ <LABEL L472>

@ After simplification:
@   if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
@ <JNEQ L475, <LOADW, <GLOBAL _nvars>>, <CONST 63>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g65>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g66>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L475>
@   i := 1; vartable[nvars+1] := name;  (* sentinel *)
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _vartable>,
@       <LSL, <LOADW, <GLOBAL _nvars>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L476>
@   while name <> vartable[i] do i := i+1 end;
@ <JEQ L478,
@   <LOADW, <LOCAL 40>>,
@   <LOADW,
@     <OFFSET, <GLOBAL _vartable>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L476>
@ <LABEL L478>
@   if i = nvars+1 then nvars := nvars+1 end;
@ <JNEQ L481,
@   <LOADW, <REGVAR 0>>,
@   <PLUS, <LOADW, <GLOBAL _nvars>>, <CONST 1>>>
@ <STOREW, <PLUS, <LOADW, <GLOBAL _nvars>>, <CONST 1>>, <GLOBAL _nvars>>
@ <LABEL L481>
@   return MakeRef(i)
@ <RESULTW, <CALL 1, <GLOBAL _MakeRef>, <ARG 0, <LOADW, <REGVAR 0>>>>>

@ After sharing:
@   if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
@ <JNEQ L475, <LOADW, <GLOBAL _nvars>>, <CONST 63>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g65>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g66>>,
@   <ARG 1, <CONST 18>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L475>
@   i := 1; vartable[nvars+1] := name;  (* sentinel *)
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _vartable>,
@       <LSL, <LOADW, <GLOBAL _nvars>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L476>
@   while name <> vartable[i] do i := i+1 end;
@ <JEQ L478,
@   <LOADW, <LOCAL 40>>,
@   <LOADW,
@     <OFFSET, <GLOBAL _vartable>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L476>
@ <LABEL L478>
@   if i = nvars+1 then nvars := nvars+1 end;
@ <DEFTEMP 1, <GLOBAL _nvars>>
@ <DEFTEMP 2, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>>
@ <JNEQ L481, <LOADW, <REGVAR 0>>, <TEMP 2>>
@ <STOREW, <TEMP 2>, <TEMP 1>>
@ <LABEL L481>
@   return MakeRef(i)
@ <DEFTEMP 3, <CALL 1, <GLOBAL _MakeRef>, <ARG 0, <LOADW, <REGVAR 0>>>>>
@ <RESULTW, <TEMP 3>>

_VarRep:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if nvars = MAXARITY then newline(); print_string("Panic: "); print_string("too many variables"); newline(); exit(2) end;
@ <JNEQ L475, <LOADW, <GLOBAL _nvars>>, <CONST 63>>
	set r0, _nvars
	ldr r0, [r0]
	cmp r0, #63
	bne .L475
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g65>>
	set r0, g65
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 18>>
	set r1, #18
@ <ARG 0, <GLOBAL g66>>
	set r0, g66
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L475>
.L475:
@   i := 1; vartable[nvars+1] := name;  (* sentinel *)
@ <STOREW, <CONST 1>, <REGVAR 0>>
	set r4, #1
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _vartable>,
@       <LSL, <LOADW, <GLOBAL _nvars>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _vartable
	set r2, _nvars
	ldr r2, [r2]
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	str r0, [r1]
@ <LABEL L476>
.L476:
@   while name <> vartable[i] do i := i+1 end;
@ <JEQ L478,
@   <LOADW, <LOCAL 40>>,
@   <LOADW,
@     <OFFSET, <GLOBAL _vartable>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _vartable
	lsl r2, r4, #2
	add r1, r1, r2
	ldr r1, [r1]
	cmp r0, r1
	beq .L478
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L476>
	b .L476
@ <LABEL L478>
.L478:
@   if i = nvars+1 then nvars := nvars+1 end;
@ <DEFTEMP 1, <GLOBAL _nvars>>
	set r5, _nvars
@ <DEFTEMP 2, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>>
	ldr r0, [r5]
	add r6, r0, #1
@ <JNEQ L481, <LOADW, <REGVAR 0>>, <TEMP 2>>
	cmp r4, r6
	bne .L481
@ <STOREW, <TEMP 2>, <TEMP 1>>
	str r6, [r5]
@ <LABEL L481>
.L481:
@   return MakeRef(i)
@ <ARG 0, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <DEFTEMP 3, <CALL 1, <GLOBAL _MakeRef>>>
	bl _MakeRef
@ <RESULTW, <TEMP 3>>
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ShowAnswer(bindings: frame);
@ Initial code:
@   if nvars = 0 then
@ <JEQ L483, <LOADW, <GLOBAL _nvars>>, <CONST 0>>
@ <JUMP L484>
@ <LABEL L483>
@     print_string("yes"); newline()
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g67>>,
@   <ARG 1, <CONST 3>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L485>
@ <LABEL L484>
@     for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <LOADW, <GLOBAL _nvars>>, <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <LABEL L486>
@ <JGT L487,
@   <LOADW, <REGVAR 0>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>>
@       WriteString(symtab[vartable[i]].name); print_string(" = ");
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <TIMES,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _vartable>,
@                 <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>,
@             <CONST 16>>>,
@         <CONST 0>>>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g68>>,
@   <ARG 1, <CONST 3>>>
@       PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <PLUS,
@       <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 7>>,
@       <TIMES, <MINUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 2>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 1>>>
@       newline()
@ <CALL 0, <GLOBAL newline>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L486>
@ <LABEL L487>
@ <LABEL L485>
@ <LABEL L482>

@ After simplification:
@   if nvars = 0 then
@ <JNEQ L484, <LOADW, <GLOBAL _nvars>>, <CONST 0>>
@     print_string("yes"); newline()
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g67>>,
@   <ARG 1, <CONST 3>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L482>
@ <LABEL L484>
@     for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <LOADW, <GLOBAL _nvars>>, <LOCAL -4>>
@ <LABEL L486>
@ <JGT L482, <LOADW, <REGVAR 0>>, <LOADW, <LOCAL -4>>>
@       WriteString(symtab[vartable[i]].name); print_string(" = ");
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _vartable>,
@               <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>,
@           <CONST 4>>>>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g68>>,
@   <ARG 1, <CONST 3>>>
@       PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <PLUS,
@       <PLUS, <LOADW, <LOCAL 40>>, <CONST 7>>,
@       <MINUS, <LSL, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 2>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 1>>>
@       newline()
@ <CALL 0, <GLOBAL newline>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L486>
@ <LABEL L482>

@ After sharing:
@   if nvars = 0 then
@ <JNEQ L484, <LOADW, <GLOBAL _nvars>>, <CONST 0>>
@     print_string("yes"); newline()
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g67>>,
@   <ARG 1, <CONST 3>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L482>
@ <LABEL L484>
@     for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <LOADW, <GLOBAL _nvars>>, <LOCAL -4>>
@ <LABEL L486>
@ <JGT L482, <LOADW, <REGVAR 0>>, <LOADW, <LOCAL -4>>>
@       WriteString(symtab[vartable[i]].name); print_string(" = ");
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _vartable>,
@               <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>,
@           <CONST 4>>>>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g68>>,
@   <ARG 1, <CONST 3>>>
@       PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <PLUS,
@       <PLUS, <LOADW, <LOCAL 40>>, <CONST 7>>,
@       <MINUS, <LSL, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 2>>>>,
@   <ARG 1, <CONST 0>>,
@   <ARG 2, <CONST 1>>>
@       newline()
@ <CALL 0, <GLOBAL newline>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L486>
@ <LABEL L482>

_ShowAnswer:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   if nvars = 0 then
@ <JNEQ L484, <LOADW, <GLOBAL _nvars>>, <CONST 0>>
	set r0, _nvars
	ldr r0, [r0]
	cmp r0, #0
	bne .L484
@     print_string("yes"); newline()
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g67>>
	set r0, g67
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <JUMP L482>
	b .L482
@ <LABEL L484>
.L484:
@     for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 0>>
	set r4, #1
@ <STOREW, <LOADW, <GLOBAL _nvars>>, <LOCAL -4>>
	set r0, _nvars
	ldr r0, [r0]
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L486>
.L486:
@ <JGT L482, <LOADW, <REGVAR 0>>, <LOADW, <LOCAL -4>>>
	set ip, #-4
	add r0, fp, ip
	ldr r0, [r0]
	cmp r4, r0
	bgt .L482
@       WriteString(symtab[vartable[i]].name); print_string(" = ");
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <LSL,
@         <LOADW,
@           <OFFSET,
@             <GLOBAL _vartable>,
@             <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>,
@         <CONST 4>>>>>
	set r0, _symtab
	set r1, _vartable
	lsl r2, r4, #2
	add r1, r1, r2
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _WriteString>>
	bl _WriteString
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g68>>
	set r0, g68
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@       PrintTerm((bindings+7+(i-1)*TERM_SIZE), NULL, EQPRIO-1);
@ <ARG 2, <CONST 1>>
	set r2, #1
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0,
@   <PLUS,
@     <PLUS, <LOADW, <LOCAL 40>>, <CONST 7>>,
@     <MINUS, <LSL, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 2>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, #7
	lsl r3, r4, #1
	sub r3, r3, #2
	add r0, r0, r3
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@       newline()
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L486>
	b .L486
@ <LABEL L482>
.L482:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Eat(expected: integer);
@ Initial code:
@   if token = expected then
@ <JEQ L489,
@   <LOADW, <GLOBAL _token>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <JUMP L490>
@ <LABEL L489>
@     if token <> DOT then Scan() end
@ <JNEQ L495, <LOADW, <GLOBAL _token>>, <CONST 10>>
@ <JUMP L496>
@ <LABEL L495>
@ <CALL 0, <GLOBAL _Scan>, <STATLINK, <CONST 0>>>
@ <JUMP L497>
@ <LABEL L496>
@ <LABEL L497>
@ <JUMP L491>
@ <LABEL L490>
@   elsif not errflag then
@ <JNEQ L493, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L492>
@ <LABEL L492>
@     ShowError();
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@     print_string("expected "); PrintToken(expected);
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g69>>,
@   <ARG 1, <CONST 9>>>
@ <CALL 1,
@   <GLOBAL _PrintToken>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>
@     print_string(", found "); PrintToken(token); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g70>>,
@   <ARG 1, <CONST 8>>>
@ <CALL 1,
@   <GLOBAL _PrintToken>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <GLOBAL _token>>>>
@ <CALL 0, <GLOBAL newline>>
@     Recover()
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L494>
@ <LABEL L493>
@ <LABEL L494>
@ <LABEL L491>
@ <LABEL L488>

@ After simplification:
@   if token = expected then
@ <JNEQ L490, <LOADW, <GLOBAL _token>>, <LOADW, <LOCAL 40>>>
@     if token <> DOT then Scan() end
@ <JEQ L488, <LOADW, <GLOBAL _token>>, <CONST 10>>
@ <CALL 0, <GLOBAL _Scan>>
@ <JUMP L488>
@ <LABEL L490>
@   elsif not errflag then
@ <JNEQ L488, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@     ShowError();
@ <CALL 0, <GLOBAL _ShowError>>
@     print_string("expected "); PrintToken(expected);
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g69>>,
@   <ARG 1, <CONST 9>>>
@ <CALL 1, <GLOBAL _PrintToken>, <ARG 0, <LOADW, <LOCAL 40>>>>
@     print_string(", found "); PrintToken(token); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g70>>,
@   <ARG 1, <CONST 8>>>
@ <CALL 1, <GLOBAL _PrintToken>, <ARG 0, <LOADW, <GLOBAL _token>>>>
@ <CALL 0, <GLOBAL newline>>
@     Recover()
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L488>

@ After sharing:
@   if token = expected then
@ <DEFTEMP 1, <LOADW, <GLOBAL _token>>>
@ <JNEQ L490, <TEMP 1>, <LOADW, <LOCAL 40>>>
@     if token <> DOT then Scan() end
@ <JEQ L488, <TEMP 1>, <CONST 10>>
@ <CALL 0, <GLOBAL _Scan>>
@ <JUMP L488>
@ <LABEL L490>
@   elsif not errflag then
@ <JNEQ L488, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@     ShowError();
@ <CALL 0, <GLOBAL _ShowError>>
@     print_string("expected "); PrintToken(expected);
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g69>>,
@   <ARG 1, <CONST 9>>>
@ <CALL 1, <GLOBAL _PrintToken>, <ARG 0, <LOADW, <LOCAL 40>>>>
@     print_string(", found "); PrintToken(token); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g70>>,
@   <ARG 1, <CONST 8>>>
@ <CALL 1, <GLOBAL _PrintToken>, <ARG 0, <LOADW, <GLOBAL _token>>>>
@ <CALL 0, <GLOBAL newline>>
@     Recover()
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L488>

_Eat:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if token = expected then
@ <DEFTEMP 1, <LOADW, <GLOBAL _token>>>
	set r0, _token
	ldr r4, [r0]
@ <JNEQ L490, <TEMP 1>, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	cmp r4, r0
	bne .L490
@     if token <> DOT then Scan() end
@ <JEQ L488, <TEMP 1>, <CONST 10>>
	cmp r4, #10
	beq .L488
@ <CALL 0, <GLOBAL _Scan>>
	bl _Scan
@ <JUMP L488>
	b .L488
@ <LABEL L490>
.L490:
@   elsif not errflag then
@ <JNEQ L488, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L488
@     ShowError();
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@     print_string("expected "); PrintToken(expected);
@ <ARG 1, <CONST 9>>
	set r1, #9
@ <ARG 0, <GLOBAL g69>>
	set r0, g69
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _PrintToken>>
	bl _PrintToken
@     print_string(", found "); PrintToken(token); newline();
@ <ARG 1, <CONST 8>>
	set r1, #8
@ <ARG 0, <GLOBAL g70>>
	set r0, g70
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 0, <LOADW, <GLOBAL _token>>>
	set r0, _token
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _PrintToken>>
	bl _PrintToken
@ <CALL 0, <GLOBAL newline>>
	bl newline
@     Recover()
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <LABEL L488>
.L488:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParseCompound(): term;
@ Initial code:
@   fun := tokval; n := 0; Eat(IDENT);
@ <STOREW, <LOADW, <GLOBAL _tokval>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 1>>>
@   if token = LPAR then
@ <JEQ L499, <LOADW, <GLOBAL _token>>, <CONST 7>>
@ <JUMP L500>
@ <LABEL L499>
@     Eat(LPAR); n := 1; arg[1] := ParseTerm();
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 7>>>
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -256>>,
@     <TIMES, <CONST 1>, <CONST 4>>>>
@     while token = COMMA do
@ <LABEL L502>
@ <JEQ L503, <LOADW, <GLOBAL _token>>, <CONST 9>>
@ <JUMP L504>
@ <LABEL L503>
@       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 9>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -256>>,
@     <TIMES, <LOADW, <REGVAR 1>>, <CONST 4>>>>
@ <JUMP L502>
@ <LABEL L504>
@     Eat(RPAR)
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 8>>>
@ <JUMP L501>
@ <LABEL L500>
@ <LABEL L501>
@   if symtab[fun].arity = -1 then
@ <JEQ L505,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 4>>>,
@   <CONST -1>>
@ <JUMP L506>
@ <LABEL L505>
@     symtab[fun].arity := n
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _symtab>,
@       <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@     <CONST 4>>>
@ <JUMP L507>
@ <LABEL L506>
@   elsif symtab[fun].arity <> n then
@ <JNEQ L508,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES, <LOADW, <REGVAR 0>>, <CONST 16>>>,
@       <CONST 4>>>,
@   <LOADW, <REGVAR 1>>>
@ <JUMP L509>
@ <LABEL L508>
@     if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
@ <JNEQ L512, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L511>
@ <LABEL L511>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g71>>,
@   <ARG 1, <CONST 20>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L513>
@ <LABEL L512>
@ <LABEL L513>
@ <JUMP L510>
@ <LABEL L509>
@ <LABEL L510>
@ <LABEL L507>
@   return MakeCompound(fun, arg)
@ <RESULTW,
@   <CALL 2,
@     <GLOBAL _MakeCompound>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <REGVAR 0>>>,
@     <ARG 1, <OFFSET, <LOCAL 0>, <CONST -256>>>>>
@ <JUMP L498>
@ <LABEL L498>

@ After simplification:
@   fun := tokval; n := 0; Eat(IDENT);
@ <STOREW, <LOADW, <GLOBAL _tokval>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 1>>>
@   if token = LPAR then
@ <JNEQ L501, <LOADW, <GLOBAL _token>>, <CONST 7>>
@     Eat(LPAR); n := 1; arg[1] := ParseTerm();
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 7>>>
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <CALL 0, <GLOBAL _ParseTerm>>, <LOCAL -252>>
@ <LABEL L502>
@     while token = COMMA do
@ <JNEQ L504, <LOADW, <GLOBAL _token>>, <CONST 9>>
@       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 9>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>
@ <JUMP L502>
@ <LABEL L504>
@     Eat(RPAR)
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 8>>>
@ <LABEL L501>
@   if symtab[fun].arity = -1 then
@ <JNEQ L506,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST -1>>
@     symtab[fun].arity := n
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 4>>>
@ <JUMP L507>
@ <LABEL L506>
@   elsif symtab[fun].arity <> n then
@ <JEQ L507,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <LOADW, <REGVAR 1>>>
@     if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
@ <JNEQ L507, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g71>>,
@   <ARG 1, <CONST 20>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L507>
@   return MakeCompound(fun, arg)
@ <RESULTW,
@   <CALL 2,
@     <GLOBAL _MakeCompound>,
@     <ARG 0, <LOADW, <REGVAR 0>>>,
@     <ARG 1, <LOCAL -256>>>>

@ After sharing:
@   fun := tokval; n := 0; Eat(IDENT);
@ <STOREW, <LOADW, <GLOBAL _tokval>>, <REGVAR 0>>
@ <STOREW, <CONST 0>, <REGVAR 1>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 1>>>
@   if token = LPAR then
@ <JNEQ L501, <LOADW, <GLOBAL _token>>, <CONST 7>>
@     Eat(LPAR); n := 1; arg[1] := ParseTerm();
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 7>>>
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW, <TEMP 1>, <LOCAL -252>>
@ <LABEL L502>
@     while token = COMMA do
@ <JNEQ L504, <LOADW, <GLOBAL _token>>, <CONST 9>>
@       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 9>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW,
@   <TEMP 2>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>
@ <JUMP L502>
@ <LABEL L504>
@     Eat(RPAR)
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 8>>>
@ <LABEL L501>
@   if symtab[fun].arity = -1 then
@ <DEFTEMP 3,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 4>>>
@ <JNEQ L506, <LOADW, <TEMP 3>>, <CONST -1>>
@     symtab[fun].arity := n
@ <STOREW, <LOADW, <REGVAR 1>>, <TEMP 3>>
@ <JUMP L507>
@ <LABEL L506>
@   elsif symtab[fun].arity <> n then
@ <JEQ L507,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <LOADW, <REGVAR 1>>>
@     if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
@ <JNEQ L507, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g71>>,
@   <ARG 1, <CONST 20>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L507>
@   return MakeCompound(fun, arg)
@ <DEFTEMP 4,
@   <CALL 2,
@     <GLOBAL _MakeCompound>,
@     <ARG 0, <LOADW, <REGVAR 0>>>,
@     <ARG 1, <LOCAL -256>>>>
@ <RESULTW, <TEMP 4>>

_ParseCompound:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #256
@   fun := tokval; n := 0; Eat(IDENT);
@ <STOREW, <LOADW, <GLOBAL _tokval>>, <REGVAR 0>>
	set r0, _tokval
	ldr r4, [r0]
@ <STOREW, <CONST 0>, <REGVAR 1>>
	set r5, #0
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@   if token = LPAR then
@ <JNEQ L501, <LOADW, <GLOBAL _token>>, <CONST 7>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #7
	bne .L501
@     Eat(LPAR); n := 1; arg[1] := ParseTerm();
@ <ARG 0, <CONST 7>>
	set r0, #7
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <STOREW, <CONST 1>, <REGVAR 1>>
	set r5, #1
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW, <TEMP 1>, <LOCAL -252>>
	set ip, #-252
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L502>
.L502:
@     while token = COMMA do
@ <JNEQ L504, <LOADW, <GLOBAL _token>>, <CONST 9>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #9
	bne .L504
@       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
@ <ARG 0, <CONST 9>>
	set r0, #9
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW,
@   <TEMP 2>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>
	set ip, #-256
	add r1, fp, ip
	lsl r2, r5, #2
	add r1, r1, r2
	str r0, [r1]
@ <JUMP L502>
	b .L502
@ <LABEL L504>
.L504:
@     Eat(RPAR)
@ <ARG 0, <CONST 8>>
	set r0, #8
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <LABEL L501>
.L501:
@   if symtab[fun].arity = -1 then
@ <DEFTEMP 3,
@   <OFFSET,
@     <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@     <CONST 4>>>
	set r0, _symtab
	lsl r1, r4, #4
	add r0, r0, r1
	add r6, r0, #4
@ <JNEQ L506, <LOADW, <TEMP 3>>, <CONST -1>>
	ldr r0, [r6]
	set r1, #-1
	cmp r0, r1
	bne .L506
@     symtab[fun].arity := n
@ <STOREW, <LOADW, <REGVAR 1>>, <TEMP 3>>
	str r5, [r6]
@ <JUMP L507>
	b .L507
@ <LABEL L506>
.L506:
@   elsif symtab[fun].arity <> n then
@ <JEQ L507,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _symtab>, <LSL, <LOADW, <REGVAR 0>>, <CONST 4>>>,
@       <CONST 4>>>,
@   <LOADW, <REGVAR 1>>>
	set r0, _symtab
	lsl r1, r4, #4
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
	cmp r0, r5
	beq .L507
@     if not errflag then ShowError(); print_string("wrong number of args"); newline(); Recover() end
@ <JNEQ L507, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L507
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 20>>
	set r1, #20
@ <ARG 0, <GLOBAL g71>>
	set r0, g71
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <LABEL L507>
.L507:
@   return MakeCompound(fun, arg)
@ <ARG 1, <LOCAL -256>>
	set ip, #-256
	add r1, fp, ip
@ <ARG 0, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <DEFTEMP 4, <CALL 2, <GLOBAL _MakeCompound>>>
	bl _MakeCompound
@ <RESULTW, <TEMP 4>>
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParsePrimary(): term;
@ Initial code:
@   if token = IDENT then t := ParseCompound()
@ <JEQ L515, <LOADW, <GLOBAL _token>>, <CONST 1>>
@ <JUMP L516>
@ <LABEL L515>
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseCompound>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L517>
@ <LABEL L516>
@   elsif token = VARIABLE then
@ <JEQ L518, <LOADW, <GLOBAL _token>>, <CONST 2>>
@ <JUMP L519>
@ <LABEL L518>
@     t := VarRep(tokval); Eat(VARIABLE)
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _VarRep>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _tokval>>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 2>>>
@ <JUMP L520>
@ <LABEL L519>
@   elsif token = NUMBER then
@ <JEQ L521, <LOADW, <GLOBAL _token>>, <CONST 3>>
@ <JUMP L522>
@ <LABEL L521>
@     t := MakeInt(tokival); Eat(NUMBER)
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _MakeInt>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _tokival>>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 3>>>
@ <JUMP L523>
@ <LABEL L522>
@   elsif token = CHCON then
@ <JEQ L524, <LOADW, <GLOBAL _token>>, <CONST 4>>
@ <JUMP L525>
@ <LABEL L524>
@     t := MakeChar(chr(tokival)); Eat(CHCON)
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _MakeChar>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _tokival>>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 4>>>
@ <JUMP L526>
@ <LABEL L525>
@   elsif token = STRCON then
@ <JEQ L527, <LOADW, <GLOBAL _token>>, <CONST 5>>
@ <JUMP L528>
@ <LABEL L527>
@     t := MakeString(toksval); Eat(STRCON)
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _MakeString>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <GLOBAL _toksval>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 5>>>
@ <JUMP L529>
@ <LABEL L528>
@   elsif token = LPAR then
@ <JEQ L530, <LOADW, <GLOBAL _token>>, <CONST 7>>
@ <JUMP L531>
@ <LABEL L530>
@     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 7>>>
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 8>>>
@ <JUMP L532>
@ <LABEL L531>
@   elsif token = LANGLE then
@ <JEQ L533, <LOADW, <GLOBAL _token>>, <CONST 15>>
@ <JUMP L534>
@ <LABEL L533>
@     t := ParseNode()
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseNode>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L535>
@ <LABEL L534>
@     if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
@ <JNEQ L537, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L536>
@ <LABEL L536>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g72>>,
@   <ARG 1, <CONST 15>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L538>
@ <LABEL L537>
@ <LABEL L538>
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L535>
@ <LABEL L532>
@ <LABEL L529>
@ <LABEL L526>
@ <LABEL L523>
@ <LABEL L520>
@ <LABEL L517>
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L514>
@ <LABEL L514>

@ After simplification:
@   if token = IDENT then t := ParseCompound()
@ <JNEQ L516, <LOADW, <GLOBAL _token>>, <CONST 1>>
@ <STOREW, <CALL 0, <GLOBAL _ParseCompound>>, <REGVAR 0>>
@ <JUMP L517>
@ <LABEL L516>
@   elsif token = VARIABLE then
@ <JNEQ L519, <LOADW, <GLOBAL _token>>, <CONST 2>>
@     t := VarRep(tokval); Eat(VARIABLE)
@ <STOREW,
@   <CALL 1, <GLOBAL _VarRep>, <ARG 0, <LOADW, <GLOBAL _tokval>>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 2>>>
@ <JUMP L517>
@ <LABEL L519>
@   elsif token = NUMBER then
@ <JNEQ L522, <LOADW, <GLOBAL _token>>, <CONST 3>>
@     t := MakeInt(tokival); Eat(NUMBER)
@ <STOREW,
@   <CALL 1, <GLOBAL _MakeInt>, <ARG 0, <LOADW, <GLOBAL _tokival>>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 3>>>
@ <JUMP L517>
@ <LABEL L522>
@   elsif token = CHCON then
@ <JNEQ L525, <LOADW, <GLOBAL _token>>, <CONST 4>>
@     t := MakeChar(chr(tokival)); Eat(CHCON)
@ <STOREW,
@   <CALL 1, <GLOBAL _MakeChar>, <ARG 0, <LOADW, <GLOBAL _tokival>>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 4>>>
@ <JUMP L517>
@ <LABEL L525>
@   elsif token = STRCON then
@ <JNEQ L528, <LOADW, <GLOBAL _token>>, <CONST 5>>
@     t := MakeString(toksval); Eat(STRCON)
@ <STOREW,
@   <CALL 1, <GLOBAL _MakeString>, <ARG 0, <GLOBAL _toksval>>>,
@   <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 5>>>
@ <JUMP L517>
@ <LABEL L528>
@   elsif token = LPAR then
@ <JNEQ L531, <LOADW, <GLOBAL _token>>, <CONST 7>>
@     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 7>>>
@ <STOREW, <CALL 0, <GLOBAL _ParseTerm>>, <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 8>>>
@ <JUMP L517>
@ <LABEL L531>
@   elsif token = LANGLE then
@ <JNEQ L534, <LOADW, <GLOBAL _token>>, <CONST 15>>
@     t := ParseNode()
@ <STOREW, <CALL 0, <GLOBAL _ParseNode>>, <REGVAR 0>>
@ <JUMP L517>
@ <LABEL L534>
@     if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
@ <JNEQ L538, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g72>>,
@   <ARG 1, <CONST 15>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L538>
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L517>
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   if token = IDENT then t := ParseCompound()
@ <JNEQ L516, <LOADW, <GLOBAL _token>>, <CONST 1>>
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseCompound>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@ <JUMP L517>
@ <LABEL L516>
@   elsif token = VARIABLE then
@ <JNEQ L519, <LOADW, <GLOBAL _token>>, <CONST 2>>
@     t := VarRep(tokval); Eat(VARIABLE)
@ <DEFTEMP 2,
@   <CALL 1, <GLOBAL _VarRep>, <ARG 0, <LOADW, <GLOBAL _tokval>>>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 2>>>
@ <JUMP L517>
@ <LABEL L519>
@   elsif token = NUMBER then
@ <JNEQ L522, <LOADW, <GLOBAL _token>>, <CONST 3>>
@     t := MakeInt(tokival); Eat(NUMBER)
@ <DEFTEMP 3,
@   <CALL 1, <GLOBAL _MakeInt>, <ARG 0, <LOADW, <GLOBAL _tokival>>>>>
@ <STOREW, <TEMP 3>, <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 3>>>
@ <JUMP L517>
@ <LABEL L522>
@   elsif token = CHCON then
@ <JNEQ L525, <LOADW, <GLOBAL _token>>, <CONST 4>>
@     t := MakeChar(chr(tokival)); Eat(CHCON)
@ <DEFTEMP 4,
@   <CALL 1, <GLOBAL _MakeChar>, <ARG 0, <LOADW, <GLOBAL _tokival>>>>>
@ <STOREW, <TEMP 4>, <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 4>>>
@ <JUMP L517>
@ <LABEL L525>
@   elsif token = STRCON then
@ <JNEQ L528, <LOADW, <GLOBAL _token>>, <CONST 5>>
@     t := MakeString(toksval); Eat(STRCON)
@ <DEFTEMP 5, <CALL 1, <GLOBAL _MakeString>, <ARG 0, <GLOBAL _toksval>>>>
@ <STOREW, <TEMP 5>, <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 5>>>
@ <JUMP L517>
@ <LABEL L528>
@   elsif token = LPAR then
@ <JNEQ L531, <LOADW, <GLOBAL _token>>, <CONST 7>>
@     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 7>>>
@ <DEFTEMP 6, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW, <TEMP 6>, <REGVAR 0>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 8>>>
@ <JUMP L517>
@ <LABEL L531>
@   elsif token = LANGLE then
@ <JNEQ L534, <LOADW, <GLOBAL _token>>, <CONST 15>>
@     t := ParseNode()
@ <DEFTEMP 7, <CALL 0, <GLOBAL _ParseNode>>>
@ <STOREW, <TEMP 7>, <REGVAR 0>>
@ <JUMP L517>
@ <LABEL L534>
@     if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
@ <JNEQ L538, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g72>>,
@   <ARG 1, <CONST 15>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L538>
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <LABEL L517>
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>

_ParsePrimary:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if token = IDENT then t := ParseCompound()
@ <JNEQ L516, <LOADW, <GLOBAL _token>>, <CONST 1>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #1
	bne .L516
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseCompound>>>
	bl _ParseCompound
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L517>
	b .L517
@ <LABEL L516>
.L516:
@   elsif token = VARIABLE then
@ <JNEQ L519, <LOADW, <GLOBAL _token>>, <CONST 2>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #2
	bne .L519
@     t := VarRep(tokval); Eat(VARIABLE)
@ <ARG 0, <LOADW, <GLOBAL _tokval>>>
	set r0, _tokval
	ldr r0, [r0]
@ <DEFTEMP 2, <CALL 1, <GLOBAL _VarRep>>>
	bl _VarRep
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <JUMP L517>
	b .L517
@ <LABEL L519>
.L519:
@   elsif token = NUMBER then
@ <JNEQ L522, <LOADW, <GLOBAL _token>>, <CONST 3>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #3
	bne .L522
@     t := MakeInt(tokival); Eat(NUMBER)
@ <ARG 0, <LOADW, <GLOBAL _tokival>>>
	set r0, _tokival
	ldr r0, [r0]
@ <DEFTEMP 3, <CALL 1, <GLOBAL _MakeInt>>>
	bl _MakeInt
@ <STOREW, <TEMP 3>, <REGVAR 0>>
	mov r4, r0
@ <ARG 0, <CONST 3>>
	set r0, #3
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <JUMP L517>
	b .L517
@ <LABEL L522>
.L522:
@   elsif token = CHCON then
@ <JNEQ L525, <LOADW, <GLOBAL _token>>, <CONST 4>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #4
	bne .L525
@     t := MakeChar(chr(tokival)); Eat(CHCON)
@ <ARG 0, <LOADW, <GLOBAL _tokival>>>
	set r0, _tokival
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 1, <GLOBAL _MakeChar>>>
	bl _MakeChar
@ <STOREW, <TEMP 4>, <REGVAR 0>>
	mov r4, r0
@ <ARG 0, <CONST 4>>
	set r0, #4
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <JUMP L517>
	b .L517
@ <LABEL L525>
.L525:
@   elsif token = STRCON then
@ <JNEQ L528, <LOADW, <GLOBAL _token>>, <CONST 5>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #5
	bne .L528
@     t := MakeString(toksval); Eat(STRCON)
@ <ARG 0, <GLOBAL _toksval>>
	set r0, _toksval
@ <DEFTEMP 5, <CALL 1, <GLOBAL _MakeString>>>
	bl _MakeString
@ <STOREW, <TEMP 5>, <REGVAR 0>>
	mov r4, r0
@ <ARG 0, <CONST 5>>
	set r0, #5
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <JUMP L517>
	b .L517
@ <LABEL L528>
.L528:
@   elsif token = LPAR then
@ <JNEQ L531, <LOADW, <GLOBAL _token>>, <CONST 7>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #7
	bne .L531
@     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
@ <ARG 0, <CONST 7>>
	set r0, #7
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <DEFTEMP 6, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW, <TEMP 6>, <REGVAR 0>>
	mov r4, r0
@ <ARG 0, <CONST 8>>
	set r0, #8
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <JUMP L517>
	b .L517
@ <LABEL L531>
.L531:
@   elsif token = LANGLE then
@ <JNEQ L534, <LOADW, <GLOBAL _token>>, <CONST 15>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #15
	bne .L534
@     t := ParseNode()
@ <DEFTEMP 7, <CALL 0, <GLOBAL _ParseNode>>>
	bl _ParseNode
@ <STOREW, <TEMP 7>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L517>
	b .L517
@ <LABEL L534>
.L534:
@     if not errflag then ShowError(); print_string("expected a term"); newline(); Recover() end; t := NULL
@ <JNEQ L538, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L538
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 15>>
	set r1, #15
@ <ARG 0, <GLOBAL g72>>
	set r0, g72
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <LABEL L538>
.L538:
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <LABEL L517>
.L517:
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParseNode(): term;
@ Initial code:
@   Eat(LANGLE);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 15>>>
@   tag := ParseTerm();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@   kids := ParseKids();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseKids>, <STATLINK, <CONST 0>>>,
@   <REGVAR 1>>
@   Eat(RANGLE);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 16>>>
@   return MakeNode(node, tag, kids)
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _node>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOADW, <REGVAR 1>>>>>
@ <JUMP L539>
@ <LABEL L539>

@ After simplification:
@   Eat(LANGLE);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 15>>>
@   tag := ParseTerm();
@ <STOREW, <CALL 0, <GLOBAL _ParseTerm>>, <REGVAR 0>>
@   kids := ParseKids();
@ <STOREW, <CALL 0, <GLOBAL _ParseKids>>, <REGVAR 1>>
@   Eat(RANGLE);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 16>>>
@   return MakeNode(node, tag, kids)
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _node>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOADW, <REGVAR 1>>>>>

@ After sharing:
@   Eat(LANGLE);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 15>>>
@   tag := ParseTerm();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   kids := ParseKids();
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseKids>>>
@ <STOREW, <TEMP 2>, <REGVAR 1>>
@   Eat(RANGLE);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 16>>>
@   return MakeNode(node, tag, kids)
@ <DEFTEMP 3,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _node>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOADW, <REGVAR 1>>>>>
@ <RESULTW, <TEMP 3>>

_ParseNode:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   Eat(LANGLE);
@ <ARG 0, <CONST 15>>
	set r0, #15
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@   tag := ParseTerm();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   kids := ParseKids();
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseKids>>>
	bl _ParseKids
@ <STOREW, <TEMP 2>, <REGVAR 1>>
	mov r5, r0
@   Eat(RANGLE);
@ <ARG 0, <CONST 16>>
	set r0, #16
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@   return MakeNode(node, tag, kids)
@ <ARG 2, <LOADW, <REGVAR 1>>>
	mov r2, r5
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0, <LOADW, <GLOBAL _node>>>
	set r0, _node
	ldr r0, [r0]
@ <DEFTEMP 3, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <RESULTW, <TEMP 3>>
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParseKids(): term;
@ Initial code:
@   if token <> COMMA then
@ <JNEQ L541, <LOADW, <GLOBAL _token>>, <CONST 9>>
@ <JUMP L542>
@ <LABEL L541>
@     return MakeNode(nilsym, NULL, NULL)
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _nilsym>>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>>
@ <JUMP L540>
@ <JUMP L543>
@ <LABEL L542>
@     Eat(COMMA);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 9>>>
@     head := ParseTerm();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@     tail := ParseKids();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseKids>, <STATLINK, <CONST 0>>>,
@   <REGVAR 1>>
@     return MakeNode(cons, head, tail)
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOADW, <REGVAR 1>>>>>
@ <JUMP L540>
@ <LABEL L543>
@ <LABEL L540>

@ After simplification:
@   if token <> COMMA then
@ <JEQ L542, <LOADW, <GLOBAL _token>>, <CONST 9>>
@     return MakeNode(nilsym, NULL, NULL)
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _nilsym>>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>>
@ <JUMP L540>
@ <LABEL L542>
@     Eat(COMMA);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 9>>>
@     head := ParseTerm();
@ <STOREW, <CALL 0, <GLOBAL _ParseTerm>>, <REGVAR 0>>
@     tail := ParseKids();
@ <STOREW, <CALL 0, <GLOBAL _ParseKids>>, <REGVAR 1>>
@     return MakeNode(cons, head, tail)
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOADW, <REGVAR 1>>>>>
@ <LABEL L540>

@ After sharing:
@   if token <> COMMA then
@ <JEQ L542, <LOADW, <GLOBAL _token>>, <CONST 9>>
@     return MakeNode(nilsym, NULL, NULL)
@ <DEFTEMP 1,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _nilsym>>>,
@     <ARG 1, <CONST 0>>,
@     <ARG 2, <CONST 0>>>>
@ <RESULTW, <TEMP 1>>
@ <JUMP L540>
@ <LABEL L542>
@     Eat(COMMA);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 9>>>
@     head := ParseTerm();
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@     tail := ParseKids();
@ <DEFTEMP 3, <CALL 0, <GLOBAL _ParseKids>>>
@ <STOREW, <TEMP 3>, <REGVAR 1>>
@     return MakeNode(cons, head, tail)
@ <DEFTEMP 4,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOADW, <REGVAR 1>>>>>
@ <RESULTW, <TEMP 4>>
@ <LABEL L540>

_ParseKids:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if token <> COMMA then
@ <JEQ L542, <LOADW, <GLOBAL _token>>, <CONST 9>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #9
	beq .L542
@     return MakeNode(nilsym, NULL, NULL)
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <LOADW, <GLOBAL _nilsym>>>
	set r0, _nilsym
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <RESULTW, <TEMP 1>>
@ <JUMP L540>
	b .L540
@ <LABEL L542>
.L542:
@     Eat(COMMA);
@ <ARG 0, <CONST 9>>
	set r0, #9
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@     head := ParseTerm();
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@     tail := ParseKids();
@ <DEFTEMP 3, <CALL 0, <GLOBAL _ParseKids>>>
	bl _ParseKids
@ <STOREW, <TEMP 3>, <REGVAR 1>>
	mov r5, r0
@     return MakeNode(cons, head, tail)
@ <ARG 2, <LOADW, <REGVAR 1>>>
	mov r2, r5
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0, <LOADW, <GLOBAL _cons>>>
	set r0, _cons
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <RESULTW, <TEMP 4>>
@ <LABEL L540>
.L540:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParseFactor(): term;
@ Initial code:
@   t := ParsePrimary();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParsePrimary>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@   if token <> COLON then
@ <JNEQ L545, <LOADW, <GLOBAL _token>>, <CONST 11>>
@ <JUMP L546>
@ <LABEL L545>
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L544>
@ <JUMP L547>
@ <LABEL L546>
@     Eat(COLON);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 11>>>
@     return MakeNode(cons, t, ParseFactor())
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <CALL 0, <GLOBAL _ParseFactor>, <STATLINK, <CONST 0>>>>>>
@ <JUMP L544>
@ <LABEL L547>
@ <LABEL L544>

@ After simplification:
@   t := ParsePrimary();
@ <STOREW, <CALL 0, <GLOBAL _ParsePrimary>>, <REGVAR 0>>
@   if token <> COLON then
@ <JEQ L546, <LOADW, <GLOBAL _token>>, <CONST 11>>
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L544>
@ <LABEL L546>
@     Eat(COLON);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 11>>>
@     return MakeNode(cons, t, ParseFactor())
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <CALL 0, <GLOBAL _ParseFactor>>>>>
@ <LABEL L544>

@ After sharing:
@   t := ParsePrimary();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParsePrimary>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   if token <> COLON then
@ <JEQ L546, <LOADW, <GLOBAL _token>>, <CONST 11>>
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L544>
@ <LABEL L546>
@     Eat(COLON);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 11>>>
@     return MakeNode(cons, t, ParseFactor())
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseFactor>>>
@ <DEFTEMP 3,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _cons>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <TEMP 2>>>>
@ <RESULTW, <TEMP 3>>
@ <LABEL L544>

_ParseFactor:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t := ParsePrimary();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParsePrimary>>>
	bl _ParsePrimary
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   if token <> COLON then
@ <JEQ L546, <LOADW, <GLOBAL _token>>, <CONST 11>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #11
	beq .L546
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <JUMP L544>
	b .L544
@ <LABEL L546>
.L546:
@     Eat(COLON);
@ <ARG 0, <CONST 11>>
	set r0, #11
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@     return MakeNode(cons, t, ParseFactor())
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseFactor>>>
	bl _ParseFactor
@ <ARG 2, <TEMP 2>>
	mov r2, r0
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0, <LOADW, <GLOBAL _cons>>>
	set r0, _cons
	ldr r0, [r0]
@ <DEFTEMP 3, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <RESULTW, <TEMP 3>>
@ <LABEL L544>
.L544:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParseTerm(): term;
@ Initial code:
@   t := ParseFactor();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseFactor>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@   if token <> EQUAL then
@ <JNEQ L549, <LOADW, <GLOBAL _token>>, <CONST 12>>
@ <JUMP L550>
@ <LABEL L549>
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L548>
@ <JUMP L551>
@ <LABEL L550>
@     Eat(EQUAL);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 12>>>
@     return MakeNode(eqsym, t, ParseFactor())
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _eqsym>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <CALL 0, <GLOBAL _ParseFactor>, <STATLINK, <CONST 0>>>>>>
@ <JUMP L548>
@ <LABEL L551>
@ <LABEL L548>

@ After simplification:
@   t := ParseFactor();
@ <STOREW, <CALL 0, <GLOBAL _ParseFactor>>, <REGVAR 0>>
@   if token <> EQUAL then
@ <JEQ L550, <LOADW, <GLOBAL _token>>, <CONST 12>>
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L548>
@ <LABEL L550>
@     Eat(EQUAL);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 12>>>
@     return MakeNode(eqsym, t, ParseFactor())
@ <RESULTW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _eqsym>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <CALL 0, <GLOBAL _ParseFactor>>>>>
@ <LABEL L548>

@ After sharing:
@   t := ParseFactor();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseFactor>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   if token <> EQUAL then
@ <JEQ L550, <LOADW, <GLOBAL _token>>, <CONST 12>>
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L548>
@ <LABEL L550>
@     Eat(EQUAL);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 12>>>
@     return MakeNode(eqsym, t, ParseFactor())
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseFactor>>>
@ <DEFTEMP 3,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _eqsym>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <TEMP 2>>>>
@ <RESULTW, <TEMP 3>>
@ <LABEL L548>

_ParseTerm:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t := ParseFactor();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseFactor>>>
	bl _ParseFactor
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   if token <> EQUAL then
@ <JEQ L550, <LOADW, <GLOBAL _token>>, <CONST 12>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #12
	beq .L550
@     return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <JUMP L548>
	b .L548
@ <LABEL L550>
.L550:
@     Eat(EQUAL);
@ <ARG 0, <CONST 12>>
	set r0, #12
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@     return MakeNode(eqsym, t, ParseFactor())
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseFactor>>>
	bl _ParseFactor
@ <ARG 2, <TEMP 2>>
	mov r2, r0
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0, <LOADW, <GLOBAL _eqsym>>>
	set r0, _eqsym
	ldr r0, [r0]
@ <DEFTEMP 3, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <RESULTW, <TEMP 3>>
@ <LABEL L548>
.L548:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc CheckAtom(a: term);
@ Initial code:
@   if lsr(mem[a], 8) <> FUNC then
@ <JNEQ L553,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JUMP L554>
@ <LABEL L553>
@     if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
@ <JNEQ L557, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L556>
@ <LABEL L556>
@ <CALL 0, <GLOBAL _ShowError>, <STATLINK, <CONST 0>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g73>>,
@   <ARG 1, <CONST 31>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>, <STATLINK, <CONST 0>>>
@ <JUMP L558>
@ <LABEL L557>
@ <LABEL L558>
@ <JUMP L555>
@ <LABEL L554>
@ <LABEL L555>
@ <LABEL L552>

@ After simplification:
@   if lsr(mem[a], 8) <> FUNC then
@ <JEQ L552,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@     if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
@ <JNEQ L552, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g73>>,
@   <ARG 1, <CONST 31>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L552>

@ After sharing:
@   if lsr(mem[a], 8) <> FUNC then
@ <JEQ L552,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@     if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
@ <JNEQ L552, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <CALL 0, <GLOBAL _ShowError>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g73>>,
@   <ARG 1, <CONST 31>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL _Recover>>
@ <LABEL L552>

_CheckAtom:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if lsr(mem[a], 8) <> FUNC then
@ <JEQ L552,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L552
@     if not errflag then ShowError(); print_string("literal must be a compound term"); newline(); Recover() end
@ <JNEQ L552, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	bne .L552
@ <CALL 0, <GLOBAL _ShowError>>
	bl _ShowError
@ <ARG 1, <CONST 31>>
	set r1, #31
@ <ARG 0, <GLOBAL g73>>
	set r0, g73
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL _Recover>>
	bl _Recover
@ <LABEL L552>
.L552:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ParseClause(): clause;
@ Initial code:
@   if token = HASH then
@ <JEQ L560, <LOADW, <GLOBAL _token>>, <CONST 17>>
@ <JUMP L561>
@ <LABEL L560>
@     Eat(HASH); head := NULL
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 17>>>
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <JUMP L562>
@ <LABEL L561>
@     head := ParseTerm();
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@     CheckAtom(head)
@ <CALL 1,
@   <GLOBAL _CheckAtom>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <REGVAR 0>>>>
@ <LABEL L562>
@   Eat(ARROW);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 6>>>
@   n := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
@   if token <> DOT then
@ <JNEQ L563, <LOADW, <GLOBAL _token>>, <CONST 10>>
@ <JUMP L564>
@ <LABEL L563>
@     more := true;
@ <STOREC, <CONST 1>, <OFFSET, <LOCAL 0>, <CONST -258>>>
@     while more do
@ <LABEL L566>
@ <JNEQ L567, <LOADC, <OFFSET, <LOCAL 0>, <CONST -258>>>, <CONST 0>>
@ <JUMP L568>
@ <LABEL L567>
@       n := n+1; minus := false;
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <STOREC, <CONST 0>, <OFFSET, <LOCAL 0>, <CONST -257>>>
@       if token = NEGATE then
@ <JEQ L569, <LOADW, <GLOBAL _token>>, <CONST 13>>
@ <JUMP L570>
@ <LABEL L569>
@ 	Eat(NEGATE); minus := true 
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 13>>>
@ <STOREC, <CONST 1>, <OFFSET, <LOCAL 0>, <CONST -257>>>
@ <JUMP L571>
@ <LABEL L570>
@ <LABEL L571>
@       t := ParseTerm(); CheckAtom(t);
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseTerm>, <STATLINK, <CONST 0>>>,
@   <REGVAR 1>>
@ <CALL 1,
@   <GLOBAL _CheckAtom>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <REGVAR 1>>>>
@       if minus then 
@ <JNEQ L572, <LOADC, <OFFSET, <LOCAL 0>, <CONST -257>>>, <CONST 0>>
@ <JUMP L573>
@ <LABEL L572>
@ 	body[n] := MakeNode(notsym, t, NULL)
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _notsym>>>,
@     <ARG 1, <LOADW, <REGVAR 1>>>,
@     <ARG 2, <CONST 0>>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -256>>,
@     <TIMES, <LOADW, <REGVAR 2>>, <CONST 4>>>>
@ <JUMP L574>
@ <LABEL L573>
@         body[n] := t
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET,
@     <OFFSET, <LOCAL 0>, <CONST -256>>,
@     <TIMES, <LOADW, <REGVAR 2>>, <CONST 4>>>>
@ <LABEL L574>
@       if token = COMMA then Eat(COMMA) else more := false end
@ <JEQ L575, <LOADW, <GLOBAL _token>>, <CONST 9>>
@ <JUMP L576>
@ <LABEL L575>
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 9>>>
@ <JUMP L577>
@ <LABEL L576>
@ <STOREC, <CONST 0>, <OFFSET, <LOCAL 0>, <CONST -258>>>
@ <LABEL L577>
@ <JUMP L566>
@ <LABEL L568>
@ <JUMP L565>
@ <LABEL L564>
@ <LABEL L565>
@   Eat(DOT);
@ <CALL 1, <GLOBAL _Eat>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 10>>>
@   if errflag then 
@ <JNEQ L578, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L579>
@ <LABEL L578>
@     return NULL
@ <RESULTW, <CONST 0>>
@ <JUMP L559>
@ <JUMP L580>
@ <LABEL L579>
@     return MakeClause(nvars, head, body, n)
@ <RESULTW,
@   <CALL 4,
@     <GLOBAL _MakeClause>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _nvars>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <OFFSET, <LOCAL 0>, <CONST -256>>>,
@     <ARG 3, <LOADW, <REGVAR 2>>>>>
@ <JUMP L559>
@ <LABEL L580>
@ <LABEL L559>

@ After simplification:
@   if token = HASH then
@ <JNEQ L561, <LOADW, <GLOBAL _token>>, <CONST 17>>
@     Eat(HASH); head := NULL
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 17>>>
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <JUMP L562>
@ <LABEL L561>
@     head := ParseTerm();
@ <STOREW, <CALL 0, <GLOBAL _ParseTerm>>, <REGVAR 0>>
@     CheckAtom(head)
@ <CALL 1, <GLOBAL _CheckAtom>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ <LABEL L562>
@   Eat(ARROW);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 6>>>
@   n := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
@   if token <> DOT then
@ <JEQ L565, <LOADW, <GLOBAL _token>>, <CONST 10>>
@     more := true;
@ <STOREC, <CONST 1>, <LOCAL -258>>
@ <LABEL L566>
@     while more do
@ <JEQ L565, <LOADC, <LOCAL -258>>, <CONST 0>>
@       n := n+1; minus := false;
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <STOREC, <CONST 0>, <LOCAL -257>>
@       if token = NEGATE then
@ <JNEQ L571, <LOADW, <GLOBAL _token>>, <CONST 13>>
@ 	Eat(NEGATE); minus := true 
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 13>>>
@ <STOREC, <CONST 1>, <LOCAL -257>>
@ <LABEL L571>
@       t := ParseTerm(); CheckAtom(t);
@ <STOREW, <CALL 0, <GLOBAL _ParseTerm>>, <REGVAR 1>>
@ <CALL 1, <GLOBAL _CheckAtom>, <ARG 0, <LOADW, <REGVAR 1>>>>
@       if minus then 
@ <JEQ L573, <LOADC, <LOCAL -257>>, <CONST 0>>
@ 	body[n] := MakeNode(notsym, t, NULL)
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _notsym>>>,
@     <ARG 1, <LOADW, <REGVAR 1>>>,
@     <ARG 2, <CONST 0>>>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 2>>, <CONST 2>>>>
@ <JUMP L574>
@ <LABEL L573>
@         body[n] := t
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 2>>, <CONST 2>>>>
@ <LABEL L574>
@       if token = COMMA then Eat(COMMA) else more := false end
@ <JNEQ L576, <LOADW, <GLOBAL _token>>, <CONST 9>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 9>>>
@ <JUMP L566>
@ <LABEL L576>
@ <STOREC, <CONST 0>, <LOCAL -258>>
@ <JUMP L566>
@ <LABEL L565>
@   Eat(DOT);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 10>>>
@   if errflag then 
@ <JEQ L579, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@     return NULL
@ <RESULTW, <CONST 0>>
@ <JUMP L559>
@ <LABEL L579>
@     return MakeClause(nvars, head, body, n)
@ <RESULTW,
@   <CALL 4,
@     <GLOBAL _MakeClause>,
@     <ARG 0, <LOADW, <GLOBAL _nvars>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOCAL -256>>,
@     <ARG 3, <LOADW, <REGVAR 2>>>>>
@ <LABEL L559>

@ After sharing:
@   if token = HASH then
@ <JNEQ L561, <LOADW, <GLOBAL _token>>, <CONST 17>>
@     Eat(HASH); head := NULL
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 17>>>
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <JUMP L562>
@ <LABEL L561>
@     head := ParseTerm();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@     CheckAtom(head)
@ <CALL 1, <GLOBAL _CheckAtom>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ <LABEL L562>
@   Eat(ARROW);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 6>>>
@   n := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
@   if token <> DOT then
@ <JEQ L565, <LOADW, <GLOBAL _token>>, <CONST 10>>
@     more := true;
@ <STOREC, <CONST 1>, <LOCAL -258>>
@ <LABEL L566>
@     while more do
@ <JEQ L565, <LOADC, <LOCAL -258>>, <CONST 0>>
@       n := n+1; minus := false;
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
@ <STOREC, <CONST 0>, <LOCAL -257>>
@       if token = NEGATE then
@ <JNEQ L571, <LOADW, <GLOBAL _token>>, <CONST 13>>
@ 	Eat(NEGATE); minus := true 
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 13>>>
@ <STOREC, <CONST 1>, <LOCAL -257>>
@ <LABEL L571>
@       t := ParseTerm(); CheckAtom(t);
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseTerm>>>
@ <STOREW, <TEMP 2>, <REGVAR 1>>
@ <CALL 1, <GLOBAL _CheckAtom>, <ARG 0, <LOADW, <REGVAR 1>>>>
@       if minus then 
@ <JEQ L573, <LOADC, <LOCAL -257>>, <CONST 0>>
@ 	body[n] := MakeNode(notsym, t, NULL)
@ <DEFTEMP 3,
@   <CALL 3,
@     <GLOBAL _MakeNode>,
@     <ARG 0, <LOADW, <GLOBAL _notsym>>>,
@     <ARG 1, <LOADW, <REGVAR 1>>>,
@     <ARG 2, <CONST 0>>>>
@ <STOREW,
@   <TEMP 3>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 2>>, <CONST 2>>>>
@ <JUMP L574>
@ <LABEL L573>
@         body[n] := t
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 2>>, <CONST 2>>>>
@ <LABEL L574>
@       if token = COMMA then Eat(COMMA) else more := false end
@ <JNEQ L576, <LOADW, <GLOBAL _token>>, <CONST 9>>
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 9>>>
@ <JUMP L566>
@ <LABEL L576>
@ <STOREC, <CONST 0>, <LOCAL -258>>
@ <JUMP L566>
@ <LABEL L565>
@   Eat(DOT);
@ <CALL 1, <GLOBAL _Eat>, <ARG 0, <CONST 10>>>
@   if errflag then 
@ <JEQ L579, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@     return NULL
@ <RESULTW, <CONST 0>>
@ <JUMP L559>
@ <LABEL L579>
@     return MakeClause(nvars, head, body, n)
@ <DEFTEMP 4,
@   <CALL 4,
@     <GLOBAL _MakeClause>,
@     <ARG 0, <LOADW, <GLOBAL _nvars>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2, <LOCAL -256>>,
@     <ARG 3, <LOADW, <REGVAR 2>>>>>
@ <RESULTW, <TEMP 4>>
@ <LABEL L559>

_ParseClause:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #264
@   if token = HASH then
@ <JNEQ L561, <LOADW, <GLOBAL _token>>, <CONST 17>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #17
	bne .L561
@     Eat(HASH); head := NULL
@ <ARG 0, <CONST 17>>
	set r0, #17
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <JUMP L562>
	b .L562
@ <LABEL L561>
.L561:
@     head := ParseTerm();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@     CheckAtom(head)
@ <ARG 0, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _CheckAtom>>
	bl _CheckAtom
@ <LABEL L562>
.L562:
@   Eat(ARROW);
@ <ARG 0, <CONST 6>>
	set r0, #6
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@   n := 0;
@ <STOREW, <CONST 0>, <REGVAR 2>>
	set r6, #0
@   if token <> DOT then
@ <JEQ L565, <LOADW, <GLOBAL _token>>, <CONST 10>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #10
	beq .L565
@     more := true;
@ <STOREC, <CONST 1>, <LOCAL -258>>
	set r0, #1
	set ip, #-258
	add r1, fp, ip
	strb r0, [r1]
@ <LABEL L566>
.L566:
@     while more do
@ <JEQ L565, <LOADC, <LOCAL -258>>, <CONST 0>>
	set ip, #-258
	add r0, fp, ip
	ldrb r0, [r0]
	cmp r0, #0
	beq .L565
@       n := n+1; minus := false;
@ <STOREW, <PLUS, <LOADW, <REGVAR 2>>, <CONST 1>>, <REGVAR 2>>
	add r6, r6, #1
@ <STOREC, <CONST 0>, <LOCAL -257>>
	set r0, #0
	set ip, #-257
	add r1, fp, ip
	strb r0, [r1]
@       if token = NEGATE then
@ <JNEQ L571, <LOADW, <GLOBAL _token>>, <CONST 13>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #13
	bne .L571
@ 	Eat(NEGATE); minus := true 
@ <ARG 0, <CONST 13>>
	set r0, #13
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <STOREC, <CONST 1>, <LOCAL -257>>
	set r0, #1
	set ip, #-257
	add r1, fp, ip
	strb r0, [r1]
@ <LABEL L571>
.L571:
@       t := ParseTerm(); CheckAtom(t);
@ <DEFTEMP 2, <CALL 0, <GLOBAL _ParseTerm>>>
	bl _ParseTerm
@ <STOREW, <TEMP 2>, <REGVAR 1>>
	mov r5, r0
@ <ARG 0, <LOADW, <REGVAR 1>>>
	mov r0, r5
@ <CALL 1, <GLOBAL _CheckAtom>>
	bl _CheckAtom
@       if minus then 
@ <JEQ L573, <LOADC, <LOCAL -257>>, <CONST 0>>
	set ip, #-257
	add r0, fp, ip
	ldrb r0, [r0]
	cmp r0, #0
	beq .L573
@ 	body[n] := MakeNode(notsym, t, NULL)
@ <ARG 2, <CONST 0>>
	set r2, #0
@ <ARG 1, <LOADW, <REGVAR 1>>>
	mov r1, r5
@ <ARG 0, <LOADW, <GLOBAL _notsym>>>
	set r0, _notsym
	ldr r0, [r0]
@ <DEFTEMP 3, <CALL 3, <GLOBAL _MakeNode>>>
	bl _MakeNode
@ <STOREW,
@   <TEMP 3>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 2>>, <CONST 2>>>>
	set ip, #-256
	add r1, fp, ip
	lsl r2, r6, #2
	add r1, r1, r2
	str r0, [r1]
@ <JUMP L574>
	b .L574
@ <LABEL L573>
.L573:
@         body[n] := t
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <LOCAL -256>, <LSL, <LOADW, <REGVAR 2>>, <CONST 2>>>>
	set ip, #-256
	add r0, fp, ip
	lsl r1, r6, #2
	add r0, r0, r1
	str r5, [r0]
@ <LABEL L574>
.L574:
@       if token = COMMA then Eat(COMMA) else more := false end
@ <JNEQ L576, <LOADW, <GLOBAL _token>>, <CONST 9>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #9
	bne .L576
@ <ARG 0, <CONST 9>>
	set r0, #9
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@ <JUMP L566>
	b .L566
@ <LABEL L576>
.L576:
@ <STOREC, <CONST 0>, <LOCAL -258>>
	set r0, #0
	set ip, #-258
	add r1, fp, ip
	strb r0, [r1]
@ <JUMP L566>
	b .L566
@ <LABEL L565>
.L565:
@   Eat(DOT);
@ <ARG 0, <CONST 10>>
	set r0, #10
@ <CALL 1, <GLOBAL _Eat>>
	bl _Eat
@   if errflag then 
@ <JEQ L579, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L579
@     return NULL
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L559>
	b .L559
@ <LABEL L579>
.L579:
@     return MakeClause(nvars, head, body, n)
@ <ARG 3, <LOADW, <REGVAR 2>>>
	mov r3, r6
@ <ARG 2, <LOCAL -256>>
	set ip, #-256
	add r2, fp, ip
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0, <LOADW, <GLOBAL _nvars>>>
	set r0, _nvars
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 4, <GLOBAL _MakeClause>>>
	bl _MakeClause
@ <RESULTW, <TEMP 4>>
@ <LABEL L559>
.L559:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ReadClause(): clause;
@ Initial code:
@   repeat
@ <LABEL L582>
@     hp := hmark; nvars := 0; errflag := false;
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
@ <STOREW, <CONST 0>, <GLOBAL _nvars>>
@ <STOREC, <CONST 0>, <GLOBAL _errflag>>
@     Scan();
@ <CALL 0, <GLOBAL _Scan>, <STATLINK, <CONST 0>>>
@     if token = EOFTOK then 
@ <JEQ L585, <LOADW, <GLOBAL _token>>, <CONST 14>>
@ <JUMP L586>
@ <LABEL L585>
@       c := NULL
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <JUMP L587>
@ <LABEL L586>
@       c := ParseClause()
@ <STOREW,
@   <CALL 0, <GLOBAL _ParseClause>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@ <LABEL L587>
@ <JNEQ L584, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JUMP L583>
@ <LABEL L584>
@ <JEQ L583, <LOADW, <GLOBAL _token>>, <CONST 14>>
@ <JUMP L582>
@ <LABEL L583>
@   return c
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L581>
@ <LABEL L581>

@ After simplification:
@ <LABEL L582>
@     hp := hmark; nvars := 0; errflag := false;
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
@ <STOREW, <CONST 0>, <GLOBAL _nvars>>
@ <STOREC, <CONST 0>, <GLOBAL _errflag>>
@     Scan();
@ <CALL 0, <GLOBAL _Scan>>
@     if token = EOFTOK then 
@ <JNEQ L586, <LOADW, <GLOBAL _token>>, <CONST 14>>
@       c := NULL
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <JUMP L587>
@ <LABEL L586>
@       c := ParseClause()
@ <STOREW, <CALL 0, <GLOBAL _ParseClause>>, <REGVAR 0>>
@ <LABEL L587>
@ <JEQ L583, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JNEQ L582, <LOADW, <GLOBAL _token>>, <CONST 14>>
@ <LABEL L583>
@   return c
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@ <LABEL L582>
@     hp := hmark; nvars := 0; errflag := false;
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
@ <STOREW, <CONST 0>, <GLOBAL _nvars>>
@ <STOREC, <CONST 0>, <GLOBAL _errflag>>
@     Scan();
@ <CALL 0, <GLOBAL _Scan>>
@     if token = EOFTOK then 
@ <JNEQ L586, <LOADW, <GLOBAL _token>>, <CONST 14>>
@       c := NULL
@ <STOREW, <CONST 0>, <REGVAR 0>>
@ <JUMP L587>
@ <LABEL L586>
@       c := ParseClause()
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseClause>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@ <LABEL L587>
@ <JEQ L583, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
@ <JNEQ L582, <LOADW, <GLOBAL _token>>, <CONST 14>>
@ <LABEL L583>
@   return c
@ <RESULTW, <LOADW, <REGVAR 0>>>

_ReadClause:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ <LABEL L582>
.L582:
@     hp := hmark; nvars := 0; errflag := false;
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
	set r0, _hmark
	ldr r0, [r0]
	set r1, _hp
	str r0, [r1]
@ <STOREW, <CONST 0>, <GLOBAL _nvars>>
	set r0, #0
	set r1, _nvars
	str r0, [r1]
@ <STOREC, <CONST 0>, <GLOBAL _errflag>>
	set r0, #0
	set r1, _errflag
	strb r0, [r1]
@     Scan();
@ <CALL 0, <GLOBAL _Scan>>
	bl _Scan
@     if token = EOFTOK then 
@ <JNEQ L586, <LOADW, <GLOBAL _token>>, <CONST 14>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #14
	bne .L586
@       c := NULL
@ <STOREW, <CONST 0>, <REGVAR 0>>
	set r4, #0
@ <JUMP L587>
	b .L587
@ <LABEL L586>
.L586:
@       c := ParseClause()
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ParseClause>>>
	bl _ParseClause
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@ <LABEL L587>
.L587:
@ <JEQ L583, <LOADC, <GLOBAL _errflag>>, <CONST 0>>
	set r0, _errflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L583
@ <JNEQ L582, <LOADW, <GLOBAL _token>>, <CONST 14>>
	set r0, _token
	ldr r0, [r0]
	cmp r0, #14
	bne .L582
@ <LABEL L583>
.L583:
@   return c
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Save(v: term);
@ Initial code:
@   if ((v < choice) or (v >= mem[choice+4])) then
@ <JLT L589,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW, <GLOBAL _choice>>>
@ <JUMP L592>
@ <LABEL L592>
@ <JGEQ L589,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 4>>, <CONST 4>>>>>
@ <JUMP L590>
@ <LABEL L589>
@     p := GloAlloc(UNDO, TRAIL_SIZE);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 6>>,
@     <ARG 1, <CONST 3>>>,
@   <REGVAR 0>>
@     mem[p+1] := v; mem[p+2] := trhead; trhead := p
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <GLOBAL _trhead>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 2>>, <CONST 4>>>>
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _trhead>>
@ <JUMP L591>
@ <LABEL L590>
@ <LABEL L591>
@ <LABEL L588>

@ After simplification:
@   if ((v < choice) or (v >= mem[choice+4])) then
@ <JLT L589, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _choice>>>
@ <JLT L588,
@   <LOADW, <LOCAL 40>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 16>>>>
@ <LABEL L589>
@     p := GloAlloc(UNDO, TRAIL_SIZE);
@ <STOREW,
@   <CALL 2, <GLOBAL _GloAlloc>, <ARG 0, <CONST 6>>, <ARG 1, <CONST 3>>>,
@   <REGVAR 0>>
@     mem[p+1] := v; mem[p+2] := trhead; trhead := p
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW,
@   <LOADW, <GLOBAL _trhead>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 8>>>
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _trhead>>
@ <LABEL L588>

@ After sharing:
@   if ((v < choice) or (v >= mem[choice+4])) then
@ <DEFTEMP 1, <LOADW, <LOCAL 40>>>
@ <DEFTEMP 2, <LOADW, <GLOBAL _choice>>>
@ <JLT L589, <TEMP 1>, <TEMP 2>>
@ <JLT L588,
@   <TEMP 1>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 2>, <CONST 2>>>,
@       <CONST 16>>>>
@ <LABEL L589>
@     p := GloAlloc(UNDO, TRAIL_SIZE);
@ <DEFTEMP 3,
@   <CALL 2, <GLOBAL _GloAlloc>, <ARG 0, <CONST 6>>, <ARG 1, <CONST 3>>>>
@ <STOREW, <TEMP 3>, <REGVAR 0>>
@     mem[p+1] := v; mem[p+2] := trhead; trhead := p
@ <DEFTEMP 4,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <LOADW, <LOCAL 40>>, <OFFSET, <TEMP 4>, <CONST 4>>>
@ <DEFTEMP 5, <GLOBAL _trhead>>
@ <STOREW, <LOADW, <TEMP 5>>, <OFFSET, <TEMP 4>, <CONST 8>>>
@ <STOREW, <LOADW, <REGVAR 0>>, <TEMP 5>>
@ <LABEL L588>

_Save:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if ((v < choice) or (v >= mem[choice+4])) then
@ <DEFTEMP 1, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r5, [r0]
@ <DEFTEMP 2, <LOADW, <GLOBAL _choice>>>
	set r0, _choice
	ldr r6, [r0]
@ <JLT L589, <TEMP 1>, <TEMP 2>>
	cmp r5, r6
	blt .L589
@ <JLT L588,
@   <TEMP 1>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 2>, <CONST 2>>>,
@       <CONST 16>>>>
	set r0, _mem
	lsl r1, r6, #2
	add r0, r0, r1
	add r0, r0, #16
	ldr r0, [r0]
	cmp r5, r0
	blt .L588
@ <LABEL L589>
.L589:
@     p := GloAlloc(UNDO, TRAIL_SIZE);
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <CONST 6>>
	set r0, #6
@ <DEFTEMP 3, <CALL 2, <GLOBAL _GloAlloc>>>
	bl _GloAlloc
@ <STOREW, <TEMP 3>, <REGVAR 0>>
	mov r4, r0
@     mem[p+1] := v; mem[p+2] := trhead; trhead := p
@ <DEFTEMP 4,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r5, r0, r1
@ <STOREW, <LOADW, <LOCAL 40>>, <OFFSET, <TEMP 4>, <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r5, #4
	str r0, [r1]
@ <DEFTEMP 5, <GLOBAL _trhead>>
	set r6, _trhead
@ <STOREW, <LOADW, <TEMP 5>>, <OFFSET, <TEMP 4>, <CONST 8>>>
	ldr r0, [r6]
	add r1, r5, #8
	str r0, [r1]
@ <STOREW, <LOADW, <REGVAR 0>>, <TEMP 5>>
	str r4, [r6]
@ <LABEL L588>
.L588:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Restore();
@ Initial code:
@   while (trhead <> mem[choice+5]) do
@ <LABEL L594>
@ <JNEQ L595,
@   <LOADW, <GLOBAL _trhead>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 5>>, <CONST 4>>>>>
@ <JUMP L596>
@ <LABEL L595>
@     v := mem[trhead+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _trhead>>, <CONST 1>>, <CONST 4>>>>,
@   <REGVAR 0>>
@     if v <> NULL then mem[v+1] := NULL end;
@ <JNEQ L597, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L598>
@ <LABEL L597>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <JUMP L599>
@ <LABEL L598>
@ <LABEL L599>
@     trhead := mem[trhead+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _trhead>>, <CONST 2>>, <CONST 4>>>>,
@   <GLOBAL _trhead>>
@ <JUMP L594>
@ <LABEL L596>
@ <LABEL L593>

@ After simplification:
@ <LABEL L594>
@   while (trhead <> mem[choice+5]) do
@ <JEQ L593,
@   <LOADW, <GLOBAL _trhead>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 20>>>>
@     v := mem[trhead+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _trhead>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <REGVAR 0>>
@     if v <> NULL then mem[v+1] := NULL end;
@ <JEQ L599, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L599>
@     trhead := mem[trhead+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _trhead>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <GLOBAL _trhead>>
@ <JUMP L594>
@ <LABEL L593>

@ After sharing:
@ <LABEL L594>
@   while (trhead <> mem[choice+5]) do
@ <DEFTEMP 1, <LOADW, <GLOBAL _trhead>>>
@ <DEFTEMP 2, <GLOBAL _mem>>
@ <JEQ L593,
@   <TEMP 1>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 2>, <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 20>>>>
@     v := mem[trhead+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET, <OFFSET, <TEMP 2>, <LSL, <TEMP 1>, <CONST 2>>>, <CONST 4>>>,
@   <REGVAR 0>>
@     if v <> NULL then mem[v+1] := NULL end;
@ <JEQ L599, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 2>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L599>
@     trhead := mem[trhead+2]
@ <DEFTEMP 3, <GLOBAL _trhead>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <TEMP 3>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <TEMP 3>>
@ <JUMP L594>
@ <LABEL L593>

_Restore:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ <LABEL L594>
.L594:
@   while (trhead <> mem[choice+5]) do
@ <DEFTEMP 1, <LOADW, <GLOBAL _trhead>>>
	set r0, _trhead
	ldr r5, [r0]
@ <DEFTEMP 2, <GLOBAL _mem>>
	set r6, _mem
@ <JEQ L593,
@   <TEMP 1>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 2>, <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 20>>>>
	set r0, _choice
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #20
	ldr r0, [r0]
	cmp r5, r0
	beq .L593
@     v := mem[trhead+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET, <OFFSET, <TEMP 2>, <LSL, <TEMP 1>, <CONST 2>>>, <CONST 4>>>,
@   <REGVAR 0>>
	lsl r0, r5, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r4, [r0]
@     if v <> NULL then mem[v+1] := NULL end;
@ <JEQ L599, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	beq .L599
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 2>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	set r0, #0
	lsl r1, r4, #2
	add r1, r6, r1
	add r1, r1, #4
	str r0, [r1]
@ <LABEL L599>
.L599:
@     trhead := mem[trhead+2]
@ <DEFTEMP 3, <GLOBAL _trhead>>
	set r5, _trhead
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <TEMP 3>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <TEMP 3>>
	set r0, _mem
	ldr r1, [r5]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #8
	ldr r0, [r0]
	str r0, [r5]
@ <JUMP L594>
	b .L594
@ <LABEL L593>
.L593:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Commit();
@ Initial code:
@   p := trhead;
@ <STOREW, <LOADW, <GLOBAL _trhead>>, <REGVAR 0>>
@   while (p <> NULL) and (p < mem[choice+4]) do
@ <LABEL L601>
@ <JNEQ L609, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L603>
@ <LABEL L609>
@ <JLT L602,
@   <LOADW, <REGVAR 0>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 4>>, <CONST 4>>>>>
@ <JUMP L603>
@ <LABEL L602>
@     if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
@ <JNEQ L607,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L605>
@ <LABEL L607>
@ <JLT L605,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>,
@   <LOADW, <GLOBAL _choice>>>
@ <JUMP L608>
@ <LABEL L608>
@ <JGEQ L605,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 4>>, <CONST 4>>>>>
@ <JUMP L604>
@ <LABEL L604>
@       mem[p+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ <JUMP L606>
@ <LABEL L605>
@ <LABEL L606>
@     p := mem[p+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 2>>, <CONST 4>>>>,
@   <REGVAR 0>>
@ <JUMP L601>
@ <LABEL L603>
@ <LABEL L600>

@ After simplification:
@   p := trhead;
@ <STOREW, <LOADW, <GLOBAL _trhead>>, <REGVAR 0>>
@ <LABEL L601>
@   while (p <> NULL) and (p < mem[choice+4]) do
@ <JEQ L600, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JGEQ L600,
@   <LOADW, <REGVAR 0>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 16>>>>
@     if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
@ <JEQ L606,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <JLT L606,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <GLOBAL _choice>>>
@ <JGEQ L606,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 16>>>>
@       mem[p+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L606>
@     p := mem[p+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <REGVAR 0>>
@ <JUMP L601>
@ <LABEL L600>

@ After sharing:
@   p := trhead;
@ <STOREW, <LOADW, <GLOBAL _trhead>>, <REGVAR 0>>
@ <LABEL L601>
@   while (p <> NULL) and (p < mem[choice+4]) do
@ <JEQ L600, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2, <LOADW, <GLOBAL _choice>>>
@ <DEFTEMP 3,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 1>, <LSL, <TEMP 2>, <CONST 2>>>,
@       <CONST 16>>>>
@ <JGEQ L600, <LOADW, <REGVAR 0>>, <TEMP 3>>
@     if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
@ <DEFTEMP 4,
@   <OFFSET,
@     <OFFSET, <TEMP 1>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ <DEFTEMP 5, <LOADW, <TEMP 4>>>
@ <JEQ L606, <TEMP 5>, <CONST 0>>
@ <JLT L606, <TEMP 5>, <TEMP 2>>
@ <JGEQ L606, <TEMP 5>, <TEMP 3>>
@       mem[p+1] := NULL
@ <STOREW, <CONST 0>, <TEMP 4>>
@ <LABEL L606>
@     p := mem[p+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <REGVAR 0>>
@ <JUMP L601>
@ <LABEL L600>

_Commit:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   p := trhead;
@ <STOREW, <LOADW, <GLOBAL _trhead>>, <REGVAR 0>>
	set r0, _trhead
	ldr r4, [r0]
@ <LABEL L601>
.L601:
@   while (p <> NULL) and (p < mem[choice+4]) do
@ <JEQ L600, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	beq .L600
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 2, <LOADW, <GLOBAL _choice>>>
	set r0, _choice
	ldr r6, [r0]
@ <DEFTEMP 3,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 1>, <LSL, <TEMP 2>, <CONST 2>>>,
@       <CONST 16>>>>
	lsl r0, r6, #2
	add r0, r5, r0
	add r0, r0, #16
	ldr r7, [r0]
@ <JGEQ L600, <LOADW, <REGVAR 0>>, <TEMP 3>>
	cmp r4, r7
	bge .L600
@     if (mem[p+1] <> NULL) and not ((mem[p+1] < choice) or (mem[p+1] >= mem[choice+4])) then
@ <DEFTEMP 4,
@   <OFFSET,
@     <OFFSET, <TEMP 1>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	lsl r0, r4, #2
	add r0, r5, r0
	add r5, r0, #4
@ <DEFTEMP 5, <LOADW, <TEMP 4>>>
	ldr r8, [r5]
@ <JEQ L606, <TEMP 5>, <CONST 0>>
	cmp r8, #0
	beq .L606
@ <JLT L606, <TEMP 5>, <TEMP 2>>
	cmp r8, r6
	blt .L606
@ <JGEQ L606, <TEMP 5>, <TEMP 3>>
	cmp r8, r7
	bge .L606
@       mem[p+1] := NULL
@ <STOREW, <CONST 0>, <TEMP 4>>
	set r0, #0
	str r0, [r5]
@ <LABEL L606>
.L606:
@     p := mem[p+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <REGVAR 0>>
	set r0, _mem
	lsl r1, r4, #2
	add r0, r0, r1
	add r0, r0, #8
	ldr r4, [r0]
@ <JUMP L601>
	b .L601
@ <LABEL L600>
.L600:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc GloCopy(t: term; e: frame): term;
@ Initial code:
@   t := Deref(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@   if (t >= gsp) then
@ <JGEQ L611,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW, <GLOBAL _gsp>>>
@ <JUMP L612>
@ <LABEL L611>
@     return t
@ <RESULTW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <JUMP L610>
@ <JUMP L613>
@ <LABEL L612>
@     case lsr(mem[t], 8) of
@ <JCASE 4 L614,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L616>
@ 	n := symtab[mem[t+1]].arity;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS,
@                   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                   <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@ 	if (t <= hp) and (n = 0) then 
@ <JLEQ L623,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW, <GLOBAL _hp>>>
@ <JUMP L619>
@ <LABEL L623>
@ <JEQ L618, <LOADW, <REGVAR 2>>, <CONST 0>>
@ <JUMP L619>
@ <LABEL L618>
@ 	  return t
@ <RESULTW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <JUMP L610>
@ <JUMP L620>
@ <LABEL L619>
@ 	  tt := GloAlloc(FUNC, TERM_SIZE+n);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 1>>,
@     <ARG 1, <PLUS, <CONST 2>, <LOADW, <REGVAR 2>>>>>,
@   <REGVAR 0>>
@ 	  mem[tt+1] := mem[t+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ 	  for i := 1 to n do
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <REGVAR 2>>, <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <LABEL L621>
@ <JGT L622,
@   <LOADW, <REGVAR 1>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>>
@ 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <LOADW, <REGVAR 1>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L621>
@ <LABEL L622>
@ 	  return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L610>
@ <LABEL L620>
@ <JUMP L615>
@ <LABEL L617>
@         tt := GloAlloc(CELL, TERM_SIZE);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 4>>,
@     <ARG 1, <CONST 2>>>,
@   <REGVAR 0>>
@         mem[tt+1] := NULL;
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@ 	Save(t); mem[t+1] := tt;
@ <CALL 1,
@   <GLOBAL _Save>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>
@ <STOREW,
@   <LOADW, <REGVAR 0>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@       <CONST 4>>>>
@         return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L610>
@ <JUMP L615>
@ <LABEL L614>
@       return t
@ <RESULTW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>
@ <JUMP L610>
@ <LABEL L615>
@ <LABEL L613>
@ <LABEL L610>

@ After simplification:
@   t := Deref(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@   if (t >= gsp) then
@ <JLT L612, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _gsp>>>
@     return t
@ <RESULTW, <LOADW, <LOCAL 40>>>
@ <JUMP L610>
@ <LABEL L612>
@     case lsr(mem[t], 8) of
@ <JCASE 4 L614,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L616>
@ 	n := symtab[mem[t+1]].arity;
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@ 	if (t <= hp) and (n = 0) then 
@ <JGT L619, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _hp>>>
@ <JNEQ L619, <LOADW, <REGVAR 2>>, <CONST 0>>
@ 	  return t
@ <RESULTW, <LOADW, <LOCAL 40>>>
@ <JUMP L610>
@ <LABEL L619>
@ 	  tt := GloAlloc(FUNC, TERM_SIZE+n);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloAlloc>,
@     <ARG 0, <CONST 1>>,
@     <ARG 1, <PLUS, <LOADW, <REGVAR 2>>, <CONST 2>>>>,
@   <REGVAR 0>>
@ 	  mem[tt+1] := mem[t+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ 	  for i := 1 to n do
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <REGVAR 2>>, <LOCAL -4>>
@ <LABEL L621>
@ <JGT L622, <LOADW, <REGVAR 1>>, <LOADW, <LOCAL -4>>>
@ 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L621>
@ <LABEL L622>
@ 	  return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L610>
@ <LABEL L617>
@         tt := GloAlloc(CELL, TERM_SIZE);
@ <STOREW,
@   <CALL 2, <GLOBAL _GloAlloc>, <ARG 0, <CONST 4>>, <ARG 1, <CONST 2>>>,
@   <REGVAR 0>>
@         mem[tt+1] := NULL;
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ 	Save(t); mem[t+1] := tt;
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 40>>>>
@ <STOREW,
@   <LOADW, <REGVAR 0>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
@         return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L610>
@ <LABEL L614>
@       return t
@ <RESULTW, <LOADW, <LOCAL 40>>>
@ <LABEL L610>

@ After sharing:
@   t := Deref(t, e);
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <LOCAL 40>>
@   if (t >= gsp) then
@ <JLT L612, <TEMP 1>, <LOADW, <GLOBAL _gsp>>>
@     return t
@ <RESULTW, <TEMP 1>>
@ <JUMP L610>
@ <LABEL L612>
@     case lsr(mem[t], 8) of
@ <JCASE 4 L614,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L616>
@ 	n := symtab[mem[t+1]].arity;
@ <DEFTEMP 2, <LOADW, <LOCAL 40>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 2>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
@ 	if (t <= hp) and (n = 0) then 
@ <JGT L619, <TEMP 2>, <LOADW, <GLOBAL _hp>>>
@ <JNEQ L619, <LOADW, <REGVAR 2>>, <CONST 0>>
@ 	  return t
@ <RESULTW, <TEMP 2>>
@ <JUMP L610>
@ <LABEL L619>
@ 	  tt := GloAlloc(FUNC, TERM_SIZE+n);
@ <DEFTEMP 3,
@   <CALL 2,
@     <GLOBAL _GloAlloc>,
@     <ARG 0, <CONST 1>>,
@     <ARG 1, <PLUS, <LOADW, <REGVAR 2>>, <CONST 2>>>>>
@ <STOREW, <TEMP 3>, <REGVAR 0>>
@ 	  mem[tt+1] := mem[t+1];
@ <DEFTEMP 4, <GLOBAL _mem>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <OFFSET,
@     <OFFSET, <TEMP 4>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ 	  for i := 1 to n do
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <REGVAR 2>>, <LOCAL -4>>
@ <LABEL L621>
@ <JGT L622, <LOADW, <REGVAR 1>>, <LOADW, <LOCAL -4>>>
@ 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
@ <DEFTEMP 5, <GLOBAL _mem>>
@ <DEFTEMP 6,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <TEMP 5>,
@             <LSL,
@               <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW,
@   <TEMP 6>,
@   <OFFSET,
@     <OFFSET,
@       <TEMP 5>,
@       <LSL,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L621>
@ <LABEL L622>
@ 	  return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L610>
@ <LABEL L617>
@         tt := GloAlloc(CELL, TERM_SIZE);
@ <DEFTEMP 7,
@   <CALL 2, <GLOBAL _GloAlloc>, <ARG 0, <CONST 4>>, <ARG 1, <CONST 2>>>>
@ <STOREW, <TEMP 7>, <REGVAR 0>>
@         mem[tt+1] := NULL;
@ <DEFTEMP 8, <GLOBAL _mem>>
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 8>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@ 	Save(t); mem[t+1] := tt;
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 40>>>>
@ <STOREW,
@   <LOADW, <REGVAR 0>>,
@   <OFFSET,
@     <OFFSET, <TEMP 8>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
@         return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L610>
@ <LABEL L614>
@       return t
@ <RESULTW, <LOADW, <LOCAL 40>>>
@ <LABEL L610>

_GloCopy:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   t := Deref(t, e);
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 1>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@   if (t >= gsp) then
@ <JLT L612, <TEMP 1>, <LOADW, <GLOBAL _gsp>>>
	set r1, _gsp
	ldr r1, [r1]
	cmp r0, r1
	blt .L612
@     return t
@ <RESULTW, <TEMP 1>>
@ <JUMP L610>
	b .L610
@ <LABEL L612>
.L612:
@     case lsr(mem[t], 8) of
@ <JCASE 4 L614,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #4
	ldrlo pc, [pc, r0, LSL #2]
	b .L614
	.word .L616
	.word .L614
	.word .L614
	.word .L617
@ <LABEL L616>
.L616:
@ 	n := symtab[mem[t+1]].arity;
@ <DEFTEMP 2, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r7, [r0]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 2>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 2>>
	set r0, _symtab
	set r1, _mem
	lsl r2, r7, #2
	add r1, r1, r2
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #4
	ldr r6, [r0]
@ 	if (t <= hp) and (n = 0) then 
@ <JGT L619, <TEMP 2>, <LOADW, <GLOBAL _hp>>>
	set r0, _hp
	ldr r0, [r0]
	cmp r7, r0
	bgt .L619
@ <JNEQ L619, <LOADW, <REGVAR 2>>, <CONST 0>>
	cmp r6, #0
	bne .L619
@ 	  return t
@ <RESULTW, <TEMP 2>>
	mov r0, r7
@ <JUMP L610>
	b .L610
@ <LABEL L619>
.L619:
@ 	  tt := GloAlloc(FUNC, TERM_SIZE+n);
@ <ARG 1, <PLUS, <LOADW, <REGVAR 2>>, <CONST 2>>>
	add r1, r6, #2
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <DEFTEMP 3, <CALL 2, <GLOBAL _GloAlloc>>>
	bl _GloAlloc
@ <STOREW, <TEMP 3>, <REGVAR 0>>
	mov r4, r0
@ 	  mem[tt+1] := mem[t+1];
@ <DEFTEMP 4, <GLOBAL _mem>>
	set r7, _mem
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <OFFSET,
@     <OFFSET, <TEMP 4>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #4
	ldr r0, [r0]
	lsl r1, r4, #2
	add r1, r7, r1
	add r1, r1, #4
	str r0, [r1]
@ 	  for i := 1 to n do
@ <STOREW, <CONST 1>, <REGVAR 1>>
	set r5, #1
@ <STOREW, <LOADW, <REGVAR 2>>, <LOCAL -4>>
	set ip, #-4
	add r0, fp, ip
	str r6, [r0]
@ <LABEL L621>
.L621:
@ <JGT L622, <LOADW, <REGVAR 1>>, <LOADW, <LOCAL -4>>>
	set ip, #-4
	add r0, fp, ip
	ldr r0, [r0]
	cmp r5, r0
	bgt .L622
@ 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
@ <DEFTEMP 5, <GLOBAL _mem>>
	set r7, _mem
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 5>,
@         <LSL,
@           <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 1>>>,
@           <CONST 2>>>,
@       <CONST 4>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r5
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 6, <CALL 2, <GLOBAL _GloCopy>>>
	bl _GloCopy
@ <STOREW,
@   <TEMP 6>,
@   <OFFSET,
@     <OFFSET,
@       <TEMP 5>,
@       <LSL,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
	add r1, r4, r5
	lsl r1, r1, #2
	add r1, r7, r1
	add r1, r1, #4
	str r0, [r1]
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L621>
	b .L621
@ <LABEL L622>
.L622:
@ 	  return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <JUMP L610>
	b .L610
@ <LABEL L617>
.L617:
@         tt := GloAlloc(CELL, TERM_SIZE);
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <CONST 4>>
	set r0, #4
@ <DEFTEMP 7, <CALL 2, <GLOBAL _GloAlloc>>>
	bl _GloAlloc
@ <STOREW, <TEMP 7>, <REGVAR 0>>
	mov r4, r0
@         mem[tt+1] := NULL;
@ <DEFTEMP 8, <GLOBAL _mem>>
	set r7, _mem
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 8>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	set r0, #0
	lsl r1, r4, #2
	add r1, r7, r1
	add r1, r1, #4
	str r0, [r1]
@ 	Save(t); mem[t+1] := tt;
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _Save>>
	bl _Save
@ <STOREW,
@   <LOADW, <REGVAR 0>>,
@   <OFFSET,
@     <OFFSET, <TEMP 8>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #4
	str r4, [r0]
@         return tt
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <JUMP L610>
	b .L610
@ <LABEL L614>
.L614:
@       return t
@ <RESULTW, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <LABEL L610>
.L610:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Share(v1, v2: term);
@ Initial code:
@   if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
@ <JLEQ L625,
@   <TIMES,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@     <MINUS,
@       <TIMES,
@         <CONST 2>,
@         <GEQ,
@           <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@           <LOADW, <GLOBAL _gsp>>>>,
@       <CONST 1>>>,
@   <TIMES,
@     <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@     <MINUS,
@       <TIMES,
@         <CONST 2>,
@         <GEQ,
@           <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@           <LOADW, <GLOBAL _gsp>>>>,
@       <CONST 1>>>>
@ <JUMP L626>
@ <LABEL L625>
@     Save(v1); mem[v1+1] := v2
@ <CALL 1,
@   <GLOBAL _Save>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@       <CONST 4>>>>
@ <JUMP L627>
@ <LABEL L626>
@     Save(v2); mem[v2+1] := v1 
@ <CALL 1,
@   <GLOBAL _Save>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>, <CONST 1>>,
@       <CONST 4>>>>
@ <LABEL L627>
@ <LABEL L624>

@ After simplification:
@   if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
@ <JGT L626,
@   <TIMES,
@     <LOADW, <LOCAL 40>>,
@     <MINUS,
@       <LSL,
@         <GEQ, <LOADW, <LOCAL 40>>, <LOADW, <GLOBAL _gsp>>>,
@         <CONST 1>>,
@       <CONST 1>>>,
@   <TIMES,
@     <LOADW, <LOCAL 44>>,
@     <MINUS,
@       <LSL,
@         <GEQ, <LOADW, <LOCAL 44>>, <LOADW, <GLOBAL _gsp>>>,
@         <CONST 1>>,
@       <CONST 1>>>>
@     Save(v1); mem[v1+1] := v2
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 40>>>>
@ <STOREW,
@   <LOADW, <LOCAL 44>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
@ <JUMP L624>
@ <LABEL L626>
@     Save(v2); mem[v2+1] := v1 
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 44>>>>
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 44>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L624>

@ After sharing:
@   if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
@ <DEFTEMP 1, <LOADW, <LOCAL 40>>>
@ <DEFTEMP 2, <LOADW, <GLOBAL _gsp>>>
@ <DEFTEMP 3, <LOADW, <LOCAL 44>>>
@ <JGT L626,
@   <TIMES,
@     <TEMP 1>,
@     <MINUS, <LSL, <GEQ, <TEMP 1>, <TEMP 2>>, <CONST 1>>, <CONST 1>>>,
@   <TIMES,
@     <TEMP 3>,
@     <MINUS, <LSL, <GEQ, <TEMP 3>, <TEMP 2>>, <CONST 1>>, <CONST 1>>>>
@     Save(v1); mem[v1+1] := v2
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <TEMP 1>>>
@ <STOREW,
@   <LOADW, <LOCAL 44>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
@ <JUMP L624>
@ <LABEL L626>
@     Save(v2); mem[v2+1] := v1 
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 44>>>>
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 44>>, <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L624>

_Share:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if (v1 * (2 * ord((v1 >= gsp)) - 1)) <= (v2 * (2 * ord((v2 >= gsp)) - 1)) then
@ <DEFTEMP 1, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r4, [r0]
@ <DEFTEMP 2, <LOADW, <GLOBAL _gsp>>>
	set r0, _gsp
	ldr r5, [r0]
@ <DEFTEMP 3, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r6, [r0]
@ <JGT L626,
@   <TIMES,
@     <TEMP 1>,
@     <MINUS, <LSL, <GEQ, <TEMP 1>, <TEMP 2>>, <CONST 1>>, <CONST 1>>>,
@   <TIMES,
@     <TEMP 3>,
@     <MINUS, <LSL, <GEQ, <TEMP 3>, <TEMP 2>>, <CONST 1>>, <CONST 1>>>>
	cmp r4, r5
	mov r0, #0
	movge r0, #1
	lsl r0, r0, #1
	sub r0, r0, #1
	mul r0, r4, r0
	cmp r6, r5
	mov r1, #0
	movge r1, #1
	lsl r1, r1, #1
	sub r1, r1, #1
	mul r1, r6, r1
	cmp r0, r1
	bgt .L626
@     Save(v1); mem[v1+1] := v2
@ <ARG 0, <TEMP 1>>
	mov r0, r4
@ <CALL 1, <GLOBAL _Save>>
	bl _Save
@ <STOREW,
@   <LOADW, <LOCAL 44>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _mem
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	str r0, [r1]
@ <JUMP L624>
	b .L624
@ <LABEL L626>
.L626:
@     Save(v2); mem[v2+1] := v1 
@ <ARG 0, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _Save>>
	bl _Save
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 44>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _mem
	set ip, #44
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	str r0, [r1]
@ <LABEL L624>
.L624:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Unify(t1: term; e1: frame; t2: term; e2: frame): boolean;
@ Initial code:
@   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 40>>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 52>>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 48>>>
@   if t1 = t2 then  (* Includes unifying a var with itself *)
@ <JEQ L629,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>
@ <JUMP L630>
@ <LABEL L629>
@     return true
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <JUMP L631>
@ <LABEL L630>
@   elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
@ <JEQ L656,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JUMP L633>
@ <LABEL L656>
@ <JEQ L632,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JUMP L633>
@ <LABEL L632>
@     Share(t1, t2); return true
@ <CALL 2,
@   <GLOBAL _Share>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@   <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <JUMP L634>
@ <LABEL L633>
@   elsif lsr(mem[t1], 8) = CELL then
@ <JEQ L635,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JUMP L636>
@ <LABEL L635>
@     Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
@ <CALL 1,
@   <GLOBAL _Save>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 52>>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@       <CONST 4>>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <JUMP L637>
@ <LABEL L636>
@   elsif lsr(mem[t2], 8) = CELL then
@ <JEQ L638,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JUMP L639>
@ <LABEL L638>
@     Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
@ <CALL 1,
@   <GLOBAL _Save>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>,
@       <CONST 4>>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <JUMP L640>
@ <LABEL L639>
@   elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
@ <JNEQ L641,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 4>>>>,
@     <CONST 8>>>
@ <JUMP L642>
@ <LABEL L641>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L628>
@ <JUMP L643>
@ <LABEL L642>
@     case lsr(mem[t1], 8) of
@ <JCASE 3 L644,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L646>
@         if (mem[t1+1] <> mem[t2+1]) then
@ <JNEQ L649,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>,
@         <CONST 4>>>>>
@ <JUMP L650>
@ <LABEL L649>
@           return false
@ <RESULTW, <CONST 0>>
@ <JUMP L628>
@ <JUMP L651>
@ <LABEL L650>
@           i := 1; match := true;
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREC, <CONST 1>, <REGVAR 1>>
@           while match and (i <= symtab[mem[t1+1]].arity) do
@ <LABEL L652>
@ <JNEQ L655, <LOADC, <REGVAR 1>>, <CONST 0>>
@ <JUMP L654>
@ <LABEL L655>
@ <JLEQ L653,
@   <LOADW, <REGVAR 0>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS,
@                   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                   <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 4>>>>
@ <JUMP L654>
@ <LABEL L653>
@             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <LOADW, <REGVAR 0>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>,
@                 <LOADW, <REGVAR 0>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 3, <LOADW, <OFFSET, <LOCAL 0>, <CONST 52>>>>>,
@   <REGVAR 1>>
@             i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L652>
@ <LABEL L654>
@           return match
@ <RESULTW, <LOADC, <REGVAR 1>>>
@ <JUMP L628>
@ <LABEL L651>
@ <JUMP L645>
@ <LABEL L647>
@         return (mem[t1+1] = mem[t2+1])
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@           <CONST 4>>>>,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>,
@           <CONST 4>>>>>>
@ <JUMP L628>
@ <JUMP L645>
@ <LABEL L648>
@         return (mem[t1+1] = mem[t2+1])
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>,
@           <CONST 4>>>>,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>,
@           <CONST 4>>>>>>
@ <JUMP L628>
@ <JUMP L645>
@ <LABEL L644>
@       newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g74>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g75>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L645>
@ <LABEL L643>
@ <LABEL L640>
@ <LABEL L637>
@ <LABEL L634>
@ <LABEL L631>
@ <LABEL L628>

@ After simplification:
@   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <LOCAL 40>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 48>>>,
@     <ARG 1, <LOADW, <LOCAL 52>>>>,
@   <LOCAL 48>>
@   if t1 = t2 then  (* Includes unifying a var with itself *)
@ <JNEQ L630, <LOADW, <LOCAL 40>>, <LOADW, <LOCAL 48>>>
@     return true
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L630>
@   elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
@ <JNEQ L633,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JNEQ L633,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@     Share(t1, t2); return true
@ <CALL 2,
@   <GLOBAL _Share>,
@   <ARG 0, <LOADW, <LOCAL 40>>>,
@   <ARG 1, <LOADW, <LOCAL 48>>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L633>
@   elsif lsr(mem[t1], 8) = CELL then
@ <JNEQ L636,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@     Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 40>>>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <LOCAL 48>>>,
@     <ARG 1, <LOADW, <LOCAL 52>>>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L636>
@   elsif lsr(mem[t2], 8) = CELL then
@ <JNEQ L639,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@     Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <LOADW, <LOCAL 48>>>>
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@     <CONST 4>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L639>
@   elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
@ <JEQ L642,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>>,
@     <CONST 8>>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L628>
@ <LABEL L642>
@     case lsr(mem[t1], 8) of
@ <JCASE 3 L644,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L646>
@         if (mem[t1+1] <> mem[t2+1]) then
@ <JEQ L650,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@       <CONST 4>>>>
@           return false
@ <RESULTW, <CONST 0>>
@ <JUMP L628>
@ <LABEL L650>
@           i := 1; match := true;
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREC, <CONST 1>, <REGVAR 1>>
@ <LABEL L652>
@           while match and (i <= symtab[mem[t1+1]].arity) do
@ <JEQ L654, <LOADC, <REGVAR 1>>, <CONST 0>>
@ <JGT L654,
@   <LOADW, <REGVAR 0>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>>
@             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <PLUS, <LOADW, <LOCAL 40>>, <LOADW, <REGVAR 0>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <PLUS, <LOADW, <LOCAL 48>>, <LOADW, <REGVAR 0>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 3, <LOADW, <LOCAL 52>>>>,
@   <REGVAR 1>>
@             i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L652>
@ <LABEL L654>
@           return match
@ <RESULTW, <LOADC, <REGVAR 1>>>
@ <JUMP L628>
@ <LABEL L647>
@         return (mem[t1+1] = mem[t2+1])
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L628>
@ <LABEL L648>
@         return (mem[t1+1] = mem[t2+1])
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L628>
@ <LABEL L644>
@       newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g74>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g75>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L628>

@ After sharing:
@   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <LOCAL 40>>
@ <DEFTEMP 2,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <LOCAL 48>>>,
@     <ARG 1, <LOADW, <LOCAL 52>>>>>
@ <STOREW, <TEMP 2>, <LOCAL 48>>
@   if t1 = t2 then  (* Includes unifying a var with itself *)
@ <JNEQ L630, <LOADW, <LOCAL 40>>, <TEMP 2>>
@     return true
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L630>
@   elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
@ <DEFTEMP 3, <GLOBAL _mem>>
@ <DEFTEMP 4, <LOADW, <LOCAL 40>>>
@ <JNEQ L633,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 3>, <LSL, <TEMP 4>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <DEFTEMP 5, <LOADW, <LOCAL 48>>>
@ <JNEQ L633,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 3>, <LSL, <TEMP 5>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@     Share(t1, t2); return true
@ <CALL 2, <GLOBAL _Share>, <ARG 0, <TEMP 4>>, <ARG 1, <TEMP 5>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L633>
@   elsif lsr(mem[t1], 8) = CELL then
@ <DEFTEMP 6, <GLOBAL _mem>>
@ <DEFTEMP 7, <LOADW, <LOCAL 40>>>
@ <JNEQ L636,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 6>, <LSL, <TEMP 7>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@     Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <TEMP 7>>>
@ <DEFTEMP 8,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <LOCAL 48>>>,
@     <ARG 1, <LOADW, <LOCAL 52>>>>>
@ <STOREW,
@   <TEMP 8>,
@   <OFFSET,
@     <OFFSET, <TEMP 6>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L636>
@   elsif lsr(mem[t2], 8) = CELL then
@ <DEFTEMP 9, <GLOBAL _mem>>
@ <DEFTEMP 10, <LOADW, <LOCAL 48>>>
@ <JNEQ L639,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 9>, <LSL, <TEMP 10>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@     Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
@ <CALL 1, <GLOBAL _Save>, <ARG 0, <TEMP 10>>>
@ <DEFTEMP 11,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW,
@   <TEMP 11>,
@   <OFFSET,
@     <OFFSET, <TEMP 9>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@     <CONST 4>>>
@ <RESULTW, <CONST 1>>
@ <JUMP L628>
@ <LABEL L639>
@   elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
@ <DEFTEMP 12, <GLOBAL _mem>>
@ <JEQ L642,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 12>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 12>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>>,
@     <CONST 8>>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L628>
@ <LABEL L642>
@     case lsr(mem[t1], 8) of
@ <JCASE 3 L644,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L646>
@         if (mem[t1+1] <> mem[t2+1]) then
@ <DEFTEMP 13, <GLOBAL _mem>>
@ <JEQ L650,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 13>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 13>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@       <CONST 4>>>>
@           return false
@ <RESULTW, <CONST 0>>
@ <JUMP L628>
@ <LABEL L650>
@           i := 1; match := true;
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREC, <CONST 1>, <REGVAR 1>>
@ <LABEL L652>
@           while match and (i <= symtab[mem[t1+1]].arity) do
@ <JEQ L654, <LOADC, <REGVAR 1>>, <CONST 0>>
@ <DEFTEMP 14, <GLOBAL _mem>>
@ <DEFTEMP 15, <LOADW, <LOCAL 40>>>
@ <JGT L654,
@   <LOADW, <REGVAR 0>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <TEMP 14>, <LSL, <TEMP 15>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>>
@             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
@ <DEFTEMP 16,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <TEMP 14>,
@             <LSL, <PLUS, <TEMP 15>, <LOADW, <REGVAR 0>>>, <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <TEMP 14>,
@             <LSL,
@               <PLUS, <LOADW, <LOCAL 48>>, <LOADW, <REGVAR 0>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 3, <LOADW, <LOCAL 52>>>>>
@ <STOREC, <TEMP 16>, <REGVAR 1>>
@             i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L652>
@ <LABEL L654>
@           return match
@ <RESULTW, <LOADC, <REGVAR 1>>>
@ <JUMP L628>
@ <LABEL L647>
@         return (mem[t1+1] = mem[t2+1])
@ <DEFTEMP 17, <GLOBAL _mem>>
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 17>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 17>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L628>
@ <LABEL L648>
@         return (mem[t1+1] = mem[t2+1])
@ <DEFTEMP 18, <GLOBAL _mem>>
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 18>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 18>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@         <CONST 4>>>>>
@ <JUMP L628>
@ <LABEL L644>
@       newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g74>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g75>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L628>

_Unify:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 1>, <LOCAL 40>>
	set ip, #40
	add r1, fp, ip
	str r0, [r1]
@ <ARG 1, <LOADW, <LOCAL 52>>>
	set ip, #52
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 2, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 2>, <LOCAL 48>>
	set ip, #48
	add r1, fp, ip
	str r0, [r1]
@   if t1 = t2 then  (* Includes unifying a var with itself *)
@ <JNEQ L630, <LOADW, <LOCAL 40>>, <TEMP 2>>
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	cmp r1, r0
	bne .L630
@     return true
@ <RESULTW, <CONST 1>>
	set r0, #1
@ <JUMP L628>
	b .L628
@ <LABEL L630>
.L630:
@   elsif (lsr(mem[t1], 8) = CELL) and (lsr(mem[t2], 8) = CELL) then
@ <DEFTEMP 3, <GLOBAL _mem>>
	set r6, _mem
@ <DEFTEMP 4, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r7, [r0]
@ <JNEQ L633,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 3>, <LSL, <TEMP 4>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
	lsl r0, r7, #2
	add r0, r6, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L633
@ <DEFTEMP 5, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r8, [r0]
@ <JNEQ L633,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 3>, <LSL, <TEMP 5>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
	lsl r0, r8, #2
	add r0, r6, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L633
@     Share(t1, t2); return true
@ <ARG 1, <TEMP 5>>
	mov r1, r8
@ <ARG 0, <TEMP 4>>
	mov r0, r7
@ <CALL 2, <GLOBAL _Share>>
	bl _Share
@ <RESULTW, <CONST 1>>
	set r0, #1
@ <JUMP L628>
	b .L628
@ <LABEL L633>
.L633:
@   elsif lsr(mem[t1], 8) = CELL then
@ <DEFTEMP 6, <GLOBAL _mem>>
	set r6, _mem
@ <DEFTEMP 7, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r7, [r0]
@ <JNEQ L636,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 6>, <LSL, <TEMP 7>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
	lsl r0, r7, #2
	add r0, r6, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L636
@     Save(t1); mem[t1+1] := GloCopy(t2, e2); return true
@ <ARG 0, <TEMP 7>>
	mov r0, r7
@ <CALL 1, <GLOBAL _Save>>
	bl _Save
@ <ARG 1, <LOADW, <LOCAL 52>>>
	set ip, #52
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 8, <CALL 2, <GLOBAL _GloCopy>>>
	bl _GloCopy
@ <STOREW,
@   <TEMP 8>,
@   <OFFSET,
@     <OFFSET, <TEMP 6>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r1, r6, r1
	add r1, r1, #4
	str r0, [r1]
@ <RESULTW, <CONST 1>>
	set r0, #1
@ <JUMP L628>
	b .L628
@ <LABEL L636>
.L636:
@   elsif lsr(mem[t2], 8) = CELL then
@ <DEFTEMP 9, <GLOBAL _mem>>
	set r6, _mem
@ <DEFTEMP 10, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r7, [r0]
@ <JNEQ L639,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 9>, <LSL, <TEMP 10>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
	lsl r0, r7, #2
	add r0, r6, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L639
@     Save(t2); mem[t2+1] := GloCopy(t1, e1); return true
@ <ARG 0, <TEMP 10>>
	mov r0, r7
@ <CALL 1, <GLOBAL _Save>>
	bl _Save
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 11, <CALL 2, <GLOBAL _GloCopy>>>
	bl _GloCopy
@ <STOREW,
@   <TEMP 11>,
@   <OFFSET,
@     <OFFSET, <TEMP 9>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #48
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r1, r6, r1
	add r1, r1, #4
	str r0, [r1]
@ <RESULTW, <CONST 1>>
	set r0, #1
@ <JUMP L628>
	b .L628
@ <LABEL L639>
.L639:
@   elsif lsr(mem[t1], 8) <> lsr(mem[t2], 8) then
@ <DEFTEMP 12, <GLOBAL _mem>>
	set r6, _mem
@ <JEQ L642,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 12>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <LSR,
@     <LOADW, <OFFSET, <TEMP 12>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>>,
@     <CONST 8>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	set ip, #48
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r1, r6, r1
	ldr r1, [r1]
	lsr r1, r1, #8
	cmp r0, r1
	beq .L642
@     return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L628>
	b .L628
@ <LABEL L642>
.L642:
@     case lsr(mem[t1], 8) of
@ <JCASE 3 L644,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #3
	ldrlo pc, [pc, r0, LSL #2]
	b .L644
	.word .L646
	.word .L647
	.word .L648
@ <LABEL L646>
.L646:
@         if (mem[t1+1] <> mem[t2+1]) then
@ <DEFTEMP 13, <GLOBAL _mem>>
	set r6, _mem
@ <JEQ L650,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 13>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 13>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@       <CONST 4>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r0, [r0]
	set ip, #48
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r1, r6, r1
	add r1, r1, #4
	ldr r1, [r1]
	cmp r0, r1
	beq .L650
@           return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L628>
	b .L628
@ <LABEL L650>
.L650:
@           i := 1; match := true;
@ <STOREW, <CONST 1>, <REGVAR 0>>
	set r4, #1
@ <STOREC, <CONST 1>, <REGVAR 1>>
	set r5, #1
@ <LABEL L652>
.L652:
@           while match and (i <= symtab[mem[t1+1]].arity) do
@ <JEQ L654, <LOADC, <REGVAR 1>>, <CONST 0>>
	cmp r5, #0
	beq .L654
@ <DEFTEMP 14, <GLOBAL _mem>>
	set r6, _mem
@ <DEFTEMP 15, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r7, [r0]
@ <JGT L654,
@   <LOADW, <REGVAR 0>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <TEMP 14>, <LSL, <TEMP 15>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>>
	set r0, _symtab
	lsl r1, r7, #2
	add r1, r6, r1
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
	cmp r4, r0
	bgt .L654
@             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
@ <ARG 3, <LOADW, <LOCAL 52>>>
	set ip, #52
	add r0, fp, ip
	ldr r3, [r0]
@ <ARG 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 14>,
@         <LSL,
@           <PLUS, <LOADW, <LOCAL 48>>, <LOADW, <REGVAR 0>>>,
@           <CONST 2>>>,
@       <CONST 4>>>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, r4
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r2, [r0]
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <TEMP 14>,
@         <LSL, <PLUS, <TEMP 15>, <LOADW, <REGVAR 0>>>, <CONST 2>>>,
@       <CONST 4>>>>
	add r0, r7, r4
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 16, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 16>, <REGVAR 1>>
	mov r5, r0
@             i := i+1
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L652>
	b .L652
@ <LABEL L654>
.L654:
@           return match
@ <RESULTW, <LOADC, <REGVAR 1>>>
	mov r0, r5
@ <JUMP L628>
	b .L628
@ <LABEL L647>
.L647:
@         return (mem[t1+1] = mem[t2+1])
@ <DEFTEMP 17, <GLOBAL _mem>>
	set r6, _mem
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 17>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 17>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@         <CONST 4>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r0, [r0]
	set ip, #48
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r1, r6, r1
	add r1, r1, #4
	ldr r1, [r1]
	cmp r0, r1
	mov r0, #0
	moveq r0, #1
@ <JUMP L628>
	b .L628
@ <LABEL L648>
.L648:
@         return (mem[t1+1] = mem[t2+1])
@ <DEFTEMP 18, <GLOBAL _mem>>
	set r6, _mem
@ <RESULTW,
@   <EQ,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 18>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <TEMP 18>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@         <CONST 4>>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r0, [r0]
	set ip, #48
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r1, r6, r1
	add r1, r1, #4
	ldr r1, [r1]
	cmp r0, r1
	mov r0, #0
	moveq r0, #1
@ <JUMP L628>
	b .L628
@ <LABEL L644>
.L644:
@       newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g74>>
	set r0, g74
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g75>>
	set r0, g75
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L628>
.L628:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Key(t: term; e: frame): integer;
@ Initial code:
@   if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
@ <JEQ L658, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 0>>
@ <JUMP L659>
@ <LABEL L658>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g76>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g77>>,
@   <ARG 1, <CONST 3>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L660>
@ <LABEL L659>
@ <LABEL L660>
@   if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;
@ <JNEQ L661,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JUMP L662>
@ <LABEL L661>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g78>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g79>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <JUMP L663>
@ <LABEL L662>
@ <LABEL L663>
@   if symtab[mem[t+1]].arity = 0 then
@ <JEQ L664,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS,
@                   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                   <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <JUMP L665>
@ <LABEL L664>
@     return 0
@ <RESULTW, <CONST 0>>
@ <JUMP L657>
@ <JUMP L666>
@ <LABEL L665>
@     t0 := Deref(mem[t+1+1], e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@                 <CONST 1>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <REGVAR 0>>
@     case lsr(mem[t0], 8) of
@ <JCASE 3 L667,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L669>
@         FUNC:      return mem[t0+1]
@ <RESULTW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>>
@ <JUMP L657>
@ <JUMP L668>
@ <LABEL L670>
@       | INT:       return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>,
@     <CONST 1>>>
@ <JUMP L657>
@ <JUMP L668>
@ <LABEL L671>
@       | CHRCTR:    return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>,
@     <CONST 1>>>
@ <JUMP L657>
@ <JUMP L668>
@ <LABEL L667>
@       return 0
@ <RESULTW, <CONST 0>>
@ <JUMP L657>
@ <LABEL L668>
@ <LABEL L666>
@ <LABEL L657>

@ After simplification:
@   if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
@ <JNEQ L660, <LOADW, <LOCAL 40>>, <CONST 0>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g76>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g77>>,
@   <ARG 1, <CONST 3>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L660>
@   if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;
@ <JEQ L663,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g78>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g79>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L663>
@   if symtab[mem[t+1]].arity = 0 then
@ <JNEQ L665,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@     return 0
@ <RESULTW, <CONST 0>>
@ <JUMP L657>
@ <LABEL L665>
@     t0 := Deref(mem[t+1+1], e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 8>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <REGVAR 0>>
@     case lsr(mem[t0], 8) of
@ <JCASE 3 L667,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L669>
@         FUNC:      return mem[t0+1]
@ <RESULTW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>>
@ <JUMP L657>
@ <LABEL L670>
@       | INT:       return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <CONST 1>>>
@ <JUMP L657>
@ <LABEL L671>
@       | CHRCTR:    return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <CONST 1>>>
@ <JUMP L657>
@ <LABEL L667>
@       return 0
@ <RESULTW, <CONST 0>>
@ <LABEL L657>

@ After sharing:
@   if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
@ <JNEQ L660, <LOADW, <LOCAL 40>>, <CONST 0>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g76>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g77>>,
@   <ARG 1, <CONST 3>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L660>
@   if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;
@ <JEQ L663,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g78>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g79>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L663>
@   if symtab[mem[t+1]].arity = 0 then
@ <JNEQ L665,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@     return 0
@ <RESULTW, <CONST 0>>
@ <JUMP L657>
@ <LABEL L665>
@     t0 := Deref(mem[t+1+1], e);
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@           <CONST 8>>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 2>, <REGVAR 0>>
@     case lsr(mem[t0], 8) of
@ <JCASE 3 L667,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <TEMP 1>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
@ <LABEL L669>
@         FUNC:      return mem[t0+1]
@ <RESULTW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>>
@ <JUMP L657>
@ <LABEL L670>
@       | INT:       return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <CONST 1>>>
@ <JUMP L657>
@ <LABEL L671>
@       | CHRCTR:    return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <CONST 1>>>
@ <JUMP L657>
@ <LABEL L667>
@       return 0
@ <RESULTW, <CONST 0>>
@ <LABEL L657>

_Key:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if t = NULL then newline(); print_string("Panic: "); print_string("Key"); newline(); exit(2) end;
@ <JNEQ L660, <LOADW, <LOCAL 40>>, <CONST 0>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #0
	bne .L660
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g76>>
	set r0, g76
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 3>>
	set r1, #3
@ <ARG 0, <GLOBAL g77>>
	set r0, g77
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L660>
.L660:
@   if lsr(mem[t], 8) <> FUNC then newline(); print_string("Panic: "); print_string("bad tag" (*t_kind(t):1, " in ", "Key1"*)); newline(); exit(2) end;
@ <JEQ L663,
@   <LSR,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
	set r0, _mem
	set ip, #40
	add r1, fp, ip
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L663
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g78>>
	set r0, g78
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g79>>
	set r0, g79
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L663>
.L663:
@   if symtab[mem[t+1]].arity = 0 then
@ <JNEQ L665,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <CONST 0>>
	set r0, _symtab
	set r1, _mem
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
	cmp r0, #0
	bne .L665
@     return 0
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L657>
	b .L657
@ <LABEL L665>
.L665:
@     t0 := Deref(mem[t+1+1], e);
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r5, _mem
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 1>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>,
@       <CONST 8>>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #8
	ldr r0, [r0]
@ <DEFTEMP 2, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW, <TEMP 2>, <REGVAR 0>>
	mov r4, r0
@     case lsr(mem[t0], 8) of
@ <JCASE 3 L667,
@   <MINUS,
@     <LSR,
@       <LOADW,
@         <OFFSET, <TEMP 1>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 1>>>
	lsl r0, r4, #2
	add r0, r5, r0
	ldr r0, [r0]
	lsr r0, r0, #8
	sub r0, r0, #1
	cmp r0, #3
	ldrlo pc, [pc, r0, LSL #2]
	b .L667
	.word .L669
	.word .L670
	.word .L671
@ <LABEL L669>
.L669:
@         FUNC:      return mem[t0+1]
@ <RESULTW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 4>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
@ <JUMP L657>
	b .L657
@ <LABEL L670>
.L670:
@       | INT:       return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <CONST 1>>>
	set r0, _mem
	lsl r1, r4, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
	add r0, r0, #1
@ <JUMP L657>
	b .L657
@ <LABEL L671>
.L671:
@       | CHRCTR:    return mem[t0+1] + 1
@ <RESULTW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@         <CONST 4>>>,
@     <CONST 1>>>
	set r0, _mem
	lsl r1, r4, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
	add r0, r0, #1
@ <JUMP L657>
	b .L657
@ <LABEL L667>
.L667:
@       return 0
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <LABEL L657>
.L657:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Search(t: term; e: frame; p: clause): clause;
@ Initial code:
@   k := Key(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Key>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>>,
@     <ARG 1, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>>>,
@   <REGVAR 0>>
@   if k <> 0 then
@ <JNEQ L673, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L674>
@ <LABEL L673>
@     while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
@ <LABEL L676>
@ <JNEQ L680, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 0>>
@ <JUMP L678>
@ <LABEL L680>
@ <JNEQ L679,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L678>
@ <LABEL L679>
@ <JNEQ L677,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW, <REGVAR 0>>>
@ <JUMP L678>
@ <LABEL L677>
@       p := mem[p+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>, <CONST 2>>,
@         <CONST 4>>>>,
@   <OFFSET, <LOCAL 0>, <CONST 48>>>
@ <JUMP L676>
@ <LABEL L678>
@ <JUMP L675>
@ <LABEL L674>
@ <LABEL L675>
@   return p
@ <RESULTW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 48>>>>
@ <JUMP L672>
@ <LABEL L672>

@ After simplification:
@   k := Key(t, e);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Key>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>,
@   <REGVAR 0>>
@   if k <> 0 then
@ <JEQ L675, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <LABEL L676>
@     while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
@ <JEQ L675, <LOADW, <LOCAL 48>>, <CONST 0>>
@ <JEQ L675,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <JEQ L675,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW, <REGVAR 0>>>
@       p := mem[p+2]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 48>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <LOCAL 48>>
@ <JUMP L676>
@ <LABEL L675>
@   return p
@ <RESULTW, <LOADW, <LOCAL 48>>>

@ After sharing:
@   k := Key(t, e);
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Key>,
@     <ARG 0, <LOADW, <LOCAL 40>>>,
@     <ARG 1, <LOADW, <LOCAL 44>>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   if k <> 0 then
@ <JEQ L675, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <LABEL L676>
@     while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
@ <DEFTEMP 2, <LOADW, <LOCAL 48>>>
@ <JEQ L675, <TEMP 2>, <CONST 0>>
@ <DEFTEMP 3, <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 2>, <CONST 2>>>>
@ <DEFTEMP 4, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
@ <JEQ L675, <TEMP 4>, <CONST 0>>
@ <JEQ L675, <TEMP 4>, <LOADW, <REGVAR 0>>>
@       p := mem[p+2]
@ <STOREW, <LOADW, <OFFSET, <TEMP 3>, <CONST 8>>>, <LOCAL 48>>
@ <JUMP L676>
@ <LABEL L675>
@   return p
@ <RESULTW, <LOADW, <LOCAL 48>>>

_Search:
	mov ip, sp
	stmfd sp!, {r0-r3}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   k := Key(t, e);
@ <ARG 1, <LOADW, <LOCAL 44>>>
	set ip, #44
	add r0, fp, ip
	ldr r1, [r0]
@ <ARG 0, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Key>>>
	bl _Key
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   if k <> 0 then
@ <JEQ L675, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	beq .L675
@ <LABEL L676>
.L676:
@     while (p <> NULL) and (mem[p+1] <> 0) and (mem[p+1] <> k) do
@ <DEFTEMP 2, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r5, [r0]
@ <JEQ L675, <TEMP 2>, <CONST 0>>
	cmp r5, #0
	beq .L675
@ <DEFTEMP 3, <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 2>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r5, #2
	add r5, r0, r1
@ <DEFTEMP 4, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
	add r0, r5, #4
	ldr r6, [r0]
@ <JEQ L675, <TEMP 4>, <CONST 0>>
	cmp r6, #0
	beq .L675
@ <JEQ L675, <TEMP 4>, <LOADW, <REGVAR 0>>>
	cmp r6, r4
	beq .L675
@       p := mem[p+2]
@ <STOREW, <LOADW, <OFFSET, <TEMP 3>, <CONST 8>>>, <LOCAL 48>>
	add r0, r5, #8
	ldr r0, [r0]
	set ip, #48
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L676>
	b .L676
@ <LABEL L675>
.L675:
@   return p
@ <RESULTW, <LOADW, <LOCAL 48>>>
	set ip, #48
	add r0, fp, ip
	ldr r0, [r0]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc PushFrame(nvars: integer; retry: clause);
@ Initial code:
@   f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _LocAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <PLUS,
@         <CONST 7>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 2>>>>>,
@   <REGVAR 0>>
@   mem[f] := current; mem[f+1] := goalframe;
@ <STOREW,
@   <LOADW, <GLOBAL _current>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <GLOBAL _goalframe>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@   mem[f+2] := retry; mem[f+3] := choice;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 2>>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <GLOBAL _choice>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 3>>, <CONST 4>>>>
@   mem[f+4] := gsp; mem[f+5] := trhead;
@ <STOREW,
@   <LOADW, <GLOBAL _gsp>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 4>>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <GLOBAL _trhead>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 5>>, <CONST 4>>>>
@   mem[f+6] := nvars;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 6>>, <CONST 4>>>>
@   for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <REGVAR 2>>
@ <LABEL L682>
@ <JGT L683, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@     mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <STOREW,
@   <PLUS, <LSL, <CONST 4>, <CONST 8>>, <CONST 2>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@         <TIMES, <MINUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 2>>>,
@       <CONST 4>>>>
@     mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@           <TIMES, <MINUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 2>>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L682>
@ <LABEL L683>
@   goalframe := f;
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _goalframe>>
@   if retry <> NULL then choice := goalframe end
@ <JNEQ L684, <LOADW, <OFFSET, <LOCAL 0>, <CONST 44>>>, <CONST 0>>
@ <JUMP L685>
@ <LABEL L684>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _choice>>
@ <JUMP L686>
@ <LABEL L685>
@ <LABEL L686>
@ <LABEL L681>

@ After simplification:
@   f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _LocAlloc>,
@     <ARG 0, <PLUS, <LSL, <LOADW, <LOCAL 40>>, <CONST 1>>, <CONST 7>>>>,
@   <REGVAR 0>>
@   mem[f] := current; mem[f+1] := goalframe;
@ <STOREW,
@   <LOADW, <GLOBAL _current>>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW,
@   <LOADW, <GLOBAL _goalframe>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@   mem[f+2] := retry; mem[f+3] := choice;
@ <STOREW,
@   <LOADW, <LOCAL 44>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 8>>>
@ <STOREW,
@   <LOADW, <GLOBAL _choice>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 12>>>
@   mem[f+4] := gsp; mem[f+5] := trhead;
@ <STOREW,
@   <LOADW, <GLOBAL _gsp>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 16>>>
@ <STOREW,
@   <LOADW, <GLOBAL _trhead>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 20>>>
@   mem[f+6] := nvars;
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 24>>>
@   for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <LOADW, <LOCAL 40>>, <REGVAR 2>>
@ <LABEL L682>
@ <JGT L683, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@     mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <STOREW,
@   <CONST 1026>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@         <MINUS, <LSL, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 2>>>,
@       <CONST 2>>>>
@     mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@           <MINUS, <LSL, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 2>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L682>
@ <LABEL L683>
@   goalframe := f;
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _goalframe>>
@   if retry <> NULL then choice := goalframe end
@ <JEQ L681, <LOADW, <LOCAL 44>>, <CONST 0>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _choice>>
@ <LABEL L681>

@ After sharing:
@   f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
@ <DEFTEMP 1,
@   <CALL 1,
@     <GLOBAL _LocAlloc>,
@     <ARG 0, <PLUS, <LSL, <LOADW, <LOCAL 40>>, <CONST 1>>, <CONST 7>>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   mem[f] := current; mem[f+1] := goalframe;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <LOADW, <GLOBAL _current>>, <TEMP 2>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <OFFSET, <TEMP 2>, <CONST 4>>>
@   mem[f+2] := retry; mem[f+3] := choice;
@ <STOREW, <LOADW, <LOCAL 44>>, <OFFSET, <TEMP 2>, <CONST 8>>>
@ <STOREW, <LOADW, <GLOBAL _choice>>, <OFFSET, <TEMP 2>, <CONST 12>>>
@   mem[f+4] := gsp; mem[f+5] := trhead;
@ <STOREW, <LOADW, <GLOBAL _gsp>>, <OFFSET, <TEMP 2>, <CONST 16>>>
@ <STOREW, <LOADW, <GLOBAL _trhead>>, <OFFSET, <TEMP 2>, <CONST 20>>>
@   mem[f+6] := nvars;
@ <DEFTEMP 3, <LOADW, <LOCAL 40>>>
@ <STOREW, <TEMP 3>, <OFFSET, <TEMP 2>, <CONST 24>>>
@   for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 1>>
@ <STOREW, <TEMP 3>, <REGVAR 2>>
@ <LABEL L682>
@ <JGT L683, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
@     mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <DEFTEMP 4,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@         <MINUS, <LSL, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 2>>>,
@       <CONST 2>>>>
@ <STOREW, <CONST 1026>, <TEMP 4>>
@     mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 4>, <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
@ <JUMP L682>
@ <LABEL L683>
@   goalframe := f;
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _goalframe>>
@   if retry <> NULL then choice := goalframe end
@ <JEQ L681, <LOADW, <LOCAL 44>>, <CONST 0>>
@ <DEFTEMP 5, <LOADW, <GLOBAL _goalframe>>>
@ <STOREW, <TEMP 5>, <GLOBAL _choice>>
@ <LABEL L681>

_PushFrame:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   f := LocAlloc((FRAME_SIZE + (nvars)*TERM_SIZE));
@ <ARG 0, <PLUS, <LSL, <LOADW, <LOCAL 40>>, <CONST 1>>, <CONST 7>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	lsl r0, r0, #1
	add r0, r0, #7
@ <DEFTEMP 1, <CALL 1, <GLOBAL _LocAlloc>>>
	bl _LocAlloc
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   mem[f] := current; mem[f+1] := goalframe;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r7, r0, r1
@ <STOREW, <LOADW, <GLOBAL _current>>, <TEMP 2>>
	set r0, _current
	ldr r0, [r0]
	str r0, [r7]
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <OFFSET, <TEMP 2>, <CONST 4>>>
	set r0, _goalframe
	ldr r0, [r0]
	add r1, r7, #4
	str r0, [r1]
@   mem[f+2] := retry; mem[f+3] := choice;
@ <STOREW, <LOADW, <LOCAL 44>>, <OFFSET, <TEMP 2>, <CONST 8>>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	add r1, r7, #8
	str r0, [r1]
@ <STOREW, <LOADW, <GLOBAL _choice>>, <OFFSET, <TEMP 2>, <CONST 12>>>
	set r0, _choice
	ldr r0, [r0]
	add r1, r7, #12
	str r0, [r1]
@   mem[f+4] := gsp; mem[f+5] := trhead;
@ <STOREW, <LOADW, <GLOBAL _gsp>>, <OFFSET, <TEMP 2>, <CONST 16>>>
	set r0, _gsp
	ldr r0, [r0]
	add r1, r7, #16
	str r0, [r1]
@ <STOREW, <LOADW, <GLOBAL _trhead>>, <OFFSET, <TEMP 2>, <CONST 20>>>
	set r0, _trhead
	ldr r0, [r0]
	add r1, r7, #20
	str r0, [r1]
@   mem[f+6] := nvars;
@ <DEFTEMP 3, <LOADW, <LOCAL 40>>>
	set ip, #40
	add r0, fp, ip
	ldr r8, [r0]
@ <STOREW, <TEMP 3>, <OFFSET, <TEMP 2>, <CONST 24>>>
	add r0, r7, #24
	str r8, [r0]
@   for i := 1 to nvars do
@ <STOREW, <CONST 1>, <REGVAR 1>>
	set r5, #1
@ <STOREW, <TEMP 3>, <REGVAR 2>>
	mov r6, r8
@ <LABEL L682>
.L682:
@ <JGT L683, <LOADW, <REGVAR 1>>, <LOADW, <REGVAR 2>>>
	cmp r5, r6
	bgt .L683
@     mem[(f+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <DEFTEMP 4,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@         <MINUS, <LSL, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 2>>>,
@       <CONST 2>>>>
	set r0, _mem
	add r1, r4, #7
	lsl r2, r5, #1
	sub r2, r2, #2
	add r1, r1, r2
	lsl r1, r1, #2
	add r7, r0, r1
@ <STOREW, <CONST 1026>, <TEMP 4>>
	set r0, #1026
	str r0, [r7]
@     mem[(f+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW, <CONST 0>, <OFFSET, <TEMP 4>, <CONST 4>>>
	set r0, #0
	add r1, r7, #4
	str r0, [r1]
@ <STOREW, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <REGVAR 1>>
	add r5, r5, #1
@ <JUMP L682>
	b .L682
@ <LABEL L683>
.L683:
@   goalframe := f;
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _goalframe>>
	set r0, _goalframe
	str r4, [r0]
@   if retry <> NULL then choice := goalframe end
@ <JEQ L681, <LOADW, <LOCAL 44>>, <CONST 0>>
	set ip, #44
	add r0, fp, ip
	ldr r0, [r0]
	cmp r0, #0
	beq .L681
@ <DEFTEMP 5, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r7, [r0]
@ <STOREW, <TEMP 5>, <GLOBAL _choice>>
	set r0, _choice
	str r7, [r0]
@ <LABEL L681>
.L681:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc TroStep();
@ Initial code:
@   if dflag then print_string("(TRO)"); newline() end;
@ <JNEQ L688, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <JUMP L689>
@ <LABEL L688>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g80>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L690>
@ <LABEL L689>
@ <LABEL L690>
@   oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
@ <STOREW,
@   <PLUS,
@     <CONST 7>,
@     <TIMES,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 6>>,
@             <CONST 4>>>>,
@       <CONST 2>>>,
@   <REGVAR 1>>
@   newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
@ <STOREW,
@   <PLUS,
@     <CONST 7>,
@     <TIMES,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <GLOBAL _prok>>, <CONST 4>>>>,
@       <CONST 2>>>,
@   <REGVAR 2>>
@   temp := LocAlloc(newsize);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _LocAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <REGVAR 2>>>>,
@   <REGVAR 0>>
@   temp := goalframe + newsize; (* copy old frame here *)
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 2>>>,
@   <REGVAR 0>>
@   for i := 1 to oldsize do 
@ <STOREW, <CONST 1>, <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <STOREW, <LOADW, <REGVAR 1>>, <OFFSET, <LOCAL 0>, <CONST -8>>>
@ <LABEL L691>
@ <JGT L692,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -8>>>>
@     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <MINUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 1>>>,
@           <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>>,
@         <CONST 4>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <MINUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>>,
@       <CONST 4>>>>
@ <STOREW,
@   <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>, <CONST 1>>,
@   <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <JUMP L691>
@ <LABEL L692>
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 6>>,
@         <CONST 4>>>>,
@   <OFFSET, <LOCAL 0>, <CONST -12>>>
@ <LABEL L693>
@ <JGT L694,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -12>>>>
@     if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
@ <JEQ L700,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <TIMES,
@               <MINUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@                 <CONST 1>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JUMP L696>
@ <LABEL L700>
@ <JNEQ L699,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <TIMES,
@               <MINUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@                 <CONST 1>>,
@               <CONST 2>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L696>
@ <LABEL L699>
@ <JLEQ L698,
@   <LOADW, <GLOBAL _goalframe>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <TIMES,
@               <MINUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@                 <CONST 1>>,
@               <CONST 2>>>,
@           <CONST 1>>,
@         <CONST 4>>>>>
@ <JUMP L696>
@ <LABEL L698>
@ <JLT L695,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <TIMES,
@               <MINUS,
@                 <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@                 <CONST 1>>,
@               <CONST 2>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 1>>>>
@ <JUMP L696>
@ <LABEL L695>
@       mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
@ <STOREW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS,
@             <PLUS,
@               <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@               <TIMES,
@                 <MINUS,
@                   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@                   <CONST 1>>,
@                 <CONST 2>>>,
@             <CONST 1>>,
@           <CONST 4>>>>,
@     <LOADW, <REGVAR 2>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@           <TIMES,
@             <MINUS,
@               <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@               <CONST 1>>,
@             <CONST 2>>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@ <JUMP L697>
@ <LABEL L696>
@ <LABEL L697>
@ <STOREW,
@   <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>, <CONST 1>>,
@   <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <JUMP L693>
@ <LABEL L694>
@   mem[goalframe+6] := mem[prok];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <LOADW, <GLOBAL _prok>>, <CONST 4>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 6>>, <CONST 4>>>>
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 6>>,
@         <CONST 4>>>>,
@   <OFFSET, <LOCAL 0>, <CONST -16>>>
@ <LABEL L701>
@ <JGT L702,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST -16>>>>
@     mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <STOREW,
@   <PLUS, <LSL, <CONST 4>, <CONST 8>>, <CONST 2>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 7>>,
@         <TIMES,
@           <MINUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>, <CONST 1>>,
@           <CONST 2>>>,
@       <CONST 4>>>>
@     mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 7>>,
@           <TIMES,
@             <MINUS,
@               <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>,
@               <CONST 1>>,
@             <CONST 2>>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@ <STOREW,
@   <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST -4>>>, <CONST 1>>,
@   <OFFSET, <LOCAL 0>, <CONST -4>>>
@ <JUMP L701>
@ <LABEL L702>
@   ok := Unify(call, temp, mem[prok+3], goalframe);
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 3>>,
@             <CONST 4>>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _ok>>
@   current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 4>>, <GLOBAL _current>>
@   lsp := temp-1
@ <STOREW, <MINUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <GLOBAL _lsp>>
@ <LABEL L687>

@ After simplification:
@   if dflag then print_string("(TRO)"); newline() end;
@ <JEQ L690, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g80>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L690>
@   oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
@ <STOREW,
@   <PLUS,
@     <LSL,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@           <CONST 24>>>,
@       <CONST 1>>,
@     <CONST 7>>,
@   <REGVAR 1>>
@   newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
@ <STOREW,
@   <PLUS,
@     <LSL,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>,
@       <CONST 1>>,
@     <CONST 7>>,
@   <REGVAR 2>>
@   temp := LocAlloc(newsize);
@ <STOREW,
@   <CALL 1, <GLOBAL _LocAlloc>, <ARG 0, <LOADW, <REGVAR 2>>>>,
@   <REGVAR 0>>
@   temp := goalframe + newsize; (* copy old frame here *)
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 2>>>,
@   <REGVAR 0>>
@   for i := 1 to oldsize do 
@ <STOREW, <CONST 1>, <LOCAL -4>>
@ <STOREW, <LOADW, <REGVAR 1>>, <LOCAL -8>>
@ <LABEL L691>
@ <JGT L692, <LOADW, <LOCAL -4>>, <LOADW, <LOCAL -8>>>
@     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <MINUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 1>>>,
@           <LOADW, <LOCAL -4>>>,
@         <CONST 2>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <MINUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <LOADW, <LOCAL -4>>>,
@       <CONST 2>>>>
@ <STOREW, <PLUS, <LOADW, <LOCAL -4>>, <CONST 1>>, <LOCAL -4>>
@ <JUMP L691>
@ <LABEL L692>
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <LOCAL -4>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@       <CONST 24>>>,
@   <LOCAL -12>>
@ <LABEL L693>
@ <JGT L694, <LOADW, <LOCAL -4>>, <LOADW, <LOCAL -12>>>
@     if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
@ <JNEQ L697,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@           <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 4>>
@ <JEQ L697,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@           <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <JGT L697,
@   <LOADW, <GLOBAL _goalframe>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@           <CONST 2>>>,
@       <CONST 4>>>>
@ <JGEQ L697,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS,
@             <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@             <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@           <CONST 2>>>,
@       <CONST 4>>>,
@   <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 1>>>>
@       mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
@ <STOREW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL,
@             <PLUS,
@               <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@               <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@             <CONST 2>>>,
@         <CONST 4>>>,
@     <LOADW, <REGVAR 2>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS,
@           <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@           <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <LABEL L697>
@ <STOREW, <PLUS, <LOADW, <LOCAL -4>>, <CONST 1>>, <LOCAL -4>>
@ <JUMP L693>
@ <LABEL L694>
@   mem[goalframe+6] := mem[prok];
@ <STOREW,
@   <LOADW,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@     <CONST 24>>>
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <LOCAL -4>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@       <CONST 24>>>,
@   <LOCAL -16>>
@ <LABEL L701>
@ <JGT L702, <LOADW, <LOCAL -4>>, <LOADW, <LOCAL -16>>>
@     mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <STOREW,
@   <CONST 1026>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 7>>,
@         <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@       <CONST 2>>>>
@     mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL,
@         <PLUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 7>>,
@           <MINUS, <LSL, <LOADW, <LOCAL -4>>, <CONST 1>>, <CONST 2>>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <LOADW, <LOCAL -4>>, <CONST 1>>, <LOCAL -4>>
@ <JUMP L701>
@ <LABEL L702>
@   ok := Unify(call, temp, mem[prok+3], goalframe);
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _ok>>
@   current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 4>>, <GLOBAL _current>>
@   lsp := temp-1
@ <STOREW, <MINUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <GLOBAL _lsp>>

@ After sharing:
@   if dflag then print_string("(TRO)"); newline() end;
@ <JEQ L690, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g80>>,
@   <ARG 1, <CONST 5>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L690>
@   oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2, <GLOBAL _goalframe>>
@ <STOREW,
@   <PLUS,
@     <LSL,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@           <CONST 24>>>,
@       <CONST 1>>,
@     <CONST 7>>,
@   <REGVAR 1>>
@   newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
@ <STOREW,
@   <PLUS,
@     <LSL,
@       <LOADW,
@         <OFFSET, <TEMP 1>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>,
@       <CONST 1>>,
@     <CONST 7>>,
@   <REGVAR 2>>
@   temp := LocAlloc(newsize);
@ <DEFTEMP 3, <CALL 1, <GLOBAL _LocAlloc>, <ARG 0, <LOADW, <REGVAR 2>>>>>
@ <STOREW, <TEMP 3>, <REGVAR 0>>
@   temp := goalframe + newsize; (* copy old frame here *)
@ <STOREW, <PLUS, <LOADW, <TEMP 2>>, <LOADW, <REGVAR 2>>>, <REGVAR 0>>
@   for i := 1 to oldsize do 
@ <STOREW, <CONST 1>, <LOCAL -4>>
@ <STOREW, <LOADW, <REGVAR 1>>, <LOCAL -8>>
@ <LABEL L691>
@ <DEFTEMP 4, <LOADW, <LOCAL -4>>>
@ <JGT L692, <TEMP 4>, <LOADW, <LOCAL -8>>>
@     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
@ <DEFTEMP 5, <GLOBAL _mem>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <TEMP 5>,
@       <LSL,
@         <MINUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 1>>>,
@           <TEMP 4>>,
@         <CONST 2>>>>,
@   <OFFSET,
@     <TEMP 5>,
@     <LSL,
@       <MINUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <TEMP 4>>,
@       <CONST 2>>>>
@ <STOREW, <PLUS, <TEMP 4>, <CONST 1>>, <LOCAL -4>>
@ <JUMP L691>
@ <LABEL L692>
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <LOCAL -4>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@       <CONST 24>>>,
@   <LOCAL -12>>
@ <LABEL L693>
@ <DEFTEMP 6, <LOADW, <LOCAL -4>>>
@ <JGT L694, <TEMP 6>, <LOADW, <LOCAL -12>>>
@     if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
@ <DEFTEMP 7,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@         <MINUS, <LSL, <TEMP 6>, <CONST 1>>, <CONST 2>>>,
@       <CONST 2>>>>
@ <JNEQ L697, <LSR, <LOADW, <TEMP 7>>, <CONST 8>>, <CONST 4>>
@ <DEFTEMP 8, <OFFSET, <TEMP 7>, <CONST 4>>>
@ <DEFTEMP 9, <LOADW, <TEMP 8>>>
@ <JEQ L697, <TEMP 9>, <CONST 0>>
@ <DEFTEMP 10, <LOADW, <GLOBAL _goalframe>>>
@ <JGT L697, <TEMP 10>, <TEMP 9>>
@ <JGEQ L697, <TEMP 9>, <PLUS, <TEMP 10>, <LOADW, <REGVAR 1>>>>
@       mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
@ <STOREW, <PLUS, <TEMP 9>, <LOADW, <REGVAR 2>>>, <TEMP 8>>
@ <LABEL L697>
@ <STOREW, <PLUS, <LOADW, <LOCAL -4>>, <CONST 1>>, <LOCAL -4>>
@ <JUMP L693>
@ <LABEL L694>
@   mem[goalframe+6] := mem[prok];
@ <DEFTEMP 11, <GLOBAL _mem>>
@ <DEFTEMP 12, <GLOBAL _goalframe>>
@ <STOREW,
@   <LOADW,
@     <OFFSET, <TEMP 11>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET, <TEMP 11>, <LSL, <LOADW, <TEMP 12>>, <CONST 2>>>,
@     <CONST 24>>>
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <LOCAL -4>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 11>, <LSL, <LOADW, <TEMP 12>>, <CONST 2>>>,
@       <CONST 24>>>,
@   <LOCAL -16>>
@ <LABEL L701>
@ <DEFTEMP 13, <LOADW, <LOCAL -4>>>
@ <JGT L702, <TEMP 13>, <LOADW, <LOCAL -16>>>
@     mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <DEFTEMP 14, <GLOBAL _mem>>
@ <DEFTEMP 15, <GLOBAL _goalframe>>
@ <DEFTEMP 16, <MINUS, <LSL, <TEMP 13>, <CONST 1>>, <CONST 2>>>
@ <STOREW,
@   <CONST 1026>,
@   <OFFSET,
@     <TEMP 14>,
@     <LSL,
@       <PLUS, <PLUS, <LOADW, <TEMP 15>>, <CONST 7>>, <TEMP 16>>,
@       <CONST 2>>>>
@     mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <TEMP 14>,
@       <LSL,
@         <PLUS, <PLUS, <LOADW, <TEMP 15>>, <CONST 7>>, <TEMP 16>>,
@         <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW, <PLUS, <TEMP 13>, <CONST 1>>, <LOCAL -4>>
@ <JUMP L701>
@ <LABEL L702>
@   ok := Unify(call, temp, mem[prok+3], goalframe);
@ <DEFTEMP 17, <GLOBAL _prok>>
@ <DEFTEMP 18,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <REGVAR 0>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <TEMP 17>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>>
@ <STOREC, <TEMP 18>, <GLOBAL _ok>>
@   current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <TEMP 17>>, <CONST 4>>, <GLOBAL _current>>
@   lsp := temp-1
@ <STOREW, <MINUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <GLOBAL _lsp>>

_TroStep:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #16
@   if dflag then print_string("(TRO)"); newline() end;
@ <JEQ L690, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
	set r0, _dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L690
@ <ARG 1, <CONST 5>>
	set r1, #5
@ <ARG 0, <GLOBAL g80>>
	set r0, g80
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <LABEL L690>
.L690:
@   oldsize := (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE); (* size of old frame *)
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r7, _mem
@ <DEFTEMP 2, <GLOBAL _goalframe>>
	set r8, _goalframe
@ <STOREW,
@   <PLUS,
@     <LSL,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@           <CONST 24>>>,
@       <CONST 1>>,
@     <CONST 7>>,
@   <REGVAR 1>>
	ldr r0, [r8]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #24
	ldr r0, [r0]
	lsl r0, r0, #1
	add r5, r0, #7
@   newsize := (FRAME_SIZE + (mem[prok])*TERM_SIZE); (* size of new frame *)
@ <STOREW,
@   <PLUS,
@     <LSL,
@       <LOADW,
@         <OFFSET, <TEMP 1>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>,
@       <CONST 1>>,
@     <CONST 7>>,
@   <REGVAR 2>>
	set r0, _prok
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	ldr r0, [r0]
	lsl r0, r0, #1
	add r6, r0, #7
@   temp := LocAlloc(newsize);
@ <ARG 0, <LOADW, <REGVAR 2>>>
	mov r0, r6
@ <DEFTEMP 3, <CALL 1, <GLOBAL _LocAlloc>>>
	bl _LocAlloc
@ <STOREW, <TEMP 3>, <REGVAR 0>>
	mov r4, r0
@   temp := goalframe + newsize; (* copy old frame here *)
@ <STOREW, <PLUS, <LOADW, <TEMP 2>>, <LOADW, <REGVAR 2>>>, <REGVAR 0>>
	ldr r0, [r8]
	add r4, r0, r6
@   for i := 1 to oldsize do 
@ <STOREW, <CONST 1>, <LOCAL -4>>
	set r0, #1
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <STOREW, <LOADW, <REGVAR 1>>, <LOCAL -8>>
	set ip, #-8
	add r0, fp, ip
	str r5, [r0]
@ <LABEL L691>
.L691:
@ <DEFTEMP 4, <LOADW, <LOCAL -4>>>
	set ip, #-4
	add r0, fp, ip
	ldr r7, [r0]
@ <JGT L692, <TEMP 4>, <LOADW, <LOCAL -8>>>
	set ip, #-8
	add r0, fp, ip
	ldr r0, [r0]
	cmp r7, r0
	bgt .L692
@     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
@ <DEFTEMP 5, <GLOBAL _mem>>
	set r8, _mem
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <TEMP 5>,
@       <LSL,
@         <MINUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <LOADW, <REGVAR 1>>>,
@           <TEMP 4>>,
@         <CONST 2>>>>,
@   <OFFSET,
@     <TEMP 5>,
@     <LSL,
@       <MINUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>,
@         <TEMP 4>>,
@       <CONST 2>>>>
	set r0, _goalframe
	ldr r0, [r0]
	add r0, r0, r5
	sub r0, r0, r7
	lsl r0, r0, #2
	add r0, r8, r0
	ldr r0, [r0]
	add r1, r4, r5
	sub r1, r1, r7
	lsl r1, r1, #2
	add r1, r8, r1
	str r0, [r1]
@ <STOREW, <PLUS, <TEMP 4>, <CONST 1>>, <LOCAL -4>>
	add r0, r7, #1
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L691>
	b .L691
@ <LABEL L692>
.L692:
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <LOCAL -4>>
	set r0, #1
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@       <CONST 24>>>,
@   <LOCAL -12>>
	set r0, _mem
	set r1, _goalframe
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #24
	ldr r0, [r0]
	set ip, #-12
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L693>
.L693:
@ <DEFTEMP 6, <LOADW, <LOCAL -4>>>
	set ip, #-4
	add r0, fp, ip
	ldr r7, [r0]
@ <JGT L694, <TEMP 6>, <LOADW, <LOCAL -12>>>
	set ip, #-12
	add r0, fp, ip
	ldr r0, [r0]
	cmp r7, r0
	bgt .L694
@     if (lsr(mem[(temp+7+(i-1)*TERM_SIZE)], 8) = CELL)
@ <DEFTEMP 7,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL,
@       <PLUS,
@         <PLUS, <LOADW, <REGVAR 0>>, <CONST 7>>,
@         <MINUS, <LSL, <TEMP 6>, <CONST 1>>, <CONST 2>>>,
@       <CONST 2>>>>
	set r0, _mem
	add r1, r4, #7
	lsl r2, r7, #1
	sub r2, r2, #2
	add r1, r1, r2
	lsl r1, r1, #2
	add r7, r0, r1
@ <JNEQ L697, <LSR, <LOADW, <TEMP 7>>, <CONST 8>>, <CONST 4>>
	ldr r0, [r7]
	lsr r0, r0, #8
	cmp r0, #4
	bne .L697
@ <DEFTEMP 8, <OFFSET, <TEMP 7>, <CONST 4>>>
	add r7, r7, #4
@ <DEFTEMP 9, <LOADW, <TEMP 8>>>
	ldr r8, [r7]
@ <JEQ L697, <TEMP 9>, <CONST 0>>
	cmp r8, #0
	beq .L697
@ <DEFTEMP 10, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r9, [r0]
@ <JGT L697, <TEMP 10>, <TEMP 9>>
	cmp r9, r8
	bgt .L697
@ <JGEQ L697, <TEMP 9>, <PLUS, <TEMP 10>, <LOADW, <REGVAR 1>>>>
	add r0, r9, r5
	cmp r8, r0
	bge .L697
@       mem[(temp+7+(i-1)*TERM_SIZE)+1] := mem[(temp+7+(i-1)*TERM_SIZE)+1] + newsize
@ <STOREW, <PLUS, <TEMP 9>, <LOADW, <REGVAR 2>>>, <TEMP 8>>
	add r0, r8, r6
	str r0, [r7]
@ <LABEL L697>
.L697:
@ <STOREW, <PLUS, <LOADW, <LOCAL -4>>, <CONST 1>>, <LOCAL -4>>
	set ip, #-4
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, #1
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L693>
	b .L693
@ <LABEL L694>
.L694:
@   mem[goalframe+6] := mem[prok];
@ <DEFTEMP 11, <GLOBAL _mem>>
	set r7, _mem
@ <DEFTEMP 12, <GLOBAL _goalframe>>
	set r8, _goalframe
@ <STOREW,
@   <LOADW,
@     <OFFSET, <TEMP 11>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>,
@   <OFFSET,
@     <OFFSET, <TEMP 11>, <LSL, <LOADW, <TEMP 12>>, <CONST 2>>>,
@     <CONST 24>>>
	set r0, _prok
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r7, r0
	ldr r0, [r0]
	ldr r1, [r8]
	lsl r1, r1, #2
	add r1, r7, r1
	add r1, r1, #24
	str r0, [r1]
@   for i := 1 to mem[goalframe+6] do
@ <STOREW, <CONST 1>, <LOCAL -4>>
	set r0, #1
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 11>, <LSL, <LOADW, <TEMP 12>>, <CONST 2>>>,
@       <CONST 24>>>,
@   <LOCAL -16>>
	ldr r0, [r8]
	lsl r0, r0, #2
	add r0, r7, r0
	add r0, r0, #24
	ldr r0, [r0]
	set ip, #-16
	add r1, fp, ip
	str r0, [r1]
@ <LABEL L701>
.L701:
@ <DEFTEMP 13, <LOADW, <LOCAL -4>>>
	set ip, #-4
	add r0, fp, ip
	ldr r7, [r0]
@ <JGT L702, <TEMP 13>, <LOADW, <LOCAL -16>>>
	set ip, #-16
	add r0, fp, ip
	ldr r0, [r0]
	cmp r7, r0
	bgt .L702
@     mem[(goalframe+7+(i-1)*TERM_SIZE)] := lsl(CELL, 8) + TERM_SIZE;
@ <DEFTEMP 14, <GLOBAL _mem>>
	set r8, _mem
@ <DEFTEMP 15, <GLOBAL _goalframe>>
	set r9, _goalframe
@ <DEFTEMP 16, <MINUS, <LSL, <TEMP 13>, <CONST 1>>, <CONST 2>>>
	lsl r0, r7, #1
	sub r0, r0, #2
@ <STOREW,
@   <CONST 1026>,
@   <OFFSET,
@     <TEMP 14>,
@     <LSL,
@       <PLUS, <PLUS, <LOADW, <TEMP 15>>, <CONST 7>>, <TEMP 16>>,
@       <CONST 2>>>>
	set r1, #1026
	ldr r2, [r9]
	add r2, r2, #7
	add r2, r2, r0
	lsl r2, r2, #2
	add r2, r8, r2
	str r1, [r2]
@     mem[(goalframe+7+(i-1)*TERM_SIZE)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <TEMP 14>,
@       <LSL,
@         <PLUS, <PLUS, <LOADW, <TEMP 15>>, <CONST 7>>, <TEMP 16>>,
@         <CONST 2>>>,
@     <CONST 4>>>
	set r1, #0
	ldr r2, [r9]
	add r2, r2, #7
	add r0, r2, r0
	lsl r0, r0, #2
	add r0, r8, r0
	add r0, r0, #4
	str r1, [r0]
@ <STOREW, <PLUS, <TEMP 13>, <CONST 1>>, <LOCAL -4>>
	add r0, r7, #1
	set ip, #-4
	add r1, fp, ip
	str r0, [r1]
@ <JUMP L701>
	b .L701
@ <LABEL L702>
.L702:
@   ok := Unify(call, temp, mem[prok+3], goalframe);
@ <DEFTEMP 17, <GLOBAL _prok>>
	set r7, _prok
@ <ARG 3, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r3, [r0]
@ <ARG 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <TEMP 17>>, <CONST 2>>>,
@       <CONST 12>>>>
	set r0, _mem
	ldr r1, [r7]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #12
	ldr r2, [r0]
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0, <LOADW, <GLOBAL _call>>>
	set r0, _call
	ldr r0, [r0]
@ <DEFTEMP 18, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 18>, <GLOBAL _ok>>
	set r1, _ok
	strb r0, [r1]
@   current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <TEMP 17>>, <CONST 4>>, <GLOBAL _current>>
	ldr r0, [r7]
	add r0, r0, #4
	set r1, _current
	str r0, [r1]
@   lsp := temp-1
@ <STOREW, <MINUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <GLOBAL _lsp>>
	sub r0, r4, #1
	set r1, _lsp
	str r0, [r1]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Step();
@ Initial code:
@   if symtab[mem[call+1]].action <> 0 then
@ <JNEQ L704,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@ <JUMP L705>
@ <LABEL L704>
@     ok := DoBuiltin(symtab[mem[call+1]].action)
@ <STOREC,
@   <CALL 1,
@     <GLOBAL _DoBuiltin>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _symtab>,
@             <TIMES,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                     <CONST 4>>>>,
@               <CONST 16>>>,
@           <CONST 8>>>>>,
@   <GLOBAL _ok>>
@ <JUMP L706>
@ <LABEL L705>
@   elsif prok = NULL then
@ <JEQ L707, <LOADW, <GLOBAL _prok>>, <CONST 0>>
@ <JUMP L708>
@ <LABEL L707>
@     ok := false
@ <STOREC, <CONST 0>, <GLOBAL _ok>>
@ <JUMP L709>
@ <LABEL L708>
@     retry := Search(call, goalframe, mem[prok+2]);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Search>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 2>>,
@             <CONST 4>>>>>>,
@   <REGVAR 0>>
@     if (mem[(current)+1] = NULL) and (choice < goalframe)
@ <JEQ L715,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>, <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L711>
@ <LABEL L715>
@ <JLT L714, <LOADW, <GLOBAL _choice>>, <LOADW, <GLOBAL _goalframe>>>
@ <JUMP L711>
@ <LABEL L714>
@ <JEQ L713, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L711>
@ <LABEL L713>
@ <JNEQ L710, <LOADW, <GLOBAL _goalframe>>, <LOADW, <GLOBAL _base>>>
@ <JUMP L711>
@ <LABEL L710>
@       TroStep()
@ <CALL 0, <GLOBAL _TroStep>, <STATLINK, <CONST 0>>>
@ <JUMP L712>
@ <LABEL L711>
@       PushFrame(mem[prok], retry);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <GLOBAL _prok>>, <CONST 4>>>>>,
@   <ARG 1, <LOADW, <REGVAR 0>>>>
@       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 3>>,
@             <CONST 4>>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _ok>>
@       current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 4>>, <GLOBAL _current>>
@     end
@ <LABEL L712>
@ <LABEL L709>
@ <LABEL L706>
@ <LABEL L703>

@ After simplification:
@   if symtab[mem[call+1]].action <> 0 then
@ <JEQ L705,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@     ok := DoBuiltin(symtab[mem[call+1]].action)
@ <STOREC,
@   <CALL 1,
@     <GLOBAL _DoBuiltin>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _symtab>,
@             <LSL,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@                   <CONST 4>>>,
@               <CONST 4>>>,
@           <CONST 8>>>>>,
@   <GLOBAL _ok>>
@ <JUMP L706>
@ <LABEL L705>
@   elsif prok = NULL then
@ <JNEQ L708, <LOADW, <GLOBAL _prok>>, <CONST 0>>
@     ok := false
@ <STOREC, <CONST 0>, <GLOBAL _ok>>
@ <JUMP L706>
@ <LABEL L708>
@     retry := Search(call, goalframe, mem[prok+2]);
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Search>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>,
@           <CONST 8>>>>>,
@   <REGVAR 0>>
@     if (mem[(current)+1] = NULL) and (choice < goalframe)
@ <JNEQ L711,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <JGEQ L711, <LOADW, <GLOBAL _choice>>, <LOADW, <GLOBAL _goalframe>>>
@ <JNEQ L711, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JEQ L711, <LOADW, <GLOBAL _goalframe>>, <LOADW, <GLOBAL _base>>>
@       TroStep()
@ <CALL 0, <GLOBAL _TroStep>>
@ <JUMP L706>
@ <LABEL L711>
@       PushFrame(mem[prok], retry);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>>>,
@   <ARG 1, <LOADW, <REGVAR 0>>>>
@       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _ok>>
@       current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <GLOBAL _prok>>, <CONST 4>>, <GLOBAL _current>>
@ <LABEL L706>

@ After sharing:
@   if symtab[mem[call+1]].action <> 0 then
@ <DEFTEMP 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 8>>>>
@ <JEQ L705, <TEMP 1>, <CONST 0>>
@     ok := DoBuiltin(symtab[mem[call+1]].action)
@ <DEFTEMP 2, <CALL 1, <GLOBAL _DoBuiltin>, <ARG 0, <TEMP 1>>>>
@ <STOREC, <TEMP 2>, <GLOBAL _ok>>
@ <JUMP L706>
@ <LABEL L705>
@   elsif prok = NULL then
@ <JNEQ L708, <LOADW, <GLOBAL _prok>>, <CONST 0>>
@     ok := false
@ <STOREC, <CONST 0>, <GLOBAL _ok>>
@ <JUMP L706>
@ <LABEL L708>
@     retry := Search(call, goalframe, mem[prok+2]);
@ <DEFTEMP 3, <GLOBAL _goalframe>>
@ <DEFTEMP 4, <GLOBAL _mem>>
@ <DEFTEMP 5,
@   <CALL 3,
@     <GLOBAL _Search>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <TEMP 3>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 4>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>,
@           <CONST 8>>>>>>
@ <STOREW, <TEMP 5>, <REGVAR 0>>
@     if (mem[(current)+1] = NULL) and (choice < goalframe)
@ <JNEQ L711,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@ <DEFTEMP 6, <LOADW, <TEMP 3>>>
@ <JGEQ L711, <LOADW, <GLOBAL _choice>>, <TEMP 6>>
@ <JNEQ L711, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JEQ L711, <TEMP 6>, <LOADW, <GLOBAL _base>>>
@       TroStep()
@ <CALL 0, <GLOBAL _TroStep>>
@ <JUMP L706>
@ <LABEL L711>
@       PushFrame(mem[prok], retry);
@ <DEFTEMP 7, <GLOBAL _mem>>
@ <DEFTEMP 8, <GLOBAL _prok>>
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <ARG 0,
@     <LOADW, <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 8>>, <CONST 2>>>>>,
@   <ARG 1, <LOADW, <REGVAR 0>>>>
@       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
@ <DEFTEMP 9, <LOADW, <GLOBAL _goalframe>>>
@ <DEFTEMP 10,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 7>, <LSL, <TEMP 9>, <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 8>>, <CONST 2>>>,
@           <CONST 12>>>>,
@     <ARG 3, <TEMP 9>>>>
@ <STOREC, <TEMP 10>, <GLOBAL _ok>>
@       current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <TEMP 8>>, <CONST 4>>, <GLOBAL _current>>
@ <LABEL L706>

_Step:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   if symtab[mem[call+1]].action <> 0 then
@ <DEFTEMP 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 8>>>>
	set r0, _symtab
	set r1, _mem
	set r2, _call
	ldr r2, [r2]
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #8
	ldr r5, [r0]
@ <JEQ L705, <TEMP 1>, <CONST 0>>
	cmp r5, #0
	beq .L705
@     ok := DoBuiltin(symtab[mem[call+1]].action)
@ <ARG 0, <TEMP 1>>
	mov r0, r5
@ <DEFTEMP 2, <CALL 1, <GLOBAL _DoBuiltin>>>
	bl _DoBuiltin
@ <STOREC, <TEMP 2>, <GLOBAL _ok>>
	set r1, _ok
	strb r0, [r1]
@ <JUMP L706>
	b .L706
@ <LABEL L705>
.L705:
@   elsif prok = NULL then
@ <JNEQ L708, <LOADW, <GLOBAL _prok>>, <CONST 0>>
	set r0, _prok
	ldr r0, [r0]
	cmp r0, #0
	bne .L708
@     ok := false
@ <STOREC, <CONST 0>, <GLOBAL _ok>>
	set r0, #0
	set r1, _ok
	strb r0, [r1]
@ <JUMP L706>
	b .L706
@ <LABEL L708>
.L708:
@     retry := Search(call, goalframe, mem[prok+2]);
@ <DEFTEMP 3, <GLOBAL _goalframe>>
	set r5, _goalframe
@ <DEFTEMP 4, <GLOBAL _mem>>
	set r6, _mem
@ <ARG 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <GLOBAL _prok>>, <CONST 2>>>,
@       <CONST 8>>>>
	set r0, _prok
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #8
	ldr r2, [r0]
@ <ARG 1, <LOADW, <TEMP 3>>>
	ldr r1, [r5]
@ <ARG 0, <LOADW, <GLOBAL _call>>>
	set r0, _call
	ldr r0, [r0]
@ <DEFTEMP 5, <CALL 3, <GLOBAL _Search>>>
	bl _Search
@ <STOREW, <TEMP 5>, <REGVAR 0>>
	mov r4, r0
@     if (mem[(current)+1] = NULL) and (choice < goalframe)
@ <JNEQ L711,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 4>, <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
	set r0, _current
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r6, r0
	add r0, r0, #4
	ldr r0, [r0]
	cmp r0, #0
	bne .L711
@ <DEFTEMP 6, <LOADW, <TEMP 3>>>
	ldr r5, [r5]
@ <JGEQ L711, <LOADW, <GLOBAL _choice>>, <TEMP 6>>
	set r0, _choice
	ldr r0, [r0]
	cmp r0, r5
	bge .L711
@ <JNEQ L711, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	bne .L711
@ <JEQ L711, <TEMP 6>, <LOADW, <GLOBAL _base>>>
	set r0, _base
	ldr r0, [r0]
	cmp r5, r0
	beq .L711
@       TroStep()
@ <CALL 0, <GLOBAL _TroStep>>
	bl _TroStep
@ <JUMP L706>
	b .L706
@ <LABEL L711>
.L711:
@       PushFrame(mem[prok], retry);
@ <DEFTEMP 7, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 8, <GLOBAL _prok>>
	set r6, _prok
@ <ARG 1, <LOADW, <REGVAR 0>>>
	mov r1, r4
@ <ARG 0,
@   <LOADW, <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 8>>, <CONST 2>>>>>
	ldr r0, [r6]
	lsl r0, r0, #2
	add r0, r5, r0
	ldr r0, [r0]
@ <CALL 2, <GLOBAL _PushFrame>>
	bl _PushFrame
@       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
@ <DEFTEMP 9, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r7, [r0]
@ <ARG 3, <TEMP 9>>
	mov r3, r7
@ <ARG 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 8>>, <CONST 2>>>,
@       <CONST 12>>>>
	ldr r0, [r6]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #12
	ldr r2, [r0]
@ <ARG 1,
@   <LOADW,
@     <OFFSET, <OFFSET, <TEMP 7>, <LSL, <TEMP 9>, <CONST 2>>>, <CONST 4>>>>
	lsl r0, r7, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r1, [r0]
@ <ARG 0, <LOADW, <GLOBAL _call>>>
	set r0, _call
	ldr r0, [r0]
@ <DEFTEMP 10, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 10>, <GLOBAL _ok>>
	set r1, _ok
	strb r0, [r1]
@       current := (prok+4);
@ <STOREW, <PLUS, <LOADW, <TEMP 8>>, <CONST 4>>, <GLOBAL _current>>
	ldr r0, [r6]
	add r0, r0, #4
	set r1, _current
	str r0, [r1]
@ <LABEL L706>
.L706:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Unwind();
@ Initial code:
@   while (mem[current] = NULL) and (goalframe <> base) do
@ <LABEL L717>
@ <JEQ L726,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <LOADW, <GLOBAL _current>>, <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L719>
@ <LABEL L726>
@ <JNEQ L718, <LOADW, <GLOBAL _goalframe>>, <LOADW, <GLOBAL _base>>>
@ <JUMP L719>
@ <LABEL L718>
@     if dflag then 
@ <JNEQ L720, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <JUMP L721>
@ <LABEL L720>
@     print_string("Exit"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g81>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g82>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES, <LOADW, <GLOBAL _goalframe>>, <CONST 4>>>>,
@           <CONST 4>>>>>,
@   <ARG 1,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@           <CONST 4>>>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L722>
@ <LABEL L721>
@ <LABEL L722>
@     current := (mem[goalframe])+1;
@ <STOREW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <GLOBAL _goalframe>>, <CONST 4>>>>,
@     <CONST 1>>,
@   <GLOBAL _current>>
@     if goalframe > choice then lsp := goalframe-1 end;
@ <JGT L723, <LOADW, <GLOBAL _goalframe>>, <LOADW, <GLOBAL _choice>>>
@ <JUMP L724>
@ <LABEL L723>
@ <STOREW,
@   <MINUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@   <GLOBAL _lsp>>
@ <JUMP L725>
@ <LABEL L724>
@ <LABEL L725>
@     goalframe := mem[goalframe+1]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@         <CONST 4>>>>,
@   <GLOBAL _goalframe>>
@ <JUMP L717>
@ <LABEL L719>
@ <LABEL L716>

@ After simplification:
@ <LABEL L717>
@   while (mem[current] = NULL) and (goalframe <> base) do
@ <JNEQ L716,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>,
@   <CONST 0>>
@ <JEQ L716, <LOADW, <GLOBAL _goalframe>>, <LOADW, <GLOBAL _base>>>
@     if dflag then 
@ <JEQ L722, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@     print_string("Exit"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g81>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g82>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>>,
@           <CONST 2>>>>>,
@   <ARG 1,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@         <CONST 4>>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L722>
@     current := (mem[goalframe])+1;
@ <STOREW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>>,
@     <CONST 1>>,
@   <GLOBAL _current>>
@     if goalframe > choice then lsp := goalframe-1 end;
@ <JLEQ L725, <LOADW, <GLOBAL _goalframe>>, <LOADW, <GLOBAL _choice>>>
@ <STOREW,
@   <MINUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@   <GLOBAL _lsp>>
@ <LABEL L725>
@     goalframe := mem[goalframe+1]
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <GLOBAL _goalframe>>
@ <JUMP L717>
@ <LABEL L716>

@ After sharing:
@ <LABEL L717>
@   while (mem[current] = NULL) and (goalframe <> base) do
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <JNEQ L716,
@   <LOADW,
@     <OFFSET, <TEMP 1>, <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>,
@   <CONST 0>>
@ <DEFTEMP 2, <GLOBAL _goalframe>>
@ <JEQ L716, <LOADW, <TEMP 2>>, <LOADW, <GLOBAL _base>>>
@     if dflag then 
@ <JEQ L722, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@     print_string("Exit"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g81>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g82>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
@ <DEFTEMP 3, <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>>
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0,
@     <LOADW, <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 3>>, <CONST 2>>>>>,
@   <ARG 1, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L722>
@     current := (mem[goalframe])+1;
@ <DEFTEMP 4, <LOADW, <GLOBAL _goalframe>>>
@ <STOREW,
@   <PLUS,
@     <LOADW, <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 4>, <CONST 2>>>>,
@     <CONST 1>>,
@   <GLOBAL _current>>
@     if goalframe > choice then lsp := goalframe-1 end;
@ <JLEQ L725, <TEMP 4>, <LOADW, <GLOBAL _choice>>>
@ <STOREW, <MINUS, <TEMP 4>, <CONST 1>>, <GLOBAL _lsp>>
@ <LABEL L725>
@     goalframe := mem[goalframe+1]
@ <DEFTEMP 5, <GLOBAL _goalframe>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <TEMP 5>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <TEMP 5>>
@ <JUMP L717>
@ <LABEL L716>

_Unwind:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@ <LABEL L717>
.L717:
@   while (mem[current] = NULL) and (goalframe <> base) do
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r4, _mem
@ <JNEQ L716,
@   <LOADW,
@     <OFFSET, <TEMP 1>, <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>,
@   <CONST 0>>
	set r0, _current
	ldr r0, [r0]
	lsl r0, r0, #2
	add r0, r4, r0
	ldr r0, [r0]
	cmp r0, #0
	bne .L716
@ <DEFTEMP 2, <GLOBAL _goalframe>>
	set r5, _goalframe
@ <JEQ L716, <LOADW, <TEMP 2>>, <LOADW, <GLOBAL _base>>>
	ldr r0, [r5]
	set r1, _base
	ldr r1, [r1]
	cmp r0, r1
	beq .L716
@     if dflag then 
@ <JEQ L722, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
	set r0, _dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L722
@     print_string("Exit"); print_string(": "); 
@ <ARG 1, <CONST 4>>
	set r1, #4
@ <ARG 0, <GLOBAL g81>>
	set r0, g81
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g82>>
	set r0, g82
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); newline()
@ <DEFTEMP 3, <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>>
	ldr r0, [r5]
	lsl r0, r0, #2
	add r5, r4, r0
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
	add r0, r5, #4
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW, <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 3>>, <CONST 2>>>>>
	ldr r0, [r5]
	lsl r0, r0, #2
	add r0, r4, r0
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <LABEL L722>
.L722:
@     current := (mem[goalframe])+1;
@ <DEFTEMP 4, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r4, [r0]
@ <STOREW,
@   <PLUS,
@     <LOADW, <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 4>, <CONST 2>>>>,
@     <CONST 1>>,
@   <GLOBAL _current>>
	set r0, _mem
	lsl r1, r4, #2
	add r0, r0, r1
	ldr r0, [r0]
	add r0, r0, #1
	set r1, _current
	str r0, [r1]
@     if goalframe > choice then lsp := goalframe-1 end;
@ <JLEQ L725, <TEMP 4>, <LOADW, <GLOBAL _choice>>>
	set r0, _choice
	ldr r0, [r0]
	cmp r4, r0
	ble .L725
@ <STOREW, <MINUS, <TEMP 4>, <CONST 1>>, <GLOBAL _lsp>>
	sub r0, r4, #1
	set r1, _lsp
	str r0, [r1]
@ <LABEL L725>
.L725:
@     goalframe := mem[goalframe+1]
@ <DEFTEMP 5, <GLOBAL _goalframe>>
	set r4, _goalframe
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <TEMP 5>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <TEMP 5>>
	set r0, _mem
	ldr r1, [r4]
	lsl r1, r1, #2
	add r0, r0, r1
	add r0, r0, #4
	ldr r0, [r0]
	str r0, [r4]
@ <JUMP L717>
	b .L717
@ <LABEL L716>
.L716:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Backtrack();
@ Initial code:
@   Restore();
@ <CALL 0, <GLOBAL _Restore>, <STATLINK, <CONST 0>>>
@   current := mem[choice]; goalframe := mem[choice+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <LOADW, <GLOBAL _choice>>, <CONST 4>>>>,
@   <GLOBAL _current>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 1>>, <CONST 4>>>>,
@   <GLOBAL _goalframe>>
@   call := Deref(mem[current], goalframe);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <GLOBAL _current>>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _call>>
@   prok := mem[choice+2]; gsp := mem[choice+4];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 2>>, <CONST 4>>>>,
@   <GLOBAL _prok>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 4>>, <CONST 4>>>>,
@   <GLOBAL _gsp>>
@   lsp := choice-1; choice := mem[choice+3];
@ <STOREW, <MINUS, <LOADW, <GLOBAL _choice>>, <CONST 1>>, <GLOBAL _lsp>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _choice>>, <CONST 3>>, <CONST 4>>>>,
@   <GLOBAL _choice>>
@   if dflag then 
@ <JNEQ L728, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <JUMP L729>
@ <LABEL L728>
@     print_string("Redo"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g83>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g84>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <GLOBAL _call>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L730>
@ <LABEL L729>
@ <LABEL L730>
@ end;
@ <LABEL L727>

@ After simplification:
@   Restore();
@ <CALL 0, <GLOBAL _Restore>>
@   current := mem[choice]; goalframe := mem[choice+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>>,
@   <GLOBAL _current>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <GLOBAL _goalframe>>
@   call := Deref(mem[current], goalframe);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _call>>
@   prok := mem[choice+2]; gsp := mem[choice+4];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 8>>>,
@   <GLOBAL _prok>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 16>>>,
@   <GLOBAL _gsp>>
@   lsp := choice-1; choice := mem[choice+3];
@ <STOREW, <MINUS, <LOADW, <GLOBAL _choice>>, <CONST 1>>, <GLOBAL _lsp>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _choice>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <GLOBAL _choice>>
@   if dflag then 
@ <JEQ L730, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@     print_string("Redo"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g83>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g84>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <LOADW, <GLOBAL _call>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L730>

@ After sharing:
@   Restore();
@ <CALL 0, <GLOBAL _Restore>>
@   current := mem[choice]; goalframe := mem[choice+1];
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2, <GLOBAL _choice>>
@ <DEFTEMP 3, <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>>
@ <DEFTEMP 4, <LOADW, <TEMP 3>>>
@ <STOREW, <TEMP 4>, <GLOBAL _current>>
@ <DEFTEMP 5, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
@ <DEFTEMP 6, <GLOBAL _goalframe>>
@ <STOREW, <TEMP 5>, <TEMP 6>>
@   call := Deref(mem[current], goalframe);
@ <DEFTEMP 7,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 1>, <LSL, <TEMP 4>, <CONST 2>>>>>,
@     <ARG 1, <TEMP 5>>>>
@ <DEFTEMP 8, <GLOBAL _call>>
@ <STOREW, <TEMP 7>, <TEMP 8>>
@   prok := mem[choice+2]; gsp := mem[choice+4];
@ <DEFTEMP 9, <LOADW, <TEMP 2>>>
@ <DEFTEMP 10, <OFFSET, <TEMP 1>, <LSL, <TEMP 9>, <CONST 2>>>>
@ <STOREW, <LOADW, <OFFSET, <TEMP 10>, <CONST 8>>>, <GLOBAL _prok>>
@ <STOREW, <LOADW, <OFFSET, <TEMP 10>, <CONST 16>>>, <GLOBAL _gsp>>
@   lsp := choice-1; choice := mem[choice+3];
@ <STOREW, <MINUS, <TEMP 9>, <CONST 1>>, <GLOBAL _lsp>>
@ <STOREW, <LOADW, <OFFSET, <TEMP 10>, <CONST 12>>>, <TEMP 2>>
@   if dflag then 
@ <JEQ L730, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@     print_string("Redo"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g83>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g84>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <LOADW, <TEMP 8>>>,
@   <ARG 1, <LOADW, <TEMP 6>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L730>

_Backtrack:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   Restore();
@ <CALL 0, <GLOBAL _Restore>>
	bl _Restore
@   current := mem[choice]; goalframe := mem[choice+1];
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r4, _mem
@ <DEFTEMP 2, <GLOBAL _choice>>
	set r5, _choice
@ <DEFTEMP 3, <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>>
	ldr r0, [r5]
	lsl r0, r0, #2
	add r6, r4, r0
@ <DEFTEMP 4, <LOADW, <TEMP 3>>>
	ldr r7, [r6]
@ <STOREW, <TEMP 4>, <GLOBAL _current>>
	set r0, _current
	str r7, [r0]
@ <DEFTEMP 5, <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>>
	add r0, r6, #4
	ldr r6, [r0]
@ <DEFTEMP 6, <GLOBAL _goalframe>>
	set r8, _goalframe
@ <STOREW, <TEMP 5>, <TEMP 6>>
	str r6, [r8]
@   call := Deref(mem[current], goalframe);
@ <ARG 1, <TEMP 5>>
	mov r1, r6
@ <ARG 0, <LOADW, <OFFSET, <TEMP 1>, <LSL, <TEMP 4>, <CONST 2>>>>>
	lsl r0, r7, #2
	add r0, r4, r0
	ldr r0, [r0]
@ <DEFTEMP 7, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <DEFTEMP 8, <GLOBAL _call>>
	set r6, _call
@ <STOREW, <TEMP 7>, <TEMP 8>>
	str r0, [r6]
@   prok := mem[choice+2]; gsp := mem[choice+4];
@ <DEFTEMP 9, <LOADW, <TEMP 2>>>
	ldr r7, [r5]
@ <DEFTEMP 10, <OFFSET, <TEMP 1>, <LSL, <TEMP 9>, <CONST 2>>>>
	lsl r0, r7, #2
	add r4, r4, r0
@ <STOREW, <LOADW, <OFFSET, <TEMP 10>, <CONST 8>>>, <GLOBAL _prok>>
	add r0, r4, #8
	ldr r0, [r0]
	set r1, _prok
	str r0, [r1]
@ <STOREW, <LOADW, <OFFSET, <TEMP 10>, <CONST 16>>>, <GLOBAL _gsp>>
	add r0, r4, #16
	ldr r0, [r0]
	set r1, _gsp
	str r0, [r1]
@   lsp := choice-1; choice := mem[choice+3];
@ <STOREW, <MINUS, <TEMP 9>, <CONST 1>>, <GLOBAL _lsp>>
	sub r0, r7, #1
	set r1, _lsp
	str r0, [r1]
@ <STOREW, <LOADW, <OFFSET, <TEMP 10>, <CONST 12>>>, <TEMP 2>>
	add r0, r4, #12
	ldr r0, [r0]
	str r0, [r5]
@   if dflag then 
@ <JEQ L730, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
	set r0, _dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L730
@     print_string("Redo"); print_string(": "); 
@ <ARG 1, <CONST 4>>
	set r1, #4
@ <ARG 0, <GLOBAL g83>>
	set r0, g83
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g84>>
	set r0, g84
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <TEMP 6>>>
	ldr r1, [r8]
@ <ARG 0, <LOADW, <TEMP 8>>>
	ldr r0, [r6]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <LABEL L730>
.L730:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Resume(flag: boolean);
@ Initial code:
@   ok := flag;
@ <STOREC, <LOADC, <OFFSET, <LOCAL 0>, <CONST 40>>>, <GLOBAL _ok>>
@   while run do
@ <LABEL L732>
@ <JNEQ L733, <LOADC, <GLOBAL _run>>, <CONST 0>>
@ <JUMP L734>
@ <LABEL L733>
@     if ok then
@ <JNEQ L735, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@ <JUMP L736>
@ <LABEL L735>
@       if mem[current] = NULL then return end;
@ <JEQ L741,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <LOADW, <GLOBAL _current>>, <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L742>
@ <LABEL L741>
@ <JUMP L731>
@ <JUMP L743>
@ <LABEL L742>
@ <LABEL L743>
@       call := Deref(mem[current], goalframe);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES, <LOADW, <GLOBAL _current>>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _call>>
@       if dflag then 
@ <JNEQ L744, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <JUMP L745>
@ <LABEL L744>
@     print_string("Call"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g85>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g86>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <GLOBAL _call>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <JUMP L746>
@ <LABEL L745>
@ <LABEL L746>
@       if (symtab[mem[call+1]].prok = NULL)
@ <JEQ L750,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@ <JUMP L748>
@ <LABEL L750>
@ <JEQ L747,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@ <JUMP L748>
@ <LABEL L747>
@ 	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g87>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g88>>,
@   <ARG 1, <CONST 27>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ 	WriteString(symtab[mem[call+1]].name);
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <OFFSET,
@           <GLOBAL _symtab>,
@           <TIMES,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                   <CONST 4>>>>,
@             <CONST 16>>>,
@         <CONST 0>>>>>
@ 	return
@ <JUMP L731>
@ <JUMP L749>
@ <LABEL L748>
@ <LABEL L749>
@       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Search>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _symtab>,
@             <TIMES,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                     <CONST 4>>>>,
@               <CONST 16>>>,
@           <CONST 12>>>>>,
@   <GLOBAL _prok>>
@ <JUMP L737>
@ <LABEL L736>
@       if choice <= base then return end;
@ <JLEQ L738, <LOADW, <GLOBAL _choice>>, <LOADW, <GLOBAL _base>>>
@ <JUMP L739>
@ <LABEL L738>
@ <JUMP L731>
@ <JUMP L740>
@ <LABEL L739>
@ <LABEL L740>
@       Backtrack()
@ <CALL 0, <GLOBAL _Backtrack>, <STATLINK, <CONST 0>>>
@ <LABEL L737>
@     Step();
@ <CALL 0, <GLOBAL _Step>, <STATLINK, <CONST 0>>>
@     if ok then Unwind() end;
@ <JNEQ L751, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@ <JUMP L752>
@ <LABEL L751>
@ <CALL 0, <GLOBAL _Unwind>, <STATLINK, <CONST 0>>>
@ <JUMP L753>
@ <LABEL L752>
@ <LABEL L753>
@   end;
@ <JUMP L732>
@ <LABEL L734>
@ end;
@ <LABEL L731>

@ After simplification:
@   ok := flag;
@ <STOREC, <LOADC, <LOCAL 40>>, <GLOBAL _ok>>
@ <LABEL L732>
@   while run do
@ <JEQ L731, <LOADC, <GLOBAL _run>>, <CONST 0>>
@     if ok then
@ <JEQ L736, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@       if mem[current] = NULL then return end;
@ <JEQ L731,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>,
@   <CONST 0>>
@       call := Deref(mem[current], goalframe);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>,
@   <GLOBAL _call>>
@       if dflag then 
@ <JEQ L746, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@     print_string("Call"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g85>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g86>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <LOADW, <GLOBAL _call>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L746>
@       if (symtab[mem[call+1]].prok = NULL)
@ <JNEQ L749,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@ <JNEQ L749,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 8>>>,
@   <CONST 0>>
@ 	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g87>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g88>>,
@   <ARG 1, <CONST 27>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ 	WriteString(symtab[mem[call+1]].name);
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>>>>
@ <JUMP L731>
@ <LABEL L749>
@       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
@ <STOREW,
@   <CALL 3,
@     <GLOBAL _Search>,
@     <ARG 0, <LOADW, <GLOBAL _call>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _symtab>,
@             <LSL,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@                   <CONST 4>>>,
@               <CONST 4>>>,
@           <CONST 12>>>>>,
@   <GLOBAL _prok>>
@ <JUMP L737>
@ <LABEL L736>
@       if choice <= base then return end;
@ <JLEQ L731, <LOADW, <GLOBAL _choice>>, <LOADW, <GLOBAL _base>>>
@       Backtrack()
@ <CALL 0, <GLOBAL _Backtrack>>
@ <LABEL L737>
@     Step();
@ <CALL 0, <GLOBAL _Step>>
@     if ok then Unwind() end;
@ <JEQ L732, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@ <CALL 0, <GLOBAL _Unwind>>
@ <JUMP L732>
@ <LABEL L731>

@ After sharing:
@   ok := flag;
@ <STOREC, <LOADC, <LOCAL 40>>, <GLOBAL _ok>>
@ <LABEL L732>
@   while run do
@ <JEQ L731, <LOADC, <GLOBAL _run>>, <CONST 0>>
@     if ok then
@ <JEQ L736, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@       if mem[current] = NULL then return end;
@ <DEFTEMP 1,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>>
@ <JEQ L731, <TEMP 1>, <CONST 0>>
@       call := Deref(mem[current], goalframe);
@ <DEFTEMP 2, <GLOBAL _goalframe>>
@ <DEFTEMP 3,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0, <TEMP 1>>,
@     <ARG 1, <LOADW, <TEMP 2>>>>>
@ <DEFTEMP 4, <GLOBAL _call>>
@ <STOREW, <TEMP 3>, <TEMP 4>>
@       if dflag then 
@ <JEQ L746, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@     print_string("Call"); print_string(": "); 
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g85>>,
@   <ARG 1, <CONST 4>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g86>>,
@   <ARG 1, <CONST 2>>>
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <LOADW, <TEMP 4>>>,
@   <ARG 1, <LOADW, <TEMP 2>>>,
@   <ARG 2, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L746>
@       if (symtab[mem[call+1]].prok = NULL)
@ <DEFTEMP 5, <GLOBAL _symtab>>
@ <DEFTEMP 6, <GLOBAL _mem>>
@ <DEFTEMP 7, <GLOBAL _call>>
@ <DEFTEMP 8,
@   <OFFSET,
@     <TEMP 5>,
@     <LSL,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 7>>, <CONST 2>>>,
@           <CONST 4>>>,
@       <CONST 4>>>>
@ <JNEQ L749, <LOADW, <OFFSET, <TEMP 8>, <CONST 12>>>, <CONST 0>>
@ <JNEQ L749, <LOADW, <OFFSET, <TEMP 8>, <CONST 8>>>, <CONST 0>>
@ 	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g87>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g88>>,
@   <ARG 1, <CONST 27>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ 	WriteString(symtab[mem[call+1]].name);
@ <CALL 1,
@   <GLOBAL _WriteString>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <TEMP 5>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 7>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>>>>
@ <JUMP L731>
@ <LABEL L749>
@       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
@ <DEFTEMP 9, <LOADW, <GLOBAL _call>>>
@ <DEFTEMP 10,
@   <CALL 3,
@     <GLOBAL _Search>,
@     <ARG 0, <TEMP 9>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _symtab>,
@             <LSL,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 9>, <CONST 2>>>,
@                   <CONST 4>>>,
@               <CONST 4>>>,
@           <CONST 12>>>>>>
@ <STOREW, <TEMP 10>, <GLOBAL _prok>>
@ <JUMP L737>
@ <LABEL L736>
@       if choice <= base then return end;
@ <JLEQ L731, <LOADW, <GLOBAL _choice>>, <LOADW, <GLOBAL _base>>>
@       Backtrack()
@ <CALL 0, <GLOBAL _Backtrack>>
@ <LABEL L737>
@     Step();
@ <CALL 0, <GLOBAL _Step>>
@     if ok then Unwind() end;
@ <JEQ L732, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@ <CALL 0, <GLOBAL _Unwind>>
@ <JUMP L732>
@ <LABEL L731>

_Resume:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   ok := flag;
@ <STOREC, <LOADC, <LOCAL 40>>, <GLOBAL _ok>>
	set ip, #40
	add r0, fp, ip
	ldrb r0, [r0]
	set r1, _ok
	strb r0, [r1]
@ <LABEL L732>
.L732:
@   while run do
@ <JEQ L731, <LOADC, <GLOBAL _run>>, <CONST 0>>
	set r0, _run
	ldrb r0, [r0]
	cmp r0, #0
	beq .L731
@     if ok then
@ <JEQ L736, <LOADC, <GLOBAL _ok>>, <CONST 0>>
	set r0, _ok
	ldrb r0, [r0]
	cmp r0, #0
	beq .L736
@       if mem[current] = NULL then return end;
@ <DEFTEMP 1,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _current>>, <CONST 2>>>>>
	set r0, _mem
	set r1, _current
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r4, [r0]
@ <JEQ L731, <TEMP 1>, <CONST 0>>
	cmp r4, #0
	beq .L731
@       call := Deref(mem[current], goalframe);
@ <DEFTEMP 2, <GLOBAL _goalframe>>
	set r5, _goalframe
@ <ARG 1, <LOADW, <TEMP 2>>>
	ldr r1, [r5]
@ <ARG 0, <TEMP 1>>
	mov r0, r4
@ <DEFTEMP 3, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <DEFTEMP 4, <GLOBAL _call>>
	set r4, _call
@ <STOREW, <TEMP 3>, <TEMP 4>>
	str r0, [r4]
@       if dflag then 
@ <JEQ L746, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
	set r0, _dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L746
@     print_string("Call"); print_string(": "); 
@ <ARG 1, <CONST 4>>
	set r1, #4
@ <ARG 0, <GLOBAL g85>>
	set r0, g85
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g86>>
	set r0, g86
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@     PrintTerm(call, goalframe, MAXPRIO); newline()
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <TEMP 2>>>
	ldr r1, [r5]
@ <ARG 0, <LOADW, <TEMP 4>>>
	ldr r0, [r4]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <LABEL L746>
.L746:
@       if (symtab[mem[call+1]].prok = NULL)
@ <DEFTEMP 5, <GLOBAL _symtab>>
	set r4, _symtab
@ <DEFTEMP 6, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 7, <GLOBAL _call>>
	set r6, _call
@ <DEFTEMP 8,
@   <OFFSET,
@     <TEMP 5>,
@     <LSL,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 7>>, <CONST 2>>>,
@           <CONST 4>>>,
@       <CONST 4>>>>
	ldr r0, [r6]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r0, [r0]
	lsl r0, r0, #4
	add r7, r4, r0
@ <JNEQ L749, <LOADW, <OFFSET, <TEMP 8>, <CONST 12>>>, <CONST 0>>
	add r0, r7, #12
	ldr r0, [r0]
	cmp r0, #0
	bne .L749
@ <JNEQ L749, <LOADW, <OFFSET, <TEMP 8>, <CONST 8>>>, <CONST 0>>
	add r0, r7, #8
	ldr r0, [r0]
	cmp r0, #0
	bne .L749
@ 	newline(); print_string("Error: "); print_string("call to undefined relation "); run := false;
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g87>>
	set r0, g87
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 27>>
	set r1, #27
@ <ARG 0, <GLOBAL g88>>
	set r0, g88
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <STOREC, <CONST 0>, <GLOBAL _run>>
	set r0, #0
	set r1, _run
	strb r0, [r1]
@ 	WriteString(symtab[mem[call+1]].name);
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <TEMP 5>,
@       <LSL,
@         <LOADW,
@           <OFFSET,
@             <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 7>>, <CONST 2>>>,
@             <CONST 4>>>,
@         <CONST 4>>>>>
	ldr r0, [r6]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r0, [r0]
	lsl r0, r0, #4
	add r0, r4, r0
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _WriteString>>
	bl _WriteString
@ <JUMP L731>
	b .L731
@ <LABEL L749>
.L749:
@       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
@ <DEFTEMP 9, <LOADW, <GLOBAL _call>>>
	set r0, _call
	ldr r4, [r0]
@ <ARG 2,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 9>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 12>>>>
	set r0, _symtab
	set r1, _mem
	lsl r2, r4, #2
	add r1, r1, r2
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #12
	ldr r2, [r0]
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <TEMP 9>>
	mov r0, r4
@ <DEFTEMP 10, <CALL 3, <GLOBAL _Search>>>
	bl _Search
@ <STOREW, <TEMP 10>, <GLOBAL _prok>>
	set r1, _prok
	str r0, [r1]
@ <JUMP L737>
	b .L737
@ <LABEL L736>
.L736:
@       if choice <= base then return end;
@ <JLEQ L731, <LOADW, <GLOBAL _choice>>, <LOADW, <GLOBAL _base>>>
	set r0, _choice
	ldr r0, [r0]
	set r1, _base
	ldr r1, [r1]
	cmp r0, r1
	ble .L731
@       Backtrack()
@ <CALL 0, <GLOBAL _Backtrack>>
	bl _Backtrack
@ <LABEL L737>
.L737:
@     Step();
@ <CALL 0, <GLOBAL _Step>>
	bl _Step
@     if ok then Unwind() end;
@ <JEQ L732, <LOADC, <GLOBAL _ok>>, <CONST 0>>
	set r0, _ok
	ldrb r0, [r0]
	cmp r0, #0
	beq .L732
@ <CALL 0, <GLOBAL _Unwind>>
	bl _Unwind
@ <JUMP L732>
	b .L732
@ <LABEL L731>
.L731:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Execute(g: clause);
@ Initial code:
@   lsp := hp; gsp := MEMSIZE+1;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _lsp>>
@ <STOREW, <CONST 25001>, <GLOBAL _gsp>>
@   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
@ <STOREW, <CONST 0>, <GLOBAL _current>>
@ <STOREW, <CONST 0>, <GLOBAL _goalframe>>
@ <STOREW, <CONST 0>, <GLOBAL _choice>>
@ <STOREW, <CONST 0>, <GLOBAL _trhead>>
@   PushFrame(mem[g], NULL);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>>>>,
@   <ARG 1, <CONST 0>>>
@   choice := goalframe; base := goalframe; current := (g+4);
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _choice>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _base>>
@ <STOREW,
@   <PLUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 4>>,
@   <GLOBAL _current>>
@   run := true;
@ <STOREC, <CONST 1>, <GLOBAL _run>>
@   Resume(true);
@ <CALL 1, <GLOBAL _Resume>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 1>>>
@   if not run then return end;
@ <JNEQ L756, <LOADC, <GLOBAL _run>>, <CONST 0>>
@ <JUMP L755>
@ <LABEL L755>
@ <JUMP L754>
@ <JUMP L757>
@ <LABEL L756>
@ <LABEL L757>
@   while ok do
@ <LABEL L758>
@ <JNEQ L759, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@ <JUMP L760>
@ <LABEL L759>
@     nsoln := nsoln+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@     ShowAnswer(base);
@ <CALL 1,
@   <GLOBAL _ShowAnswer>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <GLOBAL _base>>>>
@     newline();
@ <CALL 0, <GLOBAL newline>>
@     Resume(false);
@ <CALL 1, <GLOBAL _Resume>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 0>>>
@     if not run then return end;
@ <JNEQ L762, <LOADC, <GLOBAL _run>>, <CONST 0>>
@ <JUMP L761>
@ <LABEL L761>
@ <JUMP L754>
@ <JUMP L763>
@ <LABEL L762>
@ <LABEL L763>
@   end;
@ <JUMP L758>
@ <LABEL L760>
@   if nsoln = 0 then
@ <JEQ L764, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L765>
@ <LABEL L764>
@     print_string("no"); newline(); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g89>>,
@   <ARG 1, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL newline>>
@   end
@ <JUMP L766>
@ <LABEL L765>
@ <LABEL L766>
@ <LABEL L754>

@ After simplification:
@   lsp := hp; gsp := MEMSIZE+1;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _lsp>>
@ <STOREW, <CONST 25001>, <GLOBAL _gsp>>
@   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
@ <STOREW, <CONST 0>, <GLOBAL _current>>
@ <STOREW, <CONST 0>, <GLOBAL _goalframe>>
@ <STOREW, <CONST 0>, <GLOBAL _choice>>
@ <STOREW, <CONST 0>, <GLOBAL _trhead>>
@   PushFrame(mem[g], NULL);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>>,
@   <ARG 1, <CONST 0>>>
@   choice := goalframe; base := goalframe; current := (g+4);
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _choice>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _base>>
@ <STOREW, <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>, <GLOBAL _current>>
@   run := true;
@ <STOREC, <CONST 1>, <GLOBAL _run>>
@   Resume(true);
@ <CALL 1, <GLOBAL _Resume>, <ARG 0, <CONST 1>>>
@   if not run then return end;
@ <JEQ L754, <LOADC, <GLOBAL _run>>, <CONST 0>>
@ <LABEL L758>
@   while ok do
@ <JEQ L760, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@     nsoln := nsoln+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@     ShowAnswer(base);
@ <CALL 1, <GLOBAL _ShowAnswer>, <ARG 0, <LOADW, <GLOBAL _base>>>>
@     newline();
@ <CALL 0, <GLOBAL newline>>
@     Resume(false);
@ <CALL 1, <GLOBAL _Resume>, <ARG 0, <CONST 0>>>
@     if not run then return end;
@ <JEQ L754, <LOADC, <GLOBAL _run>>, <CONST 0>>
@ <JUMP L758>
@ <LABEL L760>
@   if nsoln = 0 then
@ <JNEQ L754, <LOADW, <REGVAR 0>>, <CONST 0>>
@     print_string("no"); newline(); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g89>>,
@   <ARG 1, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L754>

@ After sharing:
@   lsp := hp; gsp := MEMSIZE+1;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _lsp>>
@ <STOREW, <CONST 25001>, <GLOBAL _gsp>>
@   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
@ <DEFTEMP 1, <GLOBAL _current>>
@ <STOREW, <CONST 0>, <TEMP 1>>
@ <DEFTEMP 2, <GLOBAL _goalframe>>
@ <STOREW, <CONST 0>, <TEMP 2>>
@ <DEFTEMP 3, <GLOBAL _choice>>
@ <STOREW, <CONST 0>, <TEMP 3>>
@ <STOREW, <CONST 0>, <GLOBAL _trhead>>
@   PushFrame(mem[g], NULL);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <ARG 0,
@     <LOADW,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>>,
@   <ARG 1, <CONST 0>>>
@   choice := goalframe; base := goalframe; current := (g+4);
@ <DEFTEMP 4, <LOADW, <TEMP 2>>>
@ <STOREW, <TEMP 4>, <TEMP 3>>
@ <STOREW, <TEMP 4>, <GLOBAL _base>>
@ <STOREW, <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>, <TEMP 1>>
@   run := true;
@ <DEFTEMP 5, <GLOBAL _run>>
@ <STOREC, <CONST 1>, <TEMP 5>>
@   Resume(true);
@ <CALL 1, <GLOBAL _Resume>, <ARG 0, <CONST 1>>>
@   if not run then return end;
@ <JEQ L754, <LOADC, <TEMP 5>>, <CONST 0>>
@ <LABEL L758>
@   while ok do
@ <JEQ L760, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@     nsoln := nsoln+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@     ShowAnswer(base);
@ <CALL 1, <GLOBAL _ShowAnswer>, <ARG 0, <LOADW, <GLOBAL _base>>>>
@     newline();
@ <CALL 0, <GLOBAL newline>>
@     Resume(false);
@ <CALL 1, <GLOBAL _Resume>, <ARG 0, <CONST 0>>>
@     if not run then return end;
@ <JEQ L754, <LOADC, <GLOBAL _run>>, <CONST 0>>
@ <JUMP L758>
@ <LABEL L760>
@   if nsoln = 0 then
@ <JNEQ L754, <LOADW, <REGVAR 0>>, <CONST 0>>
@     print_string("no"); newline(); newline();
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g89>>,
@   <ARG 1, <CONST 2>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 0, <GLOBAL newline>>
@ <LABEL L754>

_Execute:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   lsp := hp; gsp := MEMSIZE+1;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _lsp>>
	set r0, _hp
	ldr r0, [r0]
	set r1, _lsp
	str r0, [r1]
@ <STOREW, <CONST 25001>, <GLOBAL _gsp>>
	set r0, #25001
	set r1, _gsp
	str r0, [r1]
@   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
@ <DEFTEMP 1, <GLOBAL _current>>
	set r5, _current
@ <STOREW, <CONST 0>, <TEMP 1>>
	set r0, #0
	str r0, [r5]
@ <DEFTEMP 2, <GLOBAL _goalframe>>
	set r6, _goalframe
@ <STOREW, <CONST 0>, <TEMP 2>>
	set r0, #0
	str r0, [r6]
@ <DEFTEMP 3, <GLOBAL _choice>>
	set r7, _choice
@ <STOREW, <CONST 0>, <TEMP 3>>
	set r0, #0
	str r0, [r7]
@ <STOREW, <CONST 0>, <GLOBAL _trhead>>
	set r0, #0
	set r1, _trhead
	str r0, [r1]
@   PushFrame(mem[g], NULL);
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0,
@   <LOADW,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <LOCAL 40>>, <CONST 2>>>>>
	set r0, _mem
	set ip, #40
	add r2, fp, ip
	ldr r2, [r2]
	lsl r2, r2, #2
	add r0, r0, r2
	ldr r0, [r0]
@ <CALL 2, <GLOBAL _PushFrame>>
	bl _PushFrame
@   choice := goalframe; base := goalframe; current := (g+4);
@ <DEFTEMP 4, <LOADW, <TEMP 2>>>
	ldr r6, [r6]
@ <STOREW, <TEMP 4>, <TEMP 3>>
	str r6, [r7]
@ <STOREW, <TEMP 4>, <GLOBAL _base>>
	set r0, _base
	str r6, [r0]
@ <STOREW, <PLUS, <LOADW, <LOCAL 40>>, <CONST 4>>, <TEMP 1>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	add r0, r0, #4
	str r0, [r5]
@   run := true;
@ <DEFTEMP 5, <GLOBAL _run>>
	set r5, _run
@ <STOREC, <CONST 1>, <TEMP 5>>
	set r0, #1
	strb r0, [r5]
@   Resume(true);
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <CALL 1, <GLOBAL _Resume>>
	bl _Resume
@   if not run then return end;
@ <JEQ L754, <LOADC, <TEMP 5>>, <CONST 0>>
	ldrb r0, [r5]
	cmp r0, #0
	beq .L754
@ <LABEL L758>
.L758:
@   while ok do
@ <JEQ L760, <LOADC, <GLOBAL _ok>>, <CONST 0>>
	set r0, _ok
	ldrb r0, [r0]
	cmp r0, #0
	beq .L760
@     nsoln := nsoln+1;
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@     ShowAnswer(base);
@ <ARG 0, <LOADW, <GLOBAL _base>>>
	set r0, _base
	ldr r0, [r0]
@ <CALL 1, <GLOBAL _ShowAnswer>>
	bl _ShowAnswer
@     newline();
@ <CALL 0, <GLOBAL newline>>
	bl newline
@     Resume(false);
@ <ARG 0, <CONST 0>>
	set r0, #0
@ <CALL 1, <GLOBAL _Resume>>
	bl _Resume
@     if not run then return end;
@ <JEQ L754, <LOADC, <GLOBAL _run>>, <CONST 0>>
	set r0, _run
	ldrb r0, [r0]
	cmp r0, #0
	beq .L754
@ <JUMP L758>
	b .L758
@ <LABEL L760>
.L760:
@   if nsoln = 0 then
@ <JNEQ L754, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	bne .L754
@     print_string("no"); newline(); newline();
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <GLOBAL g89>>
	set r0, g89
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <LABEL L754>
.L754:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc GetArgs();
@ Initial code:
@   for i := 1 to symtab[mem[call+1]].arity do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS, <LOADW, <GLOBAL _call>>, <CONST 1>>,
@                 <CONST 4>>>>,
@           <CONST 16>>>,
@       <CONST 4>>>,
@   <REGVAR 1>>
@ <LABEL L768>
@ <JGT L769, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
@     av[i] := Deref(mem[call+i+1], goalframe)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <PLUS, <LOADW, <GLOBAL _call>>, <LOADW, <REGVAR 0>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>,
@   <OFFSET, <GLOBAL _av>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L768>
@ <LABEL L769>
@ <LABEL L767>

@ After simplification:
@   for i := 1 to symtab[mem[call+1]].arity do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 1>>
@ <LABEL L768>
@ <JGT L767, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
@     av[i] := Deref(mem[call+i+1], goalframe)
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <PLUS, <LOADW, <GLOBAL _call>>, <LOADW, <REGVAR 0>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>,
@   <OFFSET, <GLOBAL _av>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L768>
@ <LABEL L767>

@ After sharing:
@   for i := 1 to symtab[mem[call+1]].arity do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 1>>
@ <LABEL L768>
@ <JGT L767, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
@     av[i] := Deref(mem[call+i+1], goalframe)
@ <DEFTEMP 1,
@   <CALL 2,
@     <GLOBAL _Deref>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <PLUS, <LOADW, <GLOBAL _call>>, <LOADW, <REGVAR 0>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>>>
@ <STOREW,
@   <TEMP 1>,
@   <OFFSET, <GLOBAL _av>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L768>
@ <LABEL L767>

_GetArgs:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   for i := 1 to symtab[mem[call+1]].arity do
@ <STOREW, <CONST 1>, <REGVAR 0>>
	set r4, #1
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _symtab>,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _call>>, <CONST 2>>>,
@               <CONST 4>>>,
@           <CONST 4>>>,
@       <CONST 4>>>,
@   <REGVAR 1>>
	set r0, _symtab
	set r1, _mem
	set r2, _call
	ldr r2, [r2]
	lsl r2, r2, #2
	add r1, r1, r2
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #4
	add r0, r0, r1
	add r0, r0, #4
	ldr r5, [r0]
@ <LABEL L768>
.L768:
@ <JGT L767, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 1>>>
	cmp r4, r5
	bgt .L767
@     av[i] := Deref(mem[call+i+1], goalframe)
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL,
@           <PLUS, <LOADW, <GLOBAL _call>>, <LOADW, <REGVAR 0>>>,
@           <CONST 2>>>,
@       <CONST 4>>>>
	set r0, _mem
	set r2, _call
	ldr r2, [r2]
	add r2, r2, r4
	lsl r2, r2, #2
	add r0, r0, r2
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 1, <CALL 2, <GLOBAL _Deref>>>
	bl _Deref
@ <STOREW,
@   <TEMP 1>,
@   <OFFSET, <GLOBAL _av>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r1, _av
	lsl r2, r4, #2
	add r1, r1, r2
	str r0, [r1]
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L768>
	b .L768
@ <LABEL L767>
.L767:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc NewInt(n: integer): term;
@ Initial code:
@   t := GloAlloc(INT, TERM_SIZE);
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 2>>,
@     <ARG 1, <CONST 2>>>,
@   <REGVAR 0>>
@   mem[t+1] := n;
@ <STOREW,
@   <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <CONST 4>>>>
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
@ <JUMP L770>
@ <LABEL L770>

@ After simplification:
@   t := GloAlloc(INT, TERM_SIZE);
@ <STOREW,
@   <CALL 2, <GLOBAL _GloAlloc>, <ARG 0, <CONST 2>>, <ARG 1, <CONST 2>>>,
@   <REGVAR 0>>
@   mem[t+1] := n;
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>

@ After sharing:
@   t := GloAlloc(INT, TERM_SIZE);
@ <DEFTEMP 1,
@   <CALL 2, <GLOBAL _GloAlloc>, <ARG 0, <CONST 2>>, <ARG 1, <CONST 2>>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@   mem[t+1] := n;
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>

_NewInt:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   t := GloAlloc(INT, TERM_SIZE);
@ <ARG 1, <CONST 2>>
	set r1, #2
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <DEFTEMP 1, <CALL 2, <GLOBAL _GloAlloc>>>
	bl _GloAlloc
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@   mem[t+1] := n;
@ <STOREW,
@   <LOADW, <LOCAL 40>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@     <CONST 4>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	set r1, _mem
	lsl r2, r4, #2
	add r1, r1, r2
	add r1, r1, #4
	str r0, [r1]
@   return t
@ <RESULTW, <LOADW, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoCut(): boolean;
@ Initial code:
@   choice := mem[goalframe+3];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 3>>,
@         <CONST 4>>>>,
@   <GLOBAL _choice>>
@   lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
@ <STOREW,
@   <MINUS,
@     <PLUS,
@       <LOADW, <GLOBAL _goalframe>>,
@       <PLUS,
@         <CONST 7>,
@         <TIMES,
@           <LOADW,
@             <OFFSET,
@               <GLOBAL _mem>,
@               <TIMES,
@                 <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 6>>,
@                 <CONST 4>>>>,
@           <CONST 2>>>>,
@     <CONST 1>>,
@   <GLOBAL _lsp>>
@   Commit();
@ <CALL 0, <GLOBAL _Commit>, <STATLINK, <CONST 0>>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return true
@ <RESULTW, <CONST 1>>
@ <JUMP L771>
@ <LABEL L771>

@ After simplification:
@   choice := mem[goalframe+3];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <GLOBAL _choice>>
@   lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
@ <STOREW,
@   <MINUS,
@     <PLUS,
@       <LOADW, <GLOBAL _goalframe>>,
@       <PLUS,
@         <LSL,
@           <LOADW,
@             <OFFSET,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@               <CONST 24>>>,
@           <CONST 1>>,
@         <CONST 7>>>,
@     <CONST 1>>,
@   <GLOBAL _lsp>>
@   Commit();
@ <CALL 0, <GLOBAL _Commit>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return true
@ <RESULTW, <CONST 1>>

@ After sharing:
@   choice := mem[goalframe+3];
@ <DEFTEMP 1, <LOADW, <GLOBAL _goalframe>>>
@ <DEFTEMP 2, <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 1>, <CONST 2>>>>
@ <STOREW, <LOADW, <OFFSET, <TEMP 2>, <CONST 12>>>, <GLOBAL _choice>>
@   lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
@ <STOREW,
@   <MINUS,
@     <PLUS,
@       <TEMP 1>,
@       <PLUS,
@         <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 24>>>, <CONST 1>>,
@         <CONST 7>>>,
@     <CONST 1>>,
@   <GLOBAL _lsp>>
@   Commit();
@ <CALL 0, <GLOBAL _Commit>>
@   current := (current)+1;
@ <DEFTEMP 3, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 3>>, <CONST 1>>, <TEMP 3>>
@   return true
@ <RESULTW, <CONST 1>>

_DoCut:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   choice := mem[goalframe+3];
@ <DEFTEMP 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r4, [r0]
@ <DEFTEMP 2, <OFFSET, <GLOBAL _mem>, <LSL, <TEMP 1>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r4, #2
	add r5, r0, r1
@ <STOREW, <LOADW, <OFFSET, <TEMP 2>, <CONST 12>>>, <GLOBAL _choice>>
	add r0, r5, #12
	ldr r0, [r0]
	set r1, _choice
	str r0, [r1]
@   lsp := goalframe + (FRAME_SIZE + (mem[goalframe+6])*TERM_SIZE) - 1;
@ <STOREW,
@   <MINUS,
@     <PLUS,
@       <TEMP 1>,
@       <PLUS,
@         <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 24>>>, <CONST 1>>,
@         <CONST 7>>>,
@     <CONST 1>>,
@   <GLOBAL _lsp>>
	add r0, r5, #24
	ldr r0, [r0]
	lsl r0, r0, #1
	add r0, r0, #7
	add r0, r4, r0
	sub r0, r0, #1
	set r1, _lsp
	str r0, [r1]
@   Commit();
@ <CALL 0, <GLOBAL _Commit>>
	bl _Commit
@   current := (current)+1;
@ <DEFTEMP 3, <GLOBAL _current>>
	set r4, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 3>>, <CONST 1>>, <TEMP 3>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   return true
@ <RESULTW, <CONST 1>>
	set r0, #1
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoCall(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L774,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JUMP L773>
@ <LABEL L773>
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g90>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g91>>,
@   <ARG 1, <CONST 22>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L772>
@ <JUMP L775>
@ <LABEL L774>
@     PushFrame(1, NULL);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <CONST 1>>,
@   <ARG 1, <CONST 0>>>
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@             <CONST 4>>>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 7>>,
@           <CONST 0>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@     current := callbody;
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <GLOBAL _current>>
@     return true
@ <RESULTW, <CONST 1>>
@ <JUMP L772>
@ <LABEL L775>
@ <LABEL L772>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L774,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g90>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g91>>,
@   <ARG 1, <CONST 22>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L772>
@ <LABEL L774>
@     PushFrame(1, NULL);
@ <CALL 2, <GLOBAL _PushFrame>, <ARG 0, <CONST 1>>, <ARG 1, <CONST 0>>>
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@           <CONST 4>>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@     <CONST 32>>>
@     current := callbody;
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <GLOBAL _current>>
@     return true
@ <RESULTW, <CONST 1>>
@ <LABEL L772>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L774,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g90>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g91>>,
@   <ARG 1, <CONST 22>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L772>
@ <LABEL L774>
@     PushFrame(1, NULL);
@ <CALL 2, <GLOBAL _PushFrame>, <ARG 0, <CONST 1>>, <ARG 1, <CONST 0>>>
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2, <GLOBAL _goalframe>>
@ <DEFTEMP 3,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@           <CONST 4>>>>>>
@ <STOREW,
@   <TEMP 3>,
@   <OFFSET,
@     <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@     <CONST 32>>>
@     current := callbody;
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <GLOBAL _current>>
@     return true
@ <RESULTW, <CONST 1>>
@ <LABEL L772>

_DoCall:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L774,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
	set r0, _mem
	set r1, _av
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L774
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g90>>
	set r0, g90
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 22>>
	set r1, #22
@ <ARG 0, <GLOBAL g91>>
	set r0, g91
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <STOREC, <CONST 0>, <GLOBAL _run>>
	set r0, #0
	set r1, _run
	strb r0, [r1]
@     return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L772>
	b .L772
@ <LABEL L774>
.L774:
@     PushFrame(1, NULL);
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <CALL 2, <GLOBAL _PushFrame>>
	bl _PushFrame
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r4, _mem
@ <DEFTEMP 2, <GLOBAL _goalframe>>
	set r5, _goalframe
@ <ARG 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@       <CONST 4>>>>
	ldr r0, [r5]
	lsl r0, r0, #2
	add r0, r4, r0
	add r0, r0, #4
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>
	set r0, _av
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 3, <CALL 2, <GLOBAL _GloCopy>>>
	bl _GloCopy
@ <STOREW,
@   <TEMP 3>,
@   <OFFSET,
@     <OFFSET, <TEMP 1>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@     <CONST 32>>>
	ldr r1, [r5]
	lsl r1, r1, #2
	add r1, r4, r1
	add r1, r1, #32
	str r0, [r1]
@     current := callbody;
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <GLOBAL _current>>
	set r0, _callbody
	ldr r0, [r0]
	set r1, _current
	str r0, [r1]
@     return true
@ <RESULTW, <CONST 1>>
	set r0, #1
@ <LABEL L772>
.L772:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoNot(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L778,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@ <JUMP L777>
@ <LABEL L777>
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g92>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g93>>,
@   <ARG 1, <CONST 22>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L776>
@ <JUMP L779>
@ <LABEL L778>
@     PushFrame(1, NULL);
@ <CALL 2,
@   <GLOBAL _PushFrame>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <CONST 1>>,
@   <ARG 1, <CONST 0>>>
@     savebase := base; base := goalframe; choice := goalframe;
@ <STOREW, <LOADW, <GLOBAL _base>>, <REGVAR 0>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _base>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _choice>>
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 1>>,
@             <CONST 4>>>>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES,
@       <PLUS,
@         <PLUS,
@           <PLUS, <LOADW, <GLOBAL _goalframe>>, <CONST 7>>,
@           <CONST 0>>,
@         <CONST 1>>,
@       <CONST 4>>>>
@     current := callbody;
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <GLOBAL _current>>
@     Resume(true);
@ <CALL 1, <GLOBAL _Resume>, <STATLINK, <CONST 0>>, <ARG 0, <CONST 1>>>
@     choice := mem[base+3]; goalframe := mem[base+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _base>>, <CONST 3>>, <CONST 4>>>>,
@   <GLOBAL _choice>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <GLOBAL _base>>, <CONST 1>>, <CONST 4>>>>,
@   <GLOBAL _goalframe>>
@     if not ok then
@ <JNEQ L781, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@ <JUMP L780>
@ <LABEL L780>
@       current := (mem[base])+1;
@ <STOREW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES, <LOADW, <GLOBAL _base>>, <CONST 4>>>>,
@     <CONST 1>>,
@   <GLOBAL _current>>
@       return true
@ <RESULTW, <CONST 1>>
@ <JUMP L776>
@ <JUMP L782>
@ <LABEL L781>
@       Commit();
@ <CALL 0, <GLOBAL _Commit>, <STATLINK, <CONST 0>>>
@       return false
@ <RESULTW, <CONST 0>>
@ <JUMP L776>
@ <LABEL L782>
@     lsp := base-1; base := savebase
@ <STOREW, <MINUS, <LOADW, <GLOBAL _base>>, <CONST 1>>, <GLOBAL _lsp>>
@ <STOREW, <LOADW, <REGVAR 0>>, <GLOBAL _base>>
@ <LABEL L779>
@ <LABEL L776>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L778,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g92>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g93>>,
@   <ARG 1, <CONST 22>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L776>
@ <LABEL L778>
@     PushFrame(1, NULL);
@ <CALL 2, <GLOBAL _PushFrame>, <ARG 0, <CONST 1>>, <ARG 1, <CONST 0>>>
@     savebase := base; base := goalframe; choice := goalframe;
@ <STOREW, <LOADW, <GLOBAL _base>>, <REGVAR 0>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _base>>
@ <STOREW, <LOADW, <GLOBAL _goalframe>>, <GLOBAL _choice>>
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <STOREW,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@           <CONST 4>>>>>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _goalframe>>, <CONST 2>>>,
@     <CONST 32>>>
@     current := callbody;
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <GLOBAL _current>>
@     Resume(true);
@ <CALL 1, <GLOBAL _Resume>, <ARG 0, <CONST 1>>>
@     choice := mem[base+3]; goalframe := mem[base+1];
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _base>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <GLOBAL _choice>>
@ <STOREW,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _base>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <GLOBAL _goalframe>>
@     if not ok then
@ <JNEQ L781, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@       current := (mem[base])+1;
@ <STOREW,
@   <PLUS,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <GLOBAL _base>>, <CONST 2>>>>,
@     <CONST 1>>,
@   <GLOBAL _current>>
@       return true
@ <RESULTW, <CONST 1>>
@ <JUMP L776>
@ <LABEL L781>
@       Commit();
@ <CALL 0, <GLOBAL _Commit>>
@       return false
@ <RESULTW, <CONST 0>>
@ <LABEL L776>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L778,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g92>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g93>>,
@   <ARG 1, <CONST 22>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L776>
@ <LABEL L778>
@     PushFrame(1, NULL);
@ <CALL 2, <GLOBAL _PushFrame>, <ARG 0, <CONST 1>>, <ARG 1, <CONST 0>>>
@     savebase := base; base := goalframe; choice := goalframe;
@ <DEFTEMP 1, <GLOBAL _base>>
@ <STOREW, <LOADW, <TEMP 1>>, <REGVAR 0>>
@ <DEFTEMP 2, <GLOBAL _goalframe>>
@ <DEFTEMP 3, <LOADW, <TEMP 2>>>
@ <STOREW, <TEMP 3>, <TEMP 1>>
@ <DEFTEMP 4, <GLOBAL _choice>>
@ <STOREW, <TEMP 3>, <TEMP 4>>
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <DEFTEMP 5, <GLOBAL _mem>>
@ <DEFTEMP 6,
@   <CALL 2,
@     <GLOBAL _GloCopy>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 5>, <LSL, <TEMP 3>, <CONST 2>>>,
@           <CONST 4>>>>>>
@ <STOREW,
@   <TEMP 6>,
@   <OFFSET,
@     <OFFSET, <TEMP 5>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@     <CONST 32>>>
@     current := callbody;
@ <DEFTEMP 7, <GLOBAL _current>>
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <TEMP 7>>
@     Resume(true);
@ <CALL 1, <GLOBAL _Resume>, <ARG 0, <CONST 1>>>
@     choice := mem[base+3]; goalframe := mem[base+1];
@ <DEFTEMP 8, <OFFSET, <TEMP 5>, <LSL, <LOADW, <TEMP 1>>, <CONST 2>>>>
@ <STOREW, <LOADW, <OFFSET, <TEMP 8>, <CONST 12>>>, <TEMP 4>>
@ <STOREW, <LOADW, <OFFSET, <TEMP 8>, <CONST 4>>>, <TEMP 2>>
@     if not ok then
@ <JNEQ L781, <LOADC, <GLOBAL _ok>>, <CONST 0>>
@       current := (mem[base])+1;
@ <STOREW, <PLUS, <LOADW, <TEMP 8>>, <CONST 1>>, <TEMP 7>>
@       return true
@ <RESULTW, <CONST 1>>
@ <JUMP L776>
@ <LABEL L781>
@       Commit();
@ <CALL 0, <GLOBAL _Commit>>
@       return false
@ <RESULTW, <CONST 0>>
@ <LABEL L776>

_DoNot:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   if not (lsr(mem[av[1]], 8) = FUNC) then
@ <JEQ L778,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 1>>
	set r0, _mem
	set r1, _av
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #1
	beq .L778
@     newline(); print_string("Error: "); print_string("bad argument to call/1"); run := false;
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g92>>
	set r0, g92
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 22>>
	set r1, #22
@ <ARG 0, <GLOBAL g93>>
	set r0, g93
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <STOREC, <CONST 0>, <GLOBAL _run>>
	set r0, #0
	set r1, _run
	strb r0, [r1]
@     return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L776>
	b .L776
@ <LABEL L778>
.L778:
@     PushFrame(1, NULL);
@ <ARG 1, <CONST 0>>
	set r1, #0
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <CALL 2, <GLOBAL _PushFrame>>
	bl _PushFrame
@     savebase := base; base := goalframe; choice := goalframe;
@ <DEFTEMP 1, <GLOBAL _base>>
	set r5, _base
@ <STOREW, <LOADW, <TEMP 1>>, <REGVAR 0>>
	ldr r4, [r5]
@ <DEFTEMP 2, <GLOBAL _goalframe>>
	set r6, _goalframe
@ <DEFTEMP 3, <LOADW, <TEMP 2>>>
	ldr r7, [r6]
@ <STOREW, <TEMP 3>, <TEMP 1>>
	str r7, [r5]
@ <DEFTEMP 4, <GLOBAL _choice>>
	set r8, _choice
@ <STOREW, <TEMP 3>, <TEMP 4>>
	str r7, [r8]
@     mem[(goalframe+7+(1-1)*TERM_SIZE)+1] :=
@ <DEFTEMP 5, <GLOBAL _mem>>
	set r9, _mem
@ <ARG 1,
@   <LOADW,
@     <OFFSET, <OFFSET, <TEMP 5>, <LSL, <TEMP 3>, <CONST 2>>>, <CONST 4>>>>
	lsl r0, r7, #2
	add r0, r9, r0
	add r0, r0, #4
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>
	set r0, _av
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 6, <CALL 2, <GLOBAL _GloCopy>>>
	bl _GloCopy
@ <STOREW,
@   <TEMP 6>,
@   <OFFSET,
@     <OFFSET, <TEMP 5>, <LSL, <LOADW, <TEMP 2>>, <CONST 2>>>,
@     <CONST 32>>>
	ldr r1, [r6]
	lsl r1, r1, #2
	add r1, r9, r1
	add r1, r1, #32
	str r0, [r1]
@     current := callbody;
@ <DEFTEMP 7, <GLOBAL _current>>
	set r7, _current
@ <STOREW, <LOADW, <GLOBAL _callbody>>, <TEMP 7>>
	set r0, _callbody
	ldr r0, [r0]
	str r0, [r7]
@     Resume(true);
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <CALL 1, <GLOBAL _Resume>>
	bl _Resume
@     choice := mem[base+3]; goalframe := mem[base+1];
@ <DEFTEMP 8, <OFFSET, <TEMP 5>, <LSL, <LOADW, <TEMP 1>>, <CONST 2>>>>
	ldr r0, [r5]
	lsl r0, r0, #2
	add r5, r9, r0
@ <STOREW, <LOADW, <OFFSET, <TEMP 8>, <CONST 12>>>, <TEMP 4>>
	add r0, r5, #12
	ldr r0, [r0]
	str r0, [r8]
@ <STOREW, <LOADW, <OFFSET, <TEMP 8>, <CONST 4>>>, <TEMP 2>>
	add r0, r5, #4
	ldr r0, [r0]
	str r0, [r6]
@     if not ok then
@ <JNEQ L781, <LOADC, <GLOBAL _ok>>, <CONST 0>>
	set r0, _ok
	ldrb r0, [r0]
	cmp r0, #0
	bne .L781
@       current := (mem[base])+1;
@ <STOREW, <PLUS, <LOADW, <TEMP 8>>, <CONST 1>>, <TEMP 7>>
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r7]
@       return true
@ <RESULTW, <CONST 1>>
	set r0, #1
@ <JUMP L776>
	b .L776
@ <LABEL L781>
.L781:
@       Commit();
@ <CALL 0, <GLOBAL _Commit>>
	bl _Commit
@       return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <LABEL L776>
.L776:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoPlus(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <JEQ L801,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L785>
@ <LABEL L801>
@ <JEQ L784,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L785>
@ <LABEL L784>
@     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <PLUS,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 1>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 2>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L785>
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JEQ L800,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L788>
@ <LABEL L800>
@ <JEQ L787,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L788>
@ <LABEL L787>
@     if mem[av[1]+1] <= mem[av[3]+1] then
@ <JLEQ L797,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>>
@ <JUMP L798>
@ <LABEL L797>
@       result := Unify(av[2], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <MINUS,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 3>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 1>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L799>
@ <LABEL L798>
@ <LABEL L799>
@ <JUMP L789>
@ <LABEL L788>
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JEQ L796,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L791>
@ <LABEL L796>
@ <JEQ L790,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L791>
@ <LABEL L790>
@     if mem[av[2]+1] <= mem[av[3]+1] then
@ <JLEQ L793,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>>
@ <JUMP L794>
@ <LABEL L793>
@       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <MINUS,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 3>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 2>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L795>
@ <LABEL L794>
@ <LABEL L795>
@ <JUMP L792>
@ <LABEL L791>
@     newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g94>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g95>>,
@   <ARG 1, <CONST 34>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ <LABEL L792>
@ <LABEL L789>
@ <LABEL L786>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>
@ <JUMP L783>
@ <LABEL L783>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <JNEQ L785,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JNEQ L785,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <ARG 0,
@           <PLUS,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L785>
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JNEQ L788,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JNEQ L788,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@     if mem[av[1]+1] <= mem[av[3]+1] then
@ <JGT L786,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>, <CONST 2>>>,
@       <CONST 4>>>>
@       result := Unify(av[2], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <ARG 0,
@           <MINUS,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L788>
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JNEQ L791,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JNEQ L791,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@     if mem[av[2]+1] <= mem[av[3]+1] then
@ <JGT L786,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>, <CONST 2>>>,
@       <CONST 4>>>>
@       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <ARG 0,
@           <MINUS,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L791>
@     newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g94>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g95>>,
@   <ARG 1, <CONST 34>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ <LABEL L786>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2, <GLOBAL _av>>
@ <DEFTEMP 3,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>, <CONST 2>>>>
@ <JNEQ L785, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 2>>
@ <DEFTEMP 4,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 8>>>, <CONST 2>>>>
@ <JNEQ L785, <LSR, <LOADW, <TEMP 4>>, <CONST 8>>, <CONST 2>>
@     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
@ <DEFTEMP 5,
@   <CALL 1,
@     <GLOBAL _NewInt>,
@     <ARG 0,
@       <PLUS,
@         <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@         <LOADW, <OFFSET, <TEMP 4>, <CONST 4>>>>>>>
@ <DEFTEMP 6,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 2>, <CONST 12>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <TEMP 5>>,
@     <ARG 3, <CONST 0>>>>
@ <STOREC, <TEMP 6>, <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L785>
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 7, <GLOBAL _mem>>
@ <DEFTEMP 8, <GLOBAL _av>>
@ <DEFTEMP 9,
@   <OFFSET,
@     <TEMP 7>,
@     <LSL, <LOADW, <OFFSET, <TEMP 8>, <CONST 4>>>, <CONST 2>>>>
@ <JNEQ L788, <LSR, <LOADW, <TEMP 9>>, <CONST 8>>, <CONST 2>>
@ <DEFTEMP 10,
@   <OFFSET,
@     <TEMP 7>,
@     <LSL, <LOADW, <OFFSET, <TEMP 8>, <CONST 12>>>, <CONST 2>>>>
@ <JNEQ L788, <LSR, <LOADW, <TEMP 10>>, <CONST 8>>, <CONST 2>>
@     if mem[av[1]+1] <= mem[av[3]+1] then
@ <DEFTEMP 11, <LOADW, <OFFSET, <TEMP 9>, <CONST 4>>>>
@ <DEFTEMP 12, <LOADW, <OFFSET, <TEMP 10>, <CONST 4>>>>
@ <JGT L786, <TEMP 11>, <TEMP 12>>
@       result := Unify(av[2], goalframe, 
@ <DEFTEMP 13,
@   <CALL 1, <GLOBAL _NewInt>, <ARG 0, <MINUS, <TEMP 12>, <TEMP 11>>>>>
@ <DEFTEMP 14,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 8>, <CONST 8>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <TEMP 13>>,
@     <ARG 3, <CONST 0>>>>
@ <STOREC, <TEMP 14>, <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L788>
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 15, <GLOBAL _mem>>
@ <DEFTEMP 16, <GLOBAL _av>>
@ <DEFTEMP 17,
@   <OFFSET,
@     <TEMP 15>,
@     <LSL, <LOADW, <OFFSET, <TEMP 16>, <CONST 8>>>, <CONST 2>>>>
@ <JNEQ L791, <LSR, <LOADW, <TEMP 17>>, <CONST 8>>, <CONST 2>>
@ <DEFTEMP 18,
@   <OFFSET,
@     <TEMP 15>,
@     <LSL, <LOADW, <OFFSET, <TEMP 16>, <CONST 12>>>, <CONST 2>>>>
@ <JNEQ L791, <LSR, <LOADW, <TEMP 18>>, <CONST 8>>, <CONST 2>>
@     if mem[av[2]+1] <= mem[av[3]+1] then
@ <DEFTEMP 19, <LOADW, <OFFSET, <TEMP 17>, <CONST 4>>>>
@ <DEFTEMP 20, <LOADW, <OFFSET, <TEMP 18>, <CONST 4>>>>
@ <JGT L786, <TEMP 19>, <TEMP 20>>
@       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
@ <DEFTEMP 21,
@   <CALL 1, <GLOBAL _NewInt>, <ARG 0, <MINUS, <TEMP 20>, <TEMP 19>>>>>
@ <DEFTEMP 22,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 16>, <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <TEMP 21>>,
@     <ARG 3, <CONST 0>>>>
@ <STOREC, <TEMP 22>, <REGVAR 0>>
@ <JUMP L786>
@ <LABEL L791>
@     newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g94>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g95>>,
@   <ARG 1, <CONST 34>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ <LABEL L786>
@   current := (current)+1;
@ <DEFTEMP 23, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 23>>, <CONST 1>>, <TEMP 23>>
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>

_DoPlus:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
	set r4, #0
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 2, <GLOBAL _av>>
	set r6, _av
@ <DEFTEMP 3,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>, <CONST 2>>>>
	add r0, r6, #4
	ldr r0, [r0]
	lsl r0, r0, #2
	add r7, r5, r0
@ <JNEQ L785, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r7]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L785
@ <DEFTEMP 4,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 8>>>, <CONST 2>>>>
	add r0, r6, #8
	ldr r0, [r0]
	lsl r0, r0, #2
	add r5, r5, r0
@ <JNEQ L785, <LSR, <LOADW, <TEMP 4>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L785
@     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
@ <ARG 0,
@   <PLUS,
@     <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@     <LOADW, <OFFSET, <TEMP 4>, <CONST 4>>>>>
	add r0, r7, #4
	ldr r0, [r0]
	add r1, r5, #4
	ldr r1, [r1]
	add r0, r0, r1
@ <DEFTEMP 5, <CALL 1, <GLOBAL _NewInt>>>
	bl _NewInt
@ <ARG 3, <CONST 0>>
	set r3, #0
@ <ARG 2, <TEMP 5>>
	mov r2, r0
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 2>, <CONST 12>>>>
	add r0, r6, #12
	ldr r0, [r0]
@ <DEFTEMP 6, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 6>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L786>
	b .L786
@ <LABEL L785>
.L785:
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 7, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 8, <GLOBAL _av>>
	set r6, _av
@ <DEFTEMP 9,
@   <OFFSET,
@     <TEMP 7>,
@     <LSL, <LOADW, <OFFSET, <TEMP 8>, <CONST 4>>>, <CONST 2>>>>
	add r0, r6, #4
	ldr r0, [r0]
	lsl r0, r0, #2
	add r7, r5, r0
@ <JNEQ L788, <LSR, <LOADW, <TEMP 9>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r7]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L788
@ <DEFTEMP 10,
@   <OFFSET,
@     <TEMP 7>,
@     <LSL, <LOADW, <OFFSET, <TEMP 8>, <CONST 12>>>, <CONST 2>>>>
	add r0, r6, #12
	ldr r0, [r0]
	lsl r0, r0, #2
	add r5, r5, r0
@ <JNEQ L788, <LSR, <LOADW, <TEMP 10>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L788
@     if mem[av[1]+1] <= mem[av[3]+1] then
@ <DEFTEMP 11, <LOADW, <OFFSET, <TEMP 9>, <CONST 4>>>>
	add r0, r7, #4
	ldr r7, [r0]
@ <DEFTEMP 12, <LOADW, <OFFSET, <TEMP 10>, <CONST 4>>>>
	add r0, r5, #4
	ldr r5, [r0]
@ <JGT L786, <TEMP 11>, <TEMP 12>>
	cmp r7, r5
	bgt .L786
@       result := Unify(av[2], goalframe, 
@ <ARG 0, <MINUS, <TEMP 12>, <TEMP 11>>>
	sub r0, r5, r7
@ <DEFTEMP 13, <CALL 1, <GLOBAL _NewInt>>>
	bl _NewInt
@ <ARG 3, <CONST 0>>
	set r3, #0
@ <ARG 2, <TEMP 13>>
	mov r2, r0
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 8>, <CONST 8>>>>
	add r0, r6, #8
	ldr r0, [r0]
@ <DEFTEMP 14, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 14>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L786>
	b .L786
@ <LABEL L788>
.L788:
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 15, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 16, <GLOBAL _av>>
	set r6, _av
@ <DEFTEMP 17,
@   <OFFSET,
@     <TEMP 15>,
@     <LSL, <LOADW, <OFFSET, <TEMP 16>, <CONST 8>>>, <CONST 2>>>>
	add r0, r6, #8
	ldr r0, [r0]
	lsl r0, r0, #2
	add r7, r5, r0
@ <JNEQ L791, <LSR, <LOADW, <TEMP 17>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r7]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L791
@ <DEFTEMP 18,
@   <OFFSET,
@     <TEMP 15>,
@     <LSL, <LOADW, <OFFSET, <TEMP 16>, <CONST 12>>>, <CONST 2>>>>
	add r0, r6, #12
	ldr r0, [r0]
	lsl r0, r0, #2
	add r5, r5, r0
@ <JNEQ L791, <LSR, <LOADW, <TEMP 18>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L791
@     if mem[av[2]+1] <= mem[av[3]+1] then
@ <DEFTEMP 19, <LOADW, <OFFSET, <TEMP 17>, <CONST 4>>>>
	add r0, r7, #4
	ldr r7, [r0]
@ <DEFTEMP 20, <LOADW, <OFFSET, <TEMP 18>, <CONST 4>>>>
	add r0, r5, #4
	ldr r5, [r0]
@ <JGT L786, <TEMP 19>, <TEMP 20>>
	cmp r7, r5
	bgt .L786
@       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
@ <ARG 0, <MINUS, <TEMP 20>, <TEMP 19>>>
	sub r0, r5, r7
@ <DEFTEMP 21, <CALL 1, <GLOBAL _NewInt>>>
	bl _NewInt
@ <ARG 3, <CONST 0>>
	set r3, #0
@ <ARG 2, <TEMP 21>>
	mov r2, r0
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 16>, <CONST 4>>>>
	add r0, r6, #4
	ldr r0, [r0]
@ <DEFTEMP 22, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 22>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L786>
	b .L786
@ <LABEL L791>
.L791:
@     newline(); print_string("Error: "); print_string("plus/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g94>>
	set r0, g94
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 34>>
	set r1, #34
@ <ARG 0, <GLOBAL g95>>
	set r0, g95
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <STOREC, <CONST 0>, <GLOBAL _run>>
	set r0, #0
	set r1, _run
	strb r0, [r1]
@ <LABEL L786>
.L786:
@   current := (current)+1;
@ <DEFTEMP 23, <GLOBAL _current>>
	set r5, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 23>>, <CONST 1>>, <TEMP 23>>
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoTimes(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <JEQ L826,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L804>
@ <LABEL L826>
@ <JEQ L803,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L804>
@ <LABEL L803>
@     result := Unify(av[3], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <TIMES,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 1>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>,
@             <LOADW,
@               <OFFSET,
@                 <GLOBAL _mem>,
@                 <TIMES,
@                   <PLUS,
@                     <LOADW,
@                       <OFFSET,
@                         <GLOBAL _av>,
@                         <TIMES, <CONST 2>, <CONST 4>>>>,
@                     <CONST 1>>,
@                   <CONST 4>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L804>
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JEQ L825,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L807>
@ <LABEL L825>
@ <JEQ L806,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L807>
@ <LABEL L806>
@     if mem[av[1]+1] <> 0 then
@ <JNEQ L819,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L820>
@ <LABEL L819>
@       if mem[av[3]+1] mod mem[av[1]+1] = 0 then
@ <JEQ L822,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <LOADW,
@                 <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <LOADW,
@                 <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>>,
@   <CONST 0>>
@ <JUMP L823>
@ <LABEL L822>
@         result := Unify(av[2], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <CALL 2,
@             <GLOBAL int_div>,
@             <ARG 0,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS,
@                       <LOADW,
@                         <OFFSET,
@                           <GLOBAL _av>,
@                           <TIMES, <CONST 3>, <CONST 4>>>>,
@                       <CONST 1>>,
@                     <CONST 4>>>>>,
@             <ARG 1,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS,
@                       <LOADW,
@                         <OFFSET,
@                           <GLOBAL _av>,
@                           <TIMES, <CONST 1>, <CONST 4>>>>,
@                       <CONST 1>>,
@                     <CONST 4>>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L824>
@ <LABEL L823>
@ <LABEL L824>
@ <JUMP L821>
@ <LABEL L820>
@ <LABEL L821>
@ <JUMP L808>
@ <LABEL L807>
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JEQ L818,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L810>
@ <LABEL L818>
@ <JEQ L809,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <TIMES,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@           <CONST 4>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JUMP L810>
@ <LABEL L809>
@     if mem[av[2]+1] <> 0 then
@ <JNEQ L812,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES,
@         <PLUS,
@           <LOADW,
@             <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@           <CONST 1>>,
@         <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L813>
@ <LABEL L812>
@       if mem[av[3]+1] mod mem[av[2]+1] = 0 then
@ <JEQ L815,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <LOADW,
@                 <OFFSET, <GLOBAL _av>, <TIMES, <CONST 3>, <CONST 4>>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <PLUS,
@               <LOADW,
@                 <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>,
@               <CONST 1>>,
@             <CONST 4>>>>>>,
@   <CONST 0>>
@ <JUMP L816>
@ <LABEL L815>
@         result := Unify(av[1], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <STATLINK, <CONST 0>>,
@         <ARG 0,
@           <CALL 2,
@             <GLOBAL int_div>,
@             <ARG 0,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS,
@                       <LOADW,
@                         <OFFSET,
@                           <GLOBAL _av>,
@                           <TIMES, <CONST 3>, <CONST 4>>>>,
@                       <CONST 1>>,
@                     <CONST 4>>>>>,
@             <ARG 1,
@               <LOADW,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <TIMES,
@                     <PLUS,
@                       <LOADW,
@                         <OFFSET,
@                           <GLOBAL _av>,
@                           <TIMES, <CONST 2>, <CONST 4>>>>,
@                       <CONST 1>>,
@                     <CONST 4>>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L817>
@ <LABEL L816>
@ <LABEL L817>
@ <JUMP L814>
@ <LABEL L813>
@ <LABEL L814>
@ <JUMP L811>
@ <LABEL L810>
@     newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g96>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g97>>,
@   <ARG 1, <CONST 35>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ <LABEL L811>
@ <LABEL L808>
@ <LABEL L805>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>
@ <JUMP L802>
@ <LABEL L802>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <JNEQ L804,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JNEQ L804,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@     result := Unify(av[3], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <ARG 0,
@           <TIMES,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>,
@             <LOADW,
@               <OFFSET,
@                 <OFFSET,
@                   <GLOBAL _mem>,
@                   <LSL,
@                     <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>,
@                     <CONST 2>>>,
@                 <CONST 4>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L804>
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JNEQ L807,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JNEQ L807,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@     if mem[av[1]+1] <> 0 then
@ <JEQ L805,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@       if mem[av[3]+1] mod mem[av[1]+1] = 0 then
@ <JNEQ L805,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>>,
@   <CONST 0>>
@         result := Unify(av[2], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <ARG 0,
@           <CALL 2,
@             <GLOBAL int_div>,
@             <ARG 0,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL,
@                       <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>,
@                       <CONST 2>>>,
@                   <CONST 4>>>>,
@             <ARG 1,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL,
@                       <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>,
@                       <CONST 2>>>,
@                   <CONST 4>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L807>
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <JNEQ L810,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@ <JNEQ L810,
@   <LSR,
@     <LOADW,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>, <CONST 2>>>>,
@     <CONST 8>>,
@   <CONST 2>>
@     if mem[av[2]+1] <> 0 then
@ <JEQ L805,
@   <LOADW,
@     <OFFSET,
@       <OFFSET,
@         <GLOBAL _mem>,
@         <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>, <CONST 2>>>,
@       <CONST 4>>>,
@   <CONST 0>>
@       if mem[av[3]+1] mod mem[av[2]+1] = 0 then
@ <JNEQ L805,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET,
@             <GLOBAL _mem>,
@             <LSL,
@               <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>,
@               <CONST 2>>>,
@           <CONST 4>>>>>,
@   <CONST 0>>
@         result := Unify(av[1], goalframe, 
@ <STOREC,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <CALL 1,
@         <GLOBAL _NewInt>,
@         <ARG 0,
@           <CALL 2,
@             <GLOBAL int_div>,
@             <ARG 0,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL,
@                       <LOADW, <OFFSET, <GLOBAL _av>, <CONST 12>>>,
@                       <CONST 2>>>,
@                   <CONST 4>>>>,
@             <ARG 1,
@               <LOADW,
@                 <OFFSET,
@                   <OFFSET,
@                     <GLOBAL _mem>,
@                     <LSL,
@                       <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>,
@                       <CONST 2>>>,
@                   <CONST 4>>>>>>>>,
@     <ARG 3, <CONST 0>>>,
@   <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L810>
@     newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g96>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g97>>,
@   <ARG 1, <CONST 35>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ <LABEL L805>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <DEFTEMP 1, <GLOBAL _mem>>
@ <DEFTEMP 2, <GLOBAL _av>>
@ <DEFTEMP 3,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>, <CONST 2>>>>
@ <JNEQ L804, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 2>>
@ <DEFTEMP 4,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 8>>>, <CONST 2>>>>
@ <JNEQ L804, <LSR, <LOADW, <TEMP 4>>, <CONST 8>>, <CONST 2>>
@     result := Unify(av[3], goalframe, 
@ <DEFTEMP 5,
@   <CALL 1,
@     <GLOBAL _NewInt>,
@     <ARG 0,
@       <TIMES,
@         <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@         <LOADW, <OFFSET, <TEMP 4>, <CONST 4>>>>>>>
@ <DEFTEMP 6,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 2>, <CONST 12>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <TEMP 5>>,
@     <ARG 3, <CONST 0>>>>
@ <STOREC, <TEMP 6>, <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L804>
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 7, <GLOBAL _mem>>
@ <DEFTEMP 8, <GLOBAL _av>>
@ <DEFTEMP 9, <OFFSET, <TEMP 8>, <CONST 4>>>
@ <DEFTEMP 10, <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 9>>, <CONST 2>>>>
@ <JNEQ L807, <LSR, <LOADW, <TEMP 10>>, <CONST 8>>, <CONST 2>>
@ <DEFTEMP 11, <OFFSET, <TEMP 8>, <CONST 12>>>
@ <DEFTEMP 12, <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 11>>, <CONST 2>>>>
@ <JNEQ L807, <LSR, <LOADW, <TEMP 12>>, <CONST 8>>, <CONST 2>>
@     if mem[av[1]+1] <> 0 then
@ <DEFTEMP 13, <LOADW, <OFFSET, <TEMP 10>, <CONST 4>>>>
@ <JEQ L805, <TEMP 13>, <CONST 0>>
@       if mem[av[3]+1] mod mem[av[1]+1] = 0 then
@ <DEFTEMP 14,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 12>, <CONST 4>>>>,
@     <ARG 1, <TEMP 13>>>>
@ <JNEQ L805, <TEMP 14>, <CONST 0>>
@         result := Unify(av[2], goalframe, 
@ <DEFTEMP 15,
@   <CALL 2,
@     <GLOBAL int_div>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 11>>, <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 9>>, <CONST 2>>>,
@           <CONST 4>>>>>>
@ <DEFTEMP 16, <CALL 1, <GLOBAL _NewInt>, <ARG 0, <TEMP 15>>>>
@ <DEFTEMP 17,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 8>, <CONST 8>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <TEMP 16>>,
@     <ARG 3, <CONST 0>>>>
@ <STOREC, <TEMP 17>, <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L807>
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 18, <GLOBAL _mem>>
@ <DEFTEMP 19, <GLOBAL _av>>
@ <DEFTEMP 20, <OFFSET, <TEMP 19>, <CONST 8>>>
@ <DEFTEMP 21, <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 20>>, <CONST 2>>>>
@ <JNEQ L810, <LSR, <LOADW, <TEMP 21>>, <CONST 8>>, <CONST 2>>
@ <DEFTEMP 22, <OFFSET, <TEMP 19>, <CONST 12>>>
@ <DEFTEMP 23, <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 22>>, <CONST 2>>>>
@ <JNEQ L810, <LSR, <LOADW, <TEMP 23>>, <CONST 8>>, <CONST 2>>
@     if mem[av[2]+1] <> 0 then
@ <DEFTEMP 24, <LOADW, <OFFSET, <TEMP 21>, <CONST 4>>>>
@ <JEQ L805, <TEMP 24>, <CONST 0>>
@       if mem[av[3]+1] mod mem[av[2]+1] = 0 then
@ <DEFTEMP 25,
@   <CALL 2,
@     <GLOBAL int_mod>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 23>, <CONST 4>>>>,
@     <ARG 1, <TEMP 24>>>>
@ <JNEQ L805, <TEMP 25>, <CONST 0>>
@         result := Unify(av[1], goalframe, 
@ <DEFTEMP 26,
@   <CALL 2,
@     <GLOBAL int_div>,
@     <ARG 0,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 22>>, <CONST 2>>>,
@           <CONST 4>>>>,
@     <ARG 1,
@       <LOADW,
@         <OFFSET,
@           <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 20>>, <CONST 2>>>,
@           <CONST 4>>>>>>
@ <DEFTEMP 27, <CALL 1, <GLOBAL _NewInt>, <ARG 0, <TEMP 26>>>>
@ <DEFTEMP 28,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 19>, <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <TEMP 27>>,
@     <ARG 3, <CONST 0>>>>
@ <STOREC, <TEMP 28>, <REGVAR 0>>
@ <JUMP L805>
@ <LABEL L810>
@     newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g96>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g97>>,
@   <ARG 1, <CONST 35>>>
@ <STOREC, <CONST 0>, <GLOBAL _run>>
@ <LABEL L805>
@   current := (current)+1;
@ <DEFTEMP 29, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 29>>, <CONST 1>>, <TEMP 29>>
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>

_DoTimes:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   result := false;
@ <STOREC, <CONST 0>, <REGVAR 0>>
	set r4, #0
@   if (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[2]], 8) = INT) then
@ <DEFTEMP 1, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 2, <GLOBAL _av>>
	set r6, _av
@ <DEFTEMP 3,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>, <CONST 2>>>>
	add r0, r6, #4
	ldr r0, [r0]
	lsl r0, r0, #2
	add r7, r5, r0
@ <JNEQ L804, <LSR, <LOADW, <TEMP 3>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r7]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L804
@ <DEFTEMP 4,
@   <OFFSET,
@     <TEMP 1>,
@     <LSL, <LOADW, <OFFSET, <TEMP 2>, <CONST 8>>>, <CONST 2>>>>
	add r0, r6, #8
	ldr r0, [r0]
	lsl r0, r0, #2
	add r5, r5, r0
@ <JNEQ L804, <LSR, <LOADW, <TEMP 4>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r5]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L804
@     result := Unify(av[3], goalframe, 
@ <ARG 0,
@   <TIMES,
@     <LOADW, <OFFSET, <TEMP 3>, <CONST 4>>>,
@     <LOADW, <OFFSET, <TEMP 4>, <CONST 4>>>>>
	add r0, r7, #4
	ldr r0, [r0]
	add r1, r5, #4
	ldr r1, [r1]
	mul r0, r0, r1
@ <DEFTEMP 5, <CALL 1, <GLOBAL _NewInt>>>
	bl _NewInt
@ <ARG 3, <CONST 0>>
	set r3, #0
@ <ARG 2, <TEMP 5>>
	mov r2, r0
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 2>, <CONST 12>>>>
	add r0, r6, #12
	ldr r0, [r0]
@ <DEFTEMP 6, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 6>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L805>
	b .L805
@ <LABEL L804>
.L804:
@   elsif (lsr(mem[av[1]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 7, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 8, <GLOBAL _av>>
	set r6, _av
@ <DEFTEMP 9, <OFFSET, <TEMP 8>, <CONST 4>>>
	add r7, r6, #4
@ <DEFTEMP 10, <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 9>>, <CONST 2>>>>
	ldr r0, [r7]
	lsl r0, r0, #2
	add r8, r5, r0
@ <JNEQ L807, <LSR, <LOADW, <TEMP 10>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r8]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L807
@ <DEFTEMP 11, <OFFSET, <TEMP 8>, <CONST 12>>>
	add r9, r6, #12
@ <DEFTEMP 12, <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 11>>, <CONST 2>>>>
	ldr r0, [r9]
	lsl r0, r0, #2
	add r0, r5, r0
@ <JNEQ L807, <LSR, <LOADW, <TEMP 12>>, <CONST 8>>, <CONST 2>>
	ldr r1, [r0]
	lsr r1, r1, #8
	cmp r1, #2
	bne .L807
@     if mem[av[1]+1] <> 0 then
@ <DEFTEMP 13, <LOADW, <OFFSET, <TEMP 10>, <CONST 4>>>>
	add r1, r8, #4
	ldr r8, [r1]
@ <JEQ L805, <TEMP 13>, <CONST 0>>
	cmp r8, #0
	beq .L805
@       if mem[av[3]+1] mod mem[av[1]+1] = 0 then
@ <ARG 1, <TEMP 13>>
	mov r1, r8
@ <ARG 0, <LOADW, <OFFSET, <TEMP 12>, <CONST 4>>>>
	mov r8, r0
	add r0, r8, #4
	ldr r0, [r0]
@ <DEFTEMP 14, <CALL 2, <GLOBAL int_mod>>>
	bl int_mod
@ <JNEQ L805, <TEMP 14>, <CONST 0>>
	cmp r0, #0
	bne .L805
@         result := Unify(av[2], goalframe, 
@ <ARG 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 9>>, <CONST 2>>>,
@       <CONST 4>>>>
	ldr r0, [r7]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 7>, <LSL, <LOADW, <TEMP 11>>, <CONST 2>>>,
@       <CONST 4>>>>
	ldr r0, [r9]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 15, <CALL 2, <GLOBAL int_div>>>
	bl int_div
@ <ARG 0, <TEMP 15>>
@ <DEFTEMP 16, <CALL 1, <GLOBAL _NewInt>>>
	bl _NewInt
@ <ARG 3, <CONST 0>>
	set r3, #0
@ <ARG 2, <TEMP 16>>
	mov r2, r0
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 8>, <CONST 8>>>>
	add r0, r6, #8
	ldr r0, [r0]
@ <DEFTEMP 17, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 17>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L805>
	b .L805
@ <LABEL L807>
.L807:
@   elsif (lsr(mem[av[2]], 8) = INT) and (lsr(mem[av[3]], 8) = INT) then
@ <DEFTEMP 18, <GLOBAL _mem>>
	set r5, _mem
@ <DEFTEMP 19, <GLOBAL _av>>
	set r6, _av
@ <DEFTEMP 20, <OFFSET, <TEMP 19>, <CONST 8>>>
	add r7, r6, #8
@ <DEFTEMP 21, <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 20>>, <CONST 2>>>>
	ldr r0, [r7]
	lsl r0, r0, #2
	add r8, r5, r0
@ <JNEQ L810, <LSR, <LOADW, <TEMP 21>>, <CONST 8>>, <CONST 2>>
	ldr r0, [r8]
	lsr r0, r0, #8
	cmp r0, #2
	bne .L810
@ <DEFTEMP 22, <OFFSET, <TEMP 19>, <CONST 12>>>
	add r9, r6, #12
@ <DEFTEMP 23, <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 22>>, <CONST 2>>>>
	ldr r0, [r9]
	lsl r0, r0, #2
	add r0, r5, r0
@ <JNEQ L810, <LSR, <LOADW, <TEMP 23>>, <CONST 8>>, <CONST 2>>
	ldr r1, [r0]
	lsr r1, r1, #8
	cmp r1, #2
	bne .L810
@     if mem[av[2]+1] <> 0 then
@ <DEFTEMP 24, <LOADW, <OFFSET, <TEMP 21>, <CONST 4>>>>
	add r1, r8, #4
	ldr r8, [r1]
@ <JEQ L805, <TEMP 24>, <CONST 0>>
	cmp r8, #0
	beq .L805
@       if mem[av[3]+1] mod mem[av[2]+1] = 0 then
@ <ARG 1, <TEMP 24>>
	mov r1, r8
@ <ARG 0, <LOADW, <OFFSET, <TEMP 23>, <CONST 4>>>>
	mov r8, r0
	add r0, r8, #4
	ldr r0, [r0]
@ <DEFTEMP 25, <CALL 2, <GLOBAL int_mod>>>
	bl int_mod
@ <JNEQ L805, <TEMP 25>, <CONST 0>>
	cmp r0, #0
	bne .L805
@         result := Unify(av[1], goalframe, 
@ <ARG 1,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 20>>, <CONST 2>>>,
@       <CONST 4>>>>
	ldr r0, [r7]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r1, [r0]
@ <ARG 0,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <TEMP 18>, <LSL, <LOADW, <TEMP 22>>, <CONST 2>>>,
@       <CONST 4>>>>
	ldr r0, [r9]
	lsl r0, r0, #2
	add r0, r5, r0
	add r0, r0, #4
	ldr r0, [r0]
@ <DEFTEMP 26, <CALL 2, <GLOBAL int_div>>>
	bl int_div
@ <ARG 0, <TEMP 26>>
@ <DEFTEMP 27, <CALL 1, <GLOBAL _NewInt>>>
	bl _NewInt
@ <ARG 3, <CONST 0>>
	set r3, #0
@ <ARG 2, <TEMP 27>>
	mov r2, r0
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <TEMP 19>, <CONST 4>>>>
	add r0, r6, #4
	ldr r0, [r0]
@ <DEFTEMP 28, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <STOREC, <TEMP 28>, <REGVAR 0>>
	mov r4, r0
@ <JUMP L805>
	b .L805
@ <LABEL L810>
.L810:
@     newline(); print_string("Error: "); print_string("times/3 needs at least two integers"); run := false
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g96>>
	set r0, g96
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 35>>
	set r1, #35
@ <ARG 0, <GLOBAL g97>>
	set r0, g97
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <STOREC, <CONST 0>, <GLOBAL _run>>
	set r0, #0
	set r1, _run
	strb r0, [r1]
@ <LABEL L805>
.L805:
@   current := (current)+1;
@ <DEFTEMP 29, <GLOBAL _current>>
	set r5, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 29>>, <CONST 1>>, <TEMP 29>>
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
@   return result
@ <RESULTW, <LOADC, <REGVAR 0>>>
	mov r0, r4
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoEqual(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return Unify(av[1], goalframe, av[2], goalframe)
@ <RESULTW,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2,
@       <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 2>, <CONST 4>>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>>
@ <JUMP L827>
@ <LABEL L827>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return Unify(av[1], goalframe, av[2], goalframe)
@ <RESULTW,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@     <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@     <ARG 2, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 8>>>>,
@     <ARG 3, <LOADW, <GLOBAL _goalframe>>>>>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@   return Unify(av[1], goalframe, av[2], goalframe)
@ <DEFTEMP 2, <GLOBAL _av>>
@ <DEFTEMP 3, <LOADW, <GLOBAL _goalframe>>>
@ <DEFTEMP 4,
@   <CALL 4,
@     <GLOBAL _Unify>,
@     <ARG 0, <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>>,
@     <ARG 1, <TEMP 3>>,
@     <ARG 2, <LOADW, <OFFSET, <TEMP 2>, <CONST 8>>>>,
@     <ARG 3, <TEMP 3>>>>
@ <RESULTW, <TEMP 4>>

_DoEqual:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
	set r4, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   return Unify(av[1], goalframe, av[2], goalframe)
@ <DEFTEMP 2, <GLOBAL _av>>
	set r4, _av
@ <DEFTEMP 3, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r5, [r0]
@ <ARG 3, <TEMP 3>>
	mov r3, r5
@ <ARG 2, <LOADW, <OFFSET, <TEMP 2>, <CONST 8>>>>
	add r0, r4, #8
	ldr r2, [r0]
@ <ARG 1, <TEMP 3>>
	mov r1, r5
@ <ARG 0, <LOADW, <OFFSET, <TEMP 2>, <CONST 4>>>>
	add r0, r4, #4
	ldr r0, [r0]
@ <DEFTEMP 4, <CALL 4, <GLOBAL _Unify>>>
	bl _Unify
@ <RESULTW, <TEMP 4>>
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoInteger(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return (lsr(mem[av[1]], 8) = INT)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <LOADW,
@               <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@             <CONST 4>>>>,
@       <CONST 8>>,
@     <CONST 2>>>
@ <JUMP L828>
@ <LABEL L828>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return (lsr(mem[av[1]], 8) = INT)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 2>>>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@   return (lsr(mem[av[1]], 8) = INT)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 2>>>

_DoInteger:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
	set r4, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   return (lsr(mem[av[1]], 8) = INT)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 2>>>
	set r0, _mem
	set r1, _av
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #2
	mov r0, #0
	moveq r0, #1
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoChar(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return (lsr(mem[av[1]], 8) = CHRCTR)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <TIMES,
@             <LOADW,
@               <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>,
@             <CONST 4>>>>,
@       <CONST 8>>,
@     <CONST 3>>>
@ <JUMP L829>
@ <LABEL L829>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return (lsr(mem[av[1]], 8) = CHRCTR)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 3>>>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@   return (lsr(mem[av[1]], 8) = CHRCTR)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 3>>>

_DoChar:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
	set r4, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   return (lsr(mem[av[1]], 8) = CHRCTR)
@ <RESULTW,
@   <EQ,
@     <LSR,
@       <LOADW,
@         <OFFSET,
@           <GLOBAL _mem>,
@           <LSL, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>, <CONST 2>>>>,
@       <CONST 8>>,
@     <CONST 3>>>
	set r0, _mem
	set r1, _av
	add r1, r1, #4
	ldr r1, [r1]
	lsl r1, r1, #2
	add r0, r0, r1
	ldr r0, [r0]
	lsr r0, r0, #8
	cmp r0, #3
	mov r0, #0
	moveq r0, #1
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoPrint(): boolean;
@ Initial code:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>, <STATLINK, <CONST 0>>>
@   PrintTerm(av[1], goalframe, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0,
@     <LOADW, <OFFSET, <GLOBAL _av>, <TIMES, <CONST 1>, <CONST 4>>>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return true
@ <RESULTW, <CONST 1>>
@ <JUMP L830>
@ <LABEL L830>

@ After simplification:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   PrintTerm(av[1], goalframe, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return true
@ <RESULTW, <CONST 1>>

@ After sharing:
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
@   PrintTerm(av[1], goalframe, MAXPRIO);
@ <CALL 3,
@   <GLOBAL _PrintTerm>,
@   <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>,
@   <ARG 1, <LOADW, <GLOBAL _goalframe>>>,
@   <ARG 2, <CONST 2>>>
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@   return true
@ <RESULTW, <CONST 1>>

_DoPrint:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   GetArgs();
@ <CALL 0, <GLOBAL _GetArgs>>
	bl _GetArgs
@   PrintTerm(av[1], goalframe, MAXPRIO);
@ <ARG 2, <CONST 2>>
	set r2, #2
@ <ARG 1, <LOADW, <GLOBAL _goalframe>>>
	set r0, _goalframe
	ldr r1, [r0]
@ <ARG 0, <LOADW, <OFFSET, <GLOBAL _av>, <CONST 4>>>>
	set r0, _av
	add r0, r0, #4
	ldr r0, [r0]
@ <CALL 3, <GLOBAL _PrintTerm>>
	bl _PrintTerm
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
	set r4, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   return true
@ <RESULTW, <CONST 1>>
	set r0, #1
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoNl(): boolean;
@ Initial code:
@   newline();
@ <CALL 0, <GLOBAL newline>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return true
@ <RESULTW, <CONST 1>>
@ <JUMP L831>
@ <LABEL L831>

@ After simplification:
@   newline();
@ <CALL 0, <GLOBAL newline>>
@   current := (current)+1;
@ <STOREW,
@   <PLUS, <LOADW, <GLOBAL _current>>, <CONST 1>>,
@   <GLOBAL _current>>
@   return true
@ <RESULTW, <CONST 1>>

@ After sharing:
@   newline();
@ <CALL 0, <GLOBAL newline>>
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
@   return true
@ <RESULTW, <CONST 1>>

_DoNl:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   newline();
@ <CALL 0, <GLOBAL newline>>
	bl newline
@   current := (current)+1;
@ <DEFTEMP 1, <GLOBAL _current>>
	set r4, _current
@ <STOREW, <PLUS, <LOADW, <TEMP 1>>, <CONST 1>>, <TEMP 1>>
	ldr r0, [r4]
	add r0, r0, #1
	str r0, [r4]
@   return true
@ <RESULTW, <CONST 1>>
	set r0, #1
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc DoBuiltin(action: integer): boolean;
@ Initial code:
@   case action of
@ <JCASE 11 L833,
@   <MINUS, <LOADW, <OFFSET, <LOCAL 0>, <CONST 40>>>, <CONST 1>>>
@ <LABEL L835>
@     CUT:      return DoCut()
@ <RESULTW, <CALL 0, <GLOBAL _DoCut>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L836>
@   | CALL:     return DoCall()
@ <RESULTW, <CALL 0, <GLOBAL _DoCall>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L837>
@   | PLUS:     return DoPlus()
@ <RESULTW, <CALL 0, <GLOBAL _DoPlus>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L838>
@   | TIMES:    return DoTimes()
@ <RESULTW, <CALL 0, <GLOBAL _DoTimes>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L839>
@   | ISINT:    return DoInteger()
@ <RESULTW, <CALL 0, <GLOBAL _DoInteger>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L840>
@   | ISCHAR:   return DoChar()
@ <RESULTW, <CALL 0, <GLOBAL _DoChar>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L841>
@   | NAFF:     return DoNot()
@ <RESULTW, <CALL 0, <GLOBAL _DoNot>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L842>
@   | EQUALITY: return DoEqual()
@ <RESULTW, <CALL 0, <GLOBAL _DoEqual>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L843>
@   | FAIL:     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L844>
@   | PRINT:    return DoPrint()
@ <RESULTW, <CALL 0, <GLOBAL _DoPrint>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L845>
@   | NL:	      return DoNl()
@ <RESULTW, <CALL 0, <GLOBAL _DoNl>, <STATLINK, <CONST 0>>>>
@ <JUMP L832>
@ <JUMP L834>
@ <LABEL L833>
@     newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g98>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g99>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L834>
@ <LABEL L832>

@ After simplification:
@   case action of
@ <JCASE 11 L833, <MINUS, <LOADW, <LOCAL 40>>, <CONST 1>>>
@ <LABEL L835>
@     CUT:      return DoCut()
@ <RESULTW, <CALL 0, <GLOBAL _DoCut>>>
@ <JUMP L832>
@ <LABEL L836>
@   | CALL:     return DoCall()
@ <RESULTW, <CALL 0, <GLOBAL _DoCall>>>
@ <JUMP L832>
@ <LABEL L837>
@   | PLUS:     return DoPlus()
@ <RESULTW, <CALL 0, <GLOBAL _DoPlus>>>
@ <JUMP L832>
@ <LABEL L838>
@   | TIMES:    return DoTimes()
@ <RESULTW, <CALL 0, <GLOBAL _DoTimes>>>
@ <JUMP L832>
@ <LABEL L839>
@   | ISINT:    return DoInteger()
@ <RESULTW, <CALL 0, <GLOBAL _DoInteger>>>
@ <JUMP L832>
@ <LABEL L840>
@   | ISCHAR:   return DoChar()
@ <RESULTW, <CALL 0, <GLOBAL _DoChar>>>
@ <JUMP L832>
@ <LABEL L841>
@   | NAFF:     return DoNot()
@ <RESULTW, <CALL 0, <GLOBAL _DoNot>>>
@ <JUMP L832>
@ <LABEL L842>
@   | EQUALITY: return DoEqual()
@ <RESULTW, <CALL 0, <GLOBAL _DoEqual>>>
@ <JUMP L832>
@ <LABEL L843>
@   | FAIL:     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L832>
@ <LABEL L844>
@   | PRINT:    return DoPrint()
@ <RESULTW, <CALL 0, <GLOBAL _DoPrint>>>
@ <JUMP L832>
@ <LABEL L845>
@   | NL:	      return DoNl()
@ <RESULTW, <CALL 0, <GLOBAL _DoNl>>>
@ <JUMP L832>
@ <LABEL L833>
@     newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g98>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g99>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L832>

@ After sharing:
@   case action of
@ <JCASE 11 L833, <MINUS, <LOADW, <LOCAL 40>>, <CONST 1>>>
@ <LABEL L835>
@     CUT:      return DoCut()
@ <DEFTEMP 1, <CALL 0, <GLOBAL _DoCut>>>
@ <RESULTW, <TEMP 1>>
@ <JUMP L832>
@ <LABEL L836>
@   | CALL:     return DoCall()
@ <DEFTEMP 2, <CALL 0, <GLOBAL _DoCall>>>
@ <RESULTW, <TEMP 2>>
@ <JUMP L832>
@ <LABEL L837>
@   | PLUS:     return DoPlus()
@ <DEFTEMP 3, <CALL 0, <GLOBAL _DoPlus>>>
@ <RESULTW, <TEMP 3>>
@ <JUMP L832>
@ <LABEL L838>
@   | TIMES:    return DoTimes()
@ <DEFTEMP 4, <CALL 0, <GLOBAL _DoTimes>>>
@ <RESULTW, <TEMP 4>>
@ <JUMP L832>
@ <LABEL L839>
@   | ISINT:    return DoInteger()
@ <DEFTEMP 5, <CALL 0, <GLOBAL _DoInteger>>>
@ <RESULTW, <TEMP 5>>
@ <JUMP L832>
@ <LABEL L840>
@   | ISCHAR:   return DoChar()
@ <DEFTEMP 6, <CALL 0, <GLOBAL _DoChar>>>
@ <RESULTW, <TEMP 6>>
@ <JUMP L832>
@ <LABEL L841>
@   | NAFF:     return DoNot()
@ <DEFTEMP 7, <CALL 0, <GLOBAL _DoNot>>>
@ <RESULTW, <TEMP 7>>
@ <JUMP L832>
@ <LABEL L842>
@   | EQUALITY: return DoEqual()
@ <DEFTEMP 8, <CALL 0, <GLOBAL _DoEqual>>>
@ <RESULTW, <TEMP 8>>
@ <JUMP L832>
@ <LABEL L843>
@   | FAIL:     return false
@ <RESULTW, <CONST 0>>
@ <JUMP L832>
@ <LABEL L844>
@   | PRINT:    return DoPrint()
@ <DEFTEMP 9, <CALL 0, <GLOBAL _DoPrint>>>
@ <RESULTW, <TEMP 9>>
@ <JUMP L832>
@ <LABEL L845>
@   | NL:	      return DoNl()
@ <DEFTEMP 10, <CALL 0, <GLOBAL _DoNl>>>
@ <RESULTW, <TEMP 10>>
@ <JUMP L832>
@ <LABEL L833>
@     newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g98>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 2,
@   <GLOBAL print_string>,
@   <ARG 0, <GLOBAL g99>>,
@   <ARG 1, <CONST 7>>>
@ <CALL 0, <GLOBAL newline>>
@ <CALL 1, <GLOBAL exit>, <ARG 0, <CONST 2>>>
@ <LABEL L832>

_DoBuiltin:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   case action of
@ <JCASE 11 L833, <MINUS, <LOADW, <LOCAL 40>>, <CONST 1>>>
	set ip, #40
	add r0, fp, ip
	ldr r0, [r0]
	sub r0, r0, #1
	cmp r0, #11
	ldrlo pc, [pc, r0, LSL #2]
	b .L833
	.word .L835
	.word .L836
	.word .L837
	.word .L838
	.word .L839
	.word .L840
	.word .L841
	.word .L842
	.word .L843
	.word .L844
	.word .L845
@ <LABEL L835>
.L835:
@     CUT:      return DoCut()
@ <DEFTEMP 1, <CALL 0, <GLOBAL _DoCut>>>
	bl _DoCut
@ <RESULTW, <TEMP 1>>
@ <JUMP L832>
	b .L832
@ <LABEL L836>
.L836:
@   | CALL:     return DoCall()
@ <DEFTEMP 2, <CALL 0, <GLOBAL _DoCall>>>
	bl _DoCall
@ <RESULTW, <TEMP 2>>
@ <JUMP L832>
	b .L832
@ <LABEL L837>
.L837:
@   | PLUS:     return DoPlus()
@ <DEFTEMP 3, <CALL 0, <GLOBAL _DoPlus>>>
	bl _DoPlus
@ <RESULTW, <TEMP 3>>
@ <JUMP L832>
	b .L832
@ <LABEL L838>
.L838:
@   | TIMES:    return DoTimes()
@ <DEFTEMP 4, <CALL 0, <GLOBAL _DoTimes>>>
	bl _DoTimes
@ <RESULTW, <TEMP 4>>
@ <JUMP L832>
	b .L832
@ <LABEL L839>
.L839:
@   | ISINT:    return DoInteger()
@ <DEFTEMP 5, <CALL 0, <GLOBAL _DoInteger>>>
	bl _DoInteger
@ <RESULTW, <TEMP 5>>
@ <JUMP L832>
	b .L832
@ <LABEL L840>
.L840:
@   | ISCHAR:   return DoChar()
@ <DEFTEMP 6, <CALL 0, <GLOBAL _DoChar>>>
	bl _DoChar
@ <RESULTW, <TEMP 6>>
@ <JUMP L832>
	b .L832
@ <LABEL L841>
.L841:
@   | NAFF:     return DoNot()
@ <DEFTEMP 7, <CALL 0, <GLOBAL _DoNot>>>
	bl _DoNot
@ <RESULTW, <TEMP 7>>
@ <JUMP L832>
	b .L832
@ <LABEL L842>
.L842:
@   | EQUALITY: return DoEqual()
@ <DEFTEMP 8, <CALL 0, <GLOBAL _DoEqual>>>
	bl _DoEqual
@ <RESULTW, <TEMP 8>>
@ <JUMP L832>
	b .L832
@ <LABEL L843>
.L843:
@   | FAIL:     return false
@ <RESULTW, <CONST 0>>
	set r0, #0
@ <JUMP L832>
	b .L832
@ <LABEL L844>
.L844:
@   | PRINT:    return DoPrint()
@ <DEFTEMP 9, <CALL 0, <GLOBAL _DoPrint>>>
	bl _DoPrint
@ <RESULTW, <TEMP 9>>
@ <JUMP L832>
	b .L832
@ <LABEL L845>
.L845:
@   | NL:	      return DoNl()
@ <DEFTEMP 10, <CALL 0, <GLOBAL _DoNl>>>
	bl _DoNl
@ <RESULTW, <TEMP 10>>
@ <JUMP L832>
	b .L832
@ <LABEL L833>
.L833:
@     newline(); print_string("Panic: "); print_string("bad tag" (*action:1, " in ", "DoBuiltin"*)); newline(); exit(2)
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g98>>
	set r0, g98
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <ARG 1, <CONST 7>>
	set r1, #7
@ <ARG 0, <GLOBAL g99>>
	set r0, g99
@ <CALL 2, <GLOBAL print_string>>
	bl print_string
@ <CALL 0, <GLOBAL newline>>
	bl newline
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <CALL 1, <GLOBAL exit>>
	bl exit
@ <LABEL L832>
.L832:
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc Initialize();
@ Initial code:
@   dflag := false; errcount := 0;
@ <STOREC, <CONST 0>, <GLOBAL _dflag>>
@ <STOREW, <CONST 0>, <GLOBAL _errcount>>
@   pbchar := ENDFILE; charptr := 0;
@ <STOREC, <CONST 127>, <GLOBAL _pbchar>>
@ <STOREW, <CONST 0>, <GLOBAL _charptr>>
@   hp := 0; InitSymbols();
@ <STOREW, <CONST 0>, <GLOBAL _hp>>
@ <CALL 0, <GLOBAL _InitSymbols>, <STATLINK, <CONST 0>>>
@   for i := 1 to MAXARITY do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <CONST 63>, <REGVAR 2>>
@ <LABEL L847>
@ <JGT L848, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
@     p := HeapAlloc(TERM_SIZE);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 2>>>,
@   <REGVAR 1>>
@     mem[p] := lsl(REF, 8) + TERM_SIZE;
@ <STOREW,
@   <PLUS, <LSL, <CONST 5>, <CONST 8>>, <CONST 2>>,
@   <OFFSET, <GLOBAL _mem>, <TIMES, <LOADW, <REGVAR 1>>, <CONST 4>>>>
@     mem[p+1] := i; refnode[i] := p
@ <STOREW,
@   <LOADW, <REGVAR 0>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <REGVAR 1>>, <CONST 1>>, <CONST 4>>>>
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <GLOBAL _refnode>, <TIMES, <LOADW, <REGVAR 0>>, <CONST 4>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L847>
@ <LABEL L848>
@   callbody := HeapAlloc(2);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _HeapAlloc>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 2>>>,
@   <GLOBAL _callbody>>
@   mem[callbody] := MakeRef(1);
@ <STOREW,
@   <CALL 1,
@     <GLOBAL _MakeRef>,
@     <STATLINK, <CONST 0>>,
@     <ARG 0, <CONST 1>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <LOADW, <GLOBAL _callbody>>, <CONST 4>>>>
@   mem[(callbody)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <TIMES, <PLUS, <LOADW, <GLOBAL _callbody>>, <CONST 1>>, <CONST 4>>>>
@ <LABEL L846>

@ After simplification:
@   dflag := false; errcount := 0;
@ <STOREC, <CONST 0>, <GLOBAL _dflag>>
@ <STOREW, <CONST 0>, <GLOBAL _errcount>>
@   pbchar := ENDFILE; charptr := 0;
@ <STOREC, <CONST 127>, <GLOBAL _pbchar>>
@ <STOREW, <CONST 0>, <GLOBAL _charptr>>
@   hp := 0; InitSymbols();
@ <STOREW, <CONST 0>, <GLOBAL _hp>>
@ <CALL 0, <GLOBAL _InitSymbols>>
@   for i := 1 to MAXARITY do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <CONST 63>, <REGVAR 2>>
@ <LABEL L847>
@ <JGT L848, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
@     p := HeapAlloc(TERM_SIZE);
@ <STOREW, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>, <REGVAR 1>>
@     mem[p] := lsl(REF, 8) + TERM_SIZE;
@ <STOREW,
@   <CONST 1282>,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>
@     mem[p+1] := i; refnode[i] := p
@ <STOREW,
@   <LOADW, <REGVAR 0>>,
@   <OFFSET,
@     <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>,
@     <CONST 4>>>
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <GLOBAL _refnode>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L847>
@ <LABEL L848>
@   callbody := HeapAlloc(2);
@ <STOREW,
@   <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>,
@   <GLOBAL _callbody>>
@   mem[callbody] := MakeRef(1);
@ <STOREW,
@   <CALL 1, <GLOBAL _MakeRef>, <ARG 0, <CONST 1>>>,
@   <OFFSET,
@     <GLOBAL _mem>,
@     <LSL, <LOADW, <GLOBAL _callbody>>, <CONST 2>>>>
@   mem[(callbody)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <LSL, <LOADW, <GLOBAL _callbody>>, <CONST 2>>>,
@     <CONST 4>>>

@ After sharing:
@   dflag := false; errcount := 0;
@ <STOREC, <CONST 0>, <GLOBAL _dflag>>
@ <STOREW, <CONST 0>, <GLOBAL _errcount>>
@   pbchar := ENDFILE; charptr := 0;
@ <STOREC, <CONST 127>, <GLOBAL _pbchar>>
@ <STOREW, <CONST 0>, <GLOBAL _charptr>>
@   hp := 0; InitSymbols();
@ <STOREW, <CONST 0>, <GLOBAL _hp>>
@ <CALL 0, <GLOBAL _InitSymbols>>
@   for i := 1 to MAXARITY do
@ <STOREW, <CONST 1>, <REGVAR 0>>
@ <STOREW, <CONST 63>, <REGVAR 2>>
@ <LABEL L847>
@ <JGT L848, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
@     p := HeapAlloc(TERM_SIZE);
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>>
@ <STOREW, <TEMP 1>, <REGVAR 1>>
@     mem[p] := lsl(REF, 8) + TERM_SIZE;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>
@ <STOREW, <CONST 1282>, <TEMP 2>>
@     mem[p+1] := i; refnode[i] := p
@ <STOREW, <LOADW, <REGVAR 0>>, <OFFSET, <TEMP 2>, <CONST 4>>>
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <GLOBAL _refnode>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
@ <JUMP L847>
@ <LABEL L848>
@   callbody := HeapAlloc(2);
@ <DEFTEMP 3, <CALL 1, <GLOBAL _HeapAlloc>, <ARG 0, <CONST 2>>>>
@ <DEFTEMP 4, <GLOBAL _callbody>>
@ <STOREW, <TEMP 3>, <TEMP 4>>
@   mem[callbody] := MakeRef(1);
@ <DEFTEMP 5, <CALL 1, <GLOBAL _MakeRef>, <ARG 0, <CONST 1>>>>
@ <DEFTEMP 6, <GLOBAL _mem>>
@ <STOREW,
@   <TEMP 5>,
@   <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 4>>, <CONST 2>>>>
@   mem[(callbody)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 4>>, <CONST 2>>>,
@     <CONST 4>>>

_Initialize:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   dflag := false; errcount := 0;
@ <STOREC, <CONST 0>, <GLOBAL _dflag>>
	set r0, #0
	set r1, _dflag
	strb r0, [r1]
@ <STOREW, <CONST 0>, <GLOBAL _errcount>>
	set r0, #0
	set r1, _errcount
	str r0, [r1]
@   pbchar := ENDFILE; charptr := 0;
@ <STOREC, <CONST 127>, <GLOBAL _pbchar>>
	set r0, #127
	set r1, _pbchar
	strb r0, [r1]
@ <STOREW, <CONST 0>, <GLOBAL _charptr>>
	set r0, #0
	set r1, _charptr
	str r0, [r1]
@   hp := 0; InitSymbols();
@ <STOREW, <CONST 0>, <GLOBAL _hp>>
	set r0, #0
	set r1, _hp
	str r0, [r1]
@ <CALL 0, <GLOBAL _InitSymbols>>
	bl _InitSymbols
@   for i := 1 to MAXARITY do
@ <STOREW, <CONST 1>, <REGVAR 0>>
	set r4, #1
@ <STOREW, <CONST 63>, <REGVAR 2>>
	set r6, #63
@ <LABEL L847>
.L847:
@ <JGT L848, <LOADW, <REGVAR 0>>, <LOADW, <REGVAR 2>>>
	cmp r4, r6
	bgt .L848
@     p := HeapAlloc(TERM_SIZE);
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <DEFTEMP 1, <CALL 1, <GLOBAL _HeapAlloc>>>
	bl _HeapAlloc
@ <STOREW, <TEMP 1>, <REGVAR 1>>
	mov r5, r0
@     mem[p] := lsl(REF, 8) + TERM_SIZE;
@ <DEFTEMP 2,
@   <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 1>>, <CONST 2>>>>
	set r0, _mem
	lsl r1, r5, #2
	add r7, r0, r1
@ <STOREW, <CONST 1282>, <TEMP 2>>
	set r0, #1282
	str r0, [r7]
@     mem[p+1] := i; refnode[i] := p
@ <STOREW, <LOADW, <REGVAR 0>>, <OFFSET, <TEMP 2>, <CONST 4>>>
	add r0, r7, #4
	str r4, [r0]
@ <STOREW,
@   <LOADW, <REGVAR 1>>,
@   <OFFSET, <GLOBAL _refnode>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>>
	set r0, _refnode
	lsl r1, r4, #2
	add r0, r0, r1
	str r5, [r0]
@ <STOREW, <PLUS, <LOADW, <REGVAR 0>>, <CONST 1>>, <REGVAR 0>>
	add r4, r4, #1
@ <JUMP L847>
	b .L847
@ <LABEL L848>
.L848:
@   callbody := HeapAlloc(2);
@ <ARG 0, <CONST 2>>
	set r0, #2
@ <DEFTEMP 3, <CALL 1, <GLOBAL _HeapAlloc>>>
	bl _HeapAlloc
@ <DEFTEMP 4, <GLOBAL _callbody>>
	set r7, _callbody
@ <STOREW, <TEMP 3>, <TEMP 4>>
	str r0, [r7]
@   mem[callbody] := MakeRef(1);
@ <ARG 0, <CONST 1>>
	set r0, #1
@ <DEFTEMP 5, <CALL 1, <GLOBAL _MakeRef>>>
	bl _MakeRef
@ <DEFTEMP 6, <GLOBAL _mem>>
	set r8, _mem
@ <STOREW,
@   <TEMP 5>,
@   <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 4>>, <CONST 2>>>>
	ldr r1, [r7]
	lsl r1, r1, #2
	add r1, r8, r1
	str r0, [r1]
@   mem[(callbody)+1] := NULL
@ <STOREW,
@   <CONST 0>,
@   <OFFSET,
@     <OFFSET, <TEMP 6>, <LSL, <LOADW, <TEMP 4>>, <CONST 2>>>,
@     <CONST 4>>>
	set r0, #0
	ldr r1, [r7]
	lsl r1, r1, #2
	add r1, r8, r1
	add r1, r1, #4
	str r0, [r1]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc ReadFile();
@ Initial code:
@   lineno := 1;
@ <STOREW, <CONST 1>, <GLOBAL _lineno>>
@   repeat
@ <LABEL L850>
@     hmark := hp;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _hmark>>
@     c := ReadClause();
@ <STOREW,
@   <CALL 0, <GLOBAL _ReadClause>, <STATLINK, <CONST 0>>>,
@   <REGVAR 0>>
@     if c <> NULL then
@ <JNEQ L852, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L853>
@ <LABEL L852>
@       if dflag then PrintClause(c) end;	
@ <JNEQ L855, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <JUMP L856>
@ <LABEL L855>
@ <CALL 1,
@   <GLOBAL _PrintClause>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <REGVAR 0>>>>
@ <JUMP L857>
@ <LABEL L856>
@ <LABEL L857>
@       if mem[c+3] <> NULL then
@ <JNEQ L858,
@   <LOADW,
@     <OFFSET,
@       <GLOBAL _mem>,
@       <TIMES, <PLUS, <LOADW, <REGVAR 0>>, <CONST 3>>, <CONST 4>>>>,
@   <CONST 0>>
@ <JUMP L859>
@ <LABEL L858>
@         AddClause(c)
@ <CALL 1,
@   <GLOBAL _AddClause>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <REGVAR 0>>>>
@ <JUMP L860>
@ <LABEL L859>
@         Execute(c);
@ <CALL 1,
@   <GLOBAL _Execute>,
@   <STATLINK, <CONST 0>>,
@   <ARG 0, <LOADW, <REGVAR 0>>>>
@ 	hp := hmark
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
@ <LABEL L860>
@ <JUMP L854>
@ <LABEL L853>
@ <LABEL L854>
@ <JEQ L851, <LOADW, <REGVAR 0>>, <CONST 0>>
@ <JUMP L850>
@ <LABEL L851>
@ <LABEL L849>

@ After simplification:
@   lineno := 1;
@ <STOREW, <CONST 1>, <GLOBAL _lineno>>
@ <LABEL L850>
@     hmark := hp;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _hmark>>
@     c := ReadClause();
@ <STOREW, <CALL 0, <GLOBAL _ReadClause>>, <REGVAR 0>>
@     if c <> NULL then
@ <JEQ L854, <LOADW, <REGVAR 0>>, <CONST 0>>
@       if dflag then PrintClause(c) end;	
@ <JEQ L857, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <CALL 1, <GLOBAL _PrintClause>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ <LABEL L857>
@       if mem[c+3] <> NULL then
@ <JEQ L859,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@         AddClause(c)
@ <CALL 1, <GLOBAL _AddClause>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ <JUMP L854>
@ <LABEL L859>
@         Execute(c);
@ <CALL 1, <GLOBAL _Execute>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ 	hp := hmark
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
@ <LABEL L854>
@ <JNEQ L850, <LOADW, <REGVAR 0>>, <CONST 0>>

@ After sharing:
@   lineno := 1;
@ <STOREW, <CONST 1>, <GLOBAL _lineno>>
@ <LABEL L850>
@     hmark := hp;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _hmark>>
@     c := ReadClause();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ReadClause>>>
@ <STOREW, <TEMP 1>, <REGVAR 0>>
@     if c <> NULL then
@ <JEQ L854, <LOADW, <REGVAR 0>>, <CONST 0>>
@       if dflag then PrintClause(c) end;	
@ <JEQ L857, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
@ <CALL 1, <GLOBAL _PrintClause>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ <LABEL L857>
@       if mem[c+3] <> NULL then
@ <JEQ L859,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <CONST 0>>
@         AddClause(c)
@ <CALL 1, <GLOBAL _AddClause>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ <JUMP L854>
@ <LABEL L859>
@         Execute(c);
@ <CALL 1, <GLOBAL _Execute>, <ARG 0, <LOADW, <REGVAR 0>>>>
@ 	hp := hmark
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
@ <LABEL L854>
@ <JNEQ L850, <LOADW, <REGVAR 0>>, <CONST 0>>

_ReadFile:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   lineno := 1;
@ <STOREW, <CONST 1>, <GLOBAL _lineno>>
	set r0, #1
	set r1, _lineno
	str r0, [r1]
@ <LABEL L850>
.L850:
@     hmark := hp;
@ <STOREW, <LOADW, <GLOBAL _hp>>, <GLOBAL _hmark>>
	set r0, _hp
	ldr r0, [r0]
	set r1, _hmark
	str r0, [r1]
@     c := ReadClause();
@ <DEFTEMP 1, <CALL 0, <GLOBAL _ReadClause>>>
	bl _ReadClause
@ <STOREW, <TEMP 1>, <REGVAR 0>>
	mov r4, r0
@     if c <> NULL then
@ <JEQ L854, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	beq .L854
@       if dflag then PrintClause(c) end;	
@ <JEQ L857, <LOADC, <GLOBAL _dflag>>, <CONST 0>>
	set r0, _dflag
	ldrb r0, [r0]
	cmp r0, #0
	beq .L857
@ <ARG 0, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _PrintClause>>
	bl _PrintClause
@ <LABEL L857>
.L857:
@       if mem[c+3] <> NULL then
@ <JEQ L859,
@   <LOADW,
@     <OFFSET,
@       <OFFSET, <GLOBAL _mem>, <LSL, <LOADW, <REGVAR 0>>, <CONST 2>>>,
@       <CONST 12>>>,
@   <CONST 0>>
	set r0, _mem
	lsl r1, r4, #2
	add r0, r0, r1
	add r0, r0, #12
	ldr r0, [r0]
	cmp r0, #0
	beq .L859
@         AddClause(c)
@ <ARG 0, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _AddClause>>
	bl _AddClause
@ <JUMP L854>
	b .L854
@ <LABEL L859>
.L859:
@         Execute(c);
@ <ARG 0, <LOADW, <REGVAR 0>>>
	mov r0, r4
@ <CALL 1, <GLOBAL _Execute>>
	bl _Execute
@ 	hp := hmark
@ <STOREW, <LOADW, <GLOBAL _hmark>>, <GLOBAL _hp>>
	set r0, _hmark
	ldr r0, [r0]
	set r1, _hp
	str r0, [r1]
@ <LABEL L854>
.L854:
@ <JNEQ L850, <LOADW, <REGVAR 0>>, <CONST 0>>
	cmp r4, #0
	bne .L850
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ Initial code:
@   prog("subject(                                                    ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g100>>>
@   prog("  <store,                                                   ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g101>>>
@   prog("    <load,                                                  ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g102>>>
@   prog("      <plusa,                                               ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g103>>>
@   prog("        <global(a)>,                                        ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g104>>>
@   prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g105>>>
@   prog("    <local(20)>>                                            ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g106>>>
@   prog(") :- .                                                      ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g107>>>
@   prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g108>>>
@   prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g109>>>
@   prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g110>>>
@   prog("rule(""local"", addr, <local(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g111>>>
@   prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g112>>>
@   prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g113>>>
@   prog("rule(""scale"", addr,                                         ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g114>>>
@   prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g115>>>
@   prog("rule(""*global"", reg, <global(X)>) :- .                      ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g116>>>
@   prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g117>>>
@   prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g118>>>
@   prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g119>>>
@   prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g120>>>
@   prog("rule(""const"", rand, <const(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g121>>>
@   prog("rule(""reg"", rand, reg) :- .                                 ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g122>>>
@   prog("rule(""indir"", addr, reg) :- .                               ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g123>>>
@   prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g124>>>
@   prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g125>>>
@   prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g126>>>
@   prog("  use_rule(NT, Tree, Parse).                                ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g127>>>
@   prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g128>>>
@   prog("  matchall(PS, TS, Kids, Kids0).                            ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g129>>>
@   prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g130>>>
@   prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g131>>>
@   prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g132>>>
@   prog("cost(node(X, TS), C) :-                                     ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g133>>>
@   prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g134>>>
@   prog("allcosts(nil, 0) :- .                                       ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g135>>>
@   prog("allcosts(T:TS, C) :-                                        ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g136>>>
@   prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g137>>>
@   prog("opcost('*':_, 1) :- !.                                      ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g138>>>
@   prog("opcost(_, 0) :- .                                           ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g139>>>
@   prog("answer(P, C) :-                                             ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g140>>>
@   prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g141>>>
@   prog("min(N, P) :- min1(N, 0, P).                                 ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g142>>>
@   prog("min1(N, N, P) :- call(P), !.                                ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g143>>>
@   prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g144>>>
@   prog("# :- answer(P, C).                                          ");
@ <CALL 1, <GLOBAL _prog>, <STATLINK, <CONST 0>>, <ARG 0, <GLOBAL g145>>>
@   Initialize();
@ <CALL 0, <GLOBAL _Initialize>, <STATLINK, <CONST 0>>>
@   ReadFile()
@ <CALL 0, <GLOBAL _ReadFile>, <STATLINK, <CONST 0>>>
@ <LABEL L861>

@ After simplification:
@   prog("subject(                                                    ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g100>>>
@   prog("  <store,                                                   ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g101>>>
@   prog("    <load,                                                  ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g102>>>
@   prog("      <plusa,                                               ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g103>>>
@   prog("        <global(a)>,                                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g104>>>
@   prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g105>>>
@   prog("    <local(20)>>                                            ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g106>>>
@   prog(") :- .                                                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g107>>>
@   prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g108>>>
@   prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g109>>>
@   prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g110>>>
@   prog("rule(""local"", addr, <local(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g111>>>
@   prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g112>>>
@   prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g113>>>
@   prog("rule(""scale"", addr,                                         ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g114>>>
@   prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g115>>>
@   prog("rule(""*global"", reg, <global(X)>) :- .                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g116>>>
@   prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g117>>>
@   prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g118>>>
@   prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g119>>>
@   prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g120>>>
@   prog("rule(""const"", rand, <const(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g121>>>
@   prog("rule(""reg"", rand, reg) :- .                                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g122>>>
@   prog("rule(""indir"", addr, reg) :- .                               ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g123>>>
@   prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g124>>>
@   prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g125>>>
@   prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g126>>>
@   prog("  use_rule(NT, Tree, Parse).                                ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g127>>>
@   prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g128>>>
@   prog("  matchall(PS, TS, Kids, Kids0).                            ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g129>>>
@   prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g130>>>
@   prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g131>>>
@   prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g132>>>
@   prog("cost(node(X, TS), C) :-                                     ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g133>>>
@   prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g134>>>
@   prog("allcosts(nil, 0) :- .                                       ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g135>>>
@   prog("allcosts(T:TS, C) :-                                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g136>>>
@   prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g137>>>
@   prog("opcost('*':_, 1) :- !.                                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g138>>>
@   prog("opcost(_, 0) :- .                                           ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g139>>>
@   prog("answer(P, C) :-                                             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g140>>>
@   prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g141>>>
@   prog("min(N, P) :- min1(N, 0, P).                                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g142>>>
@   prog("min1(N, N, P) :- call(P), !.                                ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g143>>>
@   prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g144>>>
@   prog("# :- answer(P, C).                                          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g145>>>
@   Initialize();
@ <CALL 0, <GLOBAL _Initialize>>
@   ReadFile()
@ <CALL 0, <GLOBAL _ReadFile>>

@ After sharing:
@   prog("subject(                                                    ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g100>>>
@   prog("  <store,                                                   ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g101>>>
@   prog("    <load,                                                  ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g102>>>
@   prog("      <plusa,                                               ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g103>>>
@   prog("        <global(a)>,                                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g104>>>
@   prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g105>>>
@   prog("    <local(20)>>                                            ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g106>>>
@   prog(") :- .                                                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g107>>>
@   prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g108>>>
@   prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g109>>>
@   prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g110>>>
@   prog("rule(""local"", addr, <local(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g111>>>
@   prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g112>>>
@   prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g113>>>
@   prog("rule(""scale"", addr,                                         ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g114>>>
@   prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g115>>>
@   prog("rule(""*global"", reg, <global(X)>) :- .                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g116>>>
@   prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g117>>>
@   prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g118>>>
@   prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g119>>>
@   prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g120>>>
@   prog("rule(""const"", rand, <const(N)>) :- .                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g121>>>
@   prog("rule(""reg"", rand, reg) :- .                                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g122>>>
@   prog("rule(""indir"", addr, reg) :- .                               ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g123>>>
@   prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g124>>>
@   prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g125>>>
@   prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g126>>>
@   prog("  use_rule(NT, Tree, Parse).                                ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g127>>>
@   prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g128>>>
@   prog("  matchall(PS, TS, Kids, Kids0).                            ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g129>>>
@   prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g130>>>
@   prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g131>>>
@   prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g132>>>
@   prog("cost(node(X, TS), C) :-                                     ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g133>>>
@   prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g134>>>
@   prog("allcosts(nil, 0) :- .                                       ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g135>>>
@   prog("allcosts(T:TS, C) :-                                        ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g136>>>
@   prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g137>>>
@   prog("opcost('*':_, 1) :- !.                                      ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g138>>>
@   prog("opcost(_, 0) :- .                                           ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g139>>>
@   prog("answer(P, C) :-                                             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g140>>>
@   prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g141>>>
@   prog("min(N, P) :- min1(N, 0, P).                                 ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g142>>>
@   prog("min1(N, N, P) :- call(P), !.                                ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g143>>>
@   prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g144>>>
@   prog("# :- answer(P, C).                                          ");
@ <CALL 1, <GLOBAL _prog>, <ARG 0, <GLOBAL g145>>>
@   Initialize();
@ <CALL 0, <GLOBAL _Initialize>>
@   ReadFile()
@ <CALL 0, <GLOBAL _ReadFile>>

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   prog("subject(                                                    ");
@ <ARG 0, <GLOBAL g100>>
	set r0, g100
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  <store,                                                   ");
@ <ARG 0, <GLOBAL g101>>
	set r0, g101
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("    <load,                                                  ");
@ <ARG 0, <GLOBAL g102>>
	set r0, g102
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("      <plusa,                                               ");
@ <ARG 0, <GLOBAL g103>>
	set r0, g103
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("        <global(a)>,                                        ");
@ <ARG 0, <GLOBAL g104>>
	set r0, g104
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("        <lsl, <load, <local(16)>>, <const(2)>>>>,           ");
@ <ARG 0, <GLOBAL g105>>
	set r0, g105
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("    <local(20)>>                                            ");
@ <ARG 0, <GLOBAL g106>>
	set r0, g106
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog(") :- .                                                      ");
@ <ARG 0, <GLOBAL g107>>
	set r0, g107
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*str"", stmt, <store, reg, addr>) :- .                 ");
@ <ARG 0, <GLOBAL g108>>
	set r0, g108
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*ldr"", reg,  <load, addr>) :- .                       ");
@ <ARG 0, <GLOBAL g109>>
	set r0, g109
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*addfp"", reg, <local(N)>) :- .                        ");
@ <ARG 0, <GLOBAL g110>>
	set r0, g110
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""local"", addr, <local(N)>) :- .                        ");
@ <ARG 0, <GLOBAL g111>>
	set r0, g111
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*add"", reg, <plusa, reg, rand>) :- .                  ");
@ <ARG 0, <GLOBAL g112>>
	set r0, g112
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""index"", addr, <plusa, reg, reg>) :- .                 ");
@ <ARG 0, <GLOBAL g113>>
	set r0, g113
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""scale"", addr,                                         ");
@ <ARG 0, <GLOBAL g114>>
	set r0, g114
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("       <plusa, reg, <lsl, reg, <const(N)>>>) :- .           ");
@ <ARG 0, <GLOBAL g115>>
	set r0, g115
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*global"", reg, <global(X)>) :- .                      ");
@ <ARG 0, <GLOBAL g116>>
	set r0, g116
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*lsl"", reg, <lsl, reg, rand>) :- .                    ");
@ <ARG 0, <GLOBAL g117>>
	set r0, g117
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""lshiftc"", rand, <lsl, reg, <const(N)>>) :- .          ");
@ <ARG 0, <GLOBAL g118>>
	set r0, g118
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""lshiftr"", rand, <lsl, reg, reg>) :- .                 ");
@ <ARG 0, <GLOBAL g119>>
	set r0, g119
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""*mov"", reg, <const(N)>) :- .                          ");
@ <ARG 0, <GLOBAL g120>>
	set r0, g120
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""const"", rand, <const(N)>) :- .                        ");
@ <ARG 0, <GLOBAL g121>>
	set r0, g121
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""reg"", rand, reg) :- .                                 ");
@ <ARG 0, <GLOBAL g122>>
	set r0, g122
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("rule(""indir"", addr, reg) :- .                               ");
@ <ARG 0, <GLOBAL g123>>
	set r0, g123
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("use_rule(NT, Tree, node(Name, Kids)) :-                     ");
@ <ARG 0, <GLOBAL g124>>
	set r0, g124
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).         ");
@ <ARG 0, <GLOBAL g125>>
	set r0, g125
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("match(NT, Tree, Parse:Kids0, Kids0) :-                      ");
@ <ARG 0, <GLOBAL g126>>
	set r0, g126
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  use_rule(NT, Tree, Parse).                                ");
@ <ARG 0, <GLOBAL g127>>
	set r0, g127
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("match(node(W, PS), node(W, TS), Kids, Kids0) :-             ");
@ <ARG 0, <GLOBAL g128>>
	set r0, g128
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  matchall(PS, TS, Kids, Kids0).                            ");
@ <ARG 0, <GLOBAL g129>>
	set r0, g129
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("matchall(nil, nil, Kids0, Kids0) :- .                       ");
@ <ARG 0, <GLOBAL g130>>
	set r0, g130
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("matchall(P:PS, T:TS, Kids, Kids0) :-                        ");
@ <ARG 0, <GLOBAL g131>>
	set r0, g131
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0). ");
@ <ARG 0, <GLOBAL g132>>
	set r0, g132
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("cost(node(X, TS), C) :-                                     ");
@ <ARG 0, <GLOBAL g133>>
	set r0, g133
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  opcost(X, A), allcosts(TS, B), plus(A, B, C).             ");
@ <ARG 0, <GLOBAL g134>>
	set r0, g134
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("allcosts(nil, 0) :- .                                       ");
@ <ARG 0, <GLOBAL g135>>
	set r0, g135
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("allcosts(T:TS, C) :-                                        ");
@ <ARG 0, <GLOBAL g136>>
	set r0, g136
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  cost(T, A), allcosts(TS, B), plus(A, B, C).               ");
@ <ARG 0, <GLOBAL g137>>
	set r0, g137
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("opcost('*':_, 1) :- !.                                      ");
@ <ARG 0, <GLOBAL g138>>
	set r0, g138
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("opcost(_, 0) :- .                                           ");
@ <ARG 0, <GLOBAL g139>>
	set r0, g139
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("answer(P, C) :-                                             ");
@ <ARG 0, <GLOBAL g140>>
	set r0, g140
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("  subject(T), use_rule(stmt, T, P), cost(P, C).             ");
@ <ARG 0, <GLOBAL g141>>
	set r0, g141
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("min(N, P) :- min1(N, 0, P).                                 ");
@ <ARG 0, <GLOBAL g142>>
	set r0, g142
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("min1(N, N, P) :- call(P), !.                                ");
@ <ARG 0, <GLOBAL g143>>
	set r0, g143
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).          ");
@ <ARG 0, <GLOBAL g144>>
	set r0, g144
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   prog("# :- answer(P, C).                                          ");
@ <ARG 0, <GLOBAL g145>>
	set r0, g145
@ <CALL 1, <GLOBAL _prog>>
	bl _prog
@   Initialize();
@ <CALL 0, <GLOBAL _Initialize>>
	bl _Initialize
@   ReadFile()
@ <CALL 0, <GLOBAL _ReadFile>>
	bl _ReadFile
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

	.comm _run, 1, 4
	.comm _dflag, 1, 4
	.comm _charptr, 4, 4
	.comm _charbuf, 2048, 4
	.comm _lsp, 4, 4
	.comm _gsp, 4, 4
	.comm _hp, 4, 4
	.comm _hmark, 4, 4
	.comm _mem, 100004, 4
	.comm _infile, 3000, 4
	.comm _pin, 4, 4
	.comm _pout, 4, 4
	.comm _pbchar, 1, 4
	.comm _lineno, 4, 4
	.comm _current, 4, 4
	.comm _call, 4, 4
	.comm _goalframe, 4, 4
	.comm _choice, 4, 4
	.comm _base, 4, 4
	.comm _prok, 4, 4
	.comm _nsymbols, 4, 4
	.comm _symtab, 8192, 4
	.comm _cons, 4, 4
	.comm _eqsym, 4, 4
	.comm _cutsym, 4, 4
	.comm _nilsym, 4, 4
	.comm _notsym, 4, 4
	.comm _node, 4, 4
	.comm _refnode, 256, 4
	.comm _token, 4, 4
	.comm _tokval, 4, 4
	.comm _tokival, 4, 4
	.comm _toksval, 128, 4
	.comm _errflag, 1, 4
	.comm _errcount, 4, 4
	.comm _nvars, 4, 4
	.comm _vartable, 256, 4
	.comm _trhead, 4, 4
	.comm _ok, 1, 4
	.comm _av, 256, 4
	.comm _callbody, 4, 4
	.data
g1:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g2:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 116, 114
	.byte 105, 110, 103, 32, 115, 112, 97, 99, 101
	.byte 0
g3:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g4:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 116, 97
	.byte 99, 107, 32, 115, 112, 97, 99, 101
	.byte 0
g5:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g6:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 116, 97
	.byte 99, 107, 32, 115, 112, 97, 99, 101
	.byte 0
g7:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g8:
	.byte 111, 117, 116, 32, 111, 102, 32, 104, 101, 97
	.byte 112, 32, 115, 112, 97, 99, 101
	.byte 0
g9:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g10:
	.byte 68, 101, 114, 101, 102
	.byte 0
g11:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g12:
	.byte 111, 117, 116, 32, 111, 102, 32, 115, 121, 109
	.byte 98, 111, 108, 32, 115, 112, 97, 99, 101
	.byte 0
g13:
	.byte 58, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g14:
	.byte 33, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g15:
	.byte 61, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g16:
	.byte 110, 105, 108, 32, 32, 32, 32, 32
	.byte 0
g17:
	.byte 110, 111, 116, 32, 32, 32, 32, 32
	.byte 0
g18:
	.byte 110, 111, 100, 101, 32, 32, 32, 32
	.byte 0
g19:
	.byte 99, 97, 108, 108, 32, 32, 32, 32
	.byte 0
g20:
	.byte 112, 108, 117, 115, 32, 32, 32, 32
	.byte 0
g21:
	.byte 116, 105, 109, 101, 115, 32, 32, 32
	.byte 0
g22:
	.byte 105, 110, 116, 101, 103, 101, 114, 32
	.byte 0
g23:
	.byte 99, 104, 97, 114, 32, 32, 32, 32
	.byte 0
g24:
	.byte 102, 97, 108, 115, 101, 32, 32, 32
	.byte 0
g25:
	.byte 112, 114, 105, 110, 116, 32, 32, 32
	.byte 0
g26:
	.byte 110, 108, 32, 32, 32, 32, 32, 32
	.byte 0
g27:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g28:
	.byte 99, 97, 110, 110, 111, 116, 32, 97, 100, 100
	.byte 32, 99, 108, 97, 117, 115, 101, 115, 32, 116
	.byte 111, 32, 98, 117, 105, 108, 116, 45, 105, 110
	.byte 32, 114, 101, 108, 97, 116, 105, 111, 110, 32
	.byte 0
g29:
	.byte 32, 61, 32
	.byte 0
g30:
	.byte 110, 111, 116, 32
	.byte 0
g31:
	.byte 44, 32
	.byte 0
g32:
	.byte 44, 32
	.byte 0
g33:
	.byte 42, 110, 117, 108, 108, 45, 116, 101, 114, 109
	.byte 42
	.byte 0
g34:
	.byte 42, 117, 110, 107, 110, 111, 119, 110, 45, 116
	.byte 101, 114, 109, 40, 116, 97, 103, 61
	.byte 0
g35:
	.byte 41, 42
	.byte 0
g36:
	.byte 42, 110, 117, 108, 108, 45, 99, 108, 97, 117
	.byte 115, 101, 42
	.byte 0
g37:
	.byte 58, 45, 32
	.byte 0
g38:
	.byte 44, 32
	.byte 0
g39:
	.byte 76, 105, 110, 101, 32
	.byte 0
g40:
	.byte 83, 121, 110, 116, 97, 120, 32, 101, 114, 114
	.byte 111, 114, 32, 45, 32
	.byte 0
g41:
	.byte 84, 111, 111, 32, 109, 97, 110, 121, 32, 101
	.byte 114, 114, 111, 114, 115, 58, 32, 73, 32, 97
	.byte 109, 32, 103, 105, 118, 105, 110, 103, 32, 117
	.byte 112
	.byte 0
g42:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g43:
	.byte 105, 100, 101, 110, 116, 105, 102, 105, 101, 114
	.byte 32, 116, 111, 111, 32, 108, 111, 110, 103
	.byte 0
g44:
	.byte 98, 97, 100, 32, 116, 111, 107, 101, 110, 32
	.byte 47
	.byte 0
g45:
	.byte 101, 110, 100, 32, 111, 102, 32, 102, 105, 108
	.byte 101, 32, 105, 110, 32, 99, 111, 109, 109, 101
	.byte 110, 116
	.byte 0
g46:
	.byte 109, 105, 115, 115, 105, 110, 103, 32, 113, 117
	.byte 111, 116, 101
	.byte 0
g47:
	.byte 117, 110, 116, 101, 114, 109, 105, 110, 97, 116
	.byte 101, 100, 32, 115, 116, 114, 105, 110, 103
	.byte 0
g48:
	.byte 105, 108, 108, 101, 103, 97, 108, 32, 99, 104
	.byte 97, 114, 97, 99, 116, 101, 114
	.byte 0
g49:
	.byte 105, 100, 101, 110, 116, 105, 102, 105, 101, 114
	.byte 32
	.byte 0
g50:
	.byte 118, 97, 114, 105, 97, 98, 108, 101, 32
	.byte 0
g51:
	.byte 110, 117, 109, 98, 101, 114
	.byte 0
g52:
	.byte 99, 104, 97, 114, 32, 99, 111, 110, 115, 116
	.byte 97, 110, 116
	.byte 0
g53:
	.byte 58, 45
	.byte 0
g54:
	.byte 40
	.byte 0
g55:
	.byte 41
	.byte 0
g56:
	.byte 44
	.byte 0
g57:
	.byte 46
	.byte 0
g58:
	.byte 58
	.byte 0
g59:
	.byte 61
	.byte 0
g60:
	.byte 115, 116, 114, 105, 110, 103, 32, 99, 111, 110
	.byte 115, 116, 97, 110, 116
	.byte 0
g61:
	.byte 60
	.byte 0
g62:
	.byte 62
	.byte 0
g63:
	.byte 35
	.byte 0
g64:
	.byte 117, 110, 107, 110, 111, 119, 110, 32, 116, 111
	.byte 107, 101, 110
	.byte 0
g65:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g66:
	.byte 116, 111, 111, 32, 109, 97, 110, 121, 32, 118
	.byte 97, 114, 105, 97, 98, 108, 101, 115
	.byte 0
g67:
	.byte 121, 101, 115
	.byte 0
g68:
	.byte 32, 61, 32
	.byte 0
g69:
	.byte 101, 120, 112, 101, 99, 116, 101, 100, 32
	.byte 0
g70:
	.byte 44, 32, 102, 111, 117, 110, 100, 32
	.byte 0
g71:
	.byte 119, 114, 111, 110, 103, 32, 110, 117, 109, 98
	.byte 101, 114, 32, 111, 102, 32, 97, 114, 103, 115
	.byte 0
g72:
	.byte 101, 120, 112, 101, 99, 116, 101, 100, 32, 97
	.byte 32, 116, 101, 114, 109
	.byte 0
g73:
	.byte 108, 105, 116, 101, 114, 97, 108, 32, 109, 117
	.byte 115, 116, 32, 98, 101, 32, 97, 32, 99, 111
	.byte 109, 112, 111, 117, 110, 100, 32, 116, 101, 114
	.byte 109
	.byte 0
g74:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g75:
	.byte 98, 97, 100, 32, 116, 97, 103
	.byte 0
g76:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g77:
	.byte 75, 101, 121
	.byte 0
g78:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g79:
	.byte 98, 97, 100, 32, 116, 97, 103
	.byte 0
g80:
	.byte 40, 84, 82, 79, 41
	.byte 0
g81:
	.byte 69, 120, 105, 116
	.byte 0
g82:
	.byte 58, 32
	.byte 0
g83:
	.byte 82, 101, 100, 111
	.byte 0
g84:
	.byte 58, 32
	.byte 0
g85:
	.byte 67, 97, 108, 108
	.byte 0
g86:
	.byte 58, 32
	.byte 0
g87:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g88:
	.byte 99, 97, 108, 108, 32, 116, 111, 32, 117, 110
	.byte 100, 101, 102, 105, 110, 101, 100, 32, 114, 101
	.byte 108, 97, 116, 105, 111, 110, 32
	.byte 0
g89:
	.byte 110, 111
	.byte 0
g90:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g91:
	.byte 98, 97, 100, 32, 97, 114, 103, 117, 109, 101
	.byte 110, 116, 32, 116, 111, 32, 99, 97, 108, 108
	.byte 47, 49
	.byte 0
g92:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g93:
	.byte 98, 97, 100, 32, 97, 114, 103, 117, 109, 101
	.byte 110, 116, 32, 116, 111, 32, 99, 97, 108, 108
	.byte 47, 49
	.byte 0
g94:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g95:
	.byte 112, 108, 117, 115, 47, 51, 32, 110, 101, 101
	.byte 100, 115, 32, 97, 116, 32, 108, 101, 97, 115
	.byte 116, 32, 116, 119, 111, 32, 105, 110, 116, 101
	.byte 103, 101, 114, 115
	.byte 0
g96:
	.byte 69, 114, 114, 111, 114, 58, 32
	.byte 0
g97:
	.byte 116, 105, 109, 101, 115, 47, 51, 32, 110, 101
	.byte 101, 100, 115, 32, 97, 116, 32, 108, 101, 97
	.byte 115, 116, 32, 116, 119, 111, 32, 105, 110, 116
	.byte 101, 103, 101, 114, 115
	.byte 0
g98:
	.byte 80, 97, 110, 105, 99, 58, 32
	.byte 0
g99:
	.byte 98, 97, 100, 32, 116, 97, 103
	.byte 0
g100:
	.byte 115, 117, 98, 106, 101, 99, 116, 40, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g101:
	.byte 32, 32, 60, 115, 116, 111, 114, 101, 44, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g102:
	.byte 32, 32, 32, 32, 60, 108, 111, 97, 100, 44
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g103:
	.byte 32, 32, 32, 32, 32, 32, 60, 112, 108, 117
	.byte 115, 97, 44, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g104:
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 60, 103
	.byte 108, 111, 98, 97, 108, 40, 97, 41, 62, 44
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g105:
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 60, 108
	.byte 115, 108, 44, 32, 60, 108, 111, 97, 100, 44
	.byte 32, 60, 108, 111, 99, 97, 108, 40, 49, 54
	.byte 41, 62, 62, 44, 32, 60, 99, 111, 110, 115
	.byte 116, 40, 50, 41, 62, 62, 62, 62, 44, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g106:
	.byte 32, 32, 32, 32, 60, 108, 111, 99, 97, 108
	.byte 40, 50, 48, 41, 62, 62, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g107:
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g108:
	.byte 114, 117, 108, 101, 40, 34, 42, 115, 116, 114
	.byte 34, 44, 32, 115, 116, 109, 116, 44, 32, 60
	.byte 115, 116, 111, 114, 101, 44, 32, 114, 101, 103
	.byte 44, 32, 97, 100, 100, 114, 62, 41, 32, 58
	.byte 45, 32, 46, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g109:
	.byte 114, 117, 108, 101, 40, 34, 42, 108, 100, 114
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 32, 60
	.byte 108, 111, 97, 100, 44, 32, 97, 100, 100, 114
	.byte 62, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g110:
	.byte 114, 117, 108, 101, 40, 34, 42, 97, 100, 100
	.byte 102, 112, 34, 44, 32, 114, 101, 103, 44, 32
	.byte 60, 108, 111, 99, 97, 108, 40, 78, 41, 62
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g111:
	.byte 114, 117, 108, 101, 40, 34, 108, 111, 99, 97
	.byte 108, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 60, 108, 111, 99, 97, 108, 40, 78, 41, 62
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g112:
	.byte 114, 117, 108, 101, 40, 34, 42, 97, 100, 100
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 60, 112
	.byte 108, 117, 115, 97, 44, 32, 114, 101, 103, 44
	.byte 32, 114, 97, 110, 100, 62, 41, 32, 58, 45
	.byte 32, 46, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g113:
	.byte 114, 117, 108, 101, 40, 34, 105, 110, 100, 101
	.byte 120, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 60, 112, 108, 117, 115, 97, 44, 32, 114, 101
	.byte 103, 44, 32, 114, 101, 103, 62, 41, 32, 58
	.byte 45, 32, 46, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g114:
	.byte 114, 117, 108, 101, 40, 34, 115, 99, 97, 108
	.byte 101, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g115:
	.byte 32, 32, 32, 32, 32, 32, 32, 60, 112, 108
	.byte 117, 115, 97, 44, 32, 114, 101, 103, 44, 32
	.byte 60, 108, 115, 108, 44, 32, 114, 101, 103, 44
	.byte 32, 60, 99, 111, 110, 115, 116, 40, 78, 41
	.byte 62, 62, 62, 41, 32, 58, 45, 32, 46, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g116:
	.byte 114, 117, 108, 101, 40, 34, 42, 103, 108, 111
	.byte 98, 97, 108, 34, 44, 32, 114, 101, 103, 44
	.byte 32, 60, 103, 108, 111, 98, 97, 108, 40, 88
	.byte 41, 62, 41, 32, 58, 45, 32, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g117:
	.byte 114, 117, 108, 101, 40, 34, 42, 108, 115, 108
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 60, 108
	.byte 115, 108, 44, 32, 114, 101, 103, 44, 32, 114
	.byte 97, 110, 100, 62, 41, 32, 58, 45, 32, 46
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g118:
	.byte 114, 117, 108, 101, 40, 34, 108, 115, 104, 105
	.byte 102, 116, 99, 34, 44, 32, 114, 97, 110, 100
	.byte 44, 32, 60, 108, 115, 108, 44, 32, 114, 101
	.byte 103, 44, 32, 60, 99, 111, 110, 115, 116, 40
	.byte 78, 41, 62, 62, 41, 32, 58, 45, 32, 46
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g119:
	.byte 114, 117, 108, 101, 40, 34, 108, 115, 104, 105
	.byte 102, 116, 114, 34, 44, 32, 114, 97, 110, 100
	.byte 44, 32, 60, 108, 115, 108, 44, 32, 114, 101
	.byte 103, 44, 32, 114, 101, 103, 62, 41, 32, 58
	.byte 45, 32, 46, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g120:
	.byte 114, 117, 108, 101, 40, 34, 42, 109, 111, 118
	.byte 34, 44, 32, 114, 101, 103, 44, 32, 60, 99
	.byte 111, 110, 115, 116, 40, 78, 41, 62, 41, 32
	.byte 58, 45, 32, 46, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g121:
	.byte 114, 117, 108, 101, 40, 34, 99, 111, 110, 115
	.byte 116, 34, 44, 32, 114, 97, 110, 100, 44, 32
	.byte 60, 99, 111, 110, 115, 116, 40, 78, 41, 62
	.byte 41, 32, 58, 45, 32, 46, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g122:
	.byte 114, 117, 108, 101, 40, 34, 114, 101, 103, 34
	.byte 44, 32, 114, 97, 110, 100, 44, 32, 114, 101
	.byte 103, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g123:
	.byte 114, 117, 108, 101, 40, 34, 105, 110, 100, 105
	.byte 114, 34, 44, 32, 97, 100, 100, 114, 44, 32
	.byte 114, 101, 103, 41, 32, 58, 45, 32, 46, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g124:
	.byte 117, 115, 101, 95, 114, 117, 108, 101, 40, 78
	.byte 84, 44, 32, 84, 114, 101, 101, 44, 32, 110
	.byte 111, 100, 101, 40, 78, 97, 109, 101, 44, 32
	.byte 75, 105, 100, 115, 41, 41, 32, 58, 45, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g125:
	.byte 32, 32, 114, 117, 108, 101, 40, 78, 97, 109
	.byte 101, 44, 32, 78, 84, 44, 32, 82, 72, 83
	.byte 41, 44, 32, 109, 97, 116, 99, 104, 40, 82
	.byte 72, 83, 44, 32, 84, 114, 101, 101, 44, 32
	.byte 75, 105, 100, 115, 44, 32, 110, 105, 108, 41
	.byte 46, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g126:
	.byte 109, 97, 116, 99, 104, 40, 78, 84, 44, 32
	.byte 84, 114, 101, 101, 44, 32, 80, 97, 114, 115
	.byte 101, 58, 75, 105, 100, 115, 48, 44, 32, 75
	.byte 105, 100, 115, 48, 41, 32, 58, 45, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g127:
	.byte 32, 32, 117, 115, 101, 95, 114, 117, 108, 101
	.byte 40, 78, 84, 44, 32, 84, 114, 101, 101, 44
	.byte 32, 80, 97, 114, 115, 101, 41, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g128:
	.byte 109, 97, 116, 99, 104, 40, 110, 111, 100, 101
	.byte 40, 87, 44, 32, 80, 83, 41, 44, 32, 110
	.byte 111, 100, 101, 40, 87, 44, 32, 84, 83, 41
	.byte 44, 32, 75, 105, 100, 115, 44, 32, 75, 105
	.byte 100, 115, 48, 41, 32, 58, 45, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g129:
	.byte 32, 32, 109, 97, 116, 99, 104, 97, 108, 108
	.byte 40, 80, 83, 44, 32, 84, 83, 44, 32, 75
	.byte 105, 100, 115, 44, 32, 75, 105, 100, 115, 48
	.byte 41, 46, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g130:
	.byte 109, 97, 116, 99, 104, 97, 108, 108, 40, 110
	.byte 105, 108, 44, 32, 110, 105, 108, 44, 32, 75
	.byte 105, 100, 115, 48, 44, 32, 75, 105, 100, 115
	.byte 48, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g131:
	.byte 109, 97, 116, 99, 104, 97, 108, 108, 40, 80
	.byte 58, 80, 83, 44, 32, 84, 58, 84, 83, 44
	.byte 32, 75, 105, 100, 115, 44, 32, 75, 105, 100
	.byte 115, 48, 41, 32, 58, 45, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g132:
	.byte 32, 32, 109, 97, 116, 99, 104, 40, 80, 44
	.byte 32, 84, 44, 32, 75, 105, 100, 115, 44, 32
	.byte 75, 105, 100, 115, 49, 41, 44, 32, 109, 97
	.byte 116, 99, 104, 97, 108, 108, 40, 80, 83, 44
	.byte 32, 84, 83, 44, 32, 75, 105, 100, 115, 49
	.byte 44, 32, 75, 105, 100, 115, 48, 41, 46, 32
	.byte 0
g133:
	.byte 99, 111, 115, 116, 40, 110, 111, 100, 101, 40
	.byte 88, 44, 32, 84, 83, 41, 44, 32, 67, 41
	.byte 32, 58, 45, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g134:
	.byte 32, 32, 111, 112, 99, 111, 115, 116, 40, 88
	.byte 44, 32, 65, 41, 44, 32, 97, 108, 108, 99
	.byte 111, 115, 116, 115, 40, 84, 83, 44, 32, 66
	.byte 41, 44, 32, 112, 108, 117, 115, 40, 65, 44
	.byte 32, 66, 44, 32, 67, 41, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g135:
	.byte 97, 108, 108, 99, 111, 115, 116, 115, 40, 110
	.byte 105, 108, 44, 32, 48, 41, 32, 58, 45, 32
	.byte 46, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g136:
	.byte 97, 108, 108, 99, 111, 115, 116, 115, 40, 84
	.byte 58, 84, 83, 44, 32, 67, 41, 32, 58, 45
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g137:
	.byte 32, 32, 99, 111, 115, 116, 40, 84, 44, 32
	.byte 65, 41, 44, 32, 97, 108, 108, 99, 111, 115
	.byte 116, 115, 40, 84, 83, 44, 32, 66, 41, 44
	.byte 32, 112, 108, 117, 115, 40, 65, 44, 32, 66
	.byte 44, 32, 67, 41, 46, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g138:
	.byte 111, 112, 99, 111, 115, 116, 40, 39, 42, 39
	.byte 58, 95, 44, 32, 49, 41, 32, 58, 45, 32
	.byte 33, 46, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g139:
	.byte 111, 112, 99, 111, 115, 116, 40, 95, 44, 32
	.byte 48, 41, 32, 58, 45, 32, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g140:
	.byte 97, 110, 115, 119, 101, 114, 40, 80, 44, 32
	.byte 67, 41, 32, 58, 45, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g141:
	.byte 32, 32, 115, 117, 98, 106, 101, 99, 116, 40
	.byte 84, 41, 44, 32, 117, 115, 101, 95, 114, 117
	.byte 108, 101, 40, 115, 116, 109, 116, 44, 32, 84
	.byte 44, 32, 80, 41, 44, 32, 99, 111, 115, 116
	.byte 40, 80, 44, 32, 67, 41, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g142:
	.byte 109, 105, 110, 40, 78, 44, 32, 80, 41, 32
	.byte 58, 45, 32, 109, 105, 110, 49, 40, 78, 44
	.byte 32, 48, 44, 32, 80, 41, 46, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g143:
	.byte 109, 105, 110, 49, 40, 78, 44, 32, 78, 44
	.byte 32, 80, 41, 32, 58, 45, 32, 99, 97, 108
	.byte 108, 40, 80, 41, 44, 32, 33, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g144:
	.byte 109, 105, 110, 49, 40, 78, 44, 32, 78, 48
	.byte 44, 32, 80, 41, 32, 58, 45, 32, 112, 108
	.byte 117, 115, 40, 78, 48, 44, 32, 49, 44, 32
	.byte 78, 49, 41, 44, 32, 109, 105, 110, 49, 40
	.byte 78, 44, 32, 78, 49, 44, 32, 80, 41, 46
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
g145:
	.byte 35, 32, 58, 45, 32, 97, 110, 115, 119, 101
	.byte 114, 40, 80, 44, 32, 67, 41, 46, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32
	.byte 0
@ End
