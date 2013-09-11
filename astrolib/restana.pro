Pro RestAna,Var,File
;Restores ANA Store files FTPed from Vax to Sun.
; Var (output variable of unknown size/type) restored variable.
; File (input string) name of the file FTPed from Vax that contains ANA
;   Stored variable.
;01-Jul-91 JAV  Create.

If N_Params() ne 2 Then Begin
  Print,'Syntax: RestAna,Var,File'
  RetAll
EndIf

;Define general header structure.
  If (Not KeyWord_Set(G)) Then Begin		;true: first call, define G
    G = { $
      GeneralHeader, $
      HdrExt: Fix(-1), $
      EleSiz: Fix(-1), $
      NumTyp: Byte(-1), $
      ArrFlg: Byte(-1), $
      Unk6:   Byte(-1), $
      Unk7:   Byte(-1), $
      Unk8:   Byte(-1), $
      Unk9:   Byte(-1) $
    }
  EndIf

  OpenR,Unit,File,/Get_Lun		;open FTPed file for read-only
  AssVar = Assoc(Unit,G)		;associate varible with disk file
  G = AssVar(0)				;read header from disk file
  FPtr = Long(10)			;pointer to last byte read from file


;Reread general header with 2 byte offset, if the variable length record
;  length is still embedded in file.
  BytSkp = 0				;bytes to skip before each block
  Dummy = G.HdrExt			;prepare to pass by value (pg. 8-5)
  ByteOrder,Dummy,/SSwap		;convert to Sun I*2 format
  If (Min(Abs(Dummy - [10,26,30,34,38,42,46,50,54,58])) eq 0) Then Begin
    BytSkp = 2				;bytes to skip before each block
    AssVar = Assoc(Unit,G,2)		;skip embedded record length
    G = AssVar(0)			;read properly aligned header
    FPtr = FPtr + BytSkp		;pointer to last byte read from file
  EndIf

;Swap bytes as necessary.
  Dummy = G.HdrExt			;prepare to pass by value (pg. 8-5)
  ByteOrder,Dummy,/SSwap		;convert to Sun I*2 format
  G.HdrExt = Dummy			;put datum back into structure
  Dummy = G.Elesiz			;prepare to pass by value (pg. 8-5)
  ByteOrder,Dummy,/SSwap		;convert to Sun I*2 format
  G.Elesiz = Dummy			;put datum back into structure

;Check whether we really have an ANA store file.
  If (G.HdrExt ne 3) or $
    (Min(Abs(G.EleSiz - [1,2,4,8])) ne 0) or $
    (Min(Abs(G.NumTyp - [6,7,8,10,11])) ne 0) or $
    ((G.ArrFlg ne 1) and (G.ArrFlg ne 4)) or $
    ((G.EleSiz eq 1) and (G.NumTyp ne 6)) or $
    ((G.EleSiz eq 2) and (G.NumTyp ne 7)) or $
    ((G.EleSiz eq 4) and (G.NumTyp ne 8) and (G.NumTyp ne 10)) or $
    ((G.EleSiz eq 8) and (G.NumTyp ne 11)) Then Begin
    Message,'File does not have ANA store format.'
    RetAll
  EndIf

;Read array information block, if present.
  If G.ArrFlg eq 4 Then Begin		;true: restoring an array
    If (Not KeyWord_Set(A)) Then Begin	;true: first array read, define A
      A = { $
        ArrayHeader, $
        Unk0:   Byte(-1), $
        Unk1:   Byte(-1), $
        Unk2:   Byte(-1), $
        NumDim: Byte(-1), $
        ArrSiz: Long(-1), $
        Unk7:   Byte(-1), $
        Unk8:   Byte(-1), $
        Unk9:   Byte(-1), $
        Unk10:  Byte(-1) $
      }
    EndIf

    AssVar = Assoc(Unit,A,FPtr)		;associate varible with disk file
    A = AssVar(0)			;read more header from disk file
    FPtr = FPtr + 12			;update pointer in file

;Byte swap as necessary.
    BytCnt = A.ArrSiz			;prepare to pass by value (pg. 8-5)
    ByteOrder,BytCnt,/LSwap		;convert to Sun I*2 format
    A.ArrSiz = BytCnt			;put datum back into structure

;Read number of elements in each dimension.
    EleDim = LonArr(A.NumDim)		;initialize dimension array
    AssVar = Assoc(Unit,EleDim,FPtr)	;prepare to read unread header
    EleDim = AssVar(0)			;read dimension sizes
    FPtr = FPtr + 4 * A.NumDim		;update file pointer
    ByteOrder,EleDim,/LSwap		;swap bytes 1234->4321

;Verify size of array agrees with dimensions
    BytChk = Long(1)			;init total data size variable
    For i=0,A.NumDim-1 Do Begin		;loop thru dimensions
      BytChk = BytChk * EleDim(i)	;multiply each dimension size
    EndFor				;to give total number of elements
    BytChk = BytChk * G.EleSiz		;multiply by bytes per element
    If (BytChk ne A.ArrSiz) Then Begin	;true: conflict in data size
      Message,'File does not have ANA store format.'
    EndIf

  EndIf Else BytCnt = G.EleSiz		;bytes of data in scalar

;Now read data into byte buffer. Will change data type later.
  Var = BytArr(BytCnt)			;init return variable
  VPtr = Long(0)			;init variable pointer
  While (BytCnt-VPtr gt 2042) Do Begin	;true: must read "full" block
    FPtr = FPtr + BytSkp		;skip embedded record length
    Buffer = BytArr(/NoZero,2044)	;init full block buffer
    AssVar = Assoc(Unit,Buffer,FPtr)	;associate variable with file
    Buffer = AssVar(0)			;read block from file

;Verify proper block extension flag (0=middle of many, 1=first of many).
    BlkXtn = Fix(Buffer(0:1),0)		;extract block extension flag
    ByteOrder,/SSwap,BlkXtn		;swap bytes 12->21
    If ((VPtr eq 0) and (BlkXtn ne 1)) or $
      ((VPtr gt 0) and (BlkXtn ne 0)) Then Begin
      Message,'Unknown data block structure.'
    EndIf

;Store data into return varible and loop back for more.
    Var(VPtr) = Buffer(2:*)		;insert data into variable
    VPtr = VPtr + 2042			;update variable pointer
    FPtr = FPtr + 2044			;update file pointer
  EndWhile

;Now read last partial or single block. Can't use structures since the
;  buffer field changes on each call, whereas structures may not change.
  FPtr = FPtr + BytSkp			;skip embedded record length
  Buffer = BytArr(/NoZero,BytCnt-VPtr+2)  ;init partial block "structure"
  AssVar = Assoc(Unit,Buffer,FPtr)	;associate variable with file
  Buffer = AssVar(0)			;read block from file

;Verify proper block extension flag (2=last of many, 3=single).
  BlkXtn = Fix(Buffer(0:1),0)		;extract block extension flag
  ByteOrder,/SSwap,BlkXtn		;swap bytes 12->21
  If ((VPtr le 2042) and (BlkXtn ne 3)) or $
    ((VPtr gt 2042) and (BlkXtn ne 2)) Then Begin
    Message,'Unknown data block structure.'
  EndIf

;Store data into return varible and loop back for more.
  Var(VPtr) = Buffer(2:*)		;insert data into variable

;Now swap the order of the bytes, as necessary.
  If (G.EleSiz eq 2) Then Begin		;two byte data
    ByteOrder,/SSwap,Var		;swap 12->21
    Var = Fix(Var,0,BytCnt/2)		;convert to I*2
  EndIf 
  If (G.EleSiz eq 4) Then Begin		;four byte data
    If (G.NumTyp eq 8) Then Begin	;true: restoring I*4
      ByteOrder,/LSwap,Var		;swap 1234->4321
      Var = Long(Var,0,BytCnt/4)	;convert to I*4
    EndIf Else Begin			;fall thru: restoring F*4
      ByteOrder,/SSwap,Var		;swap 1234->2143
      Var = Float(Var,0,BytCnt/4)	;convert to F*4
      ErrCode = Check_Math(0,1)		;inhibit math error messages
      Var = Var / 4.0			;correct exponent bias
      ErrCode = Check_Math()		;check and clear math errors
      If (ErrCode ne 0) Then Begin	;true: invalid conversion
        Message,/Info,'F*4 conversion failed for some/all elements.'
      EndIf
    EndElse
  EndIf
  If (G.EleSiz eq 8) Then Begin		;eight byte data
    ByteOrder,/SSwap,Var		;swap 12345678->21436587
    Var = Long(Var,0,BytCnt/4)		;group into longwords
    iVar = 2 * LIndGen(BytCnt/8)	;index every other longword
    ShftBits = Var(iVar) mod 8		;bits to shift into next longword
    SignBits = Var(iVar) and '8000'XL
    Var(iVar) = Var(iVar) and '7FFF'XL
    Var(iVar) = IShft(Var(iVar),-3)
    Var(iVar) = Var(iVar) or SignBits
    BiasAdjust = 894 * Long(2) ^ 20	;additive bias adjustment to exponent
    Var(iVar) = Var(iVar) + BiasAdjust  ;(comment follows)
      ;shift most significant longwords right 3 bit (except sign bit,
      ; filling most significant bits of exponent with zeros). Adjust
      ; bias of exponent by subtracting bias change shifted left 20 bits.
    Var(iVar+1) = 8192 * ShftBits + IShft(Var(iVar+1),-3)  ;(comment follows)
      ;shift least sig. longwords right 3 bits (all bits, losing last 3 bits)
      ; shift in 3 least significant bits from most sig. longwords by adding.
    Var = Double(Var,0,BytCnt/8)	;convert to F*8
    Message,/Info,'F*8 restore is not working yet!
  EndIf

;Convert to scalar or reformat array, as necessary.
  If (G.ArrFlg eq 4) Then Begin	;true: variable is an array
    Var = Reform(Var,EleDim)		;reformat array
  EndIf Else Begin			;fall thru: variable is a scalar
    Var = Var(0)			;convert to scalar
  EndElse

;Clean up and exit.
  Free_Lun,Unit				;close file and release lun
  Return
End
