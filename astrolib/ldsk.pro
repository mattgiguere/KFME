Pro LDsk,InFile
;General purpose routine to list variable WDsk'ed to disk.
; InFile (input string) name of file which is to have its contents listed.
;15-Jul-91 JAV	Create.
;10-Dec-91 JAV	Cleaned up display of multi-line comments.
;11-May-94 JAV  Added on_error trap. 
;10-Sep-94 JAV	Added path expansion.

;Verify that enough arguments were passed.
  If N_Params() lt 1 Then Begin			;true: not enough arguments
    Message,/Info,'Syntax: LDsk,File'
    RetAll					;leave user at top level
  EndIf
 
;If an error occurs below, return to the main program level. 
  On_Error, 1 

;Determine whether the requested file exists.
  File = InFile					;copy to local variable
  Siz = Size(File)				;variable information block
  If Siz(N_Elements(Siz)-2) ne 7 Then Begin	;variable type (table B.4)
    Message,'Second argument must be a string specifying the input filename.'
  EndIf
  File = Expand_Path(File)			;expand path
  Dummy = FindFile(File)			;number of files found
  If N_Elements(Dummy) gt 1 Then Begin		;true: wildcarded filename
    Message,'No wildcards are allowed in filename specification.'
  EndIf
  Dummy = Dummy(0)				;convert filename to scalar
  If StrLen(Dummy) eq 0 Then Begin		;true: file doesn't exist
    Message,'Can''t find input file: ' + File
  EndIf

;Open file.
  OpenR,Unit,File,/Get_Lun			;open file
  FInfo = FStat(Unit)				;file info block (structure)
  FSiz = FInfo.Size				;file size in bytes
  iRec = 1					;set current record pointer
  If FSiz lt 4 Then Goto,E_BadRec		;file too short

;Determine whether file contains single variable (needed to parse arguments).
  Single = 0					;assume multiple variables
  HLen = Long(0)				;init record length header
  TLen = Long(0)				;init record length tailer
  ReadU,Unit,HLen				;read record length header
  If (HLen + 8) gt FSiz Then Goto,E_BadRec	;true: head points beyond EOF
  If (HLen + 8) eq FSiz Then Single = 1		;true: single variable stored

;Skip through records, verifying header/tailer integrity.
  FPtr = 0					;init pointer to current byte
  While not EOF(Unit) Do Begin			;loop thru all records
    If (FPtr + 4) gt FSiz Then Goto,E_BadRec	;remainder too short
    Point_Lun,Unit,FPtr				;adjust IDL's file pointer
    ReadU,Unit,Hlen				;read record length header
    FPtr = FPtr + HLen + 4			;point at record length tailer
    If (FPtr + 4) gt FSiz Then Goto,E_BadRec	;true: record beyond EOF
    Point_Lun,Unit,FPtr				;point at record length tailer
    ReadU,Unit,TLen				;read record length tailer
    If HLen ne TLen Then Goto,E_BadRec		;true: bad record structure
    NPtr = FPtr + 4				;save pointer to next record

 ;Read and validate header information within requested record.
    If (HLen lt 12) Then Goto,E_BadVar		;true: block doesn't fit
    FPtr = FPtr - HLen				;point into current record
    Point_Lun,Unit,FPtr				;make IDL point into record
    FormType = Fix(-1)				;init data format type
    ReadU,Unit,FormType				;read data format type
    If FormType ne 256 Then Goto,E_BadVar	;true: bad format type
    DateTime = BytArr(6)			;init date/time stamp
    ReadU,Unit,DateTime				;read date/time stamp
    CLen = Fix(-1)				;init comment length
    ReadU,Unit,CLen				;read comment length
    FPtr = FPtr + 10				;update our file pointer
    If CLen lt 0 Then Goto,E_BadVar		;true: invalid comment size
    Comment = ''				;init default null Comment
    If CLen gt 0 Then Begin			;true: comment follows
      If (FPtr+CLen+6) gt FSiz Then Goto,E_BadVar  ;true: block doesn't fit
       Comment = String(Replicate(32b,CLen))	;init comment string
      ReadU,Unit,Comment			;read comment string
    EndIf
    VTyp = Byte(-1)				;init variable type
    ReadU,Unit,VTyp				;read variable type
    If (VTyp lt 0) or (VTyp gt 7) Then Goto,E_BadVar  ;true: bad variable type
    nDim = Byte(-1)				;init number of dimensions
    ReadU,Unit,nDim				;read number of dimensions
    If (nDim lt 0) or (nDim gt 8) Then Goto,E_BadVar  ;true: invalid # dimens
    If nDim gt 0 Then Begin			;true: dimensions follow
      Dims = LonArr(nDim)			;init dimension vector
      ReadU,Unit,Dims				;read dimension vector
    EndIf Else Dims = 1				;else: single element vector

;Print out header information.
    OutStr = String(Format='(I3,1X,3(I2.2,:,"/"))' $
      ,iRec,DateTime(1:2),DateTime(0))
    OutStr = OutStr + String(Format='(1X,3(I2.2,:,":"))',DateTime(3:5))
    Case VTyp Of				;get variable type label
      0: VLab = 'Undefined'
      1: VLab = 'Byte'
      2: VLab = 'Int'
      3: VLab = 'Long'
      4: VLab = 'Float'
      5: VLab = 'Double'
      6: VLab = 'Complex'
      7: VLab = 'String'
    EndCase
    OutStr = OutStr + ' ' + VLab		;append variable type label
    If nDim gt 0 Then Begin			;must print sizes of Dims
      OutStr = OutStr + '('			;append parenthesis
      For iDim=0,nDim-1 Do Begin		;loop thru dimensions
        OutStr = OutStr + StrTrim(String(Dims(iDim)),2) + ','
      EndFor
      StrPut,OutStr,')',StrLen(OutStr)-1
    EndIf
    If StrLen(Comment) gt 0 Then OutStr = OutStr + ' [' + Comment + ']'
    OLen = StrLen(OutStr)			;length of output string
    If OLen le 79 Then Begin			;true: fits on one line
      Print,OutStr				;print information line
    EndIf Else Begin				;else: need multiple lines
      Print,StrMid(OutStr,0,79)			;print first line
      iPos = 79					;point at next char to print
      While iPos lt OLen-1 Do Begin		;print until done
        Len = (41 < (OLen - iPos))		;print no more than 55 chars
        Print,Format='(T37,A)',StrMid(OutStr,iPos,Len)
        iPos = iPos + Len
      EndWhile
    EndElse

    iRec = iRec + 1				;increment record pointer
    FPtr = NPtr					;set pointer to next record
    Point_Lun,Unit,FPtr				;point to next record header
  EndWhile

  Free_Lun,Unit					;close file,free unit
  Return					;successful completion exit

;Error exits arrived at via Goto statements. Do not fall though here.
  E_BadRec:					;bad record error entry point 
    Free_Lun,Unit				;close file,free unit.
    Message,'Record ' + StrTrim(String(iRec),2) + ' has improper structure.'

  E_BadVar:
    Free_Lun,Unit
    Message,'Record structure is fine, but variable information block is bad.'

End
