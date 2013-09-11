Function rdlin,File,Data,Range=Range $
         ,nProc=nProc,Lande=Lande,Depth=Depth,Ref=Ref,Vmicro=Vmicro
;Reads atomic line data from *.lin files for use in Piskunov RT code.
; File (input string) name of file containing atomic data.
; Data (ouput array) array of important atomic data
;  data(0,*) atomic number (H=1, He=2, Li=3, etc.)
;  data(1,*) ionization stage (neutral=0, singly=1, etc.)
;  data(2,*) central wavelength of transition (Angstroms)
;  data(3,*) excitation potential of lower state (eV)
;  data(4,*) log(gf)
;  data(5,*) radiative damping constant
;  data(6,*) quadratic Stark damping constant
;  data(7,*) van der Waal's damping constant
; [Range=] (output vector) beginning and ending wavelengths given in header
;   of line data file.
; [nProc=] (output long) number of lines processed to get current list.
; [Lande=] (output vector) Lande-g factors for each line.
; [Depth=] (output vector) Central depth predicted for each line.
; [Ref=] (output string vector) Source for atomic data.
; [Vmicro=] (output string vector) Source for atomic data.
;01-Nov-94 JAV Create.
;23-Jan-97 JAV Adapted from sme_rdlin, set return values for lande and depth.

If N_Params() lt 2 Then Begin $
  Print,'syntax: ErrStr = rdlin(File, Data $'
  Print,'  [,Range= ,nProc= ,Lande= ,Depth= ,Ref= ,Vmicro=])'
  Retall
EndIf

;Open file.
  OpenR,Unit,File,/Get_Lun			;open disk file

;Read header information.
  nLines = 0L					;init number selected lines
  nProc = 0L					;init number processed lines
  WFirst = 0d0					;force double precision
  WLast = 0d0					;force double precision
  ReadF,Unit,WFirst,Wlast,nLines,nProc		;read header
  Range = [WFirst,Wlast]			;load return variable
  StrBuf = ''					;define string buffer
  ReadF,Unit,StrBuf				;flush column headers
  ReadF,Unit,StrBuf				;flush column headers

;Initialize atomic line data vectors.
  nCol = 9					;number of data columns
  ElName = StrArr(nLines)			;init element and ion name
  Ref    = StrArr(nLines)			;init data reference string
  Data   = DblArr(nCol+2,nLines)		;init data array
  Lande  = FltArr(nLines)			;init Lande factors
  Depth  = FltArr(nLines)			;init predicted depths

;Loop thru atomic data in file, reading into IDL.
  DblBuf = DblArr(nCol)				;init numeric buffer
  For iLine=0,nLines-1 Do Begin			;loop thru lines
    ReadF,Unit,StrBuf				;read data as string
    ElName(iLine) = StrMid(StrBuf,1,4)		;get element and ion name
    iBeg = StrPos(StrBuf,"'",6)			;find beginning of reference
    iEnd = StrPos(StrBuf,"'",iBeg+1)		;find beginning of reference
    RefLen = iEnd - iBeg - 1			;length of reference comment
    Ref(iLine) = StrMid(StrBuf,iBeg+1,RefLen)	;extract reference string
    ReadS,StrMid(StrBuf,7,iBeg-1),DblBuf	;parse numeric arguments
    Data(2:1+nCol,iLine) = DblBuf		;save results into array
    Lande(iLine) = Float(StrMid(StrBuf,61,6))	;extract predicted depth
    Depth(iLine) = Float(StrMid(StrBuf,68,6))	;extract predicted depth
  EndFor
  Free_Lun,Unit					;Release unit and file

;Convert element names into atomic number and insert into data array.
  ElemParse, ElName, AtomNumb, IonStage		;convert to atomic number
  Data(0,*) = AtomNumb				;insert atomic numbers
  Data(1,*) = IonStage				;insert ionization stage
  iBad = Where(AtomNumb le 0, nBad)		;find unknown names
  If nBad gt 0 Then Begin			;true: bad element names
    BadList = ''
    For i=0,nBad-1 Do Begin			;loop thru bad names
      If i gt 0 Then BadList = BadList + ','	;put separator, if needed
      BadList = BadList + ElName(iBad(i))	;build bad name list
    EndFor
    Return,'Unknown element name(s): ' + BadList
  EndIf
  iBad = Where(IonStage lt 0, nBad)		;find bad ionization
  If nBad gt 0 Then Begin			;true: bad element names
    BadList = ''
    For i=0,nBad-1 Do Begin			;loop thru bad names
      If i gt 0 Then BadList = BadList + ','	;put separator, if needed
      BadList = BadList + ElName(iBad(i))	;build bad name list
    EndFor
    Return,'Bad Ionization Stage(s): ' + BadList
  EndIf

;Trim leading and trailing spaces in reference comments.
  Ref = StrTrim(Ref,2)				;trim leading/trailing space

;Return line data in array, if original argument list was short.
  iKeep = [0,1,2,3,5,6,7,8]			;inidices in atomic to keep
  Data = Data(iKeep,*)				;put trimmed array in argument

  Return,''					;return without error

End
