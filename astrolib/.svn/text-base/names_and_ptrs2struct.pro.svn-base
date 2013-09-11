; Copyright (c) 1999, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; NAME:
;    names_and_ptrs2struct
;
; PURPOSE:
;   This functions constructs a structure from a name and pointer data array
;
; CATEGORY:
;   PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;   result=names_and_ptrs2struct(names,ptr_values,[sort=sort],[recurse=recurse])
;
; INPUTS:
;   names:      the the names of the meta-structure to transform to a structure
;   ptr_values: the pointer values of the meta-structure to transform to a structure
;
; KEYWORD PARAMETERS:
;  sort: if set sort_names_and_ptrs is used to sort
;  recurse: is used by recursion of names_and_ptrs2struct.
;
; PROCEDURE:
;   This routine was written after a discussion of Frank Hollands Meta-Structure
;   First idea Frank Holland, advanced idea Franz Rohrer and Reimar Bauer
;
;
; EXAMPLE:
;   d={A:1,B:{B1:0,B2:1},C:2}
;   n=struct2names_and_ptrs(d,names=names,ptr_values=ptr_values)
;   PRINT,n
;      4
;   PRINT,names
;     A B.B1 B.B2 C
;
;   PRINT,ptr_values
;   <PtrHeapVar458><PtrHeapVar459><PtrHeapVar460><PtrHeapVar461>
;
;   result=names_and_ptrs2struct(names,ptr_values)
;   help,result,/str
;   ** Structure <7d4eb68>, 2 tags, length=6, refs=1:
;   A               INT              1
;   B               STRUCT    -> <Anonymous> Array[1]
;   C               INT              2
;
;   help,result.b,/str
;   ** Structure <7d4eef8>, 2 tags, length=4, refs=2:
;   B1              INT              0
;   B2              INT              1
;
;
; MODIFICATION HISTORY:
;       Written by:     R.Bauer (ICG-1), 2000-Jan-05
;       2003-04-29 : bug description where duplicatet names are found added
;
;-

FUNCTION names_and_ptrs2struct,names,ptr_values,sort=sort,recurse=recurse,_extra=e

   IF N_PARAMS() EQ 2 THEN BEGIN
      IF N_ELEMENTS(recurse) EQ 0 THEN BEGIN
         if is_structure(e) then begin
         if is_tag(e,'no_sort') then begin
         message,'the KEYWORD: no_sort is obsolete!!!',/cont
         HELP, CALL=call
         MESSAGE, 'CALLED by: '+call[1], /INFORMATION
         endif
         endif
         test=UNIQ(names,sort(names))
         IF N_ELEMENTS(test) NE N_ELEMENTS(names) THEN BEGIN
            MESSAGE,'DUPLICATE NAMES!!'+ $
            string(10b)+arr2string(arr_equal(names))+$
            string(10b)+'========================='+$
            string(10b)+arr2STRING(names[test],sep=string(10b)),/cont
        
            RETURN,-1
         ENDIF
      ENDIF


      IF KEYWORD_SET(sort)  THEN BEGIN
         idx=sort_names_and_ptrs(names,ptr_values)
         in_names=names[idx]
         in_ptr_values=ptr_values[idx]
         ENDIF ELSE BEGIN
         in_names=names
         in_ptr_values=ptr_values
      ENDELSE


      n_names=N_ELEMENTS(in_names)
      i=0

      WHILE i LT n_names DO BEGIN
         IF STRPOS(in_names[i],'.') EQ -1 THEN BEGIN
            IF N_ELEMENTS(result) EQ 0 THEN result=CREATE_STRUCT(in_names[i],(*in_ptr_values[i])) ELSE  result=CREATE_STRUCT(result,in_names[i],(*in_ptr_values[i]))
            PTR_FREE,in_ptr_values[i]
            ENDIF ELSE BEGIN
            se=STR_SEP(in_names[i],'.')
            idx=WHERE(STRPOS(in_names ,se[0]+'.') EQ 0 ,count_se) ; am Anfang und mit . als Ende
            sub_Struct=(STRMID(in_names[idx],0,STRLEN(se[0])))[0]
            new_names=STRMID(in_names[idx],STRLEN(se[0])+1,32767)
            new_ptr_values=in_ptr_values[idx]
            ss=names_and_ptrs2struct(TEMPORARY(new_names),TEMPORARY(new_ptr_values),sort=sort,recurse=1)
            IF N_ELEMENTS(result) EQ 0 THEN BEGIN
               result=CREATE_STRUCT(sub_struct,TEMPORARY(ss))
               ENDIF ELSE BEGIN
               result=CREATE_STRUCT(result,sub_Struct,TEMPORARY(ss))
            ENDELSE
         ENDELSE
         IF N_ELEMENTS(count_se) GT 0 THEN i=I+TEMPORARY(count_se) ELSE i=i+1
      ENDWHILE

      RETURN,result


      ENDIF ELSE BEGIN
      MESSAGE,call_help(),/cont
      RETURN,-1
   ENDELSE

END
