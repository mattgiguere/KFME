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
;    struct2names_and_ptrs
;
; PURPOSE:
;   This functions creates an array of tagnames and and an array of pointers of the submitted structure
;   The return value of this function is n_elements(names) and the keywords names and ptr_values
;   This keywords are defined as output only.
;
; CATEGORY:
;   PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;   result=struct2names_and_ptrs(struct,[tagname],[tname=tname],names=names,ptr_values=ptr_values,[recurse=recurse])
;
; INPUTS:
;   struct: the structure to transform to names and ptr_values
;
; OPTIONAL INPUTS:
;   tagname: to separate the tagnames which should be expanded to names and ptr_values
;   wildcard (*) is forbidden
;
; In dieser Routine wu"rde das ganz gut funktionieren, jedoch vor der Umstellung auf idl5.3
; sind die A"nderungen in der Syntax fu"r die darauf aufbauenden Funktionen doppelt
; auszufu"hren, daher unterdru"cke ich derzeit diese Funktionalita"t.
;
;
;
; KEYWORD PARAMETERS:
;   tname: is used recursivly for the substructures
;   names: is used recursivly for the substructures to save the names
;          ONLY OUTPUT
;   ptr_values: is used recursivly for the substructures to save the pointers
;          ONLY OUTPUT
;
;   recurse: is used recursivly for the substructures
;
; PROCEDURE:
;   This routine was written after a discussion of Frank Hollands Meta-Structure
;   First idea Frank Holland, advanced idea Franz Rohrer and Reimar Bauer
;
;
;
; EXAMPLE:
;   d={A:1,B:{B1:0,B2:1},c:{B1:0,B2:1}}
;
;   n=struct2names_and_ptrs(d,names=names,ptr_values=ptr_values)
;   PRINT,n
;      5
;   PRINT,names
;     A B.B1 B.B2 C.B1 C.B2
;
;   PRINT,ptr_values
;    <PtrHeapVar1839><PtrHeapVar1840><PtrHeapVar1841><PtrHeapVar1842><PtrHeapVar1843>
;
;
;   n=struct2names_and_ptrs(d,'B',names=names,ptr_values=ptr_values)
;   PRINT,n
;      4
;   PRINT,names
;     A B.B1 B.B2 C
;
;   PRINT,ptr_values
;     <PtrHeapVar1844><PtrHeapVar1845><PtrHeapVar1846><PtrHeapVar1847>
;
;
;
; MODIFICATION HISTORY:
;       Written by:     R.Bauer (ICG-1), 2000-Jan-22
;  2000-Jul-29: bug removed if tagname was lowercase typed
;-

FUNCTION struct2names_and_ptrs,struct,tagname,tname=tname,names=names,ptr_values=ptr_values,recurse=recurse,no_copy=no_copy,_extra=p_key

   IF N_PARAMS() GT 0 THEN BEGIN
      IF is_Structure(struct) EQ 0 THEN BEGIN
         MESSAGE,call_help(),/info
         RETURN,-1
      ENDIF

      IF N_ELEMENTS(recurse) EQ 0 THEN BEGIN
; names and ptr_values only for output
         IF N_ELEMENTS(names) GT 0 THEN dummy=TEMPORARY(names)
         IF N_ELEMENTS(ptr_values) GT 0 THEN dummy=TEMPORARY(ptr_values)
      ENDIF

      IF N_ELEMENTS(tname) EQ 0 THEN tname=''

      tn=TAG_NAMES(struct)

      IF N_ELEMENTS(tagname) EQ 0 THEN in_tagname=tn ELSE in_tagname=strupcase(tagname)
      test=WHERE(STRPOS(in_tagname,'*') GT -1,count_test)
      IF count_Test  GT 0 THEN BEGIN
         STOP,tagname+' wildcard is forbidden!'
         RETURN,-1
; in dieser Routine wu"rde das ganz gut funktionieren, jedoch vor der Umstellung auf idl5.3
; sind die A"nderungen in der Syntax fu"r die darauf aufbauenden Funktionen doppelt
; auszufu"hren, daher unterdru"cke ich derzeit diese Funktionalita"t.
      ENDIF
;         IF STRPOS(tagname,'*') GT -1 THEN BEGIN
;            in_tagname=text_filter(tn,tagname)
;         ENDIF ELSE in_tagname=tagname
;      ENDELSE


      n_tn=N_ELEMENTS(tn)
      FOR i=0,n_tn-1 DO BEGIN
         k=WHERE(in_tagname EQ tn[i],count_tag)
         IF is_structure(struct.(i)) AND count_tag NE 0 THEN BEGIN ;sp anders
            IF tname NE '' THEN sub_tn=tname+'.'+tn[i] ELSE sub_tn=tn[i]
            n=struct2names_and_ptrs(struct.(i),tname=sub_tn,names=names,ptr_values=ptr_values,recurse=1,no_copy=no_copy,_extra=p_key)
            ENDIF ELSE BEGIN

            name=tname+'.'+tn[i]
            IF STRPOS(name,'.') EQ 0 THEN name=STRMID(name,1,32767)

            IF N_ELEMENTS(names) EQ 0 THEN names=name ELSE names=[names,name]
            IF N_ELEMENTS(ptr_values) EQ 0 THEN ptr_values=PTR_NEW(struct.(i),no_copy=no_copy) ELSE ptr_values=[ptr_values,PTR_NEW(struct.(i),no_copy=no_copy)]


         ENDELSE

      ENDFOR

      RETURN,N_ELEMENTS(names) ;names
      ENDIF ELSE BEGIN
      MESSAGE,call_help(),/info
      RETURN,0
   ENDELSE
END
