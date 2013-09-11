; Copyright (c) 2000, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; USERLEVEL:
;   TOPLEVEL
;
; NAME:
;    add_tag
;
; PURPOSE:
;   This functions adds a tag on an arbitrary level of a structure.
;
; CATEGORY:
;   PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;   result = add_tag(struct, tagname, tagvalue [, sub_structure = sub_structure])
;
; INPUTS:
;   struct  : the structure to which the tag will be added
;   tagname : the name of the new tag
;   tagvalue: the value of the new tag
;
; KEYWORD PARAMETERS:
;   sub_structure: to define the sub structure where the tag should be added.
;		If not given the tag will be added on the main level of the structure (see examples)
;
; OUTPUT:
;   This function returns a structure with a new tag added to it.
;
; EXAMPLE:
;
;   d = {A: 1, B: {B1: 0, B2: 1}, C: {B1: 0, B2: 1}}
;
;   help,d,/str
;   ** Structure <1331608>, 3 tags, length=10, refs=1
;   A               INT              1
;   B               STRUCT    -> <Anonymous> Array[1]
;   C               STRUCT    -> <Anonymous> Array[1]
;
;   result = add_tag(d,'ASA',11)
;   HELP, result, /STR
;   ** Structure <10530e8>, 4 tags, length=12, refs=1:
;   A               INT              1
;   B               STRUCT    -> <Anonymous> Array[1]
;   C               STRUCT    -> <Anonymous> Array[1]
;   ASA             INT             11
;
;   result = add_tag(d, 'ASA', 11, sub = 'B')
;   HELP, result.b, /STR
;   ** Structure <133ce88>, 3 tags, length=6, refs=1
;   B1              INT              0
;   B2              INT              1
;   ASA             INT             11
;
;   result = add_tag(d, 'BSB', {C11: INDGEN(3)}, sub = 'B.B3')
;   HELP, result.b, /STR
;   ** Structure <1357758>, 3 tags, length=10, refs=2:
;   B1              INT              0
;   B2              INT              1
;   B3              STRUCT    -> <Anonymous> Array[1]
;
;   result = add_tag(d, 'BSB', {C11: INDGEN(3)}, sub = ['B','C'])
;   help,result.b,/str
;   ** Structure <1055618>, 3 tags, length=10, refs=2:
;   B1              INT              0
;   B2              INT              1
;   BSB             STRUCT    -> <Anonymous> Array[1]
;
;   help,result.c,/str
;   ** Structure <1056968>, 3 tags, length=10, refs=2:
;   B1              INT              0
;   B2              INT              1
;   BSB             STRUCT    -> <Anonymous> Array[1]
;
;
;   result = add_tag(d, 'BSB', {C11: INDGEN(3)}, sub = ['B.C1','C'])
;   help,result.b,/str
;   ** Structure <1056168>, 3 tags, length=10, refs=2:
;   B1              INT              0
;   B2              INT              1
;   C1              STRUCT    -> <Anonymous> Array[1]
;
;   result = add_tag(d, 'BSB', {C11: INDGEN(3)}, sub = 'F')
;   help,result,/str
;   ** Structure <10550f8>, 4 tags, length=16, refs=1:
;   A               INT              1
;   B               STRUCT    -> <Anonymous> Array[1]
;   C               STRUCT    -> <Anonymous> Array[1]
;   F               STRUCT    -> <Anonymous> Array[1]
;
; MODIFICATION HISTORY:
;       Written by:     R.Bauer (ICG-1), 2000-Jan-05
;       14.03.2000: Header edited
;-
;

FUNCTION add_tag,value1,value2,value3,sub_structure=sub_structure,sort=sort

   IF N_PARAMS() GE 2 THEN BEGIN
      n=N_ELEMENTS(value2)
      CASE N_PARAMS() OF
         3: BEGIN
            IF is_structure(value1)  THEN BEGIN
               IF STRPOS(value2,'.') GT -1 THEN STOP, value2+ ' . is forbidden in a tag definition'
; value1 is a structure
; value2 is a tagname
; value3 is the tagvalue

               IF N_ELEMENTS(sub_Structure) GT 0 THEN  BEGIN
               n=n_elements(sub_Structure)
               for ii=0,n-1 do begin

                  IF STRPOS(sub_Structure[ii],'.') GT -1 THEN BEGIN
                     part=STR_SEP(sub_structure[ii],'.')
                     sub=part[0]  ; interne Variablen verwenden
                     build_vector,to_add,arr2string(part[0:*],sep='.')+'.'+value2
                     ENDIF ELSE BEGIN
                     build_vector,sub,STRUPCASE(sub_Structure[ii])
                     build_vector,to_add,STRUPCASE(sub_Structure[ii])+'.'+value2
                  ENDELSE
               endfor
               n_names=struct2names_and_ptrs(value1,sub,names=names,ptr_values=ptr_values)
               IF N_ELEMENTS(names) EQ 0 THEN BEGIN
                  names=STRUPCASE(sub[0])
                  ptr_values=PTR_NEW(value3)

                  ENDIF ELSE BEGIN
                  FOR i=0,n-1 DO BEGIN
                     names=[names,STRUPCASE(to_add[i])]
                     ptr_values=[ptr_values,PTR_NEW(value3)]

                  ENDFOR
               ENDELSE
            ENDIF ELSE return,create_struct(value1,value2,value3)
            idx=sort_by_name(names,tag_names(value1))
            RETURN,names_and_ptrs2struct(names[idx],ptr_values[idx],sort=sort)
            ENDIF
         END
         2: BEGIN
           ; call create_struct
         return,create_struct(value1,value2)
         END
         ELSE: MESSAGE,call_help(),/cont
         ENDCASE
      ENDIF ELSE BEGIN
      MESSAGE,call_help(),/cont
      RETURN,-1
   ENDELSE
END
