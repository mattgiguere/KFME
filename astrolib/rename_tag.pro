; Copyright (c) 1999, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
;
; USERLEVEL:
;   TOPLEVEL
;
; NAME:
;    rename_tag
;
; PURPOSE:
;   This functions renames a tag in a structure or sub structure.
;
; CATEGORY:
;   PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;   result = rename_tag(struct, tagname, new_tagname, [sub_structure=sub_structure])
;
; INPUTS:
;   struct: the structure where tags will be renamed
;   tagname: the old tag name
;   new_tagname: the new tag name
;
; KEYWORD PARAMETERS:
;   sub_structure: to define the sub structure where tag will be renamed
;                  If not set tags on the main level will be renamed.
;   /ALL: If set all tags which agree to tagname will be renamed starting from the
;         main level or the level 'sub_structure' downwards.
;
; OUTPUTS:
;   This function returns a structure with renamed tags.
;
; EXAMPLE:
;   d = {A: 1, B: {B1: 0, B2: 1}, C: {B1: 0, B2: 1}}
;
;   help,d,/str
;   ** Structure <79b4a18>, 3 tags, length=10, refs=1:
;   A               INT              1
;   B               STRUCT    -> <Anonymous> Array[1]
;   C               STRUCT    -> <Anonymous> Array[1]
;
;   help, rename_tag(d, 'A', 'XX'), /str
;   ** Structure <46ace98>, 3 tags, length=10, refs=1:
;   XX              INT              1
;   B               STRUCT    -> <Anonymous> Array[1]
;   C               STRUCT    -> <Anonymous> Array[1]
;
;   help, rename_tag(d, 'B', 'YY'), /str
;   ** Structure <46ace98>, 3 tags, length=10, refs=1:
;   A               INT              1
;   YY              STRUCT    -> <Anonymous> Array[1]
;   C               STRUCT    -> <Anonymous> Array[1]
;
;   result = rename_tag(d, 'B1', 'BY1', SUB = 'C')
;   HELP, result.c, /STR
;   ** Structure <133c8c8>, 2 tags, length=4, refs=1:
;   BY1             INT              0
;   B2              INT              1
;
;   result = rename_tag(d, 'B1', 'BY1', /ALL)
;   HELP, result.c, /STR
;   ** Structure <10551e8>, 2 tags, length=4, refs=2:
;   BY1             INT              0
;   B2              INT              1
;
;   HELP, result.b, /STR
;   ** Structure <1055378>, 2 tags, length=4, refs=2:
;   BY1             INT              0
;   B2              INT              1
;
;   d={p:{plot:1}}
;   result=rename_tag(d,'p','r')
;   help,result.r,/str
;   ** Structure <140baa0>, 1 tags, length=2, refs=2:
;   PLOT            INT              1
;
;
; MODIFICATION HISTORY:
;       Written by:     R.Bauer (ICG-1), 2000-Jan-05
;       14.03.2000: Header edited
;       2001-06-27 : bug with tags with a strlen of 1 removed
;-
FUNCTION rename_tag,struct,tagname,new_tagname,sub_Structure=sub_Structure,SORT=sort,all=all

   IF N_PARAMS() GE 1 THEN BEGIN


      IF N_ELEMENTS(struct) GT 0 THEN in_struct=struct
      IF N_ELEMENTS(tagname) GT 0 THEN in_tagname=tagname
      IF N_ELEMENTS(new_tagname) GT 0 THEN in_new_tagname=new_tagname
      IF N_ELEMENTS(sub_structure) GT 0 THEN in_sub_structure=sub_structure
; ============================================================================= same in delete_tag and replace_tagvalue and rename_tag
      IF KEYWORD_SET(all) THEN begin
      in_sub_structure=TAG_NAMES(struct)
      no=32767
      endif else no=1 ; number of replaces
      IF N_ELEMENTS(in_sub_structure) GT 0 THEN BEGIN

         n=N_ELEMENTS(in_sub_structure)
         FOR ii=0,n-1 DO BEGIN

            IF STRPOS(in_sub_structure[ii],'.') GT -1 THEN BEGIN
               part=STR_SEP(in_sub_structure[ii],'.')
               sub=STRUPCASE(part[0])  ; interne Variablen verwenden
               build_vector,to_add,STRUPCASE(arr2string(part[1:*],sep='.')+'.'+in_tagname)
               ENDIF ELSE BEGIN
               build_vector,sub,STRUPCASE(in_sub_structure[ii])
               IF N_ELEMENTS(in_tagname) GT 0 THEN build_vector,to_add, in_tagname
            ENDELSE
         ENDFOR

         IF N_ELEMENTS(to_add) GT 0 THEN in_tagname=to_Add[uniq(to_add,SORT(to_add))]
         n_names=struct2names_and_ptrs(struct,sub,names=names,ptr_values=ptr_values)
         subs=sub+'.'
         ENDIF ELSE BEGIN
         n_names=struct2names_and_ptrs(struct,names=names,ptr_values=ptr_values)
         subs=''
      ENDELSE
      ms=0b
      IF N_ELEMENTS(in_tagname) EQ 0 THEN dtext=names[strmatch2(names,STRUPCASE(subs)+'*')] ELSE BEGIN

         k=WHERE(STRPOS(in_tagname,'*') GT -1,count)
         IF count GT 0 AND subs[0] NE '' THEN BEGIN

            dtext=names[strmatch2(names,str_matrix(subs,STRUPCASE(in_tagname)))]
            ENDIF ELSE BEGIN

            del_tn=STRUPCASE(str_matrix(subs[0],in_tagname))
            FOR i=1,N_ELEMENTS(subs)-1 DO del_tn=[del_tn,STRUPCASE(str_matrix(subs[i],in_tagname))]
            del_tn=[del_tn,del_tn+'.*'] ; ?
            del_tn=del_tn[uniq(del_tn,SORT(del_tn))]
            idx=strmatch2(names,del_tn,count=c_idx)
            IF c_idx GT 0 THEN  dtext=names[idx] ELSE dtext=''

         ENDELSE
      ENDELSE
      dtext=dtext(UNIQ(dtext))

; ============================================================================= same in delete_tag and rename_tag

      idx = MAKE_ARRAY(N_ELEMENTS(names),/LONG)
      FOR i = 0L, N_ELEMENTS(names) - 1 DO idx[i] = (WHERE(dtext EQ names[i]))[0]
      idx=WHERE(idx NE -1,count_idx)
      IF count_idx GT 0 THEN BEGIN
         IF N_ELEMENTS(in_tagname) GT 0 THEN BEGIN
            k=WHERE(STRPOS(in_tagname,'*') GT -1,count)

            IF count GT 0 THEN  tn=STRUPCASE(replace_string(in_tagname,'*','')) ELSE tn = STRUPCASE(in_tagname)
            names[idx]=replace_string(names[idx],tn,STRUPCASE(in_new_tagname),no=no)
            ENDIF ELSE BEGIN
            names[idx]=replace_string(names[idx],STRUPCASE(in_sub_structure),STRUPCASE(in_new_tagname),no=no)
         ENDELSE
      ENDIF
      result=names_and_ptrs2struct(names,ptr_values,SORT=sort)

      RETURN,result

      ENDIF ELSE BEGIN
      MESSAGE,call_help(),/cont
      RETURN,-1
   ENDELSE

END
