;+
; NAME:
;       replace_tag
;
; PURPOSE:
;  Replaces a tag in a structure by an other tag. This is necessary if you need other types or dimensions of a tag name
;
; CATEGORY:
;  PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;  replace_tag, struct, old_tag, new_tag, value,[/first],[_extra=extra]
;
; INPUTS:
;	struct		- the structure to be changed
;	old_tag		- name of the tag to be changed
;	new_tag		- name of the new tag
;			      If set to '', old_tag is deleted from the structure
;	value		- value of the new tag
;
; KEYWORD PARAMETERS:
;  first: if set to 1 then replacement tag_name is set as first one.
;  _extra: is used to submit first as part of a structure
;
; RESTRICTIONS:
;	Does not work with named structures
;
; EXAMPLE:
;  test={aa:'1',bb:2}
;  replace_tag,test,'aa','ID',fltarr(10)
;  help,test,/str
;  ** Structure <1192ad8>, 2 tags, length=44, refs=1:
;     BB              INT              2
;     ID              FLOAT     Array[10]
;
;  test={aa:'1',bb:2}
;  replace_tag,test,'aa','aa',fltarr(10)
;  help,test,/str
;  ** Structure <1180518>, 2 tags, length=44, refs=1:
;     BB              INT              2
;     AA              FLOAT     Array[10]
;
;  test={aa:'1',bb:2}
;  replace_tag,test,'aa',''
;  help,test,/str
;  ** Structure <1193b18>, 1 tags, length=2, refs=1:
;     BB              INT              2
;
; MODIFICATION HISTORY:
;   Alex Schuster, MPIfnF, 8/97 - Written first
;   R.Bauer 1998-09-26  The most syntax is replaced by del_tag (F.Holland) and tag_position (F.Rohrer)
;                       Unfortunately did add_tag only work with named structures - should be discussed
;                       so I used create_struct
;   R.Bauer 1999-12-05  keyword first and _Extra added
;-


PRO replace_tag, struct, old_tag, new_tag, value,first=first,_extra=pkey

   IF N_PARAMS() LT 3 THEN BEGIN
      MESSAGE,call_help(),/cont
      RETURN
   ENDIF

   IF new_tag EQ '' THEN struct=del_tag(struct,old_tag) ELSE BEGIN
      pos=(WHERE(TAG_NAMES(struct) EQ STRUPCASE(old_tag)))[0]
      IF pos NE -1 THEN struct=del_tag(struct,old_tag)
      IF KEYWORD_SET(first) THEN struct=CREATE_STRUCT(new_tag,value,struct) ELSE $
                                        struct=CREATE_STRUCT(struct,new_tag,value)
   ENDELSE
END
