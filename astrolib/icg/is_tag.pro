;
; Copyright (c) 1997, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; NAME:
;  is_tag
;
; PURPOSE:
;   Test if tagname is defined
;
; CATEGORY:
;   PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;   Result=is_tag(structure,tagname)
;
; INPUTS:
;   structure:  the structure
;   tagname:    the tagname as string which should be searched in structure
;
; OUTPUTS:
;   Result will be 1 or 0
;
; KEYWORD PARAMETERS:
;   count: if set to a named variable it returns the number of tags equal to tagname
;          normally it is 1
;   part: if is set strpos is used to find a part of a string
;
; EXAMPLE:
;   print,is_tag(inhalt,'param')
;   1
;
; MODIFICATION HISTORY:
; 	Written by:	R.Bauer (ICG-1) , Sep. 2 1996
;               F.Rohrer (ICG-3), Mai 15 1997 downgrade to idl 3.6.1
;               R.Bauer 1998-Jul-05 previously named as find_tag now renamed for better consistens
;               R.Bauer 1998-Jul-05 upgraded to idl 5.1
;               R.Bauer 1998-Nov-19 goto removed additional test of tag_name is string added
;               R.Bauer 1999-Mar-26 keyword part is added
;   2000-Feb-18 help changed to call_help
;   2001-May-11 additional info if structure isn't defined
;
;-


FUNCTION is_tag,struct,tag_name,part=part,count=count

   IF N_PARAMS(0) LT 2  THEN BEGIN
      HELP: MESSAGE,call_help(),/cont
      RETURN,0
   ENDIF

   IF NOT is_string(tag_name) THEN BEGIN
      MESSAGE,'tag_name must be of type string',/cont
      RETURN,0
   ENDIF

   IF NOT is_structure(struct) THEN BEGIN
      help,call=call
      MESSAGE, 'structure is not defined',/cont
      print,call
      RETURN,0
   ENDIF


   tagname=STRUPCASE(tag_name)

   tags=TAG_NAMES(struct)
   if keyword_set(part) then begin
   a=WHERE(strpos(tags, tagname) eq 0,count)
   if count gt 1 then message,'WARNING: count: '+strtrim(string(count),2),/info
   endif else a=WHERE(tags EQ tagname,count)

   RETURN, count gt 0
END
