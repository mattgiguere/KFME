; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	del_tag2
;
; PURPOSE:
;	This function deletes a tag from a given structure.
;	It also handels an array of tags and/or 1-dim array of structures.
;
; CATEGORY:
;	PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;	x = del_tag2(str, tag)
;
; INPUTS:
;	str: variable name of structure
;	tag: name of tag or array of tag names to be deleted (STRING)
;
; KEYWORD PARAMETERS:
;	RETURN_ON_LAST_TAG_DELETED: Defines a variable which will be returned if the only tag
;								in str is deleted.
;	NAME: New name for a named structure (must not exist already)
;
; OPTIONAL OUTPUTS:
;	status: Status information 	=0: every thing worked fine;
;								=1: something went wrong (see messages on screen)
;
; EXAMPLE:
;	as = {Bsp1, a:0, b:'asd'}
;	bs = del_tag2(as, 'b', NAME = 'Bsp3')
;	HELP, bs, /STR :
;		** Structure BSP3, 1 tags, length=2:
;		   A               INT              0
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 11. Apr. 98.
;	This program is based on the procedure del_tag.pro but provides additional features
;	15.11.98:	call to debug deleted
;-

FUNCTION del_tag2, str, tag, status, RETURN_ON_LAST_TAG_DELETED = ret, NAME = nm

	status = 0

	; ***** Handle wrong input *****

	IF NOT is_structure(str) THEN BEGIN
		MESSAGE, 'Returned without change since parameter <str> is not of type structure.', /INFORMAT
		MESSAGE, '<status> was set to 1', /INFORMAT
		status = 1
		RETURN, str
	ENDIF
	IF NOT is_string(tag) THEN BEGIN
		MESSAGE, 'Returned unchanged structure since parameter <tag> is not of type string.', /INFORMAT
		MESSAGE, '<status> was set to 1', /INFORMAT
		status = 1
		RETURN, str
	ENDIF

	; ***** Set defaults *****

	IF N_ELEMENTS(ret) EQ 0 THEN ret = -1 ; set default return value to -1 if last tag of structure is deleted

	; ***** Handle array of structures *****

	n = N_ELEMENTS(str)
	IF n GT 1 THEN BEGIN
		IF N_ELEMENTS(nm) EQ 0 THEN nm = cr_struct_name() ; Set default name
		new_str0 = del_tag2(str[0], tag, status, RETURN_ON_LAST_TAG_DELETED = ret, NAME = nm)
		new_str = REPLICATE(new_str0, n)
		FOR i = 1L, n - 1 DO $
			new_str[i] = del_tag2(str[i], tag, status, RETURN_ON_LAST_TAG_DELETED = ret, NAME = nm)
		RETURN, new_str
	ENDIF

	; ***** Determine dimensions and tagnames *****

	ntags = N_TAGS(str)
	tn = TAG_NAMES(str)
	tag = STRUPCASE(tag)

	; ***** Handling of structures with only one tag *****

	IF ntags EQ 1 THEN $  ; Return either ret or structure if there was only one tag in structure
		IF tn[0] EQ tag[0] THEN RETURN, ret ELSE RETURN, str

	; ***** Recursive call of del_tag2 if more than one tag are to be deleted *****

	ndt = N_ELEMENTS(tag)
	IF ndt GT 1 THEN BEGIN
		new_str = str
		FOR i = 0L, N_ELEMENTS(tag) - 1 DO $
			new_str = del_tag2(new_str, tag[i], status, RETURN_ON_LAST_TAG_DELETED = ret)
		IF KEYWORD_SET(nm) THEN new_str = CREATE_STRUCT(new_str, NAME = nm)
		RETURN, new_str
	ENDIF

	; ***** Build the new structure without tag *****

	k = (WHERE(tn EQ tag[0]))[0] ; index of tag to be deleted
	CASE k OF
		-1: RETURN, str ; tag does not exist in structure
		 0: BEGIN ; tag to be deleted is 1. element of structure
			new_str = CREATE_STRUCT(tn[1], str.(1))
			IF ntags GT 2 THEN FOR i = 2L, ntags - 1 DO $
				new_str = CREATE_STRUCT(new_str, tn[i], str.(i))
		END
		ELSE: BEGIN
			new_str = CREATE_STRUCT(tn[0], str.(0))
			IF ntags GT 2 THEN FOR i = 1L, ntags - 1 DO $
				IF i NE k THEN new_str = CREATE_STRUCT(new_str, tn[i], str.(i))
		END
	ENDCASE

	IF KEYWORD_SET(nm) THEN new_str = CREATE_STRUCT(new_str, NAME = nm)

	RETURN, new_str
END

