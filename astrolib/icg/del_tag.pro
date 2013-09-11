; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	del_tag
;
; PURPOSE:
;	This function deletes a tag from a given structure.
;	It also handels an array of tags.
;
; CATEGORY:
;	PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;	x = del_tag(str, tag)
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
; EXAMPLE:
;	as = {Bsp1, a:0, b:'asd'}
;	bs = del_tag(as, 'b', NAME = 'Bsp3')
;	HELP, bs, /STR :
;		** Structure BSP3, 1 tags, length=2:
;		   A               INT              0
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 28. Jan. 98.
;	29.09.98:	Debug statement deleted.
;-

FUNCTION del_tag, structure, tag, RETURN_ON_LAST_TAG_DELETED = ret, NAME = nm

	; ***** Return -1 if structure is not of type structure *****

	IF NOT is_structure(structure) THEN RETURN, -1

	; ***** Set defaults *****

	IF N_ELEMENTS(ret) EQ 0 THEN ret = -1 ; set default return value to -1 if last tag of structure is deleted
	IF N_ELEMENTS(nm) EQ 0 THEN nm = '' ; Set default to anonymous structure

	; ***** Determine dimensions and tagnames *****

	n = N_TAGS(structure)
	tn = TAG_NAMES(structure)
	tag = STRUPCASE(tag)

	; ***** Handling of structures with only one tag *****

	IF n EQ 1 THEN $  ; Return either ret or structure if there was only one tag in structure
		IF tn[0] EQ tag[0] THEN RETURN, ret ELSE RETURN, structure

	; ***** Recursive call of del_tag if more than one tag are to be deleted *****

	IF is_array(tag) THEN BEGIN
		new_str = structure
		FOR i = 0L, N_ELEMENTS(tag) - 1 DO $
			new_str = del_tag(new_str, tag[i], RETURN_ON_LAST_TAG_DELETED = ret, NAME = nm)
		RETURN, new_str
	ENDIF

	; ***** Build the new structure without tag *****

	k = (WHERE(tn EQ tag))[0] ; index of tag to be deleted
	CASE k OF
		-1: RETURN, structure ; tag does not exist in structure
		 0: BEGIN ; tag to be deleted is 1. element of structure
				new_str = CREATE_STRUCT(tn[1], structure.(1), NAME = nm)
				IF n GT 2 THEN FOR i = 2L, n - 1 DO $
					new_str = CREATE_STRUCT(new_str, tn[i], structure.(i), NAME = nm)
			END
		ELSE: BEGIN
			new_str = CREATE_STRUCT(tn[0], structure.(0), NAME = nm)
			IF n GT 2 THEN $
				FOR i = 1L, n - 1 DO $
					IF i NE k THEN $
						new_str = CREATE_STRUCT(new_str, tn[i], structure.(i), NAME = nm)
		 	END
	ENDCASE

	RETURN, new_str
END
