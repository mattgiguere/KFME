; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	add_tag2
;
; PURPOSE:
;	This function adds a tag to a given structure.
;	Both, structure and values can be arrays.
;
; CATEGORY:
;	PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;	x = add_tag2(str, tag, value [, status])
;
; INPUTS:
;	str: variable name of structure or array of structures
;	tag: name of tag to be added (STRING)
;		 if tag already exists in str its deleted
;	value: value of tag (scalar or array)
;
; KEYWORD PARAMETERS:
;	/EXPAND_VALUES: IF set, the last dimension of value is expanded along the dimension of str
;	/FIRST:	The new tag is inserted at the beginning of str
;	NAME: A unique name for the new structure.
;
; OPTIONAL OUTPUTS:
;	status: Status information 	=0: every thing worked fine;
;								=1: something went wrong (see messages on screen)
;
; SIDE EFFECTS:
;	If an array of anonymous structures is passed into the routine an array
;	of named structures is returned.
;
; EXAMPLE:
;	as = {Bsp1, a:0, b:'asd'}
;	bs = add_tag2(as, 'c', 0.46D00, /FIRST, NAME = 'Bsp2')
;	HELP, bs, /STR :
;		** Structure BSP2, 3 tags, length=24:
;		   C               DOUBLE          0.46000000
;		   A               INT              0
;		   B               STRING    'asd'
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 09. Mar. 98.
;	This program is based on the procedure add_tag.pro but provides additional features
;	27.05.98 - Type of loop variable in FOR loop changed to LONG
;	15.11.98:	call to debug deleted
;	28.04.1999:	tag_defined exchanged by is_tag
;	22.09.1999:	Random structure name will only be generated if a tag is added to an
;					array of structures.
;-

FUNCTION add_tag2, str, tag, value, status, EXPAND_VALUES = ev, FIRST = fir, NAME = nm

	ON_ERROR, 2
	status = 0

	; ***** Test if passed parameters are ok *****

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
	IF NOT is_defined(value) THEN BEGIN
		MESSAGE, 'Returned unchanged structure since parameter <value> is not defined.', /INFORMAT
		MESSAGE, '<status> was set to 1', /INFORMAT
		status = 1
		RETURN, str
	ENDIF

	; ***** Set defaults *****

	IF N_ELEMENTS(fir) EQ 0 THEN fir = 0 ; Set default to append tag to structure
	IF N_ELEMENTS(nm) EQ 0 THEN nm = ''

	; ***** Handle array of structures *****

	n = N_ELEMENTS(str)
	IF n GT 1 THEN BEGIN
		IF nm EQ '' THEN nm = cr_struct_name() ; set default name
		dim = (SIZE(value))[0] ; dimension of value
		nv = (SIZE(value))[dim] ; Number of elements in last dimension

		IF KEYWORD_SET(ev) THEN BEGIN
			IF ((dim GT 0) AND (nv NE n)) THEN BEGIN
				MESSAGE, 'You have set the keyword EXPAND_VALUES. Therefore the parameter <value>', /INFORMAT
				MESSAGE, 'must either be passed as a scalar or as an array with its last dimension eq dim(str)', /INFORMAT
				MESSAGE, 'Returned unchanged structure; <status> was set to 1', /INFORMAT
				status = 1
				RETURN, str
			ENDIF
			CASE dim OF
				0: new_str0 = add_tag2(str[0], tag, value, status, FIRST = fir, NAME = nm)
				1: new_str0 = add_tag2(str[0], tag, value[0], status, FIRST = fir, NAME = nm)
				2: new_str0 = add_tag2(str[0], tag, value[*, 0], status, FIRST = fir, NAME = nm)
				3: new_str0 = add_tag2(str[0], tag, value[*, *, 0], status, FIRST = fir, NAME = nm)
				ELSE: BEGIN
					MESSAGE, 'Dim(value) > 3 not implemented in Program.', /INFORMAT
					MESSAGE, 'Returned unchanged structure; <status> was set to 1.', /INFORMAT
					status = 1
					RETURN, str
				END
			ENDCASE
		ENDIF ELSE new_str0 = add_tag2(str[0], tag, value, status, FIRST = fir, NAME = nm)
		new_str = REPLICATE(new_str0, n)

		IF KEYWORD_SET(ev) THEN BEGIN
			CASE dim OF
				0: FOR i = 1L, n - 1 DO new_str[i] = add_tag2(str[i], tag, value, status, FIRST = fir, NAME = nm)
				1: FOR i = 1L, n - 1 DO new_str[i] = add_tag2(str[i], tag, value[i], status, FIRST = fir, NAME = nm)
				2: FOR i = 1L, n - 1 DO new_str[i] = add_tag2(str[i], tag, value[*, i], status, FIRST = fir, NAME = nm)
				3: FOR i = 1L, n - 1 DO new_str[i] = add_tag2(str[i], tag, value[*, *, i], status, FIRST = fir, NAME = nm)
			ENDCASE
		ENDIF ELSE FOR i = 1L, n - 1 DO new_str[i] = add_tag2(str[i], tag, value, status, FIRST = fir, NAME = nm)

		RETURN, new_str
	ENDIF

	; ***** If tag is already defined in str then its deleted *****

	IF is_tag(str, tag) THEN new_str = del_tag2(str, tag) ELSE new_str = str

	; ***** Build new structure *****

	IF is_structure(new_str) THEN $ ; del_tag returns -1 if tag was only TAG in structure str
		IF KEYWORD_SET(fir) THEN $
			new_str = CREATE_STRUCT(tag, value, new_str, NAME = nm) $ ; Append str to new definition of tag
			ELSE new_str = CREATE_STRUCT(new_str, tag, value, NAME = nm) $ ; Append new definition of tag to str
		ELSE new_str = CREATE_STRUCT(tag, value, NAME = nm) ; Create new structure with one tag

	RETURN, new_str
END
