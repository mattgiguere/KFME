	FUNCTION LIST_WITH_PATH, FILENAME, PATHS, NOCURRENT=NOCURRENT, $
		COUNT = COUNT
;+
; NAME: 
;	LIST_WITH_PATH
; PURPOSE: 
;	Search for files in a specified directory path.
; EXPLANATION:
;	Lists files in a set of default paths, similar to using FINDFILE,
;	except that a list of paths to be searched can be given.
;
; CALLING SEQUENCE: 
;	Result = LIST_WITH_PATH( FILENAME, PATHS )
;
; INPUTS: 
;	FILENAME   = Name of file to be searched for.  It may contain wildcard
;		     characters, e.g. "*.dat".
;
;	PATHS	   = One or more default paths to use in the search in case
;		     FILENAME does not contain a path itself.  The individual
;		     paths are separated by commas, although in UNIX, colons
;		     can also be used.  In other words, PATHS has the same
;		     format as !PATH, except that commas can be used as a
;		     separator regardless of operating system.  The current
;		     directory is always searched first, unless the keyword
;		     NOCURRENT is set.
;
;		     A leading $ can be used in any path to signal that what
;		     follows is an environmental variable, but the $ is not
;		     necessary.  (In VMS the $ can either be part of the path,
;		     or can signal logical names for compatibility with Unix.)
;		     Environmental variables can themselves contain multiple
;		     paths.
;
; OUTPUTS: 
;	The result of the function is a list of filenames.
; EXAMPLE:
;	FILENAME = ''
;	READ, 'File to open: ', FILENAME
;	FILE = LIST_WITH_PATH( FILENAME, 'SERTS_DATA', '.fix' )
;	IF FILE NE '' THEN ...
; PROCEDURE CALLS: 
;	BREAK_PATH, CONCAT_DIR
; Category    : 
;	Utilities, Operating_system
; REVISION HISTORY:
;	Version 1, William Thompson, GSFC, 3 November 1994
;	Documentation modified Wayne Landsman HSTX  November 1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR, 2
;
;  Check the number of parameters:
;
	IF N_PARAMS() NE 2 THEN MESSAGE, 'Syntax:  Result = ' + $
		'LIST_WITH_PATH(FILENAME, PATHS)'
;
;  Reformat PATHS into an array.  The first element is the null string.
;
	PATH = BREAK_PATH(PATHS)
;
;  If NOCURRENT was set, then remove the first (blank) entry from the PATH
;  array.
;
	IF KEYWORD_SET(NOCURRENT) THEN PATH = PATH[1:*]
;
;  Step through each of the paths.
;
	FILES = FINDFILE( CONCAT_DIR( PATH[0], FILENAME ))
	FOR I = 1,N_ELEMENTS(PATH)-1 DO		$
		FILES = [FILES, FINDFILE( CONCAT_DIR( PATH[I], FILENAME ))]
;
;  Remove any null strings.  If they're all nulls, then return a single null
;  string.  At the same time, get the COUNT parameter.
;
	W = WHERE(FILES NE '', COUNT)
	IF COUNT EQ 0 THEN FILES = '' ELSE FILES = FILES[W]
;
	RETURN, FILES
	END
