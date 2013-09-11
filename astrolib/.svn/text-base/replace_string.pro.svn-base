;
; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; NAME:
;  replace_string
;
; PURPOSE:
;   This function replaces in a given string or vector of strings all values by an other. Length or only one char didn't matter.
;   It could also be used to delete a substring from a string.
;
; CATEGORY:
;   PROG_TOOLS/STRINGS
;
; CALLING SEQUENCE:
;   Result=replace_string(text,in_string,rep_string,[no_of_replaces=no_of_replaces],[pos=pos],[count=count])
;
; INPUTS:
;   text:       the text where to replace some informations
;   in_string:  the search string
;   rep_string: the string which should replace in_string
;
; KEYWORD PARAMETERS:
;   no_of_replace: if set two a number, this means in text as many times of no_of_replace in_string is reaplced by rep_string
;   pos:           if set to a number the replacement starts at this string position
;   count:         this argument returns the number of replaces
;
; OUTPUTS:
;   Result is the new text
;
; EXAMPLE:
;   help,replace_string('Dies ist ein Test',' ','_')
;   <Expression>    STRING    = 'Dies_ist_ein_Test'
;   help,replace_string('Dies ist ein Test',' ','_',pos=5)
;   <Expression>    STRING    = 'Dies ist_ein_Test'
;   help,replace_string('Dies ist ein Test',' ','_',pos=5,no=1)
;   <Expression>    STRING    = 'Dies ist_ein Test'
;   help,replace_string('Dies ist ein Test','ist','ist')
;   <Expression>    STRING    = 'Dies ist ein Test'
;   help,replace_string('Dies ist ein Test, ist ein','ist','ist nicht')
;   <Expression>    STRING    = 'Dies ist nicht ein Test, ist nicht ein'
;   help,replace_string('\\\\\\\\\','\','/')
;   <Expression>    STRING    = '/////////'
;   help,replace_string('["..\idl_html\idl_work_cat.htm"]','cat','cat_org')
;   <Expression>    STRING    = '["..\idl_html\idl_work_cat_org.htm"]'
;   print,replace_string(['12:33:00','12:33:00','12:33:00'],':','')
;   123300 123300 123300
;   print,replace_string(['12:33:00','12:33:00','12:33:00'],':','',pos=5)
;   12:3300 12:3300 12:3300
;   print,replace_string( 'asdf___ertz_j','__', '')
;   asdf_ertz_j
;   print,replace_string(['12:33:00','12:33:00','12:33:00'],':','',pos=5,count=c),c
;   12:3300 12:3300 12:3300
;   3
;   print,replace_string(['12:33:00','12:33:00','12:33:00'],':','',count=c),c
;   123300 123300 123300
;   6
;
;
; MODIFICATION HISTORY:
; 	Written by:	R.Bauer (ICG-1) , 1998-Sep-06
;   1998-09-26 bug removed with start_pos and a vector of strings
;   1998-09-26 special replacement include if a sign should be replaced by an other by n times
;   1999-09-07 bug removed with replacing '___' by '_'
;   1999-10-01 count added
;   2000-03-08 bug with no_of_replaces removed
;              if text is an array no_of_replaces is used for each element
;   2001-02-13 Loop in LONG
;-

FUNCTION replace_string,text,in_string,rep_string,pos=pos,no_of_replaces=no_of_replaces,count=count_n_replace

   IF N_PARAMS() LT 3 THEN BEGIN

      MESSAGE,call_help(),/cont
      RETURN,''
   ENDIF
   counter=0
   count_n_replace=0

   IF N_ELEMENTS(no_of_replaces) GT 0 THEN number=no_of_replaces ELSE number=1E+30

   length_in_string=STRLEN(in_string)
   length_rep_string=STRLEN(rep_string)

; Sonderfall, wenn genau 1 Zeichen der Lanege 1 durch 1 anderes Zeichen der Laenge 1 und n mal ersetzt werden soll
; Dieses Verfahren ist einfach schneller
   IF length_rep_string NE 0 THEN BEGIN
      IF length_in_string + length_rep_string EQ 2 AND number EQ 1E+30 THEN BEGIN
         new_text=BYTE(text)
         IF N_ELEMENTS(pos) EQ 0 THEN BEGIN
            change=WHERE(new_text EQ ((BYTE(in_string))[0]),count_change)
            IF count_change GT 0 THEN new_text[change]=(BYTE(rep_string))[0]
            ENDIF ELSE BEGIN
            change=WHERE(new_text[pos:*] EQ ((BYTE(in_string))[0]),count_change)
            IF count_change GT 0 THEN new_text[pos+change]=(BYTE(rep_string))[0]
         ENDELSE
         count_n_replace=count_change
         RETURN,STRING(new_text)
      ENDIF
   ENDIF

; alle anderen Faelle werden so behandelt
   IF N_ELEMENTS(pos) EQ 0 THEN start_pos=0 ELSE start_pos=pos

   n_text=N_ELEMENTS(text)-1

   FOR i=0L,n_text DO BEGIN
   counter=0
      new_text=text[i]
      IF STRPOS(new_text,in_string) NE -1 AND in_string NE rep_string THEN  BEGIN
         pos_in_string=1
         text_length=STRLEN(new_text)
         WHILE pos_in_string NE -1 AND counter LT number DO BEGIN
            pos_in_string=STRPOS(new_text,in_string,start_pos)
            IF pos_in_string GT -1 THEN BEGIN
               count_n_replace=count_n_replace+1
               new_text=STRMID(new_text,0,pos_in_string)+rep_string+STRMID(new_text,pos_in_string+length_in_string,text_length)
               start_pos=pos_in_string+length_rep_string
            ENDIF
            counter=counter+1
         ENDWHILE
         if n_elements(result) eq 0 then result=new_text else result=[result,new_text]
         IF N_ELEMENTS(pos) EQ 0 THEN start_pos=0 ELSE start_pos=pos
         ENDIF ELSE BEGIN
         if n_elements(result) eq 0 then result=new_text else result=[result,new_text]
         IF N_ELEMENTS(pos) EQ 0 THEN start_pos=0 ELSE start_pos=pos
      ENDELSE

   ENDFOR
   RETURN,result

END
