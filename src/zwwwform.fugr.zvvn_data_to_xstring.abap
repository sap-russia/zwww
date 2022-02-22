FUNCTION ZVVN_DATA_TO_XSTRING.
*"----------------------------------------------------------------------
*"*"######### #########:
*"  IMPORTING
*"     REFERENCE(DATA_LENGTH) TYPE  I
*"  EXPORTING
*"     REFERENCE(DATA_XSTRING) TYPE  XSTRING
*"  TABLES
*"      IT_DATA
*"----------------------------------------------------------------------


Perform TableToXString
  tables IT_DATA
  using DATA_LENGTH
        'BIN'
        ''
  changing DATA_XSTRING.




ENDFUNCTION.
