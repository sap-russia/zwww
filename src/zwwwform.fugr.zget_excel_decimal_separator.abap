FUNCTION ZGET_EXCEL_DECIMAL_SEPARATOR.
*"----------------------------------------------------------------------
*"*"######### #########:
*"  EXPORTING
*"     REFERENCE(SEPARATOR)
*"----------------------------------------------------------------------

*  Perform AssignCharX.

  PERFORM GET_EXCEL_DECIMAL_SEPARATOR
    USING SEPARATOR.
ENDFUNCTION.
