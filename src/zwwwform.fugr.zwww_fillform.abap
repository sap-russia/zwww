FUNCTION ZWWW_FILLFORM.
*"----------------------------------------------------------------------
*"*"######### #########:
*"  IMPORTING
*"     REFERENCE(DOC) TYPE  OLE2_OBJECT
*"     VALUE(OPTIMIZE) TYPE  N DEFAULT 100
*"     VALUE(FILE_TYPE) TYPE  CHAR50
*"     VALUE(FILE_INDEX) TYPE  STRING OPTIONAL
*"     VALUE(DEBUG_MODE) TYPE  C DEFAULT ''
*"     VALUE(WITHOUT_OLE) TYPE  C DEFAULT ''
*"     VALUE(USE_JAR) TYPE  C DEFAULT ''
*"     VALUE(FILE_NAME) OPTIONAL
*"     VALUE(FILE_LOCATION) TYPE  C DEFAULT ''
*"     VALUE(FILE_TEMP) OPTIONAL
*"     VALUE(PRINTDIALOG) TYPE  C DEFAULT ''
*"     VALUE(PROTECT) TYPE  C DEFAULT ''
*"     VALUE(DECIMAL_SEPARATOR) OPTIONAL
*"     VALUE(CLOSE_FORM) TYPE  C DEFAULT ''
*"     VALUE(DELETE_FILE) TYPE  C DEFAULT ''
*"  TABLES
*"      IT_VALUES TYPE  ZWWW_VALUES_T OPTIONAL
*"  CHANGING
*"     REFERENCE(FILE_TEXT) TYPE  STRING OPTIONAL
*"----------------------------------------------------------------------

  Field-symbols:
    <IT_VALUES> type ZWWW_VALUES,
    <it_SortVal> type tt_Sort_Val.

  Data:
    Cnt type i,
*    CodePage type TCP00-CPCODEPAGE value space,
    VarNumChr(10).

*  Perform AssignCharX.

  If IT_VALUES is requested.
    Assign IT_VALUES[] to <it_SortVal>.

    If FILE_TYPE <> 'MSPROJECT'.
      Perform SortTableValues
        tables <it_SortVal>.
    EndIf.
  EndIf.

*#################, #### ######## # ##### STRING
*  OPTIMIZE = 0. "### #### STRING

  Describe table IT_VALUES lines Cnt.

  If DEBUG_MODE = 'X'.
    Break-point.
  EndIf.

  Case FILE_TYPE.
    when 'MSPROJECT'.
      Perform FillMSProjectForm
        using <it_SortVal>
              Doc
              DEBUG_MODE.

    when 'EXCEL'.
      If Cnt >= Optimize.
        Perform Run_Excel_Macros
          using <it_SortVal>
                Doc
                FILE_TYPE
                FILE_INDEX
                DEBUG_MODE
                WITHOUT_OLE
                USE_JAR
                FILE_NAME
                FILE_LOCATION
                FILE_TEMP
                PRINTDIALOG
                PROTECT
                DECIMAL_SEPARATOR
                CLOSE_FORM
                DELETE_FILE.

*                FrontendCodepage.
      Else.
        Perform FillExcelForm
          using <it_SortVal>
                Doc
                DEBUG_MODE
                FILE_LOCATION
                USE_JAR.
*                FrontendCodepage.
      EndIf.

    when 'WORD'.
      If Cnt >= Optimize.
        Perform Run_Word_Macros
          using <it_SortVal>
                Doc
                FILE_TYPE
                FILE_INDEX
                DEBUG_MODE
                WITHOUT_OLE
                USE_JAR
                FILE_NAME
                FILE_LOCATION
                FILE_TEMP
                PRINTDIALOG
                PROTECT
                DECIMAL_SEPARATOR
                CLOSE_FORM
                DELETE_FILE.
*                FrontendCodepage.
      Else.
        Perform FillWordForm
          using <it_SortVal>
                Doc
                DEBUG_MODE.
*                FrontendCodepage.
      EndIf.

    when 'RTF'.
      Perform Fill_RTF_Form                                 "#EC V#700
        using <it_SortVal>                                  "#EC V#700
              Doc                                           "#EC V#700
              DEBUG_MODE                                    "#EC V#700
              FILE_TEXT.                                    "#EC V#700
  EndCase.

ENDFUNCTION.
