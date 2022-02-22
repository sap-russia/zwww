FUNCTION ZLCL_OPENFORM.
*"----------------------------------------------------------------------
*"*"######### #########:
*"  IMPORTING
*"     VALUE(SOURCE_NAME) TYPE  RLGRAP-FILENAME
*"     VALUE(FILE_NAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(FILE_LOCATION) TYPE  C DEFAULT ''
*"     VALUE(PRINTDIALOG) TYPE  C DEFAULT ''
*"     VALUE(PROTECT) TYPE  C DEFAULT 'X'
*"     VALUE(OPTIMIZE) TYPE  N DEFAULT 100
*"     VALUE(DEBUG_MODE) TYPE  C DEFAULT ''
*"     VALUE(DECIMAL_SEPARATOR) TYPE  C DEFAULT '.'
*"     VALUE(CLOSE_FORM) TYPE  C DEFAULT ''
*"     VALUE(DELETE_FILE) TYPE  C DEFAULT 'X'
*"     VALUE(WITHOUT_OLE) TYPE  C DEFAULT ''
*"     VALUE(USE_UNICODE) TYPE  C DEFAULT ''
*"  TABLES
*"      IT_VALUES TYPE  ZWWW_VALUES_T
*"      IT_FILES TYPE  ZWWW_FILES_T OPTIONAL
*"  EXCEPTIONS
*"      PRINTCANCEL
*"----------------------------------------------------------------------

  Data:
    return_code type i,
    FileType type t_FileType,
    sy_subrc type sy-subrc,
    File_Text type string,
    FileIndex type string,
    FILE_TEMP type string.
*    CodePage type TCP00-CPCODEPAGE value space.

  UseUnicode = USE_UNICODE.
  Perform GetCodepage                                       "#EC V#700
    using UseUnicode                                        "#EC V#700
    changing FrontendCodepage.                              "#EC V#700

  Perform DownloadLCLTemplate
    using
      SOURCE_NAME
*      FILE_NAME
      FILE_LOCATION
      FileType
      FileIndex
      File_Text
      sy_subrc
    changing
      FILE_TEMP.

  Check sy_subrc = 0.

  Perform OpenForm
    using FILE_NAME
          FILE_LOCATION
          FILE_TEMP
          FileType
          FileIndex
          File_Text
          IT_VALUES[]
          IT_FILES[]
          OPTIMIZE
          PROTECT
          PRINTDIALOG
          DEBUG_MODE
          DECIMAL_SEPARATOR
          CLOSE_FORM
          DELETE_FILE
          WITHOUT_OLE
          '' "USE_JAR
*          CODEPAGE
          return_code.

  Case return_code.
    when 1.
      Raise PrintCancel.
  EndCase.
ENDFUNCTION.
