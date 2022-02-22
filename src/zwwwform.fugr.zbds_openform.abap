FUNCTION ZBDS_OPENFORM.
*"----------------------------------------------------------------------
*"*"######### #########:
*"  IMPORTING
*"     REFERENCE(DESCRIPTION) TYPE  C
*"     REFERENCE(DOC_CLASSNAME) TYPE  SBDST_CLASSNAME DEFAULT
*"       'SOFFICEINTEGRATION'
*"     REFERENCE(DOC_CLASSTYPE) TYPE  SBDST_CLASSTYPE DEFAULT 'OT'
*"     REFERENCE(DOC_OBJECT) TYPE  SBDST_OBJECT_KEY DEFAULT
*"       'SOFFICEINTEGRATION'
*"     VALUE(FILE_NAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(FILE_LOCATION) TYPE  C DEFAULT ''
*"     VALUE(OPTIMIZE) TYPE  N DEFAULT 100
*"     VALUE(DECIMAL_SEPARATOR) TYPE  C DEFAULT '.'
*"     VALUE(PRINTDIALOG) TYPE  C DEFAULT ''
*"     VALUE(PROTECT) TYPE  C DEFAULT 'X'
*"     VALUE(DEBUG_MODE) TYPE  C DEFAULT ''
*"     VALUE(CLOSE_FORM) TYPE  C DEFAULT ''
*"     VALUE(DELETE_FILE) TYPE  C DEFAULT 'X'
*"     VALUE(WITHOUT_OLE) TYPE  C DEFAULT ''
*"     VALUE(USE_JAR) TYPE  C DEFAULT ''
*"     VALUE(USE_UNICODE) TYPE  C DEFAULT ''
*"  TABLES
*"      IT_VALUES TYPE  ZWWW_VALUES_T OPTIONAL
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

*  Perform AssignCharX.

  Perform InitParams
    changing
      FILE_LOCATION
      OPTIMIZE
      WITHOUT_OLE
      USE_JAR
      USE_UNICODE
      CLOSE_FORM.

  Perform DownloadBDSTemplate
    using
      DESCRIPTION
      DOC_CLASSNAME
      DOC_CLASSTYPE
      DOC_OBJECT
*     FILE_NAME
      FILE_LOCATION
      FileType
      FileIndex
      USE_JAR
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
          USE_JAR
*          CODEPAGE
          return_code.

  Case return_code.
    when 1.
      Raise PrintCancel.
  EndCase.
ENDFUNCTION.
