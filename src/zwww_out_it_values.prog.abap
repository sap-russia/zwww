*&---------------------------------------------------------------------*
*& Report  ZWWW_OUT_IT_VALUES
*&
*&---------------------------------------------------------------------*

REPORT  ZWWW_OUT_IT_VALUES.
* ##### # ###### ## ######### ###### ## ########## ##### it_values

Constants:
  c_ext_temp(30) value '*.xls?;*.doc?',
  c_ext_text(30) value '*.txt'.

Selection-screen begin of screen 1001 as subscreen.
Parameters:
  p_XLS type RLGRAP-FILENAME default c_ext_temp.
Selection-screen end of screen 1001.

Selection-screen begin of screen 1002 as subscreen.
Parameters:
  p_WWW type WWWDATATAB-OBJID.
Selection-screen end of screen 1002.

Selection-screen begin of screen 1003 as subscreen.
Parameters:
  p_BDS    type BAPISIGNAT-PROP_VALUE lower case,
  p_CLNAME type BAPIBDS01-CLASSNAME default 'SOFFICEINTEGRATION',
  p_CLTYPE type BAPIBDS01-CLASSTYPE default 'OT',
  p_OBJ    type BAPIBDS01-OBJKEY default 'SOFFICEINTEGRATION'.
Selection-screen end of screen 1003.

Selection-screen:
  begin of tabbed block TAB_BLOCK for 4 lines,
    tab (20) button1 user-command push1 default screen 1001,
    tab (20) button2 user-command push2 default screen 1002,
    tab (20) button3 user-command push3 default screen 1003,
  end   of block TAB_BLOCK.

Selection-screen begin of block BlTxt with frame title TEXT-BTX.
Parameters:
  p_TB1 radiobutton group gTbl default 'X' user-command ChgTbl,
  p_TB2 radiobutton group gTbl,
  p_TXT type RLGRAP-FILENAME default c_ext_text.
Parameters:
  p_VNM type ZWWW_VALUES-VAR_NAME  modif id TB2,
  p_FTX type ZWWW_VALUES-FIND_TEXT modif id TB2,
  p_VTP type ZWWW_VALUES-VAL_TYPE  modif id TB2 default 'T'.

Parameters:
  p_ROW type i default 1,
  p_COL type i default 1.
Selection-screen end of block BlTxt.

Parameters:
  p_TRG type RLGRAP-FILENAME default '',
  p_FLC type ZWWW_FILE_LOCATION value check default '',
  p_PRINT as checkbox default '',
  p_PRTCT as checkbox default '',
  p_OPTIM(9) type N default 0,
  p_CLOSE as checkbox default '',
  p_DELET as checkbox default 'X',
  p_DEBUG as checkbox default '',
  p_WHOLE as checkbox default ''.

Class ZCL_ZWWW definition load. "### ############# # 4.6C

Data:
  it_Val type standard table of ZWWW_VALUES with header line,
  it_Str type standard table of STRING with header line,
  it_Row type standard table of STRING with header line,
  NumRow type i,
  w_Str type string.

Field-symbols:
  <fld>.

Initialization.
  button1 = '###### # ##########'.
  button2 = '######### WWW (smw0)'.
  button3 = '######### BDS (oaor)'.

At Selection-Screen on value-request for p_TXT.
  Perform F4_FlName using 'P_TXT' c_ext_text.

At Selection-Screen on value-request for p_XLS.
  Perform F4_FlName using 'P_XLS' c_ext_temp.

At Selection-Screen output.
  Loop at Screen.
    Case Screen-Group1.
      when 'TB2'.
        If p_TB2 is initial.
          Screen-Active = 0.
        Else.
          Screen-Active = 1.
        EndIf.
        Modify Screen.
    EndCase.
  EndLoop.

End-of-Selection.

  Data:
    FileTxt type string,
    FlTxtCh type RLGRAP-FILENAME,
    Dir     type string,
    DirChar type char255,
    Filter  type string,
    FiltChar type char255,
    i type i,
    it_Files type standard table of FILE_TABLE "FILE_INFO
      with header line,
    Cnt type i,
    ValueStr type string.
  Field-symbols:
    <dir>,
    <filter>,
    <FileTxt>.


  If sy-saprl >= '600'.
    Assign Dir to <dir>.
    Assign Filter to <filter>.
    Assign FileTxt to <FileTxt>.
  Else.
    Assign DirChar to <dir>.
    Assign FiltChar to <filter>.
    Assign FlTxtCh to <FileTxt>.
  EndIf.

  If not p_TXT is initial.
    <FileTxt> = p_TXT.

    Perform SPLIT_FILE_AND_PATH
      using p_TXT
      changing <filter> <dir>.

    Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
      EXPORTING
        DIRECTORY  = <dir>
        FILTER     = <filter>
        FILES_ONLY = 'X'
      CHANGING
        FILE_TABLE = it_Files[]
        COUNT      = Cnt                                    "#EC V#600
*#EC V<600      FILE_COUNT = Cnt
      EXCEPTIONS
        others     = 99.


    Loop at it_Files.

      Concatenate <dir> it_Files-FILENAME into <FileTxt>.

      Refresh: it_Str, it_Val.

      Call function 'GUI_UPLOAD'
        EXPORTING
          FILENAME = <FileTxt>
        TABLES
          DATA_TAB = it_Str.

      Clear ValueStr.

      Loop at it_Str from p_ROW.
        Clear it_Val.
        w_Str = it_Str.
        Refresh it_Row.

        i = p_Col.
        While i > 1.
          Split w_Str at zcl_zwww=>char_tab into w_Str w_Str.
          i = i - 1.
        EndWhile.

        Case 'X'.
          when p_TB1.
            Split w_Str at zcl_zwww=>char_tab
              into it_Val-VAR_NAME
                   it_Val-VAR_NUM
                   it_Val-FIND_TEXT
                   it_Val-VAL_TYPE
                   it_Val-VALUE.

            Condense it_Val-VALUE.
            Append it_Val.

          when p_TB2.
            If not ValueStr is initial.
              Concatenate ValueStr
                          zcl_zwww=>char_0D
                          zcl_zwww=>char_0A
                          w_Str
                into ValueStr.
            Else.
              ValueStr = w_Str.
            EndIf.
        EndCase.
      EndLoop.

      If not ValueStr is initial.
        Clear it_Val.
        it_Val-VAR_NAME = p_VNM.
        it_Val-VAR_NUM = 1.
        it_Val-FIND_TEXT = p_FTX.
        it_Val-VAL_TYPE = p_VTP.
        it_Val-VALUE = ValueStr.
        Append it_Val.
      EndIf.

      Perform OpenForm.
    EndLoop.
  Else.
    Perform OpenForm.
  EndIf.
*&---------------------------------------------------------------------*
*&      Form  OpenForm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
Form OpenForm.
  Case TAB_BLOCK-ACTIVETAB.
    when 'PUSH1'.
      Call function 'ZLCL_OPENFORM'
        EXPORTING
          SOURCE_NAME   = p_XLS
          FILE_NAME     = p_TRG
          FILE_LOCATION = p_FLC
          PRINTDIALOG   = p_PRINT
          PROTECT       = p_PRTCT
          OPTIMIZE      = p_OPTIM
          CLOSE_FORM    = p_CLOSE
          DELETE_FILE   = p_DELET
          DEBUG_MODE    = p_DEBUG
          WITHOUT_OLE   = p_WHOLE
        TABLES
          IT_VALUES     = it_Val.

    when 'PUSH2'.
      Call function 'ZWWW_OPENFORM'
        EXPORTING
          FORM_NAME     = p_WWW
          FILE_NAME     = p_TRG
          FILE_LOCATION = p_FLC
          PRINTDIALOG   = p_PRINT
          PROTECT       = p_PRTCT
          OPTIMIZE      = p_OPTIM
          CLOSE_FORM    = p_CLOSE
          DELETE_FILE   = p_DELET
          DEBUG_MODE    = p_DEBUG
          WITHOUT_OLE   = p_WHOLE
        TABLES
          IT_VALUES     = it_Val.

    when 'PUSH3'.
      Call function 'ZBDS_OPENFORM'
        EXPORTING
          DESCRIPTION   = p_BDS
          DOC_CLASSNAME = p_CLNAME
          DOC_CLASSTYPE = p_CLTYPE
          DOC_OBJECT    = p_OBJ
          FILE_NAME     = p_TRG
          FILE_LOCATION = p_FLC
          PRINTDIALOG   = p_PRINT
          PROTECT       = p_PRTCT
          OPTIMIZE      = p_OPTIM
          CLOSE_FORM    = p_CLOSE
          DELETE_FILE   = p_DELET
          DEBUG_MODE    = p_DEBUG
          WITHOUT_OLE   = p_WHOLE
        TABLES
          IT_VALUES     = it_Val.
  EndCase.
EndForm.                    "OpenForm
*---------------------------------------------------------------------*
**       FORM F4_FlName                                                *
*---------------------------------------------------------------------*
FORM F4_FlName
  using P_FLNAME type CHAR30
        p_Ext    type CHAR30.

  Data:
    it_File_Table type standard table of SDOKPATH
      with header line,
    w_FullName type RLGRAP-FILENAME,
    w_Path     type RLGRAP-FILENAME,
    w_Name     type RLGRAP-FILENAME,
    it_DYNPREAD type standard table of DYNPREAD
      with header line,
    sy_repid  type sy-repid.

  Field-symbols:
    <FldParam>.

  Assign (P_FLNAME) to <FldParam>.

  sy_repid = sy-repid.
  Refresh it_DYNPREAD. Clear it_DYNPREAD.
  it_DYNPREAD-FIELDNAME = P_FLNAME.
  Append it_DYNPREAD.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = sy_repid
      DYNUMB     = sy-dynnr
    TABLES
      DYNPFIELDS = it_DYNPREAD
    EXCEPTIONS
      OTHERS     = 11.

  Read table it_DYNPREAD index 1.
  w_FullName = it_DYNPREAD-FIELDVALUE.

  Perform SPLIT_FILE_AND_PATH
    using w_FullName
    changing w_Name w_Path.
*  Call function 'SO_SPLIT_FILE_AND_PATH'
*    EXPORTING
*      FULL_NAME     = w_FullName
*    IMPORTING
*      FILE_PATH     = w_Path
*      STRIPPED_NAME = w_Name
*    EXCEPTIONS
*      others        = 1.

"  Concatenate '*.' p_Ext into w_Name.
  w_Name = p_Ext .

  Call function 'TMP_GUI_FILE_OPEN_DIALOG'
    EXPORTING
      INIT_DIRECTORY    = w_Path
      DEFAULT_FILENAME  = w_Name
*      DEFAULT_EXTENSION = p_Ext
*      FILE_FILTER       = w_Name
    TABLES
      FILE_TABLE        = it_File_Table.
  Read table it_File_Table index 1.
  If sy-subrc = 0.
    <FldParam> = it_File_Table-PATHNAME.
  EndIf.
ENDFORM.                                                    " F4_FlName

*&---------------------------------------------------------------------*
*&      Form  Split_File_and_Path
*&---------------------------------------------------------------------*
Form Split_File_and_Path
  using value(FULL_NAME)
  changing STRIPPED_NAME
           FILE_PATH.

*& dirlen : directoy lange
  DATA dirlen  LIKE sy-fdpos.

*& get the   dirlen
  dirlen = strlen( full_name ) - 1.

  WHILE dirlen > 0.
    IF full_name+dirlen(1) = '\'
       OR full_name+dirlen(1) = '/'. "dirlen is the part before '/' or '\'
      EXIT.
    ENDIF.
    dirlen = dirlen - 1.
  ENDWHILE.

*& get directory and filename .
  IF dirlen > 0.
*& path include '/' or '\'
    dirlen = dirlen + 1.
    file_path     = full_name(dirlen). "no offset: first part.

*&  filename
    stripped_name = full_name+dirlen. "offset without lange: rest
  ELSE.
    stripped_name = full_name.
  ENDIF.
EndForm.                    "Split_File_and_Path
