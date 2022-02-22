*&---------------------------------------------------------------------*
*& Report  ZWWW_TEST
*&---------------------------------------------------------------------*
REPORT  ZWWW_MANY_LIST.

Selection-screen begin of block bl1 with frame.
Parameters:
  EXCEL   radiobutton group gr0 default 'X',
  EXCEL_2 radiobutton group gr0,
  WORD    radiobutton group gr0.
Parameters:                                                 "#EC V#700
  RTF     radiobutton group gr0.                            "#EC V#700
Selection-screen end of block bl1.

Parameters:
*"""""""""""""""""""""""""""""""""""""""""""""" ####### ## #####:
  p_ADDRNM radiobutton group gr1 default 'X', "  ## #######
  p_DEPART radiobutton group gr1,             "  ## #######
  p_FUNCTN radiobutton group gr1.             "  ## ##########

Parameters:
  p_ROWS type i default 1000.               " ####. ###-## #######

Parameters:
  p_VVN    as checkbox,
  p_Protct as checkbox modif id VVN,
  p_Close  as checkbox modif id VVN,
  p_Optim(7)  type N default 100 modif id VVN,
  p_Debug  as checkbox modif id VVN,
  p_Wh_OLE as checkbox modif id VVN,
  p_UseJAR as checkbox modif id VVN,
  p_Copies(7) type N default 1 modif id VVN.

Types:
  Begin of t_GraphData,
    Name(40),
    Count type i,
  End of t_GraphData.

Data:
  NameTemplate type WWWDATATAB-OBJID,
  Begin of it_Usr occurs 10,
    NAME1      type ADRC-NAME1,
    CITY1      type ADRC-CITY1,
    ADDRNUMBER type ADRC-ADDRNUMBER,
    DEPARTMENT type ADCP-DEPARTMENT,
    FUNCTION   type ADCP-FUNCTION,
    BNAME type USR21-BNAME,
    NAME_LAST  type ADRP-NAME_LAST,
    Name_FIRST type ADRP-Name_FIRST,
  End of it_Usr,

  Num type i,
  NumList(3),
  NameList type ZWWW_VALUES-VAR_NAME,
  it_Val  type standard table of ZWWW_VALUES with header line,
  new_ADDRNUMBER,
  new_DEPARTMENT,
  new_FUNCTION,
  it_GraphData type sorted table of t_GraphData
    with unique key Name
    with header line,
  it_PhotoNames type standard table of WWWPARAMS-OBJID,
  it_PhotoFiles type ZWWW_FILES_T
    with header line, "standard table of RLGRAP-FILENAME,
  PhotoName type WWWPARAMS-OBJID,
  FilePhoto type RLGRAP-FILENAME,
  NumPhoto type i,
  NumUniq(5) type N.

Field-symbols:
  <fld>,
  <name>.
Data:
  fldName(50).

At Selection-Screen output.
*  If sy-uname <> 'VIKTOROV'.
  Loop at Screen.
    If Screen-Group1 = 'VVN'.
      If p_VVN is initial.
        Screen-Active = 0.
      Else.
        Screen-Active = 1.
      EndIf.
      Modify Screen.
    EndIf.

    If Screen-Name = 'P_VVN'.
      Screen-Active = 0.
      Modify Screen.
    EndIf.
  EndLoop.
*  EndIf.

At Selection-Screen.
  Case sy-ucomm.
    when 'VVN'.
      p_VVN = 'X'.
  EndCase.

Start-of-Selection.
  Case 'X'.
    when Excel or Excel_2.
      NameTemplate = 'ZWWW_MANY_LIST'.
    when Word.
      NameTemplate = 'ZWWW_MANY_WORD'.
    when RTF.                                               "#EC V#700
      NameTemplate = 'ZWWW_MANY_RTF'.                       "#EC V#700
  EndCase.


  Select *
    into corresponding fields of table it_Usr
    from USR21
    join ADRP
      on ADRP~PERSNUMBER = USR21~PERSNUMBER
    join ADCP
      on ADCP~PERSNUMBER = ADRP~PERSNUMBER
    join ADRC
      on ADRC~ADDRNUMBER = ADCP~ADDRNUMBER
    up to p_ROWS rows.

  Sort it_Usr by ADDRNUMBER DEPARTMENT FUNCTION BNAME.

******************************
  Data:
    StrVal(255),
    TypeValue.

  Define SetLine.
    Concatenate &1 &2 into it_Val-Var_Name.
    it_Val-Var_Num = &3.
    it_Val-Find_Text = &4.
    it_Val-Val_Type  = &5.
    Describe field &6 type TypeValue.
    Case TypeValue.
      when 'D'.
        Write &6 to StrVal.
        Condense StrVal.
        it_Val-Value = StrVal.
      when others.
        it_Val-Value = &6.
    EndCase.
    If &5 = 'V'.
      Concatenate &1 it_Val-Value into it_Val-Value.
    EndIf.
    Append it_Val.
  End-of-Definition.


  Case 'X'.
    when p_ADDRNM.
      fldName = 'ADDRNUMBER'.
      Assign: it_Usr-ADDRNUMBER to <fld>,
              it_Usr-NAME1 to <name>.
    when p_DEPART.
      fldName = 'DEPARTMENT'.
      Assign: it_Usr-DEPARTMENT to <fld>,
              it_Usr-DEPARTMENT to <name>.
    when p_FUNCTN.
      fldName = 'FUNCTION'.
      Assign: it_Usr-FUNCTION to <fld>,
              it_Usr-FUNCTION to <name>.
  EndCase.

  Case 'X'.
    when Excel or Excel_2 or Word.
      Perform GetPhotoNames tables it_PhotoNames.

      Perform DownloadPhoto using 'SESS_IMAGE' changing FilePhoto.
      SetLine '' 'GraphFoto'  '' '[Photo]' ''   FilePhoto.
      SetLine '' 'GraphFoto'  '' '' 'M' 'LoadPhoto'.
  EndCase.



  Num = 0.
  NumList = '0'.
  Loop at it_Usr.
    Clear it_Val.

*    At new <fld>.
    At new (fldName).

      NumList = NumList + 1.
      Condense NumList.

      If EXCEL = 'X'.
        Concatenate 'List_' NumList '!' into NameList.
      EndIf.

      new_ADDRNUMBER = 'X'.
      new_DEPARTMENT = 'X'.
      new_FUNCTION = 'X'.

      Clear it_GraphData.
      it_GraphData-Name = <name>.
    EndAt.

    it_GraphData-Count = 1.
    Collect it_GraphData.


    At new ADDRNUMBER.
      new_ADDRNUMBER = 'X'.
      Perform GetNextPhotoName tables it_PhotoNames changing PhotoName.
    EndAt.

    At new DEPARTMENT.
      new_DEPARTMENT = 'X'.
    EndAt.

    At new FUNCTION.
      new_FUNCTION = 'X'.
    EndAt.

    If new_ADDRNUMBER = 'X'.
      Clear new_ADDRNUMBER.

      Num = Num + 1.
      If NumList > 1.
        Case 'X'.
          when EXCEL_2.
            SetLine NameList 'User' Num '' 'M' 'SetNewPage'."### Excel
          when WORD.
            SetLine NameList 'User' Num '' 'V' 'NewPage'.   "### Word
            Num = Num + 1.
          when RTF.                                         "#EC V#700
            SetLine NameList 'User' Num '' 'V' 'NewPage'.   "#EC V#700
            Num = Num + 1.                                  "#EC V#700
        EndCase.
      EndIf.
      SetLine NameList 'User' Num '' 'V' 'Header'.
      SetLine NameList 'User' Num '[CITY1]' '' it_Usr-CITY1.
      SetLine NameList 'User' Num '[NAME1]' '' it_Usr-NAME1.


      Case 'X'.
        when Excel or Excel_2 or Word.
          Perform DownloadPhoto using PhotoName changing FilePhoto.
          SetLine NameList 'User' Num '[Photo]'      ''  FilePhoto.
          SetLine NameList 'User' Num ''             'M' 'LoadPhoto'.
      EndCase.
    EndIf.

    If new_DEPARTMENT = 'X'.
      Clear new_DEPARTMENT.
      Num = Num + 1.
      SetLine NameList 'User' Num ''             'V' 'Department'.
      SetLine NameList 'User' Num '[DEPARTMENT]' ''  it_Usr-DEPARTMENT.
    EndIf.

    If new_FUNCTION = 'X'.
      Clear new_FUNCTION.
      Num = Num + 1.
      SetLine NameList 'User' Num '' 'V' 'Function'.
      SetLine NameList 'User' Num '[FUNCTION]' '' it_Usr-FUNCTION.
    EndIf.

    Num = Num + 1.
    SetLine NameList 'User' Num '[BNAME]' '' it_Usr-BNAME.
    SetLine NameList 'User' Num '[NAME_LAST]' '' it_Usr-NAME_LAST.
    SetLine NameList 'User' Num '[NAME_FIRST]' '' it_Usr-NAME_FIRST.

  EndLoop.

  Num = 0.
  Loop at it_GraphData.
    Num = Num + 1.
    SetLine '' 'GraphData' Num '[Name]'  ''  it_GraphData-Name.
    SetLine '' 'GraphData' Num '[Count]' ''  it_GraphData-Count.
  EndLoop.

  SetLine '' 'CurDate' '' '' ''  sy-datum.
  SetLine '' 'CurTime' '' '' ''  sy-timlo.

  If EXCEL = 'X'.
    SetLine '' 'A_CopyList' '' '' ''  NumList.
    SetLine '' 'A_CopyList' '' '' 'M' 'CopySheets'.
  EndIf.

  SetLine '' 'Z_Protect'  '' '' ''  sy-timlo.
  SetLine '' 'Z_Protect'  '' '' 'M' 'SetProtect'.

  SetLine '' 'NewPage'    '' '' 'D' ''.

  Do p_Copies times.
    Call function 'ZWWW_OPENFORM'
      EXPORTING
        FORM_NAME   = NameTemplate
        PRINTDIALOG = ''
        PROTECT     = p_Protct
        DEBUG_MODE  = p_Debug
        CLOSE_FORM  = p_Close
        OPTIMIZE    = p_Optim
        WITHOUT_OLE = p_Wh_OLE
        USE_JAR     = p_UseJAR
      TABLES
        IT_VALUES   = it_Val
        IT_FILES    = it_PhotoFiles.
  EndDo.


*&---------------------------------------------------------------------*
*&      Form  WritePhoto
*&---------------------------------------------------------------------*
Form DownloadPhoto
  using    NamePhoto
  changing NameFile type RLGRAP-FILENAME.

  Data:
    TempDir type RLGRAP-FILENAME,
    w_Key    type WWWDATATAB,
    wwwValue type W3_QVALUE,
    FileSize type i,
    it_W3MIME type standard table of W3MIME,
    sy_subrc type sy-subrc.

*  Call function 'GUI_GET_DESKTOP_INFO'
*    EXPORTING
*      TYPE   = 4  "TmpDir
*    CHANGING
*      RETURN = TempDir.

  NumUniq = NumUniq + 1.
  w_Key-RELID = 'MI'.
  w_Key-OBJID = NamePhoto .
  Concatenate "TempDir '\'
    'X' w_Key-OBJID NumUniq '.jpg' into NameFile.

*  Call function 'DOWNLOAD_WEB_OBJECT'
*    EXPORTING
*      KEY         = w_Key
*      DESTINATION = NameFile
*    IMPORTING
*      RC          = sy_subrc.
*  Check sy_subrc = 0.

  Call function 'WWWPARAMS_READ'
    EXPORTING
      RELID  = 'MI'
      OBJID  = w_Key-OBJID
      NAME   = 'filesize'
    IMPORTING
      VALUE  = wwwValue
    EXCEPTIONS
      others = 1.

  Check sy-subrc = 0.
  FileSize = wwwValue.

  Call function 'WWWDATA_IMPORT'
    EXPORTING
      KEY    = w_Key
    TABLES
      MIME   = it_W3MIME
    EXCEPTIONS
      others = 99.
  Check sy-subrc = 0.

  Call function 'ZVVN_DATA_TO_XSTRING'
    EXPORTING
      DATA_LENGTH  = FileSize
    IMPORTING
      DATA_XSTRING = it_PhotoFiles-FILE_DATA
    TABLES
      IT_DATA      = it_W3MIME.

  it_PhotoFiles-FILE_NAME = NameFile.
*  Append NameFile to it_PhotoFiles.
  Append it_PhotoFiles.
EndForm.                    "WritePhoto

*&---------------------------------------------------------------------*
*&      Form  GetPhotoNames
*&---------------------------------------------------------------------*
Form GetPhotoNames
  tables it_PhotoNames.

  Select OBJID
    into table it_PhotoNames
    from WWWPARAMS
    up to 100 rows
    where RELID = 'MI'
      and NAME  = 'fileextension'
      and VALUE = '.JPG'
      and OBJID like '%PICTURE%'.
EndForm.                    "GetPhotoNames

*&---------------------------------------------------------------------*
*&      Form  GetNextPhotoName
*&---------------------------------------------------------------------*
Form GetNextPhotoName
  tables it_PhotoNames
  changing NamePhoto type WWWPARAMS-OBJID.

  Data: Cnt type i.

  NamePhoto = 'SESS_IMAGE'.
  NumPhoto = NumPhoto + 1.
  Describe table it_PhotoNames lines Cnt.
  check Cnt > 0.
  If NumPhoto > Cnt.
    NumPhoto = 1.
  EndIf.
  Read table it_PhotoNames into NamePhoto index NumPhoto.
EndForm.                    "GetNextPhotoName
