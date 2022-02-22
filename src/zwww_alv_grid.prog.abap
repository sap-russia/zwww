*&---------------------------------------------------------------------*
*& Report  ZWWW_ALV_GRID
*&
*&---------------------------------------------------------------------*
REPORT  ZWWW_ALV_GRID.
* ###### ###### # ###### ###### ## ALV_GRID

Data:
  it_Data type ref to data,
  Grid type ref to CL_GUI_ALV_GRID.

Field-symbols:
  <it_Data> type table.

Parameters:
  p_TabNm type TABNAME matchcode object DDSETBVIEW "DD_DBTB
    obligatory default 'USER_ADDR',
  p_Rows type i default 1000.

Parameters:
  p_Type_R radiobutton group gr1,
  p_Type_T radiobutton group gr1,
  p_Optim(10) type N modif id VVN,
  p_Close as checkbox modif id VVN,
  p_Debug as checkbox modif id VVN,
  p_Wh_OLE as checkbox modif id VVN,
  p_UseJAR as checkbox modif id VVN,
  p_UNICOD as checkbox modif id VVN.

Selection-screen Begin of screen 111.
Selection-screen End of screen 111.

Initialization.
  Class ZCL_ZWWW definition load. "### ############# # 4.6

  Data:
    CHAR_TAB type C.
  CHAR_TAB = ZCL_ZWWW=>CHAR_TAB.

At selection-screen on p_TabNm.
  Select single TABNAME
    into p_TabNm
    from DD02L
    where TABNAME  = p_TabNm
      and AS4LOCAL = 'A'.
  If sy-subrc <> 0.
    Message E100(AD) with p_TabNm.
  EndIf.

At selection-screen output.
  Data: it_Excl type standard table of RSMPE-FUNC with header line.

  Case sy-dynnr.
    when '0111'.
      CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
        EXPORTING
          P_STATUS  = 'MAIN'
        TABLES
          P_EXCLUDE = it_Excl.

  EndCase.

  If sy-uname <> 'VIKTOROV'.
    Loop at Screen.
      If Screen-Group1 = 'VVN'.
        Screen-InVisible = 1.
        Screen-Input = 0.
        Modify Screen.
      EndIf.
    EndLoop.
  EndIf.

At selection-screen.
  Case sy-dynnr.
    when '0111'.
      Case sy-ucomm.
        when 'EXCEL'.
          Case 'X'.
            when p_Type_R.
              Perform PrintExcel_R using Grid <it_Data>.
            when p_Type_T.
              Perform PrintExcel_T using Grid <it_Data>.
          EndCase.

        when 'BACK' or 'CANCEL' or 'EXIT'.
          Leave to screen 0.
      EndCase.
  EndCase.

*&---------------------------------------------------------------------*
Start-of-Selection.
*  Create data it_Data type standard table of (p_TabNm).
* ### ###### 4.6, # ### ##### #### ### ###### :)
  Perform CreateTable using p_TabNm changing it_Data.

  If sy-subrc = 0.
    Assign it_Data->* to <it_Data>.
    check sy-subrc = 0.
  Else.
    Exit.
  EndIf.

  Select *
    into table <it_Data>
    from (p_TabNm)
    up to p_Rows rows.

  If sy-subrc <> 0.
    Message S718(AD) with p_TabNm.
    Exit.
  EndIf.

End-of-Selection.

  Data:
    it_Excl type UI_FUNCTIONS with header line.

  If p_Type_T = 'X'.
    it_Excl = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
    Append it_Excl.
  EndIf.

  Create object Grid
    EXPORTING
      i_parent = CL_GUI_CONTAINER=>DEFAULT_SCREEN.

  Call method Grid->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME     = p_TabNm
      IT_TOOLBAR_EXCLUDING = it_Excl[]
    CHANGING
      IT_OUTTAB            = <it_Data>.

  Call selection-screen 111.

*&---------------------------------------------------------------------*
*&      Form  GetFormatMask
*&---------------------------------------------------------------------*
Form Get_Header_And_Mask
  using it_FIELDCATALOG type LVC_T_FCAT
        StrHeader type string
        StrMask type string.

  Field-symbols:
    <FldCat> type line of LVC_T_FCAT.
  Data:
    Mask type string.


  Clear: StrHeader, StrMask.
  Loop at it_FIELDCATALOG assigning <FldCat>
    where NO_OUT is initial.
    Concatenate StrHeader <FldCat>-SCRTEXT_L Char_Tab into StrHeader.
    Mask = <FldCat>-INTTYPE.
    If <FldCat>-DECIMALS > 0.
      Concatenate Mask '0.' into Mask.
      Do <FldCat>-DECIMALS times.
        Concatenate Mask '0' into Mask.
      EndDo.
    EndIf.
    Concatenate StrMask Mask Char_Tab into StrMask.
  EndLoop.
EndForm.                    "GetFormatMask

*&---------------------------------------------------------------------*
*&      Form  PrintExcel_R
*&---------------------------------------------------------------------*
Form PrintExcel_R
  using Grid type ref to CL_GUI_ALV_GRID
        it_Data type table.
  Data:
*    CharTab value ' ', "CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
    ET_FILTERED_ENTRIES type LVC_T_FIDX,
    EP_COLLECT00 type ref to data,
    EP_COLLECT01 type ref to data,
    EP_COLLECT02 type ref to data,
    EP_COLLECT03 type ref to data,
    EP_COLLECT04 type ref to data,
    EP_COLLECT05 type ref to data,
    EP_COLLECT06 type ref to data,
    EP_COLLECT07 type ref to data,
    EP_COLLECT08 type ref to data,
    EP_COLLECT09 type ref to data,
    ET_GROUPLEVELS type LVC_T_GRPL with header line,
    ET_FIELDCATALOG type LVC_T_FCAT,
    it_Val type standard table of ZWWW_VALUES
      with header line,
    StrValue type string,
    StrMask type string,
    StrFld type string,
    NumCols type i,
    RowNo   type i.

  Field-symbols:
    <Rec>,
    <Fld>,
    <FldCat> type line of LVC_T_FCAT,
    <it_collect> type table.

  Call method Grid->GET_FILTERED_ENTRIES
    IMPORTING
      ET_FILTERED_ENTRIES = ET_FILTERED_ENTRIES[].

  Call method Grid->GET_SUBTOTALS
    IMPORTING
      EP_COLLECT00   = EP_COLLECT00
      EP_COLLECT01   = EP_COLLECT01
      EP_COLLECT02   = EP_COLLECT02
      EP_COLLECT03   = EP_COLLECT03
      EP_COLLECT04   = EP_COLLECT04
      EP_COLLECT05   = EP_COLLECT05
      EP_COLLECT06   = EP_COLLECT06
      EP_COLLECT07   = EP_COLLECT07
      EP_COLLECT08   = EP_COLLECT08
      EP_COLLECT09   = EP_COLLECT09
      ET_GROUPLEVELS = ET_GROUPLEVELS[].

  Call method Grid->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = ET_FIELDCATALOG.

  Delete ET_FIELDCATALOG where NO_OUT = 'X'.
  Sort ET_FIELDCATALOG by COL_POS.

  Define SetVal.
    Clear it_Val.
    it_Val-VAR_NAME  = &1.
    it_Val-VAR_NUM   = &2.
    it_Val-FIND_TEXT = &3.
    it_Val-VAL_TYPE  = &4.
    it_Val-VALUE     = &5.
*    Concatenate '''' it_Val-VALUE into it_Val-VALUE.
    Append it_Val.
  End-of-definition.

  Data: FldType, CharValue(255), Len type I.

  Define Write_to.
    Describe field &1 type FldType.
    If &1 is initial.
      Case FldType.
*        when 'P'.
*          Write &1 to CharValue.
*          &2 = CharValue.
        when others.
          Clear &2.
      EndCase.
    Else.
      Case FldType.
        when 'D'. "'P' or 'F' or 'N' or 'I' or
          Write &1 to CharValue.
          &2 = CharValue.
        when others.
          &2 = &1.
      EndCase.
    EndIf.
    Len = Strlen( &2 ).
    If Len > 7000.
      &2 = &2(7000).
    EndIf.
  End-of-definition.

  Describe table ET_FIELDCATALOG lines NumCols.
  SetVal 'A_BeforeOutput' '' '' '' NumCols.
  SetVal 'A_BeforeOutput' '' '' 'M' 'BeforeOutput'.

  Clear: StrValue, StrMask.
*  Data: Mask type string.
*  Loop at ET_FIELDCATALOG assigning <FldCat>.
*    Concatenate StrValue <FldCat>-SCRTEXT_L Char_Tab into StrValue.
*    Mask = <FldCat>-INTTYPE.
*    If <FldCat>-DECIMALS > 0.
*      Concatenate Mask '0.' into Mask.
*      Do <FldCat>-DECIMALS times.
*      Concatenate Mask '0' into Mask.
*      EndDo.
*    EndIf.
*    Concatenate StrMask Mask Char_Tab into StrMask.
*  EndLoop.
  Perform Get_Header_And_Mask
    using ET_FIELDCATALOG[]
          StrValue
          StrMask.

  SetVal 'Header'      '' 'FIELD1' 'R' StrValue.
  SetVal 'FormatMask' '' 'FIELD1' 'R' StrMask.
*  SetVal 'FormatMask' '' ''       'M' 'SetFormatMask'.

  Assign EP_COLLECT00->* to <it_collect>.
  If sy-subrc = 0 and <it_collect> is assigned.
    If <it_collect> is initial.
      SetVal 'Total' '' '' 'D' ''.
      Refresh ET_GROUPLEVELS[].
    EndIf.

    If ET_GROUPLEVELS[] is initial.
      SetVal 'SubTotal' '' '' 'D' ''.

      Describe table it_Data lines NumCols.

      ET_GROUPLEVELS-INDEX_FROM = 1.
      ET_GROUPLEVELS-INDEX_TO   = NumCols.
    EndIf.

    If not <it_collect> is initial.
      ET_GROUPLEVELS-SUBTOT = 'X'.
      ET_GROUPLEVELS-CINDX_FROM = 1.
    EndIf.

    Append ET_GROUPLEVELS.
  EndIf.

  Sort ET_FILTERED_ENTRIES.

  RowNo = 0.

  Loop at ET_GROUPLEVELS.
    Case ET_GROUPLEVELS-LEVEL.
      when 0.
        Assign EP_COLLECT00->* to <it_collect>.
      when 1.
        Assign EP_COLLECT01->* to <it_collect>.
      when 2.
        Assign EP_COLLECT02->* to <it_collect>.
      when 3.
        Assign EP_COLLECT03->* to <it_collect>.
      when 4.
        Assign EP_COLLECT04->* to <it_collect>.
      when 5.
        Assign EP_COLLECT05->* to <it_collect>.
      when 6.
        Assign EP_COLLECT06->* to <it_collect>.
      when 7.
        Assign EP_COLLECT07->* to <it_collect>.
      when 8.
        Assign EP_COLLECT08->* to <it_collect>.
      when 9.
        Assign EP_COLLECT09->* to <it_collect>.
    EndCase.

    Loop at it_Data assigning <Rec>
      from ET_GROUPLEVELS-INDEX_FROM
      to   ET_GROUPLEVELS-INDEX_TO.

      Read table ET_FILTERED_ENTRIES with key table_line = sy-tabix
        transporting  no fields.
      check sy-subrc <> 0.

      Clear StrValue.
      RowNo = RowNo + 1.
      Loop at ET_FIELDCATALOG assigning <FldCat>.
        Assign component <FldCat>-FIELDNAME of structure <Rec> to <Fld>.
        Write_to <Fld> StrFld.
        Condense StrFld.
*        Concatenate '''' StrFld into StrFld.
        Concatenate StrValue StrFld Char_Tab into StrValue.
      EndLoop.

      SetVal 'Line' RowNo 'FIELD1' 'R' StrValue.
    EndLoop.

    If ET_GROUPLEVELS-SUBTOT = 'X'.
      RowNo = RowNo + 1.
      If ET_GROUPLEVELS-LEVEL > 0.
        SetVal 'Line' RowNo '' 'V' 'SubTotal'.
      Else.
        SetVal 'Line' RowNo '' 'V' 'Total'.
      EndIf.

      Read table <it_collect> index ET_GROUPLEVELS-CINDX_FROM
        assigning <Rec>.

      Clear StrValue.
      Loop at ET_FIELDCATALOG assigning <FldCat>.
        Assign component <FldCat>-COL_ID of structure <Rec> to <Fld>.
        Write_to <Fld> StrFld.
        Condense StrFld.
        Concatenate StrValue StrFld Char_Tab into StrValue.
      EndLoop.
      SetVal 'Line' RowNo 'FIELD1' 'R' StrValue.
    EndIf.
  EndLoop.

  SetVal 'Z_AfterOutput' '' '' 'M' 'AfterOutput'.

  Call function 'ZWWW_OPENFORM'
    EXPORTING
      FORM_NAME   = 'ZWWW_ALV_GRID'
      PRINTDIALOG = ''
      DEBUG_MODE  = p_Debug
      OPTIMIZE    = p_Optim
      CLOSE_FORM  = p_Close
      PROTECT     = ''
      WITHOUT_OLE = p_Wh_OLE
      USE_JAR     = p_UseJAR
      USE_UNICODE = p_UNICOD
    TABLES
      IT_VALUES   = it_Val.

EndForm.                    " PRINTEXCEL


*&---------------------------------------------------------------------*
*&      Form  PrintExcel_T
*&---------------------------------------------------------------------*
Form PrintExcel_T
  using Grid type ref to CL_GUI_ALV_GRID
        it_Data type table.

  Data:
    it_Val type standard table of ZWWW_VALUES
      with header line,
    it_FldCat type standard table of ZWWW_FIELD_CATALOG
      with header line,
    ET_FIELDCATALOG type LVC_T_FCAT
      with header line,
    NumCols type i.

  Call method Grid->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = ET_FIELDCATALOG[].

  Sort ET_FIELDCATALOG by COL_POS.

  Loop at ET_FIELDCATALOG.
    Clear it_FldCat.

    If ET_FIELDCATALOG-NO_OUT is initial.
      NumCols = NumCols + 1.
    EndIf.

    If NumCols = 1.
      it_FldCat-FIND_TEXT    = 'FIELD1'.
    EndIf.

    it_FldCat-FIELD_NAME   = ET_FIELDCATALOG-FIELDNAME.
    it_FldCat-FIELD_HEADER = ET_FIELDCATALOG-SCRTEXT_L.
    it_FldCat-NO_OUT       = ET_FIELDCATALOG-NO_OUT.
    it_FldCat-NO_ZERO      = 'X'.
    it_FldCat-COL_POS      = NumCols.
    Append it_FldCat.
  EndLoop.

  Data:
    StrValue type string,
    StrMask  type string.

  Perform Get_Header_And_Mask
    using ET_FIELDCATALOG[]
          StrValue
          StrMask.

  SetVal 'A_BeforeOutput' '' '' '' NumCols.
  SetVal 'A_BeforeOutput' '' '' 'M' 'BeforeOutput'.

  SetVal 'FormatMask' '' 'FIELD1' 'R' StrMask.

  Call function 'ZWWW_PREPARE_TABLE'
    EXPORTING
      HEADER_NAME       = 'Header'
      LINE_NAME         = 'Line'
      VAL_TYPE          = 'T'
    TABLES
      IT_ANY_TABLE      = it_Data
      IT_VALUES         = it_Val
      IT_FIELDS_CATALOG = it_FldCat.

  SetVal 'SubTotal' '' '' 'D' ''.
  SetVal 'Total'    '' '' 'D' ''.

  SetVal 'Z_AfterOutput' '' '' 'M' 'AfterOutput'.

  Call function 'ZWWW_OPENFORM'
    EXPORTING
      FORM_NAME   = 'ZWWW_ALV_GRID'
      PRINTDIALOG = ''
      DEBUG_MODE  = p_Debug
      OPTIMIZE    = p_Optim
      CLOSE_FORM  = p_Close
      PROTECT     = ''
      WITHOUT_OLE = p_Wh_OLE
      USE_JAR     = p_UseJAR
      USE_UNICODE = p_UNICOD
    TABLES
      IT_VALUES   = it_Val.
EndForm.                    "PrintExcel_T

*&---------------------------------------------------------------------*
*&      Form  CreateTable
*&---------------------------------------------------------------------*
Form CreateTable
  using p_TabNm
  changing it_Data type ref to data.

  Data:
    it_FIELDCATALOG type LVC_T_FCAT.

  Call function'LVC_FIELDCATALOG_MERGE'
    exporting I_STRUCTURE_NAME = p_TabNm
    changing  CT_FIELDCAT = it_FIELDCATALOG
    exceptions others = 1.

  Call method CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = it_FIELDCATALOG
    IMPORTING
      EP_TABLE        = it_Data.

EndForm.                    " CreateTable
