*&---------------------------------------------------------------------*
*& Report  ZWWW_MIGRATE
*&---------------------------------------------------------------------*
REPORT ZWWW_MIGRATE.

Type-pools:
  SEOO,
  SEOS,
  SEOR,
  SEOP,
  SEOT,
  SVRS2.

Tables:
*  SVERS,
  VERSOBJ,
  TFTIT,
  ENLFDIR,
  TADIR,
  E070,
  E071.

Tables:
  SSCRFIELDS.

Data:
  DEVCLASS type DEVCLASS,
  TRKORR type TRKORR.

Data:
  Stat type SY-PFKEY,
  it_Excl type standard table of RSMPE-FUNC with header line.

Data:
  FrontendCodepage type char20, "ABAP_ENCOD,
  char_tab,
  CHAR_0D  TYPE C,
  CHAR_0A  TYPE C,
  isOk       type sy-subrc value 0,
  isError    type sy-subrc value 1,
  isNotFound type sy-subrc value 98,
  isNothing  type sy-subrc value 99,
  flagOld    value 'O',
  flagNew    value 'N'.

Types:
  t_SortKey(3) type N,
  Begin of t_TADIR,
    SortKey  type t_SortKey,
    PGMID    type TADIR-PGMID,
    DEVCLASS type TADIR-DEVCLASS,
    OBJECT   type TADIR-OBJECT,
    OBJ_NAME type TADIR-OBJ_NAME,
    TEXT     type KO100-TEXT,
    tabix    type sy-tabix,
    Status,
    Icon(4),
    Message(150),
    Flag,
    Object_Path type string,
  End of t_TADIR,
  tt_TADIR type standard table of t_TADIR,
  tt_Source type standard table of char255,
  tt_E071   type standard table of E071,
  tt_E071K  type standard table of E071K,
  tt_String type standard table of string,
  Begin of t_WWWDATATAB,
    RELID type WWWDATATAB-RELID,
    OBJID type WWWDATATAB-OBJID,
    TEXT  type WWWDATATAB-TEXT,
  End of t_WWWDATATAB,
  tt_WWWDATATAB type standard table of t_WWWDATATAB,

  Begin of t_ObjectSort,
    OBJECT   type char255, "TADIR-OBJECT,
    SortKey  type t_SortKey,
    ExpImp,
  End of t_ObjectSort,
  tt_R3TR_Sort type sorted table of t_ObjectSort
    with unique key OBJECT,
  tt_LIMU_Sort type sorted table of t_ObjectSort
    with unique key OBJECT.

Types:
  Begin of t_TextPoolFile,
    LANGU type sy-langu.
        include structure TEXTPOOL. types:
  End of t_TextPoolFile.

Data:
  it_R3TR_Sort type tt_R3TR_Sort
    with header line,
  it_LIMU_Sort type tt_LIMU_Sort
    with header line.

Types:
  Begin of t_R3TR_FUGR,
    TLIBG type TLIBG occurs 0,
    TLIBT type TLIBT occurs 0,
  End of t_R3TR_FUGR,

  Begin of t_R3TR_CLAS,
    DUMMY type char1 occurs 0,
  End of t_R3TR_CLAS,

  Begin of t_R3TR_TRAN,
    TSTC  type TSTC  occurs 0,
    TSTCA type TSTCA occurs 0,
    TSTCT type TSTCT occurs 0,
    TSTCP type TSTCP occurs 0,
    TSTCC type TSTCC occurs 0,
  End of t_R3TR_TRAN,

  Begin of FileParameters,
    Name type string,
    Ext  type string,
    Length type i,
  End of FileParameters,

  Begin of t_R3TR_W3MI,
    WWWDATATAB     type WWWDATATAB occurs 0,
    W3MIME         type W3MIME occurs 0,
    FLPARAM        type FileParameters occurs 0,
  End of t_R3TR_W3MI,

  Begin of t_R3TR_TABU,
    E071K type E071K occurs 0,
    DATA  type string occurs 0,
  End of t_R3TR_TABU,

  Begin of t_R3TR_TOBJ,
    OBJH  type OBJH  occurs 0,
    OBJT  type OBJT  occurs 0,
    OBJS  type OBJS  occurs 0,
    OBJSL type OBJSL occurs 0,
    OBJM  type OBJM  occurs 0,
    TDDAT type TDDAT occurs 0,
    TVDIR type TVDIR occurs 0,
    TVIMF type TVIMF occurs 0,
  End of t_R3TR_TOBJ,

  Begin of t_R3TR_Types,
    CLAS type t_R3TR_CLAS,
    FUGR type t_R3TR_FUGR,
    TABU type t_R3TR_TABU,
    TOBJ type t_R3TR_TOBJ,
    TRAN type t_R3TR_TRAN,
    W3MI type t_R3TR_W3MI,
  End of t_R3TR_Types.

Types:
  Begin of t_LIMU_FUGT,
    TLIBT type TLIBT occurs 0,
  End of t_LIMU_FUGT,

  Begin of t_LIMU_MSAD,
    T100A type T100A occurs 0,
    T100T type T100T occurs 0,
  End of t_LIMU_MSAD,
  Begin of t_LIMU_MESS,
    T100  type T100  occurs 0,
    T100U type T100U occurs 0,
    T100X type T100X occurs 0,
  End of t_LIMU_MESS,

  Begin of t_LIMU_VARI,
    VARID      type VARID occurs 0,
    VARIT      type VARIT occurs 0,
    VARIS      type VARIS occurs 0,
    VALUTAB    type RSPARAMS   occurs 0,
    OBJECTS    type VANZ       occurs 0,
  End of t_LIMU_VARI.

TYPES:
  BEGIN OF t_SVRS2_CLSD,
    TRDIR TYPE TRDIR OCCURS 0,
    CLASS TYPE VSEOCLASS OCCURS 0,
    FRNDS TYPE SEOFRIENDS OCCURS 0,
    MDLOG TYPE SMODILOG OCCURS 0,
    MTREL TYPE SEOMETAREL OCCURS 0,
    REPS TYPE SVRS2_REPS-ABAPTEXT,
  END OF t_SVRS2_CLSD.

TYPES:
  BEGIN OF t_SVRS2_CPRI,
    ALIAS TYPE SEOALIASES OCCURS 0,
    ATTR TYPE VSEOATTRIB OCCURS 0,
    EVENT TYPE VSEOEVENT OCCURS 0,
    EXCEP TYPE VSEOEXCEP OCCURS 0,
    MDLOG TYPE SMODILOG OCCURS 0,
    METH TYPE VSEOMETHOD OCCURS 0,
    PARAM TYPE VSEOPARAM OCCURS 0,
    REPS TYPE SVRS2_REPS-ABAPTEXT,
    TRDIR TYPE TRDIR OCCURS 0,
    TYPE TYPE VSEOTYPE OCCURS 0,
    TYPEP TYPE SEOTYPEPLS OCCURS 0,
  END OF t_SVRS2_CPRI.
TYPES:
  BEGIN OF t_SVRS2_CPRO,
    ALIAS TYPE SEOALIASES OCCURS 0,
    ATTR TYPE VSEOATTRIB OCCURS 0,
    EVENT TYPE VSEOEVENT OCCURS 0,
    EXCEP TYPE VSEOEXCEP OCCURS 0,
    MDLOG TYPE SMODILOG OCCURS 0,
    METH TYPE VSEOMETHOD OCCURS 0,
    PARAM TYPE VSEOPARAM OCCURS 0,
    REDEF TYPE SEOREDEF OCCURS 0,
    REPS TYPE SVRS2_REPS-ABAPTEXT,
    TRDIR TYPE TRDIR OCCURS 0,
    TYPE TYPE VSEOTYPE OCCURS 0,
    TYPEP TYPE SEOTYPEPLS OCCURS 0,
  END OF t_SVRS2_CPRO.
TYPES:
  BEGIN OF t_SVRS2_CPUB,
    ALIAS TYPE SEOALIASES OCCURS 0,
    ATTR TYPE VSEOATTRIB OCCURS 0,
    EVENT TYPE VSEOEVENT OCCURS 0,
    EXCEP TYPE VSEOEXCEP OCCURS 0,
    IMPRL TYPE SEOIMPLREL OCCURS 0,
    MDLOG TYPE SMODILOG OCCURS 0,
    METH TYPE VSEOMETHOD OCCURS 0,
    MTREL TYPE SEOMETAREL OCCURS 0,
    PARAM TYPE VSEOPARAM OCCURS 0,
    REDEF TYPE SEOREDEF OCCURS 0,
    REPS TYPE SVRS2_REPS-ABAPTEXT,
    TRDIR TYPE TRDIR OCCURS 0,
    TYPE TYPE VSEOTYPE OCCURS 0,
    TYPEP TYPE SEOTYPEPLS OCCURS 0,
  END OF t_SVRS2_CPUB.

TYPES:
  BEGIN OF t_SVRS2_INTF,
    ALIAS TYPE SEOALIASES OCCURS 0,
    ATTR TYPE VSEOATTRIB OCCURS 0,
    COMPR TYPE VSEOCOMPRI OCCURS 0,
    EVENT TYPE VSEOEVENT OCCURS 0,
    EXCEP TYPE VSEOEXCEP OCCURS 0,
    INTF TYPE VSEOINTERF OCCURS 0,
    MDLOG TYPE SMODILOG OCCURS 0,
    METH TYPE VSEOMETHOD OCCURS 0,
    PARAM TYPE VSEOPARAM OCCURS 0,
    PREPS TYPE SVRS2_REPS-ABAPTEXT,
    REPS TYPE SVRS2_REPS-ABAPTEXT,
    TRDIR TYPE TRDIR OCCURS 0,
    TYPE TYPE VSEOTYPE OCCURS 0,
    TYPEP TYPE SEOTYPEPLS OCCURS 0,
  END OF t_SVRS2_INTF.

Types: tt_REPOSRC type table of REPOSRC"#EC V#610
         WITH NON-UNIQUE DEFAULT KEY."#EC V#610
Types: Begin of t_LIMU_REPS.
Types:   REPOSRC type tt_REPOSRC."#EC V#610
include type SVRS2_REPS.
Types: End of t_LIMU_REPS.

Types:
  Begin of t_LIMU_Types,
*    OBJTYPE type string,
*    OBJNAME type string,
    FUGT type t_LIMU_FUGT,
    MSAD type t_LIMU_MSAD,
    MESS type t_LIMU_MESS,
    VARI type t_LIMU_VARI,
    CLSD type t_SVRS2_CLSD,
    CPRI type t_SVRS2_CPRI,
    CPRO type t_SVRS2_CPRO,
    CPUB type t_SVRS2_CPUB,
    INTF type t_SVRS2_INTF,
    REPS type t_LIMU_REPS,"
  End of t_LIMU_Types.

Types: Begin of t_Objects_Ext.
include: type t_R3TR_Types.
include: type t_LIMU_Types.
Types: End   of t_Objects_Ext.

*----------------------------------------------------------------------*
*       CLASS ClGridEvent DEFINITION
*----------------------------------------------------------------------*
Class ClGridEvent definition.
  Public section.
    Class-Methods:
      HandleDblClick
        for event double_click of cl_gui_alv_grid
            importing e_row e_column.
EndClass.                    "ClGridEvent DEFINITION

Types:
  tt_Readme type standard table of char255.
Data:
  it_TADIR type tt_TADIR
    with header line,
  it_E071K type standard table of E071K
    with header line,
  w_Path(255),
  it_Readme type tt_Readme "string
    with header line,
  it_WWWReadme type standard table of string
    with header line,
  FileName type string,
  Grid type ref to CL_GUI_ALV_GRID,
*  GridEvent type ref to ClGridEvent,
  it_FCat type standard table of LVC_S_FCAT
    with header line,
  it_LANGU type standard table of sy-langu
    with header line,
  IS_LAYOUT type LVC_S_LAYO.
Data:
  obj_Zip type ref to object,"CL_ABAP_ZIP,
  Begin of FileZIP,
    Data type xstring,
    Len  type i,
    FileName type string,
  End of FileZIP.

*&---------------------------------------------------------------------*
Parameters:
  p_Export radiobutton group gr1 default 'X' user-command GR1,
  p_Import radiobutton group gr1.

Select-options:
  s_TRKORR for TRKORR,
  s_CLASS  for TADIR-DEVCLASS default 'ZWWW_EXCEL',
  s_PGMID  for TADIR-PGMID default 'R3TR',
  s_OBJECT for TADIR-OBJECT,
  s_NAME   for TADIR-OBJ_NAME,
  s_LANGU  for SY-LANGU modif id VVN.

Parameters:
  p_Path(255) default 'C:\ZWWW\',
  p_ZIP    as checkbox modif id ZIP,
  p_Rewrte as checkbox default 'X',
  Len_ABAP type i default 255,
  p_EXPERT as checkbox,
  p_VVN    as checkbox.

Selection-screen:
  Begin of Screen 100,
    Function key 1,
    Function key 2,
    Function key 3,
  End of Screen 100.

Parameters:
  p_Old type c radiobutton group GrX modif id VVN,
  p_New type c radiobutton group GrX modif id VVN,
  p_All type c radiobutton group GrX modif id VVN default 'X'.

Define PutMessage.
  Message W714(28) with &1.
End-of-Definition.

Define PutErrorMessage.
  Message E714(28) with &1.
End-of-Definition.

Define PutInfoMessage.
  Message I714(28) with &1.
End-of-Definition.

Load-of-Program.
  Perform Self_Adapting.

Initialization.
  Data:
    X_TAB(1) type x value '09',
    X_0D(1)  TYPE X VALUE '0D',
    X_0A(1)  TYPE X VALUE '0A',
    CHAR_A value 'A',
    CHAR_B value 'B',
    CHAR_01.
  Field-symbols:
    <X>, <Y>, <EI>.

*  Select single * from SVERS.
  If sy-saprl < '610'. "SVERS-VERSION(3) < '610'.
    Len_ABAP = 72.
  EndIf.

  Data:
    FILENAME_type type FUPARAREF-STRUCTURE value 'STRING'.

  Select single STRUCTURE
    into FILENAME_type
    from FUPARAREF
    where FUNCNAME = 'GUI_DOWNLOAD'
      and PARAMETER = 'FILENAME'.

  Data: CPCODEPAGE type CPCODEPAGE.

  Call function 'NLS_GET_FRONTEND_CP'
    EXPORTING
      LANGU             = sy-langu
    IMPORTING
      FRONTEND_CODEPAGE = CPCODEPAGE
    EXCEPTIONS
      others            = 99.
  FrontendCodepage = CPCODEPAGE.

  Assign CHAR_A  to <X>  type 'X'.
  Assign CHAR_B  to <Y>  type 'X'.
  Assign CHAR_01 to <EI> type 'X'.
  <EI> = <Y> - <X>.

  Assign CHAR_TAB to <X> type 'X'.
  Clear <X>.
  <X> = X_TAB * <EI>.

  Assign CHAR_0D to <X> type 'X'.
  Clear <X>.
  <X> = X_0D * <EI>.

  Assign CHAR_0A to <X> type 'X'.
  Clear <X>.
  <X> = X_0A * <EI>.

  Data: subrcZIP type sy-subrc.
  Perform ZIP_allowed
    changing subrcZIP.
  If subrcZIP = isOk.
    p_ZIP = 'X'.
  EndIf.

  SSCRFIELDS-FUNCTXT_01 = 'Export'.
  SSCRFIELDS-FUNCTXT_02 = 'Import'.
  SSCRFIELDS-FUNCTXT_03 = 'Activate'.

  Define AddSortKey.
    it_&1_Sort-OBJECT = &2.
    Describe table it_&1_Sort[] lines it_&1_Sort-SortKey.
    If &4 = '1' or &4 is initial.
      Add 1 to it_&1_Sort-SortKey.
    ElseIf &4 = '2'.
      Add 901 to it_&1_Sort-SortKey.
    EndIf.
    it_&1_Sort-ExpImp = &3.
    Insert table it_&1_Sort.
  End-of-definition.

  AddSortKey R3TR 'DEVC' 'S' ''.
  AddSortKey R3TR 'MSAG' '' ''.
  AddSortKey R3TR 'DOMA' '' ''.
  AddSortKey R3TR 'DTEL' '' ''.
  AddSortKey R3TR 'TABL' '' ''.
  AddSortKey R3TR 'VIEW' '' ''.
  AddSortKey R3TR 'TABU' '' '2'.
  AddSortKey R3TR 'SHLP' '' ''.
  AddSortKey R3TR 'ENQU' '' ''.
  AddSortKey R3TR 'TYPE' '' ''.
  AddSortKey R3TR 'TTYP' '' ''.
  AddSortKey R3TR 'FUGR' '' ''.
  AddSortKey R3TR 'PROG' '' ''.
  AddSortKey R3TR 'W3MI' '' ''.
  AddSortKey R3TR 'TRAN' '' ''.


  AddSortKey LIMU 'TABD' '' ''.
  AddSortKey LIMU 'TABT' '' ''.
  AddSortKey LIMU 'INDX' '' ''.

  AddSortKey LIMU 'CLSD' '' ''.
  AddSortKey LIMU 'CINC' '' ''.
  AddSortKey LIMU 'CPRI' '' ''.
  AddSortKey LIMU 'CPRO' '' ''.
  AddSortKey LIMU 'CPUB' '' ''.

  AddSortKey LIMU 'REPS' '' ''.
  AddSortKey LIMU 'REPT' '' ''.
  AddSortKey LIMU 'FUNC' '' ''.
  AddSortKey LIMU 'DYNP' '' ''.
  AddSortKey LIMU 'CUAD' '' ''.

  AddSortKey LIMU 'VARI' '' ''.

  s_LANGU-low = sy-langu.
  s_LANGU-sign = 'I'.
  s_LANGU-option = 'EQ'.
  Append s_LANGU.
  If sy-uname = 'VIKTOROV'.
*    p_VVN = 'X'.
  EndIf.

  If p_VVN = 'X'.
    s_LANGU-low = 'EN'.
    s_LANGU-sign = 'I'.
    s_LANGU-option = 'EQ'.
    Append s_LANGU.
  EndIf.
  Sort s_LANGU.
  Delete adjacent duplicates from s_LANGU.

At Selection-screen output.
  Data: IS_STABLE type LVC_S_STBL value 'XX'.

  Case sy-dynnr.
    when '1000'.
      If Stat is initial.
        Get pf-status Stat.
      EndIf.

      Loop at Screen.

        Case Screen-name.
          when 'S_TRKORR-LOW'. "or 'S_TRKORR-HIGH'.
*            If p_Import = 'X'.
*              Screen-required = 1.
*            Else.
*              Screen-required = 0.
*            EndIf.
*            Modify Screen.
          when 'P_EXPERT' or 'P_VVN'.
            Screen-invisible = 1.
            Modify Screen.
*          when 'P_OLD' or 'P_NEW'.
*            If sy-uname <> 'VIKTOROV'.
*              Screen-invisible = 1.
*              Modify Screen.
*            EndIf.
          when 'P_PATH'.
            If p_Expert = 'X'.
              Screen-Intensified = 1.
            Else.
              Screen-Intensified = 0.
            EndIf.
            Modify Screen.
        EndCase.

        Case Screen-Group1.
          when 'ZIP'.
            If subrcZIP <> isOk.
              p_ZIP = ''.
              Screen-Active = 0.
              Modify Screen.
            EndIf.
          when 'VVN'.
            If p_VVN <> 'X'.
              Screen-Invisible = 1.
              Screen-Input = 0.
            Else.
              Screen-Invisible = 0.
              Screen-Input = 1.
            EndIf.
            Modify Screen.
        EndCase.
      EndLoop.


    when '0100'.
      Call function 'RS_SET_SELSCREEN_STATUS'
        EXPORTING
          P_STATUS  = Stat
        TABLES
          P_EXCLUDE = it_Excl
        EXCEPTIONS
          others    = 1.
      Call method Grid->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = IS_STABLE
          I_SOFT_REFRESH = ' '.
  EndCase.

At Selection-screen on Exit-command.
  Case sy-dynnr.
    when '1000'.
    when '0100'.
      Set screen 0. "####### ## 0100-##
      Leave screen. "######## # 1000-#
  EndCase.

At Selection-screen.
  Case sy-dynnr.
    when '1000'.
      Refresh it_Excl.
      it_Excl = 'ONLI'. Append it_Excl.
      it_Excl = 'SPOS'. Append it_Excl.

      Case 'X'.
        when p_Export.
          it_Excl = 'FC02'. Append it_Excl.
        when p_Import.
          it_Excl = 'FC01'. Append it_Excl.
      EndCase.

      Case SSCRFIELDS-UCOMM.
        when 'EXPERT'.
          p_EXPERT = 'X'.
        when 'VVN'.
          p_VVN = 'X'.

          s_LANGU-low = 'EN'.
          s_LANGU-sign = 'I'.
          s_LANGU-option = 'EQ'.
          Append s_LANGU.

      EndCase.

    when '0100'.
      Case SSCRFIELDS-UCOMM.
        when 'FC01'.
          Perform Do_Export_to_Files using p_Path.
        when 'FC02'.
          Perform Do_Import_to_SAP using w_Path '1'.
          Perform Do_Import_to_SAP using w_Path '2'.
        when 'FC03'.
          Perform Activate_Selected_Objects.
      EndCase.
  EndCase.

At Selection-screen on value-request for p_Path.
  Perform F4_FlPath using 'P_PATH'. " 'xls'.

At Selection-screen on p_Path.
  Data:
    it_Path type standard table of string
      with header line.

  Split p_Path at '\' into table it_Path.
  Clear p_Path.
  Loop at it_Path where not table_line is initial.
    Concatenate p_Path it_Path '\' into p_Path.
  EndLoop.

At Selection-screen on s_TRKORR.
  If p_Import = 'X'.
    Perform CheckTRKORR using 'W'.
  EndIf.

*&---------------------------------------------------------------------*
Start-of-Selection.
  Data:
    it_OBJECT type standard table of KO100
      with key PGMID OBJECT
      with header line.
  Field-symbols:
    <tadir> type t_TADIR.

  Select SPRAS
    into table it_LANGU
    from T002
    where SPRAS in s_LANGU.

  If p_ZIP = 'X'.
    Perform ZIP_Init
      changing obj_Zip.
  EndIf.

  Case 'X'.

    when p_Import.

      Perform CheckTRKORR using 'E'.

      Perform SelectFilesForImport.

    when p_Export.

      Perform SelectDBForExport.

  EndCase.

  Call function 'TR_OBJECT_TABLE'
    TABLES
      WT_OBJECT_TEXT = it_OBJECT
    EXCEPTIONS
      others         = 1.
  Sort it_OBJECT by PGMID OBJECT.

  Loop at it_TADIR assigning <tadir>.
    Read table it_OBJECT
      with key PGMID = <tadir>-PGMID
               OBJECT = <tadir>-OBJECT
      binary search.
    If sy-subrc = 0.
      <tadir>-TEXT = it_OBJECT-TEXT.
    EndIf.
  EndLoop.


End-of-Selection.

  Create object Grid
    EXPORTING
      i_parent = CL_GUI_CONTAINER=>DEFAULT_SCREEN.
  Set handler ClGridEvent=>HandleDblClick for Grid.

  Perform CreateFieldCatalog.
  IS_LAYOUT-SEL_MODE = 'A'.
*  IS_LAYOUT-CWIDTH_OPT = 'X'.

  Call method Grid->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = IS_LAYOUT
    CHANGING
      IT_OUTTAB       = it_TADIR[]
      IT_FIELDCATALOG = it_FCat[].

  Call Selection-Screen 100.

*----------------------------------------------------------------------*

FORM F4_FlPath
  using P_FLPath type CHAR30.

  Data:
    w_Path     type RLGRAP-FILENAME,
*    w_Name     type RLGRAP-FILENAME,
    it_DYNPREAD type standard table of DYNPREAD
      with header line,
    sy_repid  type sy-repid.
  Field-symbols:
    <FldParam>.

  Assign (P_FLPATH) to <FldParam>.
  sy_repid = sy-repid.
  Refresh it_DYNPREAD. Clear it_DYNPREAD.
  it_DYNPREAD-FIELDNAME = P_FLPATH.
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
  w_Path = it_DYNPREAD-FIELDVALUE.

  CALL FUNCTION 'TMP_GUI_BROWSE_FOR_FOLDER'
    EXPORTING
*     WINDOW_TITLE          =
      INITIAL_FOLDER        = w_Path
    IMPORTING
      SELECTED_FOLDER       = w_Path
    EXCEPTIONS
      OTHERS                = 2
            .
  If sy-subrc = 0.
    <FldParam> = w_Path.
  EndIf.
ENDFORM.                                                    "F4_FlName

*&---------------------------------------------------------------------*
*&      Form  CreateFieldCatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
Form CreateFieldCatalog.
  Clear it_FCat.
  it_FCat-FieldName =  'PGMID'.
  it_FCat-Ref_Table = 'TADIR'.
  it_FCat-Ref_Field = 'PGMID'.
  it_FCat-OUTPUTLEN = 5.
  Append it_FCat.

  Clear it_FCat.
  it_FCat-FieldName =  'DEVCLASS'.
  it_FCat-Ref_Table = 'TADIR'.
  it_FCat-Ref_Field = 'DEVCLASS'.
  Append it_FCat.

  Clear it_FCat.
  it_FCat-FieldName =  'OBJECT'.
  it_FCat-Ref_Table = 'TADIR'.
  it_FCat-Ref_Field = 'OBJECT'.
  it_FCat-OUTPUTLEN = 5.
  Append it_FCat.

  Clear it_FCat.
  it_FCat-FieldName =  'TEXT'.
  it_FCat-Ref_Table = 'KO100'.
  it_FCat-Ref_Field = 'TEXT'.
  it_FCat-OUTPUTLEN = 20.
  Append it_FCat.

  Clear it_FCat.
  it_FCat-FieldName =  'OBJ_NAME'.
  it_FCat-Ref_Table = 'TADIR'.
  it_FCat-Ref_Field = 'OBJ_NAME'.
  Append it_FCat.

  Clear it_FCat.
  it_FCat-FieldName =  'STATUS'.
  it_FCat-OUTPUTLEN = 1.
  Append it_FCat.


  Clear it_FCat.
  it_FCat-FieldName =  'ICON'.
  it_FCat-OUTPUTLEN = 4.
  it_FCat-ICON = 'X'.
  Append it_FCat.

  Clear it_FCat.
  it_FCat-FieldName =  'MESSAGE'.
  it_FCat-OUTPUTLEN = 30.
  Append it_FCat.

EndForm.                    "CreateFieldCatalog

*&---------------------------------------------------------------------*
*&      Form  Read_Catalog
*&---------------------------------------------------------------------*
Form Read_Catalog
  using
    p_Path
    p_Filter
    p_FD
  changing
    it_List type table."tt_String.

  If p_Zip is initial.
    Perform GUI_Read_Catalog
      using
        p_Path
        p_Filter
        p_FD
      changing
        it_List[].
  Else.
    Perform ZIP_Read_Catalog
      using
        p_Path
        p_Filter
        p_FD
      changing
        it_List[].
  EndIf.

EndForm.                    "Read_Catalog

*&---------------------------------------------------------------------*
*&      Form  GUI_Read_Catalog
*&---------------------------------------------------------------------*
Form GUI_Read_Catalog
  using
    p_Path
    p_Filter
    p_FD
  changing
    it_List type table."tt_String.

  Data:
    Dir type string,
    Filter type string,
    Dir_char type char255,
    Filt_char type char255,
*    w_List type string,
    flgDirOnly,
    flgFileOnly,
    Cnt type i,
    it_Cat type standard table of FILE_INFO
      with header line,
    it_FILE_TABLE type standard table of SDOKPATH
      with header line,
    it_DIR_TABLE  type standard table of SDOKPATH
      with header line.

  Field-symbols:
    <line>.


  Case p_FD.
    when 'F'.
      flgFileOnly = 'X'.
    when 'D'.
      flgDirOnly = 'X'.
  EndCase.

  If sy-saprl >= '600'.
    Dir = p_Path.
    Filter = p_Filter.

    Call method CL_GUI_FRONTEND_SERVICES=>('DIRECTORY_LIST_FILES')
      EXPORTING
        DIRECTORY        = Dir
        FILTER           = Filter
        FILES_ONLY       = flgFileOnly
        DIRECTORIES_ONLY = flgDirOnly
      CHANGING
        FILE_TABLE       = it_Cat[]
        COUNT            = Cnt
      EXCEPTIONS
        others           = 1.

    Loop at it_Cat.
*    w_List = it_Cat-FILENAME.
      Append initial line to it_List.
      Read table it_List assigning <line> index sy-tabix.
      <line> = it_Cat-FILENAME.
    EndLoop.
  Else.
    Dir_char = p_Path.
    Filt_char = p_Filter.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        DIRECTORY  = Dir_char
        FILTER     = Filt_char
      TABLES
        FILE_TABLE = it_FILE_TABLE[]
        DIR_TABLE  = it_DIR_TABLE[]
      EXCEPTIONS
        OTHERS     = 99.

    If flgDirOnly = 'X'.
      Loop at it_DIR_TABLE.
        Append initial line to it_List.
        Read table it_List assigning <line> index sy-tabix.
        <line> = it_DIR_TABLE.
      EndLoop.
    EndIf.
    If flgFileOnly = 'X'.
      Loop at it_FILE_TABLE.
        Append initial line to it_List.
        Read table it_List assigning <line> index sy-tabix.
        <line> = it_FILE_TABLE.
      EndLoop.
    EndIf.
  EndIf.
EndForm.                    "GUI_Read_Catalog

*----------------------------------------------------------------------*
*       CLASS Util DEFINITION
*----------------------------------------------------------------------*
Class Util definition.
  Public section.
    Class-Methods:
      FileName_Escape
        importing i_Name type any
        returning value(r_Escape) type string,
      FileName_unEscape
        importing i_Escape type any
        returning value(r_Name) type string.
EndClass.                    "Util DEFINITION

*----------------------------------------------------------------------*
*       CLASS Util IMPLEMENTATION
*----------------------------------------------------------------------*
Class Util implementation.

  Method FileName_Escape.
*      importing i_Name type C
*      returning value(r_Escape) type string.

    Data:
      l_Str type string,
      l_Symbol,
      l_Hex type string.
    Field-symbols:
      <x> type X.

    Assign l_Symbol to <x> casting type X.

    l_Str = i_Name.
    Do 2 times.
      Case sy-index.
        when 1.
          l_Symbol = '/'.
        when 2.
          l_Symbol = '\'.
        when others.
          Exit. "Do
      EndCase.

      l_Hex = <x>.
      Replace '00' in l_Hex with ''.
      Concatenate '%' l_Hex into l_Hex.

      Do.
        Replace l_Symbol in l_Str with l_Hex.
        If sy-subrc <> 0.
          Exit. "Do
        EndIf.
      EndDo.
    EndDo.
    r_Escape = l_Str.
  EndMethod.                    "FileName_Escape

  Method FileName_unEscape.
*        importing i_Escape type any
*        returning value(r_Name) type string.

    Data:
      l_Str type string,
      l_Symbol,
      l_Hex type string.
    Field-symbols:
      <x> type X.

    Assign l_Symbol to <x> casting type X.

    l_Str = i_Escape.
    Do 2 times.
      Case sy-index.
        when 1.
          l_Symbol = '/'.
        when 2.
          l_Symbol = '\'.
        when others.
          Exit. "Do
      EndCase.

      l_Hex = <x>.
      Replace '00' in l_Hex with ''.
      Concatenate '%' l_Hex into l_Hex.

      Do.
        Replace l_Hex in l_Str with l_Symbol.
        If sy-subrc <> 0.
          Exit. "Do
        EndIf.
      EndDo.
    EndDo.
    r_Name = l_Str.
  EndMethod.                    "FileName_unEscape
EndClass.                    "Util IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  FileScanForImport
*&---------------------------------------------------------------------*
Form SelectFilesForImport.
  Data:
    Dir type string,
    DirObj type string,
    Cnt type i,
    it_CatDEVC type tt_String "standard table of FILE_INFO
      with header line,
    it_CatOBJECT type tt_String "standard table of FILE_INFO
      with header line,
    lt_Dir type tt_String
      with header line.

  Dir = p_Path.

  If p_Zip is initial.
    Perform GUI_Read_Catalog
      using
        p_Path
        '*.DEVC'
        'D'
      changing
        it_CatDEVC[].
  Else.
    Perform GUI_Read_Catalog
      using
        p_Path
        '*.DEVC.ZIP'
        'F'
      changing
        it_CatDEVC[].
  EndIf.


  Loop at it_CatDEVC.
    Clear it_TADIR.
    Translate it_CatDEVC to upper case. "-FILENAME

    Split it_CatDEVC at '.' "-FILENAME
      into it_TADIR-DEVCLASS it_TADIR-OBJECT.
    Concatenate it_TADIR-DEVCLASS '.' it_TADIR-OBJECT "'\'
      into it_TADIR-Object_Path.

    it_TADIR-DEVCLASS = util=>FileName_unEscape( it_TADIR-DEVCLASS ).

    it_TADIR-OBJ_NAME = it_TADIR-DEVCLASS.
    it_TADIR-PGMID = 'R3TR'.

    check it_TADIR-DEVCLASS in s_CLASS.

    If it_TADIR-OBJECT   in s_OBJECT and
       it_TADIR-OBJ_NAME in s_NAME.
      Append it_TADIR.
    EndIf.

    Refresh it_CatOBJECT.

    If p_Zip is initial.
      Concatenate p_Path it_CatDEVC '\' into Dir. "
    Else.
      Data: ZipFileName type string.
      Concatenate p_Path it_CatDEVC into ZipFileName.
      Perform ZIP_Load
        using ZipFileName
        changing FileZIP.

*      Concatenate it_TADIR-DEVCLASS '.' it_TADIR-OBJECT '\'
*        into Dir.
      Concatenate it_TADIR-Object_Path '\' "#### ###### ZIP
        into Dir.
    EndIf.

    Perform Read_Catalog
      using
        Dir
        '*.*'
        'D'
      changing
        it_CatOBJECT[].

    Loop at it_CatOBJECT.
      Concatenate Dir it_CatOBJECT '\' into DirObj. "-FILENAME
      Translate it_CatOBJECT to upper case. "-FILENAME

      check it_CatOBJECT(4) in s_OBJECT. "-FILENAME
      check ( p_New = 'X' and not it_CatOBJECT+4 is initial ) "-FILENAME
         or ( p_Old = 'X' and it_CatOBJECT+4 is initial ) "-FILENAME
         or p_All = 'X'.

      Case it_CatOBJECT. "-FILENAME
        when others.
          Perform ScanObjectCatalog
            using DirObj
                  it_TADIR-DEVCLASS
                  it_CatOBJECT "-FILENAME
            changing it_TADIR[].
      EndCase.
    EndLoop.
  EndLoop.

  Sort it_TADIR by
    DEVCLASS
    OBJECT
    OBJ_NAME
    PGMID.
  Delete adjacent duplicates from it_TADIR.

  Perform CheckObjects changing it_TADIR[].
  Perform GetStatusObjects changing it_TADIR[].

EndForm.                    "FileScanForImport

*&---------------------------------------------------------------------*
*&      Form  ScanObjectCatalog
*&---------------------------------------------------------------------*
Form ScanObjectCatalog
  using Dir
        DEVCLASS
        Object_Folder
  changing it_Tadir type tt_TADIR.
  Data:
    w_TADIR type t_TADIR,
    Cnt type i,
    Ext type string,
    it_Cat type tt_String "standard table of FILE_INFO
      with header line.

  Perform Read_Catalog
    using
      Dir
      '*.*'
      'D'
    changing
      it_Cat[].

  Loop at it_Cat.
    Translate it_Cat to upper case. "-FILENAME
    Clear w_TADIR.
    w_TADIR-Object_Path = it_Cat.
    w_TADIR-OBJ_NAME = it_Cat. "-FILENAME.
    w_TADIR-OBJ_NAME = util=>FileName_unEscape( w_TADIR-OBJ_NAME ).
    w_TADIR-DEVCLASS = DEVCLASS.
    Split Object_Folder at '.'
      into w_TADIR-OBJECT w_TADIR-PGMID.
    If w_TADIR-PGMID is initial.
      w_TADIR-PGMID    = 'R3TR'.
      w_TADIR-Flag = flagOld.
    Else.
      w_TADIR-Flag = flagNew.
    EndIf.
    If w_TADIR-OBJ_NAME in s_NAME.
      Append w_TADIR to it_TADIR.
    EndIf.
  EndLoop.
EndForm.                    "ScanObjectCatalog

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_SHLD
*&---------------------------------------------------------------------*
Form Import_Limu_SHLD
  using
    Obj_Name
    Struc_SHLD type SVRS2_SHLD
    subrc.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    it_DD30V type standard table of DD30V
      with header line,
    it_DD31V type standard table of DD31V,
    it_DD32P type standard table of DD32P,
    it_DD33V type standard table of DD33V.

*         DD30TV TYPE DD30TV OCCURS 0,
*         DD30V TYPE DD30V OCCURS 0,
*         DD31V TYPE DD31V OCCURS 0,
*         DD32V TYPE DD32V OCCURS 0,
*         DD33V TYPE DD33V OCCURS 0,
*         MDLOG TYPE SMODILOG OCCURS 0,
  subrc = isError.

  DDOBJNAME  = Obj_Name.
  If not it_DD30V[] is initial.
    Read table it_DD30V index 1.
  EndIf.

  Perform Table_Corresponding
    tables:
      Struc_SHLD-DD30V it_DD30V,
      Struc_SHLD-DD31V it_DD31V,
      Struc_SHLD-DD32V it_DD32P,
      Struc_SHLD-DD33V it_DD33V.

  Call function 'DDIF_SHLP_PUT'
    EXPORTING
      NAME      = DDOBJNAME
      DD30V_WA  = it_DD30V
    TABLES
      DD31V_TAB = it_DD31V
      DD32P_TAB = it_DD32P
      DD33V_TAB = it_DD33V
    EXCEPTIONS
      others    = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_Limu_SHLD

*&---------------------------------------------------------------------*
*&      Form  ImportSHLP
*&---------------------------------------------------------------------*
Form ImportSHLP
  using p_TADIR type t_TADIR
        Path
        subrc.
  Types:
    Begin of t_SHLP,
      DD30V type DD30V,
      it_DD31V type standard table of DD31V
        with key table_line,
      it_DD32P type standard table of DD32P
        with key table_line,
      it_DD33V type standard table of DD33V
        with key table_line,
    End of t_SHLP.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD30V type standard table of DD30V,
    NewSHLP type t_SHLP,
    OldSHLP type t_SHLP,
    sy_subrc type sy-subrc.

  subrc = isError.

  Concatenate Path p_TADIR-OBJ_NAME '\DD30V.txt' into FileName.
  Perform Read_File using FileName changing it_DD30V[].
  If sy-subrc = 0.
    Read table it_DD30V into NewSHLP-DD30V index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\DD31V.txt' into FileName.
  Perform Read_File using FileName changing NewSHLP-it_DD31V.

  Concatenate Path p_TADIR-OBJ_NAME '\DD32P.txt' into FileName.
  Perform Read_File using FileName changing NewSHLP-it_DD32P.

  Concatenate Path p_TADIR-OBJ_NAME '\DD33V.txt' into FileName.
  Perform Read_File using FileName changing NewSHLP-it_DD33V.

  DDOBJNAME = p_TADIR-OBJ_NAME.

  If p_Rewrte is initial.
    Call function 'DDIF_SHLP_GET'
      EXPORTING
        NAME      = DDOBJNAME
        LANGU     = sy-langu
      IMPORTING
        DD30V_WA  = OldSHLP-DD30V
      TABLES
        DD31V_TAB = OldSHLP-it_DD31V
        DD32P_TAB = OldSHLP-it_DD32P
        DD33V_TAB = OldSHLP-it_DD33V
      EXCEPTIONS
        others    = 1.
  EndIf.

  If NewSHLP <> OldSHLP and
     not NewSHLP is initial.
    Call function 'DDIF_SHLP_PUT'
      EXPORTING
        NAME      = DDOBJNAME
        DD30V_WA  = NewSHLP-DD30V
      TABLES
        DD31V_TAB = NewSHLP-it_DD31V
        DD32P_TAB = NewSHLP-it_DD32P
        DD33V_TAB = NewSHLP-it_DD33V
      EXCEPTIONS
        others    = 1.

    If sy_subrc <> 0.
      Perform WriteMessageTo using sy-subrc changing p_TADIR.
    Else.
      Perform SetClassForObject using p_TADIR.

      If sy-subrc <> 0.
        Perform WriteMessageTo using sy-subrc changing p_TADIR.
      Else.
        Perform AppendToQuery using p_TADIR.
        subrc = isOk.

        If sy-subrc <> 0.
          Perform WriteMessageTo using sy-subrc changing p_TADIR.
        Else.
*          Call function 'DDIF_SHLP_ACTIVATE'
*            EXPORTING
*              NAME   = DDOBJNAME
*            EXCEPTIONS
*              others = 1.
*          Perform WriteMessageTo using sy-subrc changing p_TADIR.
        EndIf.
      EndIf.
    EndIf.
  EndIf.
EndForm.                    "ImportSHLP



*&---------------------------------------------------------------------*
*&      Form  ImportDOMA
*&---------------------------------------------------------------------*
Form ImportDOMA
  using p_TADIR type t_TADIR
        Path
        subrc.

  Types:
    Begin of t_DOMA,
      DD01V type DD01V,
      it_DD07V type standard table of DD07V
        with key table_line,
    End of t_DOMA.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD01V type standard table of DD01V
      with header line,
    NewDOMA type t_DOMA,
    OldDOMA type t_DOMA.

  subrc = isError.

  Concatenate Path p_TADIR-OBJ_NAME '\DD01V.txt' into FileName.
  Perform Read_File using FileName changing it_DD01V[].
  If sy-subrc = 0.
    Read table it_DD01V into NewDOMA-DD01V index 1.
  EndIf.


  Concatenate Path p_TADIR-OBJ_NAME '\DD07V.txt' into FileName.
  Perform Read_File using FileName changing NewDOMA-it_DD07V.

  DDOBJNAME = p_TADIR-OBJ_NAME.

  If p_Rewrte is initial.
    Call function 'DDIF_DOMA_GET'
      EXPORTING
        NAME      = DDOBJNAME
        LANGU     = sy-langu
      IMPORTING
        DD01V_WA  = OldDOMA-DD01V
      TABLES
        DD07V_TAB = OldDOMA-it_DD07V
      EXCEPTIONS
        others    = 1.
  EndIf.

  If NewDOMA <> OldDOMA and
     not NewDOMA is initial.

    Call function 'DDIF_DOMA_PUT'
      EXPORTING
        NAME      = DDOBJNAME
        DD01V_WA  = NewDOMA-DD01V
      TABLES
        DD07V_TAB = NewDOMA-it_DD07V
      EXCEPTIONS
        others    = 1.

    If sy-subrc = 0.
      Perform SetClassForObject using p_TADIR.

      If sy-subrc = 0.
        Perform AppendToQuery using p_TADIR.
        subrc = isOk.
*        Call function 'DDIF_DOMA_ACTIVATE'
*          EXPORTING
*            NAME   = DDOBJNAME
*          EXCEPTIONS
*            others = 1.
      EndIf.
    EndIf.
  EndIf.

EndForm.                    "ImportDOMA

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_DOMD
*&---------------------------------------------------------------------*
Form Import_LIMU_DOMD
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME.
  Field-symbols:
    <DOMD> type SVRS2_DOMD,
    <DD01V>.

  subrc = isError.

  Assign Struc_LIMU to <DOMD>.

  check not <DOMD>-DD01V[] is initial.
  Read table <DOMD>-DD01V[] assigning <DD01V> index 1.

  DDOBJNAME = Obj_Name.

  Call function 'DDIF_DOMA_PUT'
    EXPORTING
      NAME      = DDOBJNAME
      DD01V_WA  = <DD01V>
    TABLES
      DD07V_TAB = <DOMD>-DD07V
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_LIMU_DOMD
*&---------------------------------------------------------------------*
*&      Form  ImportDTEL
*&---------------------------------------------------------------------*
Form ImportDTEL
  using p_TADIR type t_TADIR
        Path
        subrc.

  Types:
    Begin of t_DTEL,
      DD04V type DD04V,
      TPARA type TPARA,
    End of t_DTEL.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD04V type standard table of DD04V,
    it_TPARA type standard table of TPARA,
    NewDTEL type t_DTEL,
    OldDTEL type t_DTEL.

  subrc = isError.

  Concatenate Path p_TADIR-OBJ_NAME '\DD04V.txt' into FileName.
  Perform Read_File using FileName changing it_DD04V[].
  If sy-subrc = 0.
    Read table it_DD04V into NewDTEL-DD04V index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\TPARA.txt' into FileName.
  Perform Read_File using FileName changing it_TPARA[].
  If sy-subrc = 0.
    Read table it_TPARA into NewDTEL-TPARA index 1.
  EndIf.

  DDOBJNAME = p_TADIR-OBJ_NAME.

  If p_Rewrte is initial.
    Call function 'DDIF_DTEL_GET'
      EXPORTING
        NAME     = DDOBJNAME
        LANGU    = sy-langu
      IMPORTING
        DD04V_WA = OldDTEL-DD04V
        TPARA_WA = OldDTEL-TPARA
      EXCEPTIONS
        others   = 1.
  EndIf.

  If NewDTEL <> OldDTEL and
     not NewDTEL is initial.

    Call function 'DDIF_DTEL_PUT'
      EXPORTING
        NAME     = DDOBJNAME
        DD04V_WA = NewDTEL-DD04V
      EXCEPTIONS
        others   = 1.
    If sy-subrc = 0.
      Perform SetClassForObject using p_TADIR.

      If sy-subrc = 0.
        Perform AppendToQuery using p_TADIR.
        subrc = isOk.
*        Call function 'DDIF_DTEL_ACTIVATE'
*          EXPORTING
*            NAME   = DDOBJNAME
*          EXCEPTIONS
*            others = 1.
      EndIf.
    EndIf.
  EndIf.
EndForm.                    "ImportDTEL

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_DTEL
*&---------------------------------------------------------------------*
Form Import_LIMU_DTED
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME.
  Field-symbols:
    <DTED> type SVRS2_DTED,
    <DD04V>.

  subrc = isError.

  Assign Struc_LIMU to <DTED>.

  check not <DTED>-DD04V[] is initial.
  Read table <DTED>-DD04V[] assigning <DD04V> index 1.

  DDOBJNAME = Obj_Name.

  Call function 'DDIF_DTEL_PUT'
    EXPORTING
      NAME     = DDOBJNAME
      DD04V_WA = <DD04V>
    EXCEPTIONS
      others   = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_LIMU_DTEL


*&---------------------------------------------------------------------*
*&      Form  ImportVIEW
*&---------------------------------------------------------------------*
Form ImportVIEW
  using p_TADIR type t_TADIR
        Path
        subrc.

  Types:
    Begin of t_View,
      DD25V type DD25V,
      DD09V type DD09V,
      it_DD26V type standard table of DD26V
        with key table_line,
      it_DD27P type standard table of DD27P
        with key table_line,
      it_DD28J type standard table of DD28J
        with key table_line,
      it_DD28V type standard table of DD28V
        with key table_line,
      it_TVDIR type standard table of TVDIR
        with key table_line,
      it_TDDAT type standard table of TDDAT
        with key table_line,
    End of t_View.

  Data:
    it_DD25V type standard table of DD25V,
    it_DD09V type standard table of DD09V,
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    ViewOld type t_View,
    ViewNew type t_View,
    w_E071 type E071.

  subrc = isError.

  Concatenate Path p_TADIR-OBJ_NAME '\DD25V.txt' into FileName.
  Perform Read_File using FileName changing it_DD25V.
  If sy-subrc = 0.
    Read table it_DD25V into ViewNew-DD25V index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\DD09V.txt' into FileName.
  Perform Read_File using FileName changing it_DD09V.
  If sy-subrc = 0.
    Read table it_DD09V into ViewNew-DD09V index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\DD26V.txt' into FileName.
  Perform Read_File using FileName changing ViewNew-it_DD26V.

  Concatenate Path p_TADIR-OBJ_NAME '\DD27P.txt' into FileName.
  Perform Read_File using FileName changing ViewNew-it_DD27P.

  Concatenate Path p_TADIR-OBJ_NAME '\DD28J.txt' into FileName.
  Perform Read_File using FileName changing ViewNew-it_DD28J.

  Concatenate Path p_TADIR-OBJ_NAME '\DD28V.txt' into FileName.
  Perform Read_File using FileName changing ViewNew-it_DD28V.

  Concatenate Path p_TADIR-OBJ_NAME '\TVDIR.txt' into FileName.
  Perform Read_File using FileName changing ViewNew-it_TVDIR.

  Concatenate Path p_TADIR-OBJ_NAME '\TDDAT.txt' into FileName.
  Perform Read_File using FileName changing ViewNew-it_TDDAT.

  DDOBJNAME = p_TADIR-OBJ_NAME.

  If p_Rewrte is initial.
    Call function 'DDIF_VIEW_GET'
      EXPORTING
        NAME      = DDOBJNAME
      IMPORTING
        DD25V_WA  = ViewOld-DD25V
        DD09L_WA  = ViewOld-DD09V
      TABLES
        DD26V_TAB = ViewOld-it_DD26V
        DD27P_TAB = ViewOld-it_DD27P
        DD28J_TAB = ViewOld-it_DD28J
        DD28V_TAB = ViewOld-it_DD28V
      EXCEPTIONS
        others    = 1.

    If sy-subrc = 0.
      Select *
        into table ViewOld-it_TVDIR
        from TVDIR
        where TABNAME = DDOBJNAME.

      Select *
        into table ViewOld-it_TDDAT
        from TDDAT
        where TABNAME = DDOBJNAME.
    EndIf.
  EndIf.

  If ViewNew <> ViewOld and
     not ViewNew is initial.
    Call function 'DDIF_VIEW_PUT'
      EXPORTING
        NAME      = DDOBJNAME
        DD25V_WA  = ViewNew-DD25V
        DD09L_WA  = ViewNew-DD09V
      TABLES
        DD26V_TAB = ViewNew-it_DD26V
        DD27P_TAB = ViewNew-it_DD27P
        DD28J_TAB = ViewNew-it_DD28J
        DD28V_TAB = ViewNew-it_DD28V
      EXCEPTIONS
        others    = 1.
    If sy-subrc = 0.
      Modify TVDIR from table ViewNew-it_TVDIR.
      Modify TDDAT from table ViewNew-it_TDDAT.

      Perform SetClassForObject using p_TADIR.

      If sy-subrc = 0.
        Perform AppendToQuery using p_TADIR.
        subrc = isOk.
*        Call function 'DDIF_VIEW_ACTIVATE'
*          EXPORTING
*            NAME   = DDOBJNAME
*          EXCEPTIONS
*            others = 1.
      EndIf.
    EndIf.
  EndIf.

EndForm.                    "ImportVIEW

*&---------------------------------------------------------------------*
*&      Form  ImportTABL
*&---------------------------------------------------------------------*
Form ImportTABL
  using p_TADIR type t_TADIR
        Path
        subrc.
  Types:
    Begin of t_TABL,
      DD02V type DD02V,
      DD09L type DD09L,
      it_DD03P type standard table of DD03P with key table_line,
      it_DD05M type standard table of DD05M with key table_line,
      it_DD08V type standard table of DD08V with key table_line,
      it_DD12V type standard table of DD12V with key table_line,
      it_DD17V type standard table of DD17V with key table_line,
      it_DD35V type standard table of DD35V with key table_line,
      it_DD36M type standard table of DD36M with key table_line,
    End of t_TABL.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD02V_File type standard table of DD02V,
    it_DD09L_File type standard table of DD09L,
    it_DD03P_File type standard table of DD03P with header line,
    it_DD05M_File type standard table of DD05M with header line,
    it_DD08V_File type standard table of DD08V with header line,
    it_DD12V_File type standard table of DD12V with header line,
    it_DD17V_File type standard table of DD17V with header line,
    it_DD35V_File type standard table of DD35V with header line,
    it_DD36M_File type standard table of DD36M with header line,
    TablNew type t_TABL,
    TablOld type t_TABL,
    w_DD12V type DD12V,
    RC type sy-subrc.

  subrc = isError.

  Concatenate Path p_TADIR-OBJ_NAME '\DD02V.txt' into FileName.
  Perform Read_File using FileName changing it_DD02V_File.
  Sort it_DD02V_File.
  Delete adjacent duplicates from it_DD02V_File.

  Concatenate Path p_TADIR-OBJ_NAME '\DD09L.txt' into FileName.
  Perform Read_File using FileName changing it_DD09L_File.
  Sort it_DD09L_File.
  Delete adjacent duplicates from it_DD09L_File.

  Concatenate Path p_TADIR-OBJ_NAME '\DD03P.txt' into FileName.
  Perform Read_File using FileName changing it_DD03P_File[].
  Sort it_DD03P_File[].
  Delete adjacent duplicates from it_DD03P_File[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD05M.txt' into FileName.
  Perform Read_File using FileName changing it_DD05M_File[].
  Sort it_DD05M_File[].
  Delete adjacent duplicates from it_DD05M_File[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD08V.txt' into FileName.
  Perform Read_File using FileName changing it_DD08V_File[].
  Sort it_DD08V_File[].
  Delete adjacent duplicates from it_DD08V_File[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD12V.txt' into FileName.
  Perform Read_File using FileName changing it_DD12V_File[].
  Sort it_DD12V_File[].
  Delete adjacent duplicates from it_DD12V_File[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD17V.txt' into FileName.
  Perform Read_File using FileName changing it_DD17V_File[].
  Sort it_DD17V_File[].
  Delete adjacent duplicates from it_DD17V_File[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD35V.txt' into FileName.
  Perform Read_File using FileName changing it_DD35V_File[].
  Sort it_DD35V_File[].
  Delete adjacent duplicates from it_DD35V_File[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD36M.txt' into FileName.
  Perform Read_File using FileName changing it_DD36M_File[].
  Sort it_DD36M_File[].
  Delete adjacent duplicates from it_DD36M_File[].

  DDOBJNAME = p_TADIR-OBJ_NAME.

  Loop at it_LANGU.
    Clear TablNew.

    Loop at it_DD02V_File
      into TablNew-DD02V
      where DDLANGUAGE = space.
    EndLoop.
    Loop at it_DD02V_File
      into TablNew-DD02V
      where DDLANGUAGE = it_LANGU.
    EndLoop.

    Loop at it_DD09L_File
      into TablNew-DD09L.
    EndLoop.

    Loop at it_DD03P_File
      where DDLANGUAGE = it_LANGU
         or DDLANGUAGE = space.
      Append it_DD03P_File to TablNew-it_DD03P.
    EndLoop.
    Sort TablNew-it_DD03P descending.
    Delete adjacent duplicates from TablNew-it_DD03P
      comparing TABNAME
                FIELDNAME
                POSITION.

    Loop at it_DD05M_File.
      Append it_DD05M_File to TablNew-it_DD05M.
    EndLoop.

    Loop at it_DD08V_File
      where DDLANGUAGE = it_LANGU
         or DDLANGUAGE = space.
      Append it_DD08V_File to TablNew-it_DD08V.
    EndLoop.
    Sort TablNew-it_DD08V descending.
    Delete adjacent duplicates from TablNew-it_DD08V
      comparing TABNAME
                FIELDNAME.

    Loop at it_DD12V_File
      where DDLANGUAGE = it_LANGU
         or DDLANGUAGE = space.
      Append it_DD12V_File to TablNew-it_DD12V.
    EndLoop.
    Sort TablNew-it_DD12V descending.
    Delete adjacent duplicates from TablNew-it_DD12V
      comparing SQLTAB
                INDEXNAME
                AS4LOCAL
                AS4VERS.

    Loop at it_DD17V_File
      where DDLANGUAGE = it_LANGU
         or DDLANGUAGE = space.
      Append it_DD17V_File to TablNew-it_DD17V.
    EndLoop.
    Sort TablNew-it_DD17V descending.
    Delete adjacent duplicates from TablNew-it_DD17V
      comparing SQLTAB
                INDEXNAME
                POSITION
                AS4LOCAL.

    Loop at it_DD35V_File.
      Append it_DD35V_File to TablNew-it_DD35V.
    EndLoop.

    Loop at it_DD36M_File.
      Append it_DD36M_File to TablNew-it_DD36M.
    EndLoop.

    If p_Rewrte is initial.
      Call function 'DDIF_TABL_GET'
        EXPORTING
          NAME      = DDOBJNAME
          LANGU     = it_LANGU
        IMPORTING
          DD02V_WA  = TablOld-DD02V
          DD09L_WA  = TablOld-DD09L
        TABLES
          DD03P_TAB = TablOld-it_DD03P
          DD05M_TAB = TablOld-it_DD05M
          DD08V_TAB = TablOld-it_DD08V
          DD12V_TAB = TablOld-it_DD12V
          DD17V_TAB = TablOld-it_DD17V
          DD35V_TAB = TablOld-it_DD35V
          DD36M_TAB = TablOld-it_DD36M
        EXCEPTIONS
          others    = 1.
    EndIf.

    If TablNew <> TablOld and
       not TablNew is initial.

      Call function 'DDIF_TABL_PUT'
        EXPORTING
          NAME      = DDOBJNAME
          DD02V_WA  = TablNew-DD02V
          DD09L_WA  = TablNew-DD09L
        TABLES
          DD03P_TAB = TablNew-it_DD03P
          DD05M_TAB = TablNew-it_DD05M
          DD08V_TAB = TablNew-it_DD08V
          DD35V_TAB = TablNew-it_DD35V
          DD36M_TAB = TablNew-it_DD36M
        EXCEPTIONS
          others    = 1.

      If sy-subrc <> 0.
        Perform WriteMessageTo using sy-subrc changing p_TADIR.
      Else.
        Perform SetClassForObject using p_TADIR.

        If sy-subrc <> 0.
          Perform WriteMessageTo using sy-subrc changing p_TADIR.
        Else.
          Perform AppendToQuery using p_TADIR.
          subrc = isOk.

*        Call function 'DDIF_TABL_ACTIVATE'
*          EXPORTING
*            NAME   = DDOBJNAME
*          IMPORTING
*            RC     = RC
*          EXCEPTIONS
*            others = 1.
*        If RC <> 0.
*          Perform WriteMessageTo using sy-subrc changing p_TADIR.
*
**          Data:
**            IV_LOGNAME_MEMORY type TSTRF01-FILENAME,
**            ET_LINES type standard table of TRLOG.
**          Concatenate 'CHETABL' DDOBJNAME into IV_LOGNAME_MEMORY.
**          Call function 'TR_READ_LOG'
**            exporting
**              IV_LOG_TYPE = 'MEMORY'
**              IV_LOGNAME_MEMORY = IV_LOGNAME_MEMORY
**            tables
**              ET_LINES = ET_LINES.
*        EndIf.

          Loop at TablNew-it_DD12V into w_DD12V.
            Data:
              it_Temp_DD17V like TablNew-it_DD17V.

            it_Temp_DD17V = TablNew-it_DD17V.
            Delete it_Temp_DD17V
              where not ( SQLTAB = w_DD12V-SQLTAB and
                          INDEXNAME = w_DD12V-INDEXNAME ).

            Call function 'DDIF_INDX_PUT'
              EXPORTING
                NAME      = DDOBJNAME
                ID        = w_DD12V-INDEXNAME
                DD12V_WA  = w_DD12V
              TABLES
                DD17V_TAB = it_Temp_DD17V
              EXCEPTIONS
                others    = 1.

            If sy-subrc = 0.
*            Call function 'DDIF_INDX_ACTIVATE'
*              EXPORTING
*                NAME   = DDOBJNAME
*                ID     = w_DD12V-INDEXNAME
*              EXCEPTIONS
*                others = 1.
            EndIf.
          EndLoop.
        EndIf.
      EndIf.
    EndIf.
  EndLoop.
EndForm.                    "ImportTABL


*&---------------------------------------------------------------------*
*&      Form  Table_Corresponding
*&---------------------------------------------------------------------*
Form Table_Corresponding
  tables Tab1 Tab2.

  Data:
    it_FieldName type tt_String
      with header line.
  Field-symbols:
    <f1>,
    <f2>.

  Perform Get_Struc
    tables it_FieldName
    using Tab1.

  Refresh Tab2.
  Loop at Tab1.

*    Move-corresponding: Tab1 to Tab2. "# 4.6 ## ########
    Loop at it_FieldName.
      Assign component it_FieldName of structure Tab1 to <f1>.
      check sy-subrc = 0.
      Assign component it_FieldName of structure Tab2 to <f2>.
      check sy-subrc = 0.
      <f2> = <f1>.
    EndLoop.

    Append Tab2.
  EndLoop.
EndForm.                    "Table_Corresponding

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_TABD
*&---------------------------------------------------------------------*
Form Import_LIMU_TABD
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME,
    DD03P_TAB type table of DD03P,
    DD05M_TAB type table of DD05M,
    DD36M_TAB type table of DD36M.

  Field-symbols:
    <TABD> type SVRS2_TABD,
    <DD02V> type line of SVRS2_TABD-DD02V.

  subrc = isError.

  Assign Struc_LIMU to <TABD>.

  check not <TABD>-DD02V[] is initial.
  Read table <TABD>-DD02V[] assigning <DD02V> index 1.

  Perform Table_Corresponding tables:
    <TABD>-DD03V DD03P_TAB,
    <TABD>-DD05V DD05M_TAB,
    <TABD>-DD36V DD36M_TAB.

  DDOBJNAME = Obj_Name.
  Call function 'DD_TABD_PUT' "'DDIF_TABL_PUT'
    EXPORTING
*      NAME      = DDOBJNAME
      DD02V_WA  = <DD02V>
      TABL_NAME = <DD02V>-TABNAME
*          DD09L_WA  = TablNew-DD09L
    TABLES
      DD03P_TAB = DD03P_TAB
*      DD05M_TAB = DD05M_TAB
      DD08V_TAB = <TABD>-DD08V
      DD35V_TAB = <TABD>-DD35V
      DD36M_TAB = DD36M_TAB
    EXCEPTIONS
      others    = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_TABD

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_VIED
*&---------------------------------------------------------------------*
Form Import_LIMU_VIED
  using
    Obj_Name
    Struc_VIED type SVRS2_VIED
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME,
    it_DD25V type standard table of DD25V
      with header line,
*    it_DD09V type standard table of DD09V
*      with key table_line,
    it_DD26V type standard table of DD26V,
    it_DD27P type standard table of DD27P,
    it_DD28J type standard table of DD28J,
    it_DD28V type standard table of DD28V.

  subrc = isError.

  DDOBJNAME = Obj_Name.

  Perform Table_Corresponding
    tables:
      Struc_VIED-DD25V it_DD25V,
      Struc_VIED-DD26V it_DD26V,
      Struc_VIED-DD27V it_DD27P,
      Struc_VIED-DD28V it_DD28V.
*         DD25TV TYPE DD25TV OCCURS 0,
*         DD25V TYPE DD25V OCCURS 0,
*         DD26V TYPE DD26V OCCURS 0,
*         DD27V TYPE DD27V OCCURS 0,
*         DD28V TYPE DD28V OCCURS 0,
*         MDLOG TYPE SMODILOG OCCURS 0,

  If not it_DD25V[] is initial.
    Read table it_DD25V index 1.
  EndIf.

  Call function 'DDIF_VIEW_PUT'
    EXPORTING
      NAME      = DDOBJNAME
      DD25V_WA  = it_DD25V
*      DD09L_WA  = it_DD09V
    TABLES
      DD26V_TAB = it_DD26V[]
      DD27P_TAB = it_DD27P[]
      DD28J_TAB = it_DD28J[]
      DD28V_TAB = it_DD28V[]
    EXCEPTIONS
      others    = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_VIED


*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_VIET
*&---------------------------------------------------------------------*
Form Import_LIMU_VIET
  using
    Obj_Name
    Struc_VIET type SVRS2_VIET
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME,
    it_DD09V type standard table of DD09V
      with header line.

  subrc = isError.

  DDOBJNAME = Obj_Name.

  Perform Table_Corresponding
    tables:
      Struc_VIET-DD09V it_DD09V.

  If not it_DD09V[] is initial.
    Read table it_DD09V index 1.
  EndIf.

  Call function 'DDIF_VIEW_PUT'
    EXPORTING
      NAME     = DDOBJNAME
      DD09L_WA = it_DD09V
    EXCEPTIONS
      others   = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_VIET

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_TABT
*&---------------------------------------------------------------------*
Form Import_LIMU_TABT
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME.

  Field-symbols:
    <TABT> type SVRS2_TABT,
    <DD09V>.

  subrc = isError.

  Assign Struc_LIMU to <TABT>.

  check not <TABT>-DD09V[] is initial.
  Read table <TABT>-DD09V[] assigning <DD09V> index 1.

  DDOBJNAME = Obj_Name.

  Call function 'DDIF_TABT_PUT'
    EXPORTING
      NAME     = DDOBJNAME
      DD09L_WA = <DD09V>
    EXCEPTIONS
      others   = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_TABT

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_TTYD
*&---------------------------------------------------------------------*
Form Import_LIMU_TTYD
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME.

  Field-symbols:
    <TTYD> type SVRS2_TTYD,
    <DD40V>.

  subrc = isError.

  Assign Struc_LIMU to <TTYD>.

  check not <TTYD>-DD40V[] is initial.
  Read table <TTYD>-DD40V[] assigning <DD40V> index 1.

  DDOBJNAME = Obj_Name.

  Call function 'DDIF_TTYP_PUT'
    EXPORTING
      NAME      = DDOBJNAME
      DD40V_WA  = <DD40V>
    TABLES
      DD42V_TAB = <TTYD>-DD42V[]
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_LIMU_TTYD

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_INDX
*&---------------------------------------------------------------------*
Form Import_LIMU_INDX
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME,
    StrDummy type string.

  Field-symbols:
    <INDX> type SVRS2_INDX,
    <DD12V> type line of SVRS2_INDX-DD12V.

  subrc = isError.

  Assign Struc_LIMU to <INDX>.

  check not <INDX>-DD12V[] is initial.
  Read table <INDX>-DD12V[] assigning <DD12V> index 1.

  Split Obj_Name at space into DDOBJNAME StrDummy.

  Call function 'DDIF_INDX_PUT'
    EXPORTING
      NAME      = DDOBJNAME
      ID        = <DD12V>-INDEXNAME
      DD12V_WA  = <DD12V>
    TABLES
      DD17V_TAB = <INDX>-DD17V
    EXCEPTIONS
      others    = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_INDX



*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_TRAN
*&---------------------------------------------------------------------*
Form Import_R3TR_TRAN
  using w_TADIR type t_TADIR
        struc_TRAN type t_R3TR_TRAN
        subrc.

  Data:
    DDOBJNAME type  DDOBJNAME.

  subrc = isError.

  Modify TSTC  from table struc_TRAN-TSTC.
  Modify TSTCA from table struc_TRAN-TSTCA.
  Modify TSTCT from table struc_TRAN-TSTCT.
  Modify TSTCP from table struc_TRAN-TSTCP.
  Modify TSTCC from table struc_TRAN-TSTCC.

*  If sy-subrc = 0.
  subrc = isOk.
*  EndIf.
EndForm.                    "Import_R3TR_TRAN

*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_TOBJ
*&---------------------------------------------------------------------*
Form Import_R3TR_TOBJ
  using w_TADIR type t_TADIR
        struc_TOBJ type t_R3TR_TOBJ
        subrc.
  Data:
    IV_OBJ_NAME   type E071-OBJ_NAME,
    IV_OBJECTNAME type OBJH-OBJECTNAME,
    IV_OBJECTTYPE type OBJH-OBJECTTYPE,
    w_TVIMF type TVIMF.

  subrc = isError.

  IV_OBJ_NAME = w_TADIR-Obj_Name.

  CALL FUNCTION 'CTO_TADIR_GET_OBJECT'
    EXPORTING
      IV_OBJ_NAME   = IV_OBJ_NAME
    IMPORTING
      EV_OBJECTNAME = IV_OBJECTNAME
      EV_OBJECTTYPE = IV_OBJECTTYPE
    EXCEPTIONS
      others        = 99.
  check sy-subrc = 0.

  Delete from OBJH where OBJECTNAME = IV_OBJECTNAME.
  Delete from OBJT where OBJECTNAME = IV_OBJECTNAME.
  Delete from OBJS where OBJECTNAME = IV_OBJECTNAME.
  Delete from OBJSL where OBJECTNAME = IV_OBJECTNAME.
  Delete from OBJM where OBJECTNAME = IV_OBJECTNAME.
  Delete from TDDAT where TABNAME = IV_OBJECTNAME.
  Delete from TVDIR where TABNAME = IV_OBJECTNAME.
  Delete from TVIMF where TABNAME = IV_OBJECTNAME.

  Modify OBJH  from table struc_TOBJ-OBJH[].
  Modify OBJT  from table struc_TOBJ-OBJT[].
  Modify OBJS  from table struc_TOBJ-OBJS[].
  Modify OBJSL from table struc_TOBJ-OBJSL[].
  Modify OBJM  from table struc_TOBJ-OBJM[].
  Modify TDDAT  from table struc_TOBJ-TDDAT[].
  Modify TVDIR  from table struc_TOBJ-TVDIR[].
  Modify TVIMF  from table struc_TOBJ-TVIMF[].

*  If sy-subrc = 0.
  subrc = isOk.
*  EndIf.

EndForm.                    "Import_R3TR_TOBJ


*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_FUGR
*&---------------------------------------------------------------------*
Form Import_R3TR_FUGR
  using w_TADIR type t_TADIR
        struc_FUGR type t_R3TR_FUGR
        subrc.
  Data:
    FUNCTION_POOL type TLIBG-AREA,
    w_TLIBT type TLIBT.

  subrc = isError.

  FUNCTION_POOL = w_TADIR-Obj_Name.
  w_TLIBT-AREAT = w_TADIR-Text.

  If not struc_FUGR-TLIBT[] is initial.
    Read table struc_FUGR-TLIBT[] into w_TLIBT index 1.
  EndIf.

  Perform DeleteFUGR
    using FUNCTION_POOL.

  Call function 'FUNCTION_POOL_CREATE'
    EXPORTING
      POOL_NAME     = FUNCTION_POOL
      SHORT_TEXT    = w_TLIBT-AREAT
    EXCEPTIONS
      error_message = 2
      others        = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_R3TR_FUGR

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_FUGT
*&---------------------------------------------------------------------*
Form Import_Limu_FUGT
  using
    Obj_Name
    Struc_FUGT type t_Limu_FUGT
    subrc.

  subrc = isError.

  Modify TLIBT from table Struc_FUGT-TLIBT[].

  If sy-subrc = 0.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_Limu_FUGT

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_FUNC
*&---------------------------------------------------------------------*
Form Import_Limu_FUNC
  using
    Obj_Name
    Struc_FUNC type SVRS2_FUNC
    subrc.

  Field-symbols:
    <FUPAR> type line of SVRS2_FUNC-FUPAR.

  Data:
    FUNCNAME type  RS38L-NAME,
    it_ImportParameters type standard table of RSIMP
      with header line,
    it_ExportParameters type standard table of RSEXP
      with header line,
    it_TablesParameters type standard table of RSTBL
      with header line,
    it_ChangeParameters type standard table of RSCHA
      with header line,
    it_ExceptionList type standard table of RSEXC
      with header line,
    it_DocParameters type standard table of RSFDO
      with header line,
    w_TFTIT type line of SVRS2_FUNC-TFTIT,
    w_ENLFD type line of SVRS2_FUNC-ENLFD.

  subrc = isError.

  If not Struc_FUNC-TFTIT[] is initial.
    Read table Struc_FUNC-TFTIT[] into w_TFTIT index 1.
  EndIf.

  If not Struc_FUNC-ENLFD[] is initial.
    Read table Struc_FUNC-ENLFD[] into w_ENLFD index 1.
  EndIf.

* ######### ## ########### ############# ## #### ####### SAP
*  PERFORM fu_interface_fill(sapms38l)
*                            TABLES   l_import
*                                     l_change
*                                     l_export
*                                     l_tables
*                                     l_except
*                                     ifupararef.

  Sort Struc_FUNC-FUPAR by PPOSITION.

  Loop at Struc_FUNC-FUPAR assigning <FUPAR>.
    Case 'X'.
      when <FUPAR>-REF_CLASS.
        Concatenate 'REF TO' <FUPAR>-STRUCTURE into <FUPAR>-STRUCTURE
          separated by space.
      when <FUPAR>-LINE_OF.
        Concatenate 'LINE OF' <FUPAR>-STRUCTURE into <FUPAR>-STRUCTURE
          separated by space.
      when <FUPAR>-TABLE_OF.
        Concatenate 'TABLE OF' <FUPAR>-STRUCTURE into <FUPAR>-STRUCTURE
          separated by space.
    EndCase.

    Case <FUPAR>-PARAMTYPE.
      when 'I'.
        it_ImportParameters-PARAMETER = <FUPAR>-PARAMETER.
        it_ImportParameters-DBFIELD   = ''.
        it_ImportParameters-DEFAULT   = <FUPAR>-DEFAULTVAL.
*          it_ImportParameters-TYPES     = it_FUPARAREF-TYPE.
        it_ImportParameters-OPTIONAL  = <FUPAR>-OPTIONAL.
        it_ImportParameters-REFERENCE = <FUPAR>-REFERENCE.
        it_ImportParameters-CHANGING  = ''.
        it_ImportParameters-TYP       = <FUPAR>-STRUCTURE.
        it_ImportParameters-CLASS     = <FUPAR>-CLASS.
        it_ImportParameters-REF_CLASS = <FUPAR>-REF_CLASS.
        it_ImportParameters-LINE_OF   = <FUPAR>-LINE_OF.
        it_ImportParameters-TABLE_OF  = <FUPAR>-TABLE_OF.
        Append it_ImportParameters.
      when 'E'.
        it_ExportParameters-PARAMETER = <FUPAR>-PARAMETER.
        it_ExportParameters-DBFIELD   = ''.
*          it_ExportParameters-TYPES     = it_FUPARAREF-TYPE.
        it_ExportParameters-REFERENCE = <FUPAR>-REFERENCE.
        it_ExportParameters-TYP       = <FUPAR>-STRUCTURE.
        it_ExportParameters-CLASS     = <FUPAR>-CLASS.
        it_ExportParameters-REF_CLASS = <FUPAR>-REF_CLASS.
        it_ExportParameters-LINE_OF   = <FUPAR>-LINE_OF.
        it_ExportParameters-TABLE_OF  = <FUPAR>-TABLE_OF.
        Append it_ExportParameters.
      when 'C'.
        it_ChangeParameters-PARAMETER = <FUPAR>-PARAMETER.
        it_ChangeParameters-DBFIELD   = ''.
        it_ChangeParameters-DEFAULT   = <FUPAR>-DEFAULTVAL.
*          it_ChangeParameters-TYPES     = it_FUPARAREF-TYPE.
        it_ChangeParameters-OPTIONAL  = <FUPAR>-OPTIONAL.
        it_ChangeParameters-REFERENCE = <FUPAR>-REFERENCE.
        it_ChangeParameters-CHANGING  = 'X'.
        it_ChangeParameters-TYP       = <FUPAR>-STRUCTURE.
        it_ChangeParameters-CLASS     = <FUPAR>-CLASS.
        it_ChangeParameters-REF_CLASS = <FUPAR>-REF_CLASS.
        it_ChangeParameters-LINE_OF   = <FUPAR>-LINE_OF.
        it_ChangeParameters-TABLE_OF  = <FUPAR>-TABLE_OF.
        Append it_ChangeParameters.
      when 'T'.
        it_TablesParameters-PARAMETER = <FUPAR>-PARAMETER.
        it_TablesParameters-DBSTRUCT  = ''.

*          it_TablesParameters-TYPES     = it_FUPARAREF-TYPE.
        Clear it_TablesParameters-TYPES.
        If <FUPAR>-TYPE is initial.
          it_TablesParameters-TYPES     = 'O'.
        EndIf.

        it_TablesParameters-OPTIONAL  = <FUPAR>-OPTIONAL.
        it_TablesParameters-TYP       = <FUPAR>-STRUCTURE.
        it_TablesParameters-CLASS     = <FUPAR>-CLASS.
        it_TablesParameters-REF_CLASS = <FUPAR>-REF_CLASS.
        it_TablesParameters-LINE_OF   = <FUPAR>-LINE_OF.
        it_TablesParameters-TABLE_OF  = <FUPAR>-TABLE_OF.
        Append it_TablesParameters.
      when 'X'.
        it_ExceptionList-EXCEPTION = <FUPAR>-PARAMETER.
        Append it_ExceptionList.
    EndCase.
  EndLoop.



  FUNCNAME = Obj_Name.

  Call function 'FUNCTION_DELETE'
    EXPORTING
      FUNCNAME      = FUNCNAME
    EXCEPTIONS
      error_message = 2
      others        = 1.



  Data: FUNCTION_INCLUDE type RS38L-INCLUDE.

*    Perform PrepareFunction changing it_Source.
  Perform TrimDataPROG
    using sy-saprl
          Len_ABAP
    changing Struc_FUNC-ABAPTEXT.

*    Call function 'RS_FUNCTIONMODULE_INSERT'
  Call function 'FUNCTION_CREATE'
    EXPORTING
      FUNCNAME           = FUNCNAME
      FUNCTION_POOL      = w_ENLFD-AREA "FUNCTION_POOL
      SHORT_TEXT         = w_TFTIT-STEXT
      INTERFACE_GLOBAL   = w_ENLFD-GLOBAL
    importing
      FUNCTION_INCLUDE = FUNCTION_INCLUDE
    TABLES
      IMPORT_PARAMETER   = it_ImportParameters
      EXPORT_PARAMETER   = it_ExportParameters
      TABLES_PARAMETER   = it_TablesParameters
      CHANGING_PARAMETER = it_ChangeParameters
      EXCEPTION_LIST     = it_ExceptionList
      PARAMETER_DOCU     = it_DocParameters
*        SOURCE             = it_Source
    EXCEPTIONS
      error_message      = 2
      others             = 99.
  If sy-subrc = 0.
    Insert report FUNCTION_INCLUDE from Struc_FUNC-ABAPTEXT.
  EndIf.

  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_Limu_FUNC


*&---------------------------------------------------------------------*
*&      Form  Import_Limu_DYNP
*&---------------------------------------------------------------------*
Form Import_Limu_DYNP
  using
    Obj_Name
    Struc_DYNP type SVRS2_DYNP
    subrc.

  Types:
    Begin of t_DynId,
      PROG type D020S-PROG,
      DNUM type D020S-DNUM,
    End of t_DynId.
  Data:
    DynId type t_DynId,
    w_D020S type D020S,
    it_D020T type standard table of D020T
      with key table_line,
    it_D021S type standard table of D021S
      with key table_line,
    it_D021T type standard table of D021T
      with key table_line,
    it_D022S type standard table of D022S
      with key table_line,
    it_D023S type standard table of D023S
      with key table_line.

  Split Obj_Name at space into DynId-PROG DynId-DNUM.
  If DynId-DNUM is initial.
    DynId = Obj_Name.
  EndIf.

  check:
    not DynId-DNUM is initial,
    DynId-DNUM <> '1000'.

  subrc = isError.

**Rel. 30:               Version 32
**Rel. 31 und 40:        Version 33
**Rel. 4*:               Version 34,  35

  If not Struc_DYNP-D020S[] is initial.
    Read table Struc_DYNP-D020S[] into w_D020S index 1.
    If sy-saprl(1) <= '6' and w_D020S-MICO > 35.
      w_D020S-MICO = 35.
    EndIf.
  EndIf.

  Perform Table_Corresponding
    tables:
      Struc_DYNP-D020T[] it_D020T[],
      Struc_DYNP-D021S[] it_D021S[],
      Struc_DYNP-D021T[] it_D021T[],
      Struc_DYNP-D022S[] it_D022S[],
      Struc_DYNP-D023S[] it_D023S[].

  Export dynpro
    w_D020S
    it_D021S[]
    it_D022S[]
    it_D023S[] id DynId.
  If sy-subrc = 0.
    If not it_D020T[] is initial.
      Modify D020T from table it_D020T[].
    EndIf.
    If not it_D021T[] is initial.
      Modify D021T from table it_D021T[].
    EndIf.
  EndIf.


*######## ## ##### ## ?
*    call function 'RPY_DYNPRO_INSERT_NATIVE'
*      exporting
**       suppress_corr_checks           = ' '
**       CORRNUM                        = ' '
*        header                         = xdyn_head
*        dynprotext                     = xdyn_text
**       SUPPRESS_EXIST_CHECKS          = ' '
**       USE_CORRNUM_IMMEDIATEDLY       = ' '
**       SUPPRESS_COMMIT_WORK           = ' '
*      tables
*        fieldlist                      = idyn_fldl
*        flowlogic                      = idyn_flow
*        params                         = idyn_mcod
*     exceptions
*        cancelled                      = 1
*        already_exists                 = 2
*        program_not_exists             = 3
*        not_executed                   = 4
*        others                         = 5.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_Limu_DYNP

*########### ## LSMPIF01
constants: con_text_dynamic like rsmpe_dfun-text_type value 'D',
           con_text_static  like rsmpe_dfun-text_type value 'S',
           con_include      like rsmpe_mnl-text_type  value 'I'.
* Teilobjekttypen
* Dom#ne MP_OBJTYP
constants: con_inttype_menubar   value 'A',
           con_inttype_fkeyset   value 'P',
           con_inttype_buttonbar value 'B',
           con_inttype_function  value 'F',
           con_inttype_menu      value 'M',
           con_inttype_separator value 'S',
           con_inttype_icon      value 'I',
           con_inttype_status    value 'C',
           con_inttype_program   value 'D',
           con_inttype_dynpro    value 'E',
           con_inttype_title     value 'T',
           con_inttype_text      value 'Z',
           con_inttype_context   value 'X',
           con_inttype_symbolbar value 'Y'.

constants: con_stdtype_constant   value 'C',   " Ortskonstante D-Tasten
           con_stdtype_convar     value 'M',   " Gemischt
           con_stdtype_variable   value space. " Variable
* Dom#ne MP_TXTTYP
constants: con_texttype_obj_text  value 'M',
           con_texttype_icon_text value 'I',
           con_texttype_info_text value 'Q',
           con_texttype_note_text value 'T'.

*########### ## LSMPIF01
*&---------------------------------------------------------------------*
*&      Form  merge_texts
*&---------------------------------------------------------------------*
form merge_texts tables texts   structure rsmptexts
                        int_fun structure rsmpe_fun
                        fun     structure rsmpe_funt
                        int_mtx structure rsmpe_mnl
                        mtx     structure rsmpe_mnlt
                        int_sta structure rsmpe_sta
                        sta     structure rsmpe_stat
                        int_doc structure rsmpe_atr
                        doc     structure rsmpe_atrt
                        int_tit structure rsmpe_tit
                        tit     structure rsmpe_titt.
* Abgleichen
  sort: texts by obj_type obj_code sub_code texttype.
*
  loop at int_fun.
    clear fun.
    move-corresponding int_fun to fun.
    if int_fun-text_type = con_text_static.
      read table texts with key obj_type = con_inttype_function
                                obj_code = fun-code
                                sub_code = fun-textno
                       binary search transporting no fields.
      loop at texts from sy-tabix.
        if texts-obj_type ne con_inttype_function or
           texts-obj_code ne fun-code             or
           texts-sub_code ne fun-textno.
          exit.
        endif.
        case texts-texttype.
          when con_texttype_obj_text.
            fun-fun_text  = texts-text.
            fun-path      = texts-path.
          when con_texttype_icon_text.
            fun-icon_text = texts-text.
          when con_texttype_info_text.
            fun-info_text = texts-text.
        endcase.
      endloop.
    endif.
    append fun.
  endloop.
*
  loop at int_mtx.
    clear mtx.
    move-corresponding int_mtx to mtx.
    read table texts with key obj_type = con_inttype_menu
                              obj_code = mtx-code
                              sub_code = space
                     binary search transporting no fields.
    loop at texts from sy-tabix.
      if texts-obj_type ne con_inttype_menu or
         texts-obj_code ne mtx-code         or
         texts-sub_code ne space.
        exit.
      endif.
      case texts-texttype.
        when con_texttype_obj_text.
          mtx-text     = texts-text.
          mtx-path     = texts-path.
        when con_texttype_note_text.
          mtx-int_note = texts-text.
      endcase.
    endloop.
    append mtx.
  endloop.
*
  loop at int_sta.
    move-corresponding int_sta to sta.
    read table texts with key obj_type = con_inttype_status
                              obj_code = sta-code
                              sub_code = space
                              texttype = con_texttype_note_text
                     binary search.
    if sy-subrc = 0.
      sta-int_note = texts-text.
    endif.
    append sta.
  endloop.
*
  loop at int_doc.
    move-corresponding int_doc to doc.
    read table texts with key obj_type = doc-obj_type
                              obj_code = doc-obj_code
                              sub_code = doc-sub_code
                              texttype = con_texttype_note_text
                     binary search.
    if sy-subrc = 0.
      doc-int_note = texts-text.
    endif.
    append doc.
  endloop.
*
  loop at int_tit.
    tit-code = int_tit-code.
    read table texts with key obj_type = con_inttype_title
                              obj_code = int_tit-code
                              sub_code = space
                              texttype = con_texttype_note_text
                     binary search.
    if sy-subrc = 0.
      tit-text     = texts-text.
    endif.
    append tit.
  endloop.
endform.                    "merge_texts

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_CUAD
*&---------------------------------------------------------------------*
Form Import_Limu_CUAD
  using
    Obj_Name
    Struc_CUAD type SVRS2_CUAD
    subrc.

  Data:
    w_OBJ_NAME type TRDIR-NAME,
    TRKEY type TRKEY,
    ADM type RSMPE_ADM,
    it_ADM type standard table of RSMPE_ADM
      with key table_line,
    it_STA type standard table of RSMPE_STAT
      with key table_line,
    it_FUN type standard table of RSMPE_FUNT
      with key table_line,
    it_MEN type standard table of RSMPE_MEN
      with key table_line,
    it_MTX type standard table of RSMPE_MNLT
      with key table_line,
    it_ACT type standard table of RSMPE_ACT
      with key table_line,
    it_BUT type standard table of RSMPE_BUT
      with key table_line,
    it_PFK type standard table of RSMPE_PFK
      with key table_line,
    it_SET type standard table of RSMPE_STAF
      with key table_line,
    it_DOC type standard table of RSMPE_ATRT
      with key table_line,
    it_TIT type standard table of RSMPE_TITT
      with key table_line,
    it_BIV type standard table of RSMPE_BUTS
      with key table_line.

  Field-symbols:
    <VTEXTS_40> type line of SVRS2_CUAD-VTEXTS_40,
    <FUN> like line of it_FUN.

  subrc = isError.

  w_OBJ_NAME = OBJ_NAME.
*  TRKEY-DEVCLASS = p_TADIR-DEVCLASS.
  TRKEY-OBJ_TYPE = 'CUAD'.
  TRKEY-OBJ_NAME = Obj_Name.


  Perform merge_texts
    tables
      Struc_CUAD-VTEXTS_40 "texts   structure rsmptexts
      Struc_CUAD-VFUN_40   "int_fun structure rsmpe_fun
      it_FUN               "fun     structure rsmpe_funt
      Struc_CUAD-VMTX_40   "int_mtx structure rsmpe_mnl
      it_MTX               "mtx     structure rsmpe_mnlt
      Struc_CUAD-VSTA_40   "int_sta structure rsmpe_sta
      it_STA               "sta     structure rsmpe_stat
      Struc_CUAD-VDOC_40   "int_doc structure rsmpe_atr
      it_DOC               "doc     structure rsmpe_atrt
      Struc_CUAD-VTIT_40   "int_tit structure rsmpe_tit
      it_TIT.              "tit     structure rsmpe_titt.

  Perform Table_Corresponding
    tables:
      Struc_CUAD-VADM_40 it_ADM,
*      Struc_CUAD-VSTA_40 it_STA,
*      Struc_CUAD-VFUN_40 it_FUN,
      Struc_CUAD-VMEN_40 it_MEN,
*      Struc_CUAD-VMTX_40 it_MTX,
      Struc_CUAD-VACT_40 it_ACT,
      Struc_CUAD-VBUT_40 it_BUT,
      Struc_CUAD-VPFK_40 it_PFK,
      Struc_CUAD-VSET_40 it_SET,
*      Struc_CUAD-VDOC_40 it_DOC,
*      Struc_CUAD-VTIT_40 it_TIT,
      Struc_CUAD-VBIV_40 it_BIV.


*  Loop at Struc_CUAD-VTEXTS_40 assigning <VTEXTS_40>.
*    Read table it_FUN assigning <FUN>
*      with KEY CODE   = <VTEXTS_40>-OBJ_CODE
*               TEXTNO = <VTEXTS_40>-SUB_CODE.
*    If sy-subrc = 0.
*      Case <VTEXTS_40>-TEXTTYPE.
*        when 'I'.
*          <FUN>-ICON_TEXT = <VTEXTS_40>-TEXT.
*        when 'Q'.
*          <FUN>-INFO_TEXT = <VTEXTS_40>-TEXT.
*        when 'M'.
*          <FUN>-FUN_TEXT = <VTEXTS_40>-TEXT.
*          <FUN>-PATH = <VTEXTS_40>-PATH.
*      EndCase.
*    EndIf.
*  EndLoop.

*######## ############
*         VCTX_40 TYPE RSMPE_CTX OCCURS 0,

  If not it_ADM[] is initial.
    Read table it_ADM[] into ADM index 1.
  EndIf.

*    Call function 'RS_CUA_INTERNAL_GENERATE_LTAB'
  Call function 'RS_CUA_INTERNAL_WRITE'
    EXPORTING
      PROGRAM  = w_OBJ_NAME
      LANGUAGE = sy-langu
      TR_KEY   = TRKEY
      ADM      = ADM
    TABLES
      STA      = it_STA
      FUN      = it_FUN
      MEN      = it_MEN
      MTX      = it_MTX
      ACT      = it_ACT
      BUT      = it_BUT
      PFK      = it_PFK
      SET      = it_SET
      DOC      = it_DOC
      TIT      = it_TIT
      BIV      = it_BIV
    EXCEPTIONS
      others   = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_Limu_CUAD

*&---------------------------------------------------------------------*
*&      Form  ImportFUGR
*&---------------------------------------------------------------------*
Form ImportFUGR
  using w_TADIR type t_TADIR
        Path
        subrc.
  Data:
    ProgName type sy-repid,
    FUNCTION_POOL type TLIBG-AREA,
    it_InclName type standard table of PROGNAME
      with header line,
    it_FuncName type standard table of RS38L_INCL
      with header line,
    it_Source type tt_Source,
    it_Readme type standard table of char255 "string
      with header line,
    FileName type string,
    Filter type string,
    GENERATED type ENLFDIR-GENERATED,
    GROUP type RS38L-AREA,
    it_TFDIR     type standard table of TFDIR,
    it_TFTIT     type standard table of TFTIT
      with header line,
    it_FUNCT     type standard table of FUNCT
      with header line,
    it_ENLFDIR   type standard table of ENLFDIR
      with header line,
    it_TRDIR     type standard table of TRDIR
      with header line,
    it_TRDIRT     type standard table of TRDIRT
      with header line,
    it_FUPARAREF type standard table of SFUPARAREF
      with header line,
    Dir type string,
    Cnt type i,
    it_ImportParameters type standard table of RSIMP
      with header line,
    it_ExportParameters type standard table of RSEXP
      with header line,
    it_TablesParameters type standard table of RSTBL
      with header line,
    it_ChangeParameters type standard table of RSCHA
      with header line,
    it_ExceptionList type standard table of RSEXC
      with header line,
    it_DocParameters type standard table of RSFDO
      with header line,
    it_TLIBT type standard table of TLIBT
      with header line,
    w_PROGDIR type PROGDIR,
    Dummy type string.

  subrc = isError.

  FUNCTION_POOL = w_TADIR-OBJ_NAME.

  Concatenate Path w_TADIR-OBJ_NAME '\TLIBT.txt' into FileName.
  Perform Read_File using FileName changing it_TLIBT[].
  If not it_TLIBT[] is initial.
    Read table it_TLIBT index 1.
  EndIf.

  Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' into Dir.
*  Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*    EXPORTING
*      DIRECTORY        = Dir
*      FILTER           = '*.*'
*      DIRECTORIES_ONLY = 'X'
*    CHANGING
*      FILE_TABLE       = it_FuncName[]
*      COUNT            = Cnt
*    EXCEPTIONS
*      others           = 1.
  Perform Read_Catalog
    using
      Dir '*.*' 'D'
    changing
      it_FuncName[].

  If sy-subrc = 0.
    Perform DeleteFUGR
      using FUNCTION_POOL.
  EndIf.

  Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\' into Dir.
  Concatenate 'L' w_TADIR-OBJ_NAME '*.txt' into Filter.
*  Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*    EXPORTING
*      DIRECTORY  = Dir
*      FILTER     = Filter
*      FILES_ONLY = 'X'
*    CHANGING
*      FILE_TABLE = it_InclName[]
*      COUNT      = Cnt
*    EXCEPTIONS
*      others     = 1.

  Perform Read_Catalog
    using Dir Filter 'F'
    changing it_InclName[].


  Call function 'FUNCTION_POOL_CREATE'
    EXPORTING
      POOL_NAME     = FUNCTION_POOL
      SHORT_TEXT    = it_TLIBT-AREAT
    EXCEPTIONS
      error_message = 2
      others        = 1.

  Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\TRDIR.txt'
    into FileName.
  Perform Read_File using FileName changing it_TRDIR[].
  Sort it_TRDIR by NAME.

  Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\TRDIRT.txt'
    into FileName.
  Perform Read_File using FileName changing it_TRDIRT[].
  Sort it_TRDIRT by NAME SPRSL.

  Loop at it_InclName.
    Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\' it_InclName
      into FileName.
    Perform Read_File using FileName changing it_Source.

    If not it_Source[] is initial.
      Split it_InclName at '.' into it_InclName Dummy.
      Translate it_InclName to upper case.
      Perform TrimDataPROG
        using sy-saprl
              Len_ABAP
        changing it_Source.

      Insert report it_InclName from it_Source.

      If sy-subrc = 0.
*        Select single * from TRDIR into it_TRDIR
*          where NAME = it_InclName.
        Read table it_TRDIR with key NAME = it_InclName
          binary search.
        If sy-subrc = 0.
*          it_TRDIR-SUBC = 'I'.
          Modify TRDIR from it_TRDIR.

          Move-corresponding: it_TRDIR to w_PROGDIR.
          Call function 'UPDATE_PROGDIR'
            EXPORTING
              I_PROGDIR  = w_PROGDIR
              I_PROGNAME = w_PROGDIR-NAME
              I_STATE    = 'A'
            EXCEPTIONS
              others     = 1.

        EndIf.

        Read table it_TRDIRT
          with key NAME = it_InclName
                   SPRSL = sy-langu
          binary search.
        If sy-subrc = 0.
          Modify TRDIRT from it_TRDIRT.
        EndIf.

        Perform SetClassForObject using w_TADIR.
        If sy-subrc = 0.
*          flgModify = 'X'.
        EndIf.
      EndIf.
    EndIf.
  EndLoop.

  Data: subrcPROG type sy-subrc.
  Perform ImportPROG using w_TADIR Path subrcPROG.
  check subrcPROG = isOk.

  Loop at it_FuncName.
    Refresh:
      it_ImportParameters,
      it_ExportParameters,
      it_TablesParameters,
      it_ChangeParameters,
      it_ExceptionList,
      it_DocParameters,
      it_Source.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\ENLFDIR.txt' into FileName.
    Perform Read_File using FileName changing it_ENLFDIR[].
    Clear it_ENLFDIR.
    If not it_ENLFDIR[] is initial.
      Read table it_ENLFDIR index 1.
    EndIf.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\FUNCTION.txt' into FileName.
    Perform Read_File using FileName changing it_Source.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\TFDIR.txt' into FileName.
    Perform Read_File using FileName changing it_TFDIR.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\TFTIT.txt' into FileName.
    Perform Read_File using FileName changing it_TFTIT[].
    If not it_TFTIT[] is initial.
      Read table it_TFTIT index 1.
    EndIf.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\FUNCT.txt' into FileName.
    Perform Read_File using FileName changing it_FUNCT[].
    Loop at it_FUNCT.
      Move-corresponding: it_FUNCT to it_DocParameters.
      Append it_DocParameters.
    EndLoop.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\TRDIR.txt' into FileName.
    Perform Read_File using FileName changing it_TRDIR[].

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
      '\FUPARAREF.txt' into FileName.
    Perform Read_File using FileName changing it_FUPARAREF[].
    Sort it_FUPARAREF by PARAMTYPE PPOSITION.

    Loop at it_FUPARAREF.
      Case it_FUPARAREF-PARAMTYPE.
        when 'I'.
          it_ImportParameters-PARAMETER = it_FUPARAREF-PARAMETER.
          it_ImportParameters-DBFIELD   = ''.
          it_ImportParameters-DEFAULT   = it_FUPARAREF-DEFAULTVAL.
*          it_ImportParameters-TYPES     = it_FUPARAREF-TYPE.
          it_ImportParameters-OPTIONAL  = it_FUPARAREF-OPTIONAL.
          it_ImportParameters-REFERENCE = it_FUPARAREF-REFERENCE.
          it_ImportParameters-CHANGING  = ''.
          it_ImportParameters-TYP       = it_FUPARAREF-STRUCTURE.
          it_ImportParameters-CLASS     = it_FUPARAREF-CLASS.
          it_ImportParameters-REF_CLASS = it_FUPARAREF-REF_CLASS.
          it_ImportParameters-LINE_OF   = it_FUPARAREF-LINE_OF.
          it_ImportParameters-TABLE_OF  = it_FUPARAREF-TABLE_OF.
          Append it_ImportParameters.
        when 'E'.
          it_ExportParameters-PARAMETER = it_FUPARAREF-PARAMETER.
          it_ExportParameters-DBFIELD   = ''.
*          it_ExportParameters-TYPES     = it_FUPARAREF-TYPE.
          it_ExportParameters-REFERENCE = it_FUPARAREF-REFERENCE.
          it_ExportParameters-TYP       = it_FUPARAREF-STRUCTURE.
          it_ExportParameters-CLASS     = it_FUPARAREF-CLASS.
          it_ExportParameters-REF_CLASS = it_FUPARAREF-REF_CLASS.
          it_ExportParameters-LINE_OF   = it_FUPARAREF-LINE_OF.
          it_ExportParameters-TABLE_OF  = it_FUPARAREF-TABLE_OF.
          Append it_ExportParameters.
        when 'C'.
          it_ChangeParameters-PARAMETER = it_FUPARAREF-PARAMETER.
          it_ChangeParameters-DBFIELD   = ''.
          it_ChangeParameters-DEFAULT   = it_FUPARAREF-DEFAULTVAL.
*          it_ChangeParameters-TYPES     = it_FUPARAREF-TYPE.
          it_ChangeParameters-OPTIONAL  = it_FUPARAREF-OPTIONAL.
          it_ChangeParameters-REFERENCE = it_FUPARAREF-REFERENCE.
          it_ChangeParameters-CHANGING  = 'X'.
          it_ChangeParameters-TYP       = it_FUPARAREF-STRUCTURE.
          it_ChangeParameters-CLASS     = it_FUPARAREF-CLASS.
          it_ChangeParameters-REF_CLASS = it_FUPARAREF-REF_CLASS.
          it_ChangeParameters-LINE_OF   = it_FUPARAREF-LINE_OF.
          it_ChangeParameters-TABLE_OF  = it_FUPARAREF-TABLE_OF.
          Append it_ChangeParameters.
        when 'T'.
          it_TablesParameters-PARAMETER = it_FUPARAREF-PARAMETER.
          it_TablesParameters-DBSTRUCT  = ''.

*          it_TablesParameters-TYPES     = it_FUPARAREF-TYPE.
          Clear it_TablesParameters-TYPES.
          If it_FUPARAREF-TYPE is initial.
            it_TablesParameters-TYPES     = 'O'.
          EndIf.

          it_TablesParameters-OPTIONAL  = it_FUPARAREF-OPTIONAL.
          it_TablesParameters-TYP       = it_FUPARAREF-STRUCTURE.
          it_TablesParameters-CLASS     = it_FUPARAREF-CLASS.
          it_TablesParameters-REF_CLASS = it_FUPARAREF-REF_CLASS.
          it_TablesParameters-LINE_OF   = it_FUPARAREF-LINE_OF.
          it_TablesParameters-TABLE_OF  = it_FUPARAREF-TABLE_OF.
          Append it_TablesParameters.
        when 'X'.
          it_ExceptionList-EXCEPTION = it_FUPARAREF-PARAMETER.
          Append it_ExceptionList.
      EndCase.
    EndLoop.

    Call function 'FUNCTION_DELETE'
      EXPORTING
        FUNCNAME      = it_FuncName-FUNCNAME
      EXCEPTIONS
        error_message = 2
        others        = 1.

    Data: FUNCTION_INCLUDE type RS38L-INCLUDE.

*    Perform PrepareFunction changing it_Source.
    Perform TrimDataPROG
      using sy-saprl
            Len_ABAP
      changing it_Source.


*    Call function 'RS_FUNCTIONMODULE_INSERT'
    Call function 'FUNCTION_CREATE'
      EXPORTING
        FUNCNAME           = it_FuncName-FUNCNAME
        FUNCTION_POOL      = FUNCTION_POOL
        SHORT_TEXT         = it_TFTIT-STEXT
        INTERFACE_GLOBAL   = it_ENLFDIR-GLOBAL
      importing
        FUNCTION_INCLUDE = FUNCTION_INCLUDE
      TABLES
        IMPORT_PARAMETER   = it_ImportParameters
        EXPORT_PARAMETER   = it_ExportParameters
        TABLES_PARAMETER   = it_TablesParameters
        CHANGING_PARAMETER = it_ChangeParameters
        EXCEPTION_LIST     = it_ExceptionList
        PARAMETER_DOCU     = it_DocParameters
*        SOURCE             = it_Source
      EXCEPTIONS
        error_message      = 2
        others             = 99.

    Insert report FUNCTION_INCLUDE from it_Source.

  EndLoop.


** ##. Report RSVIEWMA
** (########) ######### ####### ####### ### ########/######
  Data:
    it_TVDIR type standard table of TVDIR,
    w_TVDIR type TVDIR,
    w_GENCB type VIMGENCB,
    w_MODE value 'A',
    w_TRACE,
    w_TEST.

  Select single *
    into w_TVDIR
    from TVDIR
    where AREA = w_TADIR-OBJ_NAME.
  If sy-subrc = 0 and not w_TVDIR is initial.
    w_GENCB-AREA = w_TVDIR-AREA.
    w_GENCB-VIEWNAME = w_TVDIR-TABNAME.

*    w_GENCB-CREPOOL = 'X'.
*    w_GENCB-CREFFUNC = 'X'.
    w_GENCB-CREPFUNC = 'X'.
*    w_GENCB-CREIMEMB = 'X'.
    w_GENCB-CRETMEMB = 'X'.
*    w_GENCB-CREFMEMB = 'X'.
*    w_GENCB-CREUFMEMB = 'X'.
*    w_GENCB-CREUPMEMB = 'X'.

*    w_GENCB-CREDYNP1 = 'X'.
*    w_GENCB-DELFFUNC = 'X'.
    w_GENCB-DELPFUNC = 'X'.
*    w_GENCB-DELIMEMB = 'X'.
    w_GENCB-DELTMEMB = 'X'.
*    w_GENCB-DELFMEMB = 'X'.
*    w_GENCB-DELUFMEMB = 'X'.
*    w_GENCB-DELUPMEMB = 'X'.
*    w_GENCB-TVDIR = 'E'.

    w_GENCB-REGPROG = 'X'.
    w_GENCB-REGTOP = 'X'.

    Perform START_GEN_VIEWMAINT_TOOL(SAPMSVIM)
      using w_TVDIR w_GENCB w_MODE w_TRACE w_TEST.
  EndIf.
EndForm.                    "ImportFUGR

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_TYPD
*&---------------------------------------------------------------------*
Form Import_Limu_TYPD
  using
    Obj_Name
    Struc_TYPD type SVRS2_TYPD
    subrc.

  Data:
    TADIR_OBJ_NAME type TADIR-OBJ_NAME,
    PROGNAME type PROGRAMM,
    Struc_REPS type SVRS2_REPS.

  subrc = isError.

  TADIR_OBJ_NAME = Obj_Name.

  Call function 'RS_TADIR_TO_PROGNAME'
    EXPORTING
      OBJECT        = 'TYPE'
      OBJ_NAME      = TADIR_OBJ_NAME
    IMPORTING
      PROGNAME      = PROGNAME
    EXCEPTIONS
      error_message = 1
      others        = 99.
  check sy-subrc = 0.
  Struc_REPS-ABAPTEXT = Struc_TYPD-ABAPTEXT.
  Struc_REPS-MDLOG    = Struc_TYPD-MDLOG.
  Struc_REPS-TRDIR    = Struc_TYPD-TRDIR.

  Perform Import_Limu_REPS
    using
      PROGNAME
      Struc_REPS
      subrc.

EndForm.                    "Import_Limu_TYPD

*&---------------------------------------------------------------------*
*&      Form  ImportProgram
*&---------------------------------------------------------------------*
Form ImportPROG
  using p_TADIR type t_TADIR
        Path
        subrc.

  Types:
    Begin of t_GuiStatus,
      ADM type RSMPE_ADM,
      it_STA type standard table of RSMPE_STAT
        with key table_line,
      it_FUN type standard table of RSMPE_FUNT
        with key table_line,
      it_MEN type standard table of RSMPE_MEN
        with key table_line,
      it_MTX type standard table of RSMPE_MNLT
        with key table_line,
      it_ACT type standard table of RSMPE_ACT
        with key table_line,
      it_BUT type standard table of RSMPE_BUT
        with key table_line,
      it_PFK type standard table of RSMPE_PFK
        with key table_line,
      it_SET type standard table of RSMPE_STAF
        with key table_line,
      it_DOC type standard table of RSMPE_ATRT
        with key table_line,
      it_TIT type standard table of RSMPE_TITT
        with key table_line,
      it_BIV type standard table of RSMPE_BUTS
        with key table_line,
    End of t_GuiStatus.

  Data:
    flgModify,
    OBJ_NAME type TADIR-OBJ_NAME,
    it_SourceOld  type tt_Source,
    it_SourceFile type tt_Source,
    MainFileName type string,
    FileName type string,
    it_TextPoolOld type standard table of TEXTPOOL
      with header line,
    it_TextPoolFile type standard table of t_TextPoolFile
      with header line,
    it_TextPool type standard table of TEXTPOOL
      with header line,
    it_DocOld type standard table of TLINE
      with header line,
    it_DocFile type standard table of TLINE
      with header line,
    it_TRDIR type standard table of TRDIR
      with header line,
    it_DDTYPET type standard table of DDTYPET
      with header line,
    w_PROGDIR type PROGDIR,
    NewGuiStatus type t_GuiStatus,
    OldGuiStatus type t_GuiStatus,
    it_ADM type standard table of RSMPE_ADM.

  subrc = isError.

  Perform GetMainProgName
    using p_TADIR
    changing OBJ_NAME
             MainFileName.

  Concatenate Path p_TADIR-OBJ_NAME '\' MainFileName '.txt'
    into FileName.
  Perform Read_File using FileName changing it_SourceFile.
  check sy-subrc = 0.

  Concatenate Path p_TADIR-OBJ_NAME '\TRDIR.txt' into FileName.
  Perform Read_File using FileName changing it_TRDIR[].
  If sy-subrc = 0.
    Sort it_TRDIR by NAME.
    Clear it_TRDIR.
    Read table it_TRDIR with key NAME = OBJ_NAME
      binary search.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\DDTYPET.txt' into FileName.
  Perform Read_File using FileName changing it_DDTYPET[].
  If sy-subrc = 0.
    Read table it_DDTYPET index 1.
  EndIf.

  Perform TrimDataPROG
    using sy-saprl
          Len_ABAP
    changing it_SourceFile.

  If p_Rewrte is initial.
    Read report OBJ_NAME into it_SourceOld.
    Perform TrimDataPROG
      using sy-saprl
            Len_ABAP
      changing it_SourceOld.
  EndIf.

  If it_SourceFile[] <> it_SourceOld[] and
     not it_SourceFile[] is initial.

    Insert report OBJ_NAME from it_SourceFile[].

    If sy-subrc = 0.
      If not it_TRDIR-NAME is initial.
        Modify TRDIR from it_TRDIR.

        Move-corresponding: it_TRDIR to w_PROGDIR.
        Call function 'UPDATE_PROGDIR'
          EXPORTING
            I_PROGDIR  = w_PROGDIR
            I_PROGNAME = w_PROGDIR-NAME
            I_STATE    = 'A'
          EXCEPTIONS
            others     = 1.
      EndIf.

      If not it_DDTYPET is initial.
        Modify DDTYPET from it_DDTYPET.
      EndIf.

      Perform SetClassForObject using p_TADIR.
      If sy-subrc = 0.
        flgModify = 'X'.
        subrc = isOk.
      EndIf.
    EndIf.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\TEXTPOOL.txt' into FileName.
  Perform Read_File using FileName changing it_TextPoolFile[].


  Loop at it_LANGU.
    Refresh it_TextPool.

    Loop at it_TextPoolFile
      where LANGU = it_LANGU.
      Move-corresponding: it_TextPoolFile to it_TextPool.
      Append it_TextPool.
    EndLoop.

    If p_Rewrte is initial.
      Read textpool OBJ_NAME into it_TextPoolOld language it_LANGU.
    EndIf.

    If it_TextPool[] <> it_TextPoolOld[] and
       not it_TextPool[] is initial.

      Insert textpool OBJ_NAME from it_TextPool language it_LANGU.

      If sy-subrc = 0.
        flgModify = 'X'.
      EndIf.
    EndIf.
  EndLoop.

* Screens
  Types:
    Begin of t_DynId,
      PROG type D020S-PROG,
      DNUM type D020S-DNUM,
    End of t_DynId.

  Types:
    Begin of t_Screen,
      D020S type D020S,
      it_D021S type standard table of D021S
        with key table_line,
      it_D022S type standard table of D022S
        with key table_line,
      it_D023S type standard table of D023S
        with key table_line,
    End of t_Screen.

  Data:
    Dir type string,
    Cnt type i,
    Ext type string,
    it_Dynnr type tt_String "table of FILE_INFO
      with header line,
    NewScreen type t_Screen,
    OldScreen type t_Screen,

    it_D020S type standard table of D020S,
*    it_D021S type standard table of D021S,
*    it_D022S type standard table of D022S,
*    it_D023S type standard table of D023S,
    DynId type t_DynId.

  Concatenate Path p_TADIR-OBJ_NAME '\SCREEN\' into Dir.
*  Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*    EXPORTING
*      DIRECTORY        = Dir
*      FILTER           = '*.*'
*      DIRECTORIES_ONLY = 'X'
*    CHANGING
*      FILE_TABLE       = it_Dynnr[]
*      COUNT            = Cnt
*    EXCEPTIONS
*      others           = 1.

  Perform Read_Catalog
    using Dir '*.*' 'D'
    changing it_Dynnr[].

  If sy-subrc = 0.
    Loop at it_Dynnr.
      DynId-PROG = OBJ_NAME.
*      Split it_Dynnr-FILENAME at '.' into DynId-DNUM Ext.
      DynId-DNUM  = it_Dynnr. "-FILENAME.

*      Refresh: it_D020S[], it_D021S[], it_D022S[], it_D023S[].
      Clear: NewScreen, OldScreen.
      Clear it_D020S.

      Concatenate Path p_TADIR-OBJ_NAME '\SCREEN\' it_Dynnr "-FILENAME
        '\D020S.txt' into FileName.
      Perform Read_File using FileName changing it_D020S[].
      If not it_D020S[] is initial.
        Read table it_D020S into NewScreen-D020S index 1.
        If sy-saprl(1) <= '4' and NewScreen-D020S-MICO > 35.
          NewScreen-D020S-MICO = 35.
        EndIf.
      EndIf.

      Concatenate Path p_TADIR-OBJ_NAME '\SCREEN\' it_Dynnr "-FILENAME
        '\D021S.txt' into FileName.
      Perform Read_File using FileName changing NewScreen-it_D021S[].
      Concatenate Path p_TADIR-OBJ_NAME '\SCREEN\' it_Dynnr "-FILENAME
        '\D022S.txt' into FileName.
      Perform Read_File using FileName changing NewScreen-it_D022S[].
      Concatenate Path p_TADIR-OBJ_NAME '\SCREEN\' it_Dynnr "-FILENAME
        '\D023S.txt' into FileName.
      Perform Read_File using FileName changing NewScreen-it_D023S[].

      If p_Rewrte is initial.
        Import dynpro
          OldScreen-D020S
          OldScreen-it_D021S[]
          OldScreen-it_D022S[]
          OldScreen-it_D023S[] id DynId.
      EndIf.

      If NewScreen <> OldScreen and
         not NewScreen is initial.

        Export dynpro
          NewScreen-D020S
          NewScreen-it_D021S[]
          NewScreen-it_D022S[]
          NewScreen-it_D023S[] id DynId.
      EndIf.
    EndLoop.
  EndIf.

* Docs
  Concatenate Path p_TADIR-OBJ_NAME '\DOC.txt' into FileName.
  Perform Read_File using FileName changing it_DocFile[].

  If p_Rewrte is initial.
    Call function 'DOC_OBJECT_GET'
      EXPORTING
        CLASS     = 'RE'
        NAME      = p_TADIR-OBJ_NAME
      TABLES
        ITF_LINES = it_DocOld
      EXCEPTIONS
        others    = 1.
  EndIf.

  If it_DocFile[] <> it_DocOld[] and
     not it_DocFile[] is initial.
    Data:
      DOKHL_OBJECT type DOKHL-OBJECT.
    DOKHL_OBJECT = OBJ_NAME. "p_TADIR-

    Call function 'DOCU_UPD_SAVE'
      EXPORTING
        ID     = 'RE'
        LANGU  = sy-langu
        OBJECT = DOKHL_OBJECT
      TABLES
        LINE   = it_DocFile[].
    If sy-subrc = 0.
      flgModify = 'X'.
    EndIf.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\ADM.txt' into FileName.
  Perform Read_File using FileName changing it_ADM[].
  If not it_ADM[] is initial.
    Read table it_ADM into NewGuiStatus-ADM index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\STA.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_STA[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\FUN.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_FUN[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\MEN.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_MEN[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\MTX.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_MTX[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\ACT.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_ACT[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\BUT.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_BUT[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\PFK.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_PFK[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\SET.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_SET[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\DOC.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_DOC[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\TIT.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_TIT[].
  Concatenate Path p_TADIR-OBJ_NAME '\GUI-STATUS\BIV.txt' into FileName.
  Perform Read_File using FileName changing NewGuiStatus-it_BIV[].

  If p_Rewrte is initial.
    Call function 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        PROGRAM = OBJ_NAME
      IMPORTING
        ADM     = OldGuiStatus-ADM
      TABLES
        STA     = OldGuiStatus-it_STA
        FUN     = OldGuiStatus-it_FUN
        MEN     = OldGuiStatus-it_MEN
        MTX     = OldGuiStatus-it_MTX
        ACT     = OldGuiStatus-it_ACT
        BUT     = OldGuiStatus-it_BUT
        PFK     = OldGuiStatus-it_PFK
        SET     = OldGuiStatus-it_SET
        DOC     = OldGuiStatus-it_DOC
        TIT     = OldGuiStatus-it_TIT
        BIV     = OldGuiStatus-it_BIV
      EXCEPTIONS
        others  = 1.
  EndIf.

  If NewGuiStatus <> OldGuiStatus and
     not NewGuiStatus is initial.


    Data:
      TRKEY type TRKEY.
    TRKEY-DEVCLASS = p_TADIR-DEVCLASS.
    TRKEY-OBJ_TYPE = p_TADIR-OBJECT.
    TRKEY-OBJ_NAME = p_TADIR-OBJ_NAME.
*    Call function 'RS_CUA_INTERNAL_GENERATE_LTAB'
    Call function 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        PROGRAM  = OBJ_NAME
        LANGUAGE = sy-langu
        TR_KEY   = TRKEY
        ADM      = NewGuiStatus-ADM
      TABLES
        STA      = NewGuiStatus-it_STA
        FUN      = NewGuiStatus-it_FUN
        MEN      = NewGuiStatus-it_MEN
        MTX      = NewGuiStatus-it_MTX
        ACT      = NewGuiStatus-it_ACT
        BUT      = NewGuiStatus-it_BUT
        PFK      = NewGuiStatus-it_PFK
        SET      = NewGuiStatus-it_SET
        DOC      = NewGuiStatus-it_DOC
        TIT      = NewGuiStatus-it_TIT
        BIV      = NewGuiStatus-it_BIV
      EXCEPTIONS
        others   = 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\VARIANTS\'
    into FileName.
  Perform ImportVariant using OBJ_NAME p_TADIR FileName.

  If flgModify = 'X'.
    subrc = isOk.
    Perform AppendToQuery using p_TADIR.
  EndIf.
EndForm.                    "ImportProgram


*&---------------------------------------------------------------------*
*&      Form  Import_Limu_VARI
*&---------------------------------------------------------------------*
Form Import_Limu_VARI
  using
    Obj_Name
    Struc_VARI type t_LIMU_VARI
    subrc.

  Data:
    w_VARID type VARID,
    w_OBJECTS like line of Struc_VARI-OBJECTS[],
    it_RSVDESC type standard table of RSVDESC
      with header line.

  subrc = isError.

  check not Struc_VARI-VARID[] is initial.
  Read table Struc_VARI-VARID[] into w_VARID index 1.

  Call function 'RS_CREATE_VARIANT'
    EXPORTING
      CURR_REPORT    = w_VARID-REPORT
      CURR_VARIANT   = w_VARID-VARIANT
      VARI_DESC      = w_VARID
    TABLES
      VARI_CONTENTS  = Struc_VARI-VALUTAB[]
      VARI_TEXT      = Struc_VARI-VARIT[]
    EXCEPTIONS
      VARIANT_EXISTS = 1
      others         = 99.
  check sy-subrc = 0 or sy-subrc = 1.

  Refresh it_RSVDESC.
  Loop at Struc_VARI-OBJECTS[] into w_OBJECTS.
    Move-corresponding: w_OBJECTS to it_RSVDESC.
    Append it_RSVDESC.
  EndLoop.

  Call function 'RS_CHANGE_CREATED_VARIANT'
    EXPORTING
      CURR_REPORT   = w_VARID-REPORT
      CURR_VARIANT  = w_VARID-VARIANT
      VARI_DESC     = w_VARID
    TABLES
      VARI_CONTENTS = Struc_VARI-VALUTAB[]
      VARI_TEXT     = Struc_VARI-VARIT[]
      VARI_SEL_DESC = it_RSVDESC
      OBJECTS       = Struc_VARI-OBJECTS[]
    EXCEPTIONS
      others        = 1.

  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_Limu_VARI

*&---------------------------------------------------------------------*
*&      Form  ImportVariant
*&---------------------------------------------------------------------*
Form ImportVariant
  using ProgName
        p_TADIR type t_TADIR
        PathVariant.
  Data:
    Cnt type i,
    it_VariName type standard table of RSVAR-VARIANT
      with header line,
    it_INFO      type standard table of RSVARIINFO
      with header line,
    it_RSPARAMS type standard table of RSPARAMS
      with header line,
    it_VARIT type standard table of VARIT
      with header line,
    it_RSVDESC type standard table of RSVDESC
      with header line,
    it_VANZ  type standard table of VANZ
      with header line,
    VARI_DESC type VARID.

*  Dir = PathVariant.

*  Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*    EXPORTING
*      DIRECTORY        = PathVariant
*      FILTER           = '*.*'
*      DIRECTORIES_ONLY = 'X'
*    CHANGING
*      FILE_TABLE       = it_VariName[]
*      COUNT            = Cnt
*    EXCEPTIONS
*      others           = 1.
  Perform Read_Catalog
    using PathVariant '*.*' 'D'
    changing it_VariName[].

  Loop at it_VariName.
    Concatenate PathVariant it_VariName '\RSVARIINFO.txt'
      into FileName.
    Perform Read_File using FileName changing it_INFO[].
    If sy-subrc = 0.
      Read table it_INFO index 1.
    EndIf.

    Concatenate PathVariant it_VariName '\RSPARAMS.txt'
      into FileName.
    Perform Read_File using FileName changing it_RSPARAMS[].

    Concatenate PathVariant it_VariName '\VANZ.txt'
      into FileName.
    Perform Read_File using FileName changing it_VANZ[].

    Refresh it_VARIT.
    Move-corresponding: it_INFO to it_VARIT.
    Append it_VARIT.

    Move-corresponding: it_INFO to VARI_DESC.

    Refresh it_RSVDESC.
    Loop at it_VANZ.
      Move-corresponding: it_VANZ to it_RSVDESC.
      Append it_RSVDESC.
    EndLoop.

    Call function 'RS_CREATE_VARIANT'
      EXPORTING
        CURR_REPORT   = ProgName
        CURR_VARIANT  = it_VariName
        VARI_DESC     = VARI_DESC
      TABLES
        VARI_CONTENTS = it_RSPARAMS
        VARI_TEXT     = it_VARIT
      EXCEPTIONS
        others        = 1.

    Call function 'RS_CHANGE_CREATED_VARIANT'
      EXPORTING
        CURR_REPORT   = ProgName
        CURR_VARIANT  = it_VariName
        VARI_DESC     = VARI_DESC
      TABLES
        VARI_CONTENTS = it_RSPARAMS
        VARI_TEXT     = it_VARIT
        VARI_SEL_DESC = it_RSVDESC
        OBJECTS       = it_VANZ
      EXCEPTIONS
        others        = 1.
  EndLoop.
EndForm.                    "ImportVariant

*&---------------------------------------------------------------------*
*&      Form  GetSelectedTADIR
*&---------------------------------------------------------------------*
Form GetSelectedTADIR
  tables it_SelTADIR type tt_TADIR.

  Data:
    it_SelRows type standard table of LVC_S_ROW
      with header line.

  Call method Grid->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = it_SelRows[].

  If it_SelRows[] is initial.
    PutMessage '## ####### ## ##### ######!'.
  EndIf.


  Loop at it_TADIR assigning <tadir>.
    <tadir>-tabix = sy-tabix.
  EndLoop.

  Loop at it_SelRows.
    Read table it_TADIR index it_SelRows-Index
      into it_SelTADIR.

    Read table it_R3TR_Sort with key OBJECT = it_SelTADIR-OBJECT
      binary search.
    If sy-subrc = 0.
      If it_SelTADIR-OBJ_NAME = sy-repid.
        Clear it_R3TR_Sort-SortKey with '9'.
      EndIf.
      it_SelTADIR-SortKey = it_R3TR_Sort-SortKey.
    EndIf.

    Append it_SelTADIR.
  EndLoop.

  Sort it_SelTADIR by
    SortKey
    DEVCLASS
    OBJECT
    OBJ_NAME
    PGMID.

EndForm.                    "GetSelectedTADIR

*&---------------------------------------------------------------------*
*&      Form  ImportToSAP
*&---------------------------------------------------------------------*
Form Do_Import_to_SAP
  using Path
        Queue.
  Data:
    it_SelRows type standard table of LVC_S_ROW
      with header line,
    it_SelTADIR type tt_TADIR
      with header line,
    subrc type sy-subrc,
    PathR3TR type string.

  Field-symbols:
    <tadir>      type t_TADIR,
    <tadir_devc> type t_TADIR.


  Perform GetSelectedTADIR
    tables it_SelTADIR.

  Loop at it_SelTADIR.
    Read table it_TADIR assigning <tadir> index it_SelTADIR-tabix.

    At new DEVCLASS.
      Perform CheckClass using <tadir>.
      If sy-subrc = 0.
        Read table it_TADIR assigning <tadir_devc>
          with key DEVCLASS = <tadir>-DEVCLASS
                   OBJECT   = 'DEVC'.
      EndIf.
      If sy-subrc <> 0.
        Exit.
      EndIf.

      If p_Zip = 'X'.
        Data:
          ZipFileName type string.
*        Concatenate p_Path <tadir>-DEVCLASS '.DEVC.ZIP'
*          into ZipFileName.
        Concatenate p_Path <tadir_devc>-Object_Path '.ZIP'
          into ZipFileName.
        Perform ZIP_Load
          using ZipFileName
          changing FileZIP.
      EndIf.
    EndAt.

    Case Queue.
      when '1'.
        check it_SelTADIR-SortKey < 900.
      when '2'.
        check it_SelTADIR-SortKey >= 900.
    EndCase.

    Data: TextProgress type string.
    Concatenate <tadir>-PGMID <tadir>-OBJECT <tadir>-TEXT
      <tadir>-OBJ_NAME
      into TextProgress separated by space.
    Call function 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = TextProgress.

    "it_SelTADIR-DEVCLASS '.DEVC\'
    Concatenate <tadir_devc>-Object_Path '\'
                it_SelTADIR-OBJECT '\'
      into Path.
    "it_SelTADIR-DEVCLASS '.DEVC\'
    Concatenate <tadir_devc>-Object_Path '\'
      into PathR3TR.
    If p_Zip is initial.
      Concatenate p_Path Path
        into Path.
      Concatenate p_Path PathR3TR
        into PathR3TR.
    EndIf.

    Clear: sy-msgno.
    subrc = isNothing..

    Case <tadir>-Flag.
      when flagNew. "p_New = 'X'.
        Perform Import_R3TR
          using <tadir>
                PathR3TR
                subrc.
*      Data:
*        it_DWINACTIV type standard table of DWINACTIV
*          with header line.
*      Move-corresponding: <tadir> to it_DWINACTIV.
*      it_DWINACTIV-UNAME = sy-uname.
*      Append it_DWINACTIV.
      when flagOld.

        Case it_SelTADIR-OBJECT.
          when 'TRAN'.
            Perform ImportTRAN
              using <tadir>
                    Path
                    subrc.
          when 'MSAG'.
            Perform ImportMSAG
              using <tadir>
                    Path
                    subrc.
          when 'PROG' or 'TYPE'.
            Perform ImportPROG
              using <tadir>
                    Path
                    subrc.
          when 'FUGR'.
            Perform ImportFUGR
              using <tadir>
                    Path
                    subrc.
          when 'TABL'.
            Perform ImportTABL
              using <tadir>
                    Path
                    subrc.
          when 'TABU'.
            Perform ImportTABU
              using <tadir>
                    Path
                    subrc.
          when 'SHLP'.
            Perform ImportSHLP
              using <tadir>
                    Path
                    subrc.
          when 'DTEL'.
            Perform ImportDTEL
              using <tadir>
                    Path
                    subrc.
          when 'DOMA'.
            Perform ImportDOMA
              using <tadir>
                    Path
                    subrc.
          when 'VIEW'.
            Perform ImportVIEW
              using <tadir>
                    Path
                    subrc.
          when 'ENQU'.
            Perform ImportENQU
              using <tadir>
                    Path.
          when 'TTYP'.
            Perform ImportTTYP
              using <tadir>
                    Path.
          when 'W3MI'.
            Perform ImportW3MI
              using <tadir>
                    Path
                    subrc.
        EndCase.
    EndCase.

    Case subrc.
      when isOk.
        Perform WriteMessageTo using isOk changing <tadir>.
      when isNothing.
        Perform WriteMessageTo using isNothing changing <tadir>.
      when others.
        Perform WriteMessageTo using isError changing <tadir>.
    EndCase.

  EndLoop.

*  If p_New = 'X'.
*    Call function 'RS_WORKING_OBJECTS_ACTIVATE'
*      EXPORTING
*        SUPPRESS_GENERATION   = 'X'
*        ACTIVATE_DDIC_OBJECTS = 'X'
*      TABLES
*        OBJECTS               = it_DWINACTIV.
*
*    Call function 'RS_WORKING_OBJECTS_ACTIVATE'
*      EXPORTING
*        SUPPRESS_GENERATION = 'X'
*      TABLES
*        OBJECTS             = it_DWINACTIV.
*  EndIf.

  Perform Activate_Selected_Objects.
  Perform GetStatusObjects changing it_TADIR[].
EndForm.                    "ImportToSAP

*&---------------------------------------------------------------------*
*&      Form  ActivateSelectedObjects
*&---------------------------------------------------------------------*
Form Activate_Selected_Objects.

  Data:
    it_SelTADIR type tt_TADIR
      with header line,
    it_DWINACTIV type standard table of DWINACTIV
      with header line,
    it_Worklist type standard table of E071
      with header line,
    it_ActObj type standard table of DDMSACTRC
      with header line.

  Call function 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = '######### ########...'.

  Perform GetSelectedTADIR
  tables it_SelTADIR.

  Loop at it_SelTADIR.
    Read table it_TADIR assigning <tadir> index it_SelTADIR-tabix.

    At new DEVCLASS.
      Perform CheckClass using <tadir>.
      If sy-subrc <> 0.
        Exit.
      EndIf.
    EndAt.

    Case <tadir>-OBJECT.
      when 'W3MI'.
        "##########
      when 'CLAS' or 'CLSD' or 'METH'
        or 'CPRI' or 'CPRO' or 'CPUB'
        or 'PROG' or 'REPS'.
        Move-corresponding: <tadir> to it_DWINACTIV.
        it_DWINACTIV-UNAME = sy-uname.
        Append it_DWINACTIV.
      when others. "'DOMA' # #.#.
        Move-corresponding: <tadir> to it_Worklist.
        Append it_Worklist.
    EndCase.
  EndLoop.

**RS_INSERT_INTO_WORKING_AREA
**RS_DELETE_FROM_WORKING_AREA
*  Call function 'RS_WORKING_OBJECTS_ACTIVATE'
*    EXPORTING
*      SUPPRESS_GENERATION   = 'X'
*      SUPPRESS_INSERT     = 'X'
*      ACTIVATE_DDIC_OBJECTS = 'X'
*    TABLES
*      OBJECTS               = it_DWINACTIV.

*  Call function 'RS_DD_WORKLIST_ACTIVATE'
*    EXPORTING
*      I_SUPPRESS_DIALOG = 'X'
*    TABLES
*      T_WORKLIST        = it_Worklist
*      T_ACT_OBJ         = it_ActObj
*    EXCEPTIONS
*      NOT_ALL_ACTIVATED = 1
*      ERROR_OCCURED     = 2
*      LOCKED            = 3
*      others            = 99.
*
  Data:
    LOGNAME type DDMASS-LOGNAME.

  Concatenate sy-uname sy-datum sy-timlo into LOGNAME.

  CALL FUNCTION 'DD_WORKLIST_ACT'
    EXPORTING
      LOGNAME              = LOGNAME
* IMPORTING
*   RC                   =
    TABLES
      WORKLIST             = it_Worklist
      ACTRC                = it_ActObj
   EXCEPTIONS
     NO_OBJECTS           = 1
     ACCESS_FAILURE       = 2
     LOCKED               = 3
     OTHERS               = 4.

  Loop at it_ActObj.
    Delete it_DWINACTIV
      where OBJECT   = it_ActObj-TYPE
        and OBJ_NAME = it_ActObj-NAME.
  EndLoop.
*  Perform Activate_Class_Components tables it_Worklist.


  Call function 'RS_WORKING_OBJECTS_ACTIVATE'
    EXPORTING
      SUPPRESS_GENERATION = 'X'
      SUPPRESS_INSERT     = 'X'
    TABLES
      OBJECTS             = it_DWINACTIV
    EXCEPTIONS
      error_message       = 1
      others              = 99.

*  Call function 'RS_DELETE_FROM_WORKING_AREA'
  .
EndForm.                    "ActivateSelectedObjects

*&---------------------------------------------------------------------*
*&      Form  Activate_Class_Components
*&---------------------------------------------------------------------*
Form Activate_Class_Components
  tables it_Worklist type tt_E071.

  Data:
   it_CLSKEYS type SEOC_CLASS_KEYS
     with header line.

  Loop at it_Worklist.
    Case it_Worklist-OBJECT.
      when 'CLAS'.
        Perform Activate_Class_Types
          using it_Worklist-OBJ_NAME.
        Perform Activate_Class_Attributes
          using it_Worklist-OBJ_NAME.
        Perform Activate_Class_Events
          using it_Worklist-OBJ_NAME.
        Perform Activate_Class_Methods
          using it_Worklist-OBJ_NAME.

        Refresh it_CLSKEYS[].
        it_CLSKEYS-CLSNAME = it_Worklist-OBJ_NAME.
        Append it_CLSKEYS.
        Call function 'SEO_CLASS_ACTIVATE'
          EXPORTING
            CLSKEYS = it_CLSKEYS[]
          EXCEPTIONS
            others  = 99.
        If sy-subrc = 0.
          Call function 'SEO_CLASS_RESET_INACTIVE'
            EXPORTING
              CLSKEY = it_CLSKEYS
            EXCEPTIONS
              others = 99.
        EndIf.
    EndCase.
  EndLoop.

EndForm.                    "Activate_Class_Components

*&---------------------------------------------------------------------*
*&      Form  Activate_Class_Types
*&---------------------------------------------------------------------*
Form Activate_Class_Types
  using ClassName.

  Data:
    CIFKEY  type SEOCLSKEY,
    it_TYPKEYS type SEOO_TYPE_KEYS
      with header line,
    it_TYPES type SEOO_TYPES_R
      with header line.

  CIFKEY = ClassName.

  Call function 'SEO_TYPE_READ_ALL'
    EXPORTING
      CIFKEY = CIFKEY
    IMPORTING
      TYPES  = it_TYPES[]
    EXCEPTIONS
      others = 99.
  Loop at it_TYPES.
    it_TYPKEYS-CLSNAME = it_TYPES-CLSNAME.
    it_TYPKEYS-CMPNAME = it_TYPES-CMPNAME.
    Append it_TYPKEYS.
  EndLoop.

  Call function 'SEO_TYPE_ACTIVATE'
    EXPORTING
      CIFKEY  = CIFKEY
      TYPKEYS = it_TYPKEYS[]
    EXCEPTIONS
      others  = 99.
EndForm.                    "Activate_Class_Attributes

*&---------------------------------------------------------------------*
*&      Form  Activate_Class_Attributes
*&---------------------------------------------------------------------*
Form Activate_Class_Attributes
  using ClassName.

  Data:
    CIFKEY  type SEOCLSKEY,
    it_ATTKEYS type SEOO_ATTRIBUTE_KEYS
      with header line,
    it_ATTRIBUTES type SEOO_ATTRIBUTES_R
      with header line.

  CIFKEY = ClassName.

  Call function 'SEO_ATTRIBUTE_READ_ALL'
    EXPORTING
      CIFKEY     = CIFKEY
    IMPORTING
      ATTRIBUTES = it_ATTRIBUTES[]
    EXCEPTIONS
      others     = 99.
  Loop at it_ATTRIBUTES.
    it_ATTKEYS-CLSNAME = it_ATTRIBUTES-CLSNAME.
    it_ATTKEYS-CMPNAME = it_ATTRIBUTES-CMPNAME.
    Append it_ATTKEYS.
  EndLoop.

  Call function 'SEO_ATTRIBUTE_ACTIVATE'
    EXPORTING
      CIFKEY  = CIFKEY
      ATTKEYS = it_ATTKEYS[]
    EXCEPTIONS
      others  = 99.
EndForm.                    "Activate_Class_Attributes

*&---------------------------------------------------------------------*
*&      Form  Activate_Class_Events
*&---------------------------------------------------------------------*
Form Activate_Class_Events
  using ClassName.

  Data:
    CIFKEY  type SEOCLSKEY,
    it_EVTKEYS type SEOO_EVENT_KEYS
      with header line,
    it_EVENTS type SEOO_EVENTS_R
      with header line.

  CIFKEY = ClassName.

  Call function 'SEO_EVENT_READ_ALL'
    EXPORTING
      CIFKEY = CIFKEY
    IMPORTING
      EVENTS = it_EVENTS[]
    EXCEPTIONS
      others = 99.
  Loop at it_EVENTS.
    it_EVTKEYS-CLSNAME = it_EVENTS-CLSNAME.
    it_EVTKEYS-CMPNAME = it_EVENTS-CMPNAME.
    Append it_EVTKEYS.
  EndLoop.

  Call function 'SEO_EVENT_ACTIVATE'
    EXPORTING
      CIFKEY  = CIFKEY
      EVTKEYS = it_EVTKEYS[]
    EXCEPTIONS
      others  = 99.
EndForm.                    "Activate_Class_Events

*&---------------------------------------------------------------------*
*&      Form  Activate_Class_Methods
*&---------------------------------------------------------------------*
Form Activate_Class_Methods
  using ClassName.

  Data:
    CIFKEY  type SEOCLSKEY,
    it_MTDKEYS type SEOO_METHOD_KEYS
      with header line,
    it_METHODS type SEOO_METHODS_R
      with header line.

  CIFKEY = ClassName.

  Call function 'SEO_METHOD_READ_ALL'
    EXPORTING
      CIFKEY  = CIFKEY
    IMPORTING
      METHODS = it_METHODS[]
    EXCEPTIONS
      others  = 99.
  Loop at it_METHODS.
    it_MTDKEYS-CLSNAME = it_METHODS-CLSNAME.
    it_MTDKEYS-CMPNAME = it_METHODS-CMPNAME.
    Append it_MTDKEYS.
  EndLoop.

  Call function 'SEO_METHOD_ACTIVATE'
    EXPORTING
      CIFKEY  = CIFKEY
      MTDKEYS = it_MTDKEYS[]
    EXCEPTIONS
      others  = 99.
EndForm.                    "Activate_Class_Methods


*&---------------------------------------------------------------------*
*&      Form  ExportToFiles
*&---------------------------------------------------------------------*
Form Do_Export_to_Files
  using p_Path.

  Data:
    Path type string,
    it_SelRows type standard table of LVC_S_ROW
      with header line,
    it_SelTADIR type tt_TADIR
      with header line,
    subrc type sy-subrc,
    subrcDEVC type sy-subrc,
    PathR3TR type string.

  Field-symbols:
    <tadir> type t_TADIR.

  Call method Grid->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = it_SelRows[].

  If it_SelRows[] is initial.
    PutMessage '## ####### ## ##### ######!'.
  EndIf.

  Loop at it_TADIR assigning <tadir>.
    <tadir>-tabix = sy-tabix.
  EndLoop.

  Loop at it_SelRows.
    Read table it_TADIR index it_SelRows-Index
      into it_SelTADIR.
    Append it_SelTADIR.
  EndLoop.

  Sort it_SelTADIR by
    DEVCLASS
    OBJECT
    OBJ_NAME
    PGMID.

  Loop at it_SelTADIR.
    Read table it_TADIR assigning <tadir> index it_SelTADIR-tabix.

    At new DEVCLASS.
      Clear FileZIP.

      subrcDEVC = isNothing.

      If p_ZIP = 'X'.
        FileName = util=>FileName_Escape( it_SelTADIR-DEVCLASS ).

        Concatenate p_Path FileName '.DEVC.ZIP'
          into FileName.

        Perform ZIP_Load
          using FileName
          changing FileZIP.
      EndIf.
    EndAt.

    At new OBJECT.
      Refresh it_Readme.
    EndAt.

    Data: TextProgress type string.
    Concatenate <tadir>-PGMID <tadir>-OBJECT <tadir>-TEXT
      <tadir>-OBJ_NAME
      into TextProgress separated by space.
    Call function 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = TextProgress.

    FileName = util=>FileName_Escape( <tadir>-DEVCLASS ).
    Concatenate FileName '.DEVC\'
      into PathR3TR.

    Concatenate PathR3TR <tadir>-OBJECT '\'
      into Path.

    If p_ZIP is initial.
      Concatenate p_Path Path     into Path.
      Concatenate p_Path PathR3TR into PathR3TR.
    EndIf.

    Clear: sy-msgno.
    subrc = isNothing.

    If p_New = 'X' or p_All = 'X'.
      Perform Export_R3TR
        tables it_Readme
        using <tadir>
              PathR3TR
              subrc.
    EndIf.

    If p_Old = 'X' or
       ( p_All = 'X' and subrc <> isOk ).

      Case <tadir>-OBJECT.
        when 'DEVC'.
          Perform ExportDEVC
            tables it_Readme
            using <tadir>
                  Path.
        when 'TRAN'.
          Perform ExportTRAN
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'MSAG'.
          Perform ExportMSAG
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'PROG' or 'TYPE'.
          Perform ExportPROG
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'FUGR'.
          Perform ExportFUGR
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
*        when 'CLAS'.
*          Perform ExportCLAS
*            tables it_Readme
*            using <tadir>
*                  Path
*                  subrc.
        when 'W3MI'.
          Perform ExportW3MI
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'TABL'.
          Perform ExportTABL
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'TABU'.
          Perform ExportTABU
            tables it_Readme
                   it_E071K
            using <tadir>
                  Path
                  subrc.
        when 'DOMA'.
          Perform ExportDOMA
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'DTEL'.
          Perform ExportDTEL
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when 'SHLP'.
          Perform ExportSHLP
            tables it_Readme
            using <tadir>-OBJ_NAME
                  Path
                  subrc.
        when 'VIEW'.
          Perform ExportVIEW
            tables it_Readme
            using <tadir>-OBJ_NAME
                  Path
                  subrc.
        when 'ENQU'.
          Perform ExportENQU
            tables it_Readme
            using <tadir>-OBJ_NAME
                  Path
                  subrc.
        when 'TTYP'.
          Perform ExportTTYP
            tables it_Readme
            using <tadir>
                  Path
                  subrc.
        when others.
          Perform Export_R3TR
            tables it_Readme
            using <tadir>
                  PathR3TR
                  subrc.
      EndCase.
    EndIf.


    Case subrc.
      when isOk.
        subrcDEVC = isOk.
        Perform WriteMessageTo using isOk changing <tadir>.
      when isNothing.
        Perform WriteMessageTo using isNothing changing <tadir>.
      when others.
        Perform WriteMessageTo using isError changing <tadir>.
    EndCase.

    At end of OBJECT.
      If not it_Readme[] is initial.
        Concatenate Path '_Readme.txt' into FileName.

        Perform Write_File
          using FileName
          changing it_Readme[].

      EndIf.
    EndAt.

    At end of DEVCLASS.
      If p_ZIP = 'X' and subrcDEVC = isOk.

        Perform ZIP_Save
          using FileZIP.

      EndIf.
    EndAt.
  EndLoop.

EndForm.                    "ExportToFiles

*&---------------------------------------------------------------------*
*&      Form  SelectForExport
*&---------------------------------------------------------------------*
Form SelectDBForExport.
  Field-symbols:
    <tadir> type t_TADIR.

  If s_TRKORR[] is initial.
    Select *
      into corresponding fields of table it_TADIR
      from TADIR
      where TADIR~PGMID    in s_PGMID
        and TADIR~DEVCLASS in s_CLASS
        and TADIR~OBJECT   in s_OBJECT
        and TADIR~OBJ_NAME in s_NAME
      order by
        TADIR~DEVCLASS
        TADIR~OBJECT
        TADIR~OBJ_NAME.

  Else.
    Clear it_TADIR.
    Select *
      into corresponding fields of E071
      from E071
      join E070
        on E070~TRKORR = E071~TRKORR
      where E071~PGMID    in s_PGMID
        and E071~OBJECT   in s_OBJECT
        and E071~OBJ_NAME in s_NAME
        and ( E070~TRKORR  in s_TRKORR or
              E070~STRKORR in s_TRKORR ).


      Move-corresponding E071 to it_TADIR.

      Perform ConvertLIMUtoR3TR changing it_TADIR.

      Data:
        w_Object type TADIR-OBJECT.

      w_Object = it_TADIR-OBJECT.
      If it_TADIR-OBJECT = 'TABU'.
        w_Object = 'TABL'.
      EndIf.

      Select TADIR~DEVCLASS
        into it_TADIR-DEVCLASS
        from TADIR
        where TADIR~OBJ_NAME = it_TADIR-OBJ_NAME
          and TADIR~PGMID    = it_TADIR-PGMID
          and TADIR~OBJECT   = w_Object.
      EndSelect.

*      If not s_CLASS[] is initial.
      check it_TADIR-DEVCLASS in s_CLASS.
*      EndIf.

      Case it_TADIR-OBJECT.
        when 'TABU'
          or 'TDAT'.

          Select *
            appending table it_E071K
            from E071K
            where TRKORR = E071-TRKORR
              and PGMID  = E071-PGMID
*(28.02.2014
*              and OBJECT = E071-OBJECT
*              and OBJNAME = E071-OBJ_NAME.
              and MASTERTYPE = E071-OBJECT
              and MASTERNAME = E071-OBJ_NAME.
*)28.02.2014
      EndCase.

      Append it_TADIR.

      Clear it_TADIR.
    EndSelect.
  EndIf.

  Loop at it_TADIR assigning <tadir>
    where PGMID = 'LIMU'.

    Perform ConvertLIMUtoR3TR changing it_TADIR.
  EndLoop.

*  Delete it_TADIR where PGMID <> 'R3TR'.

  Sort it_TADIR by
    DEVCLASS
    OBJECT
    OBJ_NAME
    PGMID.
  Delete adjacent duplicates from it_TADIR.

  Perform CheckObjects changing it_TADIR[].
  Perform GetStatusObjects changing it_TADIR[].
EndForm.                    "SelectForExport

*&---------------------------------------------------------------------*
*&      Form  ExportDEVC
*&---------------------------------------------------------------------*
Form ExportDEVC
  tables it_Readme
  using w_TADIR type t_TADIR
        Path.

  Select single CTEXT
    into it_Readme
    from TDEVCT
    where DEVCLASS = w_TADIR-OBJ_NAME
      and SPRAS = sy-langu.
  If sy-subrc = 0.
    Concatenate w_TADIR-OBJ_NAME '-' it_Readme into it_Readme
      separated by space.
    Append it_Readme.
  EndIf.
EndForm.                    "ExportDEVC


*&---------------------------------------------------------------------*
*&      Form  Get_R3TR_TRAN
*&---------------------------------------------------------------------*
Form Get_R3TR_TRAN
  using Obj_Name
  changing Struc_TRAN type t_R3TR_TRAN
           subrc.

  subrc = isError.

  Select * into table Struc_TRAN-TSTC[] from TSTC
    where TCODE = Obj_Name.
  check sy-subrc = 0.
  Select * into table Struc_TRAN-TSTCA[] from TSTCA
    where TCODE = Obj_Name.
  Select * into table Struc_TRAN-TSTCT[] from TSTCT
    where TCODE = Obj_Name and SPRSL = sy-langu.
  Select * into table Struc_TRAN-TSTCP[] from TSTCP
    where TCODE = Obj_Name.
  Select * into table Struc_TRAN-TSTCC[] from TSTCC
    where TCODE = Obj_Name.

  subrc = isOk.

EndForm.                    "Get_R3TR_TRAN
*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_TRAN
*&---------------------------------------------------------------------*
*Form Export_R3TR_TRAN
*  using Obj_Name
*        Path
*        subrc.
*  Data:
*    it_Readme type tt_Readme,
*    w_TADIR type t_TADIR.
*
*  w_TADIR-Obj_Name = Obj_Name.
*
*  Perform ExportTRAN
*    tables it_Readme
*    using w_TADIR
*          Path
*          subrc.
*EndForm.                    "Export_R3TR_TRAN

*&---------------------------------------------------------------------*
*&      Form  ExportTRAN
*&---------------------------------------------------------------------*
Form ExportTRAN
  tables it_Readme
  using w_TADIR type t_TADIR
        Path
        subrc.
  Data:
    FileName type string,
    w_TSTCT type TSTCT,
    it_TSTC type standard table of TSTC,
    it_TSTCT type standard table of TSTCT,
    it_TSTCP type standard table of TSTCP,
    it_TSTCC type standard table of TSTCC.

  subrc = isError.

  Select * into table it_TSTC from TSTC
    where TCODE = w_TADIR-OBJ_NAME.
  check sy-subrc = 0.
  Select * into table it_TSTCT from TSTCT
    where TCODE = w_TADIR-OBJ_NAME and SPRSL = sy-langu.
  Select * into table it_TSTCP from TSTCP
    where TCODE = w_TADIR-OBJ_NAME.
  Select * into table it_TSTCC from TSTCC
    where TCODE = w_TADIR-OBJ_NAME.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTC.txt' into FileName.
  Perform Write_File using FileName changing it_TSTC.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTCT.txt' into FileName.
  Perform Write_File using FileName changing it_TSTCT.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTCP.txt' into FileName.
  Perform Write_File using FileName changing it_TSTCP.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTCC.txt' into FileName.
  Perform Write_File using FileName changing it_TSTCC.

  Read table it_TSTCT into w_TSTCT index 1.
  If sy-subrc = 0.
    it_Readme = w_TSTCT-TTEXT.
    Concatenate w_TADIR-OBJ_NAME '-' it_Readme into it_Readme
      separated by space.
    Append it_Readme.
  EndIf.

  subrc = isOk.
EndForm.                    "ExportTRAN

*&---------------------------------------------------------------------*
*&      Form  ImportTRAN
*&---------------------------------------------------------------------*
Form ImportTRAN
  using w_TADIR type t_TADIR
        Path
        subrc.
  Types:
    Begin of t_TRAN,
      it_TSTC  type standard table of TSTC
        with key table_line,
      it_TSTCT type standard table of TSTCT
        with key table_line,
      it_TSTCP type standard table of TSTCP
        with key table_line,
      it_TSTCC type standard table of TSTCC
        with key table_line,
    End of t_TRAN.

  Data:
    FileName type string,
    NewTRAN type t_TRAN,
    OldTRAN type t_TRAN,
    DDOBJNAME type  DDOBJNAME.

  subrc = isError.

  DDOBJNAME = w_TADIR-OBJ_NAME.


  Concatenate Path w_TADIR-OBJ_NAME '\TSTC.txt' into FileName.
  Perform Read_File using FileName changing NewTRAN-it_TSTC.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTCT.txt' into FileName.
  Perform Read_File using FileName changing NewTRAN-it_TSTCT.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTCP.txt' into FileName.
  Perform Read_File using FileName changing NewTRAN-it_TSTCP.

  Concatenate Path w_TADIR-OBJ_NAME '\TSTCC.txt' into FileName.
  Perform Read_File using FileName changing NewTRAN-it_TSTCC.

  If p_Rewrte is initial.
    Select * into table OldTRAN-it_TSTC from TSTC
      where TCODE = w_TADIR-OBJ_NAME.
    Select * into table OldTRAN-it_TSTCT from TSTCT
      where TCODE = w_TADIR-OBJ_NAME and SPRSL = sy-langu.
    Select * into table OldTRAN-it_TSTCP from TSTCP
      where TCODE = w_TADIR-OBJ_NAME.
    Select * into table OldTRAN-it_TSTCC from TSTCC
      where TCODE = w_TADIR-OBJ_NAME.
  EndIf.

  If NewTRAN <> OldTRAN and
     not NewTRAN is initial.

    Modify TSTC  from table NewTRAN-it_TSTC.
    Modify TSTCT from table NewTRAN-it_TSTCT.
    Modify TSTCP from table NewTRAN-it_TSTCP.
    Modify TSTCC from table NewTRAN-it_TSTCC.

    Perform SetClassForObject using w_TADIR.

    If sy-subrc = 0.
      Perform AppendToQuery using w_TADIR.
      subrc = isOk.
*      Call function 'DDIF_DOMA_ACTIVATE'
*        EXPORTING
*          NAME   = DDOBJNAME
*        EXCEPTIONS
*          others = 1.
    EndIf.

  EndIf.

EndForm.                    "ImportTRAN


*&---------------------------------------------------------------------*
*&      Form  Export_LIMU_MSAD
*&---------------------------------------------------------------------*
*Form Export_LIMU_MSAD
*  using Obj_Name
*        Path
*        subrc.
*  Data:
*    FileName type string,
*    w_T100A  type T100A,
*    it_T100A type standard table of T100A,
*    it_T100T type standard table of T100T.
*
*  subrc = isError.
*
*  Select * into table it_T100A from T100A
*    where ARBGB = Obj_Name.
*  check sy-subrc = 0.
*
*  Select * into table it_T100T from T100T
*    where SPRSL = sy-langu
*      and ARBGB = Obj_Name.
*
*  Concatenate Path Obj_Name '\T100A.txt' into FileName.
*  Perform Write_File using FileName changing it_T100A.
*
*  Concatenate Path Obj_Name '\T100T.txt' into FileName.
*  Perform Write_File using FileName changing it_T100T.
*
*  subrc = isOk.
*EndForm.                    "Export_LIMU_MSAD

*&---------------------------------------------------------------------*
*&      Form  Export_LIMU_MESS
*&---------------------------------------------------------------------*
*Form Export_LIMU_MESS
*  using Obj_Name
*        Path
*        subrc.
*
*  Data:
*    FileName type string,
*    it_T100  type standard table of T100,
*    it_T100U type standard table of T100U,
*    it_T100X type standard table of T100X.
*
*  subrc = isError.
*
** CLM_CWB_MESS_GET
*
*  Select * into table it_T100 from T100
*    where SPRSL = sy-langu
*      and ARBGB = Obj_Name.
*
*  Select * into table it_T100U from T100U
*    where ARBGB = Obj_Name.
*
*  Select * into table it_T100X from T100X
*    where ARBGB = Obj_Name.
*
*  Concatenate Path Obj_Name '\T100.txt' into FileName.
*  Perform Write_File using FileName changing it_T100.
*
*  Concatenate Path Obj_Name '\T100U.txt' into FileName.
*  Perform Write_File using FileName changing it_T100U.
*
*  Concatenate Path Obj_Name '\T100X.txt' into FileName.
*  Perform Write_File using FileName changing it_T100X.
*
*  subrc = isOk.
*EndForm.                    "Export_LIMU_MESS

*&---------------------------------------------------------------------*
*&      Form  Export_MSAG
*&---------------------------------------------------------------------*
*Form Export_R3TR_MSAG
*  using Obj_Name
*        Path
*        subrc.

*  Data:
*    limu_subrc type sy-subrc,
*    limu_Path type string.
*
*  subrc = isError.
*
*  Concatenate Path Obj_Name '\MSAD.LIMU\'  into limu_Path.
*  Perform Export_LIMU_MSAD
*    using Obj_Name
*          limu_Path
*          limu_subrc.
*  If limu_subrc = 0.
*    subrc = limu_subrc.
*  EndIf.
*
*  Concatenate Path Obj_Name '\MESS.LIMU\' into limu_Path.
*  Perform Export_LIMU_MESS
*    using Obj_Name
*          limu_Path
*          limu_subrc.
*  If limu_subrc = 0.
*    subrc = limu_subrc.
*  EndIf.
*EndForm.                    "Export_MSAG

*&---------------------------------------------------------------------*
*&      Form  ExportMSAG
*&---------------------------------------------------------------------*
Form ExportMSAG
  tables it_Readme
  using w_TADIR type t_TADIR
        Path
        subrc.
  Data:
    FileName type string,
    w_T100A  type T100A,
    it_T100A type standard table of T100A,
    it_T100T type standard table of T100T,
    it_T100  type standard table of T100,
    it_T100U type standard table of T100U,
    it_T100X type standard table of T100X.

  subrc = isError.

  Select * into table it_T100A from T100A
    where ARBGB = w_TADIR-OBJ_NAME.
  check sy-subrc = 0.

  Select * into table it_T100T from T100T
    where SPRSL = sy-langu
      and ARBGB = w_TADIR-OBJ_NAME.

  Select * into table it_T100 from T100
    where SPRSL = sy-langu
      and ARBGB = w_TADIR-OBJ_NAME.

  Select * into table it_T100U from T100U
    where ARBGB = w_TADIR-OBJ_NAME.

  Select * into table it_T100X from T100X
    where ARBGB = w_TADIR-OBJ_NAME.

  Concatenate Path w_TADIR-OBJ_NAME '\T100A.txt' into FileName.
  Perform Write_File using FileName changing it_T100A.

  Concatenate Path w_TADIR-OBJ_NAME '\T100T.txt' into FileName.
  Perform Write_File using FileName changing it_T100T.

  Concatenate Path w_TADIR-OBJ_NAME '\T100.txt' into FileName.
  Perform Write_File using FileName changing it_T100.

  Concatenate Path w_TADIR-OBJ_NAME '\T100U.txt' into FileName.
  Perform Write_File using FileName changing it_T100U.

  Concatenate Path w_TADIR-OBJ_NAME '\T100X.txt' into FileName.
  Perform Write_File using FileName changing it_T100X.

  Read table it_T100A into w_T100A index 1.
  If sy-subrc = 0.
    it_Readme = w_T100A-STEXT.
    Concatenate w_TADIR-OBJ_NAME '-' it_Readme into it_Readme
      separated by space.
    Append it_Readme.
  EndIf.

  subrc = isOk.
EndForm.                    "ExportMSAG


*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_MSAD
*&---------------------------------------------------------------------*
Form Import_LIMU_MSAD
  using
    Obj_Name
    Struc_MSAD
    subrc.

  Field-symbols:
    <table> type table.

  subrc = isError.

  Assign component 'T100A' of structure Struc_MSAD to <table>.
  If sy-subrc = 0.
    Modify T100A from table <table>.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

  Assign component 'T100T' of structure Struc_MSAD to <table>.
  If sy-subrc = 0.
    Modify T100T from table <table>.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

EndForm.                    "Import_LIMU_MSAD

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_MESS
*&---------------------------------------------------------------------*
Form Import_LIMU_MESS
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    FileName  type string,
    DDOBJNAME type  DDOBJNAME.
  Field-symbols:
*    <MESS>, " type SVRS2_MESS.
    <table> type table.
  Data:
*   LANGU TYPE MSGLANGU OCCURS 0,
    it_T100  TYPE T100 OCCURS 0,
    it_T100O TYPE T100O OCCURS 0,
    it_T100U TYPE T100U OCCURS 0,
    it_T100X TYPE T100X OCCURS 0.

  subrc = isError.

*  Assign Struc_LIMU to <MESS>.

  DDOBJNAME = Obj_Name.
*CLM_CWB_MESS_SAVE
  Assign component 'T100' of structure Struc_LIMU to <table>.
  If sy-subrc = 0.
    Perform Table_Corresponding tables <table> it_T100.
    Modify T100  from table it_T100.
  EndIf.
  Assign component 'T100O' of structure Struc_LIMU to <table>.
  If sy-subrc = 0.
    Perform Table_Corresponding tables <table> it_T100O.
    Modify T100O from table it_T100O.
  EndIf.
  Assign component 'T100U' of structure Struc_LIMU to <table>.
  If sy-subrc = 0.
    Perform Table_Corresponding tables <table> it_T100U.
    Modify T100U from table it_T100U.
  EndIf.
  Assign component 'T100X' of structure Struc_LIMU to <table>.
  If sy-subrc = 0.
    Perform Table_Corresponding tables <table> it_T100X.
    Modify T100X from table it_T100X.
  EndIf.

  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_MESS

*&---------------------------------------------------------------------*
*&      Form  ImportMSAG
*&---------------------------------------------------------------------*
Form ImportMSAG
  using w_TADIR type t_TADIR
        Path
        subrc.
  Types:
    Begin of t_MSAG,
      it_T100A type standard table of T100A
        with key table_line,
      it_T100T type standard table of T100T
        with key table_line,
      it_T100  type standard table of T100
        with key table_line,
      it_T100U type standard table of T100U
        with key table_line,
      it_T100X type standard table of T100X
        with key table_line,
  End of t_MSAG.

  Data:
    FileName  type string,
    NewMSAG   type t_MSAG,
    OldMSAG   type t_MSAG,
    DDOBJNAME type  DDOBJNAME.

  subrc = isError.

  DDOBJNAME = w_TADIR-OBJ_NAME.

  Concatenate Path w_TADIR-OBJ_NAME '\T100A.txt' into FileName.
  Perform Read_File using FileName changing NewMSAG-it_T100A.

  Concatenate Path w_TADIR-OBJ_NAME '\T100T.txt' into FileName.
  Perform Read_File using FileName changing NewMSAG-it_T100T.

  Concatenate Path w_TADIR-OBJ_NAME '\T100.txt' into FileName.
  Perform Read_File using FileName changing NewMSAG-it_T100.

  Concatenate Path w_TADIR-OBJ_NAME '\T100U.txt' into FileName.
  Perform Read_File using FileName changing NewMSAG-it_T100U.

  Concatenate Path w_TADIR-OBJ_NAME '\T100X.txt' into FileName.
  Perform Read_File using FileName changing NewMSAG-it_T100X.

  If p_Rewrte is initial.
    Select * into table OldMSAG-it_T100A from T100A
      where ARBGB = w_TADIR-OBJ_NAME.

    Select * into table OldMSAG-it_T100T from T100T
      where SPRSL = sy-langu
        and ARBGB = w_TADIR-OBJ_NAME.

    Select * into table OldMSAG-it_T100 from T100
      where SPRSL = sy-langu
        and ARBGB = w_TADIR-OBJ_NAME.

    Select * into table OldMSAG-it_T100U from T100U
      where ARBGB = w_TADIR-OBJ_NAME.

    Select * into table OldMSAG-it_T100X from T100X
      where ARBGB = w_TADIR-OBJ_NAME.

  EndIf.

  If NewMSAG <> OldMSAG and
     not NewMSAG is initial.

    Modify T100A from table NewMSAG-it_T100A.
    Modify T100T from table NewMSAG-it_T100T.
    Modify T100  from table NewMSAG-it_T100 .
    Modify T100U from table NewMSAG-it_T100U.
    Modify T100X from table NewMSAG-it_T100X.

    Perform SetClassForObject using w_TADIR.

    If sy-subrc = 0.
      Perform AppendToQuery using w_TADIR.
      subrc = isOk.
*      Call function 'DDIF_DOMA_ACTIVATE'
*        EXPORTING
*          NAME   = DDOBJNAME
*        EXCEPTIONS
*          others = 1.
    EndIf.

  EndIf.

EndForm.                    "ImportMSAG

*&---------------------------------------------------------------------*
*&      Form  ExportInclude
*&---------------------------------------------------------------------*
Form ExportInclude
  tables it_Readme
  using IncludeName
        MainFileName
        PathIncl
        subrc type sy-subrc.

  Data:
    it_Source type tt_Source,
    FileName type string,
    it_TextPool type standard table of TEXTPOOL
      with header line,
    it_TextPoolFile type standard table of t_TextPoolFile
      with header line,
    it_Docum type standard table of TLINE
      with header line,
    it_TRDIR type standard table of TRDIR
      with header line,
    it_DDTYPET type standard table of DDTYPET
      with header line,
    it_ADM type standard table of RSMPE_ADM
      with header line,
    it_STA type standard table of RSMPE_STAT,
    it_FUN type standard table of RSMPE_FUNT,
    it_MEN type standard table of RSMPE_MEN,
    it_MTX type standard table of RSMPE_MNLT,
    it_ACT type standard table of RSMPE_ACT,
    it_BUT type standard table of RSMPE_BUT,
    it_PFK type standard table of RSMPE_PFK,
    it_SET type standard table of RSMPE_STAF,
    it_DOC type standard table of RSMPE_ATRT,
    it_TIT type standard table of RSMPE_TITT,
    it_BIV type standard table of RSMPE_BUTS.

  subrc = isError.

  Read report IncludeName into it_Source.
  If sy-subrc <> 0.
    Exit.
  Else.
    Select * from TRDIR into table it_TRDIR
      where NAME = IncludeName.
    Concatenate PathIncl 'TRDIR.txt' into FileName.
    Perform Write_File using FileName changing it_TRDIR[].

    If not it_TRDIR[] is initial.
      Read table it_TRDIR index 1.
    EndIf.

    If it_TRDIR-SQLX is initial or IncludeName(4) = 'SAPL'.
      Concatenate PathIncl MainFileName '.txt'
        into FileName.
      Perform Write_File using FileName changing it_Source.
    EndIf.

    Refresh it_TextPoolFile.
    Loop at it_LANGU.
      Refresh it_TextPool.
      Read textpool IncludeName into it_TextPool language it_LANGU.
      Loop at it_TextPool.
        Move-corresponding: it_TextPool to it_TextPoolFile.
        it_TextPoolFile-LANGU = it_LANGU.
        Append it_TextPoolFile.
      EndLoop.
    EndLoop.

    If not it_TextPoolFile[] is initial.
      Concatenate PathIncl 'TEXTPOOL.txt'
        into FileName.
      Perform Write_File using FileName changing it_TextPoolFile[].

      Read table it_TextPoolFile
        with key LANGU = sy-langu
                 ID = 'R'.

      If sy-subrc = 0.
        it_Readme = it_TextPoolFile-ENTRY.
        Concatenate IncludeName '-' it_Readme into it_Readme
          separated by space.
        Append it_Readme.
      EndIf.
    EndIf.

*    Case w_TADIR-OBJECT.
*      when 'TYPE'.
*        Select *
*          into table it_DDTYPET
*          from DDTYPET
*          where TYPEGROUP = w_TADIR-OBJ_NAME
*            and DDLANGUAGE = sy-langu.
*        Concatenate PathIncl w_TADIR-OBJ_NAME '\DDTYPET.txt'
*          into FileName.
*        Perform Write_File using FileName changing it_DDTYPET[].
*    EndCase.

*   Screens
*    Types:
*      Begin of t_DynId,
*        PROG type D020S-PROG,
*        DNUM type D020S-DNUM,
*      End of t_DynId.
*
*    Data:
*      it_DynId type table of t_DynId
*        with header line,
*      it_D020S type standard table of D020S
*        with header line,
*      it_D021S type standard table of D021S,
*      it_D022S type standard table of D022S,
*      it_D023S type standard table of D023S.
*
*    Select *
*      into corresponding fields of table it_DynId
*      from D020S
*      where PROG = IncludeName "w_TADIR-OBJ_NAME
*        and TYPE <> 'S'.
*    Loop at it_DynId.
*      Refresh: it_D020S[], it_D021S[], it_D022S[], it_D023S[].
*      Import dynpro it_D020S it_D021S[] it_D022S[] it_D023S[]
*        id it_DynId.
*      If sy-subrc = 0.
*        Append it_D020S.
*        Concatenate PathIncl 'SCREEN\' it_DynId-DNUM
*          '\D020S.txt' into FileName.
*        Perform Write_File using FileName changing it_D020S[].
*        Concatenate PathIncl 'SCREEN\' it_DynId-DNUM
*          '\D021S.txt' into FileName.
*        Perform Write_File using FileName changing it_D021S[].
*        Concatenate PathIncl 'SCREEN\' it_DynId-DNUM
*          '\D022S.txt' into FileName.
*        Perform Write_File using FileName changing it_D022S[].
*        Concatenate PathIncl 'SCREEN\' it_DynId-DNUM
*          '\D023S.txt' into FileName.
*        Perform Write_File using FileName changing it_D023S[].
*      EndIf.
*    EndLoop.


    Call function 'DOC_OBJECT_GET'
      EXPORTING
        CLASS     = 'RE'
        NAME      = IncludeName
      TABLES
        ITF_LINES = it_Docum
      EXCEPTIONS
        others    = 1.

    If sy-subrc = 0.
      Concatenate PathIncl 'DOC.txt' into FileName.
      Perform Write_File using FileName changing it_Docum[].
    EndIf.

*    Call function 'RS_CUA_INTERNAL_FETCH'
*      EXPORTING
*        PROGRAM = IncludeName
*      IMPORTING
*        ADM     = it_ADM
*      TABLES
*        STA     = it_STA
*        FUN     = it_FUN
*        MEN     = it_MEN
*        MTX     = it_MTX
*        ACT     = it_ACT
*        BUT     = it_BUT
*        PFK     = it_PFK
*        SET     = it_SET
*        DOC     = it_DOC
*        TIT     = it_TIT
*        BIV     = it_BIV
*      EXCEPTIONS
*        others  = 1.
*    If sy-subrc = 0.
*      If not it_ADM is initial.
*        Append it_ADM.
*        Concatenate PathIncl 'GUI-STATUS\ADM.txt'
*          into FileName.
*        Perform Write_File using FileName changing it_ADM[].
*      EndIf.
*      Concatenate PathIncl 'GUI-STATUS\STA.txt'
*        into FileName.
*      Perform Write_File using FileName changing it_STA[].
*      Concatenate PathIncl 'GUI-STATUS\FUN.txt'
*        into FileName.
*      Perform Write_File using FileName changing it_FUN[].
*      Concatenate PathIncl 'GUI-STATUS\MEN.txt'
*        into FileName.
*      Perform Write_File using FileName changing it_MEN[].
*      Concatenate PathIncl 'GUI-STATUS\MTX.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_MTX[].
*      Concatenate PathIncl 'GUI-STATUS\ACT.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_ACT[].
*      Concatenate PathIncl 'GUI-STATUS\BUT.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_BUT[].
*      Concatenate PathIncl 'GUI-STATUS\PFK.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_PFK[].
*      Concatenate PathIncl 'GUI-STATUS\SET.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_SET[].
*      Concatenate PathIncl 'GUI-STATUS\DOC.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_DOC[].
*      Concatenate PathIncl 'GUI-STATUS\TIT.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_TIT[].
*      Concatenate PathIncl 'GUI-STATUS\BIV.txt' into
*      FileName.
*      Perform Write_File using FileName changing it_BIV[].
*    EndIf.

    subrc = isOk.
  EndIf.
EndForm.                    "ExportInclude


*&---------------------------------------------------------------------*
*&      Form  ExportProgram
*&---------------------------------------------------------------------*
Form ExportPROG
  tables it_Readme
  using w_TADIR type t_TADIR
        Path
        subrc type sy-subrc.

  Data:
    it_Source type tt_Source,
    PathIncl type string,
    MainProgName type TADIR-OBJ_NAME,
    MainFileName type string,
    FileName type string,
*    it_TextPool type standard table of TEXTPOOL
*      with header line,
*    it_Docum type standard table of TLINE
*      with header line,
    it_TRDIR type standard table of TRDIR
      with header line,
    it_DDTYPET type standard table of DDTYPET
      with header line,
    it_ADM type standard table of RSMPE_ADM
      with header line,
    it_STA type standard table of RSMPE_STAT,
    it_FUN type standard table of RSMPE_FUNT,
    it_MEN type standard table of RSMPE_MEN,
    it_MTX type standard table of RSMPE_MNLT,
    it_ACT type standard table of RSMPE_ACT,
    it_BUT type standard table of RSMPE_BUT,
    it_PFK type standard table of RSMPE_PFK,
    it_SET type standard table of RSMPE_STAF,
    it_DOC type standard table of RSMPE_ATRT,
    it_TIT type standard table of RSMPE_TITT,
    it_BIV type standard table of RSMPE_BUTS.

  subrc = isError.

  Perform GetMainProgName
    using w_TADIR
    changing MainProgName
             MainFileName.

  Concatenate Path w_TADIR-OBJ_NAME '\' into PathIncl.

  Perform ExportInclude
    tables it_Readme
    using MainProgName
          MainFileName
          PathIncl
          subrc.

*  Read report MainProgName into it_Source.
  If subrc <> 0.
    Exit.
  Else.
*    Select * from TRDIR into table it_TRDIR
*      where NAME = MainProgName.
*    Concatenate Path w_TADIR-OBJ_NAME '\TRDIR.txt' into FileName.
*    Perform Write_File using FileName changing it_TRDIR[].
*
*    If not it_TRDIR[] is initial.
*      Read table it_TRDIR index 1.
*    EndIf.
*
*    If it_TRDIR-SQLX is initial or MainProgName(4) = 'SAPL'.
*      Concatenate Path w_TADIR-OBJ_NAME '\' MainFileName '.txt'
*        into FileName.
*      Perform Write_File using FileName changing it_Source.
*    EndIf.
*
*    Read textpool MainProgName into it_TextPool language sy-langu.
*
*    If sy-subrc = 0.
*      Concatenate Path w_TADIR-OBJ_NAME '\TEXTPOOL.txt'
*        into FileName.
*      Perform Write_File using FileName changing it_TextPool[].
*
*      Read table it_TextPool
*        with key ID = 'R'.
*
*      If sy-subrc = 0.
*        it_Readme = it_TextPool-ENTRY.
*        Concatenate w_TADIR-OBJ_NAME '-' it_Readme into it_Readme
*          separated by space.
*        Append it_Readme.
*      EndIf.
*    EndIf.

    Case w_TADIR-OBJECT.
      when 'TYPE'.
        Select *
          into table it_DDTYPET
          from DDTYPET
          where TYPEGROUP = w_TADIR-OBJ_NAME
            and DDLANGUAGE = sy-langu.
        Concatenate Path w_TADIR-OBJ_NAME '\DDTYPET.txt'
          into FileName.
        Perform Write_File using FileName changing it_DDTYPET[].
    EndCase.

*   Screens
    Types:
      Begin of t_DynId,
        PROG type D020S-PROG,
        DNUM type D020S-DNUM,
      End of t_DynId.

    Data:
      it_DynId type table of t_DynId
        with header line,
      it_D020S type standard table of D020S
        with header line,
      it_D021S type standard table of D021S,
      it_D022S type standard table of D022S,
      it_D023S type standard table of D023S.

    Select *
      into corresponding fields of table it_DynId
      from D020S
      where PROG = MainProgName "w_TADIR-OBJ_NAME
        and TYPE <> 'S'.
    Loop at it_DynId.
      Refresh: it_D020S[], it_D021S[], it_D022S[], it_D023S[].
      Import dynpro it_D020S it_D021S[] it_D022S[] it_D023S[]
        id it_DynId.
      If sy-subrc = 0.
        Append it_D020S.
        Concatenate Path w_TADIR-OBJ_NAME '\SCREEN\' it_DynId-DNUM
          '\D020S.txt' into FileName.
        Perform Write_File using FileName changing it_D020S[].
        Concatenate Path w_TADIR-OBJ_NAME '\SCREEN\' it_DynId-DNUM
          '\D021S.txt' into FileName.
        Perform Write_File using FileName changing it_D021S[].
        Concatenate Path w_TADIR-OBJ_NAME '\SCREEN\' it_DynId-DNUM
          '\D022S.txt' into FileName.
        Perform Write_File using FileName changing it_D022S[].
        Concatenate Path w_TADIR-OBJ_NAME '\SCREEN\' it_DynId-DNUM
          '\D023S.txt' into FileName.
        Perform Write_File using FileName changing it_D023S[].
      EndIf.
    EndLoop.


*    Call function 'DOC_OBJECT_GET'
*      EXPORTING
*        CLASS     = 'RE'
*        NAME      = MainProgName
*      TABLES
*        ITF_LINES = it_Docum
*      EXCEPTIONS
*        others    = 1.
*
*    If sy-subrc = 0.
*      Concatenate Path w_TADIR-OBJ_NAME '\DOC.txt' into FileName.
*      Perform Write_File using FileName changing it_Docum[].
*    EndIf.

    Call function 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        PROGRAM = MainProgName
      IMPORTING
        ADM     = it_ADM
      TABLES
        STA     = it_STA
        FUN     = it_FUN
        MEN     = it_MEN
        MTX     = it_MTX
        ACT     = it_ACT
        BUT     = it_BUT
        PFK     = it_PFK
        SET     = it_SET
        DOC     = it_DOC
        TIT     = it_TIT
        BIV     = it_BIV
      EXCEPTIONS
        others  = 1.
    If sy-subrc = 0.
      If not it_ADM is initial.
        Append it_ADM.
        Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\ADM.txt'
          into FileName.
        Perform Write_File using FileName changing it_ADM[].
      EndIf.
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\STA.txt'
        into FileName.
      Perform Write_File using FileName changing it_STA[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\FUN.txt'
        into FileName.
      Perform Write_File using FileName changing it_FUN[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\MEN.txt'
        into FileName.
      Perform Write_File using FileName changing it_MEN[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\MTX.txt' into
      FileName.
      Perform Write_File using FileName changing it_MTX[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\ACT.txt' into
      FileName.
      Perform Write_File using FileName changing it_ACT[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\BUT.txt' into
      FileName.
      Perform Write_File using FileName changing it_BUT[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\PFK.txt' into
      FileName.
      Perform Write_File using FileName changing it_PFK[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\SET.txt' into
      FileName.
      Perform Write_File using FileName changing it_SET[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\DOC.txt' into
      FileName.
      Perform Write_File using FileName changing it_DOC[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\TIT.txt' into
      FileName.
      Perform Write_File using FileName changing it_TIT[].
      Concatenate Path w_TADIR-OBJ_NAME '\GUI-STATUS\BIV.txt' into
      FileName.
      Perform Write_File using FileName changing it_BIV[].
    EndIf.

    Concatenate Path w_TADIR-OBJ_NAME '\VARIANTS'
      into FileName.
    Perform ExportVariant using w_TADIR FileName.

    subrc = isOk.
  EndIf.
EndForm.                    "ExportProgram


*&---------------------------------------------------------------------*
*&      Form  Get_Limu_VARI
*&---------------------------------------------------------------------*
Form Get_Limu_VARI
  using Obj_Name
  changing Struc_VARI type t_Limu_VARI
           subrc.
  Data:
    w_Report type VARID-REPORT,
    w_Variant type VARID-VARIANT.

  subrc = isError.

  Split Obj_Name at 'CUS&' into   w_Report w_Variant.
  If not w_Report is initial and
     not w_Variant is initial.
    Concatenate 'CUS&' w_Variant into w_Variant.

    Select *
      into table Struc_VARI-VARID[]
      from VARID
      client specified
      where MANDT = '000'
        and REPORT = w_Report
        and VARIANT = w_Variant.
    check sy-subrc = 0.

    Select *
      into table Struc_VARI-VARIT[]
      from VARIT
      client specified
      where MANDT = '000'
        and REPORT = w_Report
        and VARIANT = w_Variant.

    Select *
      into table Struc_VARI-VARIS[]
      from VARIS
      client specified
      where MANDT = '000'
        and REPORT = w_Report
        and VARIANT = w_Variant.

    Call function 'RS_VARIANT_CONTENTS'
      EXPORTING
        REPORT  = w_Report
        VARIANT = w_Variant
      TABLES
        VALUTAB = Struc_VARI-VALUTAB[]
        OBJECTS = Struc_VARI-OBJECTS[]
      EXCEPTIONS
        others  = 99.

    subrc = isOk.
  EndIf.
EndForm.                    "Get_Limu_VARI


*&---------------------------------------------------------------------*
*&      Form  Get_Limu_REPS
*&---------------------------------------------------------------------*
Form Get_Limu_REPS
  using Obj_Name
  changing Struc_REPS type t_Limu_REPS
           subrc.
  Data:
    Obj type SVRS2_VERSIONABLE_OBJECT,
    lt_INDX type table of INDX with header line.

  Obj-OBJTYPE = 'REPS'.
  Obj-OBJNAME = Obj_Name.

  Call function 'SVRS_GET_VERSION_REPOSITORY'
    CHANGING
      OBJ    = Obj
    EXCEPTIONS
      others = 99.
  Move-corresponding:
    Obj-REPS to Struc_REPS.

  If p_EXPERT = 'X'.
    Perform Src_Decomp in program (sy-cprog) if found
      tables
        Struc_REPS-ABAPTEXT
      using
        Obj_Name.
  EndIf.
  subrc = isOk.
EndForm.                    "Get_Limu_REPS


*&---------------------------------------------------------------------*
*&      Form  ExportVariant
*&---------------------------------------------------------------------*
Form ExportVariant
  using w_TADIR type t_TADIR
        PathVariant.

  Data:
    PROG_RANGE   type standard table of RSPRORANGE with header line,
    VARIANT_INFO type standard table of RSVARIINFO with header line,
    VAR_RANGE    type standard table of RSVARRANGE with header line,
    TEXT_RANGE   type standard table of RSTEXRANGE with header line,
    CREATED_BY   type standard table of RSNAMRANGE with header line,
    CHANGED_BY   type standard table of RSNAMRANGE with header line,
    CREADATE     type standard table of RSDATRANGE with header line,
    CHANGEDATE   type standard table of RSDATRANGE with header line,
    VALUTAB      type standard table of RSPARAMS   with header line,
    FileName     type string,
    it_INFO      type standard table of RSVARIINFO with header line,
    it_VANZ      type standard table of VANZ with header line.

  PROG_RANGE-SIGN = 'I'.
  PROG_RANGE-OPTION = 'EQ'.
  PROG_RANGE-LOW = w_TADIR-OBJ_NAME.
  Append PROG_RANGE.

  VAR_RANGE-SIGN = 'I'.
  VAR_RANGE-OPTION = 'CP'.
  VAR_RANGE-LOW = 'CUS&*'.
  Append VAR_RANGE.

  Call function 'RS_VARIANT_INFO'
    TABLES
      PROG_RANGE   = PROG_RANGE
      VAR_RANGE    = VAR_RANGE
      TEXT_RANGE   = TEXT_RANGE
      CREATED_BY   = CREATED_BY
      CHANGED_BY   = CHANGED_BY
      CREADATE     = CREADATE
      CHANGEDATE   = CHANGEDATE
      VARIANT_INFO = VARIANT_INFO.

  Loop at VARIANT_INFO.
    Refresh it_INFO.
    Append VARIANT_INFO to it_INFO.
    Concatenate PathVariant '\' VARIANT_INFO-VARIANT '\RSVARIINFO.txt'
      into FileName.
    Perform Write_File using FileName changing it_INFO[].

    Call function 'RS_VARIANT_CONTENTS'
      EXPORTING
        REPORT  = w_TADIR-OBJ_NAME
        VARIANT = VARIANT_INFO-VARIANT
      TABLES
        VALUTAB = VALUTAB
        OBJECTS = it_VANZ
      EXCEPTIONS
        others  = 99.

    If sy-subrc = 0.
      Concatenate PathVariant '\' VARIANT_INFO-VARIANT '\RSPARAMS.txt'
        into FileName.
      Perform Write_File using FileName changing VALUTAB[].

      Concatenate PathVariant '\' VARIANT_INFO-VARIANT '\VANZ.txt'
        into FileName.
      Perform Write_File using FileName changing it_VANZ[].
    EndIf.
  EndLoop.
EndForm.                    "ExportVariant


*&---------------------------------------------------------------------*
*&      Form  Get_Limu_MSAD
*&---------------------------------------------------------------------*
Form Get_Limu_MSAD
  using Obj_Name
  changing Struc_MSAD
           subrc.
  Field-symbols:
    <table> type table.

  subrc = isError.

  Assign component 'T100A' of structure Struc_MSAD to <table>.
  If sy-subrc = 0.
    Select * into corresponding fields of table <table> from T100A
      where ARBGB = Obj_Name.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

  Assign component 'T100T' of structure Struc_MSAD to <table>.
  If sy-subrc = 0.
    Select * into corresponding fields of table <table> from T100T
      where SPRSL = sy-langu
        and ARBGB = Obj_Name.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

EndForm.                    "Get_Limu_MSAD

*&---------------------------------------------------------------------*
*&      Form  Get_Limu_MESS
*&---------------------------------------------------------------------*
Form Get_Limu_MESS
  using Obj_Name
  changing Struc_MESS
           subrc.

  Data:
    w_ARBGB type T100-ARBGB,
    w_MSGNR type T100-MSGNR,
    Len type i.

  Field-symbols:
    <table> type table.

  subrc = isError.

* CLM_CWB_MESS_GET

  Len = Strlen( Obj_Name ).
  check Len > 3.
  Len = Len - 3.
  w_ARBGB = Obj_Name(Len).
  w_MSGNR = Obj_Name+Len.

  Assign component 'T100' of structure Struc_MESS to <table>.
  If sy-subrc = 0.
    Select * into corresponding fields of table <table> from T100
      where SPRSL = sy-langu
        and ARBGB = w_ARBGB
        and MSGNR = w_MSGNR.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

  Assign component 'T100U' of structure Struc_MESS to <table>.
  If sy-subrc = 0.
    Select * into corresponding fields of table <table> from T100U
      where ARBGB = w_ARBGB
        and MSGNR = w_MSGNR.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

  Assign component 'T100X' of structure Struc_MESS to <table>.
  If sy-subrc = 0.
    Select * into corresponding fields of table <table> from T100X
      where ARBGB = w_ARBGB
        and MSGNR = w_MSGNR.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.

EndForm.                    "Get_Limu_MESS

*&---------------------------------------------------------------------*
*&      Form  Get_Limu_FUGT
*&---------------------------------------------------------------------*
Form Get_Limu_FUGT
  using Obj_Name
  changing struc type t_Limu_FUGT
           subrc.
  Select *
    into table struc-TLIBT[]
    from TLIBT
    where AREA = Obj_Name
      and SPRAS = sy-langu.
  subrc = sy-subrc.
EndForm.                    "Get_Limu_FUGT

*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_FUGR
*&---------------------------------------------------------------------*
*Form Export_R3TR_FUGR
*  using OBJ_NAME
*        Path
*        subrc.
*
*  Data:
*    PathFUGR type string,
*    struc_FUGR type t_R3TR_FUGR.

*  Concatenate Path OBJ_NAME '\' into PathFUGR.
*
*  Select *
*    into table struc_FUGR-TLIBG[]
*    from TLIBG
*    where AREA = OBJ_NAME.
*
*  Select *
*    into table struc_FUGR-TLIBT[]
*    from TLIBT
*    where AREA = OBJ_NAME
*      and SPRAS = sy-langu.
*
*  Perform Download_Struc
*    using struc_FUGR
*           PathFUGR.

*EndForm.                    "Export_R3TR_FUGR

*&---------------------------------------------------------------------*
*&      Form  ExportFUGR
*&---------------------------------------------------------------------*
Form ExportFUGR
  tables it_FUGRReadme
  using w_TADIR type t_TADIR
        Path
        subrc.
  Data:
    ProgName type sy-repid,
    FUNCTION_POOL type TLIBG-AREA,
    it_InclName type standard table of PROGNAME
      with header line,
    it_FuncName type standard table of RS38L_INCL
      with header line,
    it_Source type tt_Source,
    it_Readme type standard table of char255 "string
      with header line,
    FileName type string,
    GENERATED type ENLFDIR-GENERATED,
    GROUP type RS38L-AREA,
    it_TFDIR     type standard table of TFDIR,
    it_TFTIT     type standard table of TFTIT
      with header line,
    it_FUNCT     type standard table of FUNCT,
    it_ENLFDIR   type standard table of ENLFDIR
      with header line,
    it_TRDIR     type standard table of TRDIR,
    it_TRDIRT    type standard table of TRDIRT,
    it_FUPARAREF type standard table of SFUPARAREF,
    it_TLIBT type standard table of TLIBT
      with header line.

  subrc = isError.

  Select *
    into table it_TLIBT
    from TLIBT
    where AREA = w_TADIR-OBJ_NAME
      and SPRAS = sy-langu.
  If not it_TLIBT[] is initial.
    Read table it_TLIBT index 1.
  EndIf.
  Concatenate Path w_TADIR-OBJ_NAME '\TLIBT.txt' into FileName.
  Perform Write_File using FileName changing it_TLIBT[].

  Concatenate 'SAPL' w_TADIR-OBJ_NAME into ProgName.
  Call function 'RS_GET_ALL_INCLUDES'
    EXPORTING
      PROGRAM                = ProgName
      WITH_RESERVED_INCLUDES = 'X'
    TABLES
      INCLUDETAB             = it_InclName
    EXCEPTIONS
      others                 = 1.

*  Select INCLUDE
*    appending table it_InclName
*    from D010INC
*    where MASTER = ProgName.

  Sort it_InclName.
  Delete adjacent duplicates from it_InclName.

  FUNCTION_POOL = w_TADIR-OBJ_NAME.
  Call function 'RS_FUNCTION_POOL_CONTENTS'
    EXPORTING
      FUNCTION_POOL = FUNCTION_POOL
    TABLES
      FUNCTAB       = it_FuncName
    EXCEPTIONS
      others        = 1.

  Loop at it_FuncName.
    Refresh:
      it_TFDIR,
      it_TFTIT,
      it_FUNCT,
      it_ENLFDIR,
      it_TRDIR,
      it_FUPARAREF,
      it_Source.

    Call function 'FUNC_GET_OBJECT'
      EXPORTING
        FUNCNAME   = it_FuncName-FUNCNAME
      TABLES
        PTFDIR     = it_TFDIR
        PTFTIT     = it_TFTIT
        PFUNCT     = it_FUNCT
        PENLFDIR   = it_ENLFDIR
        PTRDIR     = it_TRDIR
        PFUPARAREF = it_FUPARAREF
        UINCL      = it_Source
      EXCEPTIONS
        others     = 1.
    check sy-subrc = 0.

    If not it_ENLFDIR[] is initial.
      Read table it_ENLFDIR index 1.

      "########### ############### #############
*      check it_ENLFDIR-GENERATED is initial.
    EndIf.


    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\ENLFDIR.txt' into FileName.
    Perform Write_File using FileName changing it_ENLFDIR[].

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\FUNCTION.txt' into FileName.
    Perform Write_File using FileName changing it_Source.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\TFDIR.txt' into FileName.
    Perform Write_File using FileName changing it_TFDIR.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\TFTIT.txt' into FileName.
    Perform Write_File using FileName changing it_TFTIT[].

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\FUNCT.txt' into FileName.
    Perform Write_File using FileName changing it_FUNCT.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\TRDIR.txt' into FileName.
    Perform Write_File using FileName changing it_TRDIR.

    Concatenate Path w_TADIR-OBJ_NAME '\FUNCTION\' it_FuncName-FUNCNAME
    '\FUPARAREF.txt' into FileName.
    Perform Write_File using FileName changing it_FUPARAREF.

    If not it_TFTIT[] is initial.
      Read table it_TFTIT index 1.

      Concatenate it_FuncName-FUNCNAME '-' it_TFTIT-STEXT into it_Readme
        separated by space.
    EndIf.

    If not it_Source[] is initial.
      Append it_Readme.
    EndIf.
  EndLoop.

  Data: subrcPROG type sy-subrc.
  Perform ExportPROG
    tables it_Readme
    using w_TADIR
          Path
    changing subrc.
  check subrcPROG = isOk.

  Refresh: it_TRDIR, it_TRDIRT.

  Loop at it_InclName.
    Clear: it_Readme, GENERATED.

    FileName = it_InclName.
    Concatenate FileName '-' 'Include' into it_Readme
      separated by space.

    Clear it_FuncName.
    Read table it_FuncName with key INCLUDE = it_InclName.
    check sy-subrc <> 0.

    Clear GROUP.
    Call function 'FUNCTION_INCLUDE_SPLIT'
      IMPORTING
        GROUP   = GROUP
      CHANGING
        INCLUDE = it_InclName.

    check GROUP = w_TADIR-OBJ_NAME.

    Call function 'RS_CHARACTER_CHECK'
      EXPORTING
        OBJECTNAME = it_InclName
      EXCEPTIONS
        others     = 1.
    check sy-subrc = 0.

    Refresh it_Source.

    Read report it_InclName into it_Source.

    Concatenate FileName '.txt' into FileName.
    Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\' FileName
      into FileName.

    If not it_Source[] is initial.
      Perform Write_File using FileName changing it_Source.

      Select * from TRDIR appending table it_TRDIR
        where NAME = it_InclName.

      Select * from TRDIRT appending table it_TRDIRT
        where NAME = it_InclName and SPRSL = sy-langu.

      Append it_Readme.
    EndIf.
  EndLoop.

  Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\TRDIR.txt' into FileName.
  Perform Write_File using FileName changing it_TRDIR[].

  Concatenate Path w_TADIR-OBJ_NAME '\INCLUDE\TRDIRT.txt' into FileName.
  Perform Write_File using FileName changing it_TRDIRT[].

  If not it_Readme[] is initial.
    Concatenate Path w_TADIR-OBJ_NAME '\' '_Readme.txt' into FileName.
    Perform Write_File using FileName changing it_Readme[].
  EndIf.

  Concatenate w_TADIR-OBJ_NAME '-' it_TLIBT-AREAT into it_FUGRReadme
    separated by space.
  Append it_FUGRReadme.

  subrc = isOk.
EndForm.                    "ExportFUGR

Types:
  Begin of t_Resolved_Obj,
    PGMID    type E071-PGMID,
    OBJECT   type E071-OBJECT,
    OBJ_NAME type E071-OBJ_NAME,
  End of t_Resolved_Obj,
  tt_Resolved_Obj type standard table of t_Resolved_Obj,
  tt_VRSO type standard table of VRSO.

*&---------------------------------------------------------------------*
*&      Form  Resolve_R3TR_Obj
*&---------------------------------------------------------------------*
Form Resolve_R3TR_Obj
  using E071_OBJ type E071
  changing it_Res_Obj type tt_Resolved_Obj
           subrc.
  Data:
    ObjExt type t_Objects_Ext,
    w_Res_Obj like line of it_Res_Obj,
    it_OBJ type tt_VRSO,
    w_OBJ like line of it_OBJ,
    it_E071_LIMU type standard table of E071
      with header line.


  Refresh it_OBJ.

  Call function 'SUMO_RESOLVE_E071_OBJ'
    EXPORTING
      E071_OBJ = E071_OBJ
    TABLES
      OBJ_TAB  = it_E071_LIMU[]
    EXCEPTIONS
      others   = 98.

  Loop at it_E071_LIMU.
    Move-corresponding: it_E071_LIMU to w_Res_Obj.
    Append w_Res_Obj to it_Res_Obj.
  EndLoop.

  Call function 'SVRS_RESOLVE_E071_OBJ'
    EXPORTING
      E071_OBJ = E071_OBJ
    TABLES
      OBJ_TAB  = it_OBJ
    EXCEPTIONS
      others   = 98.
  Loop at it_OBJ into w_OBJ.
    Clear w_Res_Obj.
    w_Res_Obj-PGMID  = 'LIMU'.
    w_Res_Obj-OBJECT = w_OBJ-OBJTYPE.
    w_Res_Obj-OBJ_NAME = w_OBJ-OBJNAME.
    Append w_Res_Obj to it_Res_Obj.
  EndLoop.

  Data: ObjName type string.
  Case E071_OBJ-OBJECT.
    when 'FUGR'.
      Concatenate 'L' E071_OBJ-OBJ_NAME 'T00' into ObjName.
      w_Res_Obj-PGMID = 'LIMU'.
      w_Res_Obj-OBJECT = 'REPS'.
      w_Res_Obj-OBJ_NAME = ObjName.
      Append w_Res_Obj to it_Res_Obj.
      w_Res_Obj-PGMID = 'LIMU'.
      w_Res_Obj-OBJECT = 'REPT'.
      w_Res_Obj-OBJ_NAME = ObjName.
      Append w_Res_Obj to it_Res_Obj.
    when 'MSAG'.
*      w_OBJ-OBJTYPE = 'MSAD'.
*      w_OBJ-OBJNAME = E071_OBJ-OBJECT.
*      Append w_OBJ to it_OBJ.

*(28.02.2014
    when 'TDAT'.
      Loop at it_E071K
        where PGMID = E071_OBJ-PGMID
          and OBJECT  = 'TABU'
          and MASTERTYPE = E071_OBJ-OBJECT
          and MASTERNAME = E071_OBJ-OBJ_NAME.

        w_Res_Obj-PGMID  = it_E071K-PGMID.
        w_Res_Obj-OBJECT = it_E071K-OBJECT.
        w_Res_Obj-OBJ_NAME = it_E071K-OBJNAME.
        Collect w_Res_Obj into it_Res_Obj.
      EndLoop.
*)28.02.2014

    when others.
  EndCase.

  Field-symbols: <comp>.
  Assign component E071_OBJ-OBJECT of structure ObjExt to <comp>.
  If sy-subrc = 0.
    Move-corresponding: E071_OBJ to w_Res_Obj.
    Append w_Res_Obj to it_Res_Obj.
  EndIf.

  Sort it_Res_Obj by OBJ_NAME OBJECT PGMID descending.
  Delete adjacent duplicates from it_Res_Obj
    comparing OBJ_NAME OBJECT .
  If it_Res_Obj[] is initial.
    subrc = isError.
  EndIf.
EndForm.                    "Resolve_R3TR_Obj

*&---------------------------------------------------------------------*
*&      Form  ExportR3TR
*&---------------------------------------------------------------------*
Form Export_R3TR
  tables it_FUGRReadme
  using p_TADIR type t_TADIR
        Path
        subrc.

  Data:
    E071_OBJ type E071,
    it_Res_Obj type tt_Resolved_Obj "VRSO
      with header line,
    ObjR3TR type t_R3TR_Types,
    it_NameTable type standard table of string
      with header line,
    FileName type string,
    PathR3TR type string,
    PathLIMU type string,
    subrc_r3tr_exp type sy-subrc,
    subrc_r3tr_get type sy-subrc,
    subrc_r3tr_res type sy-subrc,
    subrc_limu_exp type sy-subrc,
    subrc_limu_all type sy-subrc,
    FormName type string,
    ProgName type string.

  Field-symbols:
    <struc_r3tr>,
    <table>.

  subrc = isNothing.
  subrc_limu_all = isNothing.
*  ProgName = sy-cprog.
*
*  Concatenate Path p_TADIR-OBJECT '.R3TR\' into PathR3TR.
*
*  Concatenate 'EXPORT_R3TR_' p_TADIR-OBJECT into FormName.
**  Try.
*  subrc_r3tr_exp = isNotFound.
*  Perform (FormName) in program (ProgName) if found
*    using
*      p_TADIR-Obj_Name
*      PathR3TR
*      subrc_r3tr_exp.
*
*  Assign component p_TADIR-OBJECT of structure ObjR3TR to <struc_r3tr>.
*  If sy-subrc = 0 and <struc_r3tr> is assigned.
*    Concatenate Path p_TADIR-OBJECT '.R3TR\' p_TADIR-OBJ_NAME '\'
*      into PathR3TR.
*
*    Concatenate 'GET_R3TR_' p_TADIR-OBJECT into FormName.
*    subrc_r3tr_get = isNotFound.
*    Perform (FormName) in program (ProgName) if found
*      using p_TADIR-Obj_Name
*      changing <struc_r3tr>
*               subrc_r3tr_get.
*    If subrc_r3tr_get = isOk.
*      Perform Download_Struc
*        using <struc_r3tr>
*              PathR3TR.
*    EndIf.
*  EndIf.

  Move-corresponding: p_TADIR to E071_OBJ.

  Perform Resolve_R3TR_Obj
    using E071_OBJ
    changing it_Res_Obj[]
             subrc_r3tr_res.


*  If subrc_r3tr_res = 0.
  Loop at it_Res_Obj
    where PGMID = 'R3TR'.

*    Concatenate Path p_TADIR-OBJECT '.R3TR\'
*                p_TADIR-OBJ_NAME '\'
*      into PathR3TR.
    FileName = util=>FileName_Escape( p_TADIR-OBJ_NAME ).
    Concatenate Path p_TADIR-OBJECT '.R3TR\'
                FileName '\'
      into PathR3TR.

*(28.02.2014
    If p_TADIR-OBJ_NAME <> it_Res_Obj-OBJ_NAME.
*      Concatenate PathR3TR it_Res_Obj-OBJECT '.R3TR\'
*                  it_Res_Obj-OBJ_NAME '\'
*        into PathR3TR.
      FileName = util=>FileName_Escape( it_Res_Obj-OBJ_NAME ).
      Concatenate PathR3TR it_Res_Obj-OBJECT '.R3TR\'
                  FileName '\'
        into PathR3TR.
    EndIf.
*)28.02.2014

    Perform Export_Object
      using it_Res_Obj-PGMID
            it_Res_Obj-OBJECT
            it_Res_Obj-OBJ_NAME
            PathR3TR
      changing subrc_r3tr_exp.
    If subrc_r3tr_exp = isOk.
      subrc_limu_all = subrc_limu_exp.
    EndIf.
  EndLoop.

  Loop at it_Res_Obj
    where PGMID = 'LIMU'.

    FileName = util=>FileName_Escape( p_TADIR-OBJ_NAME ).
    Concatenate Path p_TADIR-OBJECT '.R3TR\' FileName '\'
      into PathLIMU.
    FileName = util=>FileName_Escape( it_Res_Obj-OBJ_NAME ).
    Concatenate PathLIMU
      it_Res_Obj-OBJECT '.LIMU\' FileName '\'
      into PathLIMU.

    Perform Export_Object
      using it_Res_Obj-PGMID
            it_Res_Obj-OBJECT
            it_Res_Obj-OBJ_NAME
            PathLIMU
      changing subrc_limu_exp.
    If subrc_limu_exp = isOk.
      subrc_limu_all = subrc_limu_exp.
    EndIf.
  EndLoop.
*  EndIf.

  If "subrc_r3tr_exp <> isError and
     "subrc_r3tr_get <> isError and
     subrc_limu_all = isOk.

    subrc = isOk.

  EndIf.

EndForm.                                                    "ExportR3TR

*&---------------------------------------------------------------------*
*&      Form  Export_Object
*&---------------------------------------------------------------------*
Form Export_Object
  using Obj_PGMID
        Obj_Type "type TROBJTYPE
        Obj_Name "type TROBJ_NAME
        Path
  changing subrc.

  Data:
    Obj type SVRS2_VERSIONABLE_OBJECT,
    ObjExt type t_Objects_Ext, "t_LIMU_types,
    it_NameTable type standard table of string
      with header line,
    PathStruc type string,
    FileName type string,
    subrcRps type sy-subrc,
    subrcFrm type sy-subrc.

  Field-symbols:
    <struc>,
    <table> type table.

  subrc = isNothing.
  subrcFrm = isNothing.

*  Concatenate Path Obj_Type '.LIMU\' Obj_Name '\'
*    into PathStruc.
  PathStruc = Path.

  Obj-OBJTYPE = Obj_Type.
  Obj-OBJNAME = Obj_Name.

  Call function 'SVRS_GET_VERSION_REPOSITORY'
    CHANGING
      OBJ    = Obj
    EXCEPTIONS
      others = 99.
  If sy-subrc = 0.
    Assign component Obj-OBJTYPE of structure Obj to <struc>.
    If sy-subrc = 0.
      subrcRps = isOk.
*      Perform Download_Struc
*        using <struc>
*        PathStruc.
*      subrc = isOk.
    EndIf.
  Else.
    Data: FormName type string.
    Assign component Obj_Type of structure ObjExt to <struc>.
    If sy-subrc = 0.
      Concatenate 'GET_' Obj_PGMID '_' Obj_Type into FormName.
*      ObjExt-OBJTYPE = Obj_Type.
*      ObjExt-OBJNAME = Obj_Name.
      Perform (FormName) in program (sy-cprog) if found
        using Obj_Name
        changing <struc>
                 subrcFrm.
*      If subrcFrm = isOk.
*        Perform Download_Struc
*          using <struc>
*          PathStruc.
*        subrc = isOk.
*      EndIf.
    EndIf.
  EndIf.

  If subrcRps = isOk or subrcFrm = isOk.
    If sy-cprog = Obj_Name and "ZWWW_MIGRATE ########## # 4.6
       Obj_Type = 'REPS'.


      Assign component 'ABAPTEXT' of structure <struc> to <table>.
      Perform TrimDataPROG
        using '46C'
              72
        changing <table>.
    EndIf.

    Perform Download_Struc
      using <struc>
      PathStruc.
    subrc = isOk.
  EndIf.
EndForm.                    "Export_Object

*&---------------------------------------------------------------------*
*&      Form  Download_Struc
*&---------------------------------------------------------------------*
Form Download_Struc
  using Struc
        Path.
  Data:
    it_NameTable type standard table of string
      with header line,
    FileName type string,
    w_FLPARAM type FileParameters,
    NameBinTab type string.
  Field-symbols:
    <table> type table,
    <FLPARAM> type table.

  Perform Check_Binary
    using Struc
    changing NameBinTab.

  Perform Get_Struc tables it_NameTable using Struc.
  Loop at it_NameTable.
    check it_NameTable <> 'FLPARAM'.

    Assign component it_NameTable of structure Struc
      to <table>.
    If sy-subrc = 0.
      If it_NameTable <> NameBinTab.
        Concatenate Path it_NameTable '.txt'
          into FileName.
        Perform Write_File using FileName changing <table>.
      Else.
        Assign component 'FLPARAM' of structure Struc
          to <FLPARAM>.
        If sy-subrc = 0.
          Read table <FLPARAM> into w_FLPARAM index 1.
          Concatenate Path w_FLPARAM-Name
            into FileName.

          Perform Write_BIN_File
            using FileName
                  w_FLPARAM-Length
            changing <table>.

        EndIf.
      EndIf.
    EndIf.
  EndLoop.
EndForm.                    "Download_Struc
*&---------------------------------------------------------------------*
*&      Form  ImportR3TR
*&---------------------------------------------------------------------*
Form ImportR3TR
  using p_TADIR type t_TADIR
        Path
        subrc.
  Data:
    Dir type string,
    Ext type string,
    it_LIMUName type standard table of char255
      with header line,
    it_NameTable type standard table of char255
      with header line,
    it_ObjName type standard table of char255
      with header line,
    Cnt type i,
    Obj type SVRS2_VERSIONABLE_OBJECT,
    Vrs type VRSINFO,
    w_infoline1a type vrsinfolna,
    w_infoline1b type vrsinfolnb,
    LongOBJNAME type vrsd-objname.


  Field-symbols:
    <struc>,
    <table>.


  Concatenate Path p_TADIR-OBJ_NAME '\' into Dir.
*  Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*    EXPORTING
*      DIRECTORY        = Dir
*      FILTER           = '*.LIMU'
*      DIRECTORIES_ONLY = 'X'
*    CHANGING
*      FILE_TABLE       = it_LIMUName[]
*      COUNT            = Cnt
*    EXCEPTIONS
*      others           = 1.
  Perform Read_Catalog
    using Dir '*.LIMU' 'D'
    changing it_LIMUName[].

  Loop at it_LIMUName.
    Split it_LIMUName at '.' into it_LIMUName Ext.

    Assign component it_LIMUName of structure Obj to <struc>.
    If sy-subrc = 0.
      Concatenate Path p_TADIR-OBJ_NAME '\' it_LIMUName '\' into Dir.

      Refresh it_ObjName.

*      Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*        EXPORTING
*          DIRECTORY        = Dir
*          FILTER           = '*.*'
*          DIRECTORIES_ONLY = 'X'
*        CHANGING
*          FILE_TABLE       = it_ObjName[]
*          COUNT            = Cnt
*        EXCEPTIONS
*          others           = 1.
      Perform Read_Catalog
        using Dir '*.*' 'D'
        changing it_ObjName[].

      Loop at it_ObjName.
        Concatenate Path p_TADIR-OBJ_NAME '\' it_LIMUName '\'
                    it_ObjName '\' into Dir.
        Refresh it_NameTable.
*        Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*          EXPORTING
*            DIRECTORY  = Dir
*            FILTER     = '*.*'
*            FILES_ONLY = 'X'
*          CHANGING
*            FILE_TABLE = it_NameTable[]
*            COUNT      = Cnt
*          EXCEPTIONS
*            others     = 1.
        Perform Read_Catalog
          using Dir '*.*' 'F'
          changing it_NameTable[].


        Clear: Obj, Vrs.
        Obj-OBJTYPE = it_LIMUName.
        Obj-OBJNAME = it_ObjName.
        Vrs-VERSMODE = 'I'.

        Loop at it_NameTable.
          Translate it_NameTable to upper case.
          Split it_NameTable at '.' into it_NameTable Ext.
          Assign component it_NameTable of structure <struc>
            to <table>.
          If sy-subrc = 0.
            Concatenate Path p_TADIR-OBJ_NAME '\' it_LIMUName '\'
              it_ObjName '\' it_NameTable '.txt'
              into FileName.
            Perform Read_File using FileName changing <table>.
          EndIf.

        EndLoop.

        Call function 'SVRS_WRITE_FULL_VERSION_TO_DB'
          EXPORTING
            OBJECT         = Obj
            VINFO          = Vrs
            PI_CHECK_EMPTY = ''
          EXCEPTIONS
            others         = 99.

        Select single *
          from VERSOBJ
          where OBJECT = Obj-OBJTYPE
            and VERSNO = 99999
            and ACTFLAG = 'A'.
        If sy-subrc = 0 and not VERSOBJ-REP_WRITE is initial.

          Call function 'SVRS_SHORT2LONG_NAME'
            EXPORTING
              objtype       = Obj-OBJTYPE
              objname_short = Obj-OBJNAME
            IMPORTING
              objname_long  = LongOBJNAME.

          w_infoline1a-OBJNAME = Obj-OBJNAME.
          w_infoline1b-DATUM = sy-datum.
          w_infoline1b-AUTHOR = sy-uname.

          SUBMIT (VERSOBJ-REP_WRITE) AND RETURN
            WITH objtype = Obj-OBJTYPE
            WITH objname = LongOBJNAME
            WITH versno  = 0
            WITH infolna = w_infoline1a
            WITH infolnb = w_infoline1b.

        EndIf.
      EndLoop.
    EndIf.
  EndLoop.

EndForm.                                                    "ImportR3TR

*&---------------------------------------------------------------------*
*&      Form  Check_Binary
*&---------------------------------------------------------------------*
Form Check_Binary
  using Struc
  changing Name.

  Field-symbols:
    <tab>.
  Assign component 'W3MIME' of structure Struc to <tab>.
  If sy-subrc = 0 and <tab> is assigned.
    Name = 'W3MIME'.
  Else.
    Name = ''.
  EndIf.
EndForm.                    "Check_Binary

*&---------------------------------------------------------------------*
*&      Form  Import_R3TR
*&---------------------------------------------------------------------*
Form Import_R3TR
  using p_TADIR type t_TADIR
        Path
        subrc.
  Data:
    Dir type string,
    DirR3TR type string,
    DirLIMU type string,
    DirOBJ type string,
    Ext type string,
    it_R3TR_FlName type standard table of char255
      with header line,
    it_R3TRName type standard table of char255
      with header line,
    it_LIMUName type standard table of char255
      with header line,
    it_NameTable type standard table of char255
      with header line,
    it_ObjName type standard table of char255
      with header line,
    Cnt type i,
    ObjVrs type SVRS2_VERSIONABLE_OBJECT,
    ObjExt type t_Objects_Ext,"t_R3TR_Types,
    NameTab type char50,
    Vrs type VRSINFO,
    w_infoline1a type vrsinfolna,
    w_infoline1b type vrsinfolnb,
    LongOBJNAME type vrsd-objname,
    LIMU_subrc type sy-subrc,
    ALL_subrc  type sy-subrc,
    R3TR_subrc type sy-subrc,
    FormName type string,
    it_String type tt_String.


  Field-symbols:
    <struc_r3tr>,
    <table> type table.

  check p_TADIR-OBJECT <> 'DEVC'.

  Perform SetClassForObject using p_TADIR.
  If sy-subrc = 0.
    Perform AppendToQuery using p_TADIR.
  EndIf.

*  Concatenate Path p_TADIR-OBJECT '.R3TR\' p_TADIR-OBJ_NAME '\'
*    into Dir.
  Concatenate Path p_TADIR-OBJECT '.R3TR\' p_TADIR-Object_Path '\'
    into Dir.
* R3TR
  R3TR_subrc = isNothing.
  Assign component p_TADIR-OBJECT of structure ObjVrs to <struc_r3tr>.
  If sy-subrc <> 0.
    Assign component p_TADIR-OBJECT of structure ObjExt to <struc_r3tr>.
  EndIf.
  If sy-subrc = 0 and <struc_r3tr> is assigned.
    Perform Read_Catalog
      using Dir '*.*' 'F'
      changing it_R3TR_FlName[].

    Loop at it_R3TR_FlName.
      Translate it_R3TR_FlName to upper case.
      Concatenate Dir it_R3TR_FlName
        into FileName.
      Split it_R3TR_FlName at '.' into NameTab Ext. "it_R3TR_FlName Ext.
      Assign component NameTab of structure <struc_r3tr>
        to <table>.
      If sy-subrc = 0 and Ext = 'TXT'.
*        Concatenate Dir it_R3TR_FlName '.txt'
*          into FileName.
        If NameTab = 'ABAPTEXT'. "######## ## ######## #### ABAPTEXT
          Perform Read_File using FileName changing it_String[].
          If not it_String[] is initial.
            Delete it_String[] index 1.
          EndIf.
          Perform TrimDataPROG
            using sy-saprl
                  Len_ABAP
            changing it_String[].
          <table> = it_String[].
        Else.
          Perform Read_File using FileName changing <table>.
        EndIf.
      Else.
        If NameTab = p_TADIR-OBJ_NAME.
          Perform Check_Binary
            using <struc_r3tr>
            changing NameTab.
          If not NameTab is initial.
            Assign component NameTab of structure <struc_r3tr>
              to <table>.
            Data: Len type i.


*            Call function 'GUI_UPLOAD'
*              EXPORTING
*                FILENAME   = FileName
*                FILETYPE   = 'BIN'
*              IMPORTING
*                FILELENGTH = Len
*              TABLES
*                DATA_TAB   = <table>
*              EXCEPTIONS
*                OTHERS     = 1.
            Perform Upload_Bin_File
              using
                FileName
              changing
                Len
                <table>.
            If sy-subrc = 0.
              Data: FlParam type FileParameters.
              Assign component 'FLPARAM' of structure <struc_r3tr>
                to <table>.
              If sy-subrc = 0.
                FlParam-Name = it_R3TR_FlName.
                FlParam-Length = Len.
                Append FlParam to <table>.
              EndIf.
            EndIf.
          EndIf.
        EndIf.
      EndIf.
    EndLoop.

    Concatenate 'IMPORT_R3TR_' p_TADIR-OBJECT into FormName.

*    Try.
    R3TR_subrc = isNothing.
    Perform (FormName) in program (sy-cprog) if found
      using p_TADIR
            <struc_r3tr>
            R3TR_subrc.
*        R3TR_subrc = isOk.
*      Catch CX_SY_DYN_CALL_ILLEGAL_FORM.
*        R3TR_subrc = isNotFound.
*    EndTry.

  EndIf.

  ALL_subrc = isNothing.

*(28.02.2014
* R3TR
  Data:
    TadirR3TR type t_Tadir,
    sub_R3TR_subrc type sy-subrc.

  Perform Read_Catalog
    using Dir '*.R3TR' 'D'
    changing it_R3TRName[].
  Loop at it_R3TRName.
    Concatenate Dir it_R3TRName '\' into DirR3TR.
    Split it_R3TRName at '.' into it_R3TRName Ext.

    Refresh it_ObjName.

    Perform Read_Catalog
      using DirR3TR '*.*' 'D'
      changing it_ObjName[].
    Loop at it_ObjName.
      sub_R3TR_subrc = isNothing.

*      Concatenate DirR3TR it_ObjName '\' into DirOBJ.

      TadirR3TR = p_Tadir.
      TadirR3TR-OBJECT   = it_R3TRName.
      TadirR3TR-OBJ_NAME = it_ObjName.

      Perform Import_R3TR
        using TadirR3TR
              Dir
              sub_R3TR_subrc.
      Case sub_R3TR_subrc.
        when isError.
          ALL_subrc = isError.
        when isOk.
          If ALL_subrc = isNothing.
            ALL_subrc = isOk.
          EndIf.
      EndCase.
    EndLoop.
  EndLoop.
*)28.02.2014

* LIMU
  Perform Read_Catalog
    using Dir '*.LIMU' 'D'
    changing it_LIMUName[].

  Perform Sort_LIMU tables it_LIMUName.

  Loop at it_LIMUName.

    Concatenate Dir it_LIMUName '\' into DirLIMU.

    Split it_LIMUName at '.' into it_LIMUName Ext.

    Refresh it_ObjName.

    Perform Read_Catalog
      using DirLIMU '*.*' 'D'
      changing it_ObjName[].

    Loop at it_ObjName.
      LIMU_subrc = isNothing.

      Concatenate DirLIMU it_ObjName '\' into DirOBJ.

      Perform Import_LIMU
        using
*              p_Tadir
              it_LIMUName
              it_ObjName
              DirLIMU
        changing
          LIMU_subrc.
      Case LIMU_subrc.
        when isError.
          ALL_subrc = isError.
        when isOk.
          If ALL_subrc = isNothing.
            ALL_subrc = isOk.
          EndIf.
      EndCase.
    EndLoop.
  EndLoop.

  If R3TR_subrc = isError or ALL_subrc = isError.
    subrc = isError.
  ElseIf R3TR_subrc = isOk or ALL_subrc = isOk.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_R3TR

*&---------------------------------------------------------------------*
*&      Form  Sort_LIMU
*&---------------------------------------------------------------------*
Form Sort_LIMU
  tables it_LIMU.

  Data:
    Obj type TADIR-OBJECT,
    it_Buff type standard table of t_ObjectSort
      with header line.

  Loop at it_LIMU.
    it_Buff-OBJECT = it_LIMU.
    Obj = it_Buff-OBJECT.
    Read table it_LIMU_Sort
      with key OBJECT = Obj.
    If sy-subrc = 0.
      it_Buff-SortKey = it_LIMU_Sort-SortKey.
    Else.
      Clear it_Buff-SortKey with '9'.
    EndIf.
    Append it_Buff.
  EndLoop.

  Sort it_Buff by SortKey.
  Refresh it_LIMU.

  Loop at it_Buff.
    it_LIMU = it_Buff-OBJECT.
    Append it_LIMU.
  EndLoop.
EndForm.                    "Sort_LIMU

*&---------------------------------------------------------------------*
*&      Form  Save_TRDIR
*&---------------------------------------------------------------------*
Form Save_TRDIR
  tables
    it_TRDIR structure TRDIR.

  Data:
    w_PROGDIR type PROGDIR.

  check not it_TRDIR[] is initial.

  Read table it_TRDIR index 1.
  If sy-subrc = 0 and
     not it_TRDIR-NAME is initial.

    Modify TRDIR from it_TRDIR.

    Move-corresponding: it_TRDIR to w_PROGDIR.
    Call function 'UPDATE_PROGDIR'
      EXPORTING
        I_PROGDIR  = w_PROGDIR
        I_PROGNAME = w_PROGDIR-NAME
        I_STATE    = 'A'
      EXCEPTIONS
        others     = 1.

  EndIf.
EndForm.                    "Save_TRDIR

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU
*&---------------------------------------------------------------------*
Form Import_LIMU
  using
*        p_Tadir type t_Tadir
        Obj_Type
        Obj_Name "### Object_Path
        Path
  changing
    LIMU_subrc.

  Data:
    Dir type string,
    it_ObjName type standard table of char255
      with header line,
    Cnt type i,
    it_NameTable type standard table of char255
      with header line,
    Obj type SVRS2_VERSIONABLE_OBJECT,
    ObjExt type t_LIMU_types,
    Vrs type VRSINFO,
    Ext type string,
    FormName type string,
    ProgName type string.


  Field-symbols:
    <struc>,
    <table>.

  ProgName = sy-cprog.

  Assign component Obj_Type of structure Obj to <struc>.
  If sy-subrc <> 0.
    Assign component Obj_Type of structure ObjExt to <struc>.
  EndIf.

  If sy-subrc = 0 and <struc> is assigned.

    Concatenate Path OBJ_NAME '\' into Dir.

    Concatenate Path OBJ_NAME '\'
                into Dir.
    Refresh it_NameTable.

    Perform Read_Catalog
      using Dir '*.*' 'F'
      changing it_NameTable[].

    Clear: Obj, Vrs.
    Obj-OBJTYPE = Obj_Type.
    Obj-OBJNAME = util=>FileName_unEscape( Obj_Name ).
    Vrs-VERSMODE = 'I'.

    Loop at it_NameTable.
      Translate it_NameTable to upper case.
      Split it_NameTable at '.' into it_NameTable Ext.
      Assign component it_NameTable of structure <struc>
        to <table>.
      If sy-subrc = 0.
        Concatenate Path OBJ_NAME '\'
*          it_ObjName '\'
          it_NameTable '.txt'
          into FileName.

        If it_NameTable = 'ABAPTEXT' or
           it_NameTable = 'REPS'.
          "######## ## ######## #### ABAPTEXT

          Data: it_String type tt_String.
          Perform Read_File using FileName changing it_String[].
          If not it_String[] is initial.
            Delete it_String[] index 1.
          EndIf.
          Perform TrimDataPROG
            using sy-saprl
                  Len_ABAP
            changing it_String[].
          <table> = it_String[].
        Else.
          Perform Read_File using FileName changing <table>.
        EndIf.
      EndIf.

    EndLoop.

    Concatenate 'IMPORT_LIMU_' Obj_Type into FormName.

*    Try.
    LIMU_subrc = isNothing.
    Perform (FormName) in program (ProgName) if found
      using
        Obj-OBJNAME "Obj_Name
        <struc>
        LIMU_subrc.
*        LIMU_subrc = isOk.
*      Catch CX_SY_DYN_CALL_ILLEGAL_FORM.
*        LIMU_subrc = isNotFound.
*    EndTry.
  EndIf.
EndForm.                    "Import_LIMU



*&---------------------------------------------------------------------*
*&      Form  Import_Limu_REPT
*&---------------------------------------------------------------------*
Form Import_Limu_REPT
  using
    Obj_Name
    Struc_REPT type SVRS2_REPT
    subrc.

  Data:
    w_REPT_TEXTPOOL like line of Struc_REPT-TEXTPOOL,
    it_TextPool type standard table of TEXTPOOL
      with header line.

  subrc = isError.

  Perform Save_TRDIR tables Struc_REPT-TRDIR[].

  Loop at it_LANGU.
    Refresh it_TextPool.

    Loop at Struc_REPT-TEXTPOOL[] into w_REPT_TEXTPOOL
      where LANG = it_LANGU.
      Move-corresponding: w_REPT_TEXTPOOL to it_TextPool.
      Append it_TextPool.
    EndLoop.

    If not it_TextPool[] is initial.

      Insert textpool OBJ_NAME from it_TextPool language it_LANGU.

    EndIf.
  EndLoop.
*  If sy-subrc = 0.
  subrc = isOk.
*  EndIf.

EndFORM.                    "Import_Limu_REPT

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_REPS
*&---------------------------------------------------------------------*
Form Import_Limu_REPS
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    w_TRDIR type TRDIR,
    RepsType type TRDIR-SUBC,
    Len type i,
    Suffix1(5),
    w_ABAPTEXT type line of SVRS2_REPS-ABAPTEXT,
    it_Source type tt_Source,
    w_Source like line of it_Source.
  Field-symbols:
    <REPS> type SVRS2_REPS.

  subrc = isError.

  Assign Struc_LIMU to <REPS>.

  If "it_SourceFile[] <> it_SourceOld[] and
     not <REPS>-ABAPTEXT[] is initial.

    Loop at <REPS>-ABAPTEXT into w_ABAPTEXT.
      w_Source = w_ABAPTEXT.
      Append w_Source to it_Source.
    EndLoop.

    Perform TrimDataPROG
      using sy-saprl
            Len_ABAP
      changing it_Source.

    w_TRDIR-SUBC = '1'.
    If not <REPS>-TRDIR[] is initial.
      Read table <REPS>-TRDIR[] into w_TRDIR index 1.
    EndIf.

    RepsType = w_TRDIR-SUBC.
    Case RepsType.
      when 'T'.
        RepsType = 'I'.
    EndCase.

    Len = Strlen( OBJ_NAME ).
    If Len > 30.
      Suffix1 = OBJ_NAME+30.
      Insert report OBJ_NAME from it_Source[]
        extension type Suffix1
        program type RepsType."#EC V#600
      .
    Else.
      Insert report OBJ_NAME from it_Source[]
        program type RepsType."#EC V#600
      .
    EndIf.

    If sy-subrc = 0.
      Perform Save_TRDIR tables <REPS>-TRDIR.
      subrc = isOk.
*      Delete report OBJ_NAME state 'I'.
    EndIf.
  EndIf.
EndForm.                    "Import_Limu_REPS

**&---------------------------------------------------------------------
**&      Form  ExportCLAS
**&---------------------------------------------------------------------
*Form ExportCLAS
*  tables it_FUGRReadme
*  using p_TADIR type t_TADIR
*        Path
*        subrc.
*
*  Data:
*    CIFKEY type SEOCLSKEY,
*    MTDKEY type SEOCPDKEY,
*    it_Methods type SEOO_METHODS_R
*      with header line,
*    InclName type PROGRAMM,
*    w_TADIR type t_TADIR,
*    PathCLAS type string,
*    PathMETH type string,
*    it_Attr type SEOO_ATTRIBUTES_R,
**    it_AttrSrc type SEO_TYPE_SOURCES,
*    it_Events type SEOO_EVENTS_R,
*    it_Types type  SEOO_TYPES_R
*      with header line.
*
*
*  Perform ExportPROG
*    tables it_FUGRReadme
*    using p_TADIR
*        Path
*        subrc.
*
*  w_TADIR = p_TADIR.
*  w_TADIR-OBJ_NAME = InclName.
**  w_TADIR-OBJECT = 'PROG'.
*
*  Concatenate Path p_TADIR-OBJ_NAME '\' into PathCLAS.
*
*
*  CIFKEY-CLSNAME = p_TADIR-OBJ_NAME.
*  Call function 'SEO_METHOD_READ_ALL'
*    EXPORTING
*      CIFKEY         = CIFKEY
*      MODIF_LANGUAGE = sy-langu
*    IMPORTING
*      METHODS        = it_Methods[]
*    EXCEPTIONS
*      others         = 99.
*
*  Loop at it_Methods.
*    MTDKEY-CLSNAME = p_TADIR-OBJ_NAME.
*    MTDKEY-CPDNAME = it_Methods-CMPNAME.
*
*    Call function 'SEO_METHOD_GET_INCLUDE_BY_NAME'
*      EXPORTING
*        MTDKEY   = MTDKEY
*      IMPORTING
*        PROGNAME = InclName.
*
*    Concatenate PathCLAS 'METHOD\' it_Methods-CMPNAME '\' into PathMETH
*
*    Perform ExportInclude
*      tables it_FUGRReadme
*      using InclName
*            'METHOD'
*            PathMETH
*            subrc.
*  EndLoop.
*
*  Call function 'SEO_ATTRIBUTE_READ_ALL'
*    EXPORTING
*      CIFKEY            = CIFKEY
*      MODIF_LANGUAGE    = sy-langu
*    IMPORTING
*      ATTRIBUTES        = it_Attr
**      ATTRIBUTE_SOURCES = it_AttrSrc
*    EXCEPTIONS
*      others            = 99.
*
*  Concatenate PathClas 'ATTRIBUTES.txt'
*    into FileName.
*  Perform Write_File using FileName changing it_Attr.
*
*  Call function 'SEO_EVENT_READ_ALL'
*    EXPORTING
*      CIFKEY         = CIFKEY
*      MODIF_LANGUAGE = sy-langu
*    IMPORTING
*      EVENTS         = it_EVENTS
*    EXCEPTIONS
*      others         = 99.
*
*  Concatenate PathClas 'EVENTS.txt'
*    into FileName.
*  Perform Write_File using FileName changing it_EVENTS.
*
*
*  Call function 'SEO_TYPE_READ_ALL'
*    EXPORTING
*      CIFKEY         = CIFKEY
*      MODIF_LANGUAGE = sy-langu
*    IMPORTING
*      TYPES          = it_Types[]
*    EXCEPTIONS
*      others         = 99.
*
*  Field-symbols:
*    <types> like line of it_Types,
*    <TYPESRC>.
*
*  Loop at it_Types assigning <types>.
*    Assign component 'TYPESRC' of structure <types> to <TYPESRC>.
*    If sy-subrc = 0.
*      check not <TYPESRC> is initial.
*    EndIf.
*
*    Concatenate PathClas 'TYPESRC\' <types>-CMPNAME '.txt'
*      into FileName.
*    Perform Write_File using FileName changing <TYPESRC>.
*
*    Clear <TYPESRC>.
*  EndLoop.
*
*  Concatenate PathClas 'TYPES.txt'
*    into FileName.
*  Perform Write_File using FileName changing it_Types[].
*
*EndForm.                    "ExportCLAS

*&---------------------------------------------------------------------*
*&      Form  Get_R3TR_W3MI
*&---------------------------------------------------------------------*
Form Get_R3TR_W3MI
  using Obj_Name
  changing Struc_W3MI type t_R3TR_W3MI
           subrc.
  Data:
    w_FLPARAM type line of t_R3TR_W3MI-FLPARAM,
    FORM_NAME type WWWDATATAB-OBJID,
    it_PARAMS type standard table of WWWPARAMS
      with header line,
    w_Key     type WWWDATATAB.

  subrc = isError.

  w_Key-RELID = 'MI'.
  w_Key-OBJID = Obj_Name.

  Call function 'WWWDATA_IMPORT'
    EXPORTING
      KEY    = w_Key
    TABLES
      MIME   = Struc_W3MI-W3MIME[]
    EXCEPTIONS
      others = 1.
  check sy-subrc = 0.

  FORM_NAME  = Obj_Name.

  Call function 'WWWPARAMS_READ_ALL'
    EXPORTING
      TYPE   = 'MI'
      OBJID  = FORM_NAME
    TABLES
      PARAMS = it_PARAMS[]
    EXCEPTIONS
      others = 1.
  check sy-subrc = 0.
  check not it_PARAMS[] is initial.

  Loop at it_PARAMS.
    Translate it_PARAMS-NAME to lower case.
    Case it_PARAMS-NAME.
      when 'filesize'.
        w_FLPARAM-Length = it_PARAMS-VALUE.
      when 'fileextension'.
        w_FLPARAM-Ext = it_PARAMS-VALUE.
    EndCase.
  EndLoop.
  check not w_FLPARAM is initial.

  Concatenate Obj_Name w_FLPARAM-Ext into w_FLPARAM-Name.
  Append w_FLPARAM to Struc_W3MI-FLPARAM.

  Select *
    into corresponding fields of table Struc_W3MI-WWWDATATAB[]
    from WWWDATA
    where RELID = 'MI'
      and OBJID = Obj_Name
      and SRTF2 = 0.
  check sy-subrc = 0.

  subrc = isOk.
EndForm.                    "Get_R3TR_W3MI

*&---------------------------------------------------------------------*
*&      Form  Export_W3MI
*&---------------------------------------------------------------------*
*Form Export_R3TR_W3MI
*  using Obj_Name
*        Path
*        subrc.
*  Data:
*    it_Readme type tt_Readme,
*    w_TADIR type t_TADIR.

*  w_TADIR-Obj_Name = Obj_Name.
*
*  Perform ExportW3MI
*    tables it_Readme
*    using w_TADIR
*          Path
*          subrc.
*EndForm.                    "Export_W3MI

*&---------------------------------------------------------------------*
*&      Form  ExportW3MI
*&---------------------------------------------------------------------*
Form ExportW3MI
  tables it_Readme
  using p_TADIR type t_TADIR
        Path
        subrc.

  Data:
    sy_subrc type sy-subrc,
    Ext(255),
    w_Key     type WWWDATATAB,
    FORM_NAME type WWWDATATAB-OBJID,
    FileName  type string, "RLGRAP-FILENAME,
    it_WWWDATATAB type tt_WWWDATATAB
      with header line,
    it_PARAMS type standard table of WWWPARAMS
      with header line,
    it_MIME type standard table of W3MIME,
    Len type i,
    Name type string.

  subrc = isError.

  Name = p_TADIR-OBJ_NAME.
  Ext = ''.
  FORM_NAME = Name.

  Call function 'WWWPARAMS_READ_ALL'
    EXPORTING
      TYPE   = 'MI'
      OBJID  = FORM_NAME
    TABLES
      PARAMS = it_PARAMS
    EXCEPTIONS
      others = 1.
  check sy-subrc = 0.

  Loop at it_PARAMS.
    Translate it_PARAMS-NAME to lower case.
    Case it_PARAMS-NAME.
      when 'filesize'.
        Len = it_PARAMS-VALUE.
      when 'fileextension'.
        Ext = it_PARAMS-VALUE.
    EndCase.
  EndLoop.

  Concatenate Path Name '\' Name Ext into FileName.
  w_Key-RELID = 'MI'.
  w_Key-OBJID = FORM_NAME.

  Call function 'WWWDATA_IMPORT'
    EXPORTING
      KEY    = w_Key
    TABLES
      MIME   = it_MIME
    EXCEPTIONS
      others = 1.
  check sy-subrc = 0.

  Perform Write_BIN_File
    using FileName
          Len
    changing it_MIME[].

*  check sy-subrc = 0.

  Select single *
    into corresponding fields of it_WWWDATATAB
    from WWWDATA
    where RELID = 'MI'
      and OBJID = Name
      and SRTF2 = 0.
  If sy-subrc = 0.
    Concatenate Name '-' it_WWWDATATAB-TEXT into it_Readme
      separated by space.
    Append it_Readme.

    Append it_WWWDATATAB.
    Concatenate Path Name '\WWWDATATAB.txt' into FileName.
    Perform Write_File using FileName changing it_WWWDATATAB[].

    subrc = isOk.
  EndIf.

EndForm.                                                    "ExportW3MI

*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_W3MI
*&---------------------------------------------------------------------*
Form Import_R3TR_W3MI
  using w_TADIR type t_TADIR
        struc_W3MI type t_R3TR_W3MI
        subrc.
  Data:
    w_Key     type WWWDATATAB,
    it_PARAMS type standard table of WWWPARAMS
      with header line,
    it_MIMETYPES type standard table of MIMETYPES
      with header line,
    FlParam type FileParameters.

  subrc = isError.

  check not struc_W3MI-WWWDATATAB[] is initial.
  Read table struc_W3MI-WWWDATATAB into w_Key index 1.

  If not struc_W3MI-FLPARAM[] is initial.
    Read table struc_W3MI-FLPARAM[] into FlParam index 1.
  EndIf.

*  Move-corresponding: it_WWWDATATAB to w_Key.
  w_Key-DEVCLASS = w_TADIR-DEVCLASS.
  w_Key-CHNAME = sy-uname.
  w_Key-TDATE  = sy-datum.
  w_Key-TTIME  = sy-UZEIT.

  Call function 'WWWDATA_EXPORT'
    EXPORTING
      KEY    = w_Key
    TABLES
      MIME   = struc_W3MI-W3MIME[]
    EXCEPTIONS
      others = 1.

  If sy-subrc = 0.
    it_PARAMS-RELID = w_Key-RELID.
    it_PARAMS-OBJID = w_TADIR-OBJ_NAME.

    it_PARAMS-NAME  = 'filename'.
    it_PARAMS-VALUE = FlParam-Name.
    Append it_PARAMS.

    Data:
      it_Ext type string occurs 0
        with header line.
    Split FlParam-Name at '.' into table it_Ext.
    Loop at it_Ext.
    EndLoop.
    Translate it_Ext to upper case.
    it_PARAMS-NAME  = 'fileextension'.
    it_PARAMS-VALUE = '.'.
    it_PARAMS-VALUE+1 = it_Ext.
    Append it_PARAMS.

    Clear it_PARAMS-VALUE.
    Select * from MIMETYPES into table it_MIMETYPES.
    Loop at it_MIMETYPES.
      Translate it_MIMETYPES-extension to upper case.
      If it_MIMETYPES-extension CS it_Ext.
        it_PARAMS-VALUE = it_MIMETYPES-type.
        EXIT.
      EndIf.
    EndLoop.
    it_PARAMS-NAME  = 'mimetype'.
    Append it_PARAMS.

    Clear it_PARAMS-VALUE.
    it_PARAMS-NAME  = 'filesize'.
    it_PARAMS-VALUE(10) = FlParam-Length.
    Append it_PARAMS.

    Call function 'WWWPARAMS_UPDATE'
      TABLES
        PARAMS = it_PARAMS.

    subrc = isOk.
  EndIf.

EndForm.                    "Import_R3TR_W3MI

*&---------------------------------------------------------------------*
*&      Form  ImportW3MI
*&---------------------------------------------------------------------*
Form ImportW3MI
  using p_TADIR type t_TADIR
        Path
        subrc.

  Data:
    sy_subrc type sy-subrc,
    Ext(255),
    w_Key     type WWWDATATAB,
    FORM_NAME type WWWDATATAB-OBJID,
    FileName  type string, "RLGRAP-FILENAME,
    Filter    type string,
    Dir       type string,
    Cnt       type i,
    it_WWWDATATAB type tt_WWWDATATAB
      with header line,
    it_FileName type standard table of char255
      with header line,
    it_MIME    type standard table of W3MIME,
    it_MIMEold type standard table of W3MIME,
    Len type i,
    it_PARAMS type standard table of WWWPARAMS
      with header line,
    it_MIMETYPES type standard table of MIMETYPES
      with header line.

  subrc = isError.

  Concatenate Path p_TADIR-OBJ_NAME '\WWWDATATAB.txt' into FileName.
  Perform Read_File using FileName changing it_WWWDATATAB[].
  If not it_WWWDATATAB[] is initial.
    Read table it_WWWDATATAB index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\' into Dir.
  Concatenate p_TADIR-OBJ_NAME '.*' into Filter.
*  Call method CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
*    EXPORTING
*      DIRECTORY  = Dir
*      FILTER     = Filter
*      FILES_ONLY = 'X'
*    CHANGING
*      FILE_TABLE = it_FileName[]
*      COUNT      = Cnt
*    EXCEPTIONS
*      others     = 1.
  Perform Read_Catalog
    using Dir Filter 'F'
    changing it_FileName[].

  If not it_FileName[] is initial.
    Read table it_FileName index 1.
  EndIf.

  Move-corresponding: it_WWWDATATAB to w_Key.
  w_Key-DEVCLASS = p_TADIR-DEVCLASS.
  w_Key-CHNAME = sy-uname.
  w_Key-TDATE  = sy-datum.
  w_Key-TTIME  = sy-UZEIT.
  Concatenate Path p_TADIR-OBJ_NAME '\' it_FileName into FileName.

*  Call function 'GUI_UPLOAD'
*    EXPORTING
*      FILENAME   = FileName
*      FILETYPE   = 'BIN'
*    IMPORTING
*      FILELENGTH = Len
*    TABLES
*      DATA_TAB   = it_MIME
*    EXCEPTIONS
*      OTHERS     = 1.

  Perform Upload_Bin_File
    using
      FileName
    changing
      Len
      it_MIME[].

  If p_Rewrte is initial.
    Call function 'WWWDATA_IMPORT'
      EXPORTING
        KEY    = w_Key
      TABLES
        MIME   = it_MIMEold
      EXCEPTIONS
        others = 1.
  EndIf.

  check it_MIME <> it_MIMEold and
        not it_MIME is initial.

  Call function 'WWWDATA_EXPORT'
    EXPORTING
      KEY    = w_Key
    TABLES
      MIME   = it_MIME
    EXCEPTIONS
      others = 1.

  If sy-subrc = 0.
    it_PARAMS-RELID = w_Key-RELID.
    it_PARAMS-OBJID = p_TADIR-OBJ_NAME.

    it_PARAMS-NAME  = 'filename'.
    it_PARAMS-VALUE = it_FileName.
    Append it_PARAMS.

    Split it_FileName at '.' into table it_FileName.
    Loop at it_FileName.
    EndLoop.
    it_PARAMS-NAME  = 'fileextension'.
    it_PARAMS-VALUE = '.'.
    it_PARAMS-VALUE+1 = it_FileName.
    Append it_PARAMS.

    Clear it_PARAMS-VALUE.
    Select * from MIMETYPES into table it_MIMETYPES.
    Loop at it_MIMETYPES.
      If it_MIMETYPES-extension CS it_FileName.
        it_PARAMS-VALUE = it_MIMETYPES-type.
        EXIT.
      EndIf.
    EndLoop.
    it_PARAMS-NAME  = 'mimetype'.
    Append it_PARAMS.

    Clear it_PARAMS-VALUE.
    it_PARAMS-NAME  = 'filesize'.
    it_PARAMS-VALUE(10) = Len.
    Append it_PARAMS.

    Call function 'WWWPARAMS_UPDATE'
      TABLES
        PARAMS = it_PARAMS.

    Perform SetClassForObject using p_TADIR.
    Perform AppendToQuery using p_TADIR.
    subrc = isOk.
  EndIf.

EndForm.                                                    "ImportW3MI

*&---------------------------------------------------------------------*
*&      Form  SetLangu
*&---------------------------------------------------------------------*
Form SetLangu
  tables it_Data
  using p_Langu.

  Field-symbols:
    <it_data>,
    <langu>.
  Loop at it_Data assigning <it_data>.
    Assign component 'DDLANGUAGE' of structure <it_data> to <langu>.
    If sy-subrc <> 0.
      Exit.
    EndIf.
    <langu> = p_Langu.
  EndLoop.
EndForm.                    "SetLangu
*&---------------------------------------------------------------------*
*&      Form  ExportTABL
*&---------------------------------------------------------------------*
Form ExportTABL
  tables it_Readme
  using p_TADIR type t_TADIR
        Path
        subrc.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD02V type standard table of DD02V
      with header line,
    it_DD09L type standard table of DD09L
      with header line,
    it_DD03P_File  type standard table of DD03P,
    it_DD03P_Langu type standard table of DD03P,
    it_DD05M_File  type standard table of DD05M,
    it_DD05M_Langu type standard table of DD05M,
    it_DD08V_File  type standard table of DD08V,
    it_DD08V_Langu type standard table of DD08V,
    it_DD12V_File  type standard table of DD12V,
    it_DD12V_Langu type standard table of DD12V,
    it_DD17V_File  type standard table of DD17V,
    it_DD17V_Langu type standard table of DD17V,
    it_DD35V_File  type standard table of DD35V,
    it_DD35V_Langu type standard table of DD35V,
    it_DD36M_File  type standard table of DD36M,
    it_DD36M_Langu type standard table of DD36M,
    Name type string.

  subrc = isError.

  Name = p_TADIR-OBJ_NAME.

  DDOBJNAME = Name.

  Loop at it_LANGU.
    Clear: it_DD02V, it_DD09L.
    Refresh: it_DD03P_Langu,
             it_DD05M_Langu,
             it_DD08V_Langu,
             it_DD12V_Langu,
             it_DD17V_Langu,
             it_DD35V_Langu,
             it_DD36M_Langu.

    Call function 'DDIF_TABL_GET'
      EXPORTING
        NAME      = DDOBJNAME
        LANGU     = it_LANGU
      IMPORTING
        DD02V_WA  = it_DD02V
        DD09L_WA  = it_DD09L
      TABLES
        DD03P_TAB = it_DD03P_Langu
        DD05M_TAB = it_DD05M_Langu
        DD08V_TAB = it_DD08V_Langu
        DD12V_TAB = it_DD12V_Langu
        DD17V_TAB = it_DD17V_Langu
        DD35V_TAB = it_DD35V_Langu
        DD36M_TAB = it_DD36M_Langu
      EXCEPTIONS
        others    = 1.

    If sy-subrc = 0.
      If not it_DD02V is initial.
        it_DD02V-DDLANGUAGE = it_LANGU.
        Append it_DD02V.
      EndIf.

      If not it_DD09L is initial.
        Append it_DD09L.
      EndIf.

      Perform SetLangu tables it_DD03P_Langu using it_Langu.
      Append lines of it_DD03P_Langu to it_DD03P_File.

      Perform SetLangu tables it_DD05M_Langu using it_Langu.
      Append lines of it_DD05M_Langu to it_DD05M_File.

      Perform SetLangu tables it_DD08V_Langu using it_Langu.
      Append lines of it_DD08V_Langu to it_DD08V_File.

      Perform SetLangu tables it_DD12V_Langu using it_Langu.
      Append lines of it_DD12V_Langu to it_DD12V_File.

      Perform SetLangu tables it_DD17V_Langu using it_Langu.
      Append lines of it_DD17V_Langu to it_DD17V_File.

      Perform SetLangu tables it_DD35V_Langu using it_Langu.
      Append lines of it_DD35V_Langu to it_DD35V_File.

      Perform SetLangu tables it_DD36M_Langu using it_Langu.
      Append lines of it_DD36M_Langu to it_DD36M_File.
    EndIf.
  EndLoop.

  If not it_DD02V[] is initial.
    Sort it_DD02V[].
    Delete adjacent duplicates from it_DD02V[].
    Concatenate Path Name '\DD02V.txt' into FileName.
    Perform Write_File using FileName changing it_DD02V[].
  EndIf.

  If not it_DD09L[] is initial.
    Sort it_DD09L[].
    Delete adjacent duplicates from it_DD09L[].
    Concatenate Path Name '\DD09L.txt' into FileName.
    Perform Write_File using FileName changing it_DD09L[].
  EndIf.

  If not it_DD03P_File[] is initial.
    Sort it_DD03P_File[].
    Delete adjacent duplicates from it_DD03P_File[].
    Concatenate Path Name '\DD03P.txt' into FileName.
    Perform Write_File using FileName changing it_DD03P_File.
  EndIf.

  If not it_DD05M_File[] is initial.
    Sort it_DD05M_File[].
    Delete adjacent duplicates from it_DD05M_File[].
    Concatenate Path Name '\DD05M.txt' into FileName.
    Perform Write_File using FileName changing it_DD05M_File.
  EndIf.

  If not it_DD08V_File[] is initial.
    Sort it_DD08V_File[].
    Delete adjacent duplicates from it_DD08V_File[].
    Concatenate Path Name '\DD08V.txt' into FileName.
    Perform Write_File using FileName changing it_DD08V_File.
  EndIf.

  If not it_DD12V_File[] is initial.
    Sort it_DD12V_File[].
    Delete adjacent duplicates from it_DD12V_File[].
    Concatenate Path Name '\DD12V.txt' into FileName.
    Perform Write_File using FileName changing it_DD12V_File.
  EndIf.

  If not it_DD17V_File[] is initial.
    Sort it_DD17V_File[].
    Delete adjacent duplicates from it_DD17V_File[].
    Concatenate Path Name '\DD17V.txt' into FileName.
    Perform Write_File using FileName changing it_DD17V_File.
  EndIf.

  If not it_DD35V_File[] is initial.
    Sort it_DD35V_File[].
    Delete adjacent duplicates from it_DD35V_File[].
    Concatenate Path Name '\DD35V.txt' into FileName.
    Perform Write_File using FileName changing it_DD35V_File.
  EndIf.

  If not it_DD36M_File[] is initial.
    Sort it_DD36M_File[].
    Delete adjacent duplicates from it_DD36M_File[].
    Concatenate Path Name '\DD36M.txt' into FileName.
    Perform Write_File using FileName changing it_DD36M_File.
  EndIf.

  subrc = isOk.
EndForm.                    "ExportTABL

*&---------------------------------------------------------------------*
*&      Form  ExportTTYP
*&---------------------------------------------------------------------*
Form ExportTTYP
  tables it_Readme
  using p_TADIR type t_TADIR
        Path
        subrc.
  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD40V type standard table of DD40V
      with header line,
    it_DD42V type standard table of DD42V
      with header line,
    Name type string.

  subrc = isError.

  Name = p_TADIR-OBJ_NAME.
  DDOBJNAME = Name.

  Call function 'DDIF_TTYP_GET'
    EXPORTING
      NAME      = DDOBJNAME
      LANGU     = sy-langu
    IMPORTING
      DD40V_WA  = it_DD40V
    TABLES
      DD42V_TAB = it_DD42V
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    If not it_DD40V is initial.
      Append it_DD40V.
      Concatenate Path Name '\DD40V.txt' into FileName.
      Perform Write_File using FileName changing it_DD40V[].
    EndIf.

    Concatenate Path Name '\DD42V.txt' into FileName.
    Perform Write_File using FileName changing it_DD42V[].

    subrc = isOk.
  EndIf.
EndForm.                    "ExportTTYP


*&---------------------------------------------------------------------*
*&      Form  ImportTTYP
*&---------------------------------------------------------------------*
Form ImportTTYP
  using p_TADIR type t_TADIR
        Path.
  Types:
    Begin of t_TTYP,
      DD40V type DD40V,
      it_DD42V type standard table of DD42V
        with key table_line,
    End of t_TTYP.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    OldTTYP type t_TTYP,
    NewTTYP type t_TTYP,
    it_DD40V type standard table of DD40V
      with header line.

  DDOBJNAME = p_TADIR-OBJ_NAME.

  Concatenate Path p_TADIR-OBJ_NAME '\DD40V.txt' into FileName.
  Perform Read_File using FileName changing it_DD40V[].
  If not it_DD40V[] is initial.
    Read table it_DD40V into NewTTYP-DD40V index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\DD42V.txt' into FileName.
  Perform Read_File using FileName changing NewTTYP-it_DD42V[].

  If p_Rewrte is initial.
    Call function 'DDIF_TTYP_GET'
      EXPORTING
        NAME      = DDOBJNAME
        LANGU     = sy-langu
      IMPORTING
        DD40V_WA  = OldTTYP-DD40V
      TABLES
        DD42V_TAB = OldTTYP-it_DD42V
      EXCEPTIONS
        others    = 1.
  EndIf.

  If NewTTYP <> OldTTYP and
     not NewTTYP is initial.


    Call function 'DDIF_TTYP_PUT'
      EXPORTING
        NAME      = DDOBJNAME
*        LANGU     = sy-langu
        DD40V_WA  = NewTTYP-DD40V
      TABLES
        DD42V_TAB = NewTTYP-it_DD42V
      EXCEPTIONS
        others    = 1.

    If sy-subrc <> 0.
      Perform WriteMessageTo using sy-subrc changing p_TADIR.
    Else.
      Perform SetClassForObject using p_TADIR.

      If sy-subrc <> 0.
        Perform WriteMessageTo using sy-subrc changing p_TADIR.
      Else.
        Perform AppendToQuery using p_TADIR.

*        Call function 'DDIF_TTYP_ACTIVATE'
*          EXPORTING
*            NAME   = DDOBJNAME
*          EXCEPTIONS
*            others = 1.
      EndIf.
    EndIf.

  EndIf.
EndForm.                    "ImportTTYP

*&---------------------------------------------------------------------*
*&      Form  ExportENQU
*&---------------------------------------------------------------------*
Form ExportENQU
  tables it_Readme
  using Name
        Path
        subrc.
  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD25V type standard table of DD25V
      with header line,
    it_DD26E type standard table of DD26E
      with header line,
    it_DD27P type standard table of DD27P
      with header line,
    it_DDENA type standard table of DDENA
      with header line.

  subrc = isError.

  DDOBJNAME = Name.

  Call function 'DDIF_ENQU_GET'
    EXPORTING
      NAME      = DDOBJNAME
      LANGU     = sy-langu
    IMPORTING
      DD25V_WA  = it_DD25V
    TABLES
      DD26E_TAB = it_DD26E
      DD27P_TAB = it_DD27P
      DDENA_TAB = it_DDENA
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    If not it_DD25V is initial.
      Append it_DD25V.
      Concatenate Path Name '\DD25V.txt' into FileName.
      Perform Write_File using FileName changing it_DD25V[].
    EndIf.

    If not it_DD26E[] is initial.
      Concatenate Path Name '\DD26E.txt' into FileName.
      Perform Write_File using FileName changing it_DD26E[].
    EndIf.
    If not it_DD27P[] is initial.
      Concatenate Path Name '\DD27P.txt' into FileName.
      Perform Write_File using FileName changing it_DD27P[].
    EndIf.
    If not it_DDENA[] is initial.
      Concatenate Path Name '\DDENA.txt' into FileName.
      Perform Write_File using FileName changing it_DDENA[].
    EndIf.

    subrc = isOk.
  EndIf.
EndForm.                    "ExportENQU

*&---------------------------------------------------------------------*
*&      Form  ImportENQU
*&---------------------------------------------------------------------*
Form ImportENQU
  using p_TADIR type t_TADIR
        Path.
  Types:
    Begin of t_ENQU,
      DD25V type DD25V,
      it_DD26E type standard table of DD26E
        with key table_line,
      it_DD27P type standard table of DD27P
        with key table_line,
      it_DDENA type standard table of DDENA
        with key table_line,
    End of t_ENQU.

  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    OldENQU type t_ENQU,
    NewENQU type t_ENQU,
    it_DD25V type standard table of DD25V
      with header line.

  DDOBJNAME = p_TADIR-OBJ_NAME.

  Concatenate Path p_TADIR-OBJ_NAME '\DD25V.txt' into FileName.
  Perform Read_File using FileName changing it_DD25V[].
  If not it_DD25V[] is initial.
    Read table it_DD25V into NewENQU-DD25V index 1.
  EndIf.

  Concatenate Path p_TADIR-OBJ_NAME '\DD26E.txt' into FileName.
  Perform Read_File using FileName changing NewENQU-it_DD26E[].

  Concatenate Path p_TADIR-OBJ_NAME '\DD27P.txt' into FileName.
  Perform Read_File using FileName changing NewENQU-it_DD27P[].

  Concatenate Path p_TADIR-OBJ_NAME '\DDENA.txt' into FileName.
  Perform Read_File using FileName changing NewENQU-it_DDENA[].

  If p_Rewrte is initial.
    Call function 'DDIF_ENQU_GET'
      EXPORTING
        NAME      = DDOBJNAME
        LANGU     = sy-langu
      IMPORTING
        DD25V_WA  = OldENQU-DD25V
      TABLES
        DD26E_TAB = OldENQU-it_DD26E
        DD27P_TAB = OldENQU-it_DD27P
        DDENA_TAB = OldENQU-it_DDENA
      EXCEPTIONS
        others    = 1.
  EndIf.

  If NewENQU <> OldENQU and
     not NewENQU is initial.


    Call function 'DDIF_ENQU_PUT'
      EXPORTING
        NAME      = DDOBJNAME
        DD25V_WA  = NewENQU-DD25V
      TABLES
        DD26E_TAB = NewENQU-it_DD26E
        DD27P_TAB = NewENQU-it_DD27P
*        DDENA_TAB = NewENQU-it_DDENA
      EXCEPTIONS
        others    = 1.

    If sy-subrc <> 0.
      Perform WriteMessageTo using sy-subrc changing p_TADIR.
    Else.
      Perform SetClassForObject using p_TADIR.

      If sy-subrc <> 0.
        Perform WriteMessageTo using sy-subrc changing p_TADIR.
      Else.
        Perform AppendToQuery using p_TADIR.

*        Call function 'DDIF_ENQU_ACTIVATE'
*          EXPORTING
*            NAME   = DDOBJNAME
*          EXCEPTIONS
*            others = 1.
      EndIf.
    EndIf.

  EndIf.
EndForm.                    "ImportENQU

*&---------------------------------------------------------------------*
*&      Form  Import_LIMU_ENQD
*&---------------------------------------------------------------------*
Form Import_LIMU_ENQD
  using
    Obj_Name
    Struc_ENQD type SVRS2_ENQD
    subrc.

  Data:
    DDOBJNAME type DDOBJNAME,
    it_DD25V type standard table of DD25V
      with header line,
    it_DD26E type standard table of DD26E
      with header line,
    it_DD27P type standard table of DD27P
      with header line.
  Field-symbols:
    <DD26E> type DD26E,
    <DD27P> type DD27P.

  subrc = isError.

  DDOBJNAME = Obj_Name.

*         DD25TV TYPE DD25TV OCCURS 0,
*         DD25V TYPE DD25V OCCURS 0,
*         DD26V TYPE DD26V OCCURS 0,
*         DD27V TYPE DD27V OCCURS 0,
*         MDLOG TYPE SMODILOG OCCURS 0,
  Perform Table_Corresponding
    tables:
      Struc_ENQD-DD25V it_DD25V,
      Struc_ENQD-DD26V it_DD26E,
      Struc_ENQD-DD27V it_DD27P.

  Loop at it_DD27P assigning <DD27P>
    where FIELDNAME = '*'.

    Loop at it_DD26E assigning <DD26E>
      where TABNAME = <DD27P>-TABNAME.
      <DD26E>-ENQMODE = <DD27P>-ENQMODE.
    EndLoop.
    Delete it_DD27P.
  EndLoop.

  If not it_DD25V[] is initial.
    Read table it_DD25V index 1.
  EndIf.

  Call function 'DDIF_ENQU_PUT'
    EXPORTING
      NAME      = DDOBJNAME
      DD25V_WA  = it_DD25V
    TABLES
      DD26E_TAB = it_DD26E
      DD27P_TAB = it_DD27P
    EXCEPTIONS
      others    = 1.
  If sy-subrc = 0.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_LIMU_ENQD

*&---------------------------------------------------------------------*
*&      Form  ExportDOMA
*&---------------------------------------------------------------------*
Form ExportDOMA
  tables it_Readme
  using p_TADIR type t_TADIR
        Path
        subrc.
  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD01V type standard table of DD01V
      with header line,
    it_DD07V type standard table of DD07V
      with header line,
    Name type string.

  subrc = isError.

  Name = p_TADIR-OBJ_NAME.

  DDOBJNAME = Name.

  Call function 'DDIF_DOMA_GET'
    EXPORTING
      NAME      = DDOBJNAME
      LANGU     = sy-langu
    IMPORTING
      DD01V_WA  = it_DD01V
    TABLES
      DD07V_TAB = it_DD07V
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    If not it_DD01V is initial.
      Append it_DD01V.
      Concatenate Path Name '\DD01V.txt' into FileName.
      Perform Write_File using FileName changing it_DD01V[].
    EndIf.

    If not it_DD07V[] is initial.
      Concatenate Path Name '\DD07V.txt' into FileName.
      Perform Write_File using FileName changing it_DD07V[].
    EndIf.

    subrc = isOk.
  EndIf.
EndForm.                    "ExportDOMA

*&---------------------------------------------------------------------*
*&      Form  ExportDTEL
*&---------------------------------------------------------------------*
Form ExportDTEL
  tables it_Readme
  using p_TADIR type t_TADIR
        Path
        subrc.
  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD04V type standard table of DD04V
      with header line,
    it_TPARA type standard table of TPARA
      with header line,
    Name type string.

  subrc = isError.

  Name = p_TADIR-OBJ_NAME.
  DDOBJNAME = Name.

  Call function 'DDIF_DTEL_GET'
    EXPORTING
      NAME     = DDOBJNAME
      LANGU    = sy-langu
    IMPORTING
      DD04V_WA = it_DD04V
      TPARA_WA = it_TPARA
    EXCEPTIONS
      others   = 1.

  If sy-subrc = 0.
    If not it_DD04V is initial.
      Append it_DD04V.
      Concatenate Path Name '\DD04V.txt' into FileName.
      Perform Write_File using FileName changing it_DD04V[].
    EndIf.

    If not it_TPARA is initial.
      Append it_TPARA.
      Concatenate Path Name '\TPARA.txt' into FileName.
      Perform Write_File using FileName changing it_TPARA[].
    EndIf.

    subrc = isOk.
  EndIf.
EndForm.                    "ExportDTEL



*&---------------------------------------------------------------------*
*&      Form  ExportSHLP
*&---------------------------------------------------------------------*
Form ExportSHLP
  tables it_Readme
  using Name
        Path
        subrc.
  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD30V type standard table of DD30V
      with header line,
    it_DD31V type standard table of DD31V,
    it_DD32P type standard table of DD32P,
    it_DD33V type standard table of DD33V.

  subrc = isError.

  DDOBJNAME = Name.

  Call function 'DDIF_SHLP_GET'
    EXPORTING
      NAME      = DDOBJNAME
      LANGU     = sy-langu
    IMPORTING
      DD30V_WA  = it_DD30V
    TABLES
      DD31V_TAB = it_DD31V
      DD32P_TAB = it_DD32P
      DD33V_TAB = it_DD33V
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    If not it_DD30V is initial.
      Append it_DD30V.
      Concatenate Path Name '\DD30V.txt' into FileName.
      Perform Write_File using FileName changing it_DD30V[].
    EndIf.

    If not it_DD31V[] is initial.
      Concatenate Path Name '\DD31V.txt' into FileName.
      Perform Write_File using FileName changing it_DD31V.
    EndIf.

    If not it_DD32P[] is initial.
      Concatenate Path Name '\DD32P.txt' into FileName.
      Perform Write_File using FileName changing it_DD32P.
    EndIf.

    If not it_DD33V[] is initial.
      Concatenate Path Name '\DD33V.txt' into FileName.
      Perform Write_File using FileName changing it_DD33V.
    EndIf.

    subrc = isOk.
  EndIf.

EndForm.                    "ExportSHLP


*&---------------------------------------------------------------------*
*&      Form  Get_R3TR_TOBJ
*&---------------------------------------------------------------------*
Form Get_R3TR_TOBJ
  using Obj_Name
  changing Struc_TOBJ type t_R3TR_TOBJ
           subrc.

  Data:
*    Struc_TOBJ type t_R3TR_TOBJ,
    IV_OBJ_NAME   type E071-OBJ_NAME,
    IV_OBJECTNAME type OBJH-OBJECTNAME,
    IV_OBJECTTYPE type OBJH-OBJECTTYPE,
*    PathObj type string,
    w_OBJH type OBJH,
    w_OBJT type OBJT.

  subrc = isError.

  IV_OBJ_NAME = Obj_Name.

  CALL FUNCTION 'CTO_TADIR_GET_OBJECT'
    EXPORTING
      IV_OBJ_NAME   = IV_OBJ_NAME
    IMPORTING
      EV_OBJECTNAME = IV_OBJECTNAME
      EV_OBJECTTYPE = IV_OBJECTTYPE
    EXCEPTIONS
      others        = 99.
  check sy-subrc = 0.

  CALL FUNCTION 'CTO_OBJECT_GET'
    EXPORTING
      IV_OBJECTNAME              = IV_OBJECTNAME
      IV_OBJECTTYPE              = IV_OBJECTTYPE
*     IV_LANGUAGE                = SY-LANGU
     IV_SEL_OBJT                = 'X'
     IV_SEL_OBJS                = 'X'
     IV_SEL_OBJSL               = 'X'
     IV_SEL_OBJM                = 'X'
   IMPORTING
     ES_OBJH                    = w_OBJH
     ES_OBJT                    = w_OBJT
*     EV_OBJT_DOESNT_EXIST       =
   TABLES
     TT_OBJS                    = Struc_TOBJ-OBJS[]
     TT_OBJSL                   = Struc_TOBJ-OBJSL[]
     TT_OBJM                    = Struc_TOBJ-OBJM[]
   EXCEPTIONS
     OBJECT_NOT_DEFINED         = 1
     OTHERS                     = 2.

  If sy-subrc = 0.
    If not w_OBJH is initial.
      Append w_OBJH to Struc_TOBJ-OBJH.
    EndIf.
    If not w_OBJT is initial.
      Append w_OBJT to Struc_TOBJ-OBJT.
    EndIf.

    Select *
      into table Struc_TOBJ-TDDAT
      from TDDAT
      where TABNAME = IV_OBJECTNAME.

    Select *
      into table Struc_TOBJ-TVDIR
      from TVDIR
      where TABNAME = IV_OBJECTNAME.

    Select *
      into table Struc_TOBJ-TVIMF
      from TVIMF
      where TABNAME = IV_OBJECTNAME.

*    Concatenate Path Obj_Name '\' into PathObj.
*    Perform Download_Struc
*      using Struc_TOBJ
*            PathObj.

    subrc = isOk.
  EndIf.
EndForm.                    "Get_R3TR_TOBJ

*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_TOBJ
*&---------------------------------------------------------------------*
*Form Export_R3TR_TOBJ
*  using Obj_Name
*        Path
*        subrc.

*  Data:
*    Struc_TOBJ type t_R3TR_TOBJ,
*    IV_OBJ_NAME   type E071-OBJ_NAME,
*    IV_OBJECTNAME type OBJH-OBJECTNAME,
*    IV_OBJECTTYPE type OBJH-OBJECTTYPE,
*    PathObj type string,
*    w_OBJH type OBJH,
*    w_OBJT type OBJT.
*
*  subrc = isError.
*
*  IV_OBJ_NAME = Obj_Name.
*
*  CALL FUNCTION 'CTO_TADIR_GET_OBJECT'
*    EXPORTING
*      IV_OBJ_NAME   = IV_OBJ_NAME
*    IMPORTING
*      EV_OBJECTNAME = IV_OBJECTNAME
*      EV_OBJECTTYPE = IV_OBJECTTYPE
*    EXCEPTIONS
*      others        = 99.
*  check sy-subrc = 0.
*
*  CALL FUNCTION 'CTO_OBJECT_GET'
*    EXPORTING
*      IV_OBJECTNAME              = IV_OBJECTNAME
*      IV_OBJECTTYPE              = IV_OBJECTTYPE
**     IV_LANGUAGE                = SY-LANGU
*     IV_SEL_OBJT                = 'X'
*     IV_SEL_OBJS                = 'X'
*     IV_SEL_OBJSL               = 'X'
*     IV_SEL_OBJM                = 'X'
*   IMPORTING
*     ES_OBJH                    = w_OBJH
*     ES_OBJT                    = w_OBJT
**     EV_OBJT_DOESNT_EXIST       =
*   TABLES
*     TT_OBJS                    = Struc_TOBJ-OBJS[]
*     TT_OBJSL                   = Struc_TOBJ-OBJSL[]
*     TT_OBJM                    = Struc_TOBJ-OBJM[]
*   EXCEPTIONS
*     OBJECT_NOT_DEFINED         = 1
*     OTHERS                     = 2.
*
*  If sy-subrc = 0.
*    If not w_OBJH is initial.
*      Append w_OBJH to Struc_TOBJ-OBJH.
*    EndIf.
*    If not w_OBJT is initial.
*      Append w_OBJT to Struc_TOBJ-OBJT.
*    EndIf.
*
*    Select *
*      into table Struc_TOBJ-TDDAT
*      from TDDAT
*      where TABNAME = IV_OBJECTNAME.
*
*    Select *
*      into table Struc_TOBJ-TVDIR
*      from TVDIR
*      where TABNAME = IV_OBJECTNAME.
*
*    Select *
*      into table Struc_TOBJ-TVIMF
*      from TVIMF
*      where TABNAME = IV_OBJECTNAME.
*
*    Concatenate Path Obj_Name '\' into PathObj.
*    Perform Download_Struc
*      using Struc_TOBJ
*            PathObj.
*
*    subrc = isOk.
*  EndIf.
*
*EndForm.                    "Export_R3TR_TOBJ

*&---------------------------------------------------------------------*
*&      Form  ExportVIEW
*&---------------------------------------------------------------------*
Form ExportVIEW
  tables it_Readme
  using Name
        Path
        subrc.
  Data:
    DDOBJNAME TYPE  DDOBJNAME,
    FileName type string,
    it_DD25V type standard table of DD25V
      with header line,
    it_DD09V type standard table of DD09V
      with header line,
    it_DD26V type standard table of DD26V,
    it_DD27P type standard table of DD27P,
    it_DD28J type standard table of DD28J,
    it_DD28V type standard table of DD28V,
    it_TVDIR type standard table of TVDIR,
    it_TDDAT type standard table of TDDAT.

  subrc = isError.

  DDOBJNAME = Name.

  Call function 'DDIF_VIEW_GET'
    EXPORTING
      NAME      = DDOBJNAME
      LANGU     = sy-langu
    IMPORTING
      DD25V_WA  = it_DD25V
      DD09L_WA  = it_DD09V
    TABLES
      DD26V_TAB = it_DD26V
      DD27P_TAB = it_DD27P
      DD28J_TAB = it_DD28J
      DD28V_TAB = it_DD28V
    EXCEPTIONS
      others    = 1.

  If sy-subrc = 0.
    If not it_DD25V is initial.
      Append it_DD25V.
      Concatenate Path Name '\DD25V.txt' into FileName.
      Perform Write_File using FileName changing it_DD25V[].
    EndIf.
    If not it_DD09V is initial.
      Append it_DD09V.
      Concatenate Path Name '\DD09V.txt' into FileName.
      Perform Write_File using FileName changing it_DD09V[].
    EndIf.

    Concatenate Path Name '\DD26V.txt' into FileName.
    Perform Write_File using FileName changing it_DD26V.

    Concatenate Path Name '\DD27P.txt' into FileName.
    Perform Write_File using FileName changing it_DD27P.

    Concatenate Path Name '\DD28J.txt' into FileName.
    Perform Write_File using FileName changing it_DD28J.

    Concatenate Path Name '\DD28V.txt' into FileName.
    Perform Write_File using FileName changing it_DD28V.

    Select *
      into table it_TVDIR
      from TVDIR
      where TABNAME = DDOBJNAME.

    Concatenate Path Name '\TVDIR.txt' into FileName.
    Perform Write_File using FileName changing it_TVDIR.

    Select *
      into table it_TDDAT
      from TDDAT
      where TABNAME = DDOBJNAME.

    Concatenate Path Name '\TDDAT.txt' into FileName.
    Perform Write_File using FileName changing it_TDDAT.

    subrc = isOk.
  EndIf.
EndForm.                    "ExportVIEW

*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_TDAT
*&---------------------------------------------------------------------*
Form Export_R3TR_TDAT
  using Obj_Name
        Path
        subrc.
*########## ###### ########
*########### ### Paint Report, RW
EndForm.                    "Export_R3TR_TDAT

*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_CDAT
*&---------------------------------------------------------------------*
Form Export_R3TR_CDAT
  using Obj_Name
        Path
        subrc.
*  ####### ######## #######: ######
*########### ### APPL_LOG - ####### ########
EndForm.                    "Export_R3TR_CDAT

*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_NROB
*&---------------------------------------------------------------------*
Form Export_R3TR_NROB
  using Obj_Name
        Path
        subrc.
*  ####### ######### #######
*###########
EndForm.                    "Export_R3TR_NROB


*&---------------------------------------------------------------------*
*&      Form  Get_R3TR_TABU
*&---------------------------------------------------------------------*
Form Get_R3TR_TABU
  using Obj_Name
  changing Struc_TABU type t_R3TR_TABU
           subrc.
  Data:
    TABNAME type DD02L-TABNAME,
    it_Data type ref to data.
  Data:
    l_MTYPE    type E071K-MASTERTYPE,
    l_MTABNAME type E071K-MASTERNAME.
*    it_Key  like it_E071K[].
  Field-symbols:
    <it_Data> type table.

  subrc = isError.

  Perform CreateInternalTable using Obj_Name changing it_Data.
  check sy-subrc = 0.
  Assign it_Data->* to <it_Data>.
  check sy-subrc = 0.

  Loop at it_E071K
    where PGMID   = 'R3TR'
      and OBJECT  = 'TABU'
      and OBJNAME = Obj_Name.
    l_MTYPE = it_E071K-MASTERTYPE.
    l_MTABNAME = it_E071K-MASTERNAME.
    Append it_E071K to Struc_TABU-E071K[].
  EndLoop.

  TABNAME = Obj_Name.
  Call function 'SRTT_TABLE_GET_BY_KEYLIST'
    EXPORTING
      TABNAME   = TABNAME
*(28.02.2014
      MTYPE     = l_MTYPE
      MTABNAME  = l_MTABNAME
*)28.02.2014
    TABLES
      E071K_TAB = Struc_TABU-E071K[]
      ENTRY_TAB = <it_Data>
    EXCEPTIONS
      others    = 1.
  check sy-subrc = 0.

  Perform Table_to_StringTable
    using <it_Data> ''
    changing Struc_TABU-DATA[].

  subrc = isOk.

EndForm.                    "Get_R3TR_TABU

*&---------------------------------------------------------------------*
*&      Form  Export_R3TR_TABU
*&---------------------------------------------------------------------*
*Form Export_R3TR_TABU
*  using Obj_Name
*        Path
*        subrc.
*  Data:
*    it_Readme type tt_Readme,
*    w_TADIR type t_TADIR.
*
*  w_TADIR-PGMID = 'R3TR'.
*  w_TADIR-OBJECT = 'TABU'.
*  w_TADIR-Obj_Name = Obj_Name.
*
*  Perform ExportTABU
*    tables it_Readme
*           it_E071K
*    using w_TADIR
*          Path
*          subrc.
*
*EndForm.                    "Export_R3TR_TABU

*&---------------------------------------------------------------------*
*&      Form  ExportTABU
*&---------------------------------------------------------------------*
Form ExportTABU
  tables
    it_Readme
    it_E071K structure E071K
  using w_TADIR type t_TADIR "Name
        Path
        subrc.

  Data:
    TABNAME type DD02L-TABNAME,
    it_Data type ref to data,
    it_Key  like it_E071K[].
  Field-symbols:
    <it_Data> type table.

  subrc = isError.

  Perform CreateInternalTable using w_TADIR-OBJ_NAME changing it_Data.
  check sy-subrc = 0.
  Assign it_Data->* to <it_Data>.
  check sy-subrc = 0.

  Loop at it_E071K
    where PGMID   = w_TADIR-PGMID
      and OBJECT  = w_TADIR-OBJECT
      and OBJNAME = w_TADIR-OBJ_NAME.
    Append it_E071K to it_Key.
  EndLoop.

  TABNAME = w_TADIR-OBJ_NAME.
  Call function 'SRTT_TABLE_GET_BY_KEYLIST'
    EXPORTING
      TABNAME   = TABNAME
    TABLES
      E071K_TAB = it_Key
      ENTRY_TAB = <it_Data>
    EXCEPTIONS
      others    = 1.
  check sy-subrc = 0.

  Concatenate Path w_TADIR-OBJ_NAME '\DATA.txt' into FileName.
  Perform Write_File using FileName changing <it_Data>.

  Concatenate Path w_TADIR-OBJ_NAME '\E071K.txt' into FileName.
  Perform Write_File using FileName changing it_Key.

  subrc = isOk.
EndForm.                    "ExportTABU


*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_TABU
*&---------------------------------------------------------------------*
Form Import_R3TR_TABU
  using w_TADIR type t_TADIR
        struc_TABU type t_R3TR_TABU
        subrc.
  Data:
    subrc_parsing type sy-subrc,
    TABNAME type DD02L-TABNAME,
    it_Data type ref to data.
*    it_Key  type tt_E071K.
  Field-symbols:
    <it_DataOld> type table,
    <it_Data> type table.

  subrc = isError.

  Perform CreateInternalTable using w_TADIR-OBJ_NAME changing it_Data.

  If sy-subrc = 0.
    Assign it_Data->* to <it_Data>.
    check sy-subrc = 0.
  Else.
    Exit.
  EndIf.

  Perform Parse_Text_File
    tables struc_TABU-DATA
           <it_Data>
    changing subrc_parsing.
  check subrc_parsing = 0.

  Create data it_Data like <it_Data>.
  Assign it_Data->* to <it_DataOld>.

  TABNAME = w_TADIR-OBJ_NAME.
  Call function 'SRTT_TABLE_GET_BY_KEYLIST'
    EXPORTING
      TABNAME   = TABNAME
    TABLES
      E071K_TAB = struc_TABU-E071K[] "it_Key
      ENTRY_TAB = <it_DataOld>.
*  EndIf.

  If p_Rewrte = 'X' or
     <it_Data> <> <it_DataOld>.
*     not <it_Data> is initial.

    Delete (w_TADIR-OBJ_NAME) from table <it_DataOld>.
    Modify (w_TADIR-OBJ_NAME) from table <it_Data>.

    If sy-subrc = 0.
      subrc = isOk.
    EndIf.

  EndIf.

EndForm.                    "Import_R3TR_TABU

*&---------------------------------------------------------------------*
*&      Form  ImportTABU
*&---------------------------------------------------------------------*
Form ImportTABU
  using w_TADIR type t_TADIR "Name
        Path
        subrc.

  Data:
    TABNAME type DD02L-TABNAME,
    it_Data type ref to data,
    it_Key  type tt_E071K.
  Field-symbols:
    <it_DataOld> type table,
    <it_Data> type table.

  subrc = isError.

  Perform CreateInternalTable using w_TADIR-OBJ_NAME changing it_Data.

  If sy-subrc = 0.
    Assign it_Data->* to <it_Data>.
    check sy-subrc = 0.
  Else.
    Exit.
  EndIf.

  Create data it_Data like <it_Data>.
  Assign it_Data->* to <it_DataOld>.

  Concatenate Path w_TADIR-OBJ_NAME '\DATA.txt' into FileName.
  Perform Read_File using FileName changing <it_Data>.

  Concatenate Path w_TADIR-OBJ_NAME '\E071K.txt' into FileName.
  Perform Read_File using FileName changing it_Key.

*  If p_Rewrte is initial.
  TABNAME = w_TADIR-OBJ_NAME.
  Call function 'SRTT_TABLE_GET_BY_KEYLIST'
    EXPORTING
      TABNAME   = TABNAME
    TABLES
      E071K_TAB = it_Key
      ENTRY_TAB = <it_DataOld>.
*  EndIf.

  If p_Rewrte = 'X' or
     <it_Data> <> <it_DataOld>.
*     not <it_Data> is initial.

    Delete (w_TADIR-OBJ_NAME) from table <it_DataOld>.
    Modify (w_TADIR-OBJ_NAME) from table <it_Data>.

    If sy-subrc = 0.
      Perform AppendToQueryWithKey using w_TADIR it_Key.
      subrc = isOk.
    EndIf.

  EndIf.
EndForm.                    "ImportTABU



*&---------------------------------------------------------------------*
*&      Form  CreateInternalTable
*&---------------------------------------------------------------------*
Form CreateInternalTable
  using p_TabNm
  changing it_Data type ref to data.

  Data:
    it_FIELDCATALOG type LVC_T_FCAT,
    TABNAME type DD02L-TABNAME.

* ### ###### ERP, ################, #### ###### ##########
  Create data it_Data type standard table of (p_TabNm)."#EC V#700
  If sy-saprl(1) > '4'."#EC V#700
    Exit."#EC V#700
  EndIf."#EC V#700

* ### ###### 4.6, ########### 36 #######
  TABNAME = p_TabNm.
  Call function'LVC_FIELDCATALOG_MERGE'
    exporting I_STRUCTURE_NAME = TABNAME
    changing  CT_FIELDCAT = it_FIELDCATALOG
    exceptions others = 1.

  Call method CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = it_FIELDCATALOG
    IMPORTING
      EP_TABLE        = it_Data.

EndForm.                    " CreateInternalTable

*&---------------------------------------------------------------------*
*&      Form  AppendToQuery
*&---------------------------------------------------------------------*
Form AppendToQuery
  using p_TADIR type t_TADIR.
  Data:
    it_E071 type standard table of E071
      with header line.

  Move-corresponding: p_TADIR to it_E071.
  Append it_E071.

*  Call function 'TR_APPEND_TO_COMM'
*    EXPORTING
*      PI_KORRNUM = s_TRKORR-low
*      WI_E071    = w_E071
*    EXCEPTIONS
*      others     = 1.

*  Call function 'TRINT_APPEND_COMM'
*    EXPORTING
*      WI_TRKORR   = s_TRKORR-low
*      WI_SEL_E071 = 'X'
*    TABLES
*      WT_E071     = it_E071
*    EXCEPTIONS
*      others      = 1.

*# ########### # ####. TLOCK
  Call function 'TRINT_APPEND_TO_COMM_ARRAYS'
    EXPORTING
      WI_TRKORR                    = s_TRKORR-low
      IV_NO_OWNER_CHECK            = 'X' "### ######## #########
      IV_APPEND_AT_ORDER           = 'X' "### ######, ######## # ######
      IV_APPEND_AT_ORDER_WITH_LOCK = 'X' "# ########### # ####. TLOCK
    TABLES
      WT_E071                      = it_E071
    EXCEPTIONS
      others                       = 1.

EndForm.                    "AppendToQuery

*&---------------------------------------------------------------------*
*&      Form  AppendToQueryWithKey
*&---------------------------------------------------------------------*
Form AppendToQueryWithKey
  using p_TADIR type t_TADIR
        it_E071K type tt_E071K.
  Data:
    it_E071 type standard table of E071
      with header line.

  check not s_TRKORR-low is initial.

  Move-corresponding: p_TADIR to it_E071.
  If not it_E071K[] is initial.
    it_E071-OBJFUNC = 'K'.
  EndIf.
  Append it_E071.

  Call function 'TRINT_APPEND_COMM'
    EXPORTING
      WI_TRKORR    = s_TRKORR-low
      WI_SEL_E071  = 'X'
      WI_SEL_E071K = 'X'
    TABLES
      WT_E071      = it_E071
      WT_E071K     = it_E071K
    EXCEPTIONS
      others       = 1.
EndForm.                    "AppendToQueryWithKey

*&---------------------------------------------------------------------*
*&      Form  Table_to_StringTable
*&---------------------------------------------------------------------*
Form Table_to_StringTable
  using it_Data type table
        one_line
  changing it_StringData type table.

  Data:
    w_StringData type string,
    Str type string.

  Field-symbols:
    <data>, <fld>.

  If not it_Data[] is initial.
    Perform GetHeaderString using it_Data changing w_StringData.
    If not w_StringData is initial.
      If one_line is initial.
        Append w_StringData to it_StringData.
      EndIf.
    EndIf.

    Loop at it_Data assigning <data>.
      If one_line is initial.
        Clear w_StringData.
      EndIf.

      If not w_StringData is initial.
        Concatenate w_StringData char_0D char_0A into w_StringData.
      EndIf.

      Do.
        Assign component sy-index of structure <data> to <fld>.
        If sy-subrc <> 0.
          If sy-index = 1.
            Str = <data>.
            Concatenate w_StringData Str into w_StringData.
          EndIf.
          Exit.
        EndIf.
        If sy-index = 1.
          Str = <fld>.
          Concatenate w_StringData Str into w_StringData.
        Else.
          Str = <fld>.
          Concatenate w_StringData char_tab Str into w_StringData.
        EndIf.
      EndDo.
      If one_line is initial.
        Append w_StringData to it_StringData.
      EndIf.
    EndLoop.

    If NOT one_line is initial.
      Append w_StringData to it_StringData.
    EndIf.
  EndIf.
EndForm.                    "Table_to_StringTable



*&---------------------------------------------------------------------*
*&      Form  DownloadBinFile
*&---------------------------------------------------------------------*
Form Download_Bin_File
  using p_FileName
        p_Length
  changing ref_Data.

  Field-symbols:
    <xstr>,
    <table> type table.

  Data:
    Len type i,
    typeData,
    it_BIN type standard table of W3MIME.
*    FileNameString type string,
*    FileNameRLGRAP type RLGRAP-FILENAME.

  Field-symbols:
    <filename>.

  Len = p_Length.

  Describe field ref_Data type typeData.

  Case typeData.
    when 'h'.
      Assign ref_Data to <table>.
      check sy-subrc = 0.
    when 'y'.
      Assign it_BIN[] to <table>.
      check sy-subrc = 0.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          BUFFER        = ref_Data
        IMPORTING
          OUTPUT_LENGTH = Len
        TABLES
          BINARY_TAB    = it_BIN[].
      check sy-subrc = 0.
  EndCase.
  check <table> is assigned.

*  If sy-saprl >= '600'.
*    Assign FileNameString to <filename>.
*  Else.
*    Assign FileNameRLGRAP to <filename>.
*  EndIf.

  Data: FileNameRef type ref to data.
  Create data FileNameRef type (FILENAME_type).
  Assign FileNameRef->* to <filename>.

  <filename> = p_FileName.

  Call function 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME     = <filename>
      FILETYPE     = 'BIN'
      BIN_FILESIZE = Len
    TABLES
      DATA_TAB     = <table>
    EXCEPTIONS
      others       = 1.


EndForm.                    "DownloadBinFile


*&---------------------------------------------------------------------*
*&      Form  Table_To_XString
*&---------------------------------------------------------------------*
Form Table_To_XString
  tables it_Data
  using FileSize type i
        FileType
  changing XStr type XString.

  Data:
    it_StringData type standard table of string.
  Field-symbols:
    <line> type string.

  Clear XStr.

  Case FileType.
    when 'ASC'.
      Perform Table_to_StringTable
        using it_Data[] 'X'
        changing it_StringData[].

      If not it_StringData[] is initial.
        Read table it_StringData[] assigning <line> index 1.

        Call function 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            TEXT     = <line>
            ENCODING = FrontendCodepage
          IMPORTING
            BUFFER   = XStr
          EXCEPTIONS
            others   = 99.

      EndIf.
    when 'BIN'.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          INPUT_LENGTH = FileSize
        IMPORTING
          BUFFER       = XStr
        TABLES
          BINARY_TAB   = it_Data[]
        EXCEPTIONS
          OTHERS       = 99.
  EndCase.
EndForm.                    "Table_To_XString

*&---------------------------------------------------------------------*
*&      Form  ZIP_allowed
*&---------------------------------------------------------------------*
Form ZIP_allowed
  changing subrc.

  Data:
    SEOCLSKEY type SEOCLSKEY.
  SEOCLSKEY-CLSNAME = 'CL_ABAP_ZIP'.
  Call function 'SEO_CLASS_EXISTENCE_CHECK'
    EXPORTING
      CLSKEY = SEOCLSKEY
    EXCEPTIONS
      others = 99.

  If sy-subrc = 0.
    subrc = isOk.
  Else.
    subrc = isError.
  EndIf.
EndForm.                    "ZIP_allowed

*&---------------------------------------------------------------------*
*&      Form  ZIP_Init
*&---------------------------------------------------------------------*
Form ZIP_Init
  changing obj_Zip.

  Data:
    subrc type sy-subrc,
    SEOCLSKEY type SEOCLSKEY.

  Perform ZIP_allowed
    changing subrc.

  check subrc = isOk.
  Create object obj_Zip type ('CL_ABAP_ZIP').
EndForm.                    "Init_Pack

*&---------------------------------------------------------------------*
*&      Form  ZIP_Load
*&---------------------------------------------------------------------*
Form ZIP_Load
  using FileName
  changing FileZIP like FileZIP.

  Clear FileZIP.

  FileZIP-FileName = FileName.
  check not obj_Zip is initial.

  Perform Upload_Bin_File
    using
      FileName
    changing
      FileZIP-Len
      FileZIP-Data.

  Call method obj_Zip->('LOAD')
    EXPORTING
      ZIP = FileZIP-Data.
EndForm.                    "ZIP_Load

*&---------------------------------------------------------------------*
*&      Form  ZIP_Read_File
*&---------------------------------------------------------------------*
Form ZIP_Read_File
  using i_FileName
  changing it_Data type table.

  Data:
    l_FileName type string,
    l_FileSize type i,
    l_XString type xstring,
    l_String type string,
    lt_DataString type standard table of string,
    lt_DataBinary type standard table of char255.

  check not obj_Zip is initial.

  l_FileName = i_FileName.
  Translate l_FileName to upper case.


  Translate l_FileName using '/\'.
  Call method obj_Zip->('GET')
    EXPORTING
      NAME    = l_FileName
    IMPORTING
      CONTENT = l_XString
    EXCEPTIONS
      others  = 99.
  If sy-subrc <> 0.
    Translate l_FileName using '\/'.
    Call method obj_Zip->('GET')
      EXPORTING
        NAME    = l_FileName
      IMPORTING
        CONTENT = l_XString
      EXCEPTIONS
        others  = 99.
  EndIf.
  check sy-subrc = 0.

  Call function 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      BUFFER        = l_XString
    IMPORTING
      OUTPUT_LENGTH = l_FileSize
    TABLES
      BINARY_TAB    = lt_DataBinary[]
    EXCEPTIONS
      others        = 99.
  check sy-subrc = 0.

  Call function 'SCMS_BINARY_TO_STRING'
    EXPORTING
      INPUT_LENGTH = l_FileSize
      ENCODING     = FrontendCodePage
    IMPORTING
      TEXT_BUFFER  = l_String
    TABLES
      BINARY_TAB   = lt_DataBinary[]
    EXCEPTIONS
      others       = 99.
  check sy-subrc = 0.

  Data:
    char_LF(2).
  Concatenate char_0D char_0A into char_LF.
  Split l_String at char_LF into table lt_DataString.

  Data: Error type sy-subrc.
  Perform Parse_Text_File
    tables lt_DataString
           it_Data
    changing Error.
EndForm.                    "ZIP_Read_File

*&---------------------------------------------------------------------*
*&      Form  ZIP_Read_Names
*&---------------------------------------------------------------------*
Form ZIP_Read_Catalog
  using
    value(i_Path) type string
    value(i_Filter) type string
    value(i_FD) type CHAR1
  changing
    ct_FileNames type table. "tt_string.

  Data:
    l_Level type i,
    lt_Names type tt_string with header line,
    lt_FileNames type sorted table of string
      with unique key table_line
      with header line,
    lr_Path   type range of string with header line,
    lr_Filter type range of string with header line.

  check not obj_Zip is initial.

  Split i_Path at '\' into table lt_Names.
  Describe table lt_Names lines l_Level.
  l_Level = l_Level + 1.
  Refresh lt_Names.

  Concatenate i_Path '*' into lr_Path-low.
  lr_Path-sign = 'I'.
  lr_Path-option = 'CP'.
  Append lr_Path.

  lr_Filter-sign = 'I'.
  lr_Filter-option = 'CP'.
  lr_Filter-low = i_Filter.
  Append lr_Filter.

  Field-symbols:
    <files> type any table,
    <line>  type any,
    <field> type any,
    <filename>.
  Data:
    l_Name type string.
  Assign ('OBJ_ZIP->FILES') to <files>.
  check sy-subrc = 0 and <files> is assigned.
  Loop at <files> assigning <line>.
    Assign component 'NAME' of structure <line> to <field>.
    check sy-subrc = 0 and <field> is assigned.
    l_Name = <field>.
    Translate l_Name using '/\'.
    check l_Name in lr_Path[].

    Split l_Name at '\' into table lt_Names.
    Read table lt_Names index l_Level.
    If sy-subrc = 0.
      l_Name = lt_Names.
*      Find '.' in l_Name.
*      If sy-subrc <> 0.
      If not l_Name CS '.'.
        Concatenate l_Name '.' into l_Name.
      EndIf.
      If l_Name in lr_Filter[].
        Collect lt_Names into lt_FileNames.
      EndIf.
    EndIf.
  EndLoop.

  Loop at lt_FileNames.
    Append initial line to ct_FileNames.
    Read table ct_FileNames assigning <filename> index sy-tabix.
    <filename> = lt_FileNames.
  EndLoop.
EndForm.                    "ZIP_Read_Names

*&---------------------------------------------------------------------*
*&      Form  ZIP_Save
*&---------------------------------------------------------------------*
Form ZIP_Save
  using FileZIP like FileZIP.

  check not obj_Zip is initial.

  Call method obj_Zip->('SAVE')
    RECEIVING
      ZIP = FileZIP-Data.

  Perform Download_BIN_File
    using FileZIP-FileName
          FileZIP-Len
    changing FileZIP-Data.
EndForm.                    "ZIP_Save

*&---------------------------------------------------------------------*
*&      Form  ZIP_Pack
*&---------------------------------------------------------------------*
Form ZIP_Pack
  tables
    it_Data
  using
    FullFileName type string
    FileType
    FileSize.

  Data:
*    obj_Zip type ref to object,"CL_ABAP_ZIP,
    FileName type string,
    XStr type xstring.


  check not it_Data[] is initial.



  If "sy-subrc = 0 and
*     obj_Zip is bound.
     not obj_Zip is initial.

    Perform Table_To_XString
      tables it_Data
      using FileSize
            FileType
      changing XStr.

    FileName = FullFileName.
    Translate FileName to upper case.

*    Call function 'TRINT_SPLIT_FILE_AND_PATH'
*      EXPORTING
*        FULL_NAME     = FullFileName
*      IMPORTING
*        STRIPPED_NAME = FileName
*      EXCEPTIONS
*        others        = 99.


    Call method obj_Zip->('DELETE')
      EXPORTING
        NAME   = FileName
      EXCEPTIONS
        others = 99.

    Call method obj_Zip->('ADD')
      EXPORTING
        NAME    = FileName
        CONTENT = XStr
      EXCEPTIONS
        others  = 99.

  EndIf.
EndForm.                    "PackToZIP

*&---------------------------------------------------------------------*
*&      Form  Write_File
*&---------------------------------------------------------------------*
Form Write_File
  using p_FileName
  changing ref_Data.

  Field-symbols:
    <table> type table.

  If p_ZIP is initial.
    Perform Download_File
      using p_FileName
      changing ref_Data.
  Else.
    Assign ref_Data to <table>.

    Perform ZIP_Pack
      tables
        <table>
      using
        p_FileName
        'ASC'
        0.

  EndIf.
EndForm.                    "Write_File

*&---------------------------------------------------------------------*
*&      Form  Write_BIN_File
*&---------------------------------------------------------------------*
Form Write_BIN_File
  using p_FileName
        p_Length
  changing ref_Data.

  Field-symbols:
    <table> type table.

  If p_ZIP is initial.
    Perform Download_Bin_File
      using p_FileName
            p_Length
      changing ref_Data.

  Else.
    Assign ref_Data to <table>.

    Perform ZIP_Pack
      tables
        <table>
      using
        p_FileName
        'BIN'
        p_Length.
  EndIf.

EndForm.                    "Write_BIN_File


*&---------------------------------------------------------------------*
*&      Form  DownloadFile
*&---------------------------------------------------------------------*
Form Download_File
  using p_FileName
  changing ref_Data . "it_Data type table.

  Data:
    TypeData,
*    it_Header type standard table of string
*      with header line,
    it_StringData type standard table of string
      with header line,
    Str type string.
*    FileName type string.
*    FileNameString type string,
*    FileNameRLGRAP type RLGRAP-FILENAME.

  Field-symbols:
    <filename>,
    <data>, <fld>,
    <it_Data> type any table.

*  FileName = p_FileName.
*  If sy-saprl >= '600'.
*    Assign FileNameString to <filename>.
*  Else.
*    Assign FileNameRLGRAP to <filename>.
*  EndIf.

  Data: FileNameRef type ref to data.
  Create data FileNameRef type (FILENAME_type).
  Assign FileNameRef->* to <filename>.

  <filename> = p_FileName.

  Describe field ref_Data type TypeData.
  Case TypeData.
    when 'C'.
      it_StringData = ref_Data.
      Append it_StringData.

      Call function 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME              = <filename>
          WRITE_FIELD_SEPARATOR = ' '
          APPEND                = 'X'
*        DAT_MODE              = 'X' "?????
        TABLES
          DATA_TAB              = it_StringData
        EXCEPTIONS
          others                = 1.

    when 'h'.
      Assign ref_Data to <it_Data>.

      If not <it_Data>[] is initial.
        Perform Table_to_StringTable
          using <it_Data>[] ''
          changing it_StringData[].

        Call function 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME              = <filename>
            WRITE_FIELD_SEPARATOR = ' '
*            APPEND                = 'X'
*        DAT_MODE              = 'X' "?????
          TABLES
            DATA_TAB              = it_StringData
          EXCEPTIONS
            others                = 1.
      EndIf.
  EndCase.
EndForm.                    "DownloadFile

*&---------------------------------------------------------------------*
*&      Form  AppendFile
*&---------------------------------------------------------------------*
*Form AppendFile
*  using FileName
*  changing it_Data type table.
*
*  If not it_Data[] is initial.
*    Call function 'GUI_DOWNLOAD'
*      EXPORTING
*        FILENAME              = FileName
*        WRITE_FIELD_SEPARATOR = 'X'
*        APPEND                = 'X'
*      TABLES
*        DATA_TAB              = it_Data
*      EXCEPTIONS
*        others                = 1.
*  EndIf.
*EndForm.                    "AppendFile

*&---------------------------------------------------------------------*
*&      Form  Parse_Text_File
*&---------------------------------------------------------------------*
Form Parse_Text_File
  tables it_StringData
         it_Data
  changing Error.

  Data:
    DataPoint type ref to data,
    HEADER_ROW type i value 1,
    it_Header type standard table of String
      with header line,
    it_Line type standard table of String
      with header line,
    NextRow type sy-tabix,
    TypeLine.
*    Error type sy-subrc.

  Field-symbols:
    <Fld>,
    <data>.


  Create data DataPoint like line of it_Data.
  Assign DataPoint->* to <data>.
  Describe field <data> type TypeLine.
  If TypeLine = 'g'. "string
    it_Data[] = it_StringData[].
    EXIT. "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  EndIf.

  Read table it_StringData index HEADER_ROW.
  If sy-subrc = 0.
    Split it_StringData at Char_Tab into table it_Header.

    NextRow = HEADER_ROW + 1.
    Loop at it_StringData from NextRow.
      Split it_StringData at Char_Tab into table it_Line.

      Clear <data>.
      Loop at it_Header.
        Assign component it_Header of structure <data> to <Fld>.
        If sy-subrc = 0.
          Read table it_Line index sy-tabix.
          If sy-subrc = 0.
            Data: TypeFld, FldX type xstring.
            Field-symbols: <X> type X.
            Describe field <Fld> type TypeFld.
            Case TypeFld.
*                when 'X'.
*                  FldX = it_Line.
**                  Assign FldX to <X> casting like <Fld>.
*                  <Fld> = FldX. " <X>.
              when others.
                If it_Line is initial.
                  Clear <Fld>.
                Else.
                  <Fld> = it_Line.
                EndIf.
            EndCase.
          EndIf.
        EndIf.
      EndLoop.
      If sy-subrc <> 0.
        <data> = it_StringData.
      EndIf.
      If not <data> is initial or it_Header[] is initial.
        Append <data> to it_Data.
      EndIf.
    EndLoop.
  Else.
    Error = 2.
  EndIf.

EndForm.                    "Parse_Text_File

*&---------------------------------------------------------------------*
*&      Form  Read_File
*&---------------------------------------------------------------------*
Form Read_File
  using p_FileName
  changing it_Data type table.

  If p_Zip is initial.
    Perform Upload_File
      using p_FileName
      changing it_Data.
  Else.
    Perform ZIP_Read_File
      using p_FileName
      changing it_Data.
  EndIf.

EndForm.                    "Read_File


*&---------------------------------------------------------------------*
*&      Form  Upload_Bin_File
*&---------------------------------------------------------------------*
Form Upload_Bin_File
  using
    p_FileName
  changing
    p_Length
    ref_Data.

  Data:
    typeData,
    it_BIN type standard table of W3MIME.
*    FileNameString type string,
*    FileNameRLGRAP type RLGRAP-FILENAME.

  Field-symbols:
    <filename>,
    <table> type table.

  Describe field ref_Data type typeData.

  Case typeData.
    when 'h'.
      Assign ref_Data to <table>.
      check sy-subrc = 0.
    when 'y'.
      Assign it_BIN[] to <table>.
      check sy-subrc = 0.
  EndCase.
  check <table> is assigned.

*  If sy-saprl >= '600'.
*    Assign FileNameString to <filename>.
*  Else.
*    Assign FileNameRLGRAP to <filename>.
*  EndIf.

  Data: FileNameRef type ref to data.
  Create data FileNameRef type (FILENAME_type).
  Assign FileNameRef->* to <filename>.

  <filename> = p_FileName.

  Call function 'GUI_UPLOAD'
    EXPORTING
      FILENAME   = <filename>
      FILETYPE   = 'BIN'
    IMPORTING
      FILELENGTH = p_Length
    TABLES
      DATA_TAB   = <table>
    EXCEPTIONS
      others     = 1.
  check sy-subrc = 0.

  Case typeData.
    when 'h'.
    when 'y'.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          INPUT_LENGTH = p_Length
        IMPORTING
          BUFFER       = ref_Data
        TABLES
          BINARY_TAB   = <table>
        EXCEPTIONS
          OTHERS       = 99.
  EndCase.

EndForm.                    "Upload_Bin_File

*&---------------------------------------------------------------------*
*&      Form  UploadFile
*&---------------------------------------------------------------------*
Form Upload_File
  using p_FileName
  changing it_Data type table.

*  Call function 'GUI_UPLOAD'
*    EXPORTING
*      FILENAME            = FileName
*      HAS_FIELD_SEPARATOR = char_tab
*    TABLES
*      DATA_TAB            = it_Data
*    EXCEPTIONS
*      others              = 1.

  Data:
*    FileName type string,
    it_StringData type standard table of String
      with header line,
    Error type sy-subrc.
*    FileNameString type string,
*    FileNameRLGRAP type RLGRAP-FILENAME.

  Field-symbols:
    <filename>.

*  Field-symbols:
*    <Fld>,
*    <data>.

*  FileName = p_FileName.

  Refresh it_Data.

*  Create data DataPoint like line of it_Data.
*  Assign DataPoint->* to <data>.

*  If sy-saprl >= '600'.
*    Assign FileNameString to <filename>.
*  Else.
*    Assign FileNameRLGRAP to <filename>.
*  EndIf.

  Data: FileNameRef type ref to data.
  Create data FileNameRef type (FILENAME_type).
  Assign FileNameRef->* to <filename>.

  <filename> = p_FileName.

  Call function 'GUI_UPLOAD'
    EXPORTING
      FILENAME = <filename>
*      DAT_MODE = 'X' "???????
    TABLES
      DATA_TAB = it_StringData
    EXCEPTIONS
      OTHERS   = 17.

  If sy-subrc = 0.

    Perform Parse_Text_File
      tables it_StringData
             it_Data
      changing Error.
  Else.
    Error = 1.
  EndIf.

EndForm.                    "UploadFile

*&---------------------------------------------------------------------*
*&      Form  SetClassForObject
*&---------------------------------------------------------------------*
Form SetClassForObject
  using p_TADIR type t_TADIR.

  Call function 'TR_TADIR_INTERFACE'
    EXPORTING
      WI_TADIR_PGMID    = p_TADIR-PGMID
      WI_TADIR_OBJECT   = p_TADIR-OBJECT
      WI_TADIR_DEVCLASS = p_TADIR-DEVCLASS
      WI_TADIR_OBJ_NAME = p_TADIR-OBJ_NAME
      WI_TEST_MODUS     = space
    EXCEPTIONS
      others            = 1.
EndForm.                    "SetClassForObject

*&---------------------------------------------------------------------*
*&      Form  CheckClass
*&---------------------------------------------------------------------*
Form CheckClass
  using p_TADIR type t_TADIR.

  Call function 'TR_DEVCLASS_GET'
    EXPORTING
      IV_DEVCLASS = p_TADIR-DEVClASS
    EXCEPTIONS
      others      = 99.
  If sy-subrc <> 0.
    Message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      into p_TADIR-Message.
    sy-subrc = 1.
  EndIf.
EndForm.                    "CheckClass

*&---------------------------------------------------------------------*
*&      Form  TrimDataPROG
*&---------------------------------------------------------------------*
Form TrimDataPROG
  using p_Version
        Len_ABAP
  changing it_SourceFile type table. " type tt_Source.

  Data:
    Len type i,
    w_Line type line of tt_Source,
    new_Line(256), "type char255,
    it_Words type standard table of char255
      with header line,
    it_NewSourse type tt_Source
      with header line.

  Loop at it_SourceFile into w_Line.
    new_Line = w_Line.

    Perform CheckSourceLineVersion
      using p_Version
      changing new_Line.

    If new_Line(1) = '*' or new_Line(1) = '"'.
      new_Line = new_Line(Len_ABAP).
    EndIf.

    Len = StrLen( new_Line ).
    If Len > Len_ABAP.
      Condense new_Line.
      Len = StrLen( new_Line ).
      If Len > Len_ABAP.
        new_Line = w_Line.
        Len = StrLen( new_Line ).

        Refresh it_Words.
        Split new_Line at space into table it_Words.
        Clear new_Line.
        Clear it_NewSourse.

        Data: Idx type sy-tabix.
        Loop at it_Words.
          Len = StrLen( it_Words ).
          If Len > Len_ABAP.
            Idx = sy-tabix + 1.
            Modify it_Words from it_Words(Len_ABAP).
            Insert it_Words+Len_ABAP into it_Words index Idx.
          EndIf.
        EndLoop.

        Loop at it_Words.
          new_Line = it_NewSourse.
          Concatenate it_NewSourse it_Words into it_NewSourse
            separated by space.
          Len = StrLen( it_NewSourse ).
          If Len > Len_ABAP.
            Append new_Line to it_NewSourse.
            it_NewSourse = it_Words.
          EndIf.
        EndLoop.
        If not it_NewSourse is initial.
          Append it_NewSourse.
        EndIf.
      Else.
        Append new_Line to it_NewSourse.
      EndIf.
    Else.
      Append new_Line to it_NewSourse.
    EndIf.
  EndLoop.
  it_SourceFile[] = it_NewSourse[].
EndForm.                    "TrimDataPROG

*----------------------------------------------------------------------*
*       CLASS ClGridEvent IMPLEMENTATION
*----------------------------------------------------------------------*
Class ClGridEvent implementation.
  Method HandleDblClick.
    Data:
      w_TADIR type t_TADIR,
      Begin of Param,
        PGMID    type E071-PGMID,
        OBJECT   type E071-OBJECT,
        OBJ_NAME type E071-OBJ_NAME,
      End of Param.

    Read table it_TADIR into w_TADIR index e_row-index.

    If sy-subrc = 0.
      Move-corresponding: w_TADIR to Param.

      Call function 'TR_OBJECT_JUMP_TO_TOOL'
        EXPORTING
          IV_PGMID    = Param-PGMID
          IV_OBJECT   = Param-OBJECT
          IV_OBJ_NAME = Param-OBJ_NAME
        EXCEPTIONS
          others      = 1.
    EndIf.
  EndMethod.                    "HandleDblClick
EndClass.                    "ClGridEvent IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Get_Struc
*&---------------------------------------------------------------------*
Form Get_Struc
  tables it_Fields
  using Struc.

  TYPE-POOLS SYDES.

  Data: TD type SYDES_DESC,
        W_TYPES TYPE SYDES_TYPEINFO,
        W_NAMES TYPE SYDES_NAMEINFO,
        w_FieldNames type string,
        CurrentBack type SYDES_TYPEINFO-BACK.

  Describe field Struc into TD.
  Refresh: it_Fields.
  Clear: w_FieldNames. ", FieldNames.
  CurrentBack = -1.
  Loop at TD-TYPES into w_Types where BACK <> 0.
    check w_Types-IDX_NAME <> 0.
    If CurrentBack < 0.
      CurrentBack = w_Types-BACK.
    EndIf.
    check CurrentBack = w_Types-BACK.

    Loop at TD-NAMES into w_Names from w_Types-IDX_NAME.
      Concatenate w_FieldNames w_Names-NAME
        into w_FieldNames.

      If w_Names-CONTINUE <> '*'.
        Append w_FieldNames to it_Fields. "FieldNames.
        Clear: w_FieldNames.
        Exit.
      EndIf.
    EndLoop.
  EndLoop.
EndForm.                    "GetStruc

*&---------------------------------------------------------------------*
*&      Form  GetHeaderString
*&---------------------------------------------------------------------*
Form GetHeaderString
  using Struc
  changing FieldNames type string.

  Data:
    it_Fields type standard table of string
      with header line.

  Perform Get_Struc tables it_Fields using Struc.
  Loop at it_Fields.
    Concatenate FieldNames it_Fields into FieldNames
      separated by char_tab.
  EndLoop.
  Shift FieldNames left deleting leading char_tab.
EndForm.                    "GetHeaderString

*&---------------------------------------------------------------------*
*&      Form  PrepareFunction
*&---------------------------------------------------------------------*
Form PrepareFunction
  changing it_Source type tt_Source.

  Data:
    w_Source type line of tt_Source,
    FirstWord type string,
    Dummy type string,
    NeedDelete,
    MarkCount type i.

  Loop at it_Source into w_Source.
    Condense w_Source.
    Split w_Source at space into FirstWord Dummy.
    Translate FirstWord to upper case.
    If FirstWord = 'FUNCTION'.
      NeedDelete = 'X'.
      MarkCount = MarkCount + 1.
    ElseIf FirstWord CS '"-----------------------------------------'.
      MarkCount = MarkCount + 1.
    EndIf.

    If NeedDelete = 'X'.
      Delete it_Source.
    EndIf.

    If MarkCount >= 3.
      NeedDelete = ''.
    EndIf.

    If FirstWord = 'ENDFUNCTION.' and MarkCount >= 3.
      Delete it_Source.
    EndIf.

  EndLoop.

EndForm.                    "PrepareFunction

*&---------------------------------------------------------------------*
*&      Form  DeleteFUGR
*&---------------------------------------------------------------------*
Form DeleteFUGR
  using NameFUGR type TLIBG-AREA.

  Data:
    it_FuncName type standard table of RS38L_INCL
      with header line.

  Call function 'RS_FUNCTION_POOL_CONTENTS'
    EXPORTING
      FUNCTION_POOL = NameFUGR
    TABLES
      FUNCTAB       = it_FuncName
    EXCEPTIONS
      others        = 1.

  Loop at it_FuncName.
    Call function 'FUNCTION_DELETE'
      EXPORTING
        FUNCNAME = it_FuncName-FUNCNAME
      EXCEPTIONS
        others   = 1.
  EndLoop.

  Call function 'FUNCTION_POOL_DELETE'
    EXPORTING
      POOL   = NameFUGR
    EXCEPTIONS
      others = 1.
EndForm.                    "DeleteFUGR

*&---------------------------------------------------------------------*
*&      Form  CheckObjects
*&---------------------------------------------------------------------*
Form CheckObjects
  changing it_TADIR type tt_TADIR.

  Type-pools: ICON.
  Field-symbols:
    <tadir> type t_TADIR,
    <obj>.
  Data:
    ObjVrs type SVRS2_VERSIONABLE_OBJECT,
    ObjExt type t_Objects_Ext.

  Loop at it_TADIR assigning <tadir>.
    Clear: <tadir>-Message, <tadir>-Icon.

    Case 'X'.
      when p_New.
        Assign component <tadir>-OBJECT of structure ObjVrs to <obj>.
        If sy-subrc <> 0.
          Assign component <tadir>-OBJECT of structure ObjExt to <obj>.
        EndIf.
        If sy-subrc <> 0.
*          <tadir>-Message = '## ##############!'.
*          <tadir>-Icon = ICON_LED_YELLOW. "ICON_FAILURE.
        EndIf.
      when p_Old.
        Read table it_R3TR_Sort with key OBJECT = <tadir>-OBJECT
          binary search.
        If sy-subrc <> 0.
          <tadir>-Message = '## ##############!'.
          <tadir>-Icon = ICON_LED_YELLOW. "ICON_FAILURE.
        Else.
          Case it_R3TR_Sort-ExpImp.
            when space.
            when 'E'.
              <tadir>-Message = '###### #######!'.
              <tadir>-Icon = ICON_ENTER_MORE. "ICON_ARROW_RIGHT.
            when 'S'.
              <tadir>-Message = '###### #######!'.
              <tadir>-Icon = ICON_WARNING. "ICON_HINT.
          EndCase.
        EndIf.
    EndCase.
  EndLoop.
EndForm.                    "CheckObjects

*&---------------------------------------------------------------------*
*&      Form  GetStatusObject
*&---------------------------------------------------------------------*
Form GetStatusObject
  changing p_TADIR type t_TADIR.

  Data:
    w_Class type E071-OBJECT,
    w_Name  type E071-OBJ_NAME.


  w_Class = p_TADIR-OBJECT.
  w_Name  = p_TADIR-OBJ_NAME.

  Call function 'DD_OBJECT_EXISTS'
    EXPORTING
      CLASS  = w_Class
      NAME   = w_Name
      STATE  = ''
    IMPORTING
      EXISTS = p_TADIR-Status
    EXCEPTIONS
      others = 99.

  If sy-subrc <> 0.
    Call function 'CHECK_EXIST'
      EXPORTING
        IV_PGMID    = p_TADIR-PGMID
        IV_OBJECT   = p_TADIR-OBJECT
        IV_OBJ_NAME = p_TADIR-OBJ_NAME
      IMPORTING
        E_EXIST     = p_TADIR-Status
      EXCEPTIONS
        others      = 99.
  EndIf.
* RS_INACTIVE_OBJECTS_IN_OBJECT
EndForm.                    "GetStatusObjects

*&---------------------------------------------------------------------*
*&      Form  GetStatusObjects
*&---------------------------------------------------------------------*
Form GetStatusObjects
  changing it_TADIR type tt_TADIR.
  Field-symbols:
    <tadir> type t_TADIR.

*  Data:
*    w_Class type E071-OBJECT,
*    w_Name  type E071-OBJ_NAME.

  Loop at it_TADIR assigning <tadir>.
*    Select *
*      from DWINACTIV
*      where OBJECT   = <tadir>-OBJECT
*        and OBJ_NAME = <tadir>-OBJ_NAME.

*    w_Class = <tadir>-OBJECT.
*    w_Name  = <tadir>-OBJ_NAME.
*
*    Call function 'DD_OBJECT_EXISTS'
*      EXPORTING
*        CLASS  = w_Class
*        NAME   = w_Name
*        STATE  = ''
*      IMPORTING
*        EXISTS = <tadir>-Status
*      EXCEPTIONS
*        others = 99.
    Perform GetStatusObject
      changing <tadir>.
  EndLoop.
EndForm.                    "GetStatusObjects

*&---------------------------------------------------------------------*
*&      Form  WriteMessage
*&---------------------------------------------------------------------*
Form WriteMessage
  changing p_TADIR type t_TADIR.

  If not sy-msgno is initial.
    Message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      into p_TADIR-Message.

    Clear p_TADIR-ICON.
    Case sy-msgty.
      when 'E'.
        p_TADIR-ICON = ICON_INCOMPLETE.
    EndCase.
  EndIf.
EndForm.                    "WriteMessage

*&---------------------------------------------------------------------*
*&      Form  CheckTRKORR
*&---------------------------------------------------------------------*
Form CheckTRKORR
  using MsgType.

  check p_EXPERT <> 'X'.

  Select single *
    from E070
    where TRKORR = s_TRKORR-low
      and TRSTATUS = 'D'.
  If sy-subrc <> 0.
    Case MsgType.
      when 'E'.
        PutErrorMessage '############ ######!'.
      when others.
        PutMessage '############ ######!'.
    EndCase.
  EndIf.

EndForm.                    "CheckTRKORR

*&---------------------------------------------------------------------*
*&      Form  WriteMessageTo
*&---------------------------------------------------------------------*
Form WriteMessageTo
  using   value(subrc) type sy-subrc
  changing p_TADIR type t_TADIR.

  Case subrc.
    when isOk.
      p_TADIR-ICON = ICON_LED_GREEN.
      Case 'X'.
        when p_Export.
          p_TADIR-Message = '####### - ##!'.
*        p_TADIR-ICON = ICON_TRANSFER.
        when p_Import.
          p_TADIR-Message = '###### - ##!'.
*        p_TADIR-ICON = ICON_PDIR_BACK.
      EndCase.
    when isNothing.
      p_TADIR-ICON = ICON_LED_YELLOW.
      p_TADIR-Message = '## ##########!'.
    when others. "Else.
      If not sy-msgno is initial.
        Message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          into p_TADIR-Message.

        Clear p_TADIR-ICON.
        If sy-msgty = 'E'.
*        p_TADIR-ICON = ICON_MESSAGE_ERROR.
          p_TADIR-ICON = ICON_LED_RED.
        EndIf.
      Else.
        p_TADIR-Message = '######!'.
        p_TADIR-ICON = ICON_LED_RED.
      EndIf.
  EndCase. "EndIf.
EndForm.                    "WriteMessageTo

*&---------------------------------------------------------------------*
*&      Form  GetMainProgName
*&---------------------------------------------------------------------*
Form GetMainProgName
  using w_TADIR type t_TADIR
  changing MainProgName
           MainFileName.

  Data:
    FUGR_GROUP type RS38L-AREA,
    TYPE_NAME  type TRDIR-NAME,
    FUGR_PROGNAME_GROUP type TRDIR-NAME,
    TYPE_PROGNAME type TRDIR-NAME.

  Call function 'RS_TADIR_TO_PROGNAME'
    EXPORTING
      OBJECT        = w_TADIR-OBJECT
      OBJ_NAME      = w_TADIR-OBJ_NAME
    IMPORTING
      PROGNAME      = MainProgName
    EXCEPTIONS
      error_message = 1
      others        = 99.

  Case w_TADIR-OBJECT.
    when 'PROG'.
      MainFileName = 'REPORT'.
    when 'TYPE'.
      MainFileName = 'TYPE-POOL'.
    when 'FUGR'.
      MainFileName = MainProgName.
    when 'CLAS'.
      MainFileName = 'CLASS-POOL'.
    when others.
      MainFileName = MainProgName.
  EndCase.

EndForm.                    "GetMainProgName

*&---------------------------------------------------------------------*
*&      Form  CHECKSOURCELINEVERSION
*&---------------------------------------------------------------------*
Form CheckSourceLineVersion
  using p_Version
  changing new_Line.

  Data:
    isComment(1),
    Cmnt(10),
    Vrs(3),
    Len type i,
    Pos type i,
    Pls type i,
    Sgn.

  isComment = new_Line.

  If isComment = '*'.
    If new_Line cs '*#EC V'.
      Pos = sy-fdpos.
      Len = Strlen( new_Line ).
      Cmnt = new_Line+Pos.
      Sgn = Cmnt+6(1).
      Vrs = Cmnt+7.

      If Vrs <> space and Pos = 0.
        Case Sgn.
          when '#'.
            If p_Version >= Vrs.
              Pls = Strlen( new_Line ) - 10.
              new_Line(1) = '"'.
              Shift new_Line(Len) by Pls places right circular.
            EndIf.
          when '<'.
            If p_Version < Vrs.
              Pls = Strlen( new_Line ) - 10.
              new_Line(1) = '"'.
              Shift new_Line(Len) by Pls places right circular.
            EndIf.
        EndCase.
      EndIf.
    EndIf.
  Else.
    If new_Line cs '"#EC V'.
      Pos = sy-fdpos.
      Len = Strlen( new_Line ).
      Cmnt = new_Line+Pos.
      Sgn = Cmnt+6(1).
      Vrs = Cmnt+7.

      If Vrs <> space and Pos > 0.
        Case Sgn.
          when '#'.
            If p_Version < Vrs.
              Pls = Pos.
              Shift new_Line(Len) by Pls places left circular.
              new_Line(1) = '*'.
            EndIf.
          when '<'.
            If p_Version >= Vrs.
              Pls = Pos.
              Shift new_Line(Len) by Pls places left circular.
              new_Line(1) = '*'.
            EndIf.
        EndCase.
      EndIf.
    EndIf.
  EndIf.
EndForm.                    "CheckSourceLineVersion

*&---------------------------------------------------------------------*
*&      Form  ConvertLIMUtoR3TR
*&---------------------------------------------------------------------*
Form ConvertLIMUtoR3TR
  changing it_TADIR type t_TADIR.

  Data:
    TROBJ_NAME type TROBJ_NAME,
    w_E071 type E071,
    w_TADIR type TADIR.

  If it_TADIR-PGMID = 'LIMU'.
    If  0 = 1. "SVERS-VERSION(3) > '640'.
      TROBJ_NAME = it_TADIR-OBJ_NAME.

      Call function 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
        EXPORTING
          P_LIMU_OBJTYPE = it_TADIR-OBJECT
          P_LIMU_OBJNAME = TROBJ_NAME
        IMPORTING
          P_R3TR_OBJTYPE = it_TADIR-OBJECT
          P_R3TR_OBJNAME = TROBJ_NAME
        EXCEPTIONS
          others         = 99.
      If sy-subrc = 0.
        it_TADIR-PGMID = 'R3TR'.
        it_TADIR-OBJ_NAME = TROBJ_NAME.
      EndIf.
    Else.
      w_E071-PGMID    = it_TADIR-PGMID.
      w_E071-OBJECT   = it_TADIR-OBJECT.
      w_E071-OBJ_NAME = it_TADIR-OBJ_NAME.

      Call function 'TR_CHECK_TYPE'
        EXPORTING
          WI_E071  = w_E071
        IMPORTING
          WE_TADIR = w_TADIR
        EXCEPTIONS
          others   = 99.

      If sy-subrc = 0 and
         not w_TADIR-OBJECT is initial and
         not w_TADIR-OBJ_NAME is initial.

        it_TADIR-PGMID    = w_TADIR-PGMID.
        it_TADIR-OBJECT   = w_TADIR-OBJECT.
        it_TADIR-OBJ_NAME = w_TADIR-OBJ_NAME.
      EndIf.

    EndIf.
  EndIf.

EndForm.                    "ConvertLIMUtoR3TR

************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  Import_Limu_CINC
*&---------------------------------------------------------------------*
Form Import_Limu_CINC
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    ClassName type SEOCLSKEY,
    it_SOURCE type SEOP_SOURCE with header line,
    it_TRDIR  type TRDIR occurs 0,
    it_Methods type SEOP_METHODS_W_SOURCE.

  Field-symbols:
*    <CINC> type SVRS2_CINC,
    <ABAPTEXT> type table,
    <TRDIR> type table,
    <line>.

  subrc = isError.

*  Assign Struc_LIMU to <CINC>.

  Assign component 'ABAPTEXT' of structure Struc_LIMU to <ABAPTEXT>.
  check sy-subrc = 0.

  Loop at <ABAPTEXT> assigning <line>.
    it_SOURCE = <line>.
    Append it_SOURCE.
  EndLoop.

  ClassName = Obj_Name.
*  Replace all occurrences of  '=' in ClassName with space .
  Translate ClassName using '= '.

  Case Obj_Name+30.
    when 'CCDEF'.
      Call function 'SEO_CLASS_CREATE_SOURCE'
        EXPORTING
          CLASS                 = ClassName
          LOCALS_DEF            = it_SOURCE[]
          SUPPRESS_INDEX_UPDATE = 'X'
          VERSION               = 0
          SOURCE                = it_Methods "###### ### 4.6C
        EXCEPTIONS
          others                = 99.
    when 'CCIMP'.
      Call function 'SEO_CLASS_CREATE_SOURCE'
        EXPORTING
          CLASS                 = ClassName
          LOCALS_IMP            = it_SOURCE[]
          SUPPRESS_INDEX_UPDATE = 'X'
          VERSION               = 0
          SOURCE                = it_Methods "###### ### 4.6C
        EXCEPTIONS
          others                = 99.
    when 'CCMAC'.
      Call function 'SEO_CLASS_CREATE_SOURCE'
        EXPORTING
          CLASS                 = ClassName
          LOCALS_MAC            = it_SOURCE[]
          SUPPRESS_INDEX_UPDATE = 'X'
          VERSION               = 0
          SOURCE                = it_Methods "###### ### 4.6C
        EXCEPTIONS
          others                = 99.
  EndCase.

  If sy-subrc = 0.
    Assign component 'TRDIR' of structure Struc_LIMU to <TRDIR>.
    If sy-subrc = 0.
      Perform Table_Corresponding tables <TRDIR> it_TRDIR.
      Perform Save_TRDIR tables it_TRDIR.
    EndIf.
    subrc = isOk.
  EndIf.
EndForm.                    "Import_Limu_CINC

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_CPRI
*&---------------------------------------------------------------------*
Form Import_Limu_CPRI
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    it_Dummy type char1 occurs 0.

  Field-symbols:
*    <CPRI> type SVRS2_CPRI.
    <attr>  type table,
    <meth>  type table,
    <event> type table,
    <type>  type table,
    <param> type table,
    <excep> type table,
    <alias> type table,
    <reps>  type table,
    <trdir> type table.
*
  subrc = isError.


*  Assign Struc_LIMU to <CPRI>.
  Assign it_Dummy[] to:
    <attr>,
    <meth>,
    <event>,
    <type>,
    <param>,
    <excep>,
    <alias>,
    <reps>,
    <trdir>.
  Assign component 'ATTR'  of structure Struc_LIMU to <attr>.
  Assign component 'METH'  of structure Struc_LIMU to <meth>.
  Assign component 'EVENT' of structure Struc_LIMU to <event>.
  Assign component 'TYPE'  of structure Struc_LIMU to <type>.
  Assign component 'PARAM' of structure Struc_LIMU to <param>.
  Assign component 'EXCEP' of structure Struc_LIMU to <excep>.
  Assign component 'ALIAS' of structure Struc_LIMU to <alias>.
  Assign component 'REPS'  of structure Struc_LIMU to <reps>.
  Assign component 'TRDIR' of structure Struc_LIMU to <trdir>.

  Data: it_REDEF type standard table of SEOREDEF.

  Perform SEO_CLASS_ADD_COMPONENTS
    tables
      <attr>
      <meth>
      it_REDEF
      <event>
      <type>
      <param>
      <excep>
      <alias>
      <reps>
      <trdir>
    using
      Obj_Name
      'CPRI'.

  subrc = isOk.
EndForm.                    "Import_Limu_CPRI


*&---------------------------------------------------------------------*
*&      Form  SEO_CLASS_ADD_COMPONENTS
*&---------------------------------------------------------------------*
Form SEO_CLASS_ADD_COMPONENTS
  tables
    it_ATTR
    it_METH
    it_REDEF
    it_EVENT
    it_TYPE
    it_PARAM
    it_EXCEP
    it_ALIAS
    it_REPS
    it_TRDIR
  using
    ObjName
    ObjType.

  Data:
    ClassName type SEOCLSKEY,
    ATTR type VSEOATTRIB occurs 0,
    METH type VSEOMETHOD occurs 0,
    REDEF type SEOR_REDEFINITIONS_R,
    EVENT type VSEOEVENT occurs 0,
    TYPE type VSEOTYPE occurs 0,
    PARAM type VSEOPARAM occurs 0,
    EXCEP type VSEOEXCEP occurs 0.
  Data:
    w_REDEF like line of REDEF[].

  Field-symbols:
    <attr>,
    <meth>,
    <redef>,
    <event>,
    <type>,
    <param>,
    <excep>,
    <alias>.

  ClassName = ObjName.

  Perform Table_Corresponding tables:
    it_ATTR  ATTR,
    it_METH  METH,
    it_REDEF REDEF,
    it_EVENT EVENT,
    it_TYPE  TYPE,
    it_PARAM PARAM,
    it_EXCEP EXCEP.
*    it_ALIAS ALIAS,
*    it_REPS  REPS,
*    it_TRDIR TRDIR.

  Delete ATTR  where CLSNAME <> ClassName.
  Delete METH  where CLSNAME <> ClassName.
  Delete REDEF where CLSNAME <> ClassName.
  Delete EVENT where CLSNAME <> ClassName.
  Delete TYPE  where CLSNAME <> ClassName.
  Delete PARAM where CLSNAME <> ClassName.
  Delete EXCEP where CLSNAME <> ClassName.

  If not REDEF[] is initial.
    Data:
      INHKEY type SEORELKEY.

*    REDEF[] = it_REDEF[].

    Read table REDEF[] into w_REDEF index 1.
    INHKEY-CLSNAME = ClassName.
    INHKEY-REFCLSNAME = w_REDEF-REFCLSNAME.

    Call function 'SEO_INHERITANC_ADD_REDEF'
      EXPORTING
        INHKEY        = INHKEY
        REDEFINITIONS = REDEF[]
      EXCEPTIONS
        others        = 99.
  EndIf.

  Call function 'SEO_BUFFER_REFRESH'
    EXPORTING
      cifkey  = ClassName
      version = 0.


  Loop at ATTR assigning <attr>. "it_ATTR
*    Move-corresponding: <attr> to ATTR.
    Call function 'SEO_ATTRIBUTE_CREATE_F_DATA'
      EXPORTING
        SAVE               = ''
        SUPPRESS_LOG_ENTRY = 'X'
      CHANGING
        ATTRIBUTE          = <attr> "ATTR
      EXCEPTIONS
        others             = 99.
  EndLoop.

  Loop at METH assigning <meth>. "it_METH
*    Move-corresponding: <meth> to METH.
    Call function 'SEO_METHOD_CREATE_F_DATA'
      EXPORTING
        SAVE               = ''
        SUPPRESS_LOG_ENTRY = 'X'
      CHANGING
        METHOD             = <meth> "METH
      EXCEPTIONS
        others             = 99.
  EndLoop.

  Loop at EVENT assigning <event>. "it_EVENT
*    Move-corresponding: <event> to EVENT.
    Call function 'SEO_EVENT_CREATE_F_DATA'
      EXPORTING
        SAVE               = ''
        SUPPRESS_LOG_ENTRY = 'X'
      CHANGING
        EVENT              = <event> "EVENT
      EXCEPTIONS
        others             = 99.
  EndLoop.

  Loop at TYPE assigning <type>. "it_TYPE
*    Move-corresponding: <type> to TYPE.
    Call function 'SEO_TYPE_CREATE_F_DATA'
      EXPORTING
        SAVE   = ''
      CHANGING
        TYPE   = <type> "TYPE
      EXCEPTIONS
        others = 99.
  EndLoop.

  Loop at PARAM assigning <param>. "it_PARAM
*    Move-corresponding: <param> to PARAM.
    Call function 'SEO_PARAMETER_CREATE_F_DATA'
      EXPORTING
        SAVE               = ''
        SUPPRESS_LOG_ENTRY = 'X'
      CHANGING
        PARAMETER          = <param> "PARAM
      EXCEPTIONS
        others             = 99.
  EndLoop.

  Loop at EXCEP assigning <excep>. "it_EXCEP
*    Move-corresponding: <excep> to EXCEP.
    Call function 'SEO_EXCEPTION_CREATE_F_DATA'
      EXPORTING
        SAVE               = ''
        SUPPRESS_LOG_ENTRY = 'X'
      CHANGING
        EXCEP              = <excep> "EXCEP
      EXCEPTIONS
        others             = 99.
  EndLoop.

*  Call function 'SEO_CLIF_SAVE_ALL'
*    EXPORTING
*      cifkey                        = ClassName
*      SUPPRESS_MODIFICATION_SUPPORT = 'X'
*      suppress_corr                 = 'X'
*    EXCEPTIONS
*      db_error                      = 1
*      others                        = 2.
  Data:
    CORRNR type E071-TRKORR,
    GENFLAG.
  CORRNR = s_TRKORR-low.
  GENFLAG = 'X'.

  CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
    EXPORTING
      CIFKEY                              = ClassName
   NO_SECTIONS                         = 'X' "SEOX_FALSE
*   SECTIONS_ONLY                       = 'X' "SEOX_FALSE
      SUPPRESS_CORR                       = 'X'"#EC V#600
      SUPPRESS_MODIFICATION_SUPPORT       = 'X'"#EC V#600
      SUPPRESS_INDEX_UPDATE               = 'X'"#EC V#700
   CHANGING
     CORRNR                              = CORRNR
*    DEVCLASS                            =
     GENFLAG                             = GENFLAG
   EXCEPTIONS
     NOT_EXISTING                        = 1
     NOTHING_TO_DO                       = 2
     ACCESS_ERROR                        = 3
     DB_ERROR                            = 4
     ERROR_IN_CODE_GENERATION            = 5
     OTHERS                              = 6.
  .

  If sy-subrc = 0 and
     not it_TRDIR[] is initial.

    Data:
      LIMU_Reps type SVRS2_REPS,
      w_TRDIR type TRDIR.

    LIMU_Reps-ABAPTEXT[] = it_REPS[].
    LIMU_Reps-TRDIR[] = it_TRDIR[].
    Read table LIMU_Reps-TRDIR[] into w_TRDIR index 1.

    Data: subrc type sy-subrc.
*    Perform Import_LIMU_REPS
*      using
*        w_TRDIR-Name
*        LIMU_Reps
*        subrc.
    INSERT REPORT w_TRDIR-Name
      FROM LIMU_Reps-ABAPTEXT
*      EXTENSION TYPE srext_ext_class_private
      STATE 'I'.

    If subrc = isOk.
      Data: OBJ_NAME type E071-OBJ_NAME.
      OBJ_NAME = ClassName-CLSNAME.

      Call function 'RS_INSERT_INTO_WORKING_AREA'
        EXPORTING
          OBJECT       = ObjType "LIKE E071-OBJECT
          OBJ_NAME     = OBJ_NAME "LIKE E071-OBJ_NAME
          DELETED_FLAG = space "TYPE DWINACTIV-DELET_FLAG.
        EXCEPTIONS
          others       = 99.
    EndIf.
  EndIf.
EndForm.                    "SEO_CLASS_ADD_COMPONENTS

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_CPRO
*&---------------------------------------------------------------------*
Form Import_Limu_CPRO
  using
    Obj_Name
    Struc_LIMU
    subrc.

*  Data:
*    ClassName type SEOCLSKEY,
*    it_ATTR type SEOO_ATTRIBUTES_R " standard table of VSEOATTRIB,
*      with header line,
*    it_METH type SEOO_METHODS_R
*      with header line,
*    it_EVENT type SEOO_EVENTS_R
*      with header line,
*    it_TYPE type SEOO_TYPES_R
*      with header line,
*    it_PARAM type SEOS_PARAMETERS_R
*      with header line,
*    it_EXCEP type SEOS_EXCEPTIONS_R
*      with header line,
*    it_ALIAS type SEOO_ALIASES_R
*      with header line,
*    it_REPS type SEOP_SOURCE.
*
*
  Data:
    it_Dummy type char1 occurs 0.

  Field-symbols:
*    <CPRO> type SVRS2_CPRO.
    <attr>  type table,
    <meth>  type table,
    <redef> type table,
    <event> type table,
    <type>  type table,
    <param> type table,
    <excep> type table,
    <alias> type table,
    <reps>  type table,
    <trdir> type table.
*
  subrc = isError.

*  Assign Struc_LIMU to <CPRO>.
  Assign it_Dummy[] to:
    <attr>,
    <meth>,
    <redef>,
    <event>,
    <type>,
    <param>,
    <excep>,
    <alias>,
    <reps>,
    <trdir>.
  Assign component 'ATTR'  of structure Struc_LIMU to <attr>.
  Assign component 'METH'  of structure Struc_LIMU to <meth>.
  Assign component 'REDEF' of structure Struc_LIMU to <redef>.
  Assign component 'EVENT' of structure Struc_LIMU to <event>.
  Assign component 'TYPE'  of structure Struc_LIMU to <type>.
  Assign component 'PARAM' of structure Struc_LIMU to <param>.
  Assign component 'EXCEP' of structure Struc_LIMU to <excep>.
  Assign component 'ALIAS' of structure Struc_LIMU to <alias>.
  Assign component 'REPS'  of structure Struc_LIMU to <reps>.
  Assign component 'TRDIR' of structure Struc_LIMU to <trdir>.

*  ClassName = Obj_Name.
*
**  Perform TrimDataPROG
**    using 72
**    changing <CPRO>-REPS.
**
**  it_REPS[] = <CPRO>-REPS[].
*
*  Loop at <CPRO>-ATTR assigning <attr>.
*    Move-corresponding: <attr> to it_ATTR.
*    Append it_ATTR.
*  EndLoop.
*
*  Loop at <CPRO>-METH assigning <meth>.
*    Move-corresponding: <meth> to it_METH.
*    Append it_METH.
*  EndLoop.
*
*  Loop at <CPRO>-TYPE assigning <type>.
*    Move-corresponding: <type> to it_TYPE.
*    Append it_TYPE.
*  EndLoop.
*
*  Loop at <CPRO>-PARAM assigning <param>.
*    Move-corresponding: <param> to it_PARAM.
*    Append it_PARAM.
*  EndLoop.
*
*  Loop at <CPRO>-EXCEP assigning <excep>.
*    Move-corresponding: <excep> to it_EXCEP.
*    Append it_EXCEP.
*  EndLoop.
*
*  Loop at <CPRO>-ALIAS assigning <alias>.
*    Move-corresponding: <alias> to it_ALIAS.
*    Append it_ALIAS.
*  EndLoop.
*
*  Call function 'SEO_CLASS_ADD_COMPONENTS'
*    EXPORTING
*      CLSKEY          = ClassName
*      AUTHORITY_CHECK = ''
*    CHANGING
*      ATTRIBUTES      = it_ATTR[]
*      METHODS         = it_METH[]
*      TYPES           = it_TYPE[]
**      TYPE_SOURCE     = it_REPS[]
*      PARAMETERS      = it_PARAM[]
*      EXCEPS          = it_EXCEP[]
*      ALIASES         = it_ALIAS[]
*    exceptions
*      others = 99.
*
*  If sy-subrc = 0 and
*     not <CPRO>-TRDIR[] is initial.
*
*    Data:
*      LIMU_Reps type SVRS2_REPS,
*      w_TRDIR type TRDIR.
*
*    LIMU_Reps-ABAPTEXT[] = <CPRO>-REPS[].
*    LIMU_Reps-TRDIR[] = <CPRO>-TRDIR[].
*    Read table LIMU_Reps-TRDIR[] into w_TRDIR index 1.
*
*    Perform Import_LIMU_REPS
*      using
*        w_TRDIR-Name
*        LIMU_Reps.
*  EndIf.

  Perform SEO_CLASS_ADD_COMPONENTS
    tables
      <attr>
      <meth>
      <redef>
      <event>
      <type>
      <param>
      <excep>
      <alias>
      <reps>
      <trdir>
*      <CPRO>-ATTR
*      <CPRO>-METH
*      <CPRO>-REDEF
*      <CPRO>-EVENT
*      <CPRO>-TYPE
*      <CPRO>-PARAM
*      <CPRO>-EXCEP
*      <CPRO>-ALIAS
*      <CPRO>-REPS
*      <CPRO>-TRDIR
    using
      Obj_Name
      'CPRO'.
  subrc = isOk.
EndForm.                    "Import_Limu_CPRO


*&---------------------------------------------------------------------*
*&      Form  Import_Limu_CPUB
*&---------------------------------------------------------------------*
Form Import_Limu_CPUB
  using
    Obj_Name
    Struc_LIMU
    subrc.

*  Data:
*    ClassName type SEOCLSKEY,
*    it_ATTR type SEOO_ATTRIBUTES_R " standard table of VSEOATTRIB,
*      with header line,
*    it_METH type SEOO_METHODS_R
*      with header line,
*    it_EVENT type SEOO_EVENTS_R
*      with header line,
*    it_TYPE type SEOO_TYPES_R
*      with header line,
*    it_PARAM type SEOS_PARAMETERS_R
*      with header line,
*    it_EXCEP type SEOS_EXCEPTIONS_R
*      with header line,
*    it_ALIAS type SEOO_ALIASES_R
*      with header line.
*
*
  Data:
    it_Dummy type char1 occurs 0.

  Field-symbols:
*    <CPUB> type SVRS2_CPUB.
    <mtrel> type table,
    <attr>  type table,
    <meth>  type table,
    <redef> type table,
    <event> type table,
    <type>  type table,
    <param> type table,
    <excep> type table,
    <alias> type table,
    <reps>  type table,
    <trdir> type table.
*
  subrc = isError.
*  Assign Struc_LIMU to <CPUB>.
  Assign it_Dummy[] to:
    <mtrel>,
    <attr>,
    <meth>,
    <redef>,
    <event>,
    <type>,
    <param>,
    <excep>,
    <alias>,
    <reps>,
    <trdir>.
  Assign component 'MTREL' of structure Struc_LIMU to <mtrel>.
  Assign component 'ATTR'  of structure Struc_LIMU to <attr>.
  Assign component 'METH'  of structure Struc_LIMU to <meth>.
  Assign component 'REDEF' of structure Struc_LIMU to <redef>.
  Assign component 'EVENT' of structure Struc_LIMU to <event>.
  Assign component 'TYPE'  of structure Struc_LIMU to <type>.
  Assign component 'PARAM' of structure Struc_LIMU to <param>.
  Assign component 'EXCEP' of structure Struc_LIMU to <excep>.
  Assign component 'ALIAS' of structure Struc_LIMU to <alias>.
  Assign component 'REPS'  of structure Struc_LIMU to <reps>.
  Assign component 'TRDIR' of structure Struc_LIMU to <trdir>.

*  ClassName = Obj_Name.
*
*  Loop at <CPUB>-ATTR assigning <attr>.
*    Move-corresponding: <attr> to it_ATTR.
*    Append it_ATTR.
*  EndLoop.
*
*  Loop at <CPUB>-METH assigning <meth>.
*    Move-corresponding: <meth> to it_METH.
*    Append it_METH.
*  EndLoop.
*
*  Loop at <CPUB>-TYPE assigning <type>.
*    Move-corresponding: <type> to it_TYPE.
*    Append it_TYPE.
*  EndLoop.
*
*  Loop at <CPUB>-PARAM assigning <param>.
*    Move-corresponding: <param> to it_PARAM.
*    Append it_PARAM.
*  EndLoop.
*
*  Loop at <CPUB>-EXCEP assigning <excep>.
*    Move-corresponding: <excep> to it_EXCEP.
*    Append it_EXCEP.
*  EndLoop.
*
*  Loop at <CPUB>-ALIAS assigning <alias>.
*    Move-corresponding: <alias> to it_ALIAS.
*    Append it_ALIAS.
*  EndLoop.
*
*  Call function 'SEO_CLASS_ADD_COMPONENTS'
*    exporting
*      CLSKEY = ClassName
*      AUTHORITY_CHECK = ''
*    changing
*      ATTRIBUTES = it_ATTR[]
*      METHODS = it_METH[]
*      TYPES   = it_TYPE[]
**      TYPE_SOURCE
*      PARAMETERS = it_PARAM[]
*      EXCEPS = it_EXCEP[]
*      ALIASES = it_ALIAS[]
*    exceptions
*      others = 99.
*
*  If sy-subrc = 0 and
*     not <CPUB>-TRDIR[] is initial.
*
*    Data:
*      LIMU_Reps type SVRS2_REPS,
*      w_TRDIR type TRDIR.
*
*    LIMU_Reps-ABAPTEXT[] = <CPUB>-REPS[].
*    LIMU_Reps-TRDIR[] = <CPUB>-TRDIR[].
*    Read table LIMU_Reps-TRDIR[] into w_TRDIR index 1.
*
*    Perform Import_LIMU_REPS
*      using
*        w_TRDIR-Name
*        LIMU_Reps.
*  EndIf.
  Data:
*    it_MTREL type VSEOCOMPRI occurs 0
*      with header line.
    it_IMPL type VSEOIMPLEM occurs 0
      with header line.

  Perform Table_Corresponding tables:
    <MTREL>[] it_IMPL[].
  Loop at it_IMPL.
    Exit.
  EndLoop.

  If sy-subrc = 0.
*  CALL FUNCTION 'SEO_COMPRISING_CREATE_F_DATA'
*    EXPORTING
*      SAVE       = 'X'
*    CHANGING
*      COMPRISING = it_MTREL
*    EXCEPTIONS
*      others     = 99.
*      Data: SEOCLSKEY type SEOCLSKEY.
*      SEOCLSKEY = it_INHER-ClsName.
    Call function 'SEO_IMPLEMENTG_CREATE_F_DATA'
      changing
*          CLSKEY      = SEOCLSKEY
        IMPLEMENTING = it_IMPL
      EXCEPTIONS
        others      = 99.
  EndIf.

  Perform SEO_CLASS_ADD_COMPONENTS
    tables
      <attr>
      <meth>
      <redef>
      <event>
      <type>
      <param>
      <excep>
      <alias>
      <reps>
      <trdir>
*      <CPUB>-ATTR
*      <CPUB>-METH
*      <CPUB>-REDEF
*      <CPUB>-EVENT
*      <CPUB>-TYPE
*      <CPUB>-PARAM
*      <CPUB>-EXCEP
*      <CPUB>-ALIAS
*      <CPUB>-REPS
*      <CPUB>-TRDIR
    using
      Obj_Name
      'CPUB'.
  subrc = isOk.
EndForm.                    "Import_Limu_CPUB



*&---------------------------------------------------------------------*
*&      Form  Import_Limu_METH
*&---------------------------------------------------------------------*
Form Import_Limu_METH
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    ClassName type SEOCLSKEY,
    it_Methods type SEOP_METHODS_W_SOURCE
      with header line,
    it_Source type tt_Source,
    it_TRDIR type TRDIR occurs 0
      with header line,
    w_Source type line of seop_source,
    Cnt type i,
    StrLower(255),
    w_VSEOMETHOD type VSEOMETHOD.

  Field-symbols:
    <METH> type SVRS2_METH,
    <ABAPTEXT>,
    <TRDIR> type TRDIR.

  subrc = isError.

  Assign Struc_LIMU to <METH>.

  it_Source[] = <METH>-ABAPTEXT[].
  it_TRDIR[] = <METH>-TRDIR[].
  Describe table it_Source[] lines Cnt.
*  check Cnt > 2.

  Perform TrimDataPROG
    using sy-saprl
          72
    changing it_Source[].

  Loop at it_Source assigning <ABAPTEXT>.
    StrLower = <ABAPTEXT>.
    Translate StrLower to lower case.
    Condense StrLower.
    check StrLower(7) <> 'method '.
    check StrLower(10) <> 'endmethod.'.

    w_SOURCE = <ABAPTEXT>.
    Append w_SOURCE to it_Methods-SOURCE.
  EndLoop.

  it_Methods-cpdname = Obj_Name+30.

  Append it_Methods.

  ClassName = Obj_Name.
*  Replace all occurrences of  '=' in ClassName with space .
  Translate ClassName using '= '.

*  w_VSEOMETHOD-CLSNAME = ClassName.
*  w_VSEOMETHOD-CMPNAME = it_Methods-cpdname.
*
*  Call function 'SEO_METHOD_CREATE_F_DATA'
*    CHANGING
*      METHOD = w_VSEOMETHOD.

  Call function 'SEO_CLASS_CREATE_SOURCE'
    EXPORTING
      CLASS                 = ClassName
      SOURCE                = it_Methods[]
      SUPPRESS_INDEX_UPDATE = 'X'
      VERSION               = 0
    EXCEPTIONS
      others                = 99.
  If sy-subrc = 0.
    Data:
      MTDKEY type SEOCPDKEY,
      PROGNAME type PROGRAMM.

    Read table it_TRDIR assigning <TRDIR> index 1.

    MTDKEY-CLSNAME = ClassName.
    MTDKEY-CPDNAME = Obj_Name+30.

    Call function 'SEO_METHOD_GET_INCLUDE_BY_NAME'
      EXPORTING
        MTDKEY   = MTDKEY
      IMPORTING
        PROGNAME = PROGNAME
      EXCEPTIONS
        others   = 99.
    If sy-subrc = 0.
      <TRDIR>-NAME = PROGNAME.
      Perform Save_TRDIR tables it_TRDIR.
*      Delete report PROGNAME state 'I'.
    EndIf.

    subrc = isOk.
  EndIf.

EndForm.                    "Import_Limu_METH

*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_CLAS
*&---------------------------------------------------------------------*
Form Import_R3TR_CLAS
  using w_TADIR type t_TADIR
        struc_CLAS type t_R3TR_CLAS
        subrc.
  subrc = isError.

  Data:
    ClassName type SEOCLSKEY,
    Class type VSEOCLASS.

  ClassName = w_TADIR-Obj_Name.
  Class-CLSNAME = w_TADIR-Obj_Name.

  CALL FUNCTION 'SEO_BUFFER_REFRESH'
    EXPORTING
      cifkey  = ClassName
      version = 0. "seoc_version_inactive.

  CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
    EXPORTING
      CLSKEY        = ClassName
    EXCEPTIONS
      NOT_SPECIFIED = 1
      NOT_EXISTING  = 2
      IS_INTERFACE  = 3
      NO_TEXT       = 4
      INCONSISTENT  = 5
      OTHERS        = 6.

  Case sy-subrc.
    when 2. "NOT_EXISTING
      sy-subrc = 0.
    when others. "####### #######
      CALL FUNCTION 'SEO_CLASS_DELETE_W_DEPS'
        EXPORTING
          CLSKEY             = ClassName
*   SAVE               = SEOX_TRUE
       EXCEPTIONS
*   NOT_EXISTING       = 1
*   IS_INTERFACE       = 2
*   NOT_DELETED        = 3
*   DB_ERROR           = 4
         OTHERS             = 5.

  EndCase.

  check sy-subrc = 0.

  CALL FUNCTION 'SEO_CLASS_CREATE_F_DATA'
    EXPORTING
      save   = '' "'X'
    CHANGING
      class  = Class
    EXCEPTIONS
      others = 99.
  If sy-subrc = 0.
    Data:
*      CORRNR type TRKORR,
      GENFLAG.
    GENFLAG = 'X'.
    CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
      EXPORTING
        CIFKEY                        = ClassName
        SUPPRESS_CORR                 = 'X'"#EC V#600
        SUPPRESS_MODIFICATION_SUPPORT = 'X'"#EC V#600
        SUPPRESS_INDEX_UPDATE         = 'X'"#EC V#700
   NO_SECTIONS                         = 'X' "SEOX_FALSE
*   SECTIONS_ONLY                       = 'X' "SEOX_FALSE
      CHANGING
        GENFLAG                       = GENFLAG
      EXCEPTIONS
        OTHERS                        = 99.
    If sy-subrc = 0.
      subrc = isOk.
    EndIf.
  EndIf.
EndForm.                    "Import_R3TR_CLAS

*&---------------------------------------------------------------------*
*&      Form  Import_Limu_CLSD
*&---------------------------------------------------------------------*
Form Import_Limu_CLSD
  using
    Obj_Name
    Struc_LIMU
    subrc.

  Data:
    ClassName type SEOCLSKEY,
    Class type VSEOCLASS,
    it_MTREL type VSEOCOMPRI occurs 0
      with header line,
    it_INHER type SEOR_INHERITANCE_W occurs 0
      with header line,
    it_TRDIR type TRDIR occurs 0.

  Field-symbols:
*    <CLSD> type SVRS2_CLSD,
    <CLASS> type table,
    <MTREL> type table,
    <TRDIR> type table.
*    <mtrel>.

  subrc = isError.

*  Assign Struc_LIMU to <CLSD>.
  Assign component 'CLASS' of structure Struc_LIMU to <CLASS>.
  Assign component 'MTREL' of structure Struc_LIMU to <MTREL>.
  Assign component 'TRDIR' of structure Struc_LIMU to <TRDIR>.

  ClassName = Obj_Name.

  CALL FUNCTION 'SEO_BUFFER_REFRESH'
    EXPORTING
      cifkey  = ClassName
      version = 0. "seoc_version_inactive.

  Read table <CLASS> into Class index 1. "<CLSD>-CLASS

  CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
    EXPORTING
      CLSKEY        = ClassName
    EXCEPTIONS
      NOT_SPECIFIED = 1
      NOT_EXISTING  = 2
      IS_INTERFACE  = 3
      NO_TEXT       = 4
      INCONSISTENT  = 5
      OTHERS        = 6.

*  Case sy-subrc.
*    when 2. "NOT_EXISTING
*      sy-subrc = 0.
*    when others. "####### #######
*      CALL FUNCTION 'SEO_CLASS_DELETE_W_DEPS'
*        EXPORTING
*          CLSKEY             = ClassName
**   SAVE               = SEOX_TRUE
*       EXCEPTIONS
**   NOT_EXISTING       = 1
**   IS_INTERFACE       = 2
**   NOT_DELETED        = 3
**   DB_ERROR           = 4
*         OTHERS             = 5.
*
*  EndCase.

  check sy-subrc = 0.

*  CALL FUNCTION 'SEO_CLASS_CREATE_F_DATA'
  Call function 'SEO_CLASS_CHANGE_F_DATA'
    EXPORTING
      save   = ''
    CHANGING
      class  = Class
    EXCEPTIONS
      others = 99.
  If sy-subrc = 0.
    If not <MTREL>[] is initial. "<CLSD>-MTREL[]
*      Read table <CLSD>-MTREL assigning <mtrel> index 1.
*      Move-corresponding: <mtrel> to MTREL.
*      Move-corresponding: <mtrel> to INHER.
      Perform Table_Corresponding tables:
        <MTREL>[] it_MTREL[],
        <MTREL>[] it_INHER[].
      Read table it_MTREL index 1.
      Read table it_INHER index 1.

*      CALL FUNCTION 'SEO_COMPRISING_CREATE_F_DATA'
*        EXPORTING
*          SAVE       = 'X'
*        CHANGING
*          COMPRISING = it_MTREL
*        EXCEPTIONS
*          others     = 99.
      Call function 'SEO_INHERITANC_CREATE'
        EXPORTING
          CLSKEY      = ClassName
          INHERITANCE = it_INHER
        EXCEPTIONS
          others      = 99.
    EndIf.

    Data:
*      CORRNR type TRKORR,
      GENFLAG.
    GENFLAG = 'X'.
    CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
      EXPORTING
        CIFKEY                        = ClassName
        SUPPRESS_CORR                 = 'X'"#EC V#600
        SUPPRESS_MODIFICATION_SUPPORT = 'X'"#EC V#600
        SUPPRESS_INDEX_UPDATE         = 'X'"#EC V#700
   NO_SECTIONS                         = 'X' "SEOX_FALSE
*   SECTIONS_ONLY                       = 'X' "SEOX_FALSE
      CHANGING
*        CORRNR                        = CORRNR
        GENFLAG                       = GENFLAG
      EXCEPTIONS
        OTHERS                        = 99.

  EndIf.

  If sy-subrc = 0.
    Perform Table_Corresponding tables:
      <TRDIR>[] it_TRDIR[].
    Perform Save_TRDIR tables it_TRDIR[]. "<CLSD>-TRDIR.
    subrc = isOk.
  EndIf.

EndForm.                    "Import_Limu_CLSD


*&---------------------------------------------------------------------*
*&      Form  Import_R3TR_INTF
*&---------------------------------------------------------------------*
Form Import_R3TR_INTF
  using w_TADIR type t_TADIR
        struc_INTF "type SVRS2_INTF
        subrc.
  Data:
*    w_INTF like line of struc_INTF-INTF[],
    struc_REPS type SVRS2_REPS,
    w_TRDIR type TRDIR,
    subrcElem type sy-subrc,
    subrcReps type sy-subrc.

  Field-symbols:
    <INTF_TYPE> type table,
    <INTF>  type table,
    <COMPR> type table,
    <ATTR>  type table,
    <METH>  type table,
    <EVENT> type table,
    <PARAM> type table,
    <EXCEP> type table,
    <ALIAS> type table,
    <TYPEP> type table,
    <REPS>  type table,
    <PREPS> type table,
    <TRDIR> type table,
    <lineINTF>.

  Data:
    it_COMPRISINGS TYPE SEOR_COMPRISINGS_R,
    it_ATTRIBUTES  TYPE SEOO_ATTRIBUTES_R,
    it_METHODS     TYPE SEOO_METHODS_R,
    it_EVENTS      TYPE SEOO_EVENTS_R,
    it_PARAMETERS  TYPE SEOS_PARAMETERS_R,
    it_EXCEPS      TYPE SEOS_EXCEPTIONS_R,
    it_ALIASES     TYPE SEOO_ALIASES_R,
    it_TYPEPUSAGES TYPE SEOT_TYPEPUSAGES_R,
    it_CLSDEFERRDS TYPE SEOT_CLSDEFERRDS_R,
    it_INTDEFERRDS TYPE SEOT_INTDEFERRDS_R,
    it_TYPES       TYPE SEOO_TYPES_R.

  subrc = isError.

  Assign component 'INTF'  of structure struc_INTF to <INTF>.
  Assign component 'COMPR' of structure struc_INTF to <COMPR>.
  Assign component 'ATTR'  of structure struc_INTF to <ATTR>.
  Assign component 'METH'  of structure struc_INTF to <METH>.
  Assign component 'EVENT' of structure struc_INTF to <EVENT>.
  Assign component 'PARAM' of structure struc_INTF to <PARAM>.
  Assign component 'EXCEP' of structure struc_INTF to <EXCEP>.
  Assign component 'ALIAS' of structure struc_INTF to <ALIAS>.
  Assign component 'TYPEP' of structure struc_INTF to <TYPEP>.
  Assign component 'REPS'  of structure struc_INTF to <REPS>.
  Assign component 'PREPS' of structure struc_INTF to <PREPS>.
  Assign component 'TRDIR' of structure struc_INTF to <TRDIR>.


  check not <INTF>[] is initial.
  Read table <INTF>[] assigning <lineINTF> index 1. "into w_INTF

  Perform Table_Corresponding tables:
    <COMPR>[] it_COMPRISINGS[],
    <ATTR>[]  it_ATTRIBUTES[],
    <METH>[]  it_METHODS[],
    <EVENT>[] it_EVENTS[],
    <PARAM>[] it_PARAMETERS[],
    <EXCEP>[] it_EXCEPS[],
    <ALIAS>[] it_ALIASES[],
    <TYPEP>[] it_TYPEPUSAGES[].

  Assign component 'TYPE' of structure struc_INTF to <INTF_TYPE>.
  If sy-subrc = 0.
    Perform Table_Corresponding tables:
      <INTF_TYPE> it_TYPES[].
  EndIf.

  Delete it_ATTRIBUTES[]
    where CLSNAME <> w_TADIR-OBJ_NAME.
  Delete it_TYPES[]
    where CLSNAME <> w_TADIR-OBJ_NAME.

*  If not it_TYPES[] is initial.
*    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
*      EXPORTING
*        OVERWRITE                          = 'X'
*      CHANGING
*        INTERFACE                          = w_INTF
*        COMPRISINGS                        = it_COMPRISINGS[]
*        ATTRIBUTES                         = it_ATTRIBUTES[]
*        METHODS                            = it_METHODS[]
*        EVENTS                             = it_EVENTS[]
*        PARAMETERS                         = it_PARAMETERS[]
*        EXCEPS                             = it_EXCEPS[]
*        ALIASES                            = it_ALIASES[]
*        TYPEPUSAGES                        = it_TYPEPUSAGES[]
**      CLSDEFERRDS                        = struc_INTF-
**      INTDEFERRDS                        = struc_INTF-
*        TYPES                              = it_TYPES[]
*      EXCEPTIONS
*        OTHERS                             = 99.
*  Else.
*    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
*      EXPORTING
*        OVERWRITE                          = 'X'
*      CHANGING
*        INTERFACE                          = w_INTF
*        COMPRISINGS                        = it_COMPRISINGS[]
*        ATTRIBUTES                         = it_ATTRIBUTES[]
*        METHODS                            = it_METHODS[]
*        EVENTS                             = it_EVENTS[]
*        PARAMETERS                         = it_PARAMETERS[]
*        EXCEPS                             = it_EXCEPS[]
*        ALIASES                            = it_ALIASES[]
*        TYPEPUSAGES                        = it_TYPEPUSAGES[]
**      CLSDEFERRDS                        = struc_INTF-
**      INTDEFERRDS                        = struc_INTF-
**      TYPES                              = it_TYPES[]
*      EXCEPTIONS
*        OTHERS                             = 99.
*  EndIf.

  Data:
    intkey TYPE seoclskey.

  intkey-CLSNAME = w_TADIR-OBJ_NAME.

  CALL FUNCTION 'SEO_BUFFER_REFRESH'
    EXPORTING
      cifkey  = intkey
      version = seoc_version_inactive.

  CALL FUNCTION 'SEO_INTERFACE_DELETE_W_DEPS'
    EXPORTING
      intkey       = intkey
      save         = seox_false
    EXCEPTIONS
      not_existing = 1
      is_class     = 2
      not_deleted  = 3
      db_error     = 4
      OTHERS       = 5.

  CALL FUNCTION 'SEO_INTERFACE_CREATE_F_DATA'
    EXPORTING
      save      = seox_false
    CHANGING
      interface = <lineINTF> "w_INTF
    EXCEPTIONS
      existing  = 1
      is_class  = 2
      db_error  = 4
      OTHERS    = 5.


  check sy-subrc = 0.

  subrcElem = isOk.

  Field-symbols:
    <comprising> like line of it_COMPRISINGS[].

* create comprisings
  IF NOT it_COMPRISINGS[] IS INITIAL.
    LOOP AT it_COMPRISINGS[] ASSIGNING <comprising>.
      CALL FUNCTION 'SEO_COMPRISING_CREATE_F_DATA'
        EXPORTING
          save       = seox_false
        CHANGING
          comprising = <comprising>
        EXCEPTIONS
          OTHERS     = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <attribute> like line of it_ATTRIBUTES[].
* create attributes
  IF NOT it_ATTRIBUTES[] IS INITIAL.
    LOOP AT it_ATTRIBUTES[] ASSIGNING <attribute>.
      <attribute>-exposure = seoc_exposure_public.
      CALL FUNCTION 'SEO_ATTRIBUTE_CREATE_F_DATA'
        EXPORTING
          save      = seox_false
        CHANGING
          attribute = <attribute>
        EXCEPTIONS
          OTHERS    = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <method> like line of it_METHODS[].
* create methods
  IF NOT it_METHODS[] IS INITIAL.
    LOOP AT it_METHODS[] ASSIGNING <method>.
      <method>-exposure = seoc_exposure_public.
      <method>-redefin  = seox_false.                 "MS 20070108
      CALL FUNCTION 'SEO_METHOD_CREATE_F_DATA'
        EXPORTING
          save   = seox_false
        CHANGING
          method = <method>
        EXCEPTIONS
          OTHERS = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <lineEVENT> like line of it_EVENTS[].
* create events
  IF NOT it_EVENTS[] IS INITIAL.
    LOOP AT it_EVENTS[] ASSIGNING <lineEVENT>.
      <lineEVENT>-exposure = seoc_exposure_public.
      CALL FUNCTION 'SEO_EVENT_CREATE_F_DATA'
        EXPORTING
          save   = seox_false
        CHANGING
          event  = <lineEVENT>
        EXCEPTIONS
          OTHERS = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.


  Field-symbols:
    <type> like line of it_TYPES[].
* create types
  IF NOT it_TYPES[] IS INITIAL.
    LOOP AT it_TYPES[] ASSIGNING <type>.
      <type>-exposure = seoc_exposure_public.
      CALL FUNCTION 'SEO_TYPE_CREATE_F_DATA'
        EXPORTING
          save   = seox_false
        CHANGING
          type   = <type>
        EXCEPTIONS
          OTHERS = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <parameter> like line of it_PARAMETERS[].
* create parameters
  IF NOT it_PARAMETERS[] IS INITIAL.
    LOOP AT it_PARAMETERS[] ASSIGNING <parameter>.
      CALL FUNCTION 'SEO_PARAMETER_CREATE_F_DATA'
        EXPORTING
          save      = seox_false
        CHANGING
          parameter = <parameter>
        EXCEPTIONS
          OTHERS    = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <exception> like line of it_EXCEPS[].
* create exceptions
  IF NOT it_EXCEPS[] IS INITIAL.
    LOOP AT it_EXCEPS[] ASSIGNING <exception>.
      CALL FUNCTION 'SEO_EXCEPTION_CREATE_F_DATA'
        EXPORTING
          save   = seox_false
        CHANGING
          excep  = <exception>
        EXCEPTIONS
          OTHERS = 1.
      If sy-subrc <> 0.
        subrcElem = isError.
        Exit.
      EndIf.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <lineALIAS> like line of it_ALIASES[].
  Data:
    attribute TYPE vseoattrib,
    method TYPE vseomethod,
    event TYPE vseoevent.

* create aliases
  IF NOT it_ALIASES[] IS INITIAL.
    LOOP AT it_ALIASES[] ASSIGNING <lineALIAS>.
      IF <lineALIAS>-cmptype = seoo_cmptype_attribute.
        MOVE-CORRESPONDING <lineALIAS> TO attribute.
        attribute-alias = seox_true.
        attribute-state = seoc_state_implemented.
        CALL FUNCTION 'SEO_ATTRIBUTE_CREATE_F_DATA'
          EXPORTING
            save      = seox_false
          CHANGING
            attribute = attribute
          EXCEPTIONS
            OTHERS    = 1.
        If sy-subrc <> 0.
          subrcElem = isError.
          Exit.
        EndIf.
      ELSEIF <lineALIAS>-cmptype = seoo_cmptype_method.
        MOVE-CORRESPONDING <lineALIAS> TO method.
        method-alias = seox_true.
        method-state = seoc_state_implemented.
        CALL FUNCTION 'SEO_METHOD_CREATE_F_DATA'
          EXPORTING
            save   = seox_false
          CHANGING
            method = method
          EXCEPTIONS
            OTHERS = 1.
        If sy-subrc <> 0.
          subrcElem = isError.
          Exit.
        EndIf.
      ELSEIF <lineALIAS>-cmptype = seoo_cmptype_event.
        MOVE-CORRESPONDING <lineALIAS> TO event.
        event-alias = seox_true.
        event-state = seoc_state_implemented.
        CALL FUNCTION 'SEO_EVENT_CREATE_F_DATA'
          EXPORTING
            save   = seox_false
          CHANGING
            event  = event
          EXCEPTIONS
            OTHERS = 1.
        If sy-subrc <> 0.
          subrcElem = isError.
          Exit.
        EndIf.
      ENDIF.
    ENDLOOP.
  ENDIF.
  check subrcElem = isOk.

  Field-symbols:
    <tpu> like line of it_TYPEPUSAGES[].
  LOOP AT it_TYPEPUSAGES[] ASSIGNING <tpu>.
    CALL FUNCTION 'SEO_TYPEPUSAGE_CREATE_F_DATA'
      EXPORTING
        save          = seox_false
      CHANGING
        typepusage    = <tpu>
      EXCEPTIONS
        existing      = 1
        is_clsdeferrd = 2
        is_intdeferrd = 3
        not_created   = 4
        db_error      = 5
        OTHERS        = 6.
    If sy-subrc <> 0.
      subrcElem = isError.
      Exit.
    EndIf.
  ENDLOOP.
  check subrcElem = isOk.

  Data:
    GENFLAG value 'X'.
* save
  CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
    EXPORTING
      cifkey                        = intkey
      suppress_refactoring_entries  = 'X'"#EC V#600
      suppress_modification_support = seox_true"#EC V#600
      SUPPRESS_CORR                 = 'X'"#EC V#600
    CHANGING
      GENFLAG                       = GENFLAG
    EXCEPTIONS
      db_error                      = 1
      OTHERS                        = 2.
  check sy-subrc = 0.


  subrcReps = isOk.
  Clear struc_REPS.
  If not <PREPS>[] is initial.
    subrcReps = isError.
    struc_REPS-ABAPTEXT[] = <PREPS>[].

    Clear w_TRDIR.
    Loop at <TRDIR> into w_TRDIR.
      If w_TRDIR-NAME+30 <> 'IP'.
        Clear w_TRDIR.
      Else.
        Exit.
      EndIf.
    EndLoop.
    check not w_TRDIR is initial.

    Append w_TRDIR to struc_REPS-TRDIR[].
    Perform Import_Limu_Reps
      using w_TRDIR-NAME
            struc_REPS
            subrcReps.
  EndIf.
  check subrcReps = isOk.

  subrcReps = isOk.
  Clear struc_REPS.
  If not <REPS>[] is initial.
    subrcReps = isError.
    struc_REPS-ABAPTEXT[] = <REPS>[].

    Clear w_TRDIR.
    Loop at <TRDIR>[] into w_TRDIR.
      If w_TRDIR-NAME+30 <> 'IU'.
        Clear w_TRDIR.
      Else.
        Exit.
      EndIf.
    EndLoop.
    check not w_TRDIR is initial.

    Append w_TRDIR to struc_REPS-TRDIR[].
    Perform Import_Limu_Reps
      using w_TRDIR-NAME
            struc_REPS
            subrcReps.
  EndIf.
  check subrcReps = isOk.

  subrc = isOk.

EndForm.                    "Import_R3TR_INTF

*&---------------------------------------------------------------------*
*&      Form  SELF_ADAPTING
*&---------------------------------------------------------------------*
Form Self_Adapting.
  Data:
    cprog type sy-cprog,
    it_Source type char255 occurs 0,
    it_Adapted type char255 occurs 0,
    Len type i,
    Ofs type i,
    AbapLine type line of SVRS2_REPS-ABAPTEXT,
    Begin of CheckLine,
      SYSID type sy-SYSID,
      DBSYS type sy-DBSYS,
      OPSYS type sy-OPSYS,
      HOST  type sy-HOST,
      SAPRL type sy-SAPRL,
      SUM value 'X',
    End of CheckLine,
    CheckSum type X.
  Field-symbols:
    <X> type X.

  Clear AbapLine with 'A'.
  cprog = sy-cprog.
  Len = Strlen( AbapLine ).

  Read report cprog into it_Source[].
  it_Adapted[] = it_Source[].

  Perform TrimDataPROG
    using sy-saprl
          Len
    changing it_Adapted[].

  If it_Adapted[] <> it_Source[].
    Move-corresponding: sy to CheckLine.
    Len = Strlen( CheckLine ).
    Do Len times.
      Assign CheckLine+Ofs(1) to <X> casting.
      CheckSum = CheckSum bit-xor <X>.
      Ofs = Ofs + 1.
    EndDo.

    If CheckSum <> '55'. "## ### #######
*      break VIKTOROV.
      Insert report cprog from it_Adapted[].
      If sy-subrc = 0.
        Generate report cprog.
        If sy-subrc = 0.
          commit work and wait.
          Data: StrMess type string.
          Concatenate sy-cprog '############ ### ######' sy-saprl
            into StrMess
            separated by space.
          PutInfoMessage StrMess.

          Leave program.
*          Wait up to 2 seconds.
*          Submit (cprog) via selection-screen.
        EndIf.
      EndIf.
    EndIf.
  EndIf.
EndForm.                    " SELF_ADAPTING
