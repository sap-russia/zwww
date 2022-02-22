FUNCTION-POOL ZWWWFORM.                     "MESSAGE-ID ..

TYPE-POOLS OLE2.
Type-pools SBDST.

TYPES:
  BEGIN OF T_STRUC,
    FIELD_NAME TYPE ZWWW_FIELD_CATALOG-FIELD_NAME,
    TYPE(1),
    FIND_TEXT    TYPE ZWWW_VALUES-FIND_TEXT,
    FIELD_HEADER TYPE ZWWW_VALUES-VALUE,
    NO_OUT       TYPE ZWWW_FIELD_CATALOG-NO_OUT,
    NO_ZERO      type ZWWW_FIELD_CATALOG-NO_ZERO,
    NO_CONVEXT   type ZWWW_FIELD_CATALOG-NO_CONVEXT,
    EDIT_MASK    type LVC_EDTMSK,
    COL_POS      type I,
    tabix        type sy-tabix,
  END OF T_STRUC,

  TT_STRUC TYPE STANDARD TABLE OF T_STRUC,

  tt_Values  type standard table of ZWWW_VALUES,
  tt_Files   type standard table of ZWWW_FILES,
  t_FileType type char50,
  t_Ext type char255,

  tt_Strings type standard table of string
    with key table_line.

*Types:
*  Begin of t_Conv,
*    Chr,
*    Str(4),
*  End of t_Conv,
*  tt_Conv type standard table of t_Conv.
*
*Data:
*  it_Conv type tt_Conv with header line.

Types:
  Begin of t_TT,
    Name type string,
  End of t_TT,
  tt_TT type standard table of t_TT
    with key Name.

Types:
  Begin of t_Bookmark,
    Name(50) type c,
    PosBeg  type i,
    PosEnd  type i,
    Status,         "D - ########
    it_New type tt_Strings,
  End of t_Bookmark,
  tt_Bookmarks type standard table of t_Bookmark.
Types:
    tt_Sort_Val type standard table of ZWWW_VALUES.


Constants:
  X_TAB(1) TYPE X VALUE '09',
  X_0D(1)  TYPE X VALUE '0D',
  X_0A(1)  TYPE X VALUE '0A',
  X_FFFE(2) type X value 'FFFE'.



DATA:
  Char_Ascii(255) value '',
*  CHAR_CONV type C,
  CHAR_01  type C,
  CHAR_TAB type C,
  CHAR_0D  TYPE C,
  CHAR_0A  TYPE C,
  CHAR_LF(2) type C,
  Char_FFFE  type C,
  DECIMAL_POINT.


Type-pools:                                                 "#EC V#700
  ABAP.                                                     "#EC V#700
Data:                                                       "#EC V#700
  FrontendCodepage type ABAP_ENCODING.                      "#EC V#700
Data:
  UseUnicode  value '',
  isWebDynpro value '',
  isWebGUI    value '',
  isJavaGUI   value '',
  isMAC       value '',
  isLINUX     value '',
  isBackGrnd  value '',
  isBSP       value '',
*  useJAR      value '',
  Begin of FileJAR,
    Data type xstring,
    Len  type i,
    MimeType type string,
  End of FileJAR.

Data:
  FILENAME_type type FUPARAREF-STRUCTURE value 'STRING'.
Types:
  Begin of t_Parameters,
    Clear_Temp,
    Conv_Exit(1),
  End   of t_Parameters.
Data:
  gs_Parameters type t_Parameters.

  Data:
    g_Timlo         type string,
    g_TimloAppendix type string.

Load-of-Program.
  Select single STRUCTURE
    into FILENAME_type
    from FUPARAREF
    where FUNCNAME = 'GUI_DOWNLOAD'
      and PARAMETER = 'FILENAME'.

  Perform AssignCharX.

  Perform CheckGui.

  Perform Get_Default_Parameters
    changing gs_Parameters.

  Perform GetCodepage                                       "#EC V#700
    using UseUnicode                                        "#EC V#700
    changing FrontendCodepage.                              "#EC V#700

Define Free_object.
  If &1-Handle > 0.
    Free object &1.
    Clear &1.
  EndIf.
End-of-Definition.
