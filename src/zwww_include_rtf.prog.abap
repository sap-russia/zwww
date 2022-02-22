*&---------------------------------------------------------------------*
*&  Include           ZWWW_INCLUDE_RTF
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ConvertLineRTF
*&---------------------------------------------------------------------*
*Form ConvertLineRTF
*  changing StrLine type string.
*
*  Data:
*    C value '#',
*    Hex(2),
*    X(1) type x.
*  Field-symbols:
*     <X>, <EI>, <K>.
*
*  If StrLine(2) = '\'''.
*    Assign CHAR_01 to <EI> type 'X'.
*    Assign CHAR_CONV to <K> type 'X'.
*    Assign C to <X> type 'X'.
*    Hex = StrLine+2(2).
*    Translate Hex to upper case.
*    X = Hex.
*    <X> = X * <EI> + <K>.
*    If C >= '#'.
*      Concatenate  C StrLine+4 into StrLine.
*    EndIf.
*  EndIf.
*EndForm.                    "ConvertLineRTF

Form Hex_To_Char
  changing StrHex type string.
  Data:
    Hex(2),
    X(1) type x.
  If StrHex(2) = '\'''.
    Hex = StrHex+2(2).
    Translate Hex to upper case.
    X = Hex.
    If X <> '00'.
      X = X - 1.
      StrHex = Char_Ascii+X(1).
    Else.
      StrHex = ' '.
    EndIf.
  EndIf.
EndForm.                    "HexToChar

*&---------------------------------------------------------------------*
*&      Form  ConvertFileRTF
*&---------------------------------------------------------------------*
Form Convert_File_RTF
  changing FILE_TEXT type string.

  Data:
    i type i,
    FlLen type i,
    HexStr type string,
    ResultStr type string.

  FlLen = StrLen( FILE_TEXT ) - 3.
  check FlLen > 0.
  Do.
    HexStr = FILE_TEXT+i(4).
    Case HexStr(1).
      when '\'.
        If HexStr+1(1) = ''''.
          Perform Hex_To_Char changing HexStr. "ConvertLineRTF
          Concatenate ResultStr HexStr into ResultStr.
          i = i + 3.
        Else.
          Concatenate ResultStr HexStr(1) into ResultStr.
        EndIf.
      when CHAR_0A or CHAR_0D.
      when others.
        Concatenate ResultStr HexStr(1) into ResultStr.
    EndCase.
    i = i + 1.
    If i >= FlLen.
      Concatenate ResultStr HexStr+1 into ResultStr.
      Exit.
    EndIf.
  EndDo.
  FILE_TEXT = ResultStr.
EndForm.                    "ConvertFileRTF

*&---------------------------------------------------------------------*
*&      Form  ParseBookmarks
*&---------------------------------------------------------------------*
Form ParseBookmarks
  tables
    it_All type tt_Strings
  changing
    it_Bookmarks type tt_Bookmarks.

  Data:
    it_Res type match_result_tab
      with header line,
    w_Bookmark type t_Bookmark,
    idx type i,
    lvl type i.
  Field-symbols:
    <w_Bookmark> type t_Bookmark.

* ######## ## #### ########
  Clear w_Bookmark.
  w_Bookmark-PosBeg = 1.
  Describe table it_All lines w_Bookmark-PosEnd.
  Insert w_Bookmark into table it_Bookmarks.

* #### ###### ########
  Find all occurrences of '\bkmkstart'
    in table it_All
    results it_Res[]
    ignoring case.
  Loop at it_Res.
    Find first occurrence
      of regex '^[^\\]'
      in table it_All from it_Res-LINE
      match line idx.
    If sy-subrc = 0.
      Read table it_All index idx.
      Condense it_All.
      Translate it_All to upper case.
      w_Bookmark-Name = it_All.

      "# ###### ########
      idx = it_Res-LINE.
      lvl = 2. "## ### ##. #####
      While idx > 1 and lvl > 0.
        idx = idx - 1.
        Read table it_All index idx.
        If sy-subrc = 0.
          Case it_All.
            when '{'.
              lvl = lvl - 1.
            when '}'.
              lvl = lvl + 1.
          EndCase.
        EndIf.
      EndWhile.
      w_Bookmark-PosBeg = idx + 1.
      Append w_Bookmark to it_Bookmarks.
    EndIf.
  EndLoop.

  Sort it_Bookmarks by Name.

* #### ##### ########
  Refresh it_Res.
  Find all occurrences of '\bkmkend'
    in table it_All
    results it_Res[]
    ignoring case.
  Loop at it_Res.
    Find first occurrence
      of regex '^[^\\]'
      in table it_All from it_Res-LINE
      match line idx.
    If sy-subrc = 0.
      Read table it_All index idx.
      Condense it_All.
      Translate it_All to upper case.
      Read table it_Bookmarks assigning <w_Bookmark>
        with key Name = it_All
        binary search.
      If sy-subrc = 0.
*        "# ##### ########
*        Read table it_All index idx.
*        Condense it_All.
*        Translate it_All to upper case.
*
*        Find first occurrence
*          of regex '^\}'
*          in table it_All from idx
*          match line idx.
*
*        <w_Bookmark>-PosEnd = idx.
        lvl = 2. "## ### ##. #####
        While idx > 1 and lvl > 0.
          idx = idx - 1.
          Read table it_All index idx.
          If sy-subrc = 0.
            Case it_All.
              when '{'.
                lvl = lvl - 1.
              when '}'.
                lvl = lvl + 1.
            EndCase.
          EndIf.
        EndWhile.
        <w_Bookmark>-PosEnd = idx.

      EndIf.
    EndIf.
  EndLoop.
EndForm.                    "ParseBookmarks

*&---------------------------------------------------------------------*
*&      Form  ParseFileRTF
*&---------------------------------------------------------------------*
Form Parse_File_RTF
  tables
    it_All type tt_Strings
  using
    FILE_TEXT type string
  changing
    it_Bookmarks type tt_Bookmarks.

  Data:
    i type i,
    pos type i,
    len type i,
    C,
    FlLen type i.

  Perform Convert_File_RTF
    changing FILE_TEXT.

  FlLen = StrLen( FILE_TEXT ).

  Do FlLen times.
    C = FILE_TEXT+i(1).

    If  len = 1 and FILE_TEXT+pos(1) = '\'.
      len = len + 1.
    Else.
      Case C.
        when '{' or '}'.
          If len > 0.
            it_All = FILE_TEXT+pos(len).
            Append it_All.
          EndIf.
          it_All = FILE_TEXT+i(1).
          Append it_All.
          Clear it_All.
          pos = i + 1.
          len = 0.
        when '\'.
          If len > 0.
            it_All = FILE_TEXT+pos(len).
            Append it_All.
          EndIf.
          pos = i.
          len = 1.
        when space.
          If len > 1 and FILE_TEXT+pos(1) = '\'.
            it_All = FILE_TEXT+pos(len).
            Append it_All.
            pos = i + 1.
            len = 0.
          Else.
            len = len + 1.
          EndIf.
        when others.
          len = len + 1.
      EndCase.
    EndIf.
    i = i + 1.
  EndDo.

  If len <> 0.
    it_All = FILE_TEXT+pos(len).
    Append it_All.
  EndIf.

  Perform ParseBookmarks
    tables   it_All
    changing it_Bookmarks.

EndForm.                    "ParseFileRTF

Form Char_to_Hex
  changing Str type string.
  Data:
    Len type i,
    Ofs type i,
    Res type string,
    Chr.

  Len = Strlen( Str ).
  Do Len times.
    Ofs = sy-index - 1.
    Chr = Str+Ofs(1).
    If Chr CS '0123456789'.
    ElseIf Chr CS 'abcdefghijklmnopqrstuvwxyz'.
    ElseIf Chr CS 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    ElseIf Chr CS '\{}'.
    Else.

    EndIf.
  EndDo.
EndForm.

*&---------------------------------------------------------------------*
*&      Form  BuildFileRTF
*&---------------------------------------------------------------------*
Form Build_File_RTF
  tables
    it_All type tt_Strings
  changing
    FILE_TEXT type string.

  Data:
    LenShift  type i,
    NewShift  type i,
    PrevCmd,
    w_All type String.

  Loop at it_All.
    w_All = it_All.

    If 0 = 1. "### ####### #######
      Case it_All.
        when '{'.
          NewShift = LenShift + 2.
        when '}'.
          LenShift = LenShift - 2.
          NewShift = LenShift.
      EndCase.
      Concatenate w_All CHAR_0D CHAR_0A into w_All.
      Shift w_All by LenShift places right.
    EndIf.

    If Strlen( it_All ) > 0.
      Case it_All(1).
        when '\' or '{' or '}'.
          Concatenate FILE_TEXT w_All
            into FILE_TEXT.
        when others.
          If PrevCmd = '\'.
            Concatenate FILE_TEXT w_All
              into FILE_TEXT separated by space.
          Else.
            Concatenate FILE_TEXT w_All
              into FILE_TEXT.
          EndIf.
      EndCase.
      PrevCmd = it_All(1).
    EndIf.

    LenShift = NewShift.
  EndLoop.
EndForm.                    "BuildFileRTF

*&---------------------------------------------------------------------*
*&      Form  FillRTFForm
*&---------------------------------------------------------------------*
Form Fill_RTF_Form
  using it_Val type table
        Doc    type OLE2_OBJECT
        DEBUG_MODE
        FILE_TEXT type string.

  Types:
    Begin of t_NewAll,
      Bookmark(50),
    End of t_NewAll.

  Data:
    it_All type standard table of string with header line,
    it_Buf type standard table of string with header line,
    it_New type standard table of string with header line,
    it_Bookmarks type tt_Bookmarks.

  Data:
    Var_Error type sy-subrc,
    Idx type i.
  Field-symbols:
    <it_SortVal> type tt_Values,
    <it_Values> type ZWWW_VALUES,
    <it_Bookmarks> type t_Bookmark,
    <copyBookmark> type t_Bookmark.

  Perform Parse_File_RTF
    tables it_All
    using FILE_TEXT
    changing it_Bookmarks[].

  Clear FILE_TEXT.

*  Refresh it_Val[]. "### ####### #######
**************************************
  Assign it_Val[] to <it_Sortval>.
  Loop at <it_Sortval> assigning <it_Values>.

    At new VAR_NAME.
      Refresh it_Buf.
      Read table it_Bookmarks assigning <it_Bookmarks>
        with key Name = <it_Values>-VAR_NAME
        binary search.
      If sy-subrc = 0.
        Var_Error = 0.
        If not <it_Values>-VAR_NAME is initial.
          Append lines of it_All from <it_Bookmarks>-PosBeg to
<it_Bookmarks>-PosEnd
            to it_Buf.
        EndIf.
      Else.
        Var_Error = 1.
      EndIf.
    EndAt.

    check Var_Error = 0.

    At new VAL_TYPE.
      At new VAR_NUM.
        Refresh it_New.

        Case <it_Values>-VAL_TYPE.
          when 'V'.
            Read table it_Bookmarks assigning <copyBookmark>
              with key Name = <it_Values>-VALUE
              binary search.
            If sy-subrc = 0.
              If not <it_Values>-VAR_NAME is initial.
                Append lines of it_All
                  from <copyBookmark>-PosBeg
                  to <copyBookmark>-PosEnd
                  to it_New.
                <copyBookmark>-Status = 'D'.
              EndIf.
            EndIf.

          when 'D'.
            Read table it_Bookmarks assigning <copyBookmark>
              with key Name = <it_Values>-VAR_NAME
              binary search.
            If sy-subrc = 0.
              If not <it_Values>-VAR_NAME is initial.
                <copyBookmark>-Status = 'D'.
              EndIf.
            EndIf.

          when 'S'.
            If not <it_Values>-VAR_NUM is initial.
              it_New[] = it_Buf[].
            EndIf.

          when others.
        EndCase.
      EndAt.
    EndAt.

    If not <it_Values>-FIND_TEXT is initial.
      If <it_Values>-VAL_TYPE = 'S'.
        If <it_Values>-VAR_NUM is initial.
          Replace all occurrences of <it_Values>-FIND_TEXT
            in table it_All
            from <it_Bookmarks>-PosBeg
            to   <it_Bookmarks>-PosEnd
            with <it_Values>-VALUE
            ignoring case.
        Else.
          Replace all occurrences of <it_Values>-FIND_TEXT
            in table it_New
            with <it_Values>-VALUE
            ignoring case.
        EndIf.
      EndIf.
    Else.
      If <it_Values>-VAL_TYPE = 'S'.
      EndIf.
    EndIf.

    At end of VAR_NUM.
      If not <it_Values>-VAR_NUM is initial.
        Append lines of it_New to <it_Bookmarks>-it_New.
        <it_Bookmarks>-Status = 'I'.
      EndIf.
    EndAt.
  EndLoop.

  Sort it_Bookmarks  by PosBeg descending.
  Loop at it_Bookmarks assigning <it_Bookmarks>.
    If <it_Bookmarks>-Status = 'I' or
       <it_Bookmarks>-Status = 'D'.
      Delete it_All
        from <it_Bookmarks>-PosBeg
        to   <it_Bookmarks>-PosEnd.
    EndIf.

    If <it_Bookmarks>-Status = 'I'.
      Idx = <it_Bookmarks>-PosBeg.
      Insert lines of <it_Bookmarks>-it_New
        into it_All index Idx.
    EndIf.
  EndLoop.
**************************************
  Perform Build_File_RTF
    tables   it_All
    changing FILE_TEXT .

EndForm.                    "FillRTFForm

*&---------------------------------------------------------------------*
*&      Form  DownloadWWWTempRTF
*&---------------------------------------------------------------------*
Form DownloadWWWTempRTF
  using w_Key     type WWWDATATAB
        FORM_NAME type WWWDATATAB-OBJID
        File_Text type string.

  Data:
    mime type standard table of W3MIME with header line,
    XStr type XString,
    wwwLen type W3_QVALUE, "i.
    Len type i.

  Field-symbols:
    <mime>,
    <XStr> type X.

  Call function 'WWWDATA_IMPORT'
    EXPORTING
      KEY    = w_Key
    TABLES
      MIME   = mime
    EXCEPTIONS
      others = 99.

  Check sy-subrc = 0.

  Call function 'WWWPARAMS_READ'
    EXPORTING
      RELID  = 'MI'
      OBJID  = FORM_NAME
      NAME   = 'filesize'
    IMPORTING
      VALUE  = wwwLen
    EXCEPTIONS
      others = 1.

  Len = wwwLen.

*  Loop at MIME assigning <mime>.
*    assign <mime> to <XStr> type 'X'.
*    Concatenate XStr <XStr> into XStr
*      in byte mode.                                         "#EC V#610
*    .
*  EndLoop.
*
*  XStr = XStr(Len).

  Perform TableToXString
    tables MIME
    using Len
          'BIN'
          ''
    changing XStr.



  Data:
    Conv type ref to CL_ABAP_CONV_IN_CE.

*   ######## ## SCT2_CONVERT_TABLE
  Call method CL_ABAP_CONV_IN_CE=>Create
    RECEIVING
      CONV = Conv.
  Call method Conv->CONVERT
    EXPORTING
      INPUT = XStr
    IMPORTING
      DATA  = File_Text.
EndForm.                    "DownloadWWWTempRTF
