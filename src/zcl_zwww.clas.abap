class ZCL_ZWWW definition
  public
  final
  create public .

*"* public components of class ZCL_ZWWW
*"* do not include other source files here!!!
public section.

  class-data CHAR_TAB type CHAR1 read-only .
  class-data CHAR_0D type CHAR1 read-only .
  class-data CHAR_0A type CHAR1 read-only .
  class-data VERSION type CHAR10 read-only value '3.03'. "#EC NOTEXT .

  class-methods CLASS_CONSTRUCTOR .
  class-methods SAP_OFFICE_GET_FILE_ID
    importing
      value(I_FILE_NAME) type STRING
    exporting
      value(E_FILE_ID) type SOFOLENTI1-DOC_ID
    exceptions
      FILE_NOT_FOUND .
protected section.
*"* protected components of class ZCL_ZWWW
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ZWWW
*"* do not include other source files here!!!

  class-data CHAR_ASCII type CHAR255 .

  class-methods ASSIGN_CHAR_X .
ENDCLASS.



CLASS ZCL_ZWWW IMPLEMENTATION.


  method ASSIGN_CHAR_X.
Constants:
  X_TAB(1) TYPE X VALUE '09',
  X_0D(1)  TYPE X VALUE '0D',
  X_0A(1)  TYPE X VALUE '0A'.

  Data: L type i,
        CHAR_01  type C,
        CHAR_A value 'A',
        CHAR_B value 'B',
*        CharRusA value '#',
        X_AsciiRusA type x value 'C0'.
  Field-symbols:
    <X> type X,
    <Y> type X,
    <EI> type X.

  Concatenate
    '!"#$%&''()*+,-./0123456789:;<=>?@'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`'
    'abcdefghijklmnopqrstuvwxyz{|}~ #'
    '####################### #################### ##################'
    '################################################################'
    into Char_Ascii+32.
*  Assign CharRusA  to <X>  type 'X'.
*  Assign CHAR_CONV to <EI> type 'X'.
*  <EI> = <X> - X_AsciiRusA.

  Assign CHAR_A  to <X>  casting.
  Assign CHAR_B  to <Y>  casting.
  Assign CHAR_01 to <EI> casting.
  <EI> = <Y> - <X>.

  Assign CHAR_TAB to <X> casting.
  Clear <X>.
  <X> = X_TAB * <EI>.
  Assign CHAR_0D to <X> casting.
  Clear <X>.
  <X> = X_0D * <EI>.
  Assign CHAR_0A to <X> casting.
  Clear <X>.
  <X> = X_0A * <EI>.
  endmethod.


  method CLASS_CONSTRUCTOR.
  Call method ASSIGN_CHAR_X.
  endmethod.


  method SAP_OFFICE_GET_FILE_ID.
  Data:
    lt_DirPath type standard table of SOOD6,
    wa_DirPath like line of lt_DirPath,
    lt_FileList type standard table of SOMT,
    wa_FileList like line of lt_FileList,
    FolID type SOODK,
*    Filt  type SOFID,
    Begin of l_FileID,
      FOLTP type SOMT-FOLTP,
      FOLYR type SOMT-FOLYR,
      FOLNO type SOMT-FOLNO,
      DOCTP type SOMT-DOCTP,
      DOCYR type SOMT-DOCYR,
      DOCNO type SOMT-DOCNO,
*      OWNNAM type SOMT-OWNNAM,
    End of l_FileID.

  wa_DirPath-OBJNAM = 'TEMP'.
  Append wa_DirPath to lt_DirPath.

  Call function 'SO_FOLDER_ID_GET'
    EXPORTING
      OWNER      = sy-uname
      REGION     = 'P' "Private
    IMPORTING
      OBJECT_ID  = FolID
    TABLES
      PATH_TABLE = lt_DirPath[]
    EXCEPTIONS
      others     = 99.
  If sy-subrc <> 0.
    Raise FILE_NOT_FOUND.
  EndIf.

  Translate I_File_Name to upper case.
*  Filt-OBJDES = FileName.

  Call function 'SO_FOLDER_READ'
    EXPORTING
      OBJECT_ID   = FolID
      OWNER       = sy-uname
*      FILTER      = Filt
    TABLES
      FOLDER_CONT = lt_FileList[]
    EXCEPTIONS
      others      = 99.
  If sy-subrc <> 0.
    Raise FILE_NOT_FOUND.
  EndIf.

  Sort lt_FileList descending
    by CRDAT CRTIM.

  Loop at lt_FileList into wa_FileList
    where DOCDES = I_File_Name.
    Move-corresponding: wa_FileList to l_FileID.
    Exit.
  EndLoop.

  If sy-subrc <> 0.
    Raise FILE_NOT_FOUND.
  EndIf.

  E_File_ID = l_FileID.

  endmethod.
ENDCLASS.
