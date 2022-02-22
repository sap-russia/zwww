*&---------------------------------------------------------------------*
*& Report  ZWWW_BAR_CODE
*&---------------------------------------------------------------------*
REPORT  ZWWW_BAR_CODE.

Data:
  it_Val type standard table of ZWWW_VALUES
    with header line,
  s type string.
Types:
  char30(30),
  num20(20) type N.

Parameters:
  pCodabar type char30 default '/012/345-67-89',
  pI2OF5   type num20  default '98736820124345332442',
  pCode39  type char30 default 'TEST CODE 39',
  p128_1   type num20  default '98736820124345332442',
  p128_2   type char30 default 'Test code 128' lower case,
  pQR      type char30 default 'QR Test' lower case,
  pDM      type char30 default 'DataMatrix Test' lower case.

Data:
  wCodabar type string,
  wI2OF5   type string,
  wCode39  type string,
  w128_1   type string,
  w128_2   type string.


Initialization.

  wCodabar = '08:02202008200:008022202:0000280208008208002802008208200280008:0'.
  wI2OF5   = '00820:08802282280028822800:8820280:02::0000280:0820:20'.
  wCode39  = '80220002:020:00020:0002:082020228002028000:0220:00820202:0000:02080220'.
  w128_1   = 'C380<14<4154205609202908924501:449@'.
  w128_2   = 'B1:005<074434155<05830<4105<155023056032245032@'.

  Data:
    fmExist type SXST_PARE-EXIST.

  Call function 'CHECK_EXIST_FUNC'
    EXPORTING
      NAME   = 'ZVVN_BARCODE_GENERATE'
    IMPORTING
      EXIST  = fmExist
    EXCEPTIONS
      others = 99.

At Selection-Screen output.
  Loop at Screen.
    If fmExist <> 'X'.
      Screen-input = 0.
      Modify Screen.
    EndIf.
  EndLoop.

Start-of-Selection.

  Define SetVal.
    Clear it_Val.
    it_Val-VAR_NAME  = &1.
    it_Val-VAR_NUM   = &2.
    it_Val-FIND_TEXT = &3.
    it_Val-VAL_TYPE  = &4.
    it_Val-VALUE     = &5.
    Append it_Val.
  End-of-Definition.


End-of-Selection.
  Perform BarGenerate.

  SetVal 'LineBar' '1' '[#########]' '' 'Codabar'.
  SetVal 'LineBar' '1' '[######]'    '' pCodabar.
  SetVal 'LineBar' '1' '1234567890'     '' wCodabar.

  SetVal 'LineBar' '2' '[#########]' '' 'Interleaved 2 of 5'.
  SetVal 'LineBar' '2' '[######]'    '' pI2OF5.
  SetVal 'LineBar' '2' '1234567890'     '' wI2OF5.

  SetVal 'LineBar' '3' '[#########]' '' 'Code 39'.
  SetVal 'LineBar' '3' '[######]'    '' pCode39.
  SetVal 'LineBar' '3' '1234567890'     '' wCode39.

  SetVal 'LineBar' '4' '[#########]' '' 'Code 128'.
  SetVal 'LineBar' '4' '[######]'    '' p128_1.
  SetVal 'LineBar' '4' '1234567890'     '' w128_1.

  SetVal 'LineBar' '5' '[#########]' '' 'Code 128'.
  SetVal 'LineBar' '5' '[######]'    '' p128_2.
  SetVal 'LineBar' '5' '1234567890'     '' w128_2.

  SetVal 'LineQR' '1' '[#########]' '' 'QR Code'.
  SetVal 'LineQR' '1' '[######]'    '' pQR.
  SetVal 'LineQR' '1' '[a]'     '' 'F1DDD1F  841B F1DDD1F'.
  SetVal 'LineQR' '1' '[b]'     '' '7455547 6BFAF 7455547'.
  SetVal 'LineQR' '1' '[c]'     '' '76 78B5 FAE8452B 2  C'.
  SetVal 'LineQR' '1' '[d]'     '' 'D45544D B38A9C15D6469'.
  SetVal 'LineQR' '1' '[e]'     '' 'F 777 F 26934894E3527'.
  SetVal 'LineQR' '1' '[f]'     '' '1111111 1 111  111 1 '.

  SetVal 'LineQR' '2' '[#########]' '' 'Datamatrix Code'.
  SetVal 'LineQR' '2' '[######]'    '' pDM.
  SetVal 'LineQR' '2' '[a]'     '' 'F4B216B49CB25 72DA'.
  SetVal 'LineQR' '2' '[b]'     '' 'FE2595 A1AD8C89A8A'.
  SetVal 'LineQR' '2' '[c]'     '' 'F4B55325BCE413539A'.
  SetVal 'LineQR' '2' '[d]'     '' 'F266EE843367C121FA'.
  SetVal 'LineQR' '2' '[e]'     '' '332332232333322322'.
  SetVal 'LineQR' '2' '[f]'     '' ''.

  Call function 'ZWWW_OPENFORM'
    EXPORTING
      FORM_NAME   = 'ZWWW_BAR_CODE'
      PRINTDIALOG = ''
      PROTECT = ''
    TABLES
      IT_VALUES   = it_Val
    EXCEPTIONS
      PRINTCANCEL = 1
      others      = 99.

*&---------------------------------------------------------------------*
*&      Form  BarGenerate
*&---------------------------------------------------------------------*
Form BarGenerate.
  Data:
    Info type string.

  check fmExist = 'X'.

  Define BarGen.
    Clear: &3.
    Info = &1.

    Call function 'ZVVN_BARCODE_GENERATE'
      EXPORTING
        INFORMATION = Info
        CODE_TYPE   = &2
      IMPORTING
        CODE        = &3.
  End-of-Definition.

  BarGen pCodabar 'CODABAR'  wCodabar.
  BarGen pI2OF5   'I2OF5 '   wI2OF5.
  BarGen pCode39  'CODE_39'  wCode39.
  BarGen p128_1   'CODE_128' w128_1.
  BarGen p128_2   'CODE_128' w128_2.
EndForm.                    "BarGenerate
