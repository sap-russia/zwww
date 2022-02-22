*&---------------------------------------------------------------------*
*& Report  Z_WWW_SAMPLE_INVOICE
*&---------------------------------------------------------------------*
REPORT  ZWWW_SAMPLE_INVOICE.
* ###### ###### ##### #####-####### # Excel

Selection-screen begin of block b1 with frame.
Parameters:
  Excel radiobutton group gr1,
  Word  radiobutton group gr1.
Parameters:                                                 "#EC V#700
  RTF   radiobutton group gr1.                              "#EC V#700
Selection-screen end of block b1.

Selection-screen Begin of block b2 with frame title Text-002.
Parameters:
  p_File type RLGRAP-FILENAME modif id VVN,
  p_FLoc type ZWWW_FILE_LOCATION value check default '' modif id VVN,
  Protect as checkbox default 'X',
  Dialog as checkbox.
Parameters:
  p_Close  as checkbox modif id VVN,
  p_Delete as checkbox modif id VVN default 'X',
  p_Wh_OLE as checkbox modif id VVN,
  p_USEJAR as checkbox modif id VVN,
  p_UNICOD as checkbox modif id VVN,
  p_Debug  as checkbox modif id VVN,
  p_Optim(7)  type N modif id VVN default 100.
Selection-screen End of block b2.

Data:
  it_Val type standard table of ZWWW_VALUES
    with header line,
  NameTemplate type WWWDATATAB-OBJID.

At Selection-Screen output.
  If sy-uname <> 'VIKTOROV'.
    Loop at Screen.
      If Screen-Group1 = 'VVN'.
        Screen-Invisible = 1.
        Screen-Input = 0.
*        Modify Screen.
      EndIf.
    EndLoop.
  EndIf.

At Selection-Screen.
  Case sy-ucomm.
    when 'DEBUG'.
      p_Debug = 'X'.
  EndCase.

At user-command.
*  If sy-ucomm = 'HHH'.
*    Call method CL_ABAP_MEMORY_UTILITIES=>DO_GARBAGE_COLLECTION.
*  EndIf.

Start-of-Selection.


End-of-Selection.
  Case 'X'.
    when Excel.
      NameTemplate = 'ZWWW_SAMPLE_INVOICE'.
    when Word.
      NameTemplate = 'ZWWW_SAMPLE_INVOICE_WORD'.
    when RTF.                                               "#EC V#700
      NameTemplate = 'ZWWW_SAMPLE_INVOICE_RTF'.             "#EC V#700
  EndCase.
  write /.


  Perform Fill_Test_Data
    tables it_Val.

  Call function 'ZWWW_OPENFORM'
    EXPORTING
      FORM_NAME   = NameTemplate
*vvn( ######### ######### #############
      FILE_NAME   = p_File
      FILE_LOCATION = p_FLoc
      PRINTDIALOG = Dialog
      PROTECT     = Protect
      WITHOUT_OLE = p_Wh_OLE
      USE_JAR     = p_USEJAR
      USE_UNICODE = p_UNICOD
      DEBUG_MODE  = p_Debug
      OPTIMIZE    = p_Optim
      CLOSE_FORM  = p_Close
      DELETE_FILE = p_Delete
*)vvn
    TABLES
      IT_VALUES   = it_Val
    EXCEPTIONS
      PRINTCANCEL = 1
      others      = 99.

  Case sy-subrc.
    when 1.
*      Message '###### ######!' type 'I'.
      Message I714(28) with '###### ######!'.
    when 99.
*      Message '########### ######!' type 'I'.
      Message I714(28) with '########### ######!'.
  EndCase.

*&---------------------------------------------------------------------*
*&      Form  Fill_Test_Data
*&---------------------------------------------------------------------*
Form Fill_Test_Data
  tables it_Val.

  Class ZCL_ZWWW definition load. "### ############# # 4.6

  Data:
    ValueChar(255),
    w_Val type ZWWW_VALUES.

  Define SetVal.
    Clear w_Val.
    w_Val-VAR_NAME  = &1.
    w_Val-VAR_NUM   = &2.
    w_Val-FIND_TEXT = &3.
    w_Val-VAL_TYPE  = &4.
    Write &5 to ValueChar no-zero no-grouping left-justified.
    w_Val-VALUE     = ValueChar.
    Append w_Val to it_Val.
  End-of-Definition.

  SetVal '' '' '[#####]'    '' '123'.
  SetVal '' '' '[####]'     '' sy-datum.
  SetVal '' '' '[########]' '' '### "## ## ###"'.
  SetVal '' '' '[#####]'    '' '105203, ######, ######## 13-# ##., #.4, ###.+7-916-7008753'.
  SetVal '' '' '[###]'      '' '7719652368'.
  SetVal '' '' '[################]' '' '## ##'.
  SetVal '' '' '[###############]'  '' '## ##'.
  SetVal '' '' '[#####_####]'       '' '321'.
  SetVal '' '' '[####_####]'        '' sy-datum.
  SetVal '' '' '[##########]'       '' '### "######## ########"'.
  SetVal '' '' '[#####_##########]' '' '#. ####### ########, ##. ######### #####, ### 1, ###### 99'.
  SetVal '' '' '[###_##########]'   '' '1111111111'.
  SetVal '' '' '[######]'           '' 'ZWWW (###.##.)'.

  SetVal '######' '1' '[#####]'    '' '######### ############ ######## #### # MS Office (ZWWW)'.
  SetVal '######' '1' '[##_###]'   '' '##'.
  SetVal '######' '1' '[###-##]'   '' '1'.
  SetVal '######' '1' '[####]'     '' '1.00'.
  SetVal '######' '1' '[#####]'    '' '1.00'.
  SetVal '######' '1' '[#####]'    '' ''.
  SetVal '######' '1' '[######]'   '' '18%'.
  SetVal '######' '1' '[###]'      '' '0.18'.
  SetVal '######' '1' '[#####_###]' '' '1.18'.

  SetVal '######' '2' '[#####]'    '' '######### ############ ########## # MS Word (########## # ZWWW)'.
  SetVal '######' '2' '[##_###]'   '' '##'.
  SetVal '######' '2' '[###-##]'   '' '1'.
  SetVal '######' '2' '[####]'     '' '10.00'.
  SetVal '######' '2' '[#####]'    '' '10.00'.
  SetVal '######' '2' '[#####]'    '' ''.
  SetVal '######' '2' '[######]'   '' '18%'.
  SetVal '######' '2' '[###]'      '' '1.80'.
  SetVal '######' '2' '[#####_###]' '' '11.80'.

  SetVal '######' '3' '[#####]'    '' '#########-######### XML-#### ### ######### ########## (## ###### ZWWW)'.
  SetVal '######' '3' '[##_###]'   '' '##'.
  SetVal '######' '3' '[###-##]'   '' '1'.
  SetVal '######' '3' '[####]'     '' '100.00'.
  SetVal '######' '3' '[#####]'    '' '100.00'.
  SetVal '######' '3' '[#####]'    '' ''.
  SetVal '######' '3' '[######]'   '' '18%'.
  SetVal '######' '3' '[###]'      '' '18.00'.
  SetVal '######' '3' '[#####_###]' '' '118.00'.

  SetVal '######' '4' '[#####]'    '' '########## ########## ### ############ ############# ####### (Z_OBSALD)'.
  SetVal '######' '4' '[##_###]'   '' '##'.
  SetVal '######' '4' '[###-##]'   '' '1'.
  SetVal '######' '4' '[####]'     '' '1000.00'.
  SetVal '######' '4' '[#####]'    '' '1000.00'.
  SetVal '######' '4' '[#####]'    '' ''.
  SetVal '######' '4' '[######]'   '' '18%'.
  SetVal '######' '4' '[###]'      '' '180.00'.
  SetVal '######' '4' '[#####_###]' '' '1180.00'.

  SetVal '######' '5' '[#####]'    '' '############# ###### ####### ##### ############# ###### (## ###### Z_OBSALD)'.
  SetVal '######' '5' '[##_###]'   '' '##'.
  SetVal '######' '5' '[###-##]'   '' '1'.
  SetVal '######' '5' '[####]'     '' '10000.00'.
  SetVal '######' '5' '[#####]'    '' '10000.00'.
  SetVal '######' '5' '[#####]'    '' ''.
  SetVal '######' '5' '[######]'   '' '18%'.
  SetVal '######' '5' '[###]'      '' '1800.00'.
  SetVal '######' '5' '[#####_###]' '' '11800.00'.

  SetVal '######' '6' '[#####]'    '' '###### ######### ########### #########'.
  SetVal '######' '6' '[##_###]'   '' '###'.
  SetVal '######' '6' '[###-##]'   '' '1'.
  SetVal '######' '6' '[####]'     '' '100000.00'.
  SetVal '######' '6' '[#####]'    '' '100000.00'.
  SetVal '######' '6' '[#####]'    '' ''.
  SetVal '######' '6' '[######]'   '' '18%'.
  SetVal '######' '6' '[###]'      '' '18000.00'.
  SetVal '######' '6' '[#####_###]' '' '118000.00'.

  SetVal '######' '7' '[#####]'    '' '########## ########### ####### # ############## ########### ########### #########'.
  SetVal '######' '7' '[##_###]'   '' '##'.
  SetVal '######' '7' '[###-##]'   '' '1'.
  SetVal '######' '7' '[####]'     '' '1000000.00'.
  SetVal '######' '7' '[#####]'    '' '1000000.00'.
  SetVal '######' '7' '[#####]'    '' ''.
  SetVal '######' '7' '[######]'   '' '18%'.
  SetVal '######' '7' '[###]'      '' '180000.00'.
  SetVal '######' '7' '[#####_###]' '' '1180000.00'.

  SetVal '####' '' '[####_###]' '' '199999.98'.
  SetVal '####' '' '[####_#####_###]' '' '1311110.98'.

  SetVal '' '' '[############]' '' '######### ##### ##########'.
  SetVal '' '' '[#########]'    '' ''.

  SetVal '#_######' '' '' 'M' 'Macros_AutoFit'.

* ###### ######## ####### ###### #########
  Data: MacrosWithParams type string.
  Concatenate 'MacrosWithParams'
    ZCL_ZWWW=>CHAR_TAB
    '#######!' into MacrosWithParams.
  SetVal '#_######' '' '' 'M' MacrosWithParams.

EndForm.                    "Fill_Data
