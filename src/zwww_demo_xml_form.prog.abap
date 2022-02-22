*&---------------------------------------------------------------------*
*& Report  ZWWW_DEMO_XML_FORM
*&---------------------------------------------------------------------*
REPORT  ZWWW_DEMO_XML_FORM.

Tables:
  CVERS_TXT.

Data:
  it_Val type standard table of ZWWW_VALUES
    with header line.

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

  Select single *
    from CVERS_TXT
    where LANGU = sy-langu.

End-of-Selection.
*  SetVal 'ProcessingInstruction' '' '[Target]' '' 'xml'.
*SetVal 'ProcessingInstruction' '' '[Data]'   '' 'version="1.0"
*encoding="windows-1251"'.

  SetVal '####' '' '[######]'   ''
  'NO_PRIB_9976_9976_7712040126997650001_20110824_175234'.
  SetVal '####' '' '[########]' '' CVERS_TXT-STEXT." 'SAP ERP 6.0'.
  SetVal '####' '' '[########]' '' '5.03'.

  SetVal '####.########' '' '[###]'      '' '1151006'.
  SetVal '####.########' '' '[#######]'  '' '24.08.2011'.
  SetVal '####.########' '' '[######]'   '' '35'.
  SetVal '####.########' '' '[########]' '' '2011'.
  SetVal '####.########' '' '[#####]'    '' '9976'.
  SetVal '####.########' '' '[#######]'  '' '0'.
  SetVal '####.########' '' '[#######]'  '' '213'.

  SetVal '####.########.####' '' '[#####]' '' '74.2'.
  SetVal '####.########.####' '' '[###]'   '' '(916) 700-87-53'.

  SetVal '####.########.####.####' '' '[#######]' '' '### "## ## ###"'.
  SetVal '####.########.####.####' '' '[#####]'   '' '7719652368'.
  SetVal '####.########.####.####' '' '[###]'     '' '771901001'.

  SetVal '####.########.####.####.#########' '' '' 'D' ''.

  SetVal '####.########.#########' '' '[######]' '' '2'.

  SetVal '####.########.#########.###' '' '[#######]'  '' '#########'.
  SetVal '####.########.#########.###' '' '[###]'      '' '#####'.
  SetVal '####.########.#########.###' '' '[########]' '' '##########'.

  SetVal '####.########.#########.######' '' '[#######]' ''
  '############ #-575/10 ## 30.12.2010'.
  SetVal '####.########.#########.######' '' '[#######]' '' ''.

  SetVal '####.########.#######.#####.#######' '' '[#####]' '' '1'.
  SetVal '####.########.#######.#####.#######' '' '[#####]' ''
  '45263558000'.

  SetVal '####.########.#######.#####.#######.######' '' '[###]'   ''
  '11111111111111111111'.
  SetVal '####.########.#######.#####.#######.######' '' '[#####]' ''
  '0'.

  SetVal '####.########.#######.#####.#######.######' '' '[###]'   ''
  '11111111111111111111'.
  SetVal '####.########.#######.#####.#######.######' '' '[#####]' ''
  '0'.

*  SetVal '####.########.#######.#####.#######.######' '' '' 'D' ''.
  SetVal '####.########.#######.#####.########' '' '' 'D' ''.
  SetVal '####.########.#######.#####.########.######' '' '' 'D' ''.
  SetVal '####.########.#######.#####.########.######' '' '' 'D' ''.
  SetVal '####.########.#######.#####.#########' '' '' 'D' ''.
  SetVal '####.########.#######.#####.#########.#######' '' '' 'D' ''.

  SetVal '####.########.#######.#######' '' '[#####]'           '' '1'.
  SetVal '####.########.#######.#######' '' '[#######]'         '' '0'.
  SetVal '####.########.#######.#######' '' '[##########]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[##########]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[###########]'     '' '0'.
  SetVal '####.########.#######.#######' '' '[######]'          '' '0'.
  SetVal '####.########.#######.#######' '' '[######]'          '' '0'.
  SetVal '####.########.#######.#######' '' '[###########]'     '' '0'.
  SetVal '####.########.#######.#######' '' '[########0]'       '' '0'.
  SetVal '####.########.#######.#######' '' '[########]'        '' '0'.
  SetVal '####.########.#######.#######' '' '[#######]'         '' '0'.
  SetVal '####.########.#######.#######' '' '[########]'        '' '0'.
  SetVal '####.########.#######.#######' '' '[##########]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[#############]'   '' ''.
  SetVal '####.########.#######.#######' '' '[############]'    '' ''.
  SetVal '####.########.#######.#######' '' '[#########]'       '' '2.0'
  .
  SetVal '####.########.#######.#######' '' '[##########]'      '' ''.
  SetVal '####.########.#######.#######' '' '[##########284]'   '' ''.
  SetVal '####.########.#######.#######' '' '[#########]'       '' '0'.
  SetVal '####.########.#######.#######' '' '[###########]'     '' '0'.
  SetVal '####.########.#######.#######' '' '[############]'    '' '0'.
  SetVal '####.########.#######.#######' '' '[########]'        '' '0'.
  SetVal '####.########.#######.#######' '' '[##########]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[###########]'     '' '0'.
  SetVal '####.########.#######.#######' '' '[#######311]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[#######311##]'    '' '0'.
  SetVal '####.########.#######.#######' '' '[#######311###]'   '' '0'.
  SetVal '####.########.#######.#######' '' '[#########]'       '' '0'.
  SetVal '####.########.#######.#######' '' '[##########]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[#########]'       '' '0'.
  SetVal '####.########.#######.#######' '' '[##########]'      '' '0'.
  SetVal '####.########.#######.#######' '' '[#########]'       '' '0'.
  SetVal '####.########.#######.#######' '' '[###########]'     '' '0'.
  SetVal '####.########.#######.#######' '' '[############]'    '' '0'.
  SetVal '####.########.#######.#######' '' '[#########1##]'    '' ''.
  SetVal '####.########.#######.#######' '' '[#########1####]'  '' ''.
  SetVal '####.########.#######.#######' '' '[#########1#####]' '' ''.

  SetVal '####.########.#######.#######.##############' '' '[#####]' ''
  '1'.

  SetVal '####.########.#######.#######.##############.#######' ''
  '[###########]'   '' '0'.
  SetVal '####.########.#######.#######.##############.#######' ''
  '[###########32]' '' '0'.
  SetVal '####.########.#######.#######.##############.#######' ''
  '[###########]'   '' '0'.

  SetVal '####.########.#######.#######.##############.#######.#######'
  '' '[#########]'   '' '0'.
  SetVal '####.########.#######.#######.##############.#######.#######'
  '' '[#############]'   '' '0'.
  SetVal '####.########.#######.#######.##############.#######.#######'
  '' '[#############]'   '' '0'.
  SetVal '####.########.#######.#######.##############.#######.#######'
  '' '[#############]'   '' '0'.
  SetVal '####.########.#######.#######.##############.#######.#######'
  '' '[#############]'   '' '0'.

  SetVal
  '####.########.#######.#######.##############.#######.#########' ''
  '[###########]'   '' '0'.
  SetVal
  '####.########.#######.#######.##############.#######.#########' ''
  '[############]'  '' '0'.
  SetVal
  '####.########.#######.#######.##############.#######.#########' ''
  '[#############]' '' '0'.

  SetVal '####.########.#######.#######.##############.##########' ''
  '[############]'        '' '0'.
  SetVal '####.########.#######.#######.##############.##########' ''
  '[############]'        '' '0'.
  SetVal '####.########.#######.#######.##############.##########' ''
  '[############]'        '' '0'.
  SetVal '####.########.#######.#######.##############.##########' ''
  '[##############]'      '' '0'.
  SetVal '####.########.#######.#######.##############.##########' ''
  '[#############]'       '' '0'.
  SetVal '####.########.#######.#######.##############.##########' ''
  '[###############]'     '' '0'.
  SetVal '####.########.#######.#######.##############.##########' ''
  '[#################]'   '' '0'.

  SetVal '####.########.#######.#######.###############' '' '[#####]' ''
  '1'.

  SetVal '####.########.#######.#######.###############.########' ''
  '[############]'   '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[############]'   '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[##############]' '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[#############]'  '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[###########]'    '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[##########]  '     '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[########32]  '     '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[############]'   '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[############]'   '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[#############]'  '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[###########]'    '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[##############]' '' '0'.

  SetVal
  '####.########.#######.#######.###############.########.############'
  '' '[##############]' '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.############'
  '' '[##########]' '' '0'.

  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[##########]'       '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[######]'           '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#########10]'      '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#########30]'      '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[###########]'      '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[##########]'       '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[###########]'      '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[###############]'  '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#########30##]'    '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[################]' '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[###############]'  '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#####]'            '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#############]'    '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[########]'         '' '0'.
  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#############]'    '' '0'.

  SetVal
  '####.########.#######.#######.###############.########.########' ''
  '[#############]'    '' '0'.

  SetVal '####.########.#######.#######.###############.########' ''
  '[############]'    '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[###############]' '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[############]'    '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[###############]' '' '0'.
  SetVal '####.########.#######.#######.###############.########' ''
  '[#############]'   '' '1'.

  SetVal '####.########.#######.#######.###############.###########' ''
  '[#############]'   '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[###############]' '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[###############]' '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[##############]	'  '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[##########]'      '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[#########]'       '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[###########]'     '' ''.

  SetVal '####.########.#######.#######.###############.###########' ''
  '[#############]' '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[###########]'   '' '0'.
  SetVal '####.########.#######.#######.###############.###########' ''
  '[#############]' '' '0'.

  SetVal '####.########.#######.#######.############' '' '' 'D' ''.
  SetVal '####.########.#######.#######.############.###########' '' ''
  'D' ''.
  SetVal '####.########.#######.#######.############.###########' '' ''
  'D' ''.
  SetVal '####.########.#######.#######.############.##########' '' ''
  'D' ''.
  SetVal '####.########.#######.#######.##########' '' '' 'D' ''.
  SetVal '####.########.#######.#######.##########.################' ''
  '' 'D' ''.

  SetVal '####.########.#######.#######.#############' '' '[#####]' ''
  '1'.
  SetVal '####.########.#######.#######.#############' '' '[######]' ''
  '1'.
  SetVal '####.########.#######.#######.#############' '' '[######]' ''
  '### "## ## ###"'.
  SetVal '####.########.#######.#######.#############' '' '[#####]' ''
  '111111111'.
  SetVal '####.########.#######.#######.#############' ''
  '[############]' '' '1'.
  SetVal '####.########.#######.#######.#############' '' '[##########]'
  '' '0'.
  SetVal '####.########.#######.#######.#############' ''
  '[################]' '' '0'.
  SetVal '####.########.#######.#######.#############' '' '[##########]'
  '' '100.0'.
  SetVal '####.########.#######.#######.#############' ''
  '[###########]'      '' '0'.
  SetVal '####.########.#######.#######.#############' ''
  '[############]'     '' '18.0'.
  SetVal '####.########.#######.#######.#############' '' '[######]'
  '' '0'.
  SetVal '####.########.#######.#######.#############' ''
  '[##############]'   '' '0'.
  SetVal '####.########.#######.#######.#############' ''
  '[############]'     '' '0'.
  SetVal '####.########.#######.#######.#############' '' '[#######]'
  '' '0'.
  SetVal '####.########.#######.#######.#############' '' '[#########]'
  '' ''.
  SetVal '####.########.#######.#######.#############' ''
  '[#########1######]' '' ''.

  SetVal '####.########.#######.#############' '' '' 'D' ''.
  SetVal '####.########.#######.#######' '' '' 'D' ''.
  SetVal '####.########.#######.#######.#########' '' '' 'D' ''.
  SetVal '####.########.#######.#######.#########.############' '' ''
  'D' ''.
  SetVal '####.########.#######.#######.#########.#####' '' '' 'D' ''.
  SetVal '####.########.#######.#######.#########.#####' '' '' 'D' ''.
  SetVal '####.########.#######.#######.#########.######' '' '' 'D' ''.
  SetVal '####.########.#######.#######.########' '' '' 'D' ''.
  SetVal '####.########.#######.#######.############' '' '' 'D' ''.
  SetVal '####.########.#######.#######.############.#######' '' '' 'D'
  ''.
  SetVal '####.########.#######.#######.############.#######.#######' ''
  '' 'D' ''.
  SetVal '####.########.#######.#######.############.#######.######' ''
  '' 'D' ''.
  SetVal '####.########.#######.##########' '' '' 'D' ''.
  SetVal '####.########.#######.############' '' '' 'D' ''.
  SetVal '####.########.#######.################' '' '' 'D' ''.
  SetVal '####.########.#######.################.########' '' '' 'D' ''.
  SetVal '####.########.#######.################.#############' '' ''
  'D' ''.
  SetVal '####.########.#######.################.######' '' '' 'D' ''.
  SetVal '####.########.#######.################.###############' '' ''
  'D' ''.
  SetVal '####.########.#######.################.######' '' '' 'D' ''.
  SetVal '####.########.#######.################.##########' '' '' 'D'
  ''.
  SetVal '####.########.#######.################.############' '' '' 'D'
  ''.
  SetVal '####.########.#######.################.#########' '' '' 'D' ''
  .
  SetVal '####.########.#######.##########' '' '' 'D' ''.
  SetVal '####.########.#######.##########.#########' '' '' 'D' ''.
  SetVal '####.########.#######.#########_#########' '' '' 'D' ''.
  SetVal '####.########.#######.#########_#########.#########' '' '' 'D'
  ''.


  Call function 'ZWWW_OPENFORM'
    EXPORTING
      FORM_NAME   = 'ZWWW_DEMO_XML_FORM'
      PRINTDIALOG = ''
      OPTIMIZE    = 0
      PROTECT     = ''
    TABLES
      IT_VALUES   = it_Val.