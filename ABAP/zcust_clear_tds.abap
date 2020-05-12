REPORT zcust_clear_in.

* INCLUDE bdcrecx1.

TYPES : BEGIN OF ty_bdc,
          bukrs    TYPE bkpf-bukrs,
          waers    TYPE bkpf-waers,
          budat    TYPE bkpf-budat,
          bldat    TYPE bkpf-bldat,
          xblnr    TYPE bkpf-xblnr,
          bktxt    TYPE bkpf-bktxt,
          clr_gl   TYPE rf05a-newko,
          clr_amnt TYPE bseg-wrbtr,
          sgtxt_1  TYPE bseg-sgtxt,
          tds_gl   TYPE rf05a-newko,
          tds_amnt TYPE bseg-wrbtr,
          sgtxt_2  TYPE bseg-sgtxt,
          kunnr    TYPE bsid-kunnr,
          vbeln    TYPE vbrk-vbeln,
          blart    TYPE bkpf-blart,
          agkon    TYPE rf05a-agkon,
          augtx    TYPE rf05a-augtx,
          konto    TYPE rf05a-konto,
          bupla    TYPE bseg-bupla,
          valut    TYPE bseg-valut,
          newbs    TYPE rf05a-newbs,
          zuonr    TYPE bseg-zuonr,
          abpos    TYPE rf05a-abpos,
        END OF ty_bdc.

TABLES: bkpf,bsid.

DATA : it_bdc       TYPE STANDARD TABLE OF ty_bdc WITH HEADER LINE,
       wa_bdc       TYPE ty_bdc,
       kunnr        TYPE bsid-kunnr,
       bukrs        TYPE bkpf-bukrs,
       waers        TYPE bkpf-waers,
       lv_clr(16)   TYPE c,
       lv_tds(16)   TYPE c,
       w_struct     TYPE truxs_t_text_data,
       lv_date1(10) TYPE c,
       lv_date2(10) TYPE c,
       lv_date3(10) TYPE c,
       flag(1)      TYPE c,
       count(4)     TYPE c.

DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100.


SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE a1title.

SELECT-OPTIONS: p_bukrs FOR bukrs MODIF ID ip1 OBLIGATORY NO-EXTENSION NO INTERVALS.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.

SELECTION-SCREEN END OF BLOCK selection.


INITIALIZATION.
  a1title = 'Custom Program for Mass Customer Clearing'.

***** Path Selection *****

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_file.

***** Excel File Upload *****

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = w_struct
      i_filename           = p_file
    TABLES
      i_tab_converted_data = it_bdc[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE 'NOT CONVERTED' TYPE 'I'.
  ENDIF.
  IF it_bdc[] IS INITIAL.
    MESSAGE 'List Contains No Data' TYPE 'I'.
  ENDIF.

START-OF-SELECTION.
  IF it_bdc[] IS NOT INITIAL.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD p_bukrs-low.
    IF sy-subrc <> 0.
      MESSAGE  'You are not authorized to post with this company code.' TYPE 'E'.
    ENDIF.
    PERFORM bdc_in.

  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_in.

*  PERFORM open_group.

  LOOP AT it_bdc INTO wa_bdc.
    CLEAR: it_bdc, lv_clr, lv_tds, sy-tabix.
    flag = 'Y'.
    count = '1'.
    lv_clr = wa_bdc-clr_amnt.
    lv_tds = wa_bdc-tds_amnt.
    CONCATENATE wa_bdc-bldat+6(2) wa_bdc-bldat+4(2) wa_bdc-bldat+0(4) INTO lv_date2 SEPARATED BY '.'.
    CONCATENATE wa_bdc-budat+6(2) wa_bdc-budat+4(2) wa_bdc-budat+0(4) INTO lv_date3 SEPARATED BY '.'.
    wa_bdc-abpos = sy-tabix.

    PERFORM bdc_dynpro        USING 'SAPMF05A' '0122'.
    PERFORM bdc_field         USING 'BDC_CURSOR'
                                    'RF05A-NEWKO'.
    PERFORM bdc_field         USING 'BDC_OKCODE'
                                     '/00'.
    PERFORM bdc_field         USING 'BKPF-BLART'
                                     'DZ'.
    PERFORM bdc_field         USING 'BKPF-BUKRS'
                                     wa_bdc-bukrs.  " COMPANY CODE "
    PERFORM bdc_field         USING 'BKPF-WAERS'
                                     wa_bdc-waers.  " CURRENCY "
    PERFORM bdc_field         USING 'BKPF-BUDAT'
                                     lv_date3.      " POSTING DATE "
    PERFORM bdc_field         USING 'BKPF-BLDAT'
                                     lv_date2.      " DOCUMENT DATE "
    PERFORM bdc_field         USING 'BKPF-XBLNR'
                                     wa_bdc-xblnr.  " REFERENCE "
    PERFORM bdc_field         USING 'BKPF-BKTXT'
                                     wa_bdc-bktxt.  " HEADER TEXT "
    PERFORM bdc_field         USING 'RF05A-NEWBS'
                                     '40'.          " POSTING KEY "
    PERFORM bdc_field         USING 'RF05A-NEWKO'
                                     wa_bdc-clr_gl. " BANK GL NO. "

    IF wa_bdc-tds_amnt IS NOT INITIAL.

      PERFORM bdc_dynpro        USING 'SAPMF05A' '0300'.
      PERFORM bdc_field         USING 'BDC_CURSOR'
                                      'RF05A-NEWKO'.
      PERFORM bdc_field         USING 'BDC_OKCODE'
                                       '/00'.
      PERFORM bdc_field         USING 'BSEG-WRBTR'
                                       lv_clr.          " CLEARING AMOUNT "
      PERFORM bdc_field         USING 'BSEG-VALUT'
                                       lv_date2.        " DOCUMENT DATE "
      PERFORM bdc_field         USING 'BSEG-SGTXT'
                                       wa_bdc-sgtxt_1.  " TEXT 1 "
      PERFORM bdc_field         USING 'BDC_SUBSCR'
                                       'SAPLKACB'.
      PERFORM bdc_field         USING 'DKACB-FMORE'
                                       ''.
*    IF wa_bdc-tds_amnt IS NOT INITIAL.

      PERFORM bdc_field         USING 'RF05A-NEWBS'
                                       '40'.            " POSTING KEY "
      PERFORM bdc_field         USING 'RF05A-NEWKO'
                                        wa_bdc-tds_gl.   " TDS GL NO. "

*    PERFORM bdc_dynpro        USING 'SAPLKACB' '0002'.
*    PERFORM bdc_field         USING 'BDC_CURSOR' ' '.
*                                     'COBL-PS_PSP_PNR'.
*    PERFORM bdc_field         USING 'BDC_OKCODE'
*                                     '=ENTE'.
*    PERFORM bdc_field         USING 'BDC_SUBSCR'
*                                     'SAPLKACB'.

*    PERFORM bdc_dynpro        USING 'SAPLKACB' '0002'.
*    PERFORM bdc_field         USING 'BDC_CURSOR'
*                                     'COBL-KOSTL'.
*    PERFORM bdc_field         USING 'BDC_OKCODE'
*                                     '=ENTE'.


      PERFORM bdc_dynpro        USING 'SAPMF05A' '0300'.
      PERFORM bdc_field         USING 'BDC_CURSOR'
                                       'BSEG-VALUT'.
      PERFORM bdc_field         USING 'BDC_OKCODE'
                                       '=SL'.
      PERFORM bdc_field         USING 'BSEG-WRBTR'
                                       lv_tds.          " TDS AMOUNT "
      PERFORM bdc_field         USING 'BSEG-VALUT'
                                       lv_date2.        " DOCUMENT DATE "
      PERFORM bdc_field         USING 'BSEG-SGTXT'
                                       wa_bdc-sgtxt_2.  " TEXT 2 "
      PERFORM bdc_field         USING 'BDC_SUBSCR'
                                       'SAPLKACB'.
      PERFORM bdc_field       USING 'DKACB-FMORE'
                                       ''.
    ELSE.

      PERFORM bdc_dynpro        USING 'SAPMF05A' '0300'.
      PERFORM bdc_field         USING 'BDC_CURSOR'
                                      'RF05A-NEWKO'.
      PERFORM bdc_field         USING 'BDC_OKCODE'
                                       '=SL'.
      PERFORM bdc_field         USING 'BSEG-WRBTR'
                                       lv_clr.          " CLEARING AMOUNT "
      PERFORM bdc_field         USING 'BSEG-VALUT'
                                       lv_date2.        " DOCUMENT DATE "
      PERFORM bdc_field         USING 'BSEG-SGTXT'
                                       wa_bdc-sgtxt_1.  " TEXT 1 "
      PERFORM bdc_field         USING 'BDC_SUBSCR'
                                       'SAPLKACB'.
      PERFORM bdc_field         USING 'DKACB-FMORE'
                                       ''.
    ENDIF.

    PERFORM bdc_dynpro        USING 'SAPMF05A' '0710'.
    PERFORM bdc_field         USING 'BDC_CURSOR'
                                     'RF05A-XPOS1(03)'.
    PERFORM bdc_field         USING 'BDC_OKCODE'
                                     '=PA'.
    PERFORM bdc_field         USING 'RF05A-AGKON'
                                     wa_bdc-kunnr.    " CUSTOMER NO. "
    PERFORM bdc_field         USING 'RF05A-AGKOA'
                                     'D'.
*    PERFORM bdc_field         USING 'BDC_SUBSCR'
*                                     'SAPLKACB'.
    PERFORM bdc_field         USING 'RF05A-XNOPS'
                                     'X'.
    PERFORM bdc_field         USING 'RF05A-XPOS1(01)'
                                     ''.
    PERFORM bdc_field         USING 'RF05A-XPOS1(03)'
                                     'X'.
*
*    PERFORM bdc_dynpro        USING 'SAPDF05X' '3100'.
*    PERFORM bdc_field         USING 'BDC_OKCODE'
*                                     '=OSU'.
*    PERFORM bdc_field         USING 'BDC_SUBSCR'
*                                     'SAPDF05X'.
*    PERFORM bdc_field         USING 'BDC_CURSOR'
*                                     'DF05B-PSSKT(01)'.
*    PERFORM bdc_field         USING 'RF05A-ABPOS'
*                                     '1'.
*
*    PERFORM bdc_dynpro        USING 'SAPDF05X' '2000'.
*    PERFORM bdc_field         USING 'BDC_CURSOR'
*                                     'RF05A-XPOS1(03)'.
*    PERFORM bdc_field         USING 'BDC_OKCODE'
*                                     '=GO'.
*    PERFORM bdc_field         USING 'RF05A-XPOS1(01)'
*                                     ''.
*    PERFORM bdc_field         USING 'RF05A-XPOS1(03)'
*                                     'X'.

    PERFORM bdc_dynpro        USING 'SAPMF05A' '0731'.
    PERFORM bdc_field         USING 'BDC_CURSOR'
                                     'RF05A-SEL01(01)'.
    PERFORM bdc_field         USING 'BDC_OKCODE'
                                     '=PA'.
    PERFORM bdc_field         USING 'RF05A-SEL01(01)'
                                     wa_bdc-vbeln.   " DOCUMENT NO."

*    PERFORM bdc_dynpro        USING 'SAPDF05X' '3100'.
*    PERFORM bdc_field         USING 'BDC_OKCODE'
*                                     '=PAI'.
*    PERFORM bdc_field         USING 'BDC_SUBSCR'
*                                     'SAPDF05X'.
*    PERFORM bdc_field         USING 'BDC_CURSOR'
*                                     'DF05B-PSBET(01)'.
*    PERFORM bdc_field         USING 'RF05A-ABPOS'
*                                     '1'.
*    PERFORM bdc_dynpro        USING 'SAPDF05X' '0731'.
*    PERFORM bdc_field         USING 'BDC_CURSOR'
*                                     'RF05A-SEL01(01)'.
*    PERFORM bdc_field         USING 'BDC_OKCODE'
*                                     '=PA'.

    PERFORM bdc_dynpro        USING 'SAPDF05X' '3100'.
    PERFORM bdc_field         USING 'BDC_OKCODE'
                                     '=BU'.
    PERFORM bdc_field         USING 'BDC_SUBSCR'
                                     'SAPDF05X'.
    PERFORM bdc_field         USING 'BDC_CURSOR'
                                     'DF05B-PSBET(01)'.
    PERFORM bdc_field         USING 'RF05A-ABPOS'
                                     '1'.



    PERFORM bdc_transaction   USING 'FB05'.

*    flag = 'N'.

  ENDLOOP.
*  PERFORM close_group.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc    LIKE sy-subrc,
        counter(4) TYPE c.

* batch input session
*  IF SESSION = 'X'.
*    CALL FUNCTION 'BDC_INSERT'
*         EXPORTING TCODE     = TCODE
*         TABLES    DYNPROTAB = BDCDATA.
*    IF SMALLLOG <> 'X'.
*      WRITE: / 'BDC_INSERT'(I03),
*               TCODE,
*               'returncode:'(I05),
*               SY-SUBRC,
*               'RECORD:',
*               SY-INDEX.
*    ENDIF.
** call transaction using
*  ELSE.
  REFRESH messtab.
  CALL TRANSACTION tcode USING bdcdata
                   MODE   'N'
                   UPDATE 'S'
                   MESSAGES INTO messtab.
*    L_SUBRC = SY-SUBRC.
*  WRITE: / 'LINE',
*           sy-index.
*                'CALL_TRANSACTION',
*               TCODE,
*               'returncode:'(I05),
*               L_SUBRC,
  LOOP AT messtab.
    MESSAGE ID     messtab-msgid
            TYPE   messtab-msgtyp
            NUMBER messtab-msgnr
            INTO l_mstring
            WITH messtab-msgv1
                 messtab-msgv2
                 messtab-msgv3
                 messtab-msgv4.
    WRITE: / messtab-msgtyp, l_mstring(100).

  ENDLOOP.

  SKIP.
*    ENDIF.
** Erzeugen fehlermappe ************************************************
*    IF L_SUBRC <> 0 AND E_GROUP <> SPACE.
*      IF E_GROUP_OPENED = ' '.
*        CALL FUNCTION 'BDC_OPEN_GROUP'
*             EXPORTING  CLIENT   = SY-MANDT
*                        GROUP    = E_GROUP
*                        USER     = E_USER
*                        KEEP     = E_KEEP
*                        HOLDDATE = E_HDATE.
*         E_GROUP_OPENED = 'X'.
*      ENDIF.
*      CALL FUNCTION 'BDC_INSERT'
*           EXPORTING TCODE     = TCODE
*           TABLES    DYNPROTAB = BDCDATA.
*    ENDIF.
*  ENDIF.
  REFRESH bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
*  IF FVAL <> NODATA.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
*  ENDIF.
ENDFORM.


FORM open_dataset USING p_dataset.
  OPEN DATASET p_dataset
               FOR INPUT IN TEXT MODE
               ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: / text-e00, sy-subrc.
    STOP.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
FORM close_dataset USING p_dataset.
  CLOSE DATASET p_dataset.
ENDFORM.