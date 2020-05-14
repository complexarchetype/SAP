REPORT ZAIRCARGO_INVOICE_PD.
*-->Data decalration
TABLES: VBAK, VBRK.
TYPES: BEGIN OF TY_VBRK,
         VBELN TYPE VBRK-VBELN,
         VKORG TYPE VBRK-VKORG,
         VTWEG TYPE VBRK-VTWEG,
         FKDAT TYPE VBRK-FKDAT,
         KUNRG TYPE VBRK-KUNRG,
         BUKRS TYPE VBRK-BUKRS,
         NAME1 TYPE KNA1-NAME1,
         ADRNR TYPE KNA1-ADRNR,
       END OF TY_VBRK,

       BEGIN OF TY_BSEG,
         VBELN TYPE BSEG-VBELN,
         AUGBL TYPE BSEG-AUGBL,
       END OF TY_BSEG,

       BEGIN OF TY_INFO,
         ICON_FIELD LIKE ICON-ID,
         RMKS(132)  TYPE C,
       END OF TY_INFO.
DATA: FLAG, DATEDIFF TYPE I, P_FORM TYPE TDSFNAME, FM_NAME TYPE RS38L_FNAM,
      V_NAME   TYPE THEAD-TDNAME, MATNR TYPE VBRP-MATNR,
      IT_BSEG  TYPE TY_BSEG OCCURS 0 WITH HEADER LINE,
      T_TLINE  LIKE TLINE OCCURS 0 WITH HEADER LINE,
      IT_VBRK  TYPE TY_VBRK OCCURS 0 WITH HEADER LINE.
*--- For Invoice download and other process
DATA: CPARAM                  TYPE SSFCTRLOP, V_BIN_FILESIZE TYPE I, V_FILENAME TYPE STRING,
      ST_JOB_OUTPUT_INFO_MAIN TYPE SSFCRESCL, IT_DOCS TYPE STANDARD TABLE OF DOCS,
      IT_LINES                TYPE STANDARD TABLE OF TLINE,
      ST_CONTROL_PARAMETERS   TYPE SSFCTRLOP, OUTOP TYPE SSFCOMPOP,
      ST_JOB_OUTPUT_INFO      TYPE SSFCRESCL.
*--- For invoice automail
DATA: IT_ADR6      TYPE TABLE OF ADR6 WITH HEADER LINE,
      GS_DOCDATA   TYPE SODOCCHGI1, " DATA OF AN OBJECT WHICH CAN BE CHANGED
      GT_RECLIST   TYPE TABLE OF SOMLRECI1, " SAPOFFICE: STRUCTURE OF THE API RECIPIENT LIST
      GS_RECLIST   TYPE SOMLRECI1, " SAPOFFICE: STRUCTURE OF THE API RECIPIENT LIST
      GS_OBJBIN    TYPE SOLISTI1, " SAPOFFICE: SINGLE LIST WITH COLUMN LENGTH 255
      GS_OBJPACK   TYPE SOPCKLSTI1, " SAPOFFICE: DESCRIPTION OF IMPORTED OBJECT COMPONENTS
      GT_OBJPACK   TYPE TABLE OF SOPCKLSTI1, " SAPOFFICE: DESCRIPTION OF IMPORTED OBJECT COMPONENTS
      GS_PDF_TAB   TYPE TLINE, " WORKAREA FOR SAP SCRIPT TEXT LINES
      GT_OBJBIN    TYPE TABLE OF SOLISTI1, " SAPOFFICE: SINGLE LIST WITH COLUMN LENGTH 255
      FLAG2        TYPE C,
      V_LINES_TXT  TYPE I,
      GV_TAB_LINES TYPE I,
      GV_POS       TYPE I,
      GV_LEN       TYPE I.
DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: LS_LINE  TYPE SLIS_LISTHEADER,
      T_SORT   TYPE SLIS_T_SORTINFO_ALV,
      W_SORT   TYPE SLIS_SORTINFO_ALV,
      MONTH(2) TYPE C,
      FFNSFN   TYPE STRING,
      IT_INFO  TYPE TY_INFO OCCURS 0 WITH HEADER LINE,
      STR      TYPE STRING.
DATA: T_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      LS_LAYOUT  TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS  TYPE SLIS_T_EVENT.

*for mail
DATA : BEGIN OF ITAB OCCURS 0,
         FLD(250),
       END OF ITAB,
       CUSTOMER_INITIAL TYPE NAME1.


*-->Selection screen

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : RT1 RADIOBUTTON GROUP G1  USER-COMMAND SEL DEFAULT 'X',
             RT2 RADIOBUTTON GROUP G1,
             RT3 RADIOBUTTON GROUP G1.
SELECTION-SCREEN : END OF BLOCK B1.
SELECTION-SCREEN : BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
    P_VKORG FOR VBAK-VKORG  NO INTERVALS ,
    P_VTWEG FOR VBAK-VTWEG  NO INTERVALS,
    P_VBELN FOR VBRK-VBELN ,
    P_KUNRG FOR VBRK-KUNRG ,
    P_FKDAT FOR VBRK-FKDAT .
PARAMETERS: P_DUDAT  TYPE STRING LOWER CASE MODIF ID S1,
            P_PEROD  TYPE STRING LOWER CASE MODIF ID S1,
            DOWNLOAD TYPE IBIPPARMS-PATH MODIF ID S2.
SELECTION-SCREEN : END OF BLOCK B2.
SELECTION-SCREEN : BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : OP1 RADIOBUTTON GROUP G2 USER-COMMAND SEL DEFAULT 'X',
             OP2 RADIOBUTTON GROUP G2,
             OP3 RADIOBUTTON GROUP G2.
SELECTION-SCREEN : END OF BLOCK B3.

AT SELECTION-SCREEN OUTPUT .
  LOOP AT SCREEN.
    IF OP2 EQ 'X' OR OP3 EQ 'X' OR RT2 EQ 'X' OR RT3 EQ 'X'.
      IF SCREEN-GROUP1 = 'S1'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF OP1 EQ 'X' OR OP3 EQ 'X'.
      IF SCREEN-GROUP1 = 'S2'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR DOWNLOAD.
  DATA: L_SEL_DIR     TYPE STRING.
* Browse the Directories
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    CHANGING
      SELECTED_FOLDER      = L_SEL_DIR
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
  ELSE.
    DOWNLOAD = L_SEL_DIR.
  ENDIF.
*-->Start of selection
START-OF-SELECTION.
  LOOP AT P_VKORG.
    TRANSLATE P_VKORG-LOW TO UPPER CASE.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
           ID 'VKORG' FIELD P_VKORG-LOW
           ID 'SPART' FIELD 'AC'
           ID 'ACTVT' FIELD '03'.
    IF SY-SUBRC <> 0.
      MESSAGE 'You do not have access into this sales Org.' TYPE 'I'.
      FLAG = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
*---Fetch data as per selection parameters
  IF FLAG NE 'X'.
    IF P_VKORG IS INITIAL OR P_FKDAT IS INITIAL OR P_VTWEG IS INITIAL.
      MESSAGE 'Sales Org, Dristibution Channel and Billing Date is mandatory to process!' TYPE 'I'.
    ELSE.
      PERFORM INPUTPARAM USING P_FKDAT-LOW P_FKDAT-HIGH.
      IF FLAG NE 'X'.
        CLEAR: IT_VBRK, IT_VBRK[].
        SELECT A~VBELN A~VKORG A~VTWEG A~FKDAT A~KUNRG A~BUKRS B~NAME1 B~ADRNR
          FROM VBRK AS A INNER JOIN KNA1 AS B ON A~KUNRG = B~KUNNR
          INTO CORRESPONDING FIELDS OF TABLE IT_VBRK
          WHERE VBELN IN P_VBELN AND
                VKORG IN P_VKORG AND
                VTWEG IN P_VTWEG AND
                FKDAT IN P_FKDAT AND
                KUNRG IN P_KUNRG AND
                FKSTO NE 'X'     AND
                FKART NE 'S1'    AND
                RFBSK EQ 'C'.
        IF IT_VBRK[] IS NOT INITIAL.
*--- Getting the cleared invoices
          CLEAR: IT_BSEG, IT_BSEG[].
          SELECT VBELN AUGBL FROM BSEG INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
            FOR ALL ENTRIES IN IT_VBRK WHERE VBELN = IT_VBRK-VBELN
            AND AUGBL NE ''.

          SORT: IT_BSEG BY VBELN, IT_VBRK BY VBELN.

          DELETE ADJACENT DUPLICATES FROM IT_BSEG.
          IF RT1 EQ 'X'.
            LOOP AT IT_BSEG.
              DELETE IT_VBRK WHERE VBELN = IT_BSEG-VBELN.
            ENDLOOP.
          ELSEIF RT2 EQ 'X'.
            LOOP AT IT_VBRK.
              READ TABLE IT_BSEG WITH KEY VBELN = IT_VBRK-VBELN BINARY SEARCH.
              IF SY-SUBRC NE 0.
                DELETE IT_VBRK WHERE VBELN = IT_VBRK-VBELN.
              ENDIF.
            ENDLOOP.
          ENDIF.
*--- For Invoice due date & period information update in invoice header text
          IF OP1 EQ 'X'.
            LOOP AT IT_VBRK.
              CLEAR: V_NAME, T_TLINE-TDLINE, T_TLINE, T_TLINE[].
              T_TLINE-TDLINE = P_DUDAT.
              APPEND T_TLINE TO T_TLINE.
              V_NAME = IT_VBRK-VBELN.
              " For Due date
              CALL FUNCTION 'CREATE_TEXT'
                EXPORTING
                  FID       = '0002'
                  FLANGUAGE = SY-LANGU
                  FNAME     = V_NAME
                  FOBJECT   = 'VBBK'
                TABLES
                  FLINES    = T_TLINE.
              CLEAR: T_TLINE, T_TLINE[].
              T_TLINE-TDLINE = P_PEROD.
              APPEND T_TLINE TO T_TLINE.
              " For period
              CALL FUNCTION 'CREATE_TEXT'
                EXPORTING
                  FID       = 'Z033'
                  FLANGUAGE = SY-LANGU
                  FNAME     = V_NAME
                  FOBJECT   = 'VBBK'
                TABLES
                  FLINES    = T_TLINE.
              IF SY-SUBRC <> 0.
                WRITE:/ 'Due date and period information update failed for Invoice#:' && IT_VBRK-VBELN.
              ENDIF.
            ENDLOOP.
            IF IT_VBRK[] IS INITIAL.
              MESSAGE 'No data found to process as per you given input parameters!' TYPE 'I'.
            ELSE.
              MESSAGE 'Process completed' TYPE 'I'.
            ENDIF.
*--- For Invoice download process
          ELSEIF OP2 EQ 'X'.
            LOOP AT IT_VBRK.
              CLEAR: P_FORM, ST_JOB_OUTPUT_INFO_MAIN, IT_DOCS[], IT_DOCS, IT_LINES[], IT_LINES, V_BIN_FILESIZE.
              IF IT_VBRK-BUKRS+0(1) EQ 'I'.
                P_FORM = 'ZAC_INVOICE_SUM_IN'.
              ELSEIF IT_VBRK-BUKRS+0(1) EQ 'A'.
                P_FORM = 'ZAC_INVOICE_SUM_BD'.
              ELSEIF IT_VBRK-BUKRS+0(1) EQ 'E'.
                P_FORM = 'ZAC_INVOICE_SUM_DXB'.
              ELSEIF IT_VBRK-BUKRS+0(1) EQ 'Q'.
                P_FORM = 'ZAC_INVOICE_SUM_QTR'.
              ELSE.
                P_FORM = 'ZAC_INVOICE_SUM'.
              ENDIF.
              PERFORM GEN_SMARTFORMS.

              CALL FUNCTION 'CONVERT_OTF_2_PDF'
                IMPORTING
                  BIN_FILESIZE           = V_BIN_FILESIZE
                TABLES
                  OTF                    = ST_JOB_OUTPUT_INFO_MAIN-OTFDATA
                  DOCTAB_ARCHIVE         = IT_DOCS
                  LINES                  = IT_LINES
                EXCEPTIONS
                  ERR_CONV_NOT_POSSIBLE  = 1
                  ERR_OTF_MC_NOENDMARKER = 2
                  OTHERS                 = 3.
              IF SY-SUBRC = 0.
                CLEAR: V_FILENAME.
                CONCATENATE DOWNLOAD '\' IT_VBRK-VKORG '-' IT_VBRK-NAME1+0(10) '-' IT_VBRK-FKDAT+6(2) '.' IT_VBRK-FKDAT+4(2) '.' IT_VBRK-FKDAT+2(2) '-INV-' IT_VBRK-VBELN '.pdf'  INTO V_FILENAME.

                CALL FUNCTION 'GUI_DOWNLOAD'
                  EXPORTING
                    BIN_FILESIZE            = V_BIN_FILESIZE
                    FILENAME                = V_FILENAME
                    FILETYPE                = 'BIN'
                    CONFIRM_OVERWRITE       = ' '
                  TABLES
                    DATA_TAB                = IT_LINES
                  EXCEPTIONS
                    FILE_WRITE_ERROR        = 1
                    NO_BATCH                = 2
                    GUI_REFUSE_FILETRANSFER = 3
                    INVALID_TYPE            = 4
                    NO_AUTHORITY            = 5
                    UNKNOWN_ERROR           = 6
                    HEADER_NOT_ALLOWED      = 7
                    SEPARATOR_NOT_ALLOWED   = 8
                    FILESIZE_NOT_ALLOWED    = 9
                    HEADER_TOO_LONG         = 10
                    DP_ERROR_CREATE         = 11
                    DP_ERROR_SEND           = 12
                    DP_ERROR_WRITE          = 13
                    UNKNOWN_DP_ERROR        = 14
                    ACCESS_DENIED           = 15
                    DP_OUT_OF_MEMORY        = 16
                    DISK_FULL               = 17
                    DP_TIMEOUT              = 18
                    FILE_NOT_FOUND          = 19
                    DATAPROVIDER_EXCEPTION  = 20
                    CONTROL_FLUSH_ERROR     = 21
                    OTHERS                  = 22.
                IF SY-SUBRC <> 0.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF IT_VBRK[] IS INITIAL.
              MESSAGE 'No data found to process as per you given input parameters!' TYPE 'I'.
            ELSE.
              MESSAGE 'Process completed' TYPE 'I'.
            ENDIF.
*--- For automail process
          ELSEIF OP3 EQ 'X'.
            CLEAR: IT_ADR6, IT_ADR6[], P_FORM.
            SORT: IT_VBRK BY ADRNR, IT_ADR6 BY ADDRNUMBER.
            SELECT * FROM ADR6 INTO CORRESPONDING FIELDS OF TABLE IT_ADR6 FOR ALL ENTRIES IN IT_VBRK
              WHERE ADDRNUMBER = IT_VBRK-ADRNR.
            LOOP AT IT_VBRK.
              READ TABLE IT_ADR6 WITH KEY ADDRNUMBER = IT_VBRK-ADRNR BINARY SEARCH.
              IF SY-SUBRC EQ 0.
                IF IT_VBRK-BUKRS+0(1) EQ 'I'.
                  P_FORM = 'ZAC_INVOICE_SUM_IN'.
                ELSEIF IT_VBRK-BUKRS+0(1) EQ 'A'.
                  P_FORM = 'ZAC_INVOICE_SUM_BD'.
                ELSEIF IT_VBRK-BUKRS+0(1) EQ 'E'.
                  P_FORM = 'ZAC_INVOICE_SUM_DXB'.
                ELSEIF IT_VBRK-BUKRS+0(1) EQ 'Q'.
                  P_FORM = 'ZAC_INVOICE_SUM_QTR'.
                ELSE.
                  P_FORM = 'ZAC_INVOICE_SUM'.
                ENDIF.
                PERFORM GEN_SMARTFORMS.

                CALL FUNCTION 'CONVERT_OTF_2_PDF'
                  IMPORTING
                    BIN_FILESIZE           = V_BIN_FILESIZE
                  TABLES
                    OTF                    = ST_JOB_OUTPUT_INFO_MAIN-OTFDATA
                    DOCTAB_ARCHIVE         = IT_DOCS
                    LINES                  = IT_LINES
                  EXCEPTIONS
                    ERR_CONV_NOT_POSSIBLE  = 1
                    ERR_OTF_MC_NOENDMARKER = 2
                    OTHERS                 = 3.
*--- For mail processing
* ASSIGNING THE DESCRIPTION OF THE OBJECT SENT IN THE MAIL
                CLEAR: GS_DOCDATA, MONTH, FFNSFN.
                GS_DOCDATA-OBJ_NAME = 'Inv'.
                MONTH = IT_VBRK-FKDAT+4(2).

                IF IT_VBRK-FKDAT+6(2) BETWEEN 15 AND 27.
                  MONTH = MONTH.
                ELSE.
                  MONTH = MONTH - 1.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = MONTH
                    IMPORTING
                      OUTPUT = MONTH.
                ENDIF.
*
*                CASE MONTH.
*                  WHEN '01'.
*                    GS_DOCDATA-OBJ_DESCR = 'JAN'.
*                  WHEN '02'.
*                    GS_DOCDATA-OBJ_DESCR = 'FEB'.
*                  WHEN '03'.
*                    GS_DOCDATA-OBJ_DESCR = 'MAR'.
*                  WHEN '04'.
*                    GS_DOCDATA-OBJ_DESCR = 'APR'.
*                  WHEN '05'.
*                    GS_DOCDATA-OBJ_DESCR = 'MAY'.
*                  WHEN '06'.
*                    GS_DOCDATA-OBJ_DESCR = 'JUN'.
*                  WHEN '07'.
*                    GS_DOCDATA-OBJ_DESCR = 'JUL'.
*                  WHEN '08'.
*                    GS_DOCDATA-OBJ_DESCR = 'AUG'.
*                  WHEN '09'.
*                    GS_DOCDATA-OBJ_DESCR = 'SEP'.
*                  WHEN '10'.
*                    GS_DOCDATA-OBJ_DESCR = 'OCT'.
*                  WHEN '11'.
*                    GS_DOCDATA-OBJ_DESCR = 'NOV'.
*                  WHEN '12'.
*                    GS_DOCDATA-OBJ_DESCR = 'DEC'.
*                ENDCASE.


                CLEAR: V_NAME,T_TLINE,T_TLINE[].
                V_NAME = IT_VBRK-VBELN.

                CALL FUNCTION 'READ_TEXT'
                  EXPORTING
                    ID                      = 'Z033'
                    LANGUAGE                = SY-LANGU
                    NAME                    = V_NAME
                    OBJECT                  = 'VBBK'
                  TABLES
                    LINES                   = T_TLINE
                  EXCEPTIONS
                    ID                      = 1
                    LANGUAGE                = 2
                    NAME                    = 3
                    NOT_FOUND               = 4
                    OBJECT                  = 5
                    REFERENCE_CHECK         = 6
                    WRONG_ACCESS_TO_ARCHIVE = 7
                    OTHERS                  = 8.
                IF SY-SUBRC <> 0.
                ENDIF.
                READ TABLE T_TLINE INDEX 1.
                SELECT SINGLE BEZEI INTO STR FROM TVKBT WHERE VKBUR EQ IT_VBRK-VKORG.
                SPLIT  IT_VBRK-NAME1 AT SPACE INTO TABLE ITAB.

                READ TABLE ITAB INDEX 1.
                CONCATENATE CUSTOMER_INITIAL ITAB-FLD INTO CUSTOMER_INITIAL SEPARATED BY SPACE.
                READ TABLE ITAB INDEX 2.
                CONCATENATE CUSTOMER_INITIAL ITAB-FLD INTO CUSTOMER_INITIAL SEPARATED BY SPACE.

                CONCATENATE STR T_TLINE-TDLINE CUSTOMER_INITIAL  GS_DOCDATA-OBJ_DESCR
                  INTO GS_DOCDATA-OBJ_DESCR SEPARATED BY SPACE.


* ASSIGNING THE EMAIL ID TO STRUCTURE OF THE API RECIPIENT LIST TABLE
                CLEAR : GT_RECLIST, GS_RECLIST.
                LOOP AT IT_ADR6 WHERE ADDRNUMBER = IT_VBRK-ADRNR.
*  IF INTERNAL MAIL ID
                  GS_RECLIST-RECEIVER = IT_ADR6-SMTP_ADDR.
*  GS_RECLIST-RECEIVER = SY-UNAME.
                  GS_RECLIST-REC_TYPE = 'U'.
                  APPEND GS_RECLIST TO GT_RECLIST.
                  CLEAR GS_RECLIST.
                ENDLOOP.

* PASSING THE SAP SCRIPT TEXT LINES TO SAPOFFICE: SINGLE LIST WITH COLUMN LENGTH 255 TABLE
                CLEAR : GS_OBJBIN, GS_PDF_TAB, GT_OBJBIN.
                LOOP AT IT_LINES INTO GS_PDF_TAB.
                  GV_POS = 255 - GV_LEN.
                  IF GV_POS > 134. "LENGTH OF PDF_TABLE
                    GV_POS = 134.
                  ENDIF.
                  GS_OBJBIN+GV_LEN = GS_PDF_TAB(GV_POS).
                  GV_LEN = GV_LEN + GV_POS.
                  IF GV_LEN = 255. "LENGTH OF OUT (CONTENTS_BIN)
                    APPEND GS_OBJBIN TO GT_OBJBIN.
                    CLEAR: GS_OBJBIN, GV_LEN.
                    IF GV_POS < 134.
                      GS_OBJBIN = GS_PDF_TAB+GV_POS.
                      GV_LEN = 134 - GV_POS.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
                IF GV_LEN > 0.
                  APPEND GS_OBJBIN TO GT_OBJBIN.
                ENDIF.

                DATA:
               I_OBJTXT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE.
                DATA LV_TXT TYPE STRING.
                CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
                  EXPORTING
                    INPUT_STRING  = IT_VBRK-NAME1
*                   SEPARATORS    = ' -.,;:'
                  IMPORTING
                    OUTPUT_STRING = IT_VBRK-NAME1.

                CLEAR MATNR.
                SELECT SINGLE MATNR INTO MATNR FROM VBRP WHERE VBELN = IT_VBRK-VBELN.

                I_OBJTXT = 'Dear Business Partner,'.
                APPEND I_OBJTXT.
                CLEAR I_OBJTXT.
                APPEND I_OBJTXT.
                DATA LV_BODY TYPE STRING.
                CASE MATNR.
                  WHEN 'AC-G9' OR 'AC-E5'.
                    LV_BODY = 'Air Arabia'.
                  WHEN 'AC-SG'.
                    LV_BODY = 'Spice Jet'.
                  WHEN 'AC-8M'.
                    LV_BODY = 'Myanmar Air'.
                  WHEN 'AC-8D'.
                    LV_BODY = 'Fits Air'.
                  WHEN OTHERS.
                    LV_BODY = 'Air Cargo'.
                ENDCASE.
                CONCATENATE 'Please find attached CSR Invoice for' IT_VBRK-NAME1 LV_BODY '.'
                            INTO LV_BODY SEPARATED BY SPACE.
                I_OBJTXT = LV_BODY.
                APPEND I_OBJTXT.

                CLEAR I_OBJTXT.
                APPEND I_OBJTXT.

                LV_BODY = 'Kindly confirm the correctness of the invoice within next three working days and after this we will assume the receivables as per the attached Invoice.'.
                I_OBJTXT = LV_BODY.
                APPEND I_OBJTXT.

                CLEAR I_OBJTXT.
                APPEND I_OBJTXT.

                I_OBJTXT = 'Regards,'.
                APPEND I_OBJTXT.
                I_OBJTXT = 'Finance Desk.'.
                APPEND I_OBJTXT.

                CLEAR GS_OBJPACK-TRANSF_BIN.
                DESCRIBE TABLE I_OBJTXT LINES  V_LINES_TXT.
                GS_OBJPACK-TRANSF_BIN = SPACE.
                GS_OBJPACK-HEAD_START = 1.
                GS_OBJPACK-HEAD_NUM = 0.
                GS_OBJPACK-BODY_START = 1.
                GS_OBJPACK-BODY_NUM = V_LINES_TXT.
                GS_OBJPACK-DOC_TYPE = 'RAW'.
                APPEND GS_OBJPACK TO GT_OBJPACK.
* FILLING THE DETAILS IN SAPOFFICE: DESCRIPTION OF IMPORTED OBJECT COMPONENTS TABLE
                DESCRIBE TABLE GT_OBJBIN LINES GV_TAB_LINES.
                CLEAR GS_OBJBIN.
                READ TABLE GT_OBJBIN INTO GS_OBJBIN INDEX GV_TAB_LINES.
                IF SY-SUBRC = 0.
                  GS_OBJPACK-DOC_SIZE = ( GV_TAB_LINES - 1 ) * 255 + STRLEN( GS_OBJBIN ).
                  GS_OBJPACK-TRANSF_BIN = 'X'.
                  GS_OBJPACK-HEAD_START = 1.
                  GS_OBJPACK-HEAD_NUM = 0.
                  GS_OBJPACK-BODY_START = 1.
                  GS_OBJPACK-BODY_NUM = GV_TAB_LINES.
                  GS_OBJPACK-DOC_TYPE = 'PDF'.
                  GS_OBJPACK-OBJ_NAME = 'attachment'.
                  DATA LV_DESCR TYPE STRING.
                  CONCATENATE IT_VBRK-VKORG '-' IT_VBRK-NAME1+0(10) '-' IT_VBRK-FKDAT+6(2) '.' IT_VBRK-FKDAT+4(2) '.' IT_VBRK-FKDAT+2(2) '-INV-' IT_VBRK-VBELN INTO LV_DESCR.
                  GS_OBJPACK-OBJ_DESCR = LV_DESCR.
                  APPEND GS_OBJPACK TO GT_OBJPACK.
                ENDIF.

* SENDING THE FORM OUTPUT IN THE PDF FORMAT TO EMAIL
                CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
                  EXPORTING
                    DOCUMENT_DATA              = GS_DOCDATA
                    PUT_IN_OUTBOX              = 'X'
                    COMMIT_WORK                = 'X'
                  TABLES
                    PACKING_LIST               = GT_OBJPACK
                    CONTENTS_BIN               = GT_OBJBIN
                    CONTENTS_TXT               = I_OBJTXT
                    RECEIVERS                  = GT_RECLIST
                  EXCEPTIONS
                    TOO_MANY_RECEIVERS         = 1
                    DOCUMENT_NOT_SENT          = 2
                    DOCUMENT_TYPE_NOT_EXIST    = 3
                    OPERATION_NO_AUTHORIZATION = 4
                    PARAMETER_ERROR            = 5
                    X_ERROR                    = 6
                    ENQUEUE_ERROR              = 7
                    OTHERS                     = 8.
                IF SY-SUBRC <> 0.
                  CLEAR FLAG2.
                  CLEAR IT_INFO.
                  IT_INFO-ICON_FIELD = '@0A@'.
                  CONCATENATE 'Automail failed to send for Invoice No.' IT_VBRK-VBELN '. Check E-Mail addresses in customer master data' IT_VBRK-KUNRG
                  INTO IT_INFO-RMKS SEPARATED BY SPACE.
                  APPEND IT_INFO TO IT_INFO.
                  CLEAR IT_INFO.
                ELSE.
*    WRITE 'SENT SUCCESSFULLY'.
                  CLEAR FLAG2.
                  FLAG2 = 'X'.
                  CLEAR IT_INFO.
                  IT_INFO-ICON_FIELD = '@08@'.
                  CONCATENATE 'Automail successfully send for Invoice No.' IT_VBRK-VBELN '. For customer' IT_VBRK-KUNRG
                  INTO IT_INFO-RMKS SEPARATED BY SPACE.
                  APPEND IT_INFO TO IT_INFO.
                  CLEAR IT_INFO.
                ENDIF.
                SUBMIT RSCONN01
                WITH MODE EQ 'INT'
                AND RETURN.
                CLEAR: GS_DOCDATA,GT_OBJPACK, GT_OBJPACK[],GT_OBJBIN, GT_OBJBIN[],I_OBJTXT,I_OBJTXT[],GT_RECLIST,GT_RECLIST[],
                V_BIN_FILESIZE, ST_JOB_OUTPUT_INFO_MAIN-OTFDATA[],ST_JOB_OUTPUT_INFO_MAIN,IT_DOCS,IT_DOCS[],IT_LINES,IT_LINES[].
              ELSE.
                CLEAR IT_INFO.
                IT_INFO-ICON_FIELD = '@0A@'.
                CONCATENATE 'Automail failed to send for Invoice No.' IT_VBRK-VBELN '. E-Mail address not found in customer master data' IT_VBRK-KUNRG
                INTO IT_INFO-RMKS SEPARATED BY SPACE.
                APPEND IT_INFO TO IT_INFO.
                CLEAR IT_INFO.
              ENDIF.
            ENDLOOP.
            PERFORM BUILD_FIELDCAT.
            PERFORM FILL_LIST_HEADER  USING LT_TOP_OF_PAGE[].
            PERFORM EVENTTAB_BUILD USING GT_EVENTS[].
            PERFORM ALV_DISPLAY.
          ENDIF.
        ELSE.
          MESSAGE 'No Billing document found to process!!' TYPE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  INPUTPARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INPUTPARAM USING P_DATE-LOW P_DATE-HIGH.
  CLEAR: DATEDIFF.
  IF FLAG NE 'X'.
    PERFORM DATE_BETWEEN USING P_DATE-LOW P_DATE-HIGH.
    IF OP1 EQ 'X'.
      IF DATEDIFF GT 15.
        MESSAGE TEXT-E01 TYPE 'I'. FLAG = 'X'.
      ENDIF.
    ELSE.
      IF DATEDIFF GT 365.
        MESSAGE TEXT-E02 TYPE 'I'. FLAG = 'X'.
      ENDIF.

      IF OP1 EQ 'X' AND ( P_DUDAT IS INITIAL OR P_PEROD IS INITIAL ).
        MESSAGE 'Please provide Due Date & Period Information both to update!' TYPE 'I'.
        FLAG = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF OP1 EQ 'X' AND ( RT2 EQ 'X' OR RT3 EQ 'X' ).
    MESSAGE 'Please change the processing type to proceed!' TYPE 'I'.
    FLAG = 'X'.
  ENDIF.
  IF OP3 EQ 'X' AND ( RT2 EQ 'X' OR RT3 EQ 'X' ).
    MESSAGE 'Automail can be send for due invoices only!' TYPE 'I'.
    FLAG = 'X'.
  ENDIF.
  IF OP2 EQ 'X' AND DOWNLOAD IS INITIAL.
    MESSAGE 'Please specify invoice download folder path' TYPE 'I'.
    FLAG = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATE_BETWEEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DAT_LOW  text
*      -->DAT_HIGH  text
*----------------------------------------------------------------------*
FORM DATE_BETWEEN  USING DAT_LOW TYPE DATUM
                         DAT_HIGH TYPE DATUM.
  CLEAR DATEDIFF.
  IF DAT_HIGH IS INITIAL.
    DAT_HIGH = DAT_LOW.
  ENDIF.
  IF DAT_LOW IS INITIAL.
    DAT_LOW = DAT_HIGH.
  ENDIF.
  CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
    EXPORTING
      BEGDA = DAT_LOW
      ENDDA = DAT_HIGH
    IMPORTING
      DAYS  = DATEDIFF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GEN_SMARTFORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GEN_SMARTFORMS .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = P_FORM
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  OUTOP-TDDEST = 'LP01'.
  CPARAM-NO_DIALOG = 'X'.
  CPARAM-PREVIEW = SPACE.
  CPARAM-GETOTF = 'X'.
  CALL FUNCTION FM_NAME
    EXPORTING
      CONTROL_PARAMETERS = CPARAM
      OUTPUT_OPTIONS     = OUTOP
      USER_SETTINGS      = SPACE
      INV_NO             = IT_VBRK-VBELN
      INV_TYP            = 'ZACD'
      FKSTO              = ''
    IMPORTING
      JOB_OUTPUT_INFO    = ST_JOB_OUTPUT_INFO
*    TABLES
*     IT_LIST            = IT_LIST
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  APPEND LINES OF ST_JOB_OUTPUT_INFO-OTFDATA TO ST_JOB_OUTPUT_INFO_MAIN-OTFDATA.
  REFRESH ST_JOB_OUTPUT_INFO-OTFDATA.
ENDFORM.
FORM FILL_LIST_HEADER  USING   P_SLIS_T_LISTHEADER TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  STR = 'Automail processing status'.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = STR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  CLEAR LS_LINE.

ENDFORM.
FORM TOP_OF_PAGE.                                           "#EC CALLED
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      I_LOGO             = ''
      IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
FORM BUILD_FIELDCAT .
  DATA: FIELDCAT TYPE SLIS_FIELDCAT_ALV,COL_POS TYPE I .
  CLEAR: COL_POS.
  COL_POS = 1.
  CLEAR FIELDCAT.
  FIELDCAT-FIELDNAME     = 'ICON_FIELD'.
  FIELDCAT-TABNAME       = 'IT_INFO'.
  FIELDCAT-SELTEXT_M     = 'Status'.
  FIELDCAT-HOTSPOT       = 'X'.
  FIELDCAT-COL_POS       = COL_POS.
  FIELDCAT-OUTPUTLEN     = 5.
  APPEND FIELDCAT TO T_FIELDCAT.

  COL_POS = COL_POS + 1.
  CLEAR FIELDCAT.
  FIELDCAT-FIELDNAME     = 'RMKS'.
  FIELDCAT-TABNAME       = 'IT_INFO'.
  FIELDCAT-SELTEXT_M     = 'Remarks'.
  FIELDCAT-COL_POS       = COL_POS.
  FIELDCAT-OUTPUTLEN       = 145.
  APPEND FIELDCAT TO T_FIELDCAT.

ENDFORM.

FORM   EVENTTAB_BUILD  USING  LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
  MOVE 'TOP_OF_PAGE' TO LS_EVENT-NAME.
  MOVE 'TOP_OF_PAGE' TO LS_EVENT-FORM.
  APPEND LS_EVENT TO LT_EVENTS.
  CLEAR LS_EVENT.

ENDFORM.

FORM ALV_DISPLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_BUFFER_ACTIVE    = SPACE
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT          = LS_LAYOUT
      IT_FIELDCAT        = T_FIELDCAT[]
*     I_CALLBACK_USER_COMMAND = C_USER_COMMAND
      IT_SORT            = T_SORT[]
      I_SAVE             = 'X'
      IT_EVENTS          = GT_EVENTS[]
    TABLES
      T_OUTTAB           = IT_INFO[]
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

  IF SY-SUBRC <> 0.
  ENDIF.

ENDFORM.