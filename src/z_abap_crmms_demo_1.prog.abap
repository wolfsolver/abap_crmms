*&---------------------------------------------------------------------*
*& Report Z_ABAP_CRMMS_DEMO_1
*&---------------------------------------------------------------------*
*& Sample read. Load list of accounts
*&---------------------------------------------------------------------*
REPORT Z_ABAP_CRMMS_DEMO_1.

DATA(lc_crm) = NEW Z_CL_ABAP_CRMMS_TOOLS( ).

TYPES: BEGIN OF lty_account_list,
         accountid  TYPE string,
         name       TYPE string,
         websiteurl type string,
       END OF lty_account_list.

DATA: lt_accounts TYPE TABLE OF lty_account_list.

lc_crm->get_entities(
  EXPORTING
    i_entities                 = 'accounts'
    i_select                   = 'accountid,name,websiteurl'  " optional for performance
    i_limit                    = 10
  CHANGING
    e_tables                   = lt_accounts
   EXCEPTIONS
     missing_token              = 1
     http_communication_failure = 2
     OTHERS                     = 3
).
IF sy-subrc <> 0.
  WRITE: / 'Error'.
ENDIF.
FIELD-SYMBOLS <row> TYPE lty_account_list.

LOOP AT lt_accounts ASSIGNING <row>.
  WRITE: / 'Accfount ID: ', <row>-accountid,
         / 'Name       : ', <row>-name,
         / 'WebSite URL: ', <row>-websiteurl.
   uline.
ENDLOOP.
write: / 'end'.
