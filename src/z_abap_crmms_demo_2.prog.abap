*&---------------------------------------------------------------------*
*& Report Z_ABAP_CRMMS_DEMO_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_ABAP_CRMMS_DEMO_2.

DATA(lc_crm) = NEW Z_CL_ABAP_CRMMS_TOOLS( ).

TYPES: BEGIN OF lty_account_list,
         id         TYPE string,  " this accountid  field with different name so we need mapping
         name       TYPE string,
         websiteurl type string,
       END OF lty_account_list.

DATA: lt_accounts TYPE TABLE OF lty_account_list.

  DATA: LT_Model_SELECT TYPE /UI2/CL_JSON=>NAME_MAPPINGS .
  LT_Model_SELECT = VALUE #(
        ( abap = 'ID' json = 'accountid' )
      ).


lc_crm->get_entities(
  EXPORTING
    i_entities                 = 'accounts'
    i_limit                    = 10
    i_name_mappings            = LT_Model_SELECT
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
  WRITE: / 'Accfount ID: ', <row>-id,
         / 'Name       : ', <row>-name,
         / 'WebSite URL: ', <row>-websiteurl.

   " sample read detail account from key
   TYPES: BEGIN OF lty_account_detail,
     id type string,
     name type string,
     emailaddress1 type string,
     _owninguser_value type string,
     user        type string,
     _owningbusinessunit_value type string,
     businessunit type string,
     _transactioncurrencyid_value type string,
     isocur type string,
   END OF lty_account_detail.
   data: ls_account type lty_account_detail.

  DATA: LT_Model_ENTITY  TYPE Z_CL_ABAP_CRMMS_TOOLS=>entity_mappings.
  LT_Model_ENTITY = VALUE #(
        ( abap = 'ID' json = 'accountid' )
        ( abap = 'isocur' ENTITY = 'transactioncurrencies' SELECT = 'isocurrencycode' key = '_transactioncurrencyid_value' )
        ( abap = 'user' ENTITY = 'systemusers' SELECT = 'fullname' key = '_owninguser_value' )
        ( abap = 'businessunit' Entity = 'businessunits' SELECT = 'name' key = '_owningbusinessunit_value' )
      ).


   lc_crm->get_entity(
         EXPORTING
        i_entity                   = 'accounts'
        i_key                      = <row>-id
        i_entity_mappings          = lt_model_entity
      CHANGING
        e_entity                   = ls_account
       EXCEPTIONS
         missing_token              = 1
         http_communication_failure = 2
         json_error                 = 3
         OTHERS                     = 4
    ).

   WRITE: / '     Details:',
          / '             ID ', ls_account-id,
          / '             Name ', ls_account-name,
          / '             Email Address: ', ls_account-emailaddress1,
          / '             User         : ', ls_account-user,
          / '             Business Unit: ', ls_account-businessunit,
          / '             Currency     : ', ls_account-isocur.

uline.

ENDLOOP.
write: / 'end'.
