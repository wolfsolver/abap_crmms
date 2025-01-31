*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZABAP_CRMMS_CONF................................*
DATA:  BEGIN OF STATUS_ZABAP_CRMMS_CONF              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABAP_CRMMS_CONF              .
CONTROLS: TCTRL_ZABAP_CRMMS_CONF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABAP_CRMMS_CONF              .
TABLES: ZABAP_CRMMS_CONF               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
