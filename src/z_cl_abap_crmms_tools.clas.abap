class Z_CL_ABAP_CRMMS_TOOLS definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ENTITY_MAPPING,
      abap    TYPE STRING,
      json    TYPE STRING,
      ENTITY  TYPE string,
      SELECT  TYPE STRING,
      key     TYPE STRING ,
    END OF ENTITY_MAPPING .
  types:
    ENTITY_MAPPINGs TYPE HASHED TABLE OF ENTITY_MAPPING WITH UNIQUE KEY abap .
  types:
    BEGIN OF TY_MAP,
      ENTITY  TYPE STRING,
      KEY_NAME  TYPE STRING,
      KEY	TYPE STRING,
      SELECT  TYPE STRING ,
      json  TYPE string,
    END OF TY_MAP .
  types:
    TY_MAPS TYPE HASHED TABLE OF TY_MAP WITH UNIQUE KEY entity key_name key select .

  methods CONSTRUCTOR
    importing
      !I_LOGICAL_NAME type STRING optional
    exceptions
      NO_CONFIGURATION_FOUND .
  methods GET_TOKEN
    returning
      value(E_TOKEN) type STRING
    exceptions
      HTTP_COMMUNICATION_FAILURE .
  methods GET_ENTITIES
    importing
      !I_ENTITIES type STRING
      !I_FILTER type STRING default SPACE
      !I_SELECT type STRING default SPACE
      !I_NAME_MAPPINGS type /UI2/CL_JSON=>NAME_MAPPINGS optional
      !I_LIMIT type NUMC4 default 0
    changing
      value(E_TABLES) type STANDARD TABLE
    exceptions
      MISSING_TOKEN
      HTTP_COMMUNICATION_FAILURE .
  methods GET_ENTITY
    importing
      !I_ENTITY type STRING
      !I_KEY_NAME type STRING optional
      !I_KEY type STRING
      !I_ENTITY_MAPPINGS type ENTITY_MAPPINGS optional
      !I_SELECT type STRING optional
      !I_BYPASS_CACHE type BOOLEAN optional
    changing
      value(E_ENTITY) type DATA optional
    exceptions
      MISSING_TOKEN
      HTTP_COMMUNICATION_FAILURE
      JSON_ERROR
      ID_UNKNOW .
  methods GET_ENTITY_VALUE_FOR_KEY
    importing
      !I_ENTITY type STRING
      !I_KEY_NAME type STRING optional
      !I_KEY type STRING
      !I_SELECT type STRING
    returning
      value(E_VALUE) type STRING .
  methods INVALIDATE_CACHE .
protected section.
private section.

  data GV_URL_LOGIN type STRING .
  data GV_GRANT_TYPE type STRING .
  data GV_CLIENT_SECRET type STRING .
  data GV_CLIENT_ID type STRING .
  data GV_SCOPE type STRING .
  data GV_URL_API type STRING .
  data GV_TOKEN type STRING .
  data GT_CACHE type TY_MAPS .

  methods GET_CACHE_FOR
    importing
      !I_ENTITY type STRING
      !I_KEY_NAME type STRING optional
      !I_KEY type STRING
      !I_SELECT type STRING optional
    returning
      value(E_JSON) type STRING
    exceptions
      MISSING_TOKEN
      HTTP_COMMUNICATION_FAILURE .
  methods PUT_CACHE_FOR
    importing
      !I_ENTITY type STRING
      !I_KEY_NAME type STRING optional
      !I_KEY type STRING
      !I_SELECT type STRING optional
      value(I_JSON) type STRING
    exceptions
      MISSING_TOKEN
      HTTP_COMMUNICATION_FAILURE .
ENDCLASS.



CLASS Z_CL_ABAP_CRMMS_TOOLS IMPLEMENTATION.


  METHOD constructor.


    DATA ls_conf TYPE zabap_crmms_conf.
    IF i_logical_name IS SUPPLIED.
      SELECT SINGLE *
        INTO ls_conf
        FROM zabap_crmms_conf
        WHERE logical_name = i_logical_name.
    ELSE.
      DATA: count TYPE i.
      SELECT COUNT( * ) INTO count
       FROM zabap_crmms_conf.
      IF count = 1.
        SELECT SINGLE *
          INTO ls_conf
          FROM zabap_crmms_conf.
      ELSEIF count > 1.
        SELECT SINGLE *
          INTO ls_conf
          FROM zabap_crmms_conf
          WHERE logical_name = 'DEFAULT'.
      ENDIF.
    ENDIF.

    IF ls_conf IS INITIAL.
      RAISE no_configuration_found.
    ENDIF.

    gv_url_login = ls_conf-token_url .
    gv_grant_type = 'client_credentials'.
    gv_client_secret = ls_conf-client_secret .
    gv_client_id = ls_conf-client_id .
    gv_scope = |{ ls_conf-base_url }| & |/.default|.
    gv_url_api = |{ ls_conf-base_url }| & |/api/data/v9.2|.


  ENDMETHOD.


  METHOD GET_CACHE_FOR.
    CLEAR e_json.

    DATA: ls_cache_row TYPE ty_map.

    READ TABLE gt_cache WITH TABLE KEY
      entity   = i_entity
      key_name = i_key_name
      key	     = i_key
      select   = i_select
      INTO ls_cache_row.

    IF sy-subrc EQ 0.
      e_json = ls_cache_row-json.
      return.
    ENDIF.

    READ TABLE gt_cache WITH TABLE KEY
      entity   = i_entity
      key_name = i_key_name
      key	     = i_key
      select   = space
      INTO ls_cache_row.

    IF sy-subrc EQ 0.
      e_json = ls_cache_row-json.
      return.
    ENDIF.


  ENDMETHOD.


  method GET_ENTITIES.

    me->get_token( ).

    data: lv_url type string.
    data: lv_par. lv_par = '?'.
    CONCATENATE gv_url_api '/' i_entities into lv_url.
    if i_select ne space.
      CONCATENATE lv_url lv_par '$select=' i_select into lv_url.
      lv_par = '&'.
    endif.
    if i_filter ne space.
      CONCATENATE lv_url lv_par '$filter=' i_filter into lv_url.
      lv_par = '&'.
    endif.

    if i_limit > 0.
      CONCATENATE lv_url lv_par '$top=' i_limit  into lv_url.
      lv_par = '&'.
    endif.

  data lc_client  type ref to if_http_client.
  cl_http_client=>create_by_url( exporting url = lv_url
                                 importing client = lc_client
                                   exceptions
                                    argument_not_found = 1
                                    plugin_not_active  = 2
                                    internal_error     = 3
                                    others             = 4 ).
  if sy-subrc <> 0.
    raise http_communication_failure.
  endif.

  data lv_str type string.
  concatenate 'Bearer' gv_token into lv_str
  separated by space.

  lc_client->request->set_header_field( name = 'Authorization'
                                        value = lv_str ).

* HTTP POST request To Server
  call method lc_client->send
    exceptions
      http_communication_failure = 01
      http_invalid_state         = 02.
  if sy-subrc ne 0.
    raise http_communication_failure.
  endif.

* HTTP POST answer from client
  call method lc_client->receive
    exceptions
      http_communication_failure = 01
      http_invalid_state         = 02.
  if sy-subrc ne 0.
    raise http_communication_failure.
  endif.

  data: lv_code type i.
  data: lv_message type string.
  lc_client->response->get_status(
               importing code = lv_code
                         reason = lv_message ).

  lv_str = lc_client->response->get_cdata( ).

  TYPES:
    BEGIN OF tp_s_data,
      odatacontext        TYPE string,
      value               TYPE /ui2/cl_json=>json,
    END OF tp_s_data.
  DATA: ls_exp      TYPE tp_s_data,
        lt_mapping  TYPE /ui2/cl_json=>name_mappings.

  lt_mapping = VALUE #( ( abap = 'ODATACONTEXT' json = '@odata.context' ) ).

  /ui2/cl_json=>deserialize(
            exporting
                json = lv_str
                NAME_MAPPINGS	= lt_mapping
            changing
                data = ls_exp ).

  /ui2/cl_json=>deserialize(
            exporting
                json = ls_exp-value
                NAME_MAPPINGS	= i_name_mappings
            changing
                data = e_tables ).

  lc_client->close( ).

  endmethod.


  METHOD GET_ENTITY.

    DATA lv_cache TYPE string.
    IF i_bypass_cache = space.
      lv_cache = get_cache_for( EXPORTING
                                    i_entity = i_entity
                                    i_key_name = i_key_name
                                    i_key = i_key
                                    i_select = i_select ).
    ENDIF.
    IF lv_cache EQ space.
      me->get_token( ).

      IF gv_token EQ space. RAISE missing_token. ENDIF.

      DATA: lv_url TYPE string.
      DATA: lv_par.
      lv_par = '?'.
      CONCATENATE gv_url_api '/' i_entity '(' INTO lv_url.
      IF i_key_name NE space.
        CONCATENATE lv_url i_key_name '=' INTO lv_url.
      ENDIF.
      CONCATENATE lv_url i_key ')' INTO lv_url.
      IF i_select NE space.
        CONCATENATE lv_url '?$select=' i_select INTO lv_url.
      ELSE.
        " TODO retrive list from structure
      ENDIF.

      DATA lc_client  TYPE REF TO if_http_client.
      cl_http_client=>create_by_url( EXPORTING url = lv_url
                                     IMPORTING client = lc_client
                                       EXCEPTIONS
                                        argument_not_found = 1
                                        plugin_not_active  = 2
                                        internal_error     = 3
                                        OTHERS             = 4 ).
      IF sy-subrc <> 0.
        RAISE http_communication_failure.
      ENDIF.

      DATA lv_str TYPE string.
      CONCATENATE 'Bearer' gv_token INTO lv_str
      SEPARATED BY space.

      lc_client->request->set_header_field( name = 'Authorization'
                                            value = lv_str ).

* HTTP POST request To Server
      CALL METHOD lc_client->send
        EXCEPTIONS
          http_communication_failure = 01
          http_invalid_state         = 02.
      IF sy-subrc NE 0.
        RAISE http_communication_failure.
      ENDIF.

* HTTP POST answer from client
      CALL METHOD lc_client->receive
        EXCEPTIONS
          http_communication_failure = 01
          http_invalid_state         = 02.
      IF sy-subrc NE 0.
        RAISE http_communication_failure.
      ENDIF.

      DATA: lv_code TYPE i.
      DATA: lv_message TYPE string.
      lc_client->response->get_status(
                   IMPORTING code = lv_code
                             reason = lv_message ).

      lv_str = lc_client->response->get_cdata( ).

      " Try to handle error
      TYPES: BEGIN OF error_node,
               code    TYPE string,
               message TYPE string,
             END OF error_node.
      DATA: BEGIN OF error,
              error TYPE error_node,
            END OF error.

      /ui2/cl_json=>deserialize(
                EXPORTING
                    json = lv_str
                CHANGING
                    data = error ).
      IF error-error-code IS NOT INITIAL.
        if error-error-code eq '0x80040217'.
          raise id_unknow.
        endif.
        MESSAGE s398(00) WITH error-error-code error-error-message.
        RAISE json_error.
      ENDIF.

      put_cache_for( EXPORTING
                            i_entity = i_entity
                            i_key_name = i_key_name
                            i_key = i_key
                            i_select = i_select
                            i_json   = lv_str ).

      lc_client->close( ).

    ELSE.
      lv_str = lv_cache.
    ENDIF.

    DATA lt_name_mappings	TYPE /ui2/cl_json=>name_mappings.

    FIELD-SYMBOLS <row> TYPE entity_mapping.
    DATA: ls_name_mapping TYPE /ui2/cl_json=>name_mapping.
    LOOP AT i_entity_mappings ASSIGNING <row>.
      IF <row>-json IS NOT INITIAL.
        ls_name_mapping-abap = <row>-abap.
        ls_name_mapping-json = <row>-json.
        INSERT ls_name_mapping INTO TABLE lt_name_mappings.
      ENDIF.
    ENDLOOP.

    /ui2/cl_json=>deserialize(
              EXPORTING
                  json = lv_str
                  name_mappings  = lt_name_mappings
              CHANGING
                  data = e_entity ).

    FIELD-SYMBOLS <fs_key> TYPE any.
    FIELD-SYMBOLS <fs_value> TYPE any. " type string.
    DATA: lv_data TYPE string.
    LOOP AT i_entity_mappings ASSIGNING <row> WHERE entity NE space.
      ASSIGN COMPONENT <row>-abap   OF STRUCTURE e_entity TO <fs_value>. " CASTING type string.
      CHECK <fs_value> IS ASSIGNED.
      ASSIGN COMPONENT <row>-key    OF STRUCTURE e_entity TO <fs_key>. "  CASTING type string.
      IF NOT <fs_key> IS ASSIGNED .
        " ops we don't have key field into structure. retrive from json
        DATA: lo_data TYPE REF TO /ui2/cl_data_access,
              lr_data TYPE REF TO data.

        lr_data = /ui2/cl_json=>generate( json = lv_str ).

        /ui2/cl_data_access=>create( ir_data = lr_data iv_component = <row>-key )->value( IMPORTING ev_data = lv_data ).
        ASSIGN lv_data TO <fs_key> .

      ENDIF.
      <fs_value> = get_entity_value_for_key( EXPORTING
        i_entity                   = <row>-entity
        i_select                   = <row>-select
        i_key                      = <fs_key> ).

      UNASSIGN: <fs_key>, <fs_value>.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_ENTITY_VALUE_FOR_KEY.

    TYPES: BEGIN OF lty_val,
             value TYPE string,
           END OF lty_val.

    DATA: ls_val TYPE lty_val.

    DATA lt_mapping  TYPE entity_mappings.

    lt_mapping = VALUE #( ( abap = 'VALUE' json = i_select ) ).

    CALL METHOD me->get_entity
      EXPORTING
        i_entity                   = i_entity
        i_key_name                 = i_key_name
        i_key                      = i_key
        i_entity_mappings          = lt_mapping
        i_select                   = i_select
      CHANGING
        e_entity                   = ls_val
      EXCEPTIONS
        missing_token              = 1
        http_communication_failure = 2
        json_error                 = 3
        OTHERS                     = 4.
    IF sy-subrc EQ 0.
      e_value = ls_val-value.
    ENDIF.

  ENDMETHOD.


  method GET_TOKEN.

  if gv_token ne space.
    e_token = gv_token.
    return.
  endif.

  data l_client  type ref to if_http_client.
  data l_message type string.
  data l_code_i type i.
  data: gs_json type string.
  data: lv_str   type string.

  clear l_client.

  cl_http_client=>create_by_url( exporting url = gv_url_login
                                 importing client = l_client
                                   exceptions
                                    argument_not_found = 1
                                    plugin_not_active  = 2
                                    internal_error     = 3
                                    others             = 4 ).
  if sy-subrc <> 0.
    RAISE HTTP_COMMUNICATION_FAILURE.
  endif.

* set method POST
  call method l_client->request->set_header_field
    exporting
      name  = '~request_method'
      value = 'POST'.

* declare the HTTP-Version
  call method l_client->request->set_header_field
    exporting
      name  = '~server_protocol'
      value = 'HTTP/1.0'.

  call method l_client->request->set_header_field
    exporting
      name  = 'Connection'                                  "#EC NOTEXT
      value = 'keep-alive'.                                      "#EC NOTEXT

* soapaction
  call method l_client->request->set_header_field
    exporting
      name  = 'soapaction'                                  "#EC NOTEXT
      value = 'http://sap.com/xi/WebService/soap1.1'.       "#EC NOTEXT

* accept every HTTP-response
  call method l_client->request->set_header_field
    exporting
      name  = 'Accept'                                      "#EC NOTEXT
      value = '*/*'.

  l_client->request->set_content_type( 'application/x-www-form-urlencoded' ).

  CONCATENATE 'grant_type=' gv_grant_type INTO gs_json. "'grant_type=client_credentials'.
  l_client->request->append_cdata( data = gs_json ).

  CONCATENATE '&client_secret=' gv_client_secret INTO gs_json.  "'&client_secret=G258Q~vNHGFfNxSv9AWDZJcSX0gBbHLDeA-rrdl-'.
  l_client->request->append_cdata( data = gs_json ).

  CONCATENATE '&client_id=' gv_client_id INTO gs_json. "'&client_id=882dbca9-93f1-45cd-80a0-490bf5a5e57c'.
  l_client->request->append_cdata( data = gs_json ).

  CONCATENATE '&scope=' gv_scope INTO gs_json. "'&scope=https://arsenalia.api.crm4.dynamics.com/.default'.
  l_client->request->append_cdata( data = gs_json ).

* HTTP POST request To Server
  call method l_client->send
    exceptions
      http_communication_failure = 01
      http_invalid_state         = 02.
  if sy-subrc ne 0.
    RAISE HTTP_COMMUNICATION_FAILURE.
  endif.
* HTTP POST answer from client
  call method l_client->receive
    exceptions
      http_communication_failure = 01
      http_invalid_state         = 02.
  if sy-subrc ne 0.
    RAISE HTTP_COMMUNICATION_FAILURE.
  endif.

  l_client->response->get_status(
               importing code = l_code_i
                         reason = l_message ).

  lv_str = l_client->response->get_cdata( ).

  l_client->close( ).

  if l_code_i ne 200.
    RAISE HTTP_COMMUNICATION_FAILURE.
  endif.

  data: begin of ls_output,
          token_type     type string,
          expires_in     type string,
          ext_expires_in type string,
          access_token   type string,
        end of ls_output.

  /ui2/cl_json=>deserialize(
            exporting
                json = lv_str
            changing
                data = ls_output   "Token will be stored in the instance structure, for retrieval in other methods.
 ).

  gv_token = ls_output-access_token.
  e_token = gv_token.

  if gv_token eq space.
    RAISE HTTP_COMMUNICATION_FAILURE.
  endif.
  endmethod.


  method INVALIDATE_CACHE.

    refresh gt_cache.

  endmethod.


  METHOD PUT_CACHE_FOR.

    DATA: ls_cache_row TYPE ty_map.

    READ TABLE gt_cache WITH TABLE KEY
      entity   = i_entity
      key_name = i_key_name
      key	     = i_key
      select   = i_select
      INTO ls_cache_row.

    IF sy-subrc EQ 0.
      ls_cache_row-json     = i_json.
      MODIFY TABLE gt_cache FROM ls_cache_row.
    ELSE.

      ls_cache_row-entity = i_entity.
      ls_cache_row-key_name = i_key_name.
      ls_cache_row-key       = i_key.
      ls_cache_row-select   = i_select.
      ls_cache_row-json     = i_json.

      INSERT ls_cache_row INTO table gt_cache.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
