# abap_crmms
Abap integration with Microsoft CRM
> Fast and easy way to read data from Microsoft CRM from ABAP.

[[_TOC_]]

# Idea behind
The core idea behind this integration is to simplify the reading of data from the CRM as much as possible. A dedicated class handles the interaction with the CRM, exposing two simple methods: get_entities and get_entity.

# How to Use - Basic Usage

The most simply way is to create abap structure that rappresent json data. For example reading an user can be done with
    
```ABAP
    DATA(lc_crm) = NEW z_cl_ars_tool_crmms( ).

    data: begin of user,
      systemuserid type string,
      domainname type string,
      fullname type string,
      createdon type datum,
    end of user.

    lc_crm->get_entity(
      EXPORTING
        i_entity                   = 'systemusers'
        i_key                      = lv_key
      CHANGING
        e_entity                   = user
    ).
```

## Creating instance

`    DATA(lc_crm) = NEW z_cl_ars_tool_crmms( ).`

Create an instance with default setting. Class constructor allow receiving paramters to override default.


## Get list from crm
Get_Entities method allow to retrive list of object.
- i_entities is crm entity to read
- i_filter specify filter
- i_select specify list of fields to read
- i_mapping (optional) can specify mapping name between json and crm

example
```
TYPES: BEGIN OF lty_opp,
         opportunityid         TYPE string,
         id                    TYPE string,    " ald_opportunitynumber
       END OF lty_opp.

DATA: lt_mapping  TYPE /ui2/cl_json=>name_mappings.
lt_mapping = VALUE #( ( abap = 'ID' json = 'ald_opportunitynumber' ) ).


lc_crm->get_entities(
  EXPORTING
    i_entities                 = 'opportunities'
    i_filter                   = '_ald_companyid_value%20eq%2072a8d326-f187-eb11-a812-000d3ab4add9'
    i_select                   = 'ald_opportunitynumber,opportunityid'
    I_NAME_MAPPINGS            = lt_mapping
  CHANGING
    e_tables                   = lt_opp
   EXCEPTIONS
     missing_token              = 1
     http_communication_failure = 2
     OTHERS                     = 3
).

FIELD-SYMBOLS <row> TYPE lty_opp.

LOOP AT lt_opp ASSIGNING <row>.
  WRITE: / 'Opp id:', <row>-opportunityid,
           'pretty id:', <row>-id.
ENDLOOP.
```

## Using mapping
mapping can be used to:
- define abap name different from json
- force lookup on crm table for value that have id reference (for example to lookup user in opportunitoes)

Simply define maping table like this 

```abap
  DATA: LT_Model_ENTITY  TYPE z_cl_ars_tool_crmms=>entity_mappings.
  LT_Model_ENTITY = VALUE #(
        ( abap = 'ID' json = 'ald_opportunitynumber' )
        ( abap = 'company' ENTITY = 'ald_companies' SELECT = 'ald_name' key = '_ald_companyid_value' )
        ( abap = 'isocur' ENTITY = 'transactioncurrencies' SELECT = 'isocurrencycode' key = '_transactioncurrencyid_value' )
        ( abap = 'LOB' ENTITY = 'ald_lineofbusinesses' SELECT = 'ald_name' key = '_ald_lineofbusinessid_value' )
        ( abap = 'Sales' ENTITY = 'systemusers' SELECT = 'domainname' key = '_ald_personinchargeid_value' )
        ( abap = 'customer' ENTITY = 'accounts' SELECT = 'name' key = '_customerid_value' )
      ).
```

this allow to:
- Map ald_opportunitynumber json field to ID abap field
- retrive ald_name from entity ald_companies making a lookup using value _ald_companyid_value from opporttunity into company abap field
- do the same for currency, line of business, sales and customer.