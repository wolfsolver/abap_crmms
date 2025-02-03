# abap_crmms
Abap integration with Microsoft CRM
> Fast and easy way to read data from Microsoft CRM from ABAP.

<IMG src="https://img.shields.io/github/downloads-pre/wolfsolver/abap_crmms/latest/total">


# Idea behind
The core idea behind this integration is to simplify the reading of data from the CRM as much as possible. A dedicated class handles the interaction with the CRM, exposing two simple methods: get_entities and get_entity.

# How to Use - Basic Usage

The most simply way is to create abap structure that rappresent json data. For example reading an user can be done with
    
```ABAP
    DATA(lc_crm) = NEW z_cl_abap_crmms_tools( ).

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

`    DATA(lc_crm) = NEW z_cl_abap_crmms_tools( ).`

Create an instance with default setting. Class constructor allow receiving paramters i_logical_system to identify correct system.
System are maintein in custom table


## Get list from crm
Get_Entities method allow to retrive list of object.
- i_entities is crm entity to read
- i_filter specify filter
- i_select specify list of fields to read
- i_mapping (optional) can specify mapping name between json and crm

example
```
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
```

## Using mapping
mapping can be used to:
- define abap name different from json
- force lookup on crm table for value that have id reference (for example to lookup user in opportunitoes)

Simply define maping table like this 

```abap
  DATA: LT_Model_ENTITY  TYPE Z_CL_ABAP_CRMMS_TOOLS=>entity_mappings.
  LT_Model_ENTITY = VALUE #(
        ( abap = 'ID' json = 'accountid' )
        ( abap = 'isocur' ENTITY = 'transactioncurrencies' SELECT = 'isocurrencycode' key = '_transactioncurrencyid_value' )
        ( abap = 'user' ENTITY = 'systemusers' SELECT = 'fullname' key = '_owninguser_value' )
        ( abap = 'businessunit' Entity = 'businessunits' SELECT = 'name' key = '_owningbusinessunit_value' )
      ).
```

this allow to:
- Map accountid json field to ID abap field
- retrive isocur from entity transactioncurrencies making a lookup using value _transactioncurrencyid_value
- do the same for currency and usage

## Demo Program
Two demo program contains sample data.

# Oauth
Table zabap_crmms_conf contais entry for auth with Microsoft CRM. table can be maintened with SM30.

| Field  | content |
| ------------- | ------------- |
| token_url | contains URL for login |
| client_id  | Client ID for auth  |
| client_secret |  Secret key from CRM MS | 
| base_url | tenat CRM url (without /) |

