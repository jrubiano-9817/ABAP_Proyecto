CLASS zcl_wo_validatorjr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  METHODS:
      validate_create_order
        IMPORTING iv_customerid     TYPE string
                  iv_technicianid   TYPE string
                  iv_priority       TYPE string
        RETURNING VALUE(rv_valid)   TYPE abap_bool,

      validate_update_order
        IMPORTING iv_workorderid    TYPE z_d_workorderid_jr
        RETURNING VALUE(rv_valid)   TYPE abap_bool,

      validate_delete_order
        IMPORTING iv_workorderid    TYPE z_d_workorderid_jr
        RETURNING VALUE(rv_valid)   TYPE abap_bool,

      validate_status_and_priority
        IMPORTING iv_status         TYPE string
                  iv_priority       TYPE string
        RETURNING VALUE(rv_valid)   TYPE abap_bool,

      check_customer_exists
        IMPORTING iv_customerid     TYPE string
        RETURNING VALUE(rv_exists)  TYPE abap_bool,

      check_technician_exists
        IMPORTING iv_technicianid   TYPE string
        RETURNING VALUE(rv_exists)  TYPE abap_bool,

      check_priority_exists
        IMPORTING iv_priority       TYPE string
        RETURNING VALUE(rv_exists)  TYPE abap_bool,

      check_status_exists
        IMPORTING iv_status         TYPE string
        RETURNING VALUE(rv_exists)  TYPE abap_bool,

      check_order_exists
        IMPORTING iv_workorderid    TYPE z_d_workorderid_jr
        RETURNING VALUE(rv_exists)  TYPE abap_bool,

      check_order_status
        IMPORTING iv_workorderid    TYPE z_d_workorderid_jr
        RETURNING VALUE(rv_status)  TYPE string,

      check_order_history
        IMPORTING iv_workorderid    TYPE z_d_workorderid_jr
        RETURNING VALUE(rv_exists)  TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS ZCL_WO_VALIDATORJR IMPLEMENTATION.
    METHOD validate_create_order.
        DATA(lv_customer_exists) = check_customer_exists( iv_customerid ).
        IF lv_customer_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_technician_exists) = check_technician_exists( iv_technicianid ).
        IF lv_technician_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_priority_exists) = check_priority_exists( iv_priority ).
        IF lv_priority_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

      METHOD check_customer_exists.
        SELECT SINGLE FROM ztcustomer_jr
          FIELDS *
          WHERE customerid = @iv_customerid
          INTO @DATA(ls_customer).
        rv_exists = xsdbool( sy-subrc = 0 ).
      ENDMETHOD.

      METHOD check_technician_exists.
        SELECT SINGLE FROM zttechnicianjr
          FIELDS *
          WHERE technician_id = @iv_technicianid
          INTO @DATA(ls_technician).
        rv_exists = xsdbool( sy-subrc = 0 ).
      ENDMETHOD.

      METHOD check_priority_exists.
        SELECT SINGLE FROM ztpriority_jr
          FIELDS *
          WHERE priority_code = @iv_priority
          INTO @DATA(ls_priority).
        rv_exists = xsdbool( sy-subrc = 0 ).
      ENDMETHOD.

      METHOD check_status_exists.
        SELECT SINGLE FROM ztstatus_jr
          FIELDS *
          WHERE status_code = @iv_status
          INTO @DATA(ls_status).
        rv_exists = xsdbool( sy-subrc = 0 ).
      ENDMETHOD.

      METHOD validate_update_order.
        DATA(lv_order_exists) = check_order_exists( iv_workorderid ).
        IF lv_order_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_order_status) = check_order_status( iv_workorderid ).
        IF lv_order_status NE 'PE'.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

      METHOD check_order_exists.
        SELECT SINGLE FROM ztworkorderjr
          FIELDS *
          WHERE workorderid = @iv_workorderid
          INTO @DATA(ls_workorder).
        rv_exists = xsdbool( sy-subrc = 0 ).
      ENDMETHOD.

      METHOD check_order_status.
        SELECT SINGLE FROM ztworkorderjr
          FIELDS *
          WHERE workorderid = @iv_workorderid
          INTO @DATA(ls_workorder).
        IF sy-subrc = 0.
          rv_status = ls_workorder-status.
        ELSE.
          rv_status = ''.
        ENDIF.
      ENDMETHOD.

      METHOD validate_delete_order.
        DATA(lv_order_exists) = check_order_exists( iv_workorderid ).
        IF lv_order_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_order_status) = check_order_status( iv_workorderid ).
        IF lv_order_status NE 'PE'.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_has_history) = check_order_history( iv_workorderid ).
        IF lv_has_history IS NOT INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

      METHOD check_order_history.
        SELECT SINGLE FROM ZTHIS_WORKORDER
          FIELDS *
          WHERE workorder_id = @iv_workorderid
          INTO @DATA(ls_hisworkorder).
        rv_exists = xsdbool( sy-subrc = 0 ).
      ENDMETHOD.

      METHOD validate_status_and_priority.
        DATA(lv_status_exists) = check_status_exists( iv_status ).
        IF lv_status_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_priority_exists) = check_priority_exists( iv_priority ).
        IF lv_priority_exists IS INITIAL.
          rv_valid = abap_false.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

ENDCLASS.



