CLASS zcl_wo_crud_test_jr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
     INTERFACES if_oo_adt_classrun.

     METHODS:
        test_create_work_order
          EXPORTING rv_valid        TYPE abap_bool
                    rv_message      TYPE string,
        test_read_work_order
          EXPORTING rv_valid        TYPE abap_bool
                    rv_message      TYPE string
                    rv_ls_workorder TYPE ZTWORKORDERJR,
        test_delete_work_order
          EXPORTING rv_valid        TYPE abap_bool
                    rv_message      TYPE string,
        test_update_work_order
          EXPORTING rv_valid        TYPE abap_bool
                    rv_message      TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wo_crud_test_jr IMPLEMENTATION.

    METHOD if_oo_adt_classrun~main.
        DATA: lv_message TYPE string,
              lv_valid   TYPE abap_bool,
              lv_test    TYPE c. "1-create, 2-read, 3- unpdate, 4-delete
        DATA ls_workorder TYPE ztworkorderjr.
        lv_test = '1'.

        CASE lv_test.
          WHEN '1'.
             test_create_work_order(  IMPORTING
                                            rv_valid   = lv_valid
                                            rv_message = lv_message ).
             out->write( |{ lv_message }| ).
          WHEN '2'.
             test_read_work_order(  IMPORTING
                                            rv_valid        = lv_valid
                                            rv_message      = lv_message
                                            rv_ls_workorder = ls_workorder ).
             out->write( ls_workorder ).

          WHEN '3'.
             test_update_work_order(  IMPORTING
                                            rv_valid   = lv_valid
                                            rv_message = lv_message ).
             out->write( |{ lv_message }| ).
          WHEN '4'.
             test_delete_work_order(  IMPORTING
                                            rv_valid   = lv_valid
                                            rv_message = lv_message ).
             out->write( |{ lv_message }| ).
             IF lv_valid = abap_false.
                out->write(  |{ lv_message }| ).
             ELSE.
                out->write(  | work order delete correctly | ).
             ENDIF.
        ENDCASE.

    ENDMETHOD.
    METHOD test_create_work_order.
            DATA(lo_instanceJR) = NEW zcl_wo_crud_handlerjr( ).
            lo_instanceJR->Create_work_order( EXPORTING
                                                iv_workorder_id    = 10                                             iv_customerid      = '001'
                                                iv_technicianid    = '001'
                                                iv_creation_date   = '20250630'
                                                iv_status          = 'PE'
                                                iv_priority        = 'A'
                                                iv_description     = 'INSERTANDO PRUEBA'
                                              IMPORTING
                                                rv_valid   = rv_valid
                                                rv_message = rv_message ).


    ENDMETHOD.

    METHOD test_read_work_order.
            DATA(lo_instanceJR) = NEW  zcl_wo_crud_handlerjr( ).
            lo_instanceJR->read_work_order(
                                            EXPORTING
                                                iv_workorder_id    = 10
                                            IMPORTING
                                                rv_valid            = rv_valid
                                                rv_message          = rv_message
                                                rv_ls_work_order    = rv_ls_workorder ).

    ENDMETHOD.

    METHOD test_update_work_order.
            DATA(lo_instanceJR) = NEW zcl_wo_crud_handlerjr( ).
            lo_instanceJR->update_work_order( EXPORTING
                                                iv_workorder_id    = 10                                             iv_customerid      = '001'
                                                iv_technicianid    = '002'
                                                iv_creation_date   = '20250701'
                                                iv_status          = 'PE'
                                                iv_priority        = 'A'
                                                iv_description     = 'INSERTANDO PRUEBA'
                                              IMPORTING
                                                rv_valid   = rv_valid
                                                rv_message = rv_message ).

    ENDMETHOD.

    METHOD test_delete_work_order.
            DATA(lo_instanceJR) = NEW zcl_wo_crud_handlerjr( ).
            DATA: rv_ls_work_order     TYPE ztworkorderjr.
            lo_instanceJR->delete_work_order( EXPORTING
                                                iv_workorder_id    = 4
                                             IMPORTING
                                                rv_valid    = rv_valid
                                                rv_message  = rv_message ).

    ENDMETHOD.

ENDCLASS.
