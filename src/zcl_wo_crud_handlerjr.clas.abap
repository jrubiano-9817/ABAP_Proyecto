CLASS zcl_wo_crud_handlerjr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
  METHODS:
    Create_work_order   IMPORTING   iv_workorder_id      TYPE Z_D_WORKORDERID_JR
                                    iv_customerid        TYPE string
                                    iv_technicianid      TYPE string
                                    iv_creation_date     TYPE d
                                    iv_status            TYPE string
                                    iv_priority          TYPE string
                                    iv_description       TYPE string
                        EXPORTING   rv_valid             TYPE abap_bool
                                    rv_message           TYPE string,
   read_work_order      IMPORTING   iv_workorder_id      TYPE Z_D_WORKORDERID_JR
                        EXPORTING   rv_valid             TYPE abap_bool
                                    rv_message           TYPE string
                                    rv_ls_work_order     TYPE ztworkorderjr,
   update_work_order    IMPORTING   iv_workorder_id      TYPE Z_D_WORKORDERID_JR
                                    iv_customerid        TYPE string
                                    iv_technicianid      TYPE string
                                    iv_creation_date     TYPE d
                                    iv_status            TYPE string
                                    iv_priority          TYPE string
                                    iv_description       TYPE string
                        EXPORTING   rv_valid             TYPE abap_bool
                                    rv_message           TYPE string,
   delete_work_order    IMPORTING   iv_workorder_id      TYPE Z_D_WORKORDERID_JR
                        EXPORTING   rv_valid             TYPE abap_bool
                                    rv_message           TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wo_crud_handlerjr IMPLEMENTATION.
    METHOD Create_work_order.
        DATA: ls_workorder TYPE ztworkorderjr.
        DATA(lo_workorder) = NEW zcl_wo_validatorjr( ).
        DATA(lv_activo) = lo_workorder->validate_create_order(
                            iv_customerid     = iv_customerid
                            iv_technicianid   = iv_technicianid
                            iv_priority       = iv_priority ).

        IF lv_activo = abap_false.
          rv_message = |La validación de datos para crear la orden falló, revise|.
          rv_valid   = abap_false.
          RETURN.
        ENDIF.

        ls_workorder = VALUE #( workorderid      =  iv_workorder_id
                                customerid       =  iv_customerid
                                technician_id    =  iv_technicianid
                                creation_date    =  iv_creation_date
                                status           =  iv_status
                                priority         =  iv_priority
                                description      =  iv_description ).
        TRY.
           INSERT  ZTWORKORDERJR from @ls_workorder.
           CATCH  cx_sy_open_sql_db INTO DATA(lx_sql_db).
               rv_message = |Error al tratar de crear orden, por favor revise|.
               rv_valid = abap_false.
               RETURN.
        ENDTRY.

        IF sy-subrc = 0.
           rv_message = |Orden { iv_workorder_id }, insertada correctamente|.
           rv_valid = abap_true.
        ELSE.
           rv_message = |Orden { iv_workorder_id }, NO FUE insertada correctamente EN LA TABLA DE ORDENES|.
           rv_valid = abap_false.
        ENDIF.
    ENDMETHOD.

   METHOD update_work_order.
        DATA(lo_workorder) = NEW zcl_wo_validatorjr( ).

        DATA(lv_activo) = lo_workorder->validate_update_order( iv_workorderid = iv_workorder_id ).

        IF lv_activo = abap_false.
          rv_message = |La validación de datos de orden y status para Actualizar la orden falló, revise por favor|.
          rv_valid   = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_activo1) = lo_workorder->validate_create_order(
                                                    iv_customerid     = iv_customerid
                                                    iv_technicianid   = iv_technicianid
                                                    iv_priority       = iv_priority ).

        IF lv_activo1 = abap_false.
           rv_message = |Los datos  del cliente, tecnico o prioridad que se pretenden actualizar,  no son validos favor chequear|.
           rv_valid   = abap_false.
          RETURN.
        ENDIF.

        DATA(lv_cambios) = ''.
        SELECT SINGLE FROM ZTWORKORDERJR
        FIELDS *
        WHERE  workorderid = @iv_workorder_id
        INTO @DATA(ls_workorder).

        IF sy-subrc = 0.

           IF  ls_workorder-customerid NE iv_customerid.
               lv_cambios = lv_cambios &&  | / Customer before: { ls_workorder-customerid    }  After : { iv_customerid    } |.
           ENDIF.

           IF ls_workorder-technician_id NE iv_technicianid.
               lv_cambios = lv_cambios && | / Customer before: { ls_workorder-technician_id }  After : { iv_technicianid  } |.
           ENDIF.
           IF  ls_workorder-creation_date NE iv_creation_date.
               lv_cambios = lv_cambios && | / Customer before: { ls_workorder-creation_date }  After : { iv_creation_date } |.
           ENDIF.
           IF  ls_workorder-status NE iv_status.
               lv_cambios = lv_cambios && | / Customer before: { ls_workorder-status        }  After : { iv_status        } |.
           ENDIF.
           IF  ls_workorder-priority NE iv_priority.
               lv_cambios = lv_cambios && | / Customer before: { ls_workorder-priority      }  After : { iv_priority      } |.
           ENDIF.
           IF  ls_workorder-description NE iv_description.
               lv_cambios = lv_cambios && | / Customer before: { ls_workorder-description   }  After : { iv_description  } |.
           ENDIF.

           ls_workorder-customerid        = iv_customerid.
           ls_workorder-technician_id     = iv_technicianid.
           ls_workorder-creation_date     = iv_creation_date.
           ls_workorder-status            = iv_status.
           ls_workorder-priority          = iv_priority.
           ls_workorder-description       = iv_description.


            TRY.
               UPDATE  ZTWORKORDERJR FROM  @ls_workorder.
               CATCH  cx_sy_open_sql_db INTO DATA(lx_sql_db).
                   rv_message = |WORK ORDER { iv_workorder_id } WAS NOT UPDATE CORRECTLY: { lx_sql_db->get_text( ) }|.
                   rv_valid = abap_false.
                   RETURN.
            ENDTRY.

            " AHORA CREAMOS LA HISTORIA DE ACTUALIZACION
            DATA: LS_HIS_WORKORDER TYPE ZTHIS_WORKORDER.
            DATA: lv_count         TYPE i.
            DATA: lv_date          TYPE d.

            lv_date = sy-datum.
*           lv_date = cl_abab_context_info=>get_system_date( ).

            SELECT COUNT(*)
                 FROM ZTHIS_WORKORDER
                 INTO @lv_count.
            LS_HIS_WORKORDER = VALUE #(
                                history_id                = lv_count + 1
                                workorder_id              = iv_workorder_id
                                modification_date         = lv_date
                                change_description        = lv_cambios ).
            INSERT ZTHIS_WORKORDER FROM @LS_HIS_WORKORDER.
            IF sy-subrc = 0.
               rv_message = |HISTORIA de la Orden { iv_workorder_id }, insertada correctamente|.
               rv_valid = abap_true.
            ELSE.
               rv_message = |HISTORIA de a Orden { iv_workorder_id }, NO FUE insertada correctamente EN LA TABLA DE ZTHIS_WORKORDER|.
               rv_valid = abap_false.
            ENDIF.
        ELSE.
          rv_message = |Work Order { iv_workorder_id }, NO FUE ENCONTRADA PARA actualizar EN LA TABLA DE ZTHIS_WORKORDER|.
          rv_valid = abap_false.
        ENDIF.

    ENDMETHOD.

    METHOD read_work_order.
       SELECT SINGLE FROM ZTWORKORDERJR
          FIELDS *
          WHERE workorderid = @iv_workorder_id
          INTO @DATA(ls_workorder).
        IF sy-subrc = 0.
           rv_message = |Orden { iv_workorder_id }, existe|.
           rv_valid = abap_true.
        ELSE.
           rv_message = |Orden { iv_workorder_id }, No Existe|.
           rv_valid = abap_false.
        ENDIF.
    ENDMETHOD.

    METHOD delete_work_order.
       DATA: LS_WORKORDERJR TYPE ZTWORKORDERJR.
       DATA(lo_workorder) = NEW zcl_wo_validatorjr( ).
       IF lo_workorder->validate_delete_order( iv_workorderid = iv_workorder_id ) = abap_false.
               rv_message = |la Validacion de los datos para la orden  { iv_workorder_id }, No es correcta para el borrado|.
               rv_valid = abap_false.
               RETURN.
       ENDIF.

       SELECT SINGLE FROM ZTWORKORDERJR
          FIELDS *
          WHERE workorderid = @iv_workorder_id
          INTO @LS_WORKORDERJR.

       IF sy-subrc = 0.
          DELETE ZTWORKORDERJR FROM @LS_WORKORDERJR.
          IF sy-subrc = 0.
             rv_valid = abap_true.
             RETURN.
          ELSE.
             rv_valid = abap_false.
             RETURN.
          ENDIF.
       ELSE.
            rv_message = |PASO VALIDACION PERO NO ENCONTRÓ LA ORDEN:  { iv_workorder_id }, PARA BORRARLA|.
            rv_valid = abap_false.
            RETURN.
       ENDIF.
    ENDMETHOD.

ENDCLASS.

* Otra forma de hacerlo pero preferí dejar la primera por la estructura
*CLASS zcl_wo_crud_handlerjr DEFINITION
*  PUBLIC
*  FINAL
*  CREATE PUBLIC.
*  PUBLIC SECTION.
*    METHODS:
*      create_work_order   IMPORTING iv_workorder_id    TYPE z_d_workorderid_jr
*                                     iv_customerid      TYPE string
*                                     iv_technicianid    TYPE string
*                                     iv_creation_date   TYPE d
*                                     iv_status          TYPE string
*                                     iv_priority        TYPE string
*                                     iv_description     TYPE string
*                          EXPORTING  rv_valid           TYPE abap_bool
*                                     rv_message         TYPE string,
*      read_work_order     IMPORTING  iv_workorder_id    TYPE z_d_workorderid_jr
*                          EXPORTING  rv_valid           TYPE abap_bool
*                                     rv_message         TYPE string
*                                     rv_ls_work_order   TYPE ztworkorderjr,
*      update_work_order   IMPORTING  iv_workorder_id    TYPE z_d_workorderid_jr
*                                     iv_customerid      TYPE string
*                                     iv_technicianid    TYPE string
*                                     iv_creation_date   TYPE d
*                                     iv_status          TYPE string
*                                     iv_priority        TYPE string
*                                     iv_description     TYPE string
*                          EXPORTING  rv_valid           TYPE abap_bool
*                                     rv_message         TYPE string,
*      delete_work_order   IMPORTING  iv_workorder_id    TYPE z_d_workorderid_jr
*                          EXPORTING  rv_valid           TYPE abap_bool
*                                     rv_message         TYPE string.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*ENDCLASS.
*
*
*CLASS zcl_wo_crud_handlerjr IMPLEMENTATION.
*
*  METHOD create_work_order.
*    DATA: ls_workorder TYPE ztworkorderjr,
*          lo_workorder TYPE REF TO zcl_wo_validatorjr,
*          lv_activo    TYPE abap_bool.
*
*    lo_workorder = NEW zcl_wo_validatorjr( ).
*    lv_activo = lo_workorder->validate_create_order(
*                  iv_customerid   = iv_customerid
*                  iv_technicianid = iv_technicianid
*                  iv_priority     = iv_priority ).
*
*    IF lv_activo = abap_false.
*      rv_message = |La validación de datos para crear la orden falló, revise|.
*      rv_valid   = abap_false.
*      RETURN.
*    ENDIF.
*
*    ls_workorder = VALUE #( workorderid    = iv_workorder_id
*                            customerid     = iv_customerid
*                            technician_id  = iv_technicianid
*                            creation_date  = iv_creation_date
*                            status         = iv_status
*                            priority       = iv_priority
*                            description    = iv_description ).
*
*    TRY.
*        INSERT ztworkorderjr FROM @ls_workorder.
*        CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
*        rv_message = lx_sql_db->get_text( ).
*        rv_valid   = abap_false.
*        RETURN.
*    ENDTRY.
*
*    rv_message = |Orden { iv_workorder_id }, insertada correctamente|.
*    rv_valid = abap_true.
*  ENDMETHOD.
*
*
*  METHOD read_work_order.
*    SELECT SINGLE * FROM ztworkorderjr
*     WHERE workorderid = @iv_workorder_id
*      INTO @rv_ls_work_order.
*    IF sy-subrc = 0.
*      rv_message = |Orden { iv_workorder_id }, existe|.
*      rv_valid = abap_true.
*    ELSE.
*      rv_message = |Orden { iv_workorder_id }, no existe|.
*      rv_valid = abap_false.
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD update_work_order.
*    DATA: lo_workorder  TYPE REF TO zcl_wo_validatorjr,
*          lv_activo     TYPE abap_bool,
*          lv_activo1    TYPE abap_bool,
*          lv_cambios    TYPE string,
*          ls_workorder  TYPE ztworkorderjr,
*          ls_his_workorder TYPE zthis_workorder,
*          lv_count      TYPE i,
*          lv_date       TYPE d.
*
*    lo_workorder = NEW zcl_wo_validatorjr( ).
*    lv_activo = lo_workorder->validate_update_order( iv_workorderid = iv_workorder_id ).
*
*    IF lv_activo = abap_false.
*       rv_message = |La validación de datos de orden y status para actualizar falló, revise por favor|.
*       rv_valid   = abap_false.
*      RETURN.
*    ENDIF.
*
*    lv_activo1 = lo_workorder->validate_create_order(
*                   iv_customerid   = iv_customerid
*                   iv_technicianid = iv_technicianid
*                   iv_priority     = iv_priority ).
*
*    IF lv_activo1 = abap_false.
*       rv_message = |Los datos del cliente, técnico o prioridad no son válidos.|.
*       rv_valid   = abap_false.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE * FROM ztworkorderjr WHERE workorderid = @iv_workorder_id INTO @ls_workorder.
*
*    IF sy-subrc <> 0.
*      rv_message = |Orden { iv_workorder_id } no encontrada para actualizar.|.
*      rv_valid = abap_false.
*      RETURN.
*    ENDIF.
*
*    " Compara campos y construye descripción
*    IF ls_workorder-customerid    NE iv_customerid.
*      lv_cambios = lv_cambios && | / Customer before: { ls_workorder-customerid }  After: { iv_customerid }|.
*    ENDIF.
*    IF ls_workorder-technician_id NE iv_technicianid.
*      lv_cambios = lv_cambios && | / Technician before: { ls_workorder-technician_id }  After: { iv_technicianid }|.
*    ENDIF.
*    IF ls_workorder-creation_date NE iv_creation_date.
*      lv_cambios = lv_cambios && | / Date before: { ls_workorder-creation_date }  After: { iv_creation_date }|.
*    ENDIF.
*    IF ls_workorder-status        NE iv_status.
*      lv_cambios = lv_cambios && | / Status before: { ls_workorder-status }  After: { iv_status }|.
*    ENDIF.
*    IF ls_workorder-priority      NE iv_priority.
*      lv_cambios = lv_cambios && | / Priority before: { ls_workorder-priority }  After: { iv_priority }|.
*    ENDIF.
*    IF ls_workorder-description   NE iv_description.
*      lv_cambios = lv_cambios && | / Description before: { ls_workorder-description }  After: { iv_description }|.
*    ENDIF.
*
*    " Actualización de los campos
*    ls_workorder-customerid     = iv_customerid.
*    ls_workorder-technician_id  = iv_technicianid.
*    ls_workorder-creation_date  = iv_creation_date.
*    ls_workorder-status         = iv_status.
*    ls_workorder-priority       = iv_priority.
*    ls_workorder-description    = iv_description.
*
*    TRY.
*        UPDATE ztworkorderjr FROM @ls_workorder.
*      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
*        rv_message = |Error actualizando orden: { lx_sql_db->get_text( ) }|.
*        rv_valid = abap_false.
*        RETURN.
*    ENDTRY.
*
*    " Insertar historial
*    lv_date = sy-datum.
*    SELECT COUNT(*) FROM zthis_workorder INTO @lv_count.
*
*    ls_his_workorder = VALUE #(
*      history_id         = lv_count + 1
*      workorder_id       = iv_workorder_id
*      modification_date  = lv_date
*      change_description = lv_cambios ).
*
*    INSERT zthis_workorder FROM @ls_his_workorder.
*    IF sy-subrc = 0.
*      rv_message = |Historial de orden { iv_workorder_id } insertado correctamente.|.
*      rv_valid = abap_true.
*    ELSE.
*      rv_message = |Error insertando historial de orden { iv_workorder_id }.|.
*      rv_valid = abap_false.
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD delete_work_order.
*    DATA: ls_workorderjr TYPE ztworkorderjr,
*          lo_workorder   TYPE REF TO zcl_wo_validatorjr.
*
*    lo_workorder = NEW zcl_wo_validatorjr( ).
*    IF lo_workorder->validate_delete_order( iv_workorderid = iv_workorder_id ) = abap_false.
*      rv_message = |Validación para eliminar la orden { iv_workorder_id } falló.|.
*      rv_valid = abap_false.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE * FROM ztworkorderjr  WHERE workorderid = @iv_workorder_id INTO @ls_workorderjr.
*    IF sy-subrc = 0.
*      DELETE ztworkorderjr FROM @ls_workorderjr.
*      rv_valid = abap_true.
*    ELSE.
*      rv_message = |La orden { iv_workorder_id } no fue encontrada para eliminar.|.
*      rv_valid = abap_false.
*    ENDIF.
*  ENDMETHOD.
* ENDCLASS.

