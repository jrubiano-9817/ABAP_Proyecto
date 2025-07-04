CLASS zcl_llenar_tablas_refjr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS:
     Create_zttechnicianjr,
     Create_ztstatus_jr,
     Create_ztpriority_jr.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llenar_tablas_refjr IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    me->Create_ztstatus_jr( ).
    out->write( |Status  insertados exitosamente.| ).
    me->Create_ztpriority_jr( ).
    out->write( |Priority  insertados exitosamente.| ).
    me->Create_zttechnicianjr( ).
    out->write( |Técnicos insertados exitosamente.| ).

  ENDMETHOD.

  METHOD Create_zttechnicianjr.
    DATA: ls_technicianjr TYPE zttechnicianjr.
    ls_technicianjr-technician_id = '001'.
    ls_technicianjr-name          = 'tecnico 1'.
    ls_technicianjr-speciality    = 'Especialidad 1'.
    INSERT zttechnicianjr FROM @ls_technicianjr.

    ls_technicianjr-technician_id = '002'.
    ls_technicianjr-name          = 'tecnico 2'.
    ls_technicianjr-speciality    = 'Especialidad 2'.
    INSERT zttechnicianjr FROM @ls_technicianjr.

    ls_technicianjr-technician_id = '003'.
    ls_technicianjr-name          = 'tecnico 3'.
    ls_technicianjr-speciality    = 'Especialidad 3'.
    INSERT zttechnicianjr FROM @ls_technicianjr.
  ENDMETHOD.

  METHOD Create_ztstatus_jr.
    DATA: ls_ztstatus_jr TYPE ztstatus_jr.

    ls_ztstatus_jr-status_code         = 'PE'.
    ls_ztstatus_jr-status_description = 'Pendiente'.
    INSERT ztstatus_jr  FROM @ls_ztstatus_jr.

    ls_ztstatus_jr-status_code         = 'C0'.
    ls_ztstatus_jr-status_description  = 'Completada'.
    INSERT ztstatus_jr  FROM @ls_ztstatus_jr.

  ENDMETHOD.

  METHOD Create_ztpriority_jr.
    DATA: ls_ztpriority_jr TYPE ztpriority_jr.

    ls_ztpriority_jr-priority_code          = 'A'.
    ls_ztpriority_jr-priority_description   = 'ALTA'.
    INSERT ztpriority_jr  FROM @ls_ztpriority_jr.

    ls_ztpriority_jr-priority_code           = 'B'.
    ls_ztpriority_jr-priority_description    = 'BAJA'.
    INSERT ztpriority_jr  FROM @ls_ztpriority_jr.

  ENDMETHOD.

ENDCLASS.


