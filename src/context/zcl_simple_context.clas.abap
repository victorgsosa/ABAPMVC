CLASS zcl_simple_context DEFINITION
  PUBLIC
  CREATE PUBLIC
   .

  PUBLIC SECTION.
    INTERFACES zif_context.
    DATA elements TYPE zif_context=>elements.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check
      IMPORTING
                source         TYPE data
                target         TYPE data
      RETURNING VALUE(checked) TYPE abap_bool.

ENDCLASS.



CLASS zcl_simple_context IMPLEMENTATION.
  METHOD zif_context~get_element.
    IF NOT line_exists( me->elements[ name = name ] ).
      RAISE EXCEPTION TYPE zcx_illegal_context.
    ENDIF.
    value = me->elements[ name = name ]-value.
  ENDMETHOD.

  METHOD zif_context~get_element_as_table.
    CALL METHOD me->zif_context~get_element
      EXPORTING
        name  = name
      IMPORTING
        value = DATA(table_ref).
    ASSIGN value TO FIELD-SYMBOL(<target_value>).
    ASSIGN table_ref->* TO FIELD-SYMBOL(<source_value>).
    IF NOT check( source = <source_value> target = <target_value> ).
      RAISE EXCEPTION TYPE zcx_illegal_context.
    ENDIF.
    <target_value> = <source_value>.
  ENDMETHOD.

  METHOD zif_context~set_element.
    DATA r_new_value TYPE REF TO data.
    IF copy EQ abap_true.
      ASSIGN value->* TO FIELD-SYMBOL(<value>).
      CREATE DATA r_new_value LIKE <value>.
      ASSIGN r_new_value->* TO FIELD-SYMBOL(<new_value>).
      <new_value> = <value>.
    ELSE.
      r_new_value = value.
    ENDIF.
    IF line_exists( me->elements[ name = name ] ).
      me->elements[ name = name ]-value = r_new_value.
    ELSE.
      INSERT VALUE zif_context=>element( name = name value = r_new_value ) INTO TABLE me->elements.
    ENDIF.
  ENDMETHOD.


  METHOD check.
    checked = abap_true.
    DATA(source_type) = cl_abap_typedescr=>describe_by_data( source ).
    DATA(target_type) = cl_abap_typedescr=>describe_by_data( target ).
    IF source_type NE target_type.
      checked = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
