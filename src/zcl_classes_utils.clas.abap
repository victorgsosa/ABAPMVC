CLASS zcl_classes_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    class-methods object_to_structure
        importing
            i_object type ref to object
        exporting
            e_structure type any.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_classes_utils IMPLEMENTATION.
  METHOD object_to_structure.
    data r_value type ref to data.
    field-SYMBOLS <value> type any.
    FIELD-SYMBOLS <component> type any.
    data(object_type) = cast cl_abap_objectdescr( cl_abap_objectdescr=>describe_by_object_ref( i_object ) ).
    data(structure_type) = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( e_structure ) ).
    LOOP AT structure_type->components into data(component).
        try.
        data(accesor) = zcl_metadata_utils=>accesor_for( i_class = object_type i_name = conv #( component-name ) ).
        data(component_type) = structure_type->get_component_type( p_name = component-name ).
        CREATE DATA r_value TYPE HANDLE component_type.
        assign r_value->* to <value>.
        accesor->get_value( exporting i_parent_object = i_object importing e_value = <value> ).
        ASSIGN COMPONENT component-name of structure e_structure to <component>.
        <component> = <value>.
        catch zcx_metamodel.
        ENDTRY.
        ENDLOOP.
  ENDMETHOD.

ENDCLASS.
