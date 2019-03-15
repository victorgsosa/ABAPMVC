CLASS zcl_controller_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_invocation_handler.
    METHODS constructor
      IMPORTING
        context     TYPE REF TO zif_context
        manage_exit TYPE abap_bool DEFAULT abap_false.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS method_prefix TYPE string VALUE `HANDLE_` ##NO_TEXT.
    CONSTANTS handle_others TYPE string VALUE 'HANDLE_OTHERS' ##NO_TEXT.
    CONSTANTS screen_param TYPE abap_parmname VALUE 'SCREEN' ##NO_TEXT.
    CONSTANTS handle_pai TYPE string VALUE 'HANDLE_PAI' ##NO_TEXT.
    CONSTANTS function_code TYPE string VALUE 'IV_FUNCTION_CODE' ##NO_TEXT.
    CONSTANTS sender TYPE string VALUE 'SENDER' ##NO_TEXT.
    CONSTANTS exit_code TYPE string VALUE 'EXIT' ##NO_TEXT.
    CONSTANTS back_code TYPE string VALUE 'BACK' ##NO_TEXT.
    CONSTANTS cancel_code TYPE string VALUE 'CANCEL' ##NO_TEXT.
    DATA context TYPE REF TO zif_context.
    DATA manage_exit TYPE abap_bool.
    METHODS map_input_parameters
      IMPORTING
        i_class         TYPE REF TO cl_abap_objectdescr
        i_method        TYPE abap_methdescr
        i_context       TYPE REF TO zif_context
      RETURNING
        VALUE(r_result) TYPE abap_parmbind_tab.
    METHODS map_out_parameters
      IMPORTING
        i_method     TYPE abap_methdescr
        i_parameters TYPE abap_parmbind_tab
      CHANGING
        c_context    TYPE REF TO zif_context.
    METHODS map_output_parameters_of_kind
      IMPORTING
        i_method     TYPE abap_methdescr
        i_kind       TYPE abap_parmkind
        i_parameters TYPE abap_parmbind_tab
      CHANGING
        c_context    TYPE REF TO zif_context.
    METHODS method_for_code
      IMPORTING
        i_objectdescr   TYPE REF TO cl_abap_objectdescr
        i_code          TYPE string
      RETURNING
        VALUE(r_result) TYPE abap_methdescr.
    METHODS handle_exit
      IMPORTING
        i_code   TYPE bus_screen-function_code
        i_screen TYPE REF TO cl_bus_abstract_main_screen.
ENDCLASS.



CLASS zcl_controller_handler IMPLEMENTATION.

  METHOD constructor.

    me->context = context.
    me->manage_exit = manage_exit.

  ENDMETHOD.
  METHOD zif_invocation_handler~invoke.
    DATA(method_name) = method-name.
    IF method_name = handle_pai.
      DATA(controller) = CAST zif_screen_controller( proxy ).
      DATA(controller_type) = CAST cl_abap_objectdescr( cl_abap_objectdescr=>describe_by_object_ref( object ) ).
      DATA(code) = parameters[ name = function_code ]-value.
      ASSIGN code->* TO FIELD-SYMBOL(<code>).
      DATA(screen) = parameters[ name = sender ]-value.
      ASSIGN screen->* TO FIELD-SYMBOL(<screen>).
      "Make interceptor
      IF manage_exit = abap_true AND
        ( <code> = exit_code OR <code> = back_code OR <code> = cancel_code ).
        handle_exit( i_code = <code> i_screen = <screen> ).
        RETURN.
      ENDIF.
      DATA(handler_method) = method_for_code( i_objectdescr = controller_type i_code = CONV string( <code> ) ).
      DATA(handler_parameters) = map_input_parameters( i_class = controller_type i_method = handler_method i_context = me->context ).
      IF line_exists( handler_method-parameters[ name = screen_param parm_kind = cl_abap_objectdescr=>importing ] ).
        INSERT VALUE #( name = screen_param value = screen ) INTO TABLE handler_parameters.
      ENDIF.
      CALL METHOD object->(handler_method-name)
        PARAMETER-TABLE
        handler_parameters.
      map_out_parameters( EXPORTING i_method = handler_method i_parameters = handler_parameters CHANGING c_context = me->context ).
    ELSE.
      CALL METHOD object->(method_name)
        PARAMETER-TABLE
        parameters.
    ENDIF.
  ENDMETHOD.


  METHOD map_input_parameters.
    CLEAR r_result.
    DATA value TYPE REF TO data.
    LOOP AT i_method-parameters INTO DATA(parameter).
      IF parameter-name <> screen_param.
        IF parameter-parm_kind = cl_abap_objectdescr=>importing OR parameter-parm_kind = cl_abap_objectdescr=>changing.
          TRY.
              i_context->get_element( EXPORTING name = CONV #( parameter-name ) IMPORTING value = value ).
            CATCH zcx_illegal_context INTO DATA(exception).
              IF parameter-is_optional = abap_false .
                RAISE EXCEPTION TYPE zcx_controller
                  EXPORTING
                    previous = exception.
              ENDIF.
          ENDTRY.
        ELSE.
          DATA(parameter_type) = i_class->get_method_parameter_type( p_method_name = i_method-name p_parameter_name = parameter-name ).
          CREATE DATA value TYPE HANDLE parameter_type.
        ENDIF.
        INSERT VALUE #( name = parameter-name value = value ) INTO TABLE r_result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_out_parameters.
    map_output_parameters_of_kind(
        EXPORTING
            i_method = i_method
            i_kind    = cl_abap_objectdescr=>exporting
            i_parameters = i_parameters
        CHANGING
            c_context = c_context
    ).
    map_output_parameters_of_kind(
        EXPORTING
            i_method = i_method
            i_kind    = cl_abap_objectdescr=>changing
            i_parameters = i_parameters
        CHANGING
            c_context = c_context
    ).
    map_output_parameters_of_kind(
        EXPORTING
            i_method = i_method
            i_kind    = cl_abap_objectdescr=>returning
            i_parameters = i_parameters
        CHANGING
            c_context = c_context
    ).
  ENDMETHOD.



  METHOD map_output_parameters_of_kind.
    LOOP AT i_method-parameters INTO DATA(parameter) WHERE  parm_kind = i_kind.
      TRY.
          c_context->set_element( name = CONV #( parameter-name ) value = i_parameters[ name = parameter-name ]-value ).
        CATCH zcx_illegal_context INTO DATA(exception).
          IF parameter-is_optional = abap_false.
            RAISE EXCEPTION TYPE zcx_controller
              EXPORTING
                previous = exception.
          ENDIF.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD method_for_code.
    DATA(handler_method_name) = |{ method_prefix }{ i_code }|.
    IF  line_exists( i_objectdescr->methods[ name = handler_method_name visibility = cl_abap_objectdescr=>public ] ).
      r_result = i_objectdescr->methods[ name = handler_method_name visibility = cl_abap_objectdescr=>public ].
      RETURN.
    ENDIF.
    IF  line_exists( i_objectdescr->methods[ name = handle_others visibility = cl_abap_objectdescr=>public ] ).
      r_result = i_objectdescr->methods[ name = handle_others visibility = cl_abap_objectdescr=>public ].
      RETURN.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_controller.
  ENDMETHOD.




  METHOD handle_exit.
    CASE i_code.
      WHEN back_code OR cancel_code.
        i_screen->leave( ).
      WHEN exit_code.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
