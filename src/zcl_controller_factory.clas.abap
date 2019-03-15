CLASS zcl_controller_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_context TYPE REF TO zif_context.
    METHODS controller_for
      IMPORTING
        i_screen           TYPE REF TO cl_bus_abstract_main_screen
        i_controller_class TYPE REF TO cl_abap_classdescr
        i_parameters       TYPE abap_parmbind_tab OPTIONAL
        i_manage_exit      TYPE abap_bool DEFAULT abap_false
    returning VALUE(r_controller) type ref to zif_screen_controller.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF controller,
             controller_class TYPE REF TO cl_abap_classdescr,
             controller       TYPE REF TO zif_screen_controller,
           END OF controller.
    TYPES controller_tab TYPE HASHED TABLE OF controller WITH UNIQUE KEY controller_class.
    CONSTANTS screen_controller_class TYPE string VALUE 'ZIF_SCREEN_CONTROLLER' ##NO_TEXT.
    DATA controllers TYPE controller_tab.
    DATA context TYPE REF TO zif_context.
ENDCLASS.



CLASS zcl_controller_factory IMPLEMENTATION.

  METHOD constructor.

    me->context = i_context.

  ENDMETHOD.


  METHOD controller_for.
    IF line_exists( me->controllers[ controller_class = i_controller_class ] ).
      r_controller = me->controllers[ controller_class = i_controller_class ]-controller.
    ELSE.
      DATA(controller_interface) =  CAST cl_abap_intfdescr( cl_abap_intfdescr=>describe_by_name( screen_controller_class ) ).
      DATA(handler) = NEW zcl_controller_handler( context = me->context manage_exit = i_manage_exit ).
      r_controller ?= zcl_proxy_factory=>proxy_for(
           i_interfaces = VALUE #( ( controller_interface ) )
           i_super_class = i_controller_class
           i_invocation_handler = handler
           i_parameters = i_parameters
      ).
      SET HANDLER r_controller->handle_pai FOR i_screen.
      INSERT VALUE #( controller_class = i_controller_class controller = r_controller ) INTO TABLE me->controllers.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
