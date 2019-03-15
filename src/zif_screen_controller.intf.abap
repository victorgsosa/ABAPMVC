INTERFACE zif_screen_controller
  PUBLIC .
  METHODS handle_pai FOR EVENT process_after_input OF cl_bus_abstract_main_screen
    IMPORTING
        sender
        iv_function_code.
ENDINTERFACE.
