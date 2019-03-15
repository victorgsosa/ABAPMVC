INTERFACE zif_context
  PUBLIC .
  TYPES: BEGIN OF element,
           name  TYPE string,
           value TYPE REF TO data,
         END OF element.
  TYPES elements TYPE HASHED TABLE OF element WITH UNIQUE KEY name.
  METHODS set_element
    IMPORTING
      name  TYPE string
      value TYPE REF TO data
      copy type abap_bool DEFAULT abap_false
    RAISING
      zcx_illegal_context.

  METHODS get_element
    IMPORTING
              name  TYPE string
    EXPORTING
              value TYPE REF TO data
    RAISING   zcx_illegal_context.


  METHODS get_element_as_table
    IMPORTING
              name  TYPE string
    EXPORTING
              value TYPE ANY TABLE
    RAISING   zcx_illegal_context.

ENDINTERFACE.
