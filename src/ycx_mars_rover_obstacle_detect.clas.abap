CLASS ycx_mars_rover_obstacle_detect DEFINITION INHERITING FROM cx_dynamic_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        obstacle_position TYPE REF TO ycl_mars_rover_position.
  PRIVATE SECTION.
    DATA obstacle_position TYPE REF TO ycl_mars_rover_position.
    METHODS get_error_message
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS ycx_mars_rover_obstacle_detect IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid = textid previous = previous ).
    me->obstacle_position = obstacle_position.
  ENDMETHOD.

  METHOD get_error_message.
    result = |Obstacle detected in coordinates : X = { obstacle_position->coordinates-x }, Y =  { obstacle_position->coordinates-y }|.
  ENDMETHOD.
ENDCLASS.
