"!https://kata-log.rocks/mars-rover-kata
REPORT yr_mars_rover_wbs.

PARAMETERS :
  p_coordx TYPE i,
  p_coordy TYPE i,
  p_dir    TYPE string.

CLASS lcl_mission_control DEFINITION.
  PUBLIC SECTION.
    METHODS :
      constructor
        IMPORTING x         TYPE i
                  y         TYPE i
                  direction TYPE string,
      send_commands
        IMPORTING commands TYPE any.
ENDCLASS.

CLASS lcl_mission_control IMPLEMENTATION.
  METHOD constructor.
    DATA cardinal TYPE yif_mars_rover_direction=>cardinal.

    DATA(rover) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = x y = y )
                                      direction = cardinal ).
  ENDMETHOD.

  METHOD send_commands.
"TODO
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(mission_control) = NEW lcl_mission_control( x         = p_coordx
                                                   y         = p_coordy
                                                   direction = p_dir ).
*  mission_control->send_commands( commands ).
