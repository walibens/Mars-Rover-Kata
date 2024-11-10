"!https://kata-log.rocks/mars-rover-kata
REPORT yr_mars_rover_wbs.

INTERFACE lif_direction.
  TYPES :
    BEGIN OF ENUM cardinal,
      north,
      south,
      east,
      west,
    END OF ENUM cardinal.
ENDINTERFACE.

CLASS lcl_position DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES :
      BEGIN OF ty_coordinates,
        x TYPE i,
        y TYPE i,
      END OF ty_coordinates.

    DATA :
      coordinates TYPE ty_coordinates READ-ONLY.

    METHODS :
      constructor
        IMPORTING x TYPE i
                  y TYPE i,
      translate
        IMPORTING delta_x TYPE i
                  delta_y TYPE i.
ENDCLASS.

CLASS lcl_position IMPLEMENTATION.
  METHOD constructor.
    coordinates-x = x.
    coordinates-y = y.
  ENDMETHOD.

  METHOD translate.
    coordinates-x += delta_x.
    coordinates-y += delta_y.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rover DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :
      constructor
        IMPORTING position  TYPE REF TO lcl_position
                  direction TYPE lif_direction=>cardinal,
      retrieve_position
        RETURNING VALUE(result) TYPE lcl_position=>ty_coordinates,
      move_forward,
      retrieve_direction
        RETURNING VALUE(result) TYPE lif_direction=>cardinal.

  PRIVATE SECTION.
    DATA :
      position  TYPE REF TO lcl_position,
      direction TYPE lif_direction=>cardinal.
    METHODS
      land_in
        IMPORTING position TYPE REF TO lcl_position.
    METHODS rotate_to
      IMPORTING
        direction TYPE lif_direction=>cardinal.
ENDCLASS.

CLASS lcl_rover IMPLEMENTATION.
  METHOD constructor.
    land_in( position ).
    rotate_to( direction ).
  ENDMETHOD.

  METHOD land_in.
    me->position = position.
  ENDMETHOD.

  METHOD retrieve_position.
    result = position->coordinates.
  ENDMETHOD.

  METHOD move_forward.
    CASE direction.
      WHEN lif_direction=>north.
        position->translate( delta_x = 0 delta_y = 1 ).
      WHEN lif_direction=>west.
        position->translate( delta_x = -1 delta_y = 0 ).
    ENDCASE.
  ENDMETHOD.

  METHOD retrieve_direction.
    result = direction.
  ENDMETHOD.

  METHOD rotate_to.
    me->direction = direction.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_rover DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: initial_position_of_rover FOR TESTING,
      rover_move_forward_to_north FOR TESTING,
      direction_of_rover_is_east FOR TESTING,
      rover_move_forward_to_west FOR TESTING.
ENDCLASS.

CLASS ltc_rover IMPLEMENTATION.
  METHOD initial_position_of_rover.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>north ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 1 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD direction_of_rover_is_east.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>east ).

    cl_abap_unit_assert=>assert_equals( exp = lif_direction=>east
                                        act = cut->retrieve_direction( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_north.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>north ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 1 y = 3 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_west.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>west ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 0 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.
ENDCLASS.
