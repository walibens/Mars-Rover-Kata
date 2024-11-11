"!https://kata-log.rocks/mars-rover-kata
REPORT yr_mars_rover_wbs.

CLASS lcl_vector DEFINITION FINAL.
  PUBLIC SECTION.
    DATA :
      delta_x TYPE i READ-ONLY,
      delta_y TYPE i READ-ONLY.

    METHODS: constructor
      IMPORTING delta_x TYPE i
                delta_y TYPE i,
      reverse
        RETURNING VALUE(result) TYPE REF TO lcl_vector.
ENDCLASS.

CLASS lcl_vector IMPLEMENTATION.
  METHOD constructor.
    me->delta_x = delta_x.
    me->delta_y = delta_y.
  ENDMETHOD.

  METHOD reverse.
    me->delta_x *= -1.
    me->delta_y *= -1.
  ENDMETHOD.
ENDCLASS.

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
        IMPORTING vector TYPE REF TO lcl_vector.
ENDCLASS.

CLASS lcl_position IMPLEMENTATION.
  METHOD constructor.
    coordinates-x = x.
    coordinates-y = y.
  ENDMETHOD.

  METHOD translate.
    coordinates-x += vector->delta_x.
    coordinates-y += vector->delta_y.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rover DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES :
      tt_obstacles TYPE TABLE OF REF TO lcl_position WITH EMPTY KEY.
    METHODS :
      constructor
        IMPORTING position  TYPE REF TO lcl_position
                  direction TYPE lif_direction=>cardinal,
      retrieve_position
        RETURNING VALUE(result) TYPE lcl_position=>ty_coordinates,
      move_forward,
      retrieve_direction
        RETURNING VALUE(result) TYPE lif_direction=>cardinal,
      move_backward,
      rotate_right,
      rotate_left,
      initialize_obstacles
        IMPORTING obstacle TYPE tt_obstacles.

  PRIVATE SECTION.
    TYPES :
      BEGIN OF ty_translation_rule,
        direction TYPE lif_direction=>cardinal,
        vector    TYPE REF TO lcl_vector,
      END OF ty_translation_rule.

    DATA :
      position  TYPE REF TO lcl_position,
      direction TYPE lif_direction=>cardinal,
      obstacles TYPE TABLE OF REF TO lcl_position.

    METHODS:
      vector_rule
        IMPORTING direction     TYPE lif_direction=>cardinal
        RETURNING VALUE(result) TYPE REF TO lcl_vector,
      land_in
        IMPORTING position TYPE REF TO lcl_position,
      rotate_to
        IMPORTING
          direction TYPE lif_direction=>cardinal,
      is_target_clean
        IMPORTING
          next_position TYPE REF TO lcl_position
        RETURNING
          VALUE(result) TYPE abap_bool,
      simulate_target
        IMPORTING
          vector_for_direction TYPE REF TO lcl_vector
        RETURNING
          VALUE(result)        TYPE REF TO lcl_position.
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
    DATA(vector_for_direction) = vector_rule( direction ).
    DATA(next_position) = simulate_target( vector_for_direction ).

    IF is_target_clean( next_position ).
      position = next_position.
    ENDIF.
  ENDMETHOD.

  METHOD move_backward.
    DATA(vector_from_direction) = vector_rule( direction ).
    vector_from_direction->reverse( ).

    DATA(next_position) = simulate_target( vector_from_direction ).
    IF is_target_clean( next_position ).
      position = next_position.
    ENDIF.
  ENDMETHOD.

  METHOD vector_rule.
    DATA translation_rule TYPE TABLE OF ty_translation_rule WITH EMPTY KEY.

    translation_rule = VALUE #(
                      ( direction = lif_direction=>north vector = NEW #( delta_x = 0  delta_y = 1 ) )
                      ( direction = lif_direction=>south vector = NEW #( delta_x = 0  delta_y = -1 ) )
                      ( direction = lif_direction=>east  vector = NEW #( delta_x = 1  delta_y = 0 ) )
                      ( direction = lif_direction=>west  vector = NEW #( delta_x = -1 delta_y = 0 ) ) ).

    result = translation_rule[ direction = direction ]-vector.
  ENDMETHOD.

  METHOD retrieve_direction.
    result = direction.
  ENDMETHOD.

  METHOD rotate_to.
    me->direction = direction.
  ENDMETHOD.

  METHOD rotate_right.
    TYPES :
      BEGIN OF ty_target_cardinal,
        source_cardinal TYPE lif_direction=>cardinal,
        target_cardinal TYPE lif_direction=>cardinal,
      END OF ty_target_cardinal,
      tty_target_cardinal TYPE TABLE OF ty_target_cardinal WITH EMPTY KEY.

    DATA(right_cardinal) = VALUE tty_target_cardinal(
                            ( source_cardinal = lif_direction=>north target_cardinal = lif_direction=>east )
                            ( source_cardinal = lif_direction=>east  target_cardinal = lif_direction=>south )
                            ( source_cardinal = lif_direction=>south target_cardinal = lif_direction=>west )
                            ( source_cardinal = lif_direction=>west  target_cardinal = lif_direction=>north ) ).

    direction = right_cardinal[ source_cardinal = direction ]-target_cardinal.
  ENDMETHOD.

  METHOD rotate_left.
    TYPES :
      BEGIN OF ty_target_cardinal,
        source_cardinal TYPE lif_direction=>cardinal,
        target_cardinal TYPE lif_direction=>cardinal,
      END OF ty_target_cardinal,
      tty_target_cardinal TYPE TABLE OF ty_target_cardinal WITH EMPTY KEY.

    DATA(left_cardinal) = VALUE tty_target_cardinal(
                            ( source_cardinal = lif_direction=>north target_cardinal = lif_direction=>west )
                            ( source_cardinal = lif_direction=>west  target_cardinal = lif_direction=>south )
                            ( source_cardinal = lif_direction=>south target_cardinal = lif_direction=>east )
                            ( source_cardinal = lif_direction=>east  target_cardinal = lif_direction=>north ) ).

    direction = left_cardinal[ source_cardinal = direction ]-target_cardinal.
  ENDMETHOD.

  METHOD initialize_obstacles.
    me->obstacles = obstacle.
  ENDMETHOD.

  METHOD is_target_clean.
    result = abap_true.
    LOOP AT obstacles INTO DATA(obstacle_position).
      IF obstacle_position->coordinates-x = next_position->coordinates-x AND
         obstacle_position->coordinates-y = next_position->coordinates-y.
        result = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD simulate_target.
    result = NEW lcl_position( x = position->coordinates-x
                               y = position->coordinates-y ).
    result->translate( vector_for_direction ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_rover DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      initial_position_of_rover FOR TESTING,
      move_forward_to_north FOR TESTING,
      direction_of_rover_is_east FOR TESTING,
      move_forward_to_west FOR TESTING,
      rover_move_forward_to_south FOR TESTING,
      rover_move_forward_to_east FOR TESTING,
      move_backward_from_east FOR TESTING,
      face_west_turn_right_to_north FOR TESTING,
      face_north_turn_right_to_east FOR TESTING,
      face_west_turn_left_to_south FOR TESTING,
      stop_if_move_f_with_obst FOR TESTING,
      stop_if_move_b_with_obst FOR TESTING.
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

  METHOD move_forward_to_north.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>north ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 1 y = 3 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD move_forward_to_west.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>west ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 0 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_south.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 7 y = 9 )
                               direction = lif_direction=>south ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 7 y = 8 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_east.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 18 y = 25 )
                               direction = lif_direction=>east ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 19 y = 25 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD move_backward_from_east.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 18 y = 25 )
                               direction = lif_direction=>east ).

    cut->move_backward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 17 y = 25 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD face_west_turn_right_to_north.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 18 y = 25 )
                               direction = lif_direction=>west ).

    cut->rotate_right( ).
    cl_abap_unit_assert=>assert_equals( exp = lif_direction=>north
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD face_north_turn_right_to_east.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 18 y = 25 )
                               direction = lif_direction=>north ).

    cut->rotate_right( ).
    cl_abap_unit_assert=>assert_equals( exp = lif_direction=>east
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD face_west_turn_left_to_south.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 18 y = 25 )
                               direction = lif_direction=>west ).

    cut->rotate_left( ).
    cl_abap_unit_assert=>assert_equals( exp = lif_direction=>south
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD stop_if_move_f_with_obst.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 1 y = 4 )
                               direction = lif_direction=>north ).

    DATA(obstacles) = VALUE lcl_rover=>tt_obstacles( ( NEW lcl_position( x = 1  y = 5 ) )
                                                     ( NEW lcl_position( x = 17 y = 55 ) )
                                                     ( NEW lcl_position( x = 19 y = 51 ) )
                                                     ( NEW lcl_position( x = 0  y = 0 ) ) ).
    cut->initialize_obstacles( obstacles ).
    cut->move_forward( ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 1 y = 4 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD stop_if_move_b_with_obst.
    DATA(cut) = NEW lcl_rover( position  = NEW lcl_position( x = 16 y = 55 )
                               direction = lif_direction=>west ).

    DATA(obstacles) = VALUE lcl_rover=>tt_obstacles( ( NEW lcl_position( x = 1  y = 5 ) )
                                                     ( NEW lcl_position( x = 17 y = 55 ) )
                                                     ( NEW lcl_position( x = 19 y = 51 ) )
                                                     ( NEW lcl_position( x = 0  y = 0 ) ) ).
    cut->initialize_obstacles( obstacles ).
    cut->move_backward( ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE lcl_position=>ty_coordinates( x = 16 y = 55 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.
ENDCLASS.
