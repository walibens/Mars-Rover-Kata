"!https://kata-log.rocks/mars-rover-kata
REPORT yr_mars_rover_wbs.



*INTERFACE lif_direction.
*  TYPES :
*    BEGIN OF ENUM cardinal,
*      north,
*      south,
*      east,
*      west,
*    END OF ENUM cardinal.
*ENDINTERFACE.

CLASS lcx_obstacle_detected DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    DATA obstacle_position TYPE REF TO ycl_mars_rover_position.
    METHODS constructor
      IMPORTING
        obstacle_position TYPE REF TO ycl_mars_rover_position.
ENDCLASS.

CLASS lcx_obstacle_detected IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    me->obstacle_position = obstacle_position.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rover DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES :
      tt_obstacles TYPE TABLE OF REF TO ycl_mars_rover_position WITH EMPTY KEY.
    METHODS :
      constructor
        IMPORTING position  TYPE REF TO ycl_mars_rover_position
                  direction TYPE yif_mars_rover_direction=>cardinal,
      retrieve_position
        RETURNING VALUE(result) TYPE ycl_mars_rover_position=>ty_coordinates,
      move_forward
        RAISING lcx_obstacle_detected,
      move_backward
        RAISING lcx_obstacle_detected,
      retrieve_direction
        RETURNING VALUE(result) TYPE yif_mars_rover_direction=>cardinal,
      rotate_right,
      rotate_left,
      initialize_obstacles
        IMPORTING obstacle TYPE tt_obstacles.

  PRIVATE SECTION.
    TYPES :
      BEGIN OF ty_translation_rule,
        direction TYPE yif_mars_rover_direction=>cardinal,
        vector    TYPE REF TO ycl_mars_rover_vector,
      END OF ty_translation_rule.

    TYPES :
      BEGIN OF ty_target_cardinal,
        source_cardinal TYPE yif_mars_rover_direction=>cardinal,
        target_cardinal TYPE yif_mars_rover_direction=>cardinal,
      END OF ty_target_cardinal,
      tty_target_cardinal TYPE TABLE OF ty_target_cardinal WITH EMPTY KEY.

    DATA :
      position  TYPE REF TO ycl_mars_rover_position,
      direction TYPE yif_mars_rover_direction=>cardinal,
      obstacles TYPE TABLE OF REF TO ycl_mars_rover_position.

    METHODS:
      vector_rule
        IMPORTING direction     TYPE yif_mars_rover_direction=>cardinal
        RETURNING VALUE(result) TYPE REF TO ycl_mars_rover_vector,
      land_in
        IMPORTING position TYPE REF TO ycl_mars_rover_position,
      rotate_to
        IMPORTING direction TYPE yif_mars_rover_direction=>cardinal,
      is_obstacle_detected
        IMPORTING next_position TYPE REF TO ycl_mars_rover_position
        RETURNING VALUE(result) TYPE abap_bool,
      simulate_target
        IMPORTING vector_for_direction TYPE REF TO ycl_mars_rover_vector
        RETURNING VALUE(result)        TYPE REF TO ycl_mars_rover_position.
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

    IF is_obstacle_detected( next_position ).
      RAISE EXCEPTION NEW lcx_obstacle_detected( next_position ).
    ENDIF.
    position = next_position.
  ENDMETHOD.

  METHOD move_backward.
    DATA(vector_from_direction) = vector_rule( direction ).
    vector_from_direction->reverse( ).
    DATA(next_position) = simulate_target( vector_from_direction ).

    IF is_obstacle_detected( next_position ).
      RAISE EXCEPTION NEW lcx_obstacle_detected( next_position ).
    ENDIF.
    position = next_position.
  ENDMETHOD.

  METHOD vector_rule.
    DATA translation_rule TYPE TABLE OF ty_translation_rule WITH EMPTY KEY.

    translation_rule = VALUE #(
                      ( direction = yif_mars_rover_direction=>north vector = NEW #( delta_x = 0  delta_y = 1 ) )
                      ( direction = yif_mars_rover_direction=>south vector = NEW #( delta_x = 0  delta_y = -1 ) )
                      ( direction = yif_mars_rover_direction=>east  vector = NEW #( delta_x = 1  delta_y = 0 ) )
                      ( direction = yif_mars_rover_direction=>west  vector = NEW #( delta_x = -1 delta_y = 0 ) ) ).

    result = translation_rule[ direction = direction ]-vector.
  ENDMETHOD.

  METHOD retrieve_direction.
    result = direction.
  ENDMETHOD.

  METHOD rotate_to.
    me->direction = direction.
  ENDMETHOD.

  METHOD rotate_right.
    DATA(right_cardinal) = VALUE tty_target_cardinal(
                            ( source_cardinal = yif_mars_rover_direction=>north target_cardinal = yif_mars_rover_direction=>east )
                            ( source_cardinal = yif_mars_rover_direction=>east  target_cardinal = yif_mars_rover_direction=>south )
                            ( source_cardinal = yif_mars_rover_direction=>south target_cardinal = yif_mars_rover_direction=>west )
                            ( source_cardinal = yif_mars_rover_direction=>west  target_cardinal = yif_mars_rover_direction=>north ) ).

    direction = right_cardinal[ source_cardinal = direction ]-target_cardinal.
  ENDMETHOD.

  METHOD rotate_left.
    DATA(left_cardinal) = VALUE tty_target_cardinal(
                            ( source_cardinal = yif_mars_rover_direction=>north target_cardinal = yif_mars_rover_direction=>west )
                            ( source_cardinal = yif_mars_rover_direction=>west  target_cardinal = yif_mars_rover_direction=>south )
                            ( source_cardinal = yif_mars_rover_direction=>south target_cardinal = yif_mars_rover_direction=>east )
                            ( source_cardinal = yif_mars_rover_direction=>east  target_cardinal = yif_mars_rover_direction=>north ) ).

    direction = left_cardinal[ source_cardinal = direction ]-target_cardinal.
  ENDMETHOD.

  METHOD initialize_obstacles.
    me->obstacles = obstacle.
  ENDMETHOD.

  METHOD is_obstacle_detected.
    LOOP AT obstacles INTO DATA(obstacle_position).
      IF obstacle_position->coordinates-x = next_position->coordinates-x AND
         obstacle_position->coordinates-y = next_position->coordinates-y.
        result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD simulate_target.
    result = NEW ycl_mars_rover_position( x = position->coordinates-x
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
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>north ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 1 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD direction_of_rover_is_east.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>east ).

    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>east
                                        act = cut->retrieve_direction( ) ).
  ENDMETHOD.

  METHOD move_forward_to_north.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>north ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 1 y = 3 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD move_forward_to_west.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>west ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 0 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_south.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 7 y = 9 )
                               direction = yif_mars_rover_direction=>south ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 7 y = 8 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_east.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>east ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 19 y = 25 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD move_backward_from_east.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>east ).

    cut->move_backward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 17 y = 25 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD face_west_turn_right_to_north.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>west ).

    cut->rotate_right( ).
    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>north
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD face_north_turn_right_to_east.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>north ).

    cut->rotate_right( ).
    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>east
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD face_west_turn_left_to_south.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>west ).

    cut->rotate_left( ).
    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>south
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD stop_if_move_f_with_obst.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 4 )
                               direction = yif_mars_rover_direction=>north ).

    DATA(obstacles) = VALUE lcl_rover=>tt_obstacles( ( NEW ycl_mars_rover_position( x = 1  y = 5 ) )
                                                     ( NEW ycl_mars_rover_position( x = 17 y = 55 ) )
                                                     ( NEW ycl_mars_rover_position( x = 19 y = 51 ) )
                                                     ( NEW ycl_mars_rover_position( x = 0  y = 0 ) ) ).
    cut->initialize_obstacles( obstacles ).
    TRY.
        cut->move_forward( ).
      CATCH lcx_obstacle_detected INTO DATA(exception).
        cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 1 y = 4 )
                                            act = cut->retrieve_position( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD stop_if_move_b_with_obst.
    DATA(cut) = NEW lcl_rover( position  = NEW ycl_mars_rover_position( x = 16 y = 55 )
                               direction = yif_mars_rover_direction=>west ).

    DATA(obstacles) = VALUE lcl_rover=>tt_obstacles( ( NEW ycl_mars_rover_position( x = 1  y = 5 ) )
                                                     ( NEW ycl_mars_rover_position( x = 17 y = 55 ) )
                                                     ( NEW ycl_mars_rover_position( x = 19 y = 51 ) )
                                                     ( NEW ycl_mars_rover_position( x = 0  y = 0 ) ) ).
    cut->initialize_obstacles( obstacles ).

    TRY.
        cut->move_backward( ).
      CATCH lcx_obstacle_detected INTO DATA(exception).
        cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 16 y = 55 )
                                            act = cut->retrieve_position( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
