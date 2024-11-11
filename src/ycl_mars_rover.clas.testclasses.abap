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
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>north ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 1 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD direction_of_rover_is_east.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>east ).

    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>east
                                        act = cut->retrieve_direction( ) ).
  ENDMETHOD.

  METHOD move_forward_to_north.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>north ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 1 y = 3 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD move_forward_to_west.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 2 )
                               direction = yif_mars_rover_direction=>west ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 0 y = 2 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_south.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 7 y = 9 )
                               direction = yif_mars_rover_direction=>south ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 7 y = 8 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD rover_move_forward_to_east.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>east ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 19 y = 25 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD move_backward_from_east.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>east ).

    cut->move_backward( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 17 y = 25 )
                                        act = cut->retrieve_position( ) ).
  ENDMETHOD.

  METHOD face_west_turn_right_to_north.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>west ).

    cut->rotate_right( ).
    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>north
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD face_north_turn_right_to_east.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>north ).

    cut->rotate_right( ).
    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>east
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD face_west_turn_left_to_south.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 18 y = 25 )
                               direction = yif_mars_rover_direction=>west ).

    cut->rotate_left( ).
    cl_abap_unit_assert=>assert_equals( exp = yif_mars_rover_direction=>south
                                        act = cut->retrieve_direction( ) ).

  ENDMETHOD.

  METHOD stop_if_move_f_with_obst.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 1 y = 4 )
                               direction = yif_mars_rover_direction=>north ).

    DATA(obstacles) = VALUE ycl_mars_rover=>tt_obstacles( ( NEW ycl_mars_rover_position( x = 1  y = 5 ) )
                                                     ( NEW ycl_mars_rover_position( x = 17 y = 55 ) )
                                                     ( NEW ycl_mars_rover_position( x = 19 y = 51 ) )
                                                     ( NEW ycl_mars_rover_position( x = 0  y = 0 ) ) ).
    cut->initialize_obstacles( obstacles ).
    TRY.
        cut->move_forward( ).
      CATCH ycx_mars_rover_obstacle_detect INTO DATA(exception).
        cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 1 y = 4 )
                                            act = cut->retrieve_position( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD stop_if_move_b_with_obst.
    DATA(cut) = NEW ycl_mars_rover( position  = NEW ycl_mars_rover_position( x = 16 y = 55 )
                               direction = yif_mars_rover_direction=>west ).

    DATA(obstacles) = VALUE ycl_mars_rover=>tt_obstacles( ( NEW ycl_mars_rover_position( x = 1  y = 5 ) )
                                                     ( NEW ycl_mars_rover_position( x = 17 y = 55 ) )
                                                     ( NEW ycl_mars_rover_position( x = 19 y = 51 ) )
                                                     ( NEW ycl_mars_rover_position( x = 0  y = 0 ) ) ).
    cut->initialize_obstacles( obstacles ).

    TRY.
        cut->move_backward( ).
      CATCH ycx_mars_rover_obstacle_detect INTO DATA(exception).
        cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_mars_rover_position=>ty_coordinates( x = 16 y = 55 )
                                            act = cut->retrieve_position( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
