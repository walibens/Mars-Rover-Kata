"!https://kata-log.rocks/mars-rover-kata
report yr_mars_rover_wbs.

interface lif_direction.
  types :
    begin of enum cardinal,
      north,
      south,
      east,
      west,
    end of enum cardinal.
endinterface.

class lcl_position definition final.
  public section.
    types :
      begin of ty_coordinates,
        x type i,
        y type i,
      end of ty_coordinates.

    data :
      coordinates type ty_coordinates read-only.

    methods :
      constructor
        importing x type i
                  y type i,
      translate
        importing delta_x type i
                  delta_y type i.
endclass.

class lcl_position implementation.
  method constructor.
    coordinates-x = x.
    coordinates-y = y.
  endmethod.

  method translate.
    coordinates-x += delta_x.
    coordinates-y += delta_y.
  endmethod.
endclass.

class lcl_rover definition final.
  public section.
    methods :
      constructor
        importing position  type ref to lcl_position
                  direction type lif_direction=>cardinal,
      retrieve_position
        returning value(result) type lcl_position=>ty_coordinates,
      move_forward,
      retrieve_direction
        returning value(result) type lif_direction=>cardinal.

  private section.
    data :
      position  type ref to lcl_position,
      direction type lif_direction=>cardinal.
    methods
      land_in
        importing position type ref to lcl_position.
    methods rotate_to
      importing
        direction type lif_direction=>cardinal.
endclass.

class lcl_rover implementation.
  method constructor.
    land_in( position ).
    rotate_to( direction ).
  endmethod.

  method land_in.
    me->position = position.
  endmethod.

  method retrieve_position.
    result = position->coordinates.
  endmethod.

  method move_forward.
    position->translate( delta_x = 0 delta_y = 1 ).
  endmethod.

  method retrieve_direction.
    result = direction.
  endmethod.

  method rotate_to.
    me->direction = direction.
  endmethod.
endclass.

class ltc_rover definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      initial_position_of_rover for testing,
      rover_move_forward for testing,
      direction_of_rover_is_east for testing.

endclass.

class ltc_rover implementation.
  method initial_position_of_rover.
    data(cut) = new lcl_rover( position  = new lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>north ).

    cl_abap_unit_assert=>assert_equals( exp = value lcl_position=>ty_coordinates( x = 1 y = 2 )
                                        act = cut->retrieve_position( ) ).
  endmethod.

  method direction_of_rover_is_east.
    data(cut) = new lcl_rover( position  = new lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>east ).

    cl_abap_unit_assert=>assert_equals( exp = lif_direction=>east
                                        act = cut->retrieve_direction( ) ).
  endmethod.

  method rover_move_forward.
    data(cut) = new lcl_rover( position  = new lcl_position( x = 1 y = 2 )
                               direction = lif_direction=>north ).

    cut->move_forward( ).
    cl_abap_unit_assert=>assert_equals( exp = value lcl_position=>ty_coordinates( x = 1 y = 3 )
                                        act = cut->retrieve_position( ) ).
  endmethod.
endclass.
