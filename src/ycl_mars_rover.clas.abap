CLASS ycl_mars_rover DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
        RAISING ycx_mars_rover_obstacle_detect,
      move_backward
        RAISING ycx_mars_rover_obstacle_detect,
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

CLASS ycl_mars_rover IMPLEMENTATION.
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
      RAISE EXCEPTION NEW ycx_mars_rover_obstacle_detect( next_position ).
    ENDIF.
    position = next_position.
  ENDMETHOD.

  METHOD move_backward.
    DATA(vector_from_direction) = vector_rule( direction ).
    vector_from_direction->reverse( ).
    DATA(next_position) = simulate_target( vector_from_direction ).

    IF is_obstacle_detected( next_position ).
      RAISE EXCEPTION NEW ycx_mars_rover_obstacle_detect( next_position ).
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
