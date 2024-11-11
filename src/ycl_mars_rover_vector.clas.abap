CLASS ycl_mars_rover_vector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

 PUBLIC SECTION.
    DATA :
      delta_x TYPE i READ-ONLY,
      delta_y TYPE i READ-ONLY.

    METHODS: constructor
      IMPORTING delta_x TYPE i
                delta_y TYPE i,
      reverse
        RETURNING VALUE(result) TYPE REF TO ycl_mars_rover_vector.
ENDCLASS.

CLASS ycl_mars_rover_vector IMPLEMENTATION.
  METHOD constructor.
    me->delta_x = delta_x.
    me->delta_y = delta_y.
  ENDMETHOD.

  METHOD reverse.
    me->delta_x *= -1.
    me->delta_y *= -1.
  ENDMETHOD.
ENDCLASS.
