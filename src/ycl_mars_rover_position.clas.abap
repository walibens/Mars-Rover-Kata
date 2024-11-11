CLASS ycl_mars_rover_position DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
        IMPORTING vector TYPE REF TO ycl_mars_rover_vector.
ENDCLASS.

CLASS ycl_mars_rover_position IMPLEMENTATION.
  METHOD constructor.
    coordinates-x = x.
    coordinates-y = y.
  ENDMETHOD.

  METHOD translate.
    coordinates-x += vector->delta_x.
    coordinates-y += vector->delta_y.
  ENDMETHOD.
ENDCLASS.
