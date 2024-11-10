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

CLASS lcl_rover DEFINITION FINAL.

  PUBLIC SECTION.
    DATA :
      x TYPE i READ-ONLY,
      y TYPE i READ-ONLY.

    METHODS constructor
      IMPORTING x         TYPE i
                y         TYPE i
                direction TYPE lif_direction=>cardinal.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_rover IMPLEMENTATION.
  METHOD constructor.
    me->x = x.
    me->y = y.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_rover DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: initial_position_of_rover FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_rover IMPLEMENTATION.
  METHOD initial_position_of_rover.
    DATA(cut) = NEW lcl_rover( x = 1 y = 2 direction = lif_direction=>north ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->x ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->y ).
  ENDMETHOD.
ENDCLASS.
