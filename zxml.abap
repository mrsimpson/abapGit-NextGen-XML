REPORT zxml.

* todo, how to handle raw XML stuff, like smartforms

CONSTANTS: gc_xml_version TYPE string VALUE 'v1.0.0',
           gc_abap_version TYPE string VALUE 'v1.0.0'.      "#EC NOTEXT

DEFINE _raise.
  RAISE EXCEPTION TYPE lcx_exception
    EXPORTING
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text     TYPE string
                ix_previous TYPE REF TO cx_root OPTIONAL.

  PRIVATE SECTION.
    DATA mx_previous TYPE REF TO cx_root.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
    mx_previous = previous.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.

***********************************************************************

CLASS lcl_xml DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor.

  PROTECTED SECTION.
    DATA: mi_ixml    TYPE REF TO if_ixml,
          mi_xml_doc TYPE REF TO if_ixml_document.

    CONSTANTS: c_abapgit_tag TYPE string VALUE 'abapGit'.

    METHODS to_xml
      IMPORTING iv_normalize  TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS parse
      IMPORTING iv_normalize TYPE abap_bool DEFAULT abap_true
        iv_xml TYPE string
      RAISING lcx_exception.

  PRIVATE SECTION.
    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   lcx_exception.

    METHODS display_xml_error
      RAISING lcx_exception.

ENDCLASS.

CLASS lcl_xml IMPLEMENTATION.

  METHOD constructor.
    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).
  ENDMETHOD.

  METHOD parse.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_element        TYPE REF TO if_ixml_element,
          li_version        TYPE REF TO if_ixml_node,
          li_parser         TYPE REF TO if_ixml_parser.


    ASSERT NOT iv_xml IS INITIAL.

    li_stream_factory = mi_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->set_normalizing( iv_normalize ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).

    li_element = mi_xml_doc->find_from_name( depth = 0 name = c_abapgit_tag ).
    li_version = li_element->if_ixml_node~get_attributes( )->get_named_item_ns( 'version' ).
    IF li_version->get_value( ) <> gc_xml_version.
      display_xml_error( ).
    ENDIF.

  ENDMETHOD.

  METHOD display_xml_error.

    DATA: lv_version TYPE string.


    lv_version = |abapGit version: { gc_abap_version }|.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'abapGit XML version mismatch'
        txt1  = 'abapGit XML version mismatch'
        txt2  = 'See https://github.com/larshp/abapGit/wiki/XML-Mismatch'
        txt3  = lv_version. "#EC NOTEXT

    _raise 'XML error'.

  ENDMETHOD.

  METHOD to_xml.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( iv_normalize ).

    li_renderer->render( ).

  ENDMETHOD.

  METHOD error.

    DATA: lv_error TYPE i,
          lv_txt1  TYPE string,
          lv_txt2  TYPE string,
          lv_txt3  TYPE string,
          lv_times TYPE i,
          li_error TYPE REF TO if_ixml_parse_error.


    IF ii_parser->num_errors( ) <> 0.
      lv_times = ii_parser->num_errors( ).
      DO lv_times TIMES.
        lv_error = sy-index - 1.
        li_error = ii_parser->get_error( lv_error ).

        lv_txt1 = li_error->get_column( ).
        CONCATENATE 'Column:' lv_txt1 INTO lv_txt1.         "#EC NOTEXT
        lv_txt2 = li_error->get_line( ).
        CONCATENATE 'Line:' lv_txt2 INTO lv_txt2.           "#EC NOTEXT
        lv_txt3 = li_error->get_reason( ).

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Error from XML parser'                 "#EC NOTEXT
            txt1  = lv_txt1
            txt2  = lv_txt2
            txt3  = lv_txt3.
      ENDDO.
    ENDIF.

    _raise 'Error while parsing XML'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_xml_output DEFINITION FINAL INHERITING FROM lcl_xml CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING iv_name TYPE clike
                  ig_data TYPE any,
      render
        IMPORTING iv_normalize TYPE sap_bool DEFAULT abap_true
        RETURNING VALUE(rv_xml) TYPE string.

  PRIVATE SECTION.
    DATA: mt_stab TYPE abap_trans_srcbind_tab.

ENDCLASS.

CLASS lcl_xml_output IMPLEMENTATION.

  METHOD add.

    FIELD-SYMBOLS: <ls_stab> LIKE LINE OF mt_stab.

    APPEND INITIAL LINE TO mt_stab ASSIGNING <ls_stab>.
    <ls_stab>-name = iv_name.
    GET REFERENCE OF ig_data INTO <ls_stab>-value.

  ENDMETHOD.

  METHOD render.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    CALL TRANSFORMATION id
      SOURCE (mt_stab)
      RESULT XML mi_xml_doc.

    li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
    mi_xml_doc->get_root( )->remove_child( li_abap ).

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = 'version' value = gc_xml_version ). "#EC NOTEXT
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_xml_input DEFINITION FINAL INHERITING FROM lcl_xml CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_xml TYPE clike
        RAISING lcx_exception,
      read
        IMPORTING iv_name TYPE clike
        CHANGING cg_data TYPE any
        RAISING lcx_exception.

  PRIVATE SECTION.
    METHODS: fix_xml.

ENDCLASS.

CLASS lcl_xml_input IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    parse( iv_xml ).
    fix_xml( ).

  ENDMETHOD.

  METHOD fix_xml.

    DATA: li_git            TYPE REF TO if_ixml_element,
          li_abap           TYPE REF TO if_ixml_node.


    li_git ?= mi_xml_doc->get_root( )->get_first_child( ).
    li_abap = li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

  ENDMETHOD.

  METHOD read.

    DATA: lv_text TYPE string,
          lx_error TYPE REF TO cx_transformation_error,
          lt_rtab TYPE abap_trans_resbind_tab.

    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF lt_rtab.


    APPEND INITIAL LINE TO lt_rtab ASSIGNING <ls_rtab>.
    <ls_rtab>-name = iv_name.
    GET REFERENCE OF cg_data INTO <ls_rtab>-value.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML mi_xml_doc
          RESULT (lt_rtab).
      CATCH cx_transformation_error INTO lx_error.
        lv_text = lx_error->if_message~get_text( ).
        _raise lv_text.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_xml DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_less,
           foo TYPE c LENGTH 1,
           bar TYPE c LENGTH 1,
         END OF ty_less.

    TYPES: BEGIN OF ty_structure,
             foo TYPE c LENGTH 2,
             /sdf/sdf TYPE c LENGTH 5,
           END OF ty_structure.

    TYPES: BEGIN OF ty_more,
             foo TYPE c LENGTH 1,
             bar TYPE c LENGTH 1,
             moo TYPE c LENGTH 1,
             st TYPE ty_structure,
           END OF ty_more.

    METHODS:
      multi FOR TESTING
        RAISING lcx_exception,
      less_to_more FOR TESTING
        RAISING lcx_exception,
      more_to_less FOR TESTING
        RAISING lcx_exception,
      sequence FOR TESTING
        RAISING lcx_exception,
      transformation_error FOR TESTING
        RAISING lcx_exception,
      char10_to_char20 FOR TESTING
        RAISING lcx_exception,
      char20_to_char10 FOR TESTING
        RAISING lcx_exception.

ENDCLASS.

CLASS ltcl_xml IMPLEMENTATION.

  METHOD transformation_error.

    DATA: lo_input TYPE REF TO lcl_xml_input,
          ls_less  TYPE ty_less,
          lv_xml   TYPE string.


    lv_xml = |<?xml version="1.0" encoding="utf-16"?>\n| &&
      |<abapGit version="v1.0.0">\n| &&
      |<asx:foobar xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
      |<asx:values>\n| &&
      |<DATA><FOO>A</FOO><BAR>C</BAR></DATA>\n| &&
      |</asx:values>\n| &&
      |</asx:foobar>\n| &&
      |</abapGit>\n|.

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.

    TRY.
        lo_input->read( EXPORTING iv_name = 'DATA'
                        CHANGING cg_data = ls_less ).
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD multi.

    DATA: lo_output TYPE REF TO lcl_xml_output,
          lo_input  TYPE REF TO lcl_xml_input,
          ls_less1  TYPE ty_less,
          ls_more1  TYPE ty_more,
          ls_less2  TYPE ty_less,
          ls_more2  TYPE ty_more,
          lv_xml    TYPE string.


    ls_less1-foo = 'Q'.
    ls_more1-moo = 'A'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'LESS'
                    ig_data = ls_less1 ).
    lo_output->add( iv_name = 'MORE'
                    ig_data = ls_more1 ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'LESS'
                    CHANGING cg_data = ls_less2 ).
    lo_input->read( EXPORTING iv_name = 'MORE'
                    CHANGING cg_data = ls_more2 ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_less1
        exp = ls_less2 ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_more1
        exp = ls_more2 ).

  ENDMETHOD.

  METHOD sequence.

    TYPES: BEGIN OF ty_sequence1,
             foo TYPE c LENGTH 2,
             bar TYPE c LENGTH 1,
           END OF ty_sequence1.

    TYPES: BEGIN OF ty_sequence2,
             bar TYPE c LENGTH 1,
             foo TYPE c LENGTH 2,
           END OF ty_sequence2.

    DATA: lo_output    TYPE REF TO lcl_xml_output,
          lo_input     TYPE REF TO lcl_xml_input,
          lv_xml       TYPE string,
          ls_sequence1 TYPE ty_sequence1,
          ls_sequence2 TYPE ty_sequence2.


    ls_sequence1-foo = 'AB'.
    ls_sequence1-bar = 'C'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_sequence1 ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_sequence2 ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_sequence2-foo
        exp = 'AB' ).

  ENDMETHOD.

  METHOD less_to_more.

    DATA: lo_output TYPE REF TO lcl_xml_output,
          lo_input  TYPE REF TO lcl_xml_input,
          lv_xml    TYPE string,
          ls_less   TYPE ty_less,
          ls_more   TYPE ty_more.


    ls_less-foo = 'F'.
    ls_less-bar = 'B'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_less ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_more ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_more-foo
        exp = 'F' ).

  ENDMETHOD.

  METHOD more_to_less.

    DATA: lo_output TYPE REF TO lcl_xml_output,
          lo_input  TYPE REF TO lcl_xml_input,
          lv_xml    TYPE string,
          ls_less   TYPE ty_less,
          ls_more   TYPE ty_more.


    ls_more-foo    = 'F'.
    ls_more-bar    = 'B'.
    ls_more-st-foo = 'A'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_more ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_less ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_less-foo
        exp = 'F' ).

  ENDMETHOD.

  METHOD char10_to_char20.

    CONSTANTS: c_value TYPE c LENGTH 10 VALUE 'ABAP_ROCKS'.

    DATA: lo_output TYPE REF TO lcl_xml_output,
          lo_input  TYPE REF TO lcl_xml_input,
          lv_xml    TYPE string,
          lv_char10 TYPE c LENGTH 10,
          lv_char20 TYPE c LENGTH 20.


    lv_char10 = c_value.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = lv_char10 ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = lv_char20 ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_char20
        exp = c_value ).

  ENDMETHOD.

  METHOD char20_to_char10.

    CONSTANTS: c_value TYPE c LENGTH 20 VALUE 'ABAP_ROCKS123456'.

    DATA: lo_output TYPE REF TO lcl_xml_output,
          lo_input  TYPE REF TO lcl_xml_input,
          lv_xml    TYPE string,
          lv_char10 TYPE c LENGTH 10,
          lv_char20 TYPE c LENGTH 20.


    lv_char20 = c_value.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = lv_char20 ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = lv_char10 ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_char10
        exp = c_value(10) ).

  ENDMETHOD.

ENDCLASS.
