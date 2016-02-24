REPORT zxml.

CONSTANTS: gc_xml_version  TYPE string VALUE 'v0.next'.

CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.

DEFINE _raise.
  RAISE EXCEPTION TYPE lcx_exception.                       "#EC NOTEXT
  write: &1.
END-OF-DEFINITION.

* REQUIREMENTS:
* rewrite of lot of the serialization to put all data into one structure

* todo, how to handle raw XML stuff, like smartforms
* todo, add minor versionining in constructor
* todo, error handling for CALL TRANSFORMATION
* todo, check abapGit xml versions

CLASS lcl_xml DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_xml TYPE string OPTIONAL,
      build
        IMPORTING ig_any  TYPE any
        RETURNING VALUE(rv_xml) TYPE string,
      parse
        CHANGING cg_any TYPE any
        RAISING lcx_exception.

  PRIVATE SECTION.
    DATA: mv_xml     TYPE string,
          mi_ixml    TYPE REF TO if_ixml,
          mi_xml_doc TYPE REF TO if_ixml_document.

    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   lcx_exception.

    METHODS render
      RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.

CLASS lcl_xml IMPLEMENTATION.

  METHOD constructor.

    mv_xml = iv_xml.
    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).

  ENDMETHOD.

  METHOD render.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_string ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->render( ).

  ENDMETHOD.

  METHOD build.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    CALL TRANSFORMATION id
      SOURCE data = ig_any
      RESULT XML mi_xml_doc.

    li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
    mi_xml_doc->get_root( )->remove_child( li_abap ).

    li_git = mi_xml_doc->create_element( 'abapGit' ).
    li_git->set_attribute( name = 'version' value = gc_xml_version ). "#EC NOTEXT
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = render( ).

  ENDMETHOD.

  METHOD parse.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          li_node           TYPE REF TO if_ixml_node,
          li_git            TYPE REF TO if_ixml_element,
          li_abap           TYPE REF TO if_ixml_element.


    ASSERT NOT mv_xml IS INITIAL.

    li_stream_factory = mi_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( mv_xml ).
    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->set_normalizing( abap_false ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).

    li_git ?= mi_xml_doc->get_root( )->get_first_child( ).
    li_abap ?= li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

    CALL TRANSFORMATION id
      SOURCE XML mi_xml_doc
      RESULT data = cg_any.

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

CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
      TYPES: BEGIN OF ty_less,
             foo TYPE c LENGTH 1,
             bar TYPE c LENGTH 1,
           END OF ty_less.

    TYPES: BEGIN OF structure,
             foo TYPE c LENGTH 2,
           END OF structure.

    TYPES: BEGIN OF ty_more,
             foo TYPE c LENGTH 1,
             bar TYPE c LENGTH 1,
             moo TYPE c LENGTH 1,
             st TYPE structure,
           END OF ty_more.

    METHODS:
      less_to_more FOR TESTING
        RAISING lcx_exception,
      more_to_less FOR TESTING
        RAISING lcx_exception.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD less_to_more.

    DATA: lo_xml  TYPE REF TO lcl_xml,
          lv_xml  TYPE string,
          ls_less TYPE ty_less,
          ls_more TYPE ty_more.


    ls_less-foo = 'F'.
    ls_less-bar = 'B'.

    CREATE OBJECT lo_xml.
    lv_xml = lo_xml->build( ls_less ).

    CREATE OBJECT lo_xml
      EXPORTING
        iv_xml = lv_xml.
    lo_xml->parse( CHANGING cg_any = ls_more ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_more-foo
        exp = 'F' ).

  ENDMETHOD.

  METHOD more_to_less.

    DATA: lo_xml  TYPE REF TO lcl_xml,
          lv_xml  TYPE string,
          ls_less TYPE ty_less,
          ls_more TYPE ty_more.


    ls_more-foo = 'F'.
    ls_more-bar = 'B'.
    ls_more-st-foo = 'A'.

    CREATE OBJECT lo_xml.
    lv_xml = lo_xml->build( ls_more ).

    CREATE OBJECT lo_xml
      EXPORTING
        iv_xml = lv_xml.
    lo_xml->parse( CHANGING cg_any = ls_less ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_less-foo
        exp = 'F' ).

  ENDMETHOD.

ENDCLASS.
