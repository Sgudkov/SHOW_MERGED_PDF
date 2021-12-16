CLASS lcl_html_viewer DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA:
      mv_pdf_data     TYPE xstring,
      mo_container    TYPE REF TO cl_gui_custom_container,
      mo_html_control TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS:
      pbo,
	  print,
      pdf_show,
      send_pdf_to_spool
        IMPORTING
          is_outputdocparams TYPE sfpoutputparams
          is_sfpjoboutput    TYPE sfpjoboutput.

ENDCLASS.

CLASS lcl_html_viewer IMPLEMENTATION.

  METHOD pdf_show.
    TYPES: lt_pdf_table(1000) TYPE x.

    DATA: l_url(80)  TYPE c,
          l_pdf_data TYPE STANDARD TABLE OF lt_pdf_table,
          l_pdf_line TYPE lt_pdf_table,
          l_offset   TYPE i,
          l_len      TYPE i.

* Variables to create unique URL for HTML control
    DATA: indexp TYPE i, indexn TYPE i.
    DATA: i_url(80) TYPE c, p_part(4) TYPE c, n_part(4) TYPE c.

    DATA: lt_html TYPE TABLE OF w3html.
    DATA: l_html TYPE w3html.
    DATA: l_filename TYPE text255.
    DATA: l_symsg TYPE symsg.
    DATA: num_part(5) TYPE n.

    l_len = xstrlen( mv_pdf_data ).
    WHILE l_len >= 1000.
      l_pdf_line = mv_pdf_data+l_offset(1000).
      APPEND l_pdf_line TO l_pdf_data.
      ADD 1000 TO l_offset.
      SUBTRACT 1000 FROM l_len.
    ENDWHILE.

    IF l_len > 0.
      l_pdf_line = mv_pdf_data+l_offset(l_len).
      APPEND l_pdf_line TO l_pdf_data.
    ENDIF.

    CONCATENATE '00000001' '.pdf' INTO i_url.

    l_len = xstrlen( mv_pdf_data ).
    IF l_len > 0.

      CALL METHOD mo_html_control->load_data
        EXPORTING
          url          = i_url
          size         = l_len
          type         = 'application'
          subtype      = 'pdf'
        IMPORTING
          assigned_url = l_url
        CHANGING
          data_table   = l_pdf_data
        EXCEPTIONS
          OTHERS       = 1.

    ENDIF.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* Show data
    CALL METHOD mo_html_control->show_url
      EXPORTING
        url    = l_url
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD pbo.

    DATA: l_pdf_alignment TYPE i,
          l_count         TYPE i,
          l_noprint       TYPE fpboolean,
          l_noarc         TYPE fpboolean,
          l_noprintarc    TYPE fpboolean.

* container
    IF mo_container IS INITIAL.
      CREATE OBJECT mo_container
        EXPORTING
          container_name = 'HTML'
        EXCEPTIONS
          OTHERS         = 1.
      IF sy-subrc <> 0.
        MESSAGE e150(fprunx).
        RETURN.
      ENDIF.
    ENDIF.
* html control
    IF mo_html_control IS INITIAL.
      CREATE OBJECT mo_html_control
        EXPORTING
          parent = mo_container
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        MESSAGE e150(fprunx).
        RETURN.
      ENDIF.
*   alignment
      l_pdf_alignment = mo_html_control->align_at_left  +
                        mo_html_control->align_at_right +
                        mo_html_control->align_at_top   +
                        mo_html_control->align_at_bottom.
      CALL METHOD mo_html_control->set_alignment
        EXPORTING
          alignment = l_pdf_alignment
        EXCEPTIONS
          OTHERS    = 1.
      IF sy-subrc <> 0.
        MESSAGE e150(fprunx).
        RETURN.
      ENDIF.
    ENDIF.

    pdf_show( ).

  ENDMETHOD.

  METHOD print.

    DATA:
      ls_outputparams TYPE sfpoutputparams,
      lt_pdf          TYPE tfpcontent.

    ls_outputparams-dest = p_dev.
    ls_outputparams-nodialog = abap_true.
    ls_outputparams-bumode   = 'M'.
    ls_outputparams-getpdf   = 'M'.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

	"Call your FM 


    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    "Get PDF like table type 
    CALL FUNCTION 'FPCOMP_GET_PDF_TABLE'
      IMPORTING
        e_pdf_table = lt_pdf.

    DATA(lo_pdf_merger) = NEW cl_rspo_pdf_merge( ).

    "Add documents
    LOOP AT lt_pdf ASSIGNING FIELD-SYMBOL(<ls_pdf>).
      lo_pdf_merger->add_document( <ls_pdf> ).
    ENDLOOP.
	
	"Merge documents
    lo_pdf_merger->merge_documents( IMPORTING merged_document = lcl_html_viewer=>mv_pdf_data ).
	
	"Send merged doc to spool
    send_pdf_to_spool(
      EXPORTING
        is_outputdocparams = ls_outputparams
        is_sfpjoboutput    = ls_sfpjoboutput
     ).


    "SHOW PDF
    CALL SCREEN 0500.

  ENDMETHOD.

 METHOD send_pdf_to_spool.

    DATA: size       TYPE i,
          total_size TYPE i,
          spoolid    TYPE rspoid,
          copies     TYPE rspocopies,
          lifetime,
          pages      TYPE fppagecount.

    size = xstrlen( lcl_html_viewer=>mv_pdf_data ).
    ADD size TO total_size.

    pages = is_sfpjoboutput-remaining_pages.

    DATA(ls_outputparams) = is_outputdocparams.

    copies = ls_outputparams-copies.
    lifetime = ls_outputparams-lifetime.

    CALL FUNCTION 'ADS_CREATE_PDF_SPOOLJOB'
      EXPORTING
        dest              = ls_outputparams-dest
        pages             = pages
        pdf_data          = lcl_html_viewer=>mv_pdf_data
        name              = ls_outputparams-dataset
        suffix1           = ls_outputparams-suffix1
        suffix2           = ls_outputparams-suffix2
        copies            = copies
        immediate_print   = ls_outputparams-reqimm
        auto_delete       = ls_outputparams-reqdel
        titleline         = ls_outputparams-covtitle
        receiver          = ls_outputparams-receiver
        division          = ls_outputparams-division
        authority         = ls_outputparams-authority
        lifetime          = lifetime
      IMPORTING
        spoolid           = spoolid
      EXCEPTIONS
        no_data           = 1
        not_pdf           = 2
        wrong_devtype     = 3
        operation_failed  = 4
        cannot_write_file = 5
        device_missing    = 6
        no_such_device    = 7
        OTHERS            = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.
  
ENDCLASS.