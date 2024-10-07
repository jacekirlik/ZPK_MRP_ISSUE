*&---------------------------------------------------------------------*
*& Include          ZPR_MRP_ISSUE_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_get_data DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_result,
        material               TYPE C_MRPMasterDataIssueTP-material,
        plant                  TYPE C_MRPMasterDataIssueTP-plant,
        creationdatetime       TYPE C_MRPMasterDataIssueTP-creationdatetime,
        systemmessagetype      TYPE C_MRPMasterDataIssueTP-systemmessagetype,
        systemmessagenumber    TYPE C_MRPMasterDataIssueTP-systemmessagenumber,
        systemmessagevariable1 TYPE C_MRPMasterDataIssueTP-systemmessagevariable1,
        systemmessagevariable2 TYPE C_MRPMasterDataIssueTP-systemmessagevariable2,
        systemmessagevariable3 TYPE C_MRPMasterDataIssueTP-systemmessagevariable3,
        systemmessagevariable4 TYPE C_MRPMasterDataIssueTP-systemmessagevariable4,
        mrpmasterdataissuetext TYPE C_MRPMasterDataIssueTP-mrpmasterdataissuetext,
        msgli                  TYPE rm61r-msgli,
        dsdav                  TYPE rm61r-dsdav,
      END OF ts_result.
    TYPES: tt_result TYPE STANDARD TABLE OF ts_result WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_output,
        material            TYPE C_MRPMasterDataIssueTP-material,
        plant               TYPE C_MRPMasterDataIssueTP-plant,
        dsdav               TYPE rm61r-dsdav,
        systemmessagetype   TYPE C_MRPMasterDataIssueTP-systemmessagetype,
        systemmessagenumber TYPE C_MRPMasterDataIssueTP-systemmessagenumber,
        msgli               TYPE rm61r-msgli,
        zmailm              TYPE z_mailm,
        zmailc              TYPE z_mailc,
      END OF ts_output.
    TYPES: tt_output TYPE STANDARD TABLE OF ts_output WITH EMPTY KEY.


    METHODS: result_data
      RETURNING VALUE(lt_result) TYPE tt_result.

    METHODS: output_data
      IMPORTING it_result        TYPE tt_result
      RETURNING VALUE(lt_output) TYPE tt_output.
ENDCLASS.


CLASS lcl_get_data IMPLEMENTATION.
  METHOD result_data.
* Get result data
    SELECT
      material,
      plant,
      creationdatetime,
      systemmessagetype,
      systemmessagenumber,
      systemmessagevariable1,
      systemmessagevariable2,
      systemmessagevariable3,
      systemmessagevariable4,
      mrpmasterdataissuetext
    FROM C_MRPMasterDataIssueTP
    WHERE plant IN @s_plant
    INTO TABLE @lt_result.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).
      <fs_result>-msgli = <fs_result>-mrpmasterdataissuetext.
      REPLACE '& ' WITH '&1' INTO <fs_result>-msgli.
      REPLACE '& ' WITH '&2' INTO <fs_result>-msgli.
      REPLACE '& ' WITH '&3' INTO <fs_result>-msgli.
      REPLACE '& ' WITH '&4' INTO <fs_result>-msgli.
      REPLACE '&1' WITH <fs_result>-systemmessagevariable1 INTO <fs_result>-msgli.
      REPLACE '&2' WITH <fs_result>-systemmessagevariable2 INTO <fs_result>-msgli.
      REPLACE '&3' WITH <fs_result>-systemmessagevariable3 INTO <fs_result>-msgli.
      REPLACE '&4' WITH <fs_result>-systemmessagevariable4 INTO <fs_result>-msgli.
      CONDENSE <fs_result>-msgli.
      CONVERT TIME STAMP <fs_result>-creationdatetime TIME ZONE sy-zonlo INTO DATE <fs_result>-dsdav.
    ENDLOOP.

    SORT lt_result ASCENDING BY systemmessagetype plant material creationdatetime.
    DELETE lt_result WHERE systemmessagetype = 'S'
                     OR systemmessagetype = 'I'.
  ENDMETHOD.


  METHOD output_data.
    SELECT
      material,
      plant,
      dsdav,
      systemmessagetype,
      systemmessagenumber,
      msgli,
      zmailm,
      zmailc
    FROM @it_result AS result

    LEFT OUTER JOIN zmrpmsgmail
      ON zmrpmsgmail~werks = result~plant

    WHERE
* MsgType vs *
*--------------------
    ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 1, 1 )
    AND zmrpmsgmail~zmsgnr = '*' )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 3, 1 )
    AND zmrpmsgmail~zmsgnr = '*' )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 5, 1 )
    AND zmrpmsgmail~zmsgnr = '*' )

* MsgType vs MsgNR
*---------------------
    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 1, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 1, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 3, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 1, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 5, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 1, 3 ) )
*---------------------
    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 1, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 5, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 3, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 5, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 5, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 5, 3 ) )
*---------------------
    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 1, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 9, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 3, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 9, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 5, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 9, 3 ) )
*---------------------
    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 1, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 13, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 3, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 13, 3 ) )

    OR ( dsdav IN @s_date
    AND systemmessagetype = substring( zmrpmsgmail~zmsgty, 5, 1 )
    AND systemmessagenumber = substring( zmrpmsgmail~zmsgnr, 13, 3 ) )

    INTO CORRESPONDING FIELDS OF TABLE @lt_output.
  ENDMETHOD.
ENDCLASS.





CLASS lcl_set_output DEFINITION INHERITING FROM lcl_get_data.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_alvmail,
        material            TYPE C_MRPMasterDataIssueTP-material,
        plant               TYPE C_MRPMasterDataIssueTP-plant,
        dsdav               TYPE rm61r-dsdav,
        systemmessagetype   TYPE C_MRPMasterDataIssueTP-systemmessagetype,
        systemmessagenumber TYPE C_MRPMasterDataIssueTP-systemmessagenumber,
        msgli               TYPE rm61r-msgli,
        zmailm              TYPE z_mailm,
        zmailc              TYPE z_mailc,
        t_color             TYPE lvc_t_scol,
      END OF ts_alvmail.
    TYPES: tt_alvmail TYPE STANDARD TABLE OF ts_alvmail WITH EMPTY KEY.

    DATA: lt_alvmail TYPE tt_alvmail.


    METHODS: alv_recipient
      IMPORTING it_output TYPE tt_output.

    METHODS: email_send
      IMPORTING it_output TYPE tt_output.
ENDCLASS.


CLASS lcl_set_output IMPLEMENTATION.
  METHOD alv_recipient.
    MOVE-CORRESPONDING it_output TO lt_alvmail.

    LOOP AT lt_alvmail ASSIGNING FIELD-SYMBOL(<fs_alvmail>) WHERE systemmessagetype = 'E' OR systemmessagetype = 'A'.
      CLEAR gs_color.
      gs_color-fname = 'SYSTEMMESSAGETYPE'.
      gs_color-color-col = 6.
      gs_color-color-int = 0.
      APPEND gs_color TO <fs_alvmail>-t_color.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = cl_salv
                                CHANGING t_table = lt_alvmail ).

        cl_columns = cl_salv->get_columns( ).
        cl_columns->set_color_column( 'T_COLOR' ).
        cl_columns->set_optimize( ).

        cl_display = cl_salv->get_display_settings( ).
        cl_display->set_list_header( |MRP Error Notification| ).

        cl_salv->get_functions( )->set_all( abap_true ).
        cl_salv->display( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD email_send.
    lt_mails = CORRESPONDING #( it_output MAPPING werks = plant
                                                  zmailm = zmailm
                                                  zmailc = zmailc ).
    SORT lt_mails ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_mails.

    lv_sender = sy-uname.

    LOOP AT lt_mails ASSIGNING FIELD-SYMBOL(<fs_mails>).
* Prepare table for email
      CLEAR gs_mailtxt.
      CLEAR gt_mailtxt.
      gs_mailtxt-line = |<html> <body>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<p>Dear user,<br>Please check MRP messages below:</br></p>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<table style="MARGIN: 10px" bordercolor="#abb2b9"|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |cellspacing="0" cellpadding="3" border="1">|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<tbody><tr>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<th bgcolor="#d5d8dc">Material</th>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<th bgcolor="#d5d8dc">Plant</th>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<th bgcolor="#d5d8dc">MsgType</th>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<th bgcolor="#d5d8dc">MsgNr</th>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<th bgcolor="#d5d8dc">Message</th></tr>|.
      APPEND gs_mailtxt TO gt_mailtxt.


* Fill the table
      LOOP AT it_output ASSIGNING FIELD-SYMBOL(<fs_output>).
        CLEAR gs_mailtxt.
        IF <fs_mails>-werks = <fs_output>-plant
          AND <fs_mails>-zmailm = <fs_output>-zmailm
          AND <fs_mails>-zmailc = <fs_output>-zmailc.
          IF <fs_output>-systemmessagetype = 'E' OR <fs_output>-systemmessagetype = 'A'.
            gs_mailtxt-line = |<tr style="background-color:#f1948a"><td>{ <fs_output>-material }</td>|.
            APPEND gs_mailtxt TO gt_mailtxt.
          ELSE.
            gs_mailtxt-line = |<tr><td>{ <fs_output>-material }</td>|.
            APPEND gs_mailtxt TO gt_mailtxt.
          ENDIF.
          gs_mailtxt-line = |<td>{ <fs_output>-plant }</td>|.
          APPEND gs_mailtxt TO gt_mailtxt.
          gs_mailtxt-line = |<td>{ <fs_output>-systemmessagetype }</td>|.
          APPEND gs_mailtxt TO gt_mailtxt.
          gs_mailtxt-line = |<td>{ <fs_output>-systemmessagenumber }</td>|.
          APPEND gs_mailtxt TO gt_mailtxt.
          gs_mailtxt-line = |<td>{ <fs_output>-msgli }</td></tr>|.
          APPEND gs_mailtxt TO gt_mailtxt.
        ENDIF.
      ENDLOOP.

* End of table + footer
      gs_mailtxt-line = |</tbody> </table>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |<p>For more details check Fiori App: |.
      APPEND gs_mailtxt TO gt_mailtxt.
      DATA(url) = '"Direct link to Fiori App"'.
      gs_mailtxt-line = |<a href={ url }>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |Display MRP Master Data Issues</a></p>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      gs_mailtxt-line = |</body> </html>|.
      APPEND gs_mailtxt TO gt_mailtxt.
      CLEAR gs_mailtxt.



* set subject
      lv_subject = |Plant { <fs_mails>-werks }| & | - | & |MRP Error Notification|.


      TRY.
* creeate document (email)
          lo_document = cl_document_bcs=>create_document( i_type = 'HTM'
                                                                i_text = gt_mailtxt
                                                                i_subject = CONV so_obj_des( lv_subject )
                                                                ).

* create send request
          lo_send_request = cl_bcs=>create_persistent( ).
          lo_send_request->set_message_subject( ip_subject = lv_subject ).
          lo_send_request->set_document( lo_document ).

* create sender
          lo_sender = cl_sapuser_bcs=>create( lv_sender ).
          lo_send_request->set_sender( lo_sender ).



* set recpipients
          CLEAR gs_mailrec.
          CLEAR gt_mailrec.
          IF <fs_mails>-zmailm IS NOT INITIAL.
            FIND ',' IN <fs_mails>-zmailm.
            IF sy-subrc <> 0.
              gs_mailrec-receiver = <fs_mails>-zmailm.
              APPEND gs_mailrec TO gt_mailrec.
              CLEAR gs_mailrec.
            ELSE.
              SPLIT <fs_mails>-zmailm AT ',' INTO TABLE DATA(lt_segments_m).
              LOOP AT lt_segments_m INTO DATA(segment_m).
                gs_mailrec-receiver = segment_m.
                APPEND gs_mailrec TO gt_mailrec.
                CLEAR gs_mailrec.
              ENDLOOP.
            ENDIF.

            LOOP AT gt_mailrec INTO gs_mailrec WHERE copy = abap_false.
              lv_recipient = gs_mailrec-receiver.
              lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_recipient ).
              lo_send_request->add_recipient( i_recipient = lo_recipient ).
              CLEAR lv_recipient.
            ENDLOOP.
          ENDIF.


          IF <fs_mails>-zmailc IS NOT INITIAL.
            FIND ',' IN <fs_mails>-zmailc.
            IF sy-subrc <> 0.
              gs_mailrec-receiver = <fs_mails>-zmailc.
              gs_mailrec-copy = abap_true.
              APPEND gs_mailrec TO gt_mailrec.
              CLEAR gs_mailrec.
            ELSE.
              SPLIT <fs_mails>-zmailc AT ',' INTO TABLE DATA(lt_segments_c).
              LOOP AT lt_segments_c INTO DATA(segment_c).
                gs_mailrec-receiver = segment_c.
                gs_mailrec-copy = abap_true.
                APPEND gs_mailrec TO gt_mailrec.
                CLEAR gs_mailrec.
              ENDLOOP.
            ENDIF.

            LOOP AT gt_mailrec INTO gs_mailrec WHERE copy = abap_true.
              lv_recipient = gs_mailrec-receiver.
              lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_recipient ).
              lo_send_request->add_recipient( i_recipient = lo_recipient
                                              i_copy = abap_true ).
              CLEAR lv_recipient.
            ENDLOOP.
          ENDIF.

* send document (email)
          lo_send_request->send( ).
          COMMIT WORK.
        CATCH cx_bcs.
      ENDTRY.

    ENDLOOP.
    MESSAGE 'Email sent' TYPE 'S'.
  ENDMETHOD.
ENDCLASS.
