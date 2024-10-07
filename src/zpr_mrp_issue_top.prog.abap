*&---------------------------------------------------------------------*
*& Include          ZPR_MRP_ISSUE_TOP
*&---------------------------------------------------------------------*
  DATA: gs_C_MRPMasterDataIssueTP TYPE C_MRPMasterDataIssueTP.

  DATA: gt_mailtxt TYPE TABLE OF solisti1,
        gs_mailtxt TYPE solisti1.

  DATA: gs_color   TYPE lvc_s_scol.

  DATA: cl_salv    TYPE REF TO cl_salv_table,
        cl_columns TYPE REF TO cl_salv_columns_table,
        cl_display TYPE REF TO cl_salv_display_settings.

  DATA: lt_mails TYPE TABLE OF zmrpmsgmail.

  DATA: gt_mailrec TYPE TABLE OF somlreci1,
        gs_mailrec TYPE  somlreci1.

  DATA: lv_subject   TYPE string,
        lv_sender    TYPE uname,
        lv_recipient TYPE ad_smtpadr.

  DATA: lo_document     TYPE REF TO cl_document_bcs,
        lo_send_request TYPE REF TO cl_bcs,
        lo_sender       TYPE REF TO cl_sapuser_bcs,
        lo_recipient    TYPE REF TO cl_cam_address_bcs.
