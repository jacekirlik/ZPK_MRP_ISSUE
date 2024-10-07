*&---------------------------------------------------------------------*
*& Include          ZPR_MRP_ISSUE_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK main WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    s_plant FOR gs_C_MRPMasterDataIssueTP-plant ,
    s_date FOR sy-datum.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_mail RADIOBUTTON GROUP rb1 DEFAULT 'X',
              p_alvr RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK main.
