interface ZIF_BC_BO_BLOCKCHAIN_C
  public .


  interfaces /BOBF/IF_LIB_CONSTANTS .

  constants:
    BEGIN OF SC_ACTION,
      BEGIN OF BLOCK,
 CREATE_BLOCK                   TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741AC1CB',
 DELETE_BLOCK                   TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741B41CB',
 SAVE_BLOCK                     TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741BC1CB',
 UPDATE_BLOCK                   TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741B01CB',
 VALIDATE_BLOCK                 TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741B81CB',
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
 CREATE_EXECUTION_LOG           TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9B1E9211C3722A3E5',
 DELETE_EXECUTION_LOG           TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9B1E9211C372323E5',
 SAVE_EXECUTION_LOG             TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9B1E9211C3723A3E5',
 UPDATE_EXECUTION_LOG           TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9B1E9211C3722E3E5',
 VALIDATE_EXECUTION_LOG         TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9B1E9211C372363E5',
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
 CREATE_ROOT                    TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC8207417A1CB',
 DELETE_ROOT                    TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741821CB',
 LOCK_ROOT                      TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC8207416E1CB',
 PROCESS_POW                    TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9B0D1550A450EF37C',
 SAVE_ROOT                      TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC8207418A1CB',
 UNLOCK_ROOT                    TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741721CB',
 UPDATE_ROOT                    TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC8207417E1CB',
 VALIDATE_ROOT                  TYPE /BOBF/ACT_KEY VALUE '06789A0505941ED9AF9DC820741861CB',
      END OF ROOT,
    END OF SC_ACTION .
  constants:
    BEGIN OF SC_ACTION_ATTRIBUTE,
        BEGIN OF ROOT,
        BEGIN OF LOCK_ROOT,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
 LEGACY_DAC_KEY                 TYPE STRING VALUE 'LEGACY_DAC_KEY',
        END OF LOCK_ROOT,
        BEGIN OF PROCESS_POW,
 ONLY_NEW_BLOCKS                TYPE STRING VALUE 'ONLY_NEW_BLOCKS',
 ID_REQUEST                     TYPE STRING VALUE 'ID_REQUEST',
 MODIFY_BLOCK                   TYPE STRING VALUE 'MODIFY_BLOCK',
        END OF PROCESS_POW,
        BEGIN OF UNLOCK_ROOT,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
 LEGACY_DAC_KEY                 TYPE STRING VALUE 'LEGACY_DAC_KEY',
        END OF UNLOCK_ROOT,
      END OF ROOT,
    END OF SC_ACTION_ATTRIBUTE .
  constants:
    BEGIN OF SC_ALTERNATIVE_KEY,
      BEGIN OF BLOCK,
 KEY_BLOCK                      TYPE /BOBF/OBM_ALTKEY_KEY VALUE '06789A0505941ED9AF9DC820741E21CB',
      END OF BLOCK,
      BEGIN OF ROOT,
 KEY_BLOCK                      TYPE /BOBF/OBM_ALTKEY_KEY VALUE '06789A0505941ED9AF9DC820741C81CB',
      END OF ROOT,
    END OF SC_ALTERNATIVE_KEY .
  constants:
    BEGIN OF SC_ASSOCIATION,
      BEGIN OF BLOCK,
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741A61CB',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741AA1CB',
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741C01CB',
 TO_ROOT                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741C21CB',
      END OF BLOCK,
      BEGIN OF BLOCK_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741C41CB',
      END OF BLOCK_MESSAGE,
      BEGIN OF BLOCK_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741C61CB',
      END OF BLOCK_PROPERTY,
      BEGIN OF EXECUTION_LOG,
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C372243E5',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C372283E5',
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C3723E3E5',
 TO_ROOT                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C372403E5',
      END OF EXECUTION_LOG,
      BEGIN OF EXECUTION_LOG_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C372423E5',
      END OF EXECUTION_LOG_MESSAGE,
      BEGIN OF EXECUTION_LOG_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C372443E5',
      END OF EXECUTION_LOG_PROPERTY,
      BEGIN OF ROOT,
 BLOCK                          TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741A01CB',
 EXECUTION_LOG                  TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9B1E9211C3721E3E5',
 LOCK                           TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC8207416C1CB',
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741681CB',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741781CB',
      END OF ROOT,
      BEGIN OF ROOT_LOCK,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741901CB',
      END OF ROOT_LOCK,
      BEGIN OF ROOT_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC8207418E1CB',
      END OF ROOT_MESSAGE,
      BEGIN OF ROOT_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '06789A0505941ED9AF9DC820741921CB',
      END OF ROOT_PROPERTY,
    END OF SC_ASSOCIATION .
  constants:
    BEGIN OF SC_ASSOCIATION_ATTRIBUTE,
      BEGIN OF BLOCK,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF ROOT,
    END OF SC_ASSOCIATION_ATTRIBUTE .
  constants:
    SC_BO_KEY  TYPE /BOBF/OBM_BO_KEY VALUE '06789A0505941ED9AF9D7DED3F09814C' .
  constants:
    SC_BO_NAME TYPE /BOBF/OBM_NAME VALUE 'ZBC_BO_BLOCKCHAIN' .
  constants:
    BEGIN OF SC_DETERMINATION,
      BEGIN OF BLOCK,
 CALCULATE_DIFF_POW             TYPE /BOBF/DET_KEY VALUE '06789A0505941ED9B0D1EF4D75E65473',
 NEW_BLOCK                      TYPE /BOBF/DET_KEY VALUE '06789A0505941ED9AFA11B81A6BD86E3',
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
 NEW_EXECUTION                  TYPE /BOBF/DET_KEY VALUE '06789A0505941ED9B1E93B162151440D',
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
 GET_TEXT                       TYPE /BOBF/DET_KEY VALUE '06789A0505941EE9B0E7DAC5D75EF6E1',
 NEW_HEADER                     TYPE /BOBF/DET_KEY VALUE '06789A0505941ED9AFA0EA703551E686',
 UPDATE_HEADER                  TYPE /BOBF/DET_KEY VALUE '06789A0505941ED9AFD1B9EEADCB0EB6',
      END OF ROOT,
    END OF SC_DETERMINATION .
  constants:
    SC_MODEL_VERSION TYPE /BOBF/CONF_VERSION VALUE '00000' .
  constants:
    BEGIN OF SC_NODE,
 BLOCK                          TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC820741981CB',
 BLOCK_MESSAGE                  TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC820741A41CB',
 BLOCK_PROPERTY                 TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC820741A81CB',
 EXECUTION_LOG                  TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9B1E9211C372103E5',
 EXECUTION_LOG_MESSAGE          TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9B1E9211C372223E5',
 EXECUTION_LOG_PROPERTY         TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9B1E9211C372263E5',
 ROOT                           TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC820741621CB',
 ROOT_LOCK                      TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC8207416A1CB',
 ROOT_MESSAGE                   TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC820741661CB',
 ROOT_PROPERTY                  TYPE /BOBF/OBM_NODE_KEY VALUE '06789A0505941ED9AF9DC820741761CB',
    END OF SC_NODE .
  constants:
    BEGIN OF SC_NODE_ATTRIBUTE,
      BEGIN OF BLOCK,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  ID_BLOCK                       TYPE STRING VALUE 'ID_BLOCK',
  INDEX_BLOCK                    TYPE STRING VALUE 'INDEX_BLOCK',
  PREV_HASH                      TYPE STRING VALUE 'PREV_HASH',
  TIMESTAMP                      TYPE STRING VALUE 'TIMESTAMP',
  NONCE                          TYPE STRING VALUE 'NONCE',
  DATA                           TYPE STRING VALUE 'DATA',
  ID_REQUEST                     TYPE STRING VALUE 'ID_REQUEST',
  HASH                           TYPE STRING VALUE 'HASH',
  POW_INIT_DATE                  TYPE STRING VALUE 'POW_INIT_DATE',
  POW_INIT_TIME                  TYPE STRING VALUE 'POW_INIT_TIME',
  POW_END_DATE                   TYPE STRING VALUE 'POW_END_DATE',
  POW_END_TIME                   TYPE STRING VALUE 'POW_END_TIME',
  TRANSIENT_NODE_DATA            TYPE STRING VALUE 'TRANSIENT_NODE_DATA',
  POW_DIFF_SECONDS               TYPE STRING VALUE 'POW_DIFF_SECONDS',
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  ID_BLOCK                       TYPE STRING VALUE 'ID_BLOCK',
  ERDAT                          TYPE STRING VALUE 'ERDAT',
  ERZET                          TYPE STRING VALUE 'ERZET',
  ERNAM                          TYPE STRING VALUE 'ERNAM',
  JOBNAME                        TYPE STRING VALUE 'JOBNAME',
  JOBCOUNT                       TYPE STRING VALUE 'JOBCOUNT',
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  ID_BLOCK                       TYPE STRING VALUE 'ID_BLOCK',
  STATUS                         TYPE STRING VALUE 'STATUS',
  ERDAT                          TYPE STRING VALUE 'ERDAT',
  ERZET                          TYPE STRING VALUE 'ERZET',
  ERNAM                          TYPE STRING VALUE 'ERNAM',
  AEDAT                          TYPE STRING VALUE 'AEDAT',
  AETIME                         TYPE STRING VALUE 'AETIME',
  AUNAME                         TYPE STRING VALUE 'AUNAME',
  TRANSIENT_NODE_DATA            TYPE STRING VALUE 'TRANSIENT_NODE_DATA',
  STATUS_TEXT                    TYPE STRING VALUE 'STATUS_TEXT',
      END OF ROOT,
    END OF SC_NODE_ATTRIBUTE .
  constants:
    BEGIN OF SC_NODE_CATEGORY,
      BEGIN OF BLOCK,
 BLOCK                          TYPE /BOBF/OBM_NODE_CAT_KEY VALUE '06789A0505941ED9AF9DC8207419E1CB',
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
 EXECUTION_LOG                  TYPE /BOBF/OBM_NODE_CAT_KEY VALUE '06789A0505941ED9B1E9211C3721C3E5',
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
 ROOT                           TYPE /BOBF/OBM_NODE_CAT_KEY VALUE '06789A0505941ED9AF9DC820741641CB',
      END OF ROOT,
    END OF SC_NODE_CATEGORY .
  constants:
    BEGIN OF SC_QUERY,
      BEGIN OF BLOCK,
 SELECT_ALL                     TYPE /BOBF/OBM_QUERY_KEY VALUE '06789A0505941ED9AF9DC8207419A1CB',
 SELECT_BY_ELEMENTS             TYPE /BOBF/OBM_QUERY_KEY VALUE '06789A0505941ED9AF9DC8207419C1CB',
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
 SELECT_ALL                     TYPE /BOBF/OBM_QUERY_KEY VALUE '06789A0505941ED9B1E9211C372123E5',
 SELECT_BY_ELEMENTS             TYPE /BOBF/OBM_QUERY_KEY VALUE '06789A0505941ED9B1E9211C372143E5',
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
 SELECT_ALL                     TYPE /BOBF/OBM_QUERY_KEY VALUE '06789A0505941ED9AF9DC820741941CB',
 SELECT_BY_ELEMENTS             TYPE /BOBF/OBM_QUERY_KEY VALUE '06789A0505941ED9AF9DC820741961CB',
      END OF ROOT,
    END OF SC_QUERY .
  constants:
    BEGIN OF SC_QUERY_ATTRIBUTE,
      BEGIN OF BLOCK,
        BEGIN OF SELECT_BY_ELEMENTS,
 KEY                            TYPE STRING VALUE 'KEY',
 PARENT_KEY                     TYPE STRING VALUE 'PARENT_KEY',
 ROOT_KEY                       TYPE STRING VALUE 'ROOT_KEY',
 NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
 ID_BLOCK                       TYPE STRING VALUE 'ID_BLOCK',
 INDEX_BLOCK                    TYPE STRING VALUE 'INDEX_BLOCK',
 PREV_HASH                      TYPE STRING VALUE 'PREV_HASH',
 TIMESTAMP                      TYPE STRING VALUE 'TIMESTAMP',
 NONCE                          TYPE STRING VALUE 'NONCE',
 DATA                           TYPE STRING VALUE 'DATA',
 ID_REQUEST                     TYPE STRING VALUE 'ID_REQUEST',
 HASH                           TYPE STRING VALUE 'HASH',
 POW_INIT_DATE                  TYPE STRING VALUE 'POW_INIT_DATE',
 POW_INIT_TIME                  TYPE STRING VALUE 'POW_INIT_TIME',
 POW_END_DATE                   TYPE STRING VALUE 'POW_END_DATE',
 POW_END_TIME                   TYPE STRING VALUE 'POW_END_TIME',
 TRANSIENT_NODE_DATA            TYPE STRING VALUE 'TRANSIENT_NODE_DATA',
 POW_DIFF_SECONDS               TYPE STRING VALUE 'POW_DIFF_SECONDS',
        END OF SELECT_BY_ELEMENTS,
      END OF BLOCK,
      BEGIN OF EXECUTION_LOG,
        BEGIN OF SELECT_BY_ELEMENTS,
 KEY                            TYPE STRING VALUE 'KEY',
 PARENT_KEY                     TYPE STRING VALUE 'PARENT_KEY',
 ROOT_KEY                       TYPE STRING VALUE 'ROOT_KEY',
 NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
 ID_BLOCK                       TYPE STRING VALUE 'ID_BLOCK',
 ERDAT                          TYPE STRING VALUE 'ERDAT',
 ERZET                          TYPE STRING VALUE 'ERZET',
 ERNAM                          TYPE STRING VALUE 'ERNAM',
 JOBNAME                        TYPE STRING VALUE 'JOBNAME',
 JOBCOUNT                       TYPE STRING VALUE 'JOBCOUNT',
        END OF SELECT_BY_ELEMENTS,
      END OF EXECUTION_LOG,
      BEGIN OF ROOT,
        BEGIN OF SELECT_BY_ELEMENTS,
 KEY                            TYPE STRING VALUE 'KEY',
 PARENT_KEY                     TYPE STRING VALUE 'PARENT_KEY',
 ROOT_KEY                       TYPE STRING VALUE 'ROOT_KEY',
 NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
 ID_BLOCK                       TYPE STRING VALUE 'ID_BLOCK',
 STATUS                         TYPE STRING VALUE 'STATUS',
 ERDAT                          TYPE STRING VALUE 'ERDAT',
 ERZET                          TYPE STRING VALUE 'ERZET',
 ERNAM                          TYPE STRING VALUE 'ERNAM',
 AEDAT                          TYPE STRING VALUE 'AEDAT',
 AETIME                         TYPE STRING VALUE 'AETIME',
 AUNAME                         TYPE STRING VALUE 'AUNAME',
 TRANSIENT_NODE_DATA            TYPE STRING VALUE 'TRANSIENT_NODE_DATA',
 STATUS_TEXT                    TYPE STRING VALUE 'STATUS_TEXT',
        END OF SELECT_BY_ELEMENTS,
      END OF ROOT,
    END OF SC_QUERY_ATTRIBUTE .
  constants:
    BEGIN OF SC_VALIDATION,
      BEGIN OF BLOCK,
 ALTKEY_UNIQUENESS_CHECK        TYPE /BOBF/VAL_KEY VALUE '06789A0505941ED9AF9DC820741E41CB',
      END OF BLOCK,
      BEGIN OF ROOT,
 ALTKEY_UNIQUENESS_CHECK        TYPE /BOBF/VAL_KEY VALUE '06789A0505941ED9AF9DC820741CA1CB',
      END OF ROOT,
    END OF SC_VALIDATION .
endinterface.