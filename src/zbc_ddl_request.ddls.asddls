@AbapCatalog.sqlViewName: 'ZBC_CDS_REQUEST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BC - Request'

-- Anotations for BOPF
@ObjectModel: {

    representativeKey: 'ID_REQUEST', -- Key for BOPF
    semanticKey: ['ID_REQUEST'], -- table key
    compositionRoot: true, -- Root node
    transactionalProcessingEnabled: true, -- Only for root node, Enables the transactional runtime support
    updateEnabled: true,
    deleteEnabled: true,
    createEnabled: true,
    writeActivePersistence: 'ZBC_T_0001' -- Table where the data is saved
}


define view ZBC_DDL_REQUEST
  as select from zbc_t_0001 as request
{

      @ObjectModel.mandatory: true
  key request.id_request,
      @ObjectModel.readOnly: true
      request.erdat,
      @ObjectModel.readOnly: true
      request.ernam,
      @ObjectModel.readOnly: true
      request.erzet,
      @ObjectModel.mandatory: true
      request.data

}
