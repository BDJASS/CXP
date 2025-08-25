@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cxp0006a.p
    Purpose     : Basado en tesa0120.p

    Syntax      :

    Description : HU06 AUTORIZACION DE GASTOS 
                  /AutorizacionGastos

    Author(s)   : sis10
    Created     : Thu Aug 21 11:30:57 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* Tabla principal (cabecera) */
DEFINE TEMP-TABLE ttAut NO-UNDO
    FIELD Folio        LIKE Cheque.NumCheque      
    FIELD IdBanco      LIKE Banco.Id-Banco         
    FIELD Banco        LIKE Banco.NomCto           
    FIELD Beneficiario LIKE Cheque.Benef          
    FIELD FecReg       LIKE Cheque.FecReg                    
    FIELD Importe      LIKE Cheque.Importe        
    FIELD Aut          LIKE Cheque.Negociable     
    FIELD B            AS CHARACTER 
    FIELD CtaCheque    LIKE Cheque.Id-CtaCheq
    FIELD Tipo         LIKE Cheque.Tipo
    FIELD Elaboro      LIKE Cheque.Iniciales
    FIELD Concepto     LIKE Cheque.Concepto 
    INDEX idxFolio IS UNIQUE PRIMARY Folio.

/* Tabla detalle */
DEFINE TEMP-TABLE ttDet NO-UNDO
    FIELD Folio   LIKE Cheque.NumCheque   
    FIELD Cuenta  LIKE DetGasto.Id-Cuenta      
    FIELD Sc      LIKE DetGasto.scta 
    FIELD Ssc     LIKE DetGasto.sscta
    FIELD Sssc    LIKE DetGasto.ssscta
    FIELD Descr   LIKE DetGasto.Descr
    FIELD Importe AS CHARACTER
    FIELD Iva     LIKE DetGasto.IVA
    FIELD CA      LIKE DetGasto.TipoMov.  
    
    
/* Tabla detalle */
DEFINE TEMP-TABLE ttAutDet NO-UNDO
    FIELD Folio          LIKE Cheque.NumCheque
    FIELD FormaPago      LIKE DetCheque.Refer 
    FIELD EntradaC       LIKE EntFP.Id-EC    
    FIELD Factura        LIKE DetCheque.NumFac       
    FIELD TotEntrada     AS DECIMAL FORMAT "zzzzz,zz9"         
    FIELD TotEntradaCPP  AS DECIMAL FORMAT "zzzzz,zz9"       
    FIELD NoSol          AS DECIMAL FORMAT "zzzzzz9"       
    FIELD OtroDesc       AS DECIMAL FORMAT "zzzz,zz9"             
    FIELD ImporteFactura AS DECIMAL FORMAT "zzzzz,zz9"           
    FIELD PP             AS DECIMAL FORMAT "z9.99"       
    FIELD TotalPagar     AS DECIMAL FORMAT "zzzzz,zz9" 
    INDEX idxDetalle IS PRIMARY Folio Factura.
    
DEFINE TEMP-TABLE ttEntCompra NO-UNDO
    FIELD EntradaC        LIKE EntFP.Id-EC  
    FIELD TipoCambioDolar LIKE TipoCambio.Importe
    FIELD Codigo          LIKE DetEC.Id-Articulo       
    FIELD Descripcion     LIKE DetEC.Descr        
    FIELD Colorr          LIKE Kolor.Descr      
    FIELD CantPres        LIKE DetEC.CantPres       
    FIELD CantUM          LIKE DetEC.CantUMI           
    FIELD PrecioUnit      AS DECIMAL FORMAT "zz,zz9.999999"         
    FIELD Importe         AS DECIMAL FORMAT "zz,zz9.999999"  .  

DEFINE DATASET dsAut
    FOR ttAut, ttDet
    DATA-RELATION drAut FOR ttAut, ttDet
    RELATION-FIELDS(Folio, Folio)
    NESTED.   
    

DEFINE            VARIABLE      l-Benef     AS CHARACTER    NO-UNDO FORMAT 'x(50)'.
DEFINE            VARIABLE      l-BusBenef  AS CHARACTER    NO-UNDO FORMAT 'x(50)'.
DEFINE            VARIABLE      l-Usuario   LIKE Usuario.Id-User NO-UNDO.
DEFINE            VARIABLE      l-TipoChe   LIKE Cheque.Tipo NO-UNDO.
DEFINE NEW SHARED VARIABLE      s-recid     AS RECID   NO-UNDO.
DEFINE NEW SHARED VARIABLE      s-Banco     LIKE Cheque.Id-Banco NO-UNDO.
DEFINE NEW SHARED VARIABLE      s-CtaCheq   LIKE Cheque.Id-CtaCheq NO-UNDO.
DEFINE NEW SHARED VARIABLE      s-NumCheque LIKE Cheque.NumCheque NO-UNDO.
DEFINE         VARIABLE l-base      AS DATE    NO-UNDO.
DEFINE         VARIABLE l-Buzon     AS CHARACTER    NO-UNDO.
DEFINE         VARIABLE l-rfc       AS CHARACTER    NO-UNDO.
DEFINE         VARIABLE l-NumFac    AS CHARACTER    NO-UNDO.
DEFINE NEW SHARED VARIABLE      l-Banamex   AS DECIMAL FORMAT "$zz,zzz,zz9.99" LABEL "Banamex" NO-UNDO.
DEFINE NEW SHARED VARIABLE      l-Santander AS DECIMAL FORMAT "$zz,zzz,zz9.99" LABEL "Santander" NO-UNDO.

DEFINE            VARIABLE      l-SegCambio AS LOGICAL NO-UNDO.
DEFINE         VARIABLE l-renumber  AS INTEGER NO-UNDO.
DEFINE         VARIABLE l-i         AS INTEGER NO-UNDO.
DEFINE         VARIABLE l-RenAct    AS INTEGER NO-UNDO.
DEFINE         VARIABLE l-RenSig    AS INTEGER NO-UNDO.
DEFINE         VARIABLE l-RenInc    AS INTEGER NO-UNDO.
DEFINE         VARIABLE l-RenRec    AS INTEGER NO-UNDO.
DEFINE         VARIABLE l-RenInd    AS INTEGER NO-UNDO.
DEFINE            VARIABLE      l-Concepto  AS CHARACTER    FORMAT "x(50)" NO-UNDO EXTENT 2.
DEFINE            VARIABLE      l-entro     AS LOGICAL NO-UNDO.
DEFINE            VARIABLE      l-Total     AS DECIMAL    FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE      l-TotIVA    LIKE l-Total NO-UNDO.
DEFINE            VARIABLE      l-TotIVA2   LIKE l-Total NO-UNDO.
DEFINE            VARIABLE      l-TotIVA3   LIKE l-Total NO-UNDO.
DEFINE            VARIABLE      l-ImpCheq   LIKE Cheque.Importe NO-UNDO.
DEFINE            VARIABLE      l-SeqDet    AS INTEGER     NO-UNDO.
DEFINE            VARIABLE      l-TotCA     AS DECIMAL    NO-UNDO FORMAT "-Z,ZZZ,ZZ9.99" .
DEFINE            VARIABLE      l-ImpCA     AS CHARACTER    FORMAT "x(15)" NO-UNDO.
   

DEFINE BUFFER bf-Cheque    FOR Cheque.   
DEFINE BUFFER b-DetGasto FOR DetGasto.
DEFINE BUFFER b-Cheque   FOR Cheque.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetTransferenciaGastos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  IdUsuario AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsAut.
    
    
    /* Inicia log */
    LOG-MANAGER:WRITE-MESSAGE("/AutorizacionGastos[GET] Ejecutado por usuario: " + IdUsuario). 
    FIND Usuario WHERE Usuario.Id-User = IdUsuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "Usuario Incorrecto "
            IdError   = TRUE.
        LOG-MANAGER:WRITE-MESSAGE("ERROR: Usuario no encontrado: " + IdUsuario).
        RETURN.
    END.
    l-Usuario = Usuario.Id-User.

    IF NOT CAN-DO("sge,ogg,franc,dgag,gee,ALEX",l-Usuario) THEN 
    DO:
        ASSIGN 
            Respuesta = "Usuario No Autorizado Para Esta Opcion"
            IdError   = TRUE.
        LOG-MANAGER:WRITE-MESSAGE("ERROR: Usuario no autorizado: " + l-Usuario).
        RETURN.
    END.
    
    FOR EACH Cheque WHERE Cheque.FecReg >= 01/01/2014 AND (Cheque.Id-Banco = 1 OR Cheque.Id-Banco = 25) 
        AND Cheque.NumCheque >= 900000
        AND Cheque.Tipo = 2
        AND Cheque.Estatus = 0    
        NO-LOCK:
        FIND b-Cheque WHERE RECID(b-Cheque) = RECID(Cheque) EXCLUSIVE-LOCK NO-WAIT.
        IF AVAILABLE b-Cheque THEN 
        DO:
            FOR EACH DetGasto WHERE DetGasto.Id-Banco = b-Cheque.Id-Banco
                AND DetGasto.Id-CtaCheq = b-Cheque.Id-CtaCheq
                AND DetGasto.NumCheque = b-Cheque.NumCheque
                EXCLUSIVE-LOCK:
                DELETE DetGasto.
            END.
            DELETE b-Cheque.
        END.
        RELEASE b-Cheque.
    END.
    FOR EACH Cheque WHERE Cheque.FecReg >= 01/01/2014 AND (Cheque.Id-Banco = 1 OR Cheque.Id-Banco = 25) 
        AND Cheque.NumCheque >= 200000 
        AND Cheque.NumCheque < 900000 
        AND Cheque.Tipo = 2
        AND Cheque.Estatus = 0   
        NO-LOCK:
        FIND b-Cheque WHERE RECID(b-Cheque) = RECID(Cheque)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE b-Cheque THEN 
        DO:
            ASSIGN 
                b-Cheque.Negociable = FALSE.
   
            IF b-Cheque.Negociable = TRUE THEN 
            DO:
                IF b-Cheque.Id-Banco = 1 THEN
                    l-Banamex = l-Banamex + b-Cheque.Importe.
                ELSE
                    l-Santander = l-Santander + b-Cheque.Importe.
            END. 
    
        END.
        RELEASE b-Cheque.
    END.
    
    LOG-MANAGER:WRITE-MESSAGE(">> Procesando Cheques para generar dsAut"). 
    FOR EACH Cheque WHERE Cheque.FecReg >= 01/01/2014 AND (Cheque.Id-Banco = 1 OR Cheque.Id-Banco = 25) 
        AND Cheque.NumCheque >= 200000 
        AND Cheque.NumCheque < 900000 
        AND Cheque.Tipo = 2   
        AND Cheque.Estatus = 0   
        NO-LOCK:
       
        FIND Banco WHERE Banco.Id-Banco = Cheque.Id-Banco NO-LOCK NO-ERROR.
       
        RUN Fecha_Vence.   
        CREATE ttAut.
        ASSIGN
            ttAut.Folio        = Cheque.NumCheque
            ttAut.IdBanco      = Banco.Id-Banco
            ttAut.Banco        = Banco.NomCto
            ttAut.Beneficiario = Cheque.Benef
            ttAut.FecReg       = Cheque.FecReg
            ttAut.Importe      = Cheque.Importe 
            ttAut.Aut          = Cheque.Negociable 
            ttAut.B            = l-Buzon
            ttAut.CtaCheque    = Cheque.Id-CtaCheq
            ttAut.Tipo         = Cheque.Tipo
            ttAut.Elaboro      = Cheque.Iniciales  
            ttAut.Concepto     = Cheque.Concepto + " " + Cheque.Concepto2 .   
            
            
        FOR EACH DetGasto WHERE DetGasto.Id-CtaCheq = Cheque.Id-CtaCheq 
            AND DetGasto.Id-Banco   = Cheque.Id-Banco   
            AND DetGasto.NumCheque  = Cheque.NumCheque NO-LOCK :
            
            ASSIGN 
                l-ImpCA = ''.
            IF DetGasto.TipoMov = 'C' THEN
                ASSIGN l-ImpCA = TRIM(STRING(DetGasto.PrecUnit,'>>>,>>>,>>9.99')) +
     FILL(' ', 15 - LENGTH(TRIM(STRING(DetGasto.PrecUnit,'>>>,>>>,>>9.99')))).
            ELSE IF DetGasto.TipoMov = 'A' THEN
                    ASSIGN l-ImpCA = FILL(' ', 15 -
               LENGTH(TRIM(STRING(DetGasto.PrecUnit,'>>>,>>>,>>9.99')))) +
               TRIM(STRING(DetGasto.PrecUnit,'>>>,>>>,>>9.99')).
            CREATE ttDet.    
            ASSIGN
                ttDet.Folio   = Cheque.NumCheque
                ttDet.Cuenta  = DetGasto.Id-Cuenta  
                ttDet.Sc      = DetGasto.scta
                ttDet.Ssc     = DetGasto.sscta
                ttDet.Sssc    = DetGasto.ssscta
                ttDet.Descr   = DetGasto.Descr
                ttDet.Importe = l-ImpCA
                ttDet.Iva     = DetGasto.IVA
                ttDet.CA      = DetGasto.TipoMov.  
            
            
              
        END. 
         
    END.      
          

RETURN.
END PROCEDURE.  

PROCEDURE fecha_vence:  
    
    l-Buzon = ''.
    l-rfc   = ''.
    FIND FIRST ProvGastos WHERE ProvGastos.Nombre = Cheque.Benef 
        NO-LOCK NO-ERROR.          
    IF AVAILABLE ProvGastos THEN 
        l-rfc = CAPS(TRIM(REPLACE(ProvGastos.RFC,' ',''))).        
    ELSE 
    DO:
        FIND FIRST Prestserv WHERE PrestServ.Nombre = Cheque.Benef 
            NO-LOCK NO-ERROR.
        IF AVAILABLE PrestServ THEN 
            l-rfc = CAPS(TRIM(REPLACE(PrestServ.RFC,' ',''))).        
        ELSE 
        DO:
            FIND FIRST Prov WHERE Prov.nombre = Cheque.Benef NO-LOCK NO-ERROR.
            IF AVAILABLE Prov THEN 
                l-rfc = CAPS(TRIM(REPLACE(Prov.RFC,' ',''))).        
        END.
    END.
    l-rfc = CAPS(TRIM(REPLACE(l-rfc,'-',''))).        
    l-NumFac = CAPS(TRIM(REPLACE(Cheque.Refer,' ',''))).        
    FIND FIRST eDocRecibo WHERE eDocRecibo.Receptor BEGINS l-rfc AND
        TRIM(eDocRecibo.Serie) + TRIM(STRING(eDocRecibo.Folio)) = l-NumFac
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE eDocRecibo THEN l-Buzon = 'N'.
END PROCEDURE.   

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DarAutorizacion:   
    DEFINE INPUT  PARAMETER TABLE FOR ttAut.
    DEFINE OUTPUT PARAMETER IdError   AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER NO-UNDO.
    
    
    LOG-MANAGER:WRITE-MESSAGE(" /AutorizacionDeGastos >> Autorizar").
    /* Validar que existan registros */
    IF NOT CAN-FIND(FIRST ttAut) THEN 
    DO:
        ASSIGN 
            IdError   = TRUE
            Respuesta = "No se recibieron datos en ttAut.".
        RETURN.
    END.    
    DO TRANSACTION:
      

        /* Validar que los 3 campos obligatorios existan en cada registro */
        FOR EACH ttAut:
            IF ttAut.Folio   = 0 
                OR ttAut.FecReg  = ? 
                OR ttAut.Importe = 0 THEN 
            DO:
                ASSIGN 
                    IdError   = TRUE
                    Respuesta = "Campos obligatorios faltantes: Folio, FecReg o Importe.".
                RETURN.
            END.
        END.

        /* Si pasa las validaciones, ejecutar lógica normal */
        FOR EACH ttAut:
            FOR EACH Cheque WHERE Cheque.FecReg     = ttAut.FecReg  
                AND Cheque.NumCheque  = ttAut.Folio
                AND Cheque.Tipo       = 2
                AND Cheque.Estatus    = 0
                AND Cheque.Negociable = FALSE
                AND Cheque.Importe    = ttAut.Importe
                NO-LOCK:
                FIND bf-Cheque WHERE RECID(bf-Cheque) = RECID(Cheque)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bf-Cheque THEN 
                DO:
                    ASSIGN 
                        bf-Cheque.Estatus    = 4
                        bf-Cheque.FecEst     = TODAY
                        bf-Cheque.Negociable = TRUE.  
                END.
                RELEASE bf-Cheque.
            END.
        END.

        /* Proceso OK */
        ASSIGN 
            IdError   = FALSE
            Respuesta = "Autorización ejecutada correctamente.".
    END.
  
    RETURN.       
END PROCEDURE.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeshacerAutorizacion: 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE INPUT PARAMETER iConfirmar  AS LOGICAL.
    
    IF iConfirmar = ? THEN iConfirmar = FALSE.
    
    LOG-MANAGER:WRITE-MESSAGE(" /AutorizacionDeGastos >> Deshacer").
    FIND FIRST Cheque WHERE Cheque.Estatus = 4
        AND Cheque.Tipo = 2
        AND Cheque.FecEst = TODAY
        AND Cheque.NumCheque >= 200000
        NO-LOCK NO-ERROR.
    IF AVAILABLE Cheque THEN 
    DO:
        
        IF iConfirmar = FALSE THEN 
        DO:
            
            ASSIGN 
                IdError   = TRUE
                Respuesta = "Confirma en deshacer las Operaciones Autorizadas ?".
            RETURN.  
            
        END.  
        DO TRANSACTION:
            FOR EACH Cheque WHERE Cheque.FecReg >= 01/01/2012 
                AND Cheque.NumCheque >= 200000 
                AND Cheque.Tipo = 2
                AND Cheque.Estatus = 4
                AND Cheque.Negociable = TRUE
                AND Cheque.FecEst = TODAY
                NO-LOCK:
                FIND bf-Cheque WHERE RECID(bf-Cheque) = RECID(Cheque)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bf-Cheque THEN 
                DO:
                    ASSIGN 
                        bf-Cheque.Estatus    = 0
                        bf-Cheque.FecEst     = ?
                        bf-Cheque.Negociable = NO.
                END.
                RELEASE bf-Cheque.
            END.
        END.

   
        /* Proceso OK */
        ASSIGN 
            IdError   = FALSE
            Respuesta = "Proceso ejecutado correctamente.".  
    END.
    ELSE 
    DO: 
        ASSIGN 
            Respuesta = "No existen operaciones autorizadas de hoy".
        IdError = TRUE.
    END.      
    
    RETURN.      
END PROCEDURE.