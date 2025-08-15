@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : tipocambio.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Wed Aug 06 16:31:30 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
DEFINE TEMP-TABLE ttTipoCambio
    FIELD IdMoneda  AS INTEGER 
    FIELD DescrMoneda AS CHARACTER
    FIELD Importe AS DECIMAL
    FIELD Promedio AS DECIMAL
    FIELD Adquirido AS DECIMAL
    FIELD Ponderado AS DECIMAL
    FIELD Fecha AS DATE.

DEFINE TEMP-TABLE ttDetTipoCambio
    FIELD IdMoneda        AS INTEGER
    FIELD Fecha      AS DATE    
    FIELD Importe    AS DECIMAL.
    
    
    DEFINE DATASET dsTipoCambio FOR 
    ttTipoCambio,
    ttDetTipoCambio
    DATA-RELATION TiposCambio FOR ttTipoCambio, ttDetTipoCambio
    RELATION-FIELDS (IdMoneda, IdMoneda).

DEFINE VARIABLE l-TCAnter    LIKE TCReal.Importe NO-UNDO.

DEFINE VARIABLE l-monedaTC AS CHARACTER.
DEFINE VARIABLE l-Fecha AS DATE NO-UNDO.
DEF VAR l-Prom1 LIKE TCReal.Importe NO-UNDO.
DEF VAR l-Cont1 AS INTEGER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE l-Suma AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-Contador AS INTEGER NO-UNDO.
DEFINE VARIABLE l-MontoAdquirido AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-TotalPonderado AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-TCReciente AS DECIMAL NO-UNDO.
DEFINE VARIABLE l-FechaReciente AS DATE NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetTipoCambio:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER l-FechaIni AS DATE NO-UNDO.
DEFINE INPUT PARAMETER l-FechaFin AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsTipoCambio.
DEFINE OUTPUT PARAMETER oError AS CHARACTER NO-UNDO.


FIND adosa.URL WHERE adosa.URL.Parametro = "MonedasTC" NO-LOCK NO-ERROR.
IF AVAILABLE adosa.URL THEN ASSIGN l-monedaTC = adosa.URL.Valor.
ELSE 
    DO:
        ASSIGN 
            oError = "No existe el parametro MonedasTipoCambio, favor de hablar a sistemas".
        UNDO, RETURN.
    END.

/* PROCESAMIENTO PRINCIPAL */
DO i = 1 TO NUM-ENTRIES(l-monedaTC):
    DEFINE VARIABLE cMoneda AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iMoneda AS INTEGER NO-UNDO.
    
    cMoneda = ENTRY(i, l-monedaTC).
    iMoneda = INTEGER(cMoneda) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    
    /* Reiniciamos acumuladores para cada moneda */
    ASSIGN 
        l-Suma = 0
        l-Contador = 0
        l-MontoAdquirido = 0
        l-TotalPonderado = 0
        l-TCReciente = 0
        l-FechaReciente = DATE(1, 1, 1).
    
    /* 1. Procesamos TCReal para obtener datos hist�ricos */
    DO l-Fecha = l-FechaIni TO l-FechaFin:
        FIND FIRST TCReal WHERE TCReal.Id-Moneda = iMoneda 
                      AND TCReal.FecReg = l-Fecha 
                      NO-LOCK NO-ERROR.
        
        IF AVAILABLE TCReal THEN DO:
            /* Agregamos registro al detalle */
            CREATE ttDetTipoCambio.
            ASSIGN 
                ttDetTipoCambio.IdMoneda = TCReal.Id-Moneda
                ttDetTipoCambio.Fecha = TCReal.FecReg
                ttDetTipoCambio.Importe = TCReal.Importe.
            
            /* Acumulamos para el promedio */
            ASSIGN 
                l-Suma = l-Suma + TCReal.Importe
                l-Contador = l-Contador + 1.
            
            /* Obtenemos el tipo de cambio m�s reciente */
            IF TCReal.FecReg > l-FechaReciente THEN
                ASSIGN 
                    l-FechaReciente = TCReal.FecReg
                    l-TCReciente = TCReal.Importe.
        END.
    END.
    
    /* 2. Calculamos montos adquiridos y ponderados (cheques) */
    FOR EACH Cheque WHERE Cheque.Estatus = 9
                     AND Cheque.FecEst >= l-FechaIni
                     AND Cheque.FecEst <= l-FechaFin
                     AND Cheque.Tipo = 1
                     NO-LOCK,
       EACH DetCheque WHERE DetCheque.Id-Banco = Cheque.Id-Banco
                        AND DetCheque.Id-CtaCheq = Cheque.Id-CtaCheq
                        AND DetCheque.NumCheque = Cheque.NumCheque
                        NO-LOCK,
       FIRST FP WHERE FP.Id-FP = DetCheque.Refer NO-LOCK:
       IF iMoneda = 3 THEN DO:       
            IF FP.Id-Moneda = 3 OR FP.Id-Moneda = 5 THEN
                ASSIGN 
                   l-MontoAdquirido = l-MontoAdquirido + DetCheque.ImpPagado
                   l-TotalPonderado = l-TotalPonderado + (DetCheque.ImpPagado * DetCheque.TC).
       END.
       ELSE DO:           
           IF FP.Id-Moneda = 7 THEN
               ASSIGN 
                   l-MontoAdquirido = l-MontoAdquirido + DetCheque.ImpPagado
                   l-TotalPonderado = l-TotalPonderado + (DetCheque.ImpPagado * DetCheque.TC).           
       END.
    END.
    
    /* 3. Obtenemos descripci�n de la moneda (opcional) */
    FIND FIRST Moneda WHERE Moneda.Id-Moneda = iMoneda NO-LOCK NO-ERROR.
    
    /* 4. Creamos registro resumen en ttTipoCambio */
    CREATE ttTipoCambio.
    ASSIGN 
        ttTipoCambio.IdMoneda = iMoneda
        ttTipoCambio.DescrMoneda = (IF AVAILABLE Moneda THEN Moneda.Nombre ELSE "MONEDA " + STRING(iMoneda))
        ttTipoCambio.Importe = l-TCReciente  /* �ltimo TC registrado */
        ttTipoCambio.Promedio = (IF l-Contador > 0 THEN l-Suma / l-Contador ELSE 0)
        ttTipoCambio.Adquirido = l-MontoAdquirido
        ttTipoCambio.Ponderado = (IF l-MontoAdquirido > 0 THEN l-TotalPonderado / l-MontoAdquirido ELSE 0)
        ttTipoCambio.Fecha = l-FechaReciente. /* Fecha del �ltimo TC */
END.
RETURN.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostTipoCambio:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pMoneda AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER pFecReg AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER pImporte AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER pUser AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oMensaje AS CHARACTER NO-UNDO.


FIND Moneda WHERE Moneda.Id-Moneda = pMoneda NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE Moneda THEN 
    DO:
        ASSIGN oMensaje = "La moneda seleccionada no esta registrada".
        RETURN.
    END.
    
    IF WEEKDAY(pFecReg) = 1 THEN 
    DO:
        ASSIGN oMensaje = "No es posible registar T.C. en domingo".
        RETURN.
    END.
    
    IF pFecReg > TODAY THEN 
    DO:
        ASSIGN oMensaje = "No es posible registar T.C. de fechas futuras...".
        RETURN.
    END.
    
    IF pImporte = 0 THEN DO:
        ASSIGN oMensaje = "No es posible registar el T.C. en ceros....".
        RETURN.
    END.
    


    l-TCAnter = 0.
    FOR EACH TCReal WHERE TCReal.Id-Moneda = pMoneda AND TCReal.FecReg <= pFecReg NO-LOCK BY TCReal.FecReg DESCENDING:
        l-TCAnter = TCReal.Importe.
        LEAVE.
    END.
    
    FIND FIRST TCReal WHERE TCReal.Id-Moneda = pMoneda
                        AND TCReal.FecReg = pFecReg EXCLUSIVE-LOCK NO-ERROR.
                        
                        
    
    IF pImporte = 0 THEN DO:
        ASSIGN oMensaje = "No es posible registar el T.C. en ceros....".
        RETURN.
    END.
                
    IF pImporte >= l-TCAnter * 1.05 OR
        pImporte <= l-TCAnter * 0.95 THEN DO:
        ASSIGN oMensaje = "T.C. con demasiada variacion respecto al anterior, verificar... (Mensaje solo informativo, puede continuar)".
    END.


IF NOT AVAILABLE TCReal THEN DO:
        CREATE TCReal.
        ASSIGN 
            TCReal.Id-Moneda = pMoneda
            TCReal.FecReg    = pFecReg.
    END.

    ASSIGN 
        TCReal.FecCap  = TODAY
        TCReal.HorCap  = TIME
        TCReal.Id-User = pUser.
        TCReal.Importe = pImporte.
    RELEASE TCReal.

    IF pMoneda = 3 THEN DO:
        FIND FIRST TCReal WHERE TCReal.Id-Moneda = 5
                            AND TCReal.FecReg = pFecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE TCReal THEN DO:
            CREATE TCReal.
            ASSIGN 
                TCReal.Id-Moneda = 5
                TCReal.FecReg    = pFecReg.
        END.
        ASSIGN 
            TCReal.Importe = pImporte
            TCReal.FecCap  = TODAY
            TCReal.HorCap  = TIME
            TCReal.Id-User = pUser.
        RELEASE TCReal.
    END.
    ASSIGN oMensaje = "OK".
RETURN.    
END PROCEDURE.

