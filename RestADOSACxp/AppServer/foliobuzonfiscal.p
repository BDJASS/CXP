@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : foliobuzonfiscal.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Sat Aug 09 16:37:27 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttEdocRecibo
    FIELD NomReceptor AS CHARACTER 
    FIELD Fecha       AS DATE
    FIELD TotalFac    AS DECIMAL
    FIELD UUID        AS CHARACTER
    FIELD Serie       AS CHARACTER
    FIELD Folio       AS INTEGER.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetBuzonFiscal:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-FechaIni AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER l-FechaFin AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEdocRecibo.


    EMPTY TEMP-TABLE ttEdocRecibo.
    FOR EACH eDocRecibo WHERE eDocRecibo.Fecha >= l-FechaIni
        AND eDocRecibo.Fecha <= l-FechaFin
        AND eDocRecibo.Folio = 0
        NO-LOCK:
        CREATE ttEdocRecibo.
        ASSIGN 
            ttEdocRecibo.NomReceptor = eDocRecibo.Nom_Receptor
            ttEdocRecibo.Fecha       = eDocRecibo.Fecha
            ttEdocRecibo.TotalFac    = eDocRecibo.Total
            ttEdocRecibo.UUID        = eDocRecibo.UUID
            ttEdocRecibo.Serie       = eDocRecibo.Serie
            ttEdocRecibo.Folio       = eDocRecibo.Folio.
    END.
    RETURN.
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostBuzonFiscal:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pUUID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pSerie AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pFolio AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oMensaje AS CHARACTER NO-UNDO.

    FIND FIRST eDocRecibo WHERE eDocRecibo.UUID = pUUID EXCLUSIVE-LOCK NO-ERROR.
       
    IF AVAILABLE eDocRecibo THEN 
    DO:
        ASSIGN 
            eDocRecibo.Serie = pSerie
            eDocRecibo.Folio = pFolio.
        RELEASE eDocRecibo.
       
        ASSIGN 
            oMensaje = "OK".
    END.
    ELSE 
    DO:   
        ASSIGN 
            oMensaje = "Este UUID no existe en los registros".
    END.
        
    RETURN.
END PROCEDURE.

