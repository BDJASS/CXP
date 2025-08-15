@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cxp0005c.p
    Purpose     : BASADO comb0010.p - comb0013.p
    URI         : /OrdenDeCompraPorEntrada
    Syntax      : 

    Description : Modulo CXP HU5.

    Author(s)   : sis10     
    Created     : 
    Notes       :   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


  
DEF VAR l-Archivo AS CHARACTER NO-UNDO.
DEF STREAM S-Salida.
DEF BUFFER b-EC    FOR EC.
DEF BUFFER b-DetEC FOR DetEC.

DEF TEMP-TABLE w-DetOC
    FIELD Id-Articulo LIKE DetOC.Id-Articulo
    FIELD Id-Color    LIKE DetOC.Id-Color
    FIELD Cancelado   LIKE DetOC.Cancelado
    FIELD CantRecUMI  LIKE DetOC.CantRecUMI
    FIELD CantPedUMI  LIKE DetOC.CantPedUMI
    INDEX Idx-Def Id-Articulo Id-Color.

DEF TEMP-TABLE ttOrdenCompraEntrada
    FIELD IdEC           LIKE EC.Id-EC
    FIELD FecReg         LIKE EC.FecReg
    FIELD IdUbic         LIKE EC.Id-Ubic
    FIELD NumFac         LIKE EC.NumFac
    FIELD IdArticulo    LIKE DetEC.Id-Articulo
    FIELD Descr          AS CHAR
    FIELD IdColor       LIKE DetEC.Id-Color
    FIELD Kolor          AS CHAR
    FIELD CantRec        LIKE DetEC.CantUMI
    FIELD CantAcum        LIKE DetEC.CantUMI
    FIELD CantidadPedida AS DECIMAL
    FIELD Exceso         LIKE DetEC.CantUMI
    INDEX Idx-Def FecReg      IdEC IdArticulo IdColor
    INDEX Idx-Art IdArticulo IdColor.

    DEFINE VARIABLE l-NOrden  AS INTEGER NO-UNDO.
/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE OrdenDeCompraPorEntrada:
    /*
     Programa : comb0019.i
     Funcion  : Manda la rutina para imprimir detalle de entradas de la o.c.
     Autor    : FLC
     Fecha    : 18 FEB 2011
   */

    /*
      Programa : comb0013.p
      Funcion  : Impresion de Detalle de Entradas de la O.C.
      Autor    : FLC
      Fecha    : 18 FEB 2011
    */

    DEFINE INPUT  PARAMETER l-OC LIKE OC.Id-OC NO-UNDO.
    DEFINE INPUT  PARAMETER IdUsuario AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError   AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttOrdenCompraEntrada.
 
 
    /* El Programa de Pantalla Negra acomoda por Entrada / Articulo 
     Se pondra de manera Default que se regresa por Entrada    */
  
    ASSIGN 
        l-NOrden = 1. /* Acomoda la temporal por Entrada */ 
    
    FIND FIRST Usuario WHERE Usuario.Id-User = IdUsuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN DO:
     ASSIGN 
              IdError = TRUE
              Respuesta = "No existe Usuario".
              RETURN.        
    END.
    FIND OC WHERE OC.Id-OC = l-OC NO-LOCK NO-ERROR.
    IF NOT AVAILABLE OC THEN DO:
      ASSIGN 
              IdError = TRUE
              Respuesta = "No existe OC".
              RETURN.   
    END.
    EMPTY TEMP-TABLE w-DetOC. 
    
        LOG-MANAGER:WRITE-MESSAGE("/OrdenDeCompraPorEntrada" + " " +  
                              " Procesando OC " + STRING(OC.Id-OC) +
                              " para usuario " + IdUsuario).
                                  
    FOR EACH DetOC WHERE DetOC.Id-OC = OC.Id-OC NO-LOCK:
        CREATE w-DetOC.
        ASSIGN 
            w-DetOC.Id-Articulo = DetOC.Id-Articulo
            w-DetOC.Id-Color    = DetOC.Id-Color
            w-DetOC.Cancelado   = DetOC.Cancelado
            w-DetOC.CantRecUMI  = 0
            w-DetOC.CantPedUMI  = DetOC.CantPedUMI.
    END.
    FOR EACH b-EC WHERE b-EC.Id-OC = OC.Id-OC NO-LOCK BY b-EC.FecReg BY b-EC.Id-EC:
      
        FOR EACH b-DetEC WHERE b-DetEC.Id-EC = b-EC.Id-EC NO-LOCK:
            IF b-DetEC.CantUMI - b-DetEC.CancUMI <= 0 THEN NEXT.
            CREATE ttOrdenCompraEntrada.
            ASSIGN 
                ttOrdenCompraEntrada.IdEC       = b-EC.Id-EC
                ttOrdenCompraEntrada.FecReg     = b-EC.FecReg
                ttOrdenCompraEntrada.IdUbic     = b-EC.Id-Ubic
                ttOrdenCompraEntrada.NumFac     = b-EC.NumFac
                ttOrdenCompraEntrada.IdArticulo = b-DetEC.Id-Articulo
                ttOrdenCompraEntrada.IdColor    = b-DetEC.Id-Color
                ttOrdenCompraEntrada.CantRec    = b-DetEC.CantUMI - b-DetEC.CancUMI.
          
            FIND FIRST w-DetOC WHERE w-DetOC.Id-Articulo = b-DetEC.Id-Articulo
                AND w-DetOC.Id-Color    = b-DetEC.Id-Color
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE w-DetOC THEN 
            DO:
                CREATE w-DetOC.
                ASSIGN 
                    w-DetOC.Id-Articulo = b-DetEC.Id-Articulo
                    w-DetOC.Id-Color    = b-DetEC.Id-Color
                    w-DetOC.Cancelado   = FALSE
                    w-DetOC.CantRecUMI  = 0
                    w-DetOC.CantPedUMI  = 0.
            END.
                               
            ASSIGN 
                w-DetOC.CantRecUMI = w-DetOC.CantRecUMI + (b-DetEC.CantUMI - b-DetEC.CancUMI).
            ASSIGN 
                ttOrdenCompraEntrada.CantAcum = w-DetOC.CantRecUMI
                ttOrdenCompraEntrada.Exceso  = w-DetOC.CantRecUMI - w-DetOC.CantPedUMI.
            IF ttOrdenCompraEntrada.Exceso < 0 THEN ASSIGN ttOrdenCompraEntrada.Exceso = 0.
            RELEASE w-DetOC. 
        END.
    END.

    MESSAGE 'Procesando reporte. Espere un momento ...'.
  
    IF l-NOrden = 1 THEN 
    DO:
        FOR EACH ttOrdenCompraEntrada NO-LOCK BREAK BY ttOrdenCompraEntrada.FecReg
            BY ttOrdenCompraEntrada.IdEC
            BY ttOrdenCompraEntrada.IdArticulo
            BY ttOrdenCompraEntrada.IdColor:
            RUN ImpLinea.
        END.
    END.
    ELSE 
    DO:
        FOR EACH ttOrdenCompraEntrada NO-LOCK BREAK BY ttOrdenCompraEntrada.IdArticulo
            BY ttOrdenCompraEntrada.IdColor
            BY ttOrdenCompraEntrada.FecReg
            BY ttOrdenCompraEntrada.IdEC:
            RUN ImpLinea.
        END.
    END.

    LOG-MANAGER:WRITE-MESSAGE("Proceso finalizado para OC " + STRING(OC.Id-OC)).
    RETURN.       
END PROCEDURE.

PROCEDURE ImpLinea.
    FIND Articulo WHERE Articulo.Id-Articulo = ttOrdenCompraEntrada.IdArticulo  NO-LOCK NO-ERROR.
    FIND Kolor WHERE Kolor.Id-Color = ttOrdenCompraEntrada.IdColor NO-LOCK NO-ERROR.
    FIND FIRST w-DetOC WHERE w-DetOC.Id-Art = ttOrdenCompraEntrada.IdArt
        AND w-DetOC.Id-Color = ttOrdenCompraEntrada.IdColor
        NO-LOCK NO-ERROR.
          
    ASSIGN
        ttOrdenCompraEntrada.Descr          = Articulo.Descr 
        WHEN AVAILABLE Articulo
        ttOrdenCompraEntrada.Kolor          = Kolor.Descr    
        WHEN AVAILABLE Kolor
        ttOrdenCompraEntrada.CantidadPedida = w-DetOC.CantPedUMI 
        WHEN AVAILABLE w-DetOC .
END.   