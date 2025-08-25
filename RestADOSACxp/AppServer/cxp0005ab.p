@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cxp0005b.p
    Purpose     : BASADO cxpc0040.p
    URI         : /FormaDePago
    Syntax      : Autorizacion de Pagos [Transferencias/Proveedores]

    Description : Modulo CXP HU5

    Author(s)   : sis10     
    Created     : 
    Notes       :   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */

/* Tabla principal (cabecera) */
DEFINE TEMP-TABLE ttFormaDePago NO-UNDO   
    FIELD Tipo           AS INTEGER
    FIELD OrdenDeCompra  AS CHARACTER      
    FIELD Fecha          AS DATE   
    FIELD Almacen        LIKE EC.Id-EC          
    FIELD EntradaAlmacen AS CHARACTER
    FIELD Versionn       LIKE FP.Version          
    FIELD Proveedor      AS CHARACTER        
    FIELD Factura        LIKE FP.NumFac                     
    FIELD FechaFactura   AS DATE               
    FIELD FechaEmbarque  LIKE FP.FecEmb 
    FIELD Bloque         AS CHARACTER .
    
    
DEFINE TEMP-TABLE ttImportes NO-UNDO
    FIELD ImpSiva  AS DECIMAL
    FIELD ImpRet   AS DECIMAL
    FIELD ImpDes   AS DECIMAL
    FIELD AntSiva  AS DECIMAL
    FIELD TotFact  AS DECIMAL
    FIELD TotFinal AS DECIMAL.
    
DEFINE TEMP-TABLE ttDescr NO-UNDO
    FIELD Tipo        AS CHARACTER
    FIELD Descripcion AS CHARACTER
    FIELD ImpCIva     AS DECIMAL
    FIELD ImpSIva     AS DECIMAL.
    
DEFINE TEMP-TABLE ttPorDias NO-UNDO
    FIELD Dias     AS INTEGER
    FIELD FecVence AS DATE
    FIELD Des1     AS CHARACTER
    FIELD Des2     AS CHARACTER
    FIELD Des3     AS CHARACTER
    FIELD Des4     AS CHARACTER
    FIELD DesSIva  AS DECIMAL
    FIELD ImpSIva  AS DECIMAL
    FIELD NetPag   AS DECIMAL.
    
    
    
DEFINE TEMP-TABLE ttPie NO-UNDO  
    FIELD Obs       AS CHARACTER
    FIELD Capturo   AS CHARACTER
    FIELD Verifico  AS CHARACTER
    FIELD TotEnt    AS CHARACTER       
    FIELD Comprador AS CHARACTER
    FIELD Fecha     AS DATE.
   

DEFINE DATASET dsForma
    FOR ttFormaDePago,ttDescr,ttImportes,ttPorDias,ttPie.           

DEFINE            VARIABLE l-entrada    AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE            VARIABLE l-ordenes    AS CHARACTER FORMAT "x(30)" NO-UNDO.

DEFINE            VARIABLE l-Mensaj     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-reporte    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-DD1        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-DD2        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-DD3        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-DD4        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-hecho      AS DATE      FORMAT 99/99/9999 NO-UNDO.
DEFINE            VARIABLE l-hecho1     AS DATE      FORMAT 99/99/9999 NO-UNDO.
DEFINE            VARIABLE l-usuario    LIKE Usuario.Id-User NO-UNDO.
DEFINE            VARIABLE l-CA1        AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE            VARIABLE l-CA2        AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE            VARIABLE l-CA3        AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE            VARIABLE l-CA4        AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE            VARIABLE l-Des1       AS CHARACTER FORMAT "x(38)" NO-UNDO.
DEFINE            VARIABLE l-Des2       AS CHARACTER FORMAT "x(38)" NO-UNDO.
DEFINE            VARIABLE l-Des3       AS CHARACTER FORMAT "x(38)" NO-UNDO.
DEFINE            VARIABLE l-Des4       AS CHARACTER FORMAT "x(38)" NO-UNDO.
DEFINE            VARIABLE l-Imp1       AS DECIMAL   FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp2       AS DECIMAL   FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp3       AS DECIMAL   FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp4       AS DECIMAL   FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp11      AS DECIMAL   FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp22      AS DECIMAL   FORMAT "-z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp33      AS DECIMAL   FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-Imp44      AS DECIMAL   FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-SinIVA     AS DECIMAL   FORMAT "-ZZZZZZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpDesc    AS DECIMAL   FORMAT "-ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-TotFact    AS DECIMAL   FORMAT "-ZZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpRet     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-tipocambio AS DECIMAL.
DEFINE            VARIABLE l-TotAntFact LIKE l-TotFact NO-UNDO.
DEFINE            VARIABLE l-precunit   AS DECIMAL.
DEFINE            VARIABLE l-Difunit    AS DECIMAL.

DEFINE            VARIABLE l-dias1      AS INTEGER   FORMAT "-Z9" NO-UNDO.
DEFINE            VARIABLE l-dias2      AS INTEGER   FORMAT "-Z9" NO-UNDO.
DEFINE            VARIABLE l-dias3      AS INTEGER   FORMAT "-Z9" NO-UNDO.
DEFINE            VARIABLE l-dias4      AS INTEGER   FORMAT "-Z9" NO-UNDO.
DEFINE            VARIABLE l-dias5      AS INTEGER   FORMAT "-Z9" NO-UNDO.
DEFINE            VARIABLE l-Fecha1     AS DATE      NO-UNDO.
DEFINE            VARIABLE l-Fecha2     AS DATE      NO-UNDO.
DEFINE            VARIABLE l-Fecha3     AS DATE      NO-UNDO.
DEFINE            VARIABLE l-Fecha4     AS DATE      NO-UNDO.
DEFINE            VARIABLE l-Fecha5     AS DATE      NO-UNDO.

DEFINE            VARIABLE l-DDesc1     AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc2     AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc3     AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc4     AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc5     AS DECIMAL   FORMAT "Z9.99" NO-UNDO.

DEFINE            VARIABLE l-DDesc11    AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc22    AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc33    AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc44    AS DECIMAL   FORMAT "Z9.99" NO-UNDO.
DEFINE            VARIABLE l-DDesc55    AS DECIMAL   FORMAT "Z9.99" NO-UNDO.

DEFINE            VARIABLE l-DDesc111   AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc222   AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc333   AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc444   AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc555   AS DECIMAL   FORMAT "Z9.9" NO-UNDO.

DEFINE            VARIABLE l-DDesc1111  AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc2222  AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc3333  AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc4444  AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc5555  AS DECIMAL   FORMAT "Z9.9" NO-UNDO.

DEFINE            VARIABLE l-DDesc11111 AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc22222 AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc33333 AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc44444 AS DECIMAL   FORMAT "Z9.9" NO-UNDO.
DEFINE            VARIABLE l-DDesc55555 AS DECIMAL   FORMAT "Z9.9" NO-UNDO.

DEFINE            VARIABLE l-DesSIVA1   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-DesSIVA2   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-DesSIVA3   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-DesSIVA4   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-DesSIVA5   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpSIVA1   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpSIVA2   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpSIVA3   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpSIVA4   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-ImpSIVA5   AS DECIMAL   FORMAT "ZZZ,ZZ9.99" NO-UNDO.

DEFINE            VARIABLE l-NetoP1     AS DECIMAL   FORMAT "ZZZzZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-NetoP2     AS DECIMAL   FORMAT "ZZZzZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-NetoP3     AS DECIMAL   FORMAT "ZZZzZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-NetoP4     AS DECIMAL   FORMAT "ZZZzZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-NetoP5     AS DECIMAL   FORMAT "ZZZZZZZ9.99" NO-UNDO.
DEFINE            VARIABLE l-notascargo AS DECIMAL   NO-UNDO.

DEFINE            VARIABLE l-Entro1     AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE            VARIABLE l-Entro2     AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE            VARIABLE l-Entro3     AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE            VARIABLE l-Entro4     AS LOGICAL   NO-UNDO INITIAL TRUE.

DEFINE            VARIABLE l-letra1     AS CHARACTER FORMAT "X" NO-UNDO.
DEFINE            VARIABLE l-letra2     AS CHARACTER FORMAT "X" NO-UNDO.
DEFINE            VARIABLE l-letra3     AS CHARACTER FORMAT "X" NO-UNDO.
DEFINE            VARIABLE l-letra4     AS CHARACTER FORMAT "X" NO-UNDO.
DEFINE            VARIABLE l-letra5     AS CHARACTER FORMAT "X" NO-UNDO.

DEFINE            VARIABLE l-DesLetra1  AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE            VARIABLE l-DesLetra2  AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE            VARIABLE l-DesLetra3  AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE            VARIABLE l-DesLetra4  AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE            VARIABLE l-DesLetra5  AS CHARACTER FORMAT "X(15)" NO-UNDO.

DEFINE            VARIABLE l-print1     AS LOGI      INITIAL FALSE NO-UNDO.
DEFINE            VARIABLE l-print2     AS LOGI      INITIAL FALSE NO-UNDO.
DEFINE            VARIABLE l-print3     AS LOGI      INITIAL FALSE NO-UNDO.
DEFINE            VARIABLE l-print4     AS LOGI      INITIAL FALSE NO-UNDO.
DEFINE            VARIABLE l-print5     AS LOGI      INITIAL FALSE NO-UNDO.

DEFINE            VARIABLE l-Ref        LIKE MovProv.Refer NO-UNDO.
DEFINE            VARIABLE l-FecReg     LIKE MovProv.FecReg NO-UNDO.
DEFINE            VARIABLE l-FecVen     LIKE MovProv.FecVenc NO-UNDO.
DEFINE            VARIABLE l-Temp       AS CHARACTER NO-UNDO FORMAT 'x(2)'.
DEFINE            VARIABLE l-Simb       AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb2      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb3      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb4      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb5      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb6      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb7      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb8      AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb9      AS CHARACTER NO-UNDO FORMAT 'x(2)'.
DEFINE            VARIABLE l-Simb10     AS CHARACTER NO-UNDO FORMAT 'x(2)'.
DEFINE            VARIABLE l-Simb11     AS CHARACTER NO-UNDO FORMAT 'x(2)'.
DEFINE            VARIABLE l-Simb12     AS CHARACTER NO-UNDO FORMAT 'x(2)'.
DEFINE            VARIABLE l-Simb13     AS CHARACTER NO-UNDO FORMAT 'x(2)'.
DEFINE            VARIABLE l-Simb14     AS CHARACTER NO-UNDO FORMAT 'x(3)'.

DEFINE            VARIABLE l-Simb15     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb16     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb17     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb18     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb19     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb20     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb21     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb22     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb23     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb24     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb25     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb26     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb27     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-Simb28     AS CHARACTER NO-UNDO FORMAT 'x(3)'.
DEFINE            VARIABLE l-NumAju     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE l-Cons       LIKE DetFP.SeqDet NO-UNDO.

DEFINE            VARIABLE l-OtraHoja   AS INTEGER   NO-UNDO.
DEFINE            VARIABLE l-NumP       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE l-Pag        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-Hoja       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE l-totec      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-pedidoesp  AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-totimp     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-totdesc    AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-ImpNoSolic AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE l-SwCanc     AS LOGICAL   NO-UNDO. 
DEFINE            VARIABLE l-Exceso     LIKE DetEC.CantUMI NO-UNDO.
DEFINE            VARIABLE l-FecCanc    LIKE CancOC.FecCan NO-UNDO.

DEFINE            VARIABLE v-totec      LIKE l-totec NO-UNDO.
DEFINE            VARIABLE v-equiv      LIKE artpres.equiv NO-UNDO.

DEFINE NEW SHARED VARIABLE l-despp      AS DECIMAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-costoesp   AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-MovProv FOR MovProv.
DEFINE BUFFER b-EC       FOR EC.
DEFINE BUFFER b-DetEC    FOR DetEC.

DEFINE TEMP-TABLE w-DetOC
    FIELD Id-Articulo LIKE /* ARemoto. */ DetOC.Id-Articulo
    FIELD Id-Color    LIKE /* ARemoto. */ DetOC.Id-Color
    FIELD Cancelado   LIKE /* ARemoto. */ DetOC.Cancelado
    FIELD CantRecUMI  LIKE /* ARemoto. */ DetOC.CantRecUMI
    FIELD CantPedUMI  LIKE /* ARemoto. */ DetOC.CantPedUMI
    INDEX Idx-Def Id-Articulo Id-Color.
    
DEFINE NEW SHARED TEMP-TABLE t-EC
    FIELD Bodega  AS CHARACTER
    FIELD Id-EC   LIKE Adosa.EC.Id-EC
    FIELD Id-Prov LIKE Adosa.EC.Id-Prov
    FIELD FecReg  LIKE Adosa.EC.FecReg
    FIELD NumFac  LIKE Adosa.EC.NumFac
    FIELD NumRem  LIKE Adosa.EC.NumRem
    FIELD Id-OC   LIKE Adosa.EC.Id-OC
    FIELD FecEmb  LIKE Adosa.EC.FecEmb
    FIELD Id-Ubic LIKE Adosa.EC.Id-Ubic
    INDEX Idx-EC Id-EC.


/* ***************************  Main Block  *************************** */

DEFINE NEW SHARED VARIABLE l-entro   AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-login   AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-menu    AS CHARACTER INITIAL ["Proveedor","Folio","Fecha"]
    EXTENT 3 FORMAT "x(10)" NO-UNDO.
DEFINE            VARIABLE l-fp      AS CHARACTER FORMAT "x(60)" NO-UNDO.
DEFINE            VARIABLE l-impreso AS LOGICAL   FORMAT "Si/No" NO-UNDO.
DEFINE            VARIABLE l-prov    LIKE Prov.Id-Prov NO-UNDO.
DEFINE            VARIABLE l-finfp   LIKE FP.Id-FP NO-UNDO.
DEFINE            VARIABLE l-inifec  LIKE FP.FecReg NO-UNDO.
DEFINE            VARIABLE l-finfec  LIKE FP.FecReg NO-UNDO.
DEFINE            VARIABLE l-yess    AS LOGI      NO-UNDO.
DEFINE            VARIABLE l-indice  AS INTEGER   NO-UNDO.
DEFINE            VARIABLE l-Def     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-ps      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE l-arch2   AS CHARACTER.   
DEFINE            VARIABLE v-nombre  AS CHARACTER.

DEFINE            VARIABLE l-SumDesc AS DECIMAL   NO-UNDO.
/* **********************  Internal Procedures  *********************** */



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE FormaDePago:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  IdUsuario AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  l-inifp  LIKE FP.Id-FP  NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsForma.

    /* Inicia log */
    LOG-MANAGER:WRITE-MESSAGE("/FormaDePago[GET] Ejecutado por usuario: " + IdUsuario).
    FIND Usuario WHERE Usuario.Id-User = IdUsuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "Usuario Incorrecto "
            IdError   = TRUE.
        LOG-MANAGER:WRITE-MESSAGE("ERROR: Usuario no encontrado en /FormaDePago: " + IdUsuario).
        RETURN.
    END.
    FOR EACH FP WHERE FP.Id-FP >= l-inifp AND FP.Id-FP <= l-inifp NO-LOCK:
        FIND Prov OF FP NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Prov) THEN NEXT.   
        IF SUBSTRING(FP.Id-FP, 1, 1) = 'M' THEN 
        DO:  
            RUN cxpc0045.p (INPUT FP.Id-FP, INPUT Prov.FormaPago, INPUT 0,INPUT IdUsuario,OUTPUT Respuesta).
            ASSIGN 
                l-ps = "ps".
        END.   
        IF SUBSTRING(FP.Id-FP, 1, 1) = 'F' THEN 
            RUN cxpc0044.p (INPUT FP.Id-FP, INPUT Prov.FormaPago,INPUT IdUsuario,OUTPUT Respuesta).
        ASSIGN 
            l-yess = TRUE.
    END.   
    RETURN.         
END PROCEDURE.

PROCEDURE "cxpc0045.p":
    /* ---------------------------------------------------------------
     Purpose: 
      Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
      Modulo   : Cuentas por Cobrar
      Programa : cxpc0045.p
      Funcion  : Factura de Foma de Pago (Reporte)
      Autor    : IOC
      Fecha    : 04-01-1996
    --------------------------------------------------------------------- */
    /*
    Los parametros son :
    1) El numero de FORMA DE PAGO que es FP.Id-FP
    2) El tipo de FORMA DE PAGO al Proveedor Prov.FormaPago
       Valores de l-tipo son:
       1 = ENVIAR   2 = VIENE  3 = DEPOSITAR 
    */

    DEFINE INPUT PARAMETER l-FP     LIKE FP.Id-FP        NO-UNDO.
    DEFINE INPUT PARAMETER l-tipo   LIKE Prov.FormaPago  NO-UNDO.
    DEFINE INPUT PARAMETER l-ConSis AS INTEGER               NO-UNDO.
    DEFINE INPUT PARAMETER  IdUsuario AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    
    
    DO TRANSACTION :
        FIND FIRST FP WHERE FP.Id-FP = l-FP EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE (FP)  THEN 
        DO:
            ASSIGN 
                Respuesta = "No existe esta FORMA DE PAGO...".  
            RETURN.   
        END.
        ASSIGN 
            FP.Impreso = TRUE
            FP.Version = FP.Versio + 1.
    END.  

    RELEASE FP.

    FIND FIRST FP WHERE FP.Id-FP = l-FP NO-LOCK NO-ERROR.
    
    LOG-MANAGER:WRITE-MESSAGE("/FormaDePago[GET] Tipo M : " + l-FP). 
    
    ASSIGN 
        l-Mensaj = '' 
        l-Temp   = " Des1   Des2   Des3   Des4 "
        l-FecVen = IF FP.FecEmb > Fp.FecFac THEN FP.FecEmb ELSE FP.FecFac.
    IF l-ConSis = 1 THEN l-Mensaj = "FACTURA SIN OC. ".
    IF l-ConSis = 2 THEN l-Mensaj = "FACTURA CONCILIADA POR SISTEMA".
  
    ASSIGN 
        l-ImpRet = FP.ImpRet.
      
    FIND Usuario WHERE Usuario.Id-User = FP.Id-Per NO-LOCK NO-ERROR.
    l-Usuario = CAPS(IdUsuario).
    FIND Prov OF FP NO-LOCK NO-ERROR .
    FIND Comprador WHERE Comprador.Id-Comp = Prov.Id-Comp NO-LOCK NO-ERROR.
    /* FIND FIRST EntFP WHERE EntFP.Id-Fp = FP.Id-FP NO-LOCK NO-ERROR.
    IF AVAILABLE(EntFP) THEN DO: */
    ASSIGN 
        l-totec = 0
        l-despp = 0.
    l-ImpNoSolic = 0.
    l-SwCanc = FALSE.
    l-ordenes = "".

    FOR EACH EntFP WHERE EntFP.Id-FP = FP.Id-FP NO-LOCK:
        /* <<<<< Si Modificas Aqui tambien hazlo en cxpc0047.p >>>>> */
        IF AVAILABLE EntFP THEN 
            FIND FP WHERE FP.Id-FP = EntFP.Id-FP NO-LOCK NO-ERROR.
 
        EMPTY TEMP-TABLE w-DetOC.
        FOR EACH EC WHERE EC.Id-EC = EntFP.Id-EC NO-LOCK :

            CREATE t-EC.
            ASSIGN 
                t-EC.Bodega  = 'MATRIZ'
                t-EC.Id-EC   = EC.Id-EC
                t-EC.Id-Prov = EC.Id-Prov
                t-EC.FecReg  = EC.FecReg
                t-EC.NumFac  = EC.NumFac
                t-EC.NumRem  = EC.NumRem
                t-EC.Id-OC   = EC.Id-OC
                t-EC.FecEmb  = EC.FecEmb
                t-EC.Id-Ubic = EC.Id-Ubic.
            IF EC.Id-OC > 0 THEN 
            DO:
                IF NOT CAN-DO(l-ordenes,TRIM(STRING(EC.Id-OC,"zzzzz9"))) THEN 
                DO:
                    IF l-ordenes > "" THEN l-ordenes = l-ordenes + ", ".
                    ASSIGN 
                        l-ordenes = l-ordenes + TRIM(STRING(EC.Id-OC,"zzzzz9")).
                END.
          
                FIND OC WHERE OC.Id-OC = EC.Id-OC NO-LOCK NO-ERROR.
                IF AVAILABLE OC THEN 
                DO:
                    FOR EACH DetOC WHERE DetOC.Id-OC = OC.Id-OC NO-LOCK:
                        CREATE w-DetOC.
                        ASSIGN 
                            w-DetOC.Id-Articulo = DetOC.Id-Articulo
                            w-DetOC.Id-Color    = DetOC.Id-Color
                            w-DetOC.Cancelado   = DetOC.Cancelado
                            w-DetOC.CantRecUMI  = 0
                            w-DetOC.CantPedUMI  = DetOC.CantPedUMI.
                    END.
                    FOR EACH b-EC WHERE b-EC.Id-OC = EC.Id-OC AND b-EC.FecReg >= OC.FecReg 
                        NO-LOCK BY b-EC.FecReg BY b-EC.Id-EC:
                        FOR EACH b-DetEC WHERE b-DetEC.Id-EC = b-EC.Id-EC NO-LOCK:
                            FIND FIRST w-DetOC WHERE w-DetOC.Id-Articulo = b-DetEC.Id-Articulo
                                AND w-DetOC.Id-Color    = b-DetEC.Id-Color
                                EXCLUSIVE-LOCK NO-ERROR.
                            IF AVAILABLE w-DetOC THEN 
                                ASSIGN w-DetOC.CantRecUMI = w-DetOC.CantRecUMI + (b-DetEC.CantUMI - b-DetEC.CancUMI).
                            RELEASE w-DetOC. 
                        END.
                        IF b-EC.Id-EC = EC.Id-EC THEN LEAVE.
                    END.
                END.
            END.
            ELSE RELEASE OC.
      
            l-FecCanc = ?. 
            IF AVAILABLE OC THEN 
            DO:
                IF OC.Tipo = "3" THEN 
                    ASSIGN l-pedidoesp = "     PEDIDO ESPECIAL      ".
                ELSE IF OC.id-Pedido <> "" THEN 
                        ASSIGN l-pedidoesp = " PEDIDO ESPECIAL (" + OC.Id-Pedido + ")".
                    ELSE ASSIGN l-pedidoesp = "                          ".
                IF OC.Estatus >= 3 THEN 
                DO:
                    FIND FIRST CancOC WHERE CancOC.Id-OC = OC.Id-OC NO-LOCK NO-ERROR.
                    IF AVAILABLE CancOC THEN 
                        l-FecCanc = CancOC.FecCan.
                END.
            END. /* si existe oc */
      
            IF l-consis <> 2 THEN 
            DO:
                ASSIGN 
                    l-tipocambio = 1.
                FOR EACH DetEC WHERE DetEC.Id-EC = EC.Id-EC AND
                    (DetEC.CantUMI - DetEC.CancUMI) <> 0 NO-LOCK.

                    l-Exceso = 0.
                    IF NOT AVAILABLE OC OR 
                        (AVAILABLE OC AND OC.Estatus >= 3 AND l-FecCanc < EC.FecReg) 
                        THEN 
                    DO: 
                        ASSIGN 
                            l-Exceso = (DetEc.CantUMI - DetEC.CancUMI).
                        IF AVAILABLE OC THEN ASSIGN l-SwCanc = TRUE. 
                    END.
                    ELSE 
                    DO:
                        IF OC.Estatus >= 3 THEN
                            FIND FIRST w-DetOC WHERE w-DetOC.Id-Art = DetEC.Id-Art
                                AND w-DetOC.Id-Color = DetEC.Id-Color
                                NO-LOCK NO-ERROR.
                        ELSE 
                            FIND FIRST w-DetOC WHERE w-DetOC.Id-Art = DetEC.Id-Art
                                AND w-DetOC.Id-Color = DetEC.Id-Color
                                AND NOT w-DetOC.Cancelado NO-LOCK NO-ERROR.
                        IF AVAILABLE w-DetOC THEN 
                        DO:
                            IF w-DetOC.CantRecUMI > w-DetOC.CantPedUMI
                                THEN ASSIGN l-Exceso = (w-DetOC.CantRecUMI - w-DetOC.CantPedUMI).
                            IF w-DetOC.Cancelado THEN ASSIGN l-SwCanc = TRUE. 
                        END.
                        ELSE 
                        DO:
                            ASSIGN 
                                l-Exceso = (DetEc.CantUMI - DetEC.CancUMI).
                            FIND FIRST w-DetOC WHERE w-DetOC.Id-Art = DetEC.Id-Art
                                AND w-DetOC.Id-Color = DetEC.Id-Color
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE w-DetOC THEN ASSIGN l-SwCanc = TRUE.
                        END.
                    END.
                    FIND DetOC WHERE DetOC.Id-OC = EC.Id-OC
                        AND DetOC.Id-Art = DetEC.Id-Art
                        AND DetOC.Id-Color = DetEC.Id-Color
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE detoc AND detoc.costoesp THEN
                        v-costoesp = "CE".                
               
                    IF AVAILABLE DetOC THEN  
                    DO:


                        FIND FIRST ArtPres WHERE ArtPres.Id-Pres     = DetEC.Id-Pres AND
                            ArtPres.Id-Articulo = DetEC.Id-ARticulo
                            NO-LOCK NO-ERROR.

                        RUN /usr2/adosa/procs/coma0116.p (INPUT DetOC.Id-OC, 
                            INPUT DetOC.Id-Articulo, 
                            INPUT DetOc.Id-Color,
                            INPUT-OUTPUT l-totimp,
                            INPUT-OUTPUT l-totdesc).

                        IF AVAILABLE FP AND DetOC.Id-Moneda <> FP.Id-Moneda
                            AND Fp.Id-Moneda <> 5 THEN
                            FIND FIRST TipoCambio WHERE TipoCambio.Id-Moneda = DetOC.Id-Moneda
                                NO-LOCK NO-ERROR.
                        ELSE
                            FIND FIRST TipoCambio WHERE TipoCambio.Id-Moneda = 1 NO-LOCK.
                          
                        ASSIGN 
                            l-tipocambio = IF AVAILABLE TipoCambio
                                        THEN TipoCambio.Importe
                                        ELSE 1.

                        IF AVAILABLE ArtPres THEN 
                            v-equiv = artpres.equiv.
                        ELSE
                            v-equiv = 1.

                        ASSIGN 
                            l-precunit = (l-totimp /  DetOC.CantPedUMI)
                            l-totimp   = l-precunit * ( DetEC.CantUMI - DetEC.CancUMI)  
                            l-totec    = (l-totec + l-totimp).

                        FIND Artprov WHERE Artprov.Id-Prov = OC.Id-Prov AND ArtProv.Id-Articulo = DetOC.Id-Articulo
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE ArtProv AND ArtProv.IncPP = TRUE AND ArtProv.DescPP > 0 THEN 
                        DO:
                            l-despp = l-despp + (l-totimp * ArtProv.DescPP / 100).
                        END.
                  
                        IF l-Exceso > 0 THEN /* Aqui suma el valor de cantidad en exceso de un renglon que SI existe en la OC */ 
                            ASSIGN l-ImpNoSolic = l-ImpNoSolic + (l-precunit * l-Exceso).

                    END. /* del available detoc */ 
                    ELSE 
                    DO:
                        FIND FIRST adosa.HCosto WHERE HCosto.Id-Articulo = DetEC.Id-Articulo AND
                            HCosto.Id-Prov     = EC.Id-Prov NO-LOCK NO-ERROR.
                        IF AVAILABLE HCosto THEN 
                        DO:
                            FOR EACH HCosto WHERE HCosto.Id-Articulo = DetEC.Id-Articulo
                                AND HCosto.Id-Prov     = EC.Id-Prov 
                                NO-LOCK BREAK BY HCosto.Consecutivo DESCENDING:
    
                                FIND moneda WHERE moneda.id-moneda = hcosto.id-moneda
                                    NO-LOCK NO-ERROR.
                                FIND FIRST tipocambio WHERE tipocambio.id-moneda =
                                    hcosto.id-moneda NO-LOCK NO-ERROR.
                                IF hcosto.id-moneda = 5 THEN 
                                    FIND FIRST tipocambio WHERE tipocambio.id-moneda = 3 
                                        NO-LOCK NO-ERROR.
                                ASSIGN 
                                    l-tipocambio = IF AVAILABLE TipoCambio
                                               THEN TipoCambio.Importe
                                               ELSE 1.
    
                                FIND Artprov WHERE Artprov.Id-Prov = HCosto.Id-Prov AND ArtProv.Id-Articulo = HCosto.Id-Articulo
                                    NO-LOCK NO-ERROR.
                        
                                IF AVAILABLE OC THEN 
                                DO:
                                    IF HCosto.FecReg <= OC.FecReg OR LAST-OF(HCosto.Consecutivo) THEN 
                                    DO:
                                        ASSIGN 
                                            l-precunit   = HCosto.Costo / l-tipocambio
                                            l-DifUnit    = l-PrecUnit - (l-PrecUnit / (1 - (adosa.Artprov.DescPP / 100)))
                                            l-PrecUnit   = l-PrecUnit - l-DifUnit
                                            l-totimp     = l-precunit * (DetEC.CantUMI - DetEC.CancUMI)
                                            l-totec      = l-totec + 
                                                     (((l-precunit * (DetEc.CantUMI - DetEC.CancUMI))) *
                                                      IF Prov.Tipo <> 'E' AND Prov.Tipo <> 'X' THEN l-TipoCambio ELSE 1)
                                            l-ImpNoSolic = l-ImpNoSolic +
                                                     (((l-precunit * (DetEc.CantUMI - DetEC.CancUMI))) *
                                                      IF Prov.Tipo <> 'E' AND Prov.Tipo <> 'X' THEN l-TipoCambio ELSE 1).
                                        IF AVAILABLE ArtProv AND ArtProv.DescPP > 0 
                                            AND ArtProv.IncPP = TRUE THEN 
                                        DO:
                                            l-DesPP = l-DesPP + (l-TotImp * ArtProv.DescPP / 100).
                                        END.
                                        LEAVE.
                                    END.
                                END.
                                ELSE 
                                DO:
                                    ASSIGN 
                                        l-precunit   = HCosto.Costo / l-tipocambio
                                        l-totec      = l-totec +
                                                 (((l-precunit * (DetEc.CantUMI - DetEC.CancUMI))) *
                                                  IF Prov.Tipo <> 'E' AND Prov.Tipo <> 'X' THEN l-TipoCambio ELSE 1)
                                        l-ImpNoSolic = l-ImpNoSolic +
                                                 (((l-precunit * (DetEc.CantUMI - DetEC.CancUMI))) *
                                                  IF Prov.Tipo <> 'E' AND Prov.Tipo <> 'X' THEN l-TipoCambio ELSE 1)
                                        l-DifUnit    = l-PrecUnit - (l-PrecUnit / (1 - (adosa.Artprov.DescPP / 100)))
                                        l-PrecUnit   = l-PrecUnit - l-DifUnit
                                        l-totimp     = l-precunit * (DetEC.CantUMI - DetEC.CancUMI).
                                    LEAVE.
                                END.
                            END. /* del for each hcosto */
                        END. /* del if available hcosto */
                        ELSE 
                        DO:
                            RUN /usr2/adosa/procs/cnfa0450.p(INPUT DetEC.Id-Articulo,
                                INPUT-OUTPUT l-PrecUnit,
                                INPUT EC.Id-Prov).
                            FIND Artprov WHERE Artprov.Id-Prov = EC.Id-Prov 
                                AND ArtProv.Id-Articulo = DetEC.Id-Articulo
                                NO-LOCK NO-ERROR.
                            ASSIGN 
                                l-totec      = l-totec +
                                             (((l-precunit * (DetEc.CantUMI - DetEC.CancUMI))) *
                                              IF Prov.Tipo <> 'E' AND Prov.Tipo <> 'X' THEN l-TipoCambio ELSE 1)
                                l-ImpNoSolic = l-ImpNoSolic +
                                             (((l-precunit * (DetEc.CantUMI - DetEC.CancUMI))) *
                                              IF Prov.Tipo <> 'E' AND Prov.Tipo <> 'X' THEN l-TipoCambio ELSE 1)
                                l-DifUnit    = l-PrecUnit - (l-PrecUnit / (1 - (adosa.Artprov.DescPP / 100)))
                                l-PrecUnit   = l-PrecUnit - l-DifUnit
                                l-totimp     = l-precunit * (DetEC.CantUMI - DetEC.CancUMI).
                     
                        END.
                 

                    END. /* del else del if */
                END. /* del for each detec */
            END.
            RELEASE t-EC.
        END.

        FOR EACH t-EC WHERE t-EC.Id-EC = EntFP.Id-EC NO-LOCK :
            IF l-entrada <> "" THEN ASSIGN l-entrada = l-entrada + ", ".
            ASSIGN 
                l-entrada = l-entrada + CAPS(t-EC.Id-EC).
        END.
        FIND FIRST t-EC WHERE t-EC.Id-EC = EntFP.Id-EC NO-LOCK NO-ERROR.

    END. /* FOR EACH adosa.entfp */  
    
    l-entrada = "".    
    FOR EACH t-EC NO-LOCK :

        IF l-entrada <> "" THEN ASSIGN l-entrada = l-entrada + ", ".
        ASSIGN 
            l-entrada = l-entrada + CAPS(t-EC.Id-EC).
    END.
    
    IF l-consis <> 2 THEN 
    DO:
        ASSIGN 
            l-Mensaj = l-mensaj + "TOT ENT." + TRIM(STRING(l-totec,"->>>,>>>,>>9.99")).
        IF l-despp > 0 THEN
            ASSIGN l-Mensaj = l-Mensaj + ", INC PP." + TRIM(STRING(l-totec - l-despp,"->>>,>>9.99")).

        IF v-costoesp <> "" THEN
            l-mensaj = l-mensaj + " (" + TRIM(v-costoesp) + ")". 
        IF l-ImpNoSolic > 0 THEN 
        DO:
            IF l-SwCanc THEN
                ASSIGN l-Mensaj = l-Mensaj + ", NO SOL+CANC " + TRIM(STRING(l-ImpNoSolic,"$>>>,>>>,>>9.99")).
            ELSE ASSIGN l-Mensaj = l-Mensaj + ", MCIA NO SOL " + TRIM(STRING(l-ImpNoSolic,"$>>>,>>>,>>9.99")).
        END.
    END.
    
    FIND Moneda WHERE Moneda.Id-Moneda = FP.Id-Moneda NO-LOCK NO-ERROR.
    l-Simb9  = IF AVAILABLE Moneda THEN Moneda.Simbolo ELSE ''.

    l-Simb10 = l-Simb9. 
    l-Simb11 = l-Simb9. 
    l-Simb12 = l-Simb9.
    l-Simb13 = l-Simb9.

    l-Simb = IF AVAILABLE Moneda THEN TRIM("|" + Moneda.Simbolo) ELSE ''.
    l-Simb2  = l-Simb. 
    l-Simb3  = l-Simb. 
    l-Simb4  = l-Simb. 
    l-Simb5  = l-Simb.
    l-Simb6  = l-Simb. 
    l-Simb7  = l-Simb. 
    l-Simb8  = l-Simb.

    l-simb14 = l-Simb.
    l-Simb15  = l-Simb. 
    l-Simb16  = l-Simb. 
    l-Simb17  = l-Simb.
    l-Simb18  = l-Simb. 
    l-Simb19  = l-Simb. 
    l-Simb20  = l-Simb.
    l-Simb21  = l-Simb. 
    l-Simb22  = l-Simb. 
    l-Simb23  = l-Simb.
    l-Simb24  = l-Simb. 
    l-Simb25  = l-Simb. 
    l-Simb26  = l-Simb.
    l-simb27  = l-Simb. 
    l-Simb28  = l-Simb.
    
    
    ASSIGN 
        l-Ref    = '' 
        l-FecReg = ?.
    FIND FIRST MovProv WHERE MovProv.Id-Acr = FP.Id-Prov AND
        MovProv.Asocia = FP.Id-FP AND MovProv.Tipo = 103 NO-LOCK NO-ERROR.
    IF AVAILABLE MovProv THEN 
    DO:
        FIND FIRST bf-MovProv WHERE bf-MovProv.Tipo = 302 AND
            bf-MovProv.Id-Acr = MovProv.Id-Acr AND
            bf-MovProv.Asocia = MovProv.Refer NO-LOCK NO-ERROR.
        ASSIGN 
            l-Ref    = bf-MovProv.Refer 
            l-FecReg = bf-MovProv.FecReg.
    END.
    l-des1   = "".
    l-des2   = "". 
    l-des3   = "". 
    l-des4   = "".
    l-NumP   = 1.
    l-NumAju = 0.
    FOR EACH DetFP WHERE DetFP.Id-FP = FP.Id-FP NO-LOCK BY DetFP.SeqDet:
        IF DetFP.TipoMov = "X" THEN 
        DO:
            ASSIGN 
                l-SumDesc = 1.
            FOR EACH DescFP WHERE DescFP.Id-FP = DetFP.Id-FP AND
                DescFP.Id-Articulo = DetFP.Descr AND
                DescFP.Id-Color    = DetFP.Id-Color NO-LOCK.
                ASSIGN 
                    l-SumDesc = l-SumDesc * (1 - (DescFP.Dcto / 100)).
            END.
            ASSIGN 
                l-SinIVA = l-SinIVA + (DetFP.PrecUnit * DetFP.Cant) * (l-SumDesc).
        END.
        IF DetFP.TipoMov = "A" OR DetFP.TipoMov = "C" THEN l-NumAju = l-NumAju + 1.
        IF DetFP.TipoMov = "A" THEN 
            ASSIGN l-ImpDesc = l-ImpDesc + (DetFP.PrecUnit * DetFP.Cant).
        IF DetFP.TipoMov = "C" THEN 
            ASSIGN l-ImpDesc    = l-ImpDesc - (DetFP.PrecUnit * DetFP.Cant)
                l-notascargo = l-notascargo + (DetFP.PrecUnit * DetFP.Cant).

        ASSIGN 
            l-Dias1     = FP.PlazoPP1         
            l-Dias2     = FP.PlazoPP2
            l-DDesc1    = FP.DctoPP11         
            l-DDesc2    = FP.DctoPP21
            l-DDesc11   = FP.DctoPP12         
            l-DDesc22   = FP.DctoPP22
            l-DDesc111  = FP.DctoPP13         
            l-DDesc222  = FP.DctoPP23
            l-DDesc1111 = FP.DctoPP14         
            l-DDesc2222 = FP.DctoPP24
            l-Fecha1    = l-FecVen + FP.PlazoPP1 
            l-Fecha2    = l-FecVen + FP.PlazoPP2

            l-Dias3     = FP.PlazoPP3         
            l-Dias4     = FP.PlazoPP4
            l-DDesc3    = FP.DctoPP31         
            l-DDesc4    = FP.DctoPP41
            l-DDesc33   = FP.DctoPP32         
            l-DDesc44   = FP.DctoPP42
            l-DDesc333  = FP.DctoPP33         
            l-DDesc444  = FP.DctoPP43
            l-DDesc3333 = FP.DctoPP34         
            l-DDesc4444 = FP.DctoPP44
            l-Fecha3    = l-FecVen + FP.PlazoPP3 
            l-Fecha4    = l-FecVen + FP.PlazoPP4.

        ASSIGN 
            l-Dias5     = FP.Id-Plazo
            l-DDesc5    = 0 
            l-DDesc55   = 0 
            l-DDesc555  = 0 
            l-DDesc5555 = 0
            l-Fecha5    = l-FecVen + FP.Id-Plazo.
        IF l-Entro1 THEN
            ASSIGN l-Des1   = DetFP.Descr + "  " + DetFP.Descr2
                l-CA1    = CAPS(DetFP.TipoMov)
                l-Imp1   = DetFP.PrecUnit
                l-Imp11  = l-Imp1 * ( 1 + (DetFP.PorcIVA / 100))
                l-print1 = IF CAPS(DetFP.TipoMov) <> "X" THEN TRUE ELSE FALSE
                l-des1   = IF l-print1 THEN l-des1 ELSE ""
                l-Entro1 = IF CAPS(DetFp.TipoMov) <> "X" THEN FALSE ELSE TRUE.
        ELSE IF l-Entro2 THEN
                ASSIGN l-Des2   = DetFP.Descr + "  " + DetFP.Descr2
                    l-CA2    = CAPS(DetFP.TipoMov)
                    l-Imp2   = DetFP.PrecUnit
                    l-Imp22  = l-Imp2 * ( 1 + (DetFP.PorcIVA / 100))
                    l-print2 = IF CAPS(DetFP.TipoMov) <> "X" THEN TRUE ELSE FALSE
                    l-des2   = IF l-print2 THEN l-des2 ELSE ""
                    l-Entro2 = IF CAPS(DetFP.TipoMov) <> "X"
                              THEN FALSE ELSE TRUE.
            ELSE IF l-Entro3 THEN
                    ASSIGN l-Des3   = DetFP.Descr + "  " + DetFP.Descr2
                        l-CA3    = CAPS(DetFP.TipoMov)
                        l-Imp3   = DetFP.PrecUnit
                        l-Imp33  = l-Imp3 * ( 1 + (DetFP.PorcIVA / 100))
                        l-print3 = IF CAPS(DetFP.TipoMov) <> "X"
                                 THEN TRUE ELSE FALSE
                        l-des3   = IF l-print3 THEN l-des3 ELSE ""
                        l-Entro3 = IF CAPS(DetFP.TipoMov) <> "X"
                                 THEN FALSE ELSE TRUE.
                ELSE IF l-Entro4 THEN
                        ASSIGN l-Des4   = DetFP.Descr + "  " + DetFP.Descr2
                            l-CA4    = CAPS(DetFP.TipoMov)
                            l-Imp4   = DetFP.PrecUnit
                            l-Cons   = DetFP.SeqDet
                            l-Imp44  = l-Imp4 * ( 1 + (DetFP.PorcIVA / 100))
                            l-print4 = IF CAPS(DetFP.TipoMov) <> "X"
                                    THEN TRUE ELSE FALSE
                            l-des4   = IF l-print4 THEN l-des4 ELSE ""
                            l-Entro4 = IF CAPS(DetFP.TipoMov) <> "X"
                                    THEN FALSE ELSE TRUE.
    END.
    l-NumP = TRUNCATE(l-NumAju / 4, 0).  
    IF  l-NumAju MOD 4 <> 0 THEN l-NumP = l-NumP + 1.
    IF l-numaju = 0 THEN l-NumP = l-NumP + 1.
    l-Pag = '1/' + STRING(l-NumP,'9').
    IF l-Des1 <> '' THEN l-Des1 = (l-Des1) + FILL(" ", 62 - LENGTH(l-Des1)).
    IF l-Des2 <> '' THEN l-Des2 = (l-Des2) + FILL(" ", 62 - LENGTH(l-Des2)).
    IF l-Des3 <> '' THEN l-Des3 = (l-Des3) + FILL(" ", 62 - LENGTH(l-Des3)).
    IF l-Des4 <> '' THEN l-Des4 = (l-Des4) + FILL(" ", 62 - LENGTH(l-Des4)).
    ASSIGN 
        l-TotFact    = l-SinIVA - l-ImpDesc
        l-TotAntFact = l-TotFact - (FP.TotAnt / (1 + (FP.PorcIVA / 100))).
    IF l-tipo = "1" THEN 
    DO:
        ASSIGN 
            l-letra1 = (IF Prov.Atraves = 1 THEN "X" ELSE " ")
            l-letra2 = (IF Prov.Atraves = 2 THEN "X" ELSE " ").
            
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 1
            ttFormaDePago.OrdenDeCompra  = l-ordenes
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = t-EC.Id-Ubic 
            WHEN AVAILABLE t-EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + "" +  STRING(FP.Benef)     
            ttFormaDePago.Bloque         = "ENVIAR [X] "
                                             + "CORREO[" + l-letra1 + "]"
                                             + " AUTOBUS [" + l-letra2 + "]"
                                             + " MENSAJERIA : " + Prov.Mensaj.                   
    END.
    IF  l-tipo = "2" THEN 
    DO:
        ASSIGN 
            l-letra3 = (IF Prov.Llamar THEN "X" ELSE " ")
            l-letra4 = (IF Prov.Llamar THEN " " ELSE "X").
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 2
            ttFormaDePago.OrdenDeCompra  = l-ordenes
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = t-EC.Id-Ubic 
            WHEN AVAILABLE t-EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)          
            ttFormaDePago.Bloque         = "VIENE[X] " + "LLAMAR SI[" + l-letra3 + "] "
                                                            + "NO[" + l-letra4 + "] "
                                                            + "TELEFONO: " + STRING(Prov.LlamarTel) + " "  
                                                            + "SR(ITA): " + STRING(Prov.LlamarCon).                      
    END.   
    IF  l-tipo ="3" THEN 
    DO:
        
        FIND FIRST Banco WHERE Banco.Id-Banco = Prov.Id-Banco NO-LOCK NO-ERROR.
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 3
            ttFormaDePago.OrdenDeCompra  = l-ordenes
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = t-EC.Id-Ubic 
            WHEN AVAILABLE t-EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)  
            ttFormaDePago.Bloque         = "DEPOSITAR[X] " + " BANCO : " + STRING(Banco.Id-Banco) + " " + STRING(Banco.Nombre) + " CUENTA: " + STRING(Prov.Cuenta) + ", ABA " + STRING(Prov.ABA).  
        
    END.
    IF  l-tipo = "" THEN 
    DO:
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 4
            ttFormaDePago.OrdenDeCompra  = l-ordenes
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = t-EC.Id-Ubic 
            WHEN AVAILABLE t-EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)    
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)  
            ttFormaDePago.Bloque         = "DEPOSITAR[ ]    BANCO :   CUENTA:      ,ABA ".  
        
    END.
      
    
    CREATE ttImportes.
    ASSIGN 
        ttImportes.ImpSiva  = l-SinIVA
        ttImportes.ImpRet   = l-ImpRet
        ttImportes.ImpDes   = l-ImpDesc
        ttImportes.AntSiva  = (FP.TotAnt / (1 + (FP.PorcIVA / 100)))
        ttImportes.TotFact  = l-TotFact
        ttImportes.TotFinal = l-TotAntFact.
          
    CREATE ttPie. /* forma */
    ASSIGN 
        ttPie.Obs       = FP.Coment1
        ttPie.Capturo   = Usuario.Nom-Usuario 
        WHEN AVAILABLE Usuario
        ttPie.Verifico  = l-Mensaj
        ttPie.Comprador = Comprador.Nombre 
        WHEN AVAILABLE Comprador
        ttPie.Fecha     = FP.FecReg.
     
    IF l-print1 THEN 
    DO:
        CREATE ttDescr. 
        ASSIGN 
            ttDescr.Tipo        = l-CA1
            ttDescr.Descripcion = l-Des1
            ttDescr.ImpCIva     = ROUND(l-Imp11,2)  
            ttDescr.ImpSIva     = ROUND(l-Imp1,2).   
    END.
    IF l-print2 THEN 
    DO:
        CREATE ttDescr. 
        ASSIGN 
            ttDescr.Tipo        = l-CA2
            ttDescr.Descripcion = l-Des2
            ttDescr.ImpCIva     = ROUND(l-Imp22,2)
            ttDescr.ImpSIva     = ROUND(l-Imp2,2).   
    END.
    IF l-print3 THEN 
    DO:
        CREATE ttDescr. 
        ASSIGN 
            ttDescr.Tipo        = l-CA3
            ttDescr.Descripcion = l-Des3
            ttDescr.ImpCIva     = ROUND(l-Imp33,2)
            ttDescr.ImpSIva     = ROUND(l-Imp3,2).   
    END.
    IF l-print4 THEN 
    DO:
        CREATE ttDescr. 
        ASSIGN 
            ttDescr.Tipo        = l-CA4
            ttDescr.Descripcion = l-Des4
            ttDescr.ImpCIva     = ROUND(l-Imp44,2)
            ttDescr.ImpSIva     = ROUND(l-Imp4,2).   
    END.
    l-DesLetra1 = "                       ".
    IF l-Dias1 > 0 AND l-DDesc1 <> 0 THEN 
    DO:
        ASSIGN 
            l-totantfact = l-totantfact - l-notascargo
            l-DD1        = l-TotAntFact  * (l-DDesc1 / 100)
            l-DD2        = (l-TotAntFact - l-DD1) * (l-DDesc11 / 100)
            l-DD3        = (l-TotAntFact - l-DD1 - l-DD2) * (l-DDesc111 / 100)
            l-DD4        = (l-TotAntFact - l-DD1 - l-DD2 - l-DD3) *
                        (l-DDesc1111 / 100)
            l-totantfact = l-totantfact + l-notascargo
            l-DesLetra1  = STRING(l-DDesc1,"Z9.99")   + "  " +
                         STRING(l-DDesc11,"Z9.99")  + "  " +
                         STRING(l-DDesc111,"Z9.99") + "  " +
                         STRING(l-DDesc1111,"z9.99")
            l-DesSIVA1   = l-DD1 + l-DD2 + l-DD3 + l-DD4
            l-ImpSIVA1   = l-TotAntFact - l-DesSIVA1
            l-NetoP1     = l-ImpSIVA1 * ( 1 + (FP.PorcIVA / 100)).
    
        CREATE ttPorDias.
        ASSIGN 
            ttPorDias.Dias     = l-Dias1 
            ttPorDias.FecVence = l-Fecha1 
            ttPorDias.Des1     = STRING(l-DDesc1,"Z9.99")
            ttPorDias.Des2     = STRING(l-DDesc11,"Z9.99")
            ttPorDias.Des3     = STRING(l-DDesc111,"Z9.99")
            ttPorDias.Des4     = STRING(l-DDesc1111,"Z9.99")  
            ttPorDias.DesSIva  = ROUND(l-DesSIVA1,2) 
            ttPorDias.ImpSIva  = ROUND(l-ImpSIVA1,2)
            ttPorDias.NetPag   = ROUND(l-NetoP1,2).
    END.
    l-DesLetra2 = "                       ".
    IF l-Dias2 > 0 AND l-DDesc2 <> 0 THEN 
    DO:
        ASSIGN 
            l-DD1       = l-TotAntFact * (l-DDesc2 / 100)
            l-DD2       = (l-TotAntFact - l-DD1) * (l-DDesc22 / 100)
            l-DD3       = (l-TotAntFact - l-DD1 - l-DD2) * (l-DDesc222 / 100)
            l-DD4       = (l-TotAntFact - l-DD1 - l-DD2 - l-DD3) *
                        (l-DDesc2222 / 100)
            l-DesLetra2 = STRING(l-DDesc2,"z9.99")   + "  " +
                         STRING(l-DDesc22,"z9.99")  + "  " +
                         STRING(l-DDesc222,"z9.99") + "  " +
                         STRING(l-DDesc2222,"z9.99")
            l-DesSIVA2  = l-DD1 + l-DD2 + l-DD3 + l-DD4
            l-ImpSIVA2  = l-TotAntFact - l-DesSIVA2
            l-NetoP2    = l-ImpSIVA2 * ( 1 + (FP.PorcIVA / 100)).
        CREATE ttPorDias.
        ASSIGN 
            ttPorDias.Dias     = l-Dias2 
            ttPorDias.FecVence = l-Fecha2 
            ttPorDias.Des1     = STRING(l-DDesc2,"Z9.99")
            ttPorDias.Des2     = STRING(l-DDesc22,"Z9.99")
            ttPorDias.Des3     = STRING(l-DDesc222,"Z9.99")
            ttPorDias.Des4     = STRING(l-DDesc2222,"Z9.99") 
            ttPorDias.DesSIva  = ROUND(l-DesSIVA2,2)
            ttPorDias.ImpSIva  = ROUND(l-ImpSIVA2,2)
            ttPorDias.NetPag   = ROUND(l-NetoP2,2).  
    END.
    l-DesLetra3 = "                       ".
    IF l-Dias3 > 0 AND l-DDesc3 <> 0 THEN 
    DO:
        ASSIGN 
            l-DD1       = l-TotAntFact * (l-DDesc3 / 100)
            l-DD2       = (l-TotAntFact - l-DD1) * (l-DDesc33 / 100 )
            l-DD3       = (l-TotAntFact - l-DD1 - l-DD2) * (l-DDesc333 / 100)
            l-DD4       = (l-TotAntFact - l-DD1 - l-DD2 - l-DD3) *
                        (l-DDesc3333 / 100)
            l-DesLetra3 = STRING(l-DDesc3,"z9.99")   + "  " +
                         STRING(l-DDesc33,"z9.99")  + "  " +
                         STRING(l-DDesc333,"z9.99") + "  " +
                         STRING(l-DDesc3333,"z9.99")
            l-DesSIVA3  = l-DD1 + l-DD2 + l-DD3 + l-DD4
            l-ImpSIVA3  = l-TotAntFact - l-DesSIVA3
            l-NetoP3    = l-ImpSIVA3 * ( 1 + (FP.PorcIVA / 100)).
           
           
        CREATE ttPorDias.
        ASSIGN 
            ttPorDias.Dias     = l-Dias3 
            ttPorDias.FecVence = l-Fecha3 
            ttPorDias.Des1     = STRING(l-DDesc3,"Z9.99")
            ttPorDias.Des2     = STRING(l-DDesc33,"Z9.99")
            ttPorDias.Des3     = STRING(l-DDesc333,"Z9.99")
            ttPorDias.Des4     = STRING(l-DDesc3333,"Z9.99") 
            ttPorDias.DesSIva  = ROUND(l-DesSIVA3,2)
            ttPorDias.ImpSIva  = ROUND(l-ImpSIVA3,2)
            ttPorDias.NetPag   = ROUND(l-NetoP3,2).
    END.
   
    l-DesLetra4 = "                       ".
    IF l-Dias4 > 0 AND l-DDesc4 <> 0 THEN 
    DO:
        ASSIGN 
            l-DD1       = l-TotAntFact * (l-DDesc4 / 100)
            l-DD2       = (l-TotAntFact - l-DD1) * (l-DDesc44 / 100)
            l-DD3       = (l-TotAntFact - l-DD1 - l-DD2) * (l-DDesc444 / 100)
            l-DD4       = (l-TotAntFact - l-DD1 - l-DD2 - l-DD3) *
                        (l-DDesc4444 / 100)
            l-DesLetra4 = STRING(l-DDesc4,"z9.99")   + "  " +
                         STRING(l-DDesc44,"z9.99")  + "  " +
                         STRING(l-DDesc444,"z9.99") + "  " +
                         STRING(l-DDesc4444,"z9.99")
            l-DesSIVA4  = l-DD1 + l-DD2 + l-DD3 + l-DD4
            l-ImpSIVA4  = l-TotAntFact - l-DesSIVA4
            l-NetoP4    = l-ImpSIVA4 * ( 1 + (FP.PorcIVA / 100)).
           
        CREATE ttPorDias.
        ASSIGN 
            ttPorDias.Dias     = l-Dias4 
            ttPorDias.FecVence = l-Fecha4 
            ttPorDias.Des1     = STRING(l-DDesc4,"Z9.99")
            ttPorDias.Des2     = STRING(l-DDesc44,"Z9.99")
            ttPorDias.Des3     = STRING(l-DDesc444,"Z9.99")
            ttPorDias.Des4     = STRING(l-DDesc4444,"Z9.99") 
            ttPorDias.DesSIva  = ROUND(l-DesSIVA4,2)
            ttPorDias.ImpSIva  = ROUND(l-ImpSIVA4,2)
            ttPorDias.NetPag   = ROUND(l-NetoP4,2).
    END.
   
    ASSIGN 
        l-DD1       = l-TotAntFact * (l-DDesc5 / 100)
        l-DD2       = (l-TotAntFact - l-DD1) * (l-DDesc55 / 100)
        l-DD3       = (l-TotAntFact - l-DD1 - l-DD2) * (l-DDesc555 / 100)
        l-DD4       = (l-TotAntFact - l-DD1 - l-DD2 - l-DD3) *
                      (l-DDesc5555 / 100)
        l-DesLetra5 = STRING(l-DDesc5,"z9.99")   + "  " +
                       STRING(l-DDesc55,"z9.99")  + "  " +
                       STRING(l-DDesc555,"z9.99") + "  " +
                       STRING(l-DDesc5555,"z9.99")
        l-DesSIVA5  = l-DD1 + l-DD2 + l-DD3 + l-DD4
        l-ImpSIVA5  = l-TotAntFact - l-DesSIVA5.
    l-NetoP5   = (l-ImpSIVA5 * ( 1 + (FP.PorcIVA / 100))) - FP.ImpRet.
    IF l-DDesc5 = 0 THEN l-DesLetra5 = '                       '. /* 23 espacios */
  
    CREATE ttPorDias.
    ASSIGN 
        ttPorDias.Dias     = l-Dias5 
        ttPorDias.FecVence = l-Fecha5 
        ttPorDias.Des1     = STRING(l-DDesc5,"Z9.99")
        ttPorDias.Des2     = STRING(l-DDesc55,"Z9.99")
        ttPorDias.Des3     = STRING(l-DDesc555,"Z9.99")
        ttPorDias.Des4     = STRING(l-DDesc5555,"Z9.99")
        ttPorDias.DesSIva  = ROUND(l-DesSIVA5,2)
        ttPorDias.ImpSIva  = ROUND(l-ImpSIVA5,2)
        ttPorDias.NetPag   = ROUND(l-NetoP5,2).      
  
          
    IF l-NumAju > 4 THEN 
    DO:
        /* Segunda Pagina de Formas de Pagos Cuando son mas de 4 Ajuste */
        l-Hoja  = 1.
        l-Cons = l-Cons + 1.
        DO WHILE TRUE:
            l-Entro1 = TRUE.  
            l-Entro2 = TRUE.  
            l-Entro3 = TRUE.  
            l-Entro4 = TRUE.
            l-Print1 = FALSE.  
            l-Print2 = FALSE.   
            l-Print3 = FALSE.  
            l-Print4 = FALSE.
            l-des1 = "                                                              ".
            l-des2 = "                                                              ". 
            l-des3 = "                                                              ". 
            l-des4 = "                                                              ".
            ASSIGN 
                l-CA1   = "" 
                l-Imp1  = 0 
                l-Imp11 = 0
                l-CA2   = "" 
                l-Imp2  = 0 
                l-Imp22 = 0
                l-CA3   = "" 
                l-Imp3  = 0 
                l-Imp33 = 0
                l-CA4   = "" 
                l-Imp4  = 0 
                l-Imp44 = 0.
            l-OtraHoja = 0.
            FOR EACH DetFP WHERE DetFP.Id-FP = FP.Id-FP AND DetFP.TipoMov <> 'X' AND   
                DetFP.SeqDet >= l-Cons NO-LOCK BY DetFP.SeqDet:
                IF DetFP.TipoMov = "A" THEN ASSIGN l-ImpDesc = l-ImpDesc + (DetFP.PrecUnit * DetFP.Cant).
                IF DetFP.TipoMov = "C" THEN ASSIGN l-ImpDesc = l-ImpDesc - (DetFP.PrecUnit * DetFP.Cant).   

                ASSIGN 
                    l-Dias1     = FP.PlazoPP1           
                    l-Dias2     = FP.PlazoPP2
                    l-DDesc1    = FP.DctoPP11         
                    l-DDesc2    = FP.DctoPP21
                    l-DDesc11   = FP.DctoPP12         
                    l-DDesc22   = FP.DctoPP22
                    l-DDesc111  = FP.DctoPP13         
                    l-DDesc222  = FP.DctoPP23
                    l-DDesc1111 = FP.DctoPP14         
                    l-DDesc2222 = FP.DctoPP24
                    l-Fecha1    = l-FecVen + FP.PlazoPP1
                    l-Fecha2    = l-FecVen + FP.PlazoPP2
         
                    l-Dias3     = FP.PlazoPP3         
                    l-Dias4     = FP.PlazoPP4
                    l-DDesc3    = FP.DctoPP31         
                    l-DDesc4    = FP.DctoPP41
                    l-DDesc33   = FP.DctoPP32         
                    l-DDesc44   = FP.DctoPP42
                    l-DDesc333  = FP.DctoPP33         
                    l-DDesc444  = FP.DctoPP43
                    l-DDesc3333 = FP.DctoPP34         
                    l-DDesc4444 = FP.DctoPP44
                    l-Fecha3    = l-FecVen + FP.PlazoPP3 
                    l-Fecha4    = l-FecVen + FP.PlazoPP4

                    l-Dias5     = FP.Id-Plazo         
                    l-DDesc5    = 0 
                    l-DDesc55   = 0 
                    l-DDesc555  = 0                   
                    l-DDesc5555 = 0 
                    l-Fecha5    = l-FecVen + FP.Id-Plazo.
                IF l-Entro1 THEN
                    ASSIGN l-Des1   = DetFP.Descr + "  " + DetFP.Descr2
                        l-CA1    = CAPS(DetFP.TipoMov)
                        l-Imp1   = DetFP.PrecUnit
                        l-Imp11  = l-Imp1 * ( 1 + (DetFP.PorcIVA / 100))
                        l-print1 = IF CAPS(DetFP.TipoMov) <> "X" THEN TRUE ELSE FALSE
                        l-des1   = IF l-print1 THEN l-des1 ELSE
                "                                                              "
                        l-Entro1 = IF CAPS(DetFp.TipoMov) <> "X" THEN FALSE ELSE TRUE.
                ELSE IF l-Entro2 THEN
                        ASSIGN l-Des2   = DetFP.Descr + "  " + DetFP.Descr2
                            l-CA2    = CAPS(DetFP.TipoMov)
                            l-Imp2   = DetFP.PrecUnit
                            l-Imp22  = l-Imp2 * ( 1 + (DetFP.PorcIVA / 100))
                            l-print2 = IF CAPS(DetFP.TipoMov) <> "X" THEN TRUE ELSE FALSE
                            l-des2   = IF l-print2 THEN l-des2 ELSE
          "                                                              "
                            l-Entro2 = IF CAPS(DetFP.TipoMov) <> "X"
                              THEN FALSE ELSE TRUE.
                    ELSE IF l-Entro3 THEN
                            ASSIGN l-Des3   = DetFP.Descr + "  " + DetFP.Descr2
                                l-CA3    = CAPS(DetFP.TipoMov)
                                l-Imp3   = DetFP.PrecUnit
                                l-Imp33  = l-Imp3 * ( 1 + (DetFP.PorcIVA / 100))
                                l-print3 = IF CAPS(DetFP.TipoMov) <> "X"
                              THEN TRUE ELSE FALSE
                                l-des3   = IF l-print3 THEN l-des3 ELSE
                "                                                              "
                                l-Entro3 = IF CAPS(DetFP.TipoMov) <> "X"
                              THEN FALSE ELSE TRUE.
                        ELSE IF l-Entro4 THEN
                                ASSIGN l-Des4   = DetFP.Descr + "  " + DetFP.Descr2
                                    l-CA4    = CAPS(DetFP.TipoMov)
                                    l-Imp4   = DetFP.PrecUnit
                                    l-Cons   = DetFP.SeqDet
                                    l-Imp44  = l-Imp4 * ( 1 + (DetFP.PorcIVA / 100))
                                    l-print4 = IF CAPS(DetFP.TipoMov) <> "X"
                              THEN TRUE ELSE FALSE
                                    l-des4   = IF l-print4 THEN l-des4 ELSE
                "                                                              "
                                    l-Entro4 = IF CAPS(DetFP.TipoMov) <> "X"
                              THEN FALSE ELSE TRUE.
                l-Cons = l-Cons + 1.
                l-OtraHoja = l-OtraHoja + 1.
                IF l-OtraHoja = 4 THEN LEAVE. 
            END.   
            IF l-Entro1 THEN NEXT. /* TENIA LEAVE VALIDAR CON MAS DE 4 AJUSTES */
            l-Hoja = l-Hoja + 1.
            l-Pag = STRING(l-Hoja,'9') + '/' + STRING(l-NumP,'9').
            IF l-Des1 <> '' THEN l-Des1 = (l-Des1) + FILL(" ", 62 - LENGTH(l-Des1)).
            IF l-Des2 <> '' THEN l-Des2 = (l-Des2) + FILL(" ", 62 - LENGTH(l-Des2)).
            IF l-Des3 <> '' THEN l-Des3 = (l-Des3) + FILL(" ", 62 - LENGTH(l-Des3)).
            IF l-Des4 <> '' THEN l-Des4 = (l-Des4) + FILL(" ", 62 - LENGTH(l-Des4)).

            /*  
            IF l-tipo = "1" THEN 
            DO:
                ASSIGN 
                    l-letra1 = (IF Prov.Atraves = 1 THEN "X" ELSE " ")
                    l-letra2 = (IF Prov.Atraves = 2 THEN "X" ELSE " ").
                CREATE ttFormaDePago.    
                ASSIGN 
                    ttFormaDePago.Tipo           = 1
                    ttFormaDePago.Fecha          = TODAY
                    ttFormaDePago.Almacen        = t-EC.Id-Ubic 
                    WHEN AVAILABLE t-EC
                    ttFormaDePago.EntradaAlmacen = l-entrada
                    ttFormaDePago.Versionn       = FP.Version 
                    ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
                    ttFormaDePago.FechaFactura   = FP.FecFac
                    ttFormaDePago.FechaEmbarque  = FP.FecEmb
                    ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + "" +  STRING(FP.Benef)     
                    ttFormaDePago.Bloque         = "ENVIAR [X] "
                                             + "CORREO[" + l-letra1 + "]"
                                             + " AUTOBUS [" + l-letra2 + "]"
                                             + " MENSAJERIA : " + Prov.Mensaj.
            END.
            IF l-tipo = "2" THEN 
            DO:
                ASSIGN 
                    l-letra3 = (IF Prov.Llamar THEN "X" ELSE " ")
                    l-letra4 = (IF Prov.Llamar THEN " " ELSE "X").
                CREATE ttFormaDePago.    
                ASSIGN 
                    ttFormaDePago.Tipo           = 2
                    ttFormaDePago.Fecha          = TODAY
                    ttFormaDePago.Almacen        = t-EC.Id-Ubic 
                    WHEN AVAILABLE t-EC
                    ttFormaDePago.EntradaAlmacen = l-entrada
                    ttFormaDePago.Versionn       = FP.Version 
                    ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
                    ttFormaDePago.FechaFactura   = FP.FecFac
                    ttFormaDePago.FechaEmbarque  = FP.FecEmb
                    ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)          
                    ttFormaDePago.Bloque         = "VIENE[X] " + "LLAMAR SI[" + l-letra3 + "] "
                                                            + "NO[" + l-letra4 + "] "
                                                            + "TELEFONO: " + STRING(Prov.LlamarTel) + " "  
                                                            + "SR(ITA): " + STRING(Prov.LlamarCon).  
            END.
            IF l-tipo = "3" THEN 
            DO:
          
        
                FIND FIRST Banco WHERE Banco.Id-Banco = Prov.Id-Banco NO-LOCK NO-ERROR.
                CREATE ttFormaDePago.    
                ASSIGN 
                    ttFormaDePago.Tipo           = 3
                    ttFormaDePago.Fecha          = TODAY
                    ttFormaDePago.Almacen        = t-EC.Id-Ubic 
                    WHEN AVAILABLE t-EC
                    ttFormaDePago.EntradaAlmacen = l-entrada
                    ttFormaDePago.Versionn       = FP.Version 
                    ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
                    ttFormaDePago.FechaFactura   = FP.FecFac
                    ttFormaDePago.FechaEmbarque  = FP.FecEmb
                    ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)  
                    ttFormaDePago.Bloque         = "DEPOSITAR[X] " + " BANCO : " + STRING(Banco.Id-Banco) + " " + STRING(Banco.Nombre) + " CUENTA: " + STRING(Prov.Cuenta) + ", ABA " + STRING(Prov.ABA).
          
            END.
      
      
            IF l-tipo = "" THEN 
            DO:
                CREATE ttFormaDePago.    
                ASSIGN 
                    ttFormaDePago.Tipo           = 4
                    ttFormaDePago.Fecha          = TODAY
                    ttFormaDePago.Almacen        = t-EC.Id-Ubic 
                    WHEN AVAILABLE t-EC
                    ttFormaDePago.EntradaAlmacen = l-entrada
                    ttFormaDePago.Versionn       = FP.Version 
                    ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)    
                    ttFormaDePago.FechaFactura   = FP.FecFac
                    ttFormaDePago.FechaEmbarque  = FP.FecEmb
                    ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)  
                    ttFormaDePago.Bloque         = "DEPOSITAR[ ]    BANCO :   CUENTA:      ,ABA ".  
            END.
            
            */
            l-SinIVA = 0.  
            l-ImpDesc = 0.  
            l-TotFact = 0.  
            l-totAntFact = 0.   



            IF l-print1 THEN 
            DO:
                CREATE ttDescr. 
                ASSIGN 
                    ttDescr.Tipo        = l-CA1
                    ttDescr.Descripcion = l-Des1
                    ttDescr.ImpCIva     = ROUND(l-Imp11,2)  
                    ttDescr.ImpSIva     = ROUND(l-Imp1,2).   
            END.
            IF l-print2 THEN 
            DO:
                CREATE ttDescr. 
                ASSIGN 
                    ttDescr.Tipo        = l-CA2
                    ttDescr.Descripcion = l-Des2
                    ttDescr.ImpCIva     = ROUND(l-Imp22,2)
                    ttDescr.ImpSIva     = ROUND(l-Imp2,2).   
            END.
            IF l-print3 THEN 
            DO:
                CREATE ttDescr. 
                ASSIGN 
                    ttDescr.Tipo        = l-CA3
                    ttDescr.Descripcion = l-Des3
                    ttDescr.ImpCIva     = ROUND(l-Imp33,2)
                    ttDescr.ImpSIva     = ROUND(l-Imp3,2).   
            END.
            IF l-print4 THEN 
            DO:
                CREATE ttDescr. 
                ASSIGN 
                    ttDescr.Tipo        = l-CA4
                    ttDescr.Descripcion = l-Des4
                    ttDescr.ImpCIva     = ROUND(l-Imp44,2)
                    ttDescr.ImpSIva     = ROUND(l-Imp4,2).   
            END.

            l-DesLetra1 = "                       ".
            l-DesLetra2 = "                       ".
            l-DesLetra3 = "                       ".
            l-DesLetra4 = "                       ".


            ASSIGN 
                l-DD1      = 0 
                l-DD2      = 0 
                l-DD3      = 0 
                l-DD4      = 0
                l-DesSIVA5 = 0 
                l-ImpSIVA5 = 0 
                l-NetoP5   = 0.
            IF l-DD1 = 0 THEN l-DesLetra5 = '                       '. /* 23 espacios */

        END. /* End del while para repetir las veces que sea necesario la FP's */
    
    
    
    END.   /* Termina de Imprimir la Segunda de la Forma de Pago */
  
    LOG-MANAGER:WRITE-MESSAGE("/FormaDePago[GET] Tipo M : " + l-FP + " Termina").  
END PROCEDURE.
PROCEDURE "cxpc0044.p":
    /*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : cxpc0044.p
  Funcion  : Factura de Foma de Pago (Reporte)
  Autor    : IOC
  Fecha    : 04-01-1996
  */
    /*
    Los parametros son :
    1) El numero de FORMA DE PAGO que es FP.Id-FP
    2) El tipo de FORMA DE PAGO al Proveedor Prov.FormaPago
       Valores de l-tipo son:
       1 = ENVIAR   2 = VIENE  3 = DEPOSITAR 
    */

    DEFINE INPUT PARAMETER l-FP     LIKE FP.Id-FP        NO-UNDO.
    DEFINE INPUT PARAMETER l-tipo   LIKE Prov.FormaPago  NO-UNDO.
    DEFINE INPUT PARAMETER  IdUsuario AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    
    ASSIGN
        l-hecho1 = TODAY.
    DO TRANSACTION :
        FIND FIRST FP WHERE FP.Id-FP = l-FP EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE (FP)  THEN 
        DO:
            ASSIGN 
                Respuesta = "No existe esta FORMA DE PAGO...".  
            RETURN.   
        END.
    END.  
    
    LOG-MANAGER:WRITE-MESSAGE("/FormaDePago[GET] Tipo F : " + l-FP).    
    FIND Usuario WHERE Usuario.Id-User = IdUsuario NO-LOCK NO-ERROR.
    FIND FIRST Prov OF FP NO-LOCK NO-ERROR .
    FIND FIRST Comprador WHERE Comprador.Id-Comp = Prov.Id-Comp NO-LOCK NO-ERROR.
    FIND FIRST EntFP WHERE EntFP.Id-Fp = FP.Id-FP NO-LOCK NO-ERROR.
    IF AVAILABLE(EntFP) THEN 
    DO:
        FOR EACH EC WHERE EC.Id-EC = EntFP.Id-EC NO-LOCK :
            ASSIGN 
                l-entrada = l-entrada + CAPS(EC.Id-EC) + ", ".
        END.
        IF LENGTH(l-entrada) > 0 THEN
            SUBSTRING(l-entrada,LENGTH(l-entrada) - 1, 2) = CHR(0).
        FIND FIRST EC WHERE EC.Id-EC = EntFP.Id-EC NO-LOCK.
    END. 

    IF l-tipo = "1" THEN 
    DO:
        ASSIGN 
            l-letra1 = (IF Prov.Atraves = 1 THEN "X" ELSE " ")
            l-letra2 = (IF Prov.Atraves = 2 THEN "X" ELSE " ").
            
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 1
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = EC.Id-Ubic 
            WHEN AVAILABLE EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + "" +  STRING(FP.Benef)     
            ttFormaDePago.Bloque         = "ENVIAR [X] "
                                             + "CORREO[" + l-letra1 + "]"
                                             + " AUTOBUS [" + l-letra2 + "]"
                                             + " MENSAJERIA : " + Prov.Mensaj.                   
    END.
    IF  l-tipo = "2" THEN 
    DO:
        ASSIGN 
            l-letra3 = (IF Prov.Llamar THEN "X" ELSE " ")
            l-letra4 = (IF Prov.Llamar THEN " " ELSE "X").
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 2
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = EC.Id-Ubic 
            WHEN AVAILABLE EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)          
            ttFormaDePago.Bloque         = "VIENE[X] " + "LLAMAR SI[" + l-letra3 + "] "
                                                            + "NO[" + l-letra4 + "] "
                                                            + "TELEFONO: " + STRING(Prov.LlamarTel) + " "  
                                                            + "SR(ITA): " + STRING(Prov.LlamarCon).                      
    END.   
    IF  l-tipo ="3" THEN 
    DO:
        
        FIND FIRST Banco WHERE Banco.Id-Banco = Prov.Id-Banco NO-LOCK NO-ERROR.
        CREATE ttFormaDePago.    
        ASSIGN 
            ttFormaDePago.Tipo           = 3
            ttFormaDePago.Fecha          = TODAY
            ttFormaDePago.Almacen        = EC.Id-Ubic 
            WHEN AVAILABLE EC
            ttFormaDePago.EntradaAlmacen = l-entrada
            ttFormaDePago.Versionn       = FP.Version 
            ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
            ttFormaDePago.FechaFactura   = FP.FecFac
            ttFormaDePago.FechaEmbarque  = FP.FecEmb
            ttFormaDePago.Proveedor      = STRING(FP.Id-Prov) + " " +  STRING(FP.Benef)  
            ttFormaDePago.Bloque         = "DEPOSITAR[X] " + " BANCO : " + STRING(Banco.Id-Banco) + " " + STRING(Banco.Nombre) + " CUENTA: " + STRING(Prov.Cuenta) + ", ABA " + STRING(Prov.ABA).  
        
    END.
    IF l-tipo <> "" THEN 
    DO:
        CREATE ttDescr. 
        ASSIGN 
            ttDescr.Tipo        = l-CA1
            ttDescr.Descripcion = "ANTICIPO O.C. " +  STRING(FP.Id-OC) + " " + STRING(FP.ConceptoAnt) 
            ttDescr.ImpCIva     = FP.Importe.   
            
        CREATE ttPie. /* forma */
        ASSIGN 
            ttPie.Obs       = ""
            ttPie.Capturo   = Usuario.Nom-Usuario 
            WHEN AVAILABLE Usuario
            ttPie.Verifico  = ""
            ttPie.Comprador = Comprador.Nombre 
            WHEN AVAILABLE Comprador
            ttPie.Fecha     = l-hecho1.    
    END.   
    
    DO TRANSACTION:
        FIND FP WHERE Fp.Id-FP = l-FP EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN 
            FP.Version = FP.Version + 1.
    END.
    RELEASE FP.    
    LOG-MANAGER:WRITE-MESSAGE("/FormaDePago[GET] Tipo M : " + l-FP + " Termina").     
END PROCEDURE.