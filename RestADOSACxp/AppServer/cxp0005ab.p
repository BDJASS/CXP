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


DEF    VAR      l-fp      AS CHAR      FORMAT "x(60)" NO-UNDO.
DEF    VAR      l-impreso AS LOGICAL   FORMAT "Si/No" NO-UNDO.
DEF    VAR      l-prov    LIKE Prov.Id-Prov NO-UNDO.
DEF    VAR      l-finfp   LIKE FP.Id-FP NO-UNDO.
DEF    VAR      l-inifec  LIKE FP.FecReg NO-UNDO.
DEF    VAR      l-finfec  LIKE FP.FecReg NO-UNDO.
DEF    VAR      l-reporte AS CHAR      NO-UNDO.
DEF    VAR      l-yess    AS LOGI      NO-UNDO.
DEF    VAR      l-indice  AS INT       NO-UNDO.
DEFINE VARIABLE l-Def     AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ps      AS CHARACTER NO-UNDO.
DEF    VAR      l-arch2   AS CHAR.
DEF    VAR      v-nombre  AS CHAR.

/* Tabla principal (cabecera) */
DEFINE TEMP-TABLE ttFormaDePago NO-UNDO
    FIELD Folio          LIKE Cheque.NumCheque      
    FIELD OrdenDeCompra  AS CHAR      
    FIELD Fecha          AS DATE 
    FIELD Almacen        AS CHAR        
    FIELD EntradaAlmacen AS CHAR
    FIELD Versionn       LIKE FP.Version          
    FIELD Proveedor      AS CHAR        
    FIELD Factura        AS CHAR                     
    FIELD FechaFactura   AS DATE               
    FIELD FechaEmbarque  AS DATE 
    FIELD Depositar      AS CHAR
    FIELD Correo      AS CHAR
    FIELD Autobus      AS CHAR
    FIELD Mensajeria      AS CHAR
    FIELD Banco          AS CHAR
    FIELD Cuenta         AS INT      
    INDEX idxFolio IS UNIQUE PRIMARY Folio.

/* Tabla detalle */
DEFINE TEMP-TABLE ttDet NO-UNDO
    FIELD Folio       LIKE Cheque.NumCheque   
    FIELD Factura     LIKE DetCheque.NumFac       
    FIELD Buzon       AS CHAR    
    FIELD FechaVenc   AS DATE 
    FIELD ImpOriginal AS DECIMAL FORMAT "zzzzzz9"       
    FIELD PP1         AS DECIMAL FORMAT ">9.99"
    FIELD PP2         AS DECIMAL FORMAT ">9.99"
    FIELD PP3         AS DECIMAL FORMAT ">9.99"
    FIELD PP4         AS DECIMAL FORMAT ">9.99"
    FIELD ImpPagado   AS DECIMAL FORMAT "zzzzzz9"
    INDEX idxFolio IS PRIMARY Folio
    INDEX idxDetalle          Folio Factura.
    
    
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

DEFINE DATASET dsForma
    FOR ttFormaDePago, ttDet,ttAutDet, ttEntCompra
    DATA-RELATION drAut FOR ttAut, ttDet
    RELATION-FIELDS(Folio, Folio)
    //NESTED
    DATA-RELATION drAut2 FOR ttDet, ttAutDet
    RELATION-FIELDS(Folio, Folio)
    //NESTED  
    DATA-RELATION drDetEnt FOR ttAutDet, ttEntCompra
    RELATION-FIELDS(EntradaC, EntradaC)
    NESTED.  

DEF            VAR l-entrada    AS CHAR      FORMAT "x(30)" NO-UNDO.
DEF            VAR l-ordenes    AS CHAR      FORMAT "x(30)" NO-UNDO.

DEF            VAR l-Mensaj     AS CHAR      NO-UNDO.
DEF            VAR l-reporte    AS CHAR      NO-UNDO.
DEF            VAR l-DD1        AS DECI      NO-UNDO.
DEF            VAR l-DD2        AS DECI      NO-UNDO.
DEF            VAR l-DD3        AS DECI      NO-UNDO.
DEF            VAR l-DD4        AS DECI      NO-UNDO.
DEF            VAR l-hecho      AS DATE      FORMAT 99/99/9999 NO-UNDO.
DEF            VAR l-hecho1     AS DATE      FORMAT 99/99/9999 NO-UNDO.
DEF            VAR l-usuario    LIKE Usuario.Id-User NO-UNDO.
DEF            VAR l-CA1        AS CHAR      FORMAT "x" NO-UNDO.
DEF            VAR l-CA2        AS CHAR      FORMAT "x" NO-UNDO.
DEF            VAR l-CA3        AS CHAR      FORMAT "x" NO-UNDO.
DEF            VAR l-CA4        AS CHAR      FORMAT "x" NO-UNDO.
DEF            VAR l-Des1       AS CHAR      FORMAT "x(38)" NO-UNDO.
DEF            VAR l-Des2       AS CHAR      FORMAT "x(38)" NO-UNDO.
DEF            VAR l-Des3       AS CHAR      FORMAT "x(38)" NO-UNDO.
DEF            VAR l-Des4       AS CHAR      FORMAT "x(38)" NO-UNDO.
DEF            VAR l-Imp1       AS DECI      FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp2       AS DECI      FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp3       AS DECI      FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp4       AS DECI      FORMAT "Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp11      AS DECI      FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp22      AS DECI      FORMAT "-z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp33      AS DECI      FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-Imp44      AS DECI      FORMAT "-Z,ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-SinIVA     AS DECI      FORMAT "-ZZZZZZZ9.99" NO-UNDO.
DEF            VAR l-ImpDesc    AS DECI      FORMAT "-ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-TotFact    AS DECI      FORMAT "-ZZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-ImpRet     AS DECIMAL   NO-UNDO.
DEF            VAR l-tipocambio AS DECI.
DEF            VAR l-TotAntFact LIKE l-TotFact NO-UNDO.
DEF            VAR l-precunit   AS DECI.
DEF            VAR l-Difunit    AS DECI.

DEF            VAR l-dias1      AS INTE      FORMAT "-Z9" NO-UNDO.
DEF            VAR l-dias2      AS INTE      FORMAT "-Z9" NO-UNDO.
DEF            VAR l-dias3      AS INTE      FORMAT "-Z9" NO-UNDO.
DEF            VAR l-dias4      AS INTE      FORMAT "-Z9" NO-UNDO.
DEF            VAR l-dias5      AS INTE      FORMAT "-Z9" NO-UNDO.
DEF            VAR l-Fecha1     AS DATE      NO-UNDO.
DEF            VAR l-Fecha2     AS DATE      NO-UNDO.
DEF            VAR l-Fecha3     AS DATE      NO-UNDO.
DEF            VAR l-Fecha4     AS DATE      NO-UNDO.
DEF            VAR l-Fecha5     AS DATE      NO-UNDO.

DEF            VAR l-DDesc1     AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc2     AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc3     AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc4     AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc5     AS DECI      FORMAT "Z9.99" NO-UNDO.

DEF            VAR l-DDesc11    AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc22    AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc33    AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc44    AS DECI      FORMAT "Z9.99" NO-UNDO.
DEF            VAR l-DDesc55    AS DECI      FORMAT "Z9.99" NO-UNDO.

DEF            VAR l-DDesc111   AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc222   AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc333   AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc444   AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc555   AS DECI      FORMAT "Z9.9" NO-UNDO.

DEF            VAR l-DDesc1111  AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc2222  AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc3333  AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc4444  AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc5555  AS DECI      FORMAT "Z9.9" NO-UNDO.

DEF            VAR l-DDesc11111 AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc22222 AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc33333 AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc44444 AS DECI      FORMAT "Z9.9" NO-UNDO.
DEF            VAR l-DDesc55555 AS DECI      FORMAT "Z9.9" NO-UNDO.

DEF            VAR l-DesSIVA1   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-DesSIVA2   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-DesSIVA3   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-DesSIVA4   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-DesSIVA5   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-ImpSIVA1   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-ImpSIVA2   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-ImpSIVA3   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-ImpSIVA4   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.
DEF            VAR l-ImpSIVA5   AS DECI      FORMAT "ZZZ,ZZ9.99" NO-UNDO.

DEF            VAR l-NetoP1     AS DECI      FORMAT "ZZZzZZ9.99" NO-UNDO.
DEF            VAR l-NetoP2     AS DECI      FORMAT "ZZZzZZ9.99" NO-UNDO.
DEF            VAR l-NetoP3     AS DECI      FORMAT "ZZZzZZ9.99" NO-UNDO.
DEF            VAR l-NetoP4     AS DECI      FORMAT "ZZZzZZ9.99" NO-UNDO.
DEF            VAR l-NetoP5     AS DECI      FORMAT "ZZZZZZZ9.99" NO-UNDO.
DEF            VAR l-notascargo AS DECI      NO-UNDO.

DEF            VAR l-Entro1     AS LOGICAL   NO-UNDO INITIAL TRUE.
DEF            VAR l-Entro2     AS LOGICAL   NO-UNDO INITIAL TRUE.
DEF            VAR l-Entro3     AS LOGICAL   NO-UNDO INITIAL TRUE.
DEF            VAR l-Entro4     AS LOGICAL   NO-UNDO INITIAL TRUE.

DEF            VAR l-letra1     AS CHAR      FORMAT "X" NO-UNDO.
DEF            VAR l-letra2     AS CHAR      FORMAT "X" NO-UNDO.
DEF            VAR l-letra3     AS CHAR      FORMAT "X" NO-UNDO.
DEF            VAR l-letra4     AS CHAR      FORMAT "X" NO-UNDO.
DEF            VAR l-letra5     AS CHAR      FORMAT "X" NO-UNDO.

DEF            VAR l-DesLetra1  AS CHAR      FORMAT "X(15)" NO-UNDO.
DEF            VAR l-DesLetra2  AS CHAR      FORMAT "X(15)" NO-UNDO.
DEF            VAR l-DesLetra3  AS CHAR      FORMAT "X(15)" NO-UNDO.
DEF            VAR l-DesLetra4  AS CHAR      FORMAT "X(15)" NO-UNDO.
DEF            VAR l-DesLetra5  AS CHAR      FORMAT "X(15)" NO-UNDO.

DEF            VAR l-print1     AS LOGI      INITIAL FALSE NO-UNDO.
DEF            VAR l-print2     AS LOGI      INITIAL FALSE NO-UNDO.
DEF            VAR l-print3     AS LOGI      INITIAL FALSE NO-UNDO.
DEF            VAR l-print4     AS LOGI      INITIAL FALSE NO-UNDO.
DEF            VAR l-print5     AS LOGI      INITIAL FALSE NO-UNDO.

DEF            VAR l-Ref        LIKE MovProv.Refer NO-UNDO.
DEF            VAR l-FecReg     LIKE MovProv.FecReg NO-UNDO.
DEF            VAR l-FecVen     LIKE MovProv.FecVenc NO-UNDO.
DEF            VAR l-Temp       AS CHAR      NO-UNDO FORMAT 'x(2)'.
DEF            VAR l-Simb       AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb2      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb3      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb4      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb5      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb6      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb7      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb8      AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb9      AS CHAR      NO-UNDO FORMAT 'x(2)'.
DEF            VAR l-Simb10     AS CHAR      NO-UNDO FORMAT 'x(2)'.
DEF            VAR l-Simb11     AS CHAR      NO-UNDO FORMAT 'x(2)'.
DEF            VAR l-Simb12     AS CHAR      NO-UNDO FORMAT 'x(2)'.
DEF            VAR l-Simb13     AS CHAR      NO-UNDO FORMAT 'x(2)'.
DEF            VAR l-Simb14     AS CHAR      NO-UNDO FORMAT 'x(3)'.

DEF            VAR l-Simb15     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb16     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb17     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb18     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb19     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb20     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb21     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb22     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb23     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb24     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb25     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb26     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb27     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-Simb28     AS CHAR      NO-UNDO FORMAT 'x(3)'.
DEF            VAR l-NumAju     AS INT       NO-UNDO.
DEF            VAR l-Cons       LIKE DetFP.SeqDet NO-UNDO.

DEF            VAR l-OtraHoja   AS INTE      NO-UNDO.
DEF            VAR l-NumP       AS INTE      NO-UNDO.
DEF            VAR l-Pag        AS CHAR      NO-UNDO.
DEF            VAR l-Hoja       AS INTE      NO-UNDO.
DEF            VAR l-totec      AS DECI      NO-UNDO.
DEF            VAR l-pedidoesp  AS CHAR      NO-UNDO.
DEF            VAR l-totimp     AS DECI      NO-UNDO.
DEF            VAR l-totdesc    AS DECI      NO-UNDO.
DEF            VAR l-ImpNoSolic AS DECIMAL   NO-UNDO.
DEF            VAR l-SwCanc     AS LOGICAL   NO-UNDO. 
DEF            VAR l-Exceso     LIKE DetEC.CantUMI NO-UNDO.
DEF            VAR l-FecCanc    LIKE CancOC.FecCan NO-UNDO.

DEF            VAR v-totec      LIKE l-totec NO-UNDO.
DEF            VAR v-equiv      LIKE artpres.equiv NO-UNDO.

DEF NEW SHARED VAR l-despp      AS DECI      NO-UNDO.
DEF NEW SHARED VAR v-costoesp   AS CHARACTER NO-UNDO.

DEF BUFFER bf-MovProv FOR MovProv.
DEF BUFFER b-EC       FOR EC.
DEF BUFFER b-DetEC    FOR DetEC.

DEF TEMP-TABLE w-DetOC
    FIELD Id-Articulo LIKE /* ARemoto. */ DetOC.Id-Articulo
    FIELD Id-Color    LIKE /* ARemoto. */ DetOC.Id-Color
    FIELD Cancelado   LIKE /* ARemoto. */ DetOC.Cancelado
    FIELD CantRecUMI  LIKE /* ARemoto. */ DetOC.CantRecUMI
    FIELD CantPedUMI  LIKE /* ARemoto. */ DetOC.CantPedUMI
    INDEX Idx-Def Id-Articulo Id-Color.
    
DEF NEW SHARED TEMP-TABLE t-EC
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



/* **********************  Internal Procedures  *********************** */



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE FormaDePago:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  IdUsuario AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER l-inifp  LIKE FP.Id-FP  NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsForma.

    /* Inicia log */
    LOG-MANAGER:WRITE-MESSAGE("/FormaDePago[GET] Ejecutado por usuario: " + IdUsuario).
    
    FOR EACH FP WHERE FP.Id-FP >= l-inifp AND FP.Id-FP <= l-finfp NO-LOCK:
        FIND Prov OF FP NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Prov) THEN NEXT.
        IF SUBSTRING(FP.Id-FP, 1, 1) = 'M' THEN 
        DO:  
            RUN cxpc0045.p (INPUT FP.Id-FP, INPUT Prov.FormaPago, INPUT 0,INPUT IdUsuario,OUTPUT Respuesta).
            ASSIGN 
                l-ps = "ps".
        END.   
        IF SUBSTRING(FP.Id-FP, 1, 1) = 'F' THEN 
            RUN cxpc0044.p (INPUT FP.Id-FP, INPUT Prov.FormaPago).
        ASSIGN 
            l-yess = TRUE.
    END.
          
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

    DEF INPUT PARAMETER l-FP     LIKE FP.Id-FP        NO-UNDO.
    DEF INPUT PARAMETER l-tipo   LIKE Prov.FormaPago  NO-UNDO.
    DEF INPUT PARAMETER l-ConSis AS INT               NO-UNDO.
    DEFINE INPUT PARAMETER  IdUsuario AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 

    DO TRANSACTION :
        FIND FIRST FP WHERE FP.Id-FP = l-FP EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE (FP)  THEN 
        DO:
            ASSIGN 
                Respuesta = "No existe esta FORMA DE PAGO..."
                IdError   = TRUE.
            RETURN.
        END.
        ASSIGN 
            FP.Impreso = TRUE
            FP.Version = FP.Versio + 1.
    END.  

    RELEASE FP.

    FIND FIRST FP WHERE FP.Id-FP = l-FP NO-LOCK NO-ERROR.
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
    
                                find moneda where moneda.id-moneda = hcosto.id-moneda
                                    no-lock no-error.
                                FIND FIRST tipocambio where tipocambio.id-moneda =
                                    hcosto.id-moneda no-lock no-error.
                                if hcosto.id-moneda = 5 then 
                                    find FIRST tipocambio where tipocambio.id-moneda = 3 
                                        no-lock no-error.
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
    
    DEF VAR l-SumDesc AS DECI NO-UNDO.
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
              ttFormaDePago.Fecha          = TODAY
              ttFormaDePago.Almacen        = t-EC.Id-Ubic WHEN AVAILABLE t-EC
              ttFormaDePago.EntradaAlmacen = l-entrada
              ttFormaDePago.Versionn       = FP.Version 
              ttFormaDePago.Factura        = (IF FP.NumFac <> "" THEN FP.NumFac ELSE FP.NumRem)
              ttFormaDePago.FechaFactura   = FP.FecFac
              ttFormaDePago.FechaEmbarque  = FP.FecEmb
              ttFormaDePago.Proveedor      =  FP.Id-Prov + "" +  FP.Benef
               l-letra1   l-letra2   Prov.Mensaj  
            WITH FRAME f-f1.
            
            
    END.
    
     
END PROCEDURE.