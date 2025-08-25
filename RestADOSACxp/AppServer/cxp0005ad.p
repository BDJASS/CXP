@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cxp0005ad.p
    Purpose     : BASADO comb0010.p
    URI         : /OrdenDeCompra
    Syntax      : 

    Description : Modulo CXP HU5

    Author(s)   : sis10     
    Created     : 
    Notes       :   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */

/*
  Empresa  : Consultoria en Informatica Ejecutiva
  Programa : comb0121.i
  Funcion  : Manda la rutina para imprimir la orden de compra
  Autor    : RCO
  Fecha    : 09 DIC 96
*/

/* Programa: comc0280.p
   Funcion: Impresion de la Orden de Compra */


   
DEFINE VARIABLE l-nomEnv   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-dirEnv   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-colEnv   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-telEnv   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-faxEnv   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-cpEnv    AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-attEnv   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-nomFac   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-dirFac   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-colFac   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-cpFac    AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE l-rfcFac   AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE l-archivo  AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-seq      AS INTEGER.
DEFINE VARIABLE l-prNeto   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-netos    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-prec     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-espIng   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenPre   AS CHARACTER EXTENT 2 INITIAL ['Si','No'] NO-UNDO
    FORMAT 'X(2)'.
DEFINE VARIABLE l-SMenPre  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenPre  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenDes   AS CHARACTER EXTENT 2 INITIAL ['Lista','C/Descuento'] NO-UNDO.
DEFINE VARIABLE l-SMenDes  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenDes  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenIdi   AS CHARACTER EXTENT 2 INITIAL ['Espanol','Ingles'] NO-UNDO.
DEFINE VARIABLE l-SMenIdi  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenIdi  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenUni   AS CHARACTER EXTENT 2 INITIAL ['UDC','UMI'] NO-UNDO.
DEFINE VARIABLE l-SMenUni  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenUni  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenFra   AS CHARACTER EXTENT 2 INITIAL ['Si','No'] NO-UNDO.
DEFINE VARIABLE l-SMenFra  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenFra  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenRen   AS CHARACTER EXTENT 2 INITIAL ['Uno','Dos'] NO-UNDO.
DEFINE VARIABLE l-SMenRen  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenRen  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-MenCEs   AS CHARACTER EXTENT 2 INITIAL ['No','Si'] NO-UNDO.
DEFINE VARIABLE l-SMenCEs  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMenCEs  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-Mensul   AS CHARACTER EXTENT 2 INITIAL ['Todo','Pendiente'] NO-UNDO.
DEFINE VARIABLE l-SMensul  AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-CMensul  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-Coment4  LIKE OC.Coment3 NO-UNDO.
DEFINE VARIABLE l-cantidad AS DECIMAL   FORMAT ">>>>>>>9.99" NO-UNDO.
DEFINE VARIABLE l-Importe  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-NumPag   AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-TotNac   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TCMN     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotExt   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TCE      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotImp   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-Desc     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-LTipCam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-TipCam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-Fax      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-RID      AS RECID     NO-UNDO.
DEFINE VARIABLE l-descr    AS CHARACTER FORMAT "x(72)" NO-UNDO.
DEFINE VARIABLE l-Compra   LIKE Comprador.Nombre NO-UNDO.
DEFINE VARIABLE l-Plazo    LIKE Plazo.Descr NO-UNDO.
DEFINE VARIABLE l-Attn     LIKE Prov.Attn NO-UNDO.
DEFINE VARIABLE l-NumRen   AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-DesArt   LIKE Articulo.Descr NO-UNDO.
DEFINE VARIABLE l-DColor   LIKE Kolor.Descr NO-UNDO.
DEFINE VARIABLE l-DArtic   LIKE Articulo.Descr NO-UNDO.
DEFINE VARIABLE l-prov     AS INTEGER.
DEFINE VARIABLE l-Opc      AS CHARACTER NO-UNDO EXTENT 2
    INITIAL ['Normal','Con aviso de no surtir'].
DEFINE VARIABLE l-iOpc     AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-lOpc     AS LOGI      NO-UNDO.
DEFINE VARIABLE l-Coment3  LIKE OC.Coment3 NO-UNDO.
DEFINE BUFFER b-Ciudad  FOR Ciudad.
DEFINE BUFFER b-Estado  FOR Estado.
DEFINE BUFFER b-Pais    FOR Pais.
DEFINE BUFFER c-Ciudad  FOR Ciudad.
DEFINE BUFFER c-Estado  FOR Estado.
DEFINE BUFFER c-Pais    FOR Pais.
DEFINE BUFFER b-ArtPres FOR ArtPres.
DEFINE STREAM s-salida.  
DEFINE STREAM s-orden.
DEFINE VARIABLE l-coment    AS CHARACTER FORMAT 'x(30)' NO-UNDO.

DEFINE VARIABLE v-login     AS CHARACTER NO-UNDO.
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-archparam AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-Ciudad    LIKE Ciudad.Nombre NO-UNDO.
DEFINE VARIABLE v-Estado    LIKE estado.nombre NO-UNDO.
DEFINE VARIABLE v-Deleg     LIKE Pedido.delegacion1 NO-UNDO.
   

/* Definición de la temp-table para el encabezado */
DEFINE TEMP-TABLE ttEncabezado NO-UNDO
    FIELD Empresa   AS CHARACTER
    FIELD Titulo    AS CHARACTER
    FIELD Direccion AS CHARACTER
    FIELD Folio     AS CHARACTER
    FIELD TelFax    AS CHARACTER
    FIELD Fecha     AS CHARACTER
    FIELD CP        AS CHARACTER
    FIELD Hoja      AS CHARACTER
    FIELD Proveedor AS CHARACTER
    FIELD ProvDir1  AS CHARACTER
    FIELD ProvDir2  AS CHARACTER
    FIELD ProvTel   AS CHARACTER
    FIELD ProvAttn  AS CHARACTER
    FIELD FacturarA AS CHARACTER
    FIELD EnviarA   AS CHARACTER
    FIELD DirFact   AS CHARACTER
    FIELD DirEnv    AS CHARACTER
    FIELD ColFact   AS CHARACTER
    FIELD ColEnv    AS CHARACTER
    FIELD CpFact    AS CHARACTER
    FIELD CpEnv     AS CHARACTER
    FIELD RfcFact   AS CHARACTER
    FIELD TelEnv    AS CHARACTER
    FIELD AttnEnv   AS CHARACTER
    .

/* Definición de la temp-table para la parte logística del encabezado */
DEFINE TEMP-TABLE ttEncabezado2 NO-UNDO
    FIELD Flete        AS CHARACTER
    FIELD TipoFlete    AS CHARACTER
    FIELD FechaSurt    AS CHARACTER
    FIELD Transporte   AS CHARACTER
    FIELD TerminosPago AS CHARACTER
    FIELD Precios      AS CHARACTER
    FIELD Moneda       AS CHARACTER
    FIELD TipoCambio   AS CHARACTER
    FIELD Observacion1 AS CHARACTER
    FIELD TipoOC       AS CHARACTER
    FIELD Observacion2 AS CHARACTER
    FIELD Observacion3 AS CHARACTER
    FIELD Observacion4 AS CHARACTER
    .




/* Definición de la temp-table para el detalle de la orden */
DEFINE TEMP-TABLE ttDetalleOC NO-UNDO
    FIELD Num         AS INTEGER   LABEL "No."
    FIELD Cantidad    AS DECIMAL   LABEL "Cantidad"
    FIELD SinCargo    AS DECIMAL   LABEL "Sin Cargo"
    FIELD UM          AS CHARACTER LABEL "UM"
    FIELD Descripcion AS CHARACTER LABEL "Descripcion / Color"
    FIELD Moneda      AS CHARACTER LABEL "Moneda"
    FIELD PrecioUnit  AS DECIMAL   LABEL "Precio Unitario"
    FIELD Importe     AS DECIMAL   LABEL "Importe"
    FIELD IndicaPre   LIKE DetOC.IndicaPre
    .

/* Definición de la temp-table para la nota de impresión */
DEFINE TEMP-TABLE ttNotaImp NO-UNDO
    FIELD Nota AS CHARACTER
    .

/* Definición de la temp-table para el pie de la orden */
DEFINE TEMP-TABLE ttPieOrden NO-UNDO
    FIELD Partidas      AS INTEGER
    FIELD TotalMN       AS DECIMAL
    FIELD CargoEspMN    AS DECIMAL
    FIELD TotalME       AS DECIMAL  
    FIELD CargoEspME    AS DECIMAL
    FIELD Nota          AS CHARACTER
    FIELD ElaboradaPor  AS CHARACTER
    FIELD AgenteCompras AS CHARACTER
    FIELD Firma         AS CHARACTER
    .

/* Relacionar detalle con encabezado */
DEFINE DATASET dsOrdenCompra FOR
    ttEncabezado,
    ttEncabezado2,
    ttNotaImp,  
    ttDetalleOC,
    ttPieOrden.    
      
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE OrdenDeCompra:      

    DEFINE INPUT PARAMETER l-oc LIKE OC.Id-OC NO-UNDO.
    DEFINE INPUT PARAMETER  IdUsuario AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsOrdenCompra.


    FIND OC WHERE OC.Id-OC = l-oc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE OC THEN 
    DO:
        ASSIGN 
            IdError   = TRUE
            Respuesta = "No Existe OC".
        RETURN.
    END.
    IF oc.enviara = 2 THEN
        ASSIGN l-Coment = 'REMISION SIN PRECIO AL CLIENTE'.
    ELSE
        ASSIGN l-Coment = 'FACTURA'.
    FIND Prov OF OC NO-LOCK.
    ASSIGN 
        l-prov = Prov.Id-Prov.
    ASSIGN 
        l-Fax = Prov.Fax.

    ASSIGN 
        l-SMenPre = 1
        l-SMenDes = 1
        l-SMenIdi = (IF Prov.Id-Moneda = 1 THEN 1 ELSE 2)
        l-SMenUni = 1
        l-SMenFra = 2
        l-SMenRen = 1
        l-SMenCEs = 1
        l-SMensul = 1
        l-Attn    = Prov.Attn
        l-espIng  = TRUE.

    /* ------------------------------------------ */


    ASSIGN     
        l-Prec    = IF l-SMenPre = 1 THEN TRUE ELSE FALSE
        l-Netos   = IF l-SMenDes = 1 THEN FALSE ELSE TRUE
        l-EspIng  = IF l-SMenIdi = 1 THEN TRUE ELSE FALSE
        l-Coment3 = IF l-iOpc = 1 THEN OC.Coment3
                       ELSE '* * * COPIA DE OC, NO SURTIR * * *'
        l-coment4 = IF l-SMensul = 1 THEN ""
                        ELSE '* * *SEGUIMIENTO A ORDEN DE COMPRA * * *'.
    
                        

    FIND Usuario WHERE Usuario.Id-User = OC.Id-Elabora NO-LOCK NO-ERROR.
    FIND Ciudad OF Prov NO-LOCK NO-ERROR.
    FIND Estado OF Prov NO-LOCK NO-ERROR.
    FIND Pais OF Prov NO-LOCK NO-ERROR.
    FIND TipoFlete OF OC NO-LOCK NO-ERROR.
    FIND Plazo OF OC NO-LOCK NO-ERROR.
    FIND Comprador OF OC NO-LOCK NO-ERROR.
    FIND Transporte OF OC NO-LOCK NO-ERROR.

    IF OC.EnviarA = 1 THEN 
    DO:
        FIND Almacen WHERE Almacen.Id-Alm = OC.Id-Enviar NO-LOCK NO-ERROR.
        IF AVAILABLE Almacen THEN 
        DO:
            FIND b-Ciudad OF Almacen NO-LOCK NO-ERROR.
            FIND b-Estado OF Almacen NO-LOCK NO-ERROR.
            FIND b-Pais OF Almacen NO-LOCK NO-ERROR.
            ASSIGN
                l-nomEnv = Almacen.Refer
                l-dirEnv = Almacen.CalleNo
                l-colEnv = Almacen.Colonia
                l-telEnv = Almacen.Tel1
                l-faxEnv = Almacen.Fax
                l-cpEnv  = Almacen.CP
                l-attEnv = Almacen.Resp.
        END.
    END.
    ELSE IF OC.EnviarA = 2 THEN 
        DO:
            FIND Cliente WHERE Cliente.Id-Cliente = INT(OC.Id-Enviar) NO-LOCK NO-ERROR.
            FIND FIRST adosa.Pedido WHERE adosa.Pedido.Id-Pedido = adosa.OC.Id-Pedido NO-LOCK NO-ERROR.
            IF AVAILABLE adosa.Pedido AND adosa.Pedido.CalleNo1 <> '' THEN 
            DO: /* Pone la direccion de embarque del pedido */
                ASSIGN
                    l-nomEnv = adosa.Pedido.RazonSocial
                    l-dirEnv = adosa.Pedido.CalleNo1
                    l-colEnv = adosa.Pedido.Colonia1 
                    l-telEnv = adosa.Pedido.Tel1
                    l-faxEnv = adosa.Pedido.Fax1
                    l-cpEnv  = adosa.Pedido.CP1
                    l-attEnv = adosa.Pedido.Attn
                    v-Ciudad = adosa.Pedido.Ciudad1
                    v-Estado = adosa.Pedido.estado1  
                    v-Deleg  = IF adosa.pedido.delegacion1 <> '' THEN 'DELEG: ' + adosa.Pedido.Delegacion1 ELSE ''.
                IF v-deleg <> '' THEN
                    ASSIGN l-colenv = l-colenv + ' ' + v-deleg.
            END.
            ELSE 
            DO: /* Pone la direccion del cliente */
                FIND b-Ciudad WHERE b-Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
                IF AVAILABLE b-Ciudad THEN
                    FIND b-Estado WHERE b-Estado.Id-Estado = b-Ciudad.Id-Estado NO-LOCK NO-ERROR.
                IF AVAILABLE b-Estado THEN
                    FIND b-Pais WHERE b-Pais.Id-Pais = b-Estado.Id-Pais NO-LOCK NO-ERROR.
                ASSIGN
                    l-nomEnv = Cliente.RazonSocial
                    l-dirEnv = Cliente.CalleNo  
                    l-colEnv = Cliente.Colonia
                    l-telEnv = Cliente.Tel1
                    l-faxEnv = Cliente.Fax
                    l-cpEnv  = Cliente.CP
                    l-attEnv = Cliente.Propietario.
            END.
        END.
        ELSE 
        DO:
            FIND AgAduanal WHERE AgAduanal.Id-AgAd = OC.Id-Enviar NO-LOCK NO-ERROR.
            IF AVAILABLE AgAduanal THEN 
            DO:
                FIND b-Ciudad OF AgAduanal NO-LOCK NO-ERROR.
                FIND b-Estado OF AgAduanal NO-LOCK NO-ERROR.
                IF AVAILABLE b-Estado THEN FIND b-Pais OF b-Estado NO-LOCK NO-ERROR.
                ASSIGN
                    l-nomEnv = AgAduanal.Nombre
                    l-dirEnv = AgAduanal.CalleNo
                    l-colEnv = AgAduanal.Colonia
                    l-telEnv = AgAduanal.Tel1
                    l-faxEnv = AgAduanal.Fax
                    l-cpEnv  = AgAduanal.CP
                    l-attEnv = AgAduanal.Attn.
            END.
        END.
    IF OC.FacturarA = 0 THEN 
    DO:
        FIND c-Ciudad WHERE c-Ciudad.Id-Ciudad = 118 NO-LOCK NO-ERROR.
        FIND c-Estado WHERE c-Estado.Id-Estado =  "019" NO-LOCK NO-ERROR.
        FIND c-Pais WHERE c-Pais.Id-Pais = "MEX" NO-LOCK NO-ERROR.
        ASSIGN
            l-nomFac = "ABASTECEDORA DE OFICINAS, S.A. DE C.V."
            l-dirFac = "ZARAGOZA NORTE # 435"
            l-colFac = "COL. MONTERREY CENTRO"
            l-CPFac  = "64000"
            l-rfcFac = "AOF-870529-IU7".
    END.
    ELSE 
    DO:
        FIND Cliente WHERE Cliente.Id-Cliente = INT(OC.Id-Facturar) NO-LOCK NO-ERROR.
        FIND c-Ciudad WHERE c-Ciudad.Id-Ciudad = Cliente.Id-Ciudad NO-LOCK NO-ERROR.
        IF AVAILABLE c-Ciudad THEN
            FIND c-Estado WHERE c-Estado.Id-Estado = Ciudad.Id-Estado NO-LOCK NO-ERROR.
        IF AVAILABLE c-Estado THEN
            FIND c-Pais WHERE c-Pais.Id-Pais = Estado.Id-Pais NO-LOCK NO-ERROR.
        ASSIGN
            l-nomFac = Cliente.RazonSocial
            l-dirFac = Cliente.CalleNo
            l-colFac = "COL. " + TRIM(Cliente.Colonia)
            l-cpFac  = Cliente.CP
            l-rfcFac = Cliente.RFC.
    END.

    /* Establecer el numero de paginas */
    FOR EACH DetOC OF OC WHERE NOT DetOC.Cancelado NO-LOCK:
        ACCUMULATE DetOC.Id-OC (COUNT).
    END.

    ASSIGN  
        l-NumRen = IF l-SMenFra = 1 THEN
                    (IF l-SMenRen = 1 THEN 2 ELSE 3)
                   ELSE
                    (IF l-SMenRen = 1 THEN 1 ELSE 2)
        l-NumRen = l-NumRen + (IF l-SMenCEs = 2 THEN 1 ELSE 0)
        l-NumPag = INTEGER(TRUNCATE(((ACCUM COUNT DetOC.Id-OC) * l-NumRen) / 23,
                   0)).
    l-NumPag = IF ((ACCUM COUNT DetOC.Id-OC) * l-NumRen) < 23 THEN 1
    ELSE IF ((ACCUM COUNT DetOC.Id-OC) * l-NumRen)
        MODULO 23 > 0 THEN l-NumPag + 1 ELSE l-NumPag.
    /* Fin de establecer el numero de paginas */
    ASSIGN 
        l-lTipCam = IF OC.TipCam <> 0 THEN (IF l-EspIng THEN 'TIPO DE CAMBIO:'
                   ELSE "EXCHANGE RATE   :") ELSE ""
        l-TipCam  = IF OC.TipCam <> 0 THEN STRING(OC.TipCam,'ZZ9.9999') ELSE ""
        l-Compra  = IF AVAILABLE Comprador THEN Comprador.Nombre ELSE ""
        l-Plazo   = IF AVAILABLE Plazo THEN Plazo.Descr ELSE "".
       
    IF l-Plazo = ? THEN 
        ASSIGN l-Plazo = "".

    FORM HEADER
        CHR(15) FORMAT 'x'
        SKIP
        "Partidas:"            (l-Seq - 1) FORMAT '999'
        "Total MN:"            l-TotNac    FORMAT '>,>>>,>>>,>>9.99' 
        "Cargo Especial M.N.:" l-TCMN      FORMAT '>,>>>,>>9.99'
        "Total M.E.:"          l-TotExt    FORMAT '>,>>>,>>9.99'
        "Cargo Especial M.E.:" l-TCE       FORMAT '>,>>>,>>9.99'
        FILL('-',137) FORMAT 'X(137)'
        "NOTA: LA PRESENTE ORDEN INVALIDA CUALQUIER ORDEN VERBAL. NO DUPLICAR, Y HACER REFERENCIA A ESTA ORDEN DE COMPRA AL FACTURAR." SKIP
        "Elaborada por :" Usuario.Nom-Usuario
        "Agente de Compras :" AT 50 CHR(18) + l-Compra + Chr(15) FORMAT 'X(39)' "Firma:"
        CHR(18) FORMAT 'X(3)'
        WITH FRAME f-Pie NO-BOX PAGE-BOTTOM WIDTH 138.





    CREATE ttEncabezado.
    ASSIGN
        ttEncabezado.Empresa   = "Abastecedora de Oficinas S.A. de C.V."
        ttEncabezado.Titulo    = "ORDEN DE COMPRA"
        ttEncabezado.Direccion = "Zaragoza Norte # 435, Col. Monterrey Centro"
        ttEncabezado.Folio     = STRING(l-OC,"999999")
        ttEncabezado.TelFax    = "Tel.: (81)8158-1500 Fax (81)8158-1560"
        ttEncabezado.Fecha     = STRING(OC.FecReg)
        ttEncabezado.CP        = "C.P. 64000 - Monterrey, N.L. Mexico"
        ttEncabezado.Hoja      = STRING(l-NumPag,'>>9')
        ttEncabezado.Proveedor = Prov.Nombre
        ttEncabezado.ProvDir1  = TRIM(Prov.CalleNo) + " " + TRIM(Prov.Colonia)
        ttEncabezado.ProvDir2  = "C.P. " + Prov.CP + " " +
                             (IF AVAILABLE Ciudad THEN TRIM(Ciudad.Nombre) ELSE "") + ", " +
                             (IF AVAILABLE Estado THEN TRIM(Estado.Nombre) ELSE "") + ", " +
                             (IF AVAILABLE Pais   THEN TRIM(Pais.Nombre)   ELSE "")
        ttEncabezado.ProvTel   = "Tels.: " + TRIM(Prov.Tel1) + ", " +
                             TRIM(Prov.Tel2) + " Fax: " + TRIM(Prov.Fax)
        ttEncabezado.ProvAttn  = TRIM(l-Attn)
        ttEncabezado.FacturarA = l-nomFac
        ttEncabezado.EnviarA   = l-nomEnv
        ttEncabezado.DirFact   = l-dirFac
        ttEncabezado.DirEnv    = l-dirEnv
        ttEncabezado.ColFact   = l-ColFac
        ttEncabezado.ColEnv    = l-ColEnv
        ttEncabezado.CpFact    = l-cpFac
        ttEncabezado.CpEnv     = l-cpEnv
        ttEncabezado.RfcFact   = l-rfcFac
        ttEncabezado.TelEnv    = l-telEnv + " Fax: " + l-faxEnv
        ttEncabezado.AttnEnv   = l-attEnv
        .


    CREATE ttEncabezado2.
    ASSIGN
        ttEncabezado2.Flete        = "FLETE"
        ttEncabezado2.TipoFlete    = TipoFlete.Descr
        ttEncabezado2.FechaSurt    = STRING(OC.FecSurt, "99/99/9999")
        ttEncabezado2.Transporte   = Transporte.Nombre
        ttEncabezado2.TerminosPago = IF OC.FecPag = ? 
                                  THEN l-Plazo 
                                  ELSE STRING(OC.FecPag,"99/99/9999")
        ttEncabezado2.Precios      = IF l-SMenPre = 2 THEN "" 
                             ELSE IF l-SMenDes = 1 THEN "LISTA"
                             ELSE "C/DESCUENTO"
        ttEncabezado2.Moneda       = l-LTipCam
        ttEncabezado2.TipoCambio   = STRING(l-TipCam)
        ttEncabezado2.Observacion1 = OC.Coment1
        ttEncabezado2.TipoOC       = (IF OC.Tipo = "1" THEN "NORMAL"
                             ELSE IF OC.Tipo = "2" THEN "PROGRAMADO"
                             ELSE IF OC.Tipo = "3" THEN "ESPECIAL"
                             ELSE IF OC.Tipo = "4" THEN "T. ESCOLAR"
                             ELSE IF OC.Tipo = "5" THEN "T. NAVIDAD"
                             ELSE IF OC.Tipo = "6" THEN "T. OFICINA"
                             ELSE "")
        ttEncabezado2.Observacion2 = OC.Coment2
        ttEncabezado2.Observacion3 = l-Coment3
        ttEncabezado2.Observacion4 = l-Coment4
        .


    FORM HEADER
        CHR(15) + CHR(14) + CHR(27) + "-1" + "** FAVOR DE ENVIAR LA " + TRIM(l-coment) + " EN SOBRE ANEXO A LA GUIA **" + CHR(27) + "-0" FORMAT 'X(100)'
        WITH FRAME f-NotaImp OVERLAY NO-BOX NO-LABELS WIDTH 134 PAGE-TOP.


    CREATE ttNotaImp.
    ASSIGN
        ttNotaImp.Nota = "** FAVOR DE ENVIAR LA "
                  + TRIM(l-coment)
                  + " EN SOBRE ANEXO A LA GUIA **"
        .


    FORM HEADER
        CHR(15) + CHR(14) + CHR(27) + "-1" + "**ART. PEND. DE SURTIR AL:" + STRING(TODAY,"99/99/9999") + " COTEJAR CON LO SURTIDO O AVISAR EN CASO DE NO PODER SURTIR A:" + usuario.e-mail2 + CHR(27) FORMAT 'X(130)'
        WITH FRAME f-NotaImp2 OVERLAY NO-BOX NO-LABELS WIDTH 134 PAGE-TOP.




    FORM
        l-Seq FORMAT 'ZZ9' COLUMN-LABEL "No."
        DetOC.CantPedPres  COLUMN-LABEL "Cantidad" FORMAT ">>>>,>>9.99"
        DetOC.CantSC       COLUMN-LABEL 'Sin    ! Cargo   ' FORMAT '>>>,>>9.99'
        ArtPres.Descr      COLUMN-LABEL "UM" FORMAT "X(9)"
        l-descr            COLUMN-LABEL "Descripcion                                            Color         "
        Moneda.Simbolo     COLUMN-LABEL '   '
        DetOC.PrecUnit     COLUMN-LABEL "Precio!Unitario" FORMAT ">>>>9.99"
        l-Importe          COLUMN-LABEL "Importe" FORMAT ">>>>>>>>>9.99"
        DetOC.IndicaPre    NO-LABELS
        WITH FRAME f-det NO-BOX WIDTH 138 DOWN.


    ASSIGN 
        l-seq    = 1
        l-TotNac = 0
        l-TCMN   = 0
        l-TotExt = 0
        l-TCE    = 0.
     
       
FOR EACH DetOC OF OC WHERE NOT DetOC.Cancelado NO-LOCK:

    /* Calcular el precio unitario (Precio Neto) menos sus descuentos */   
    ASSIGN 
        l-prNeto = DetOC.PrecUnit.

    IF DetOC.IndicaPre <> 'C' THEN 
    DO:
        FOR EACH DescOC WHERE DescOC.Id-OC       = DetOC.Id-OC AND
            DescOC.Id-Articulo = DetOC.Id-Articulo AND
            DescOC.Id-Color    = DetOC.Id-Color NO-LOCK:
            ASSIGN 
                l-prNeto = l-prNeto * (1 - DescOC.Dcto / 100).
        END.
    END.
  
    FIND Articulo OF DetOC NO-LOCK NO-ERROR.
    
    IF AVAILABLE Articulo THEN 
    DO:
        FIND Marca OF Articulo NO-LOCK NO-ERROR.
        ASSIGN 
            l-DesArt                = MINIMUM(Articulo.generico," ") + Articulo.generico    +
                      (IF AVAILABLE Marca THEN
                      MINIMUM(Marca.Descr      ," ") + Marca.Descr ELSE "") +
                      MINIMUM(Articulo.Modelo  ," ") + Articulo.Modelo      +
                      MINIMUM(Articulo.Medidas ," ") + Articulo.Medidas     +
                      MINIMUM(Articulo.Peso    ," ") + Articulo.Peso        +
                      MINIMUM(Articulo.Resto   ," ") + Articulo.Resto
            SUBSTRING(l-DesArt,1,1) = ""
            l-DArtic                = (IF l-EspIng THEN 
                            l-DesArt 
                        ELSE 
                            (IF LENGTH(TRIM(Articulo.Name)) > 0 THEN 
                                Articulo.Name 
                            ELSE l-DesArt)).
    END.
    ELSE 
        ASSIGN 
            l-DesArt = ""
            l-DArtic = "".

    FIND ArtProv WHERE ArtProv.Id-Articulo = Articulo.Id-Articulo AND
        ArtProv.Id-Prov = Articulo.Id-Prov 
        NO-LOCK NO-ERROR.
  
    /* Unidad Minima de Inventario (UMI) */
    FIND b-ArtPres WHERE b-ArtPres.Id-Articulo = Articulo.Id-Articulo AND
        b-ArtPres.Id-Pres     = Articulo.Id-UMI 
        NO-LOCK NO-ERROR.
  
    /* Unidad de Compra (UDC) */
    FIND ArtPres WHERE ArtPres.Id-Articulo = DetOC.Id-Articulo AND
        ArtPres.Id-Pres = DetOC.Id-Pres 
        NO-LOCK NO-ERROR.
    FIND Kolor WHERE Kolor.Id-Color = DetOC.Id-Color 
        NO-LOCK NO-ERROR.

    IF AVAILABLE Kolor AND DetOC.Id-Color > 0 THEN 
        ASSIGN 
            l-DColor = (IF l-EspIng THEN 
                            TRIM(Kolor.Descr) 
                        ELSE 
                            (IF LENGTH(TRIM(Kolor.Ext-Desc)) > 0 THEN 
                                TRIM(Kolor.Ext-Desc) 
                            ELSE TRIM(Kolor.Descr))).
    ELSE 
        ASSIGN l-DColor = "".

    /* Regresa el total de la partida sin descuentos y el descuento aparte */
    RUN /usr2/adosa/procs/coma0116.p (DetOC.Id-OC,
        DetOC.Id-Articulo,
        DetOC.Id-Color,
        INPUT-OUTPUT l-TotImp,
        INPUT-OUTPUT l-Desc).

    FIND Moneda OF DetOC NO-LOCK NO-ERROR.
  
    ASSIGN 
        l-descr = (IF LENGTH(TRIM(l-DColor)) > 0 THEN 
                        (STRING(SUBSTRING(l-DArtic,1,54),'x(54)') + " " + STRING(l-DColor,'x(14)')) 
                   ELSE 
                       STRING(l-DArtic,'x(72)')).


    
    IF l-espIng THEN 
    DO:
    
        CREATE ttDetalleOC.
           
        ASSIGN 
            ttDetalleOC.Num         = l-seq
            ttDetalleOC.Cantidad    = DetOC.CantPedPres
            ttDetalleOC.SinCargo    = (IF DetOC.CantSC <> 0 
                                         THEN DetOC.CantSC / 
                                        (IF l-SMenUni = 2  THEN 1 
                                         ELSE IF AVAILABLE ArtPres 
                                              THEN ArtPres.Equiv 
                                              ELSE 1)
                                         ELSE 0)
            ttDetalleOC.UM          = (IF l-SMenUni = 2 
                                        THEN b-ArtPres.Descr 
                                        ELSE IF AVAILABLE ArtPres 
                                        THEN ArtPres.Descr 
                                        ELSE "")
            ttDetalleOC.Descripcion = l-descr  
            ttDetalleOC.Moneda      = Moneda.Simbolo 
            WHEN l-Prec 
            ttDetalleOC.PrecioUnit  = l-prNeto / (IF l-SMenUni = 2 
                                           THEN (IF AVAILABLE ArtPres 
                                                   THEN ArtPres.Equiv 
                                                   ELSE 1) 
                                           ELSE 1)
            ttDetalleOC.Importe     = (DetOC.PrecUnit / 
                                          (IF l-SMenUni = 2 
                                             THEN (IF AVAILABLE ArtPres 
                                             THEN ArtPres.Equiv 
                                             ELSE 1) 
                                           ELSE 1))
                                         * (IF l-SMenUni = 2 
                                    THEN (IF l-SMenSul = 2 
                                            THEN l-cantidad 
                                            ELSE DetOC.CantPedUMI) 
                                    ELSE (IF l-SMenSul = 2 
                                            THEN l-cantidad 
                                            ELSE DetOC.CantPedPres))
            ttDetalleOC.IndicaPre   = DetOC.IndicaPre.
    END.
    

    IF AVAILABLE Moneda AND l-SMenSul = 1 THEN 
    DO:
        
        IF Moneda.Id-Moneda = 1 THEN 
        DO:
            ASSIGN 
                l-TotNac = l-TotNac + (IF l-Netos THEN l-TotImp ELSE l-TotImp + l-Desc)
                l-TCMN   = l-TCMN + (/* DetOC.CantPedPres * */ DetOC.CargEsp).
        END.
        ELSE 
        DO:
        
            ASSIGN 
                l-TotExt = l-TotExt + (IF l-Netos THEN l-TotImp ELSE l-TotImp + l-Desc)
                l-TCE    = l-TCE + (/* DetOC.CantPedPres * */ DetOC.CargEsp).
        END.
        
    END.


    ASSIGN 
        l-seq = l-seq + 1.

END.
/* Poblamos la temp-table */
CREATE ttPieOrden.
ASSIGN
    ttPieOrden.Partidas      = (l-Seq - 1)
    ttPieOrden.TotalMN       = l-TotNac
    ttPieOrden.CargoEspMN    = l-TCMN
    ttPieOrden.TotalME       = l-TotExt
    ttPieOrden.CargoEspME    = l-TCE
    ttPieOrden.Nota          = "NOTA: LA PRESENTE ORDEN INVALIDA CUALQUIER ORDEN VERBAL. " +
                               "NO DUPLICAR, Y HACER REFERENCIA A ESTA ORDEN DE COMPRA AL FACTURAR."
    ttPieOrden.ElaboradaPor  = Usuario.Nom-Usuario
    ttPieOrden.AgenteCompras = l-Compra
    ttPieOrden.Firma         = ""  /* puedes asignar una firma digital o nombre si aplica */
    .
/* Verificar si hay partidas activas */
FIND FIRST DetOC OF OC WHERE NOT DetOC.Cancelado NO-LOCK NO-ERROR.
IF NOT AVAILABLE DetOC THEN DO:
    ASSIGN 
        IdError   = TRUE
        Respuesta = "CANCELADA".
    RETURN.
END. 

RETURN.
END PROCEDURE.