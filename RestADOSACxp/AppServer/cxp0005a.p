@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cxp0005a.p
    Purpose     : BASADO tesa0110.p
    URI         : /AutorizacionPagos
    Syntax      : Autorizacion de Pagos [Transferencias/Proveedores]

    Description : Modulo CXP HU5

    Author(s)   : sis10   
    Created     : Wed Jul 30 11:35:10 CST 2025
    Notes       :   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */

DEF            VAR      l-Benef     AS CHAR    NO-UNDO FORMAT 'x(50)'.
DEF            VAR      l-BusBenef  AS CHAR    NO-UNDO FORMAT 'x(50)'.
DEF            VAR      l-Usuario   LIKE Usuario.Id-User NO-UNDO.    
DEF            VAR      l-TipoChe   LIKE Cheque.Tipo NO-UNDO.
DEF NEW SHARED VAR      s-recid     AS RECID   NO-UNDO.   
DEF NEW SHARED VAR      s-Banco     LIKE Cheque.Id-Banco NO-UNDO.
DEF NEW SHARED VAR      s-CtaCheq   LIKE Cheque.Id-CtaCheq NO-UNDO.
DEF NEW SHARED VAR      s-NumCheque LIKE Cheque.NumCheque NO-UNDO.
DEFINE         VARIABLE l-fvenc     AS DATE    NO-UNDO.
DEFINE         VARIABLE l-base      AS DATE    NO-UNDO.
DEFINE         VARIABLE l-IndicaPP  AS CHAR    NO-UNDO.
DEFINE         VARIABLE l-Buzon     AS CHAR    NO-UNDO.
DEFINE         VARIABLE l-rfc       AS CHAR    NO-UNDO.
DEFINE         VARIABLE l-NumFac    AS CHAR    NO-UNDO.
DEF NEW SHARED VAR      l-Banamex   AS DECIMAL FORMAT "$zz,zzz,zz9.99" LABEL "Banamex" NO-UNDO.
DEF NEW SHARED VAR      l-Santander AS DECIMAL FORMAT "$zz,zzz,zz9.99" LABEL "Santander" NO-UNDO.

DEF            VAR      l-SegCambio AS LOGICAL NO-UNDO.
DEF BUFFER b-Cheque FOR Cheque.

DEF VAR l-totimp     AS DECI      FORMAT "z,zzz,zz9.99" DECIMALS 10 NO-UNDO.
DEF VAR l-DifMult    AS DECI      FORMAT "z,zzz,zz9.99" DECIMALS 10 NO-UNDO.
DEF VAR l-totec      AS DECI      FORMAT "z,zzz,zz9.99" DECIMALS 10 NO-UNDO.
DEF VAR l-totdesc    AS DECI      FORMAT "z,zzz,zz9.99" DECIMALS 10 NO-UNDO.
DEF VAR l-DesPP      AS DECI      FORMAT "z,zzz,zz9.99" DECIMALS 10 NO-UNDO.
DEF VAR l-precunit   AS DECI      FORMAT "zz,zz9.999999" DECIMALS 10 NO-UNDO.
DEF VAR l-DifUnit    AS DECI      FORMAT "zz,zz9.99" DECIMALS 10 NO-UNDO.
DEF VAR l-simbolo    AS CHAR      NO-UNDO.

DEF VAR l-tipocambio AS DECI      DECIMALS 8 NO-UNDO.
DEF VAR l-aste       AS CHAR      NO-UNDO.
DEF VAR v-cancelado  AS LOGICAL   NO-UNDO.
DEF VAR v-artnoenc   AS CHARACTER NO-UNDO.
DEF VAR v-error      AS LOGICAL   INITIAL FALSE NO-UNDO.


DEF BUFFER b-artpres  FOR ArtPres.
DEF BUFFER bf-artpres FOR artpres.


/* Tabla principal (cabecera) */
DEFINE TEMP-TABLE ttAut NO-UNDO
    FIELD Folio        LIKE Cheque.NumCheque      
    FIELD IdBanco      LIKE Banco.Id-Banco         
    FIELD Banco        LIKE Banco.NomCto           
    FIELD Beneficiario LIKE Cheque.Benef          
    FIELD FecReg       LIKE Cheque.FecReg         
    FIELD FecVenc      AS DATE                     
    FIELD PP           AS CHARACTER              
    FIELD Importe      LIKE Cheque.Importe        
    FIELD Aut          LIKE Cheque.Negociable     
    FIELD B            AS CHARACTER 
    FIELD CtaCheque    LIKE Cheque.Id-CtaCheq
    INDEX idxFolio IS UNIQUE PRIMARY Folio.

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
    FOR ttAut, ttAutDet, ttEntCompra
    DATA-RELATION drAut FOR ttAut, ttAutDet
    RELATION-FIELDS(Folio, Folio)
    NESTED
    DATA-RELATION drDetEnt FOR ttAutDet, ttEntCompra
    RELATION-FIELDS(EntradaC, EntradaC)
    NESTED.



DEF VAR l-rpFac  LIKE DetCheque.NumFac NO-UNDO.
DEF VAR l-rpImp1 AS DECIMAL FORMAT "zzzzz,zz9" NO-UNDO.
DEF VAR l-rpImp2 AS DECIMAL FORMAT "zzzzz,zz9" NO-UNDO.
DEF VAR l-rpImp3 AS DECIMAL NO-UNDO.
DEF VAR l-rpImp4 AS DECIMAL NO-UNDO.
DEF VAR l-rpImp5 AS DECIMAL NO-UNDO.
DEF VAR l-rpImp6 AS DECIMAL NO-UNDO.
DEF VAR l-rpImp7 AS DECIMAL NO-UNDO.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE TransfPendAut:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  IdUsuario AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER DATASET FOR dsAut.

    /* Inicia log */
    LOG-MANAGER:WRITE-MESSAGE("/AutorizacionPagos[GET] Ejecutado por usuario: " + IdUsuario).
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
        AND Cheque.Tipo = 1   
        AND Cheque.Estatus = 0   
        NO-LOCK:
        FIND b-Cheque WHERE RECID(b-Cheque) = RECID(Cheque) EXCLUSIVE-LOCK NO-WAIT.
        IF AVAILABLE b-Cheque THEN 
        DO:
            FOR EACH DetCheque WHERE DetCheque.Id-Banco = b-Cheque.Id-Banco
                AND DetCheque.Id-CtaCheq = b-Cheque.Id-CtaCheq
                AND DetCheque.NumCheque = b-Cheque.NumCheque
                EXCLUSIVE-LOCK:
                DELETE DetCheque.
            END.
            DELETE b-Cheque.
        END.
        RELEASE b-Cheque.
    END.
    FOR EACH Cheque WHERE Cheque.FecReg >= 01/01/2014 AND (Cheque.Id-Banco = 1 OR Cheque.Id-Banco = 25) 
        AND Cheque.NumCheque >= 200000 
        AND Cheque.NumCheque < 900000 
        AND Cheque.Tipo = 1   
        AND Cheque.Estatus = 0   
        NO-LOCK:
        FIND b-Cheque WHERE RECID(b-Cheque) = RECID(Cheque)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE b-Cheque THEN 
        DO:
            IF b-Cheque.CasaBolsa MATCHES "*BASE*" THEN
                ASSIGN b-Cheque.Negociable = TRUE.
            ELSE
                ASSIGN b-Cheque.Negociable = FALSE.
    
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
        AND Cheque.Tipo = 1   
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
            ttAut.FecVenc      = l-fvenc
            ttAut.PP           = l-indicaPP
            ttAut.Importe      = Cheque.Importe 
            ttAut.Aut          = Cheque.Negociable 
            ttAut.B            = l-Buzon
            ttAut.CtaCheque    = Cheque.Id-CtaCheq .        
          
        FOR EACH DetCheque OF Cheque NO-LOCK BY DetCheque.NumFac :
            RUN /usr2/adosa/procs/cxpd0045.p(INPUT DetCheque.Refer,
                OUTPUT l-rpfac,
                OUTPUT l-rpimp1,
                OUTPUT l-rpimp2,  
                OUTPUT l-rpimp3,
                OUTPUT l-rpimp4,
                OUTPUT l-rpimp5,
                OUTPUT l-rpimp6,
                OUTPUT l-rpimp7).
            
            FIND FIRST entFP WHERE entFP.Id-Fp = DetCheque.Refer NO-LOCK NO-ERROR.
            
                      
            CREATE ttAutDet.
            ASSIGN 
                ttAutDet.Folio          = Cheque.NumCheque
                ttAutDet.FormaPago      = DetCheque.Refer
                ttAutDet.EntradaC       = IF AVAILABLE entFP THEN entFP.Id-Ec ELSE ""
                ttAutDet.Factura        = l-rpfac
                ttAutDet.TotEntrada     = ROUND(l-rpImp1, 0)
                ttAutDet.TotEntradaCPP  = ROUND(l-rpImp2, 0)
                ttAutDet.NoSol          = l-rpImp3
                ttAutDet.OtroDesc       = l-rpImp5
                ttAutDet.ImporteFactura = ROUND(l-rpImp6, 0)
                ttAutDet.PP             = l-rpImp4
                ttAutDet.TotalPagar     = ROUND(l-rpImp7, 0). 
                
            /* Solo si hay entradaC válida se ejecuta entradaCompra */
            IF AVAILABLE entFP AND entFP.Id-Ec <> "" THEN
                RUN entradaCompra(INPUT entFP.Id-Ec).    
                      
        END.  
            
            
    END.
    LOG-MANAGER:WRITE-MESSAGE("/AutorizacionPagos[GET] Fin por usuario: " + IdUsuario).   
          
END PROCEDURE.

PROCEDURE fecha_vence: 
    
    l-IndicaPP = ''.
    l-Buzon    = ''.
    FIND Prov WHERE Prov.Id-Prov = Cheque.Id-Prov NO-LOCK NO-ERROR.
    l-rfc = CAPS(TRIM(REPLACE(Prov.RFC,' ',''))).        
    l-rfc = CAPS(TRIM(REPLACE(l-rfc,'-',''))).
    l-fvenc = ?.        
    FOR EACH DetCheque WHERE DetCheque.Id-Banco = Cheque.Id-Banco  
        AND DetCheque.Id-CtaCheq = Cheque.Id-CtaCheq 
        AND DetCheque.NumCheque = Cheque.NumCheque NO-LOCK,
        EACH Fp WHERE Fp.Id-Fp = DetCheque.Refer AND Fp.NumFac = DetCheque.NumFac NO-LOCK BREAK BY Fp.FechaVenc DESC:
       
        IF l-fvenc = ? THEN l-fvenc = Fp.FechaVenc.
        IF Prov.Tipo <> 'E' THEN 
        DO:
            l-NumFac = CAPS(TRIM(REPLACE(DetCheque.NumFac,' ',''))).        
            FIND FIRST eDocRecibo WHERE eDocRecibo.Receptor BEGINS l-rfc AND
                TRIM(eDocRecibo.Serie) + TRIM(STRING(eDocRecibo.Folio)) = l-NumFac
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE eDocRecibo THEN l-Buzon = 'N'.
        END.
       
        IF Fp.PlazoPP1 > 0 AND Fp.DctoPP11 > 0 THEN 
        DO:
            IF FP.FecEmb > FP.FecFac THEN
                l-base = FP.FecEmb.
            ELSE
                l-base = FP.FecFac.  

            /*IF DetCheque.Id-Banco = 1 AND DetCheque.NumCheque = 207371 AND USERID("dictdb") = "franc" THEN 
               MESSAGE "antes" skip l-base skip fp.plazopp1 skip l-fvenc view-as alert-box.*/

            IF l-base + FP.PlazoPP1 < l-fVenc THEN
                l-fvenc = l-base + FP.PlazoPP1.
            l-IndicaPP = 'P'.
        /*IF DetCheque.Id-Banco = 1 AND DetCheque.NumCheque = 207371 AND USERID("dictdb") = "franc" THEN 
           MESSAGE "despues" skip l-base skip fp.plazopp1 skip l-fvenc view-as alert-box.*/
        END.
             
        IF Fp.FechaVenc < l-fvenc THEN l-fvenc = Fp.FechaVenc.
    END.     
       
END PROCEDURE.   

PROCEDURE entradaCompra:
    /*------------------------------------------------------------------------
    File        : cxp0005a.p
    Purpose     : BASADO cxpc0048.p
    URI         : 
    Syntax      : Programa para pantalla Valorizacion de la Entrada
                  Detalle Entrada por Compras
    Description : Modulo CXP HU5

    Author(s)   : sis10       
    Created     : Wed Jul 30 11:35:10 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-ec   LIKE ec.id-ec  NO-UNDO.
    FOR EACH EC WHERE EC.Id-EC = ip-ec NO-LOCK :

        FIND FIRST ENTFP WHERE EntFP.Id-EC = EC.Id-EC 
            NO-LOCK NO-ERROR.
        IF AVAILABLE EntFP THEN 
            FIND FP WHERE FP.Id-FP = EntFP.Id-FP 
                NO-LOCK NO-ERROR.
        
        FIND Prov WHERE Prov.Id-Prov = EC.Id-Prov 
            NO-LOCK NO-ERROR.
        
        FIND OC WHERE OC.Id-OC = EC.Id-OC 
            NO-LOCK NO-ERROR.        

        FOR EACH DetEC WHERE DetEC.Id-EC = EC.Id-EC AND
            (DetEC.CantUMI - DetEC.CancUMI) > 0 
            NO-LOCK:
            ASSIGN
                l-PrecUnit  = 0
                l-TotImp    = 0
                v-cancelado = FALSE
                v-error     = FALSE.
        
            FIND FIRST DetOC WHERE DetOC.Id-OC       = EC.Id-OC AND
                DetOC.Id-Articulo = DetEC.Id-Articulo AND
                DetOC.Id-Color    = DetEC.Id-Color 
                NO-LOCK NO-ERROR.

            FIND FIRST ArtPres WHERE ArtPres.Id-Pres     = DetEC.Id-Pres AND
                ArtPres.Id-Articulo = DetEC.Id-ARticulo
                NO-LOCK NO-ERROR.
            ASSIGN 
                l-aste = "".

            IF AVAILABLE DetOC THEN 
            DO:
                ASSIGN
                    l-totimp  = 0
                    l-totdesc = 0.

                /* Calculo de descuentos */
                RUN /usr2/adosa/procs/coma0116.p (INPUT DetOC.Id-OC, 
                    INPUT DetOC.Id-Articulo, 
                    INPUT DetOc.Id-Color,
                    INPUT-OUTPUT l-totimp,
                    INPUT-OUTPUT l-totdesc).
                           
                IF AVAILABLE FP AND 
                    DetOC.Id-Moneda <> FP.Id-Moneda AND FP.Id-Moneda <> 5 THEN 
                    
                    FIND FIRST TipoCambio WHERE TipoCambio.Id-Moneda = DetOC.Id-Moneda 
                        NO-LOCK NO-ERROR.
                ELSE 
                    FIND FIRST TipoCambio WHERE TipoCambio.Id-Moneda = 1 
                        NO-LOCK.
            
                ASSIGN 
                    l-tipocambio = IF AVAILABLE TipoCambio THEN TipoCambio.Importe
                                    ELSE 1.

                IF AVAILABLE FP THEN 
                    FIND Moneda WHERE Moneda.Id-Moneda = FP.Id-Moneda
                        NO-LOCK NO-ERROR.
                ELSE 
                    FIND Moneda WHERE Moneda.Id-Moneda = 1 
                        NO-LOCK NO-ERROR.
            
                ASSIGN
                    l-precunit = (l-totimp / DetOC.CantPedUMI) /** (IF AVAILABLE tipocambio THEN tipocambio.importe ELSE 1)*/ 
                    /*/ (IF AVAILABLE ArtPres THEN  
                        ArtPres.Equiv
                        ELSE 1)) */
                    l-totimp   = l-precunit * (DetEC.CantUMI - /*(IF AVAILABLE ArtPres THEN */
                                                                        DetEC.CancUMI /*/ ArtPres.Equiv
                                                                      ELSE 0) */ )
                    l-simbolo  = Moneda.Simbolo
                    l-totec    = l-totec + l-totimp.
            

                FIND ArtProv WHERE ArtProv.Id-Prov = Prov.Id-Prov AND 
                    ArtProv.Id-Articulo = DetOC.Id-Articulo
                    NO-LOCK NO-ERROR.
                IF AVAILABLE ArtProv THEN 
                DO:
                    IF ArtProv.DescPP > 0 AND 
                        ArtProv.IncPP = TRUE THEN 
                    DO:

                        l-DesPP = l-DesPP + (l-TotImp * ArtProv.DescPP / 100).
                    END.
                END.

            END. /* del available detoc */ 
            ELSE 
            DO:

                v-error = TRUE.

                FIND FIRST HCosto WHERE HCosto.Id-Articulo = DetEC.Id-Articulo
                    AND HCosto.Id-Prov = EC.Id-Prov
                    AND HCosto.FecReg <= EC.FecReg NO-LOCK NO-ERROR.

                ASSIGN 
                    l-simbolo = "$".

                v-error = FALSE.

                IF AVAILABLE HCosto THEN 
                DO:

                    FIND moneda WHERE moneda.id-moneda = hcosto.id-moneda
                        NO-LOCK NO-ERROR.
                    FIND FIRST tipocambio WHERE tipocambio.id-moneda = hcosto.id-moneda 
                        NO-LOCK NO-ERROR.
                    IF hcosto.id-moneda = 5 THEN 
                        FIND FIRST tipocambio WHERE tipocambio.id-moneda = 3 
                            NO-LOCK NO-ERROR.
        
                    ASSIGN
                        l-simbolo    = moneda.simbolo
                        l-tipocambio = tipocambio.importe.
    
                    ASSIGN
                        l-precunit = HCosto.Costo / l-tipocambio
                        l-totec    = l-totec +
                                        (((l-PrecUnit *(DetEC.CantUMI - DetEC.CancUMI))) *
                                            IF Prov.Tipo<> 'E' THEN 
                                                l-TipoCambio ELSE 1)
                        l-totimp   = l-precunit * (DetEC.CantUMI - DetEC.CancUMI)
                        l-aste     = "*".

                    FIND ArtProv WHERE ArtProv.Id-Prov = Prov.Id-Prov AND 
                        ArtProv.Id-Articulo = DetEC.Id-Articulo
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE ArtProv AND 
                        ArtProv.DescPP > 0 AND 
                        ArtProv.IncPP = TRUE THEN 
                    DO:

                        ASSIGN
                            l-DifUnit  = l-PrecUnit - (l-PrecUnit / (1 - (Artprov.DescPP / 100)))
                            l-DifMult  = l-DifUnit * (DetEC.CantUMI - DetEC.CancUMI)
                            l-precunit = l-PrecUnit - l-DifUnit
                            l-totec    = l-totec - l-DifMult
                            l-totimp   = l-TotImp - l-DifMult.

                        ASSIGN 
                            l-DesPP = l-DesPP + (l-TotImp * ArtProv.DescPP / 100).

                    END.
                END.
                ELSE 
                DO:

                    FIND ArtProv WHERE ArtProv.Id-Prov = Prov.Id-Prov AND 
                        ArtProv.Id-Articulo = DetEC.Id-Articulo
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE ArtProv THEN 
                    DO:
                        FIND moneda WHERE moneda.id-moneda = ArtProv.id-moneda
                            NO-LOCK NO-ERROR.
                        FIND FIRST tipocambio WHERE tipocambio.id-moneda = ArtProv.id-moneda 
                            NO-LOCK NO-ERROR.
                        IF ArtProv.Id-moneda = 5 THEN 
                            FIND FIRST tipocambio WHERE tipocambio.id-moneda = 3 
                                NO-LOCK NO-ERROR.
                        ASSIGN
                            l-simbolo    = moneda.simbolo
                            l-tipocambio = tipocambio.importe.

                        RUN /usr2/adosa/procs/cnfa0450.p(INPUT ArtProv.Id-Articulo,
                            INPUT-OUTPUT l-PrecUnit,
                            INPUT ArtProv.Id-Prov).
        
                        ASSIGN
                            l-totec = l-totec +
                                            (((l-PrecUnit *(DetEC.CantUMI - DetEC.CancUMI))) *
                                                IF Prov.Tipo<> 'E' THEN 
                                                    l-TipoCambio ELSE 1)
                            l-totimp   = l-precunit * (DetEC.CantUMI - DetEC.CancUMI)
                            l-aste     = "*".

                        IF ArtProv.DescPP > 0 AND 
                            ArtProv.IncPP = TRUE THEN 
                        DO:
                            ASSIGN
                                l-DifUnit  = l-PrecUnit - (l-PrecUnit / (1 - (Artprov.DescPP / 100)))
                                l-DifMult  = l-DifUnit * (DetEC.CantUMI - DetEC.CancUMI)
                                l-precunit = l-PrecUnit - l-DifUnit
                                l-totec    = l-totec - l-DifMult
                                l-totimp   = l-TotImp - l-DifMult.
    
                            ASSIGN 
                                l-DesPP = l-DesPP + (l-TotImp * ArtProv.DescPP / 100).
                        END.
                    END.
                END.
                

                IF v-error THEN 
                DO:
                    ASSIGN
                        v-artnoenc = v-artnoenc + detec.id-articulo + ","
                        l-precunit = 0
                        l-totimp   = 0.
                END.

            END. /* del else del if */

            IF v-artnoenc > "" THEN
                v-artnoenc = SUBSTRING(v-artnoenc,1,LENGTH(v-artnoenc) - 1).

            FIND Kolor WHERE Kolor.Id-Color = DetEC.Id-Color 
                NO-LOCK NO-ERROR.
            
            FIND FIRST TipoCambio WHERE TipoCambio.Id-Moneda = 3 
                NO-LOCK NO-ERROR.
            CREATE ttEntCompra.
            ASSIGN
                ttEntCompra.EntradaC        = EC.Id-EC
                ttEntCompra.TipoCambioDolar = IF AVAILABLE TipoCambio THEN TipoCambio.Importe ELSE 0
                ttEntCompra.Codigo          = DetEC.Id-Articulo
                ttEntCompra.Descripcion     = DetEC.Descr
                ttEntCompra.Colorr          = kolor.descr
                ttEntCompra.CantPres        = ((IF AVAILABLE ArtPres THEN 
                                        DetEC.CancUMI / ArtPres.Equiv
                                        ELSE 0))                         
                ttEntCompra.CantUM          = (DetEC.CantUMI - DetEC.CancUMI)
                ttEntCompra.PrecioUnit      = l-precunit   
                ttEntCompra.Importe         = l-totimp.
                         

        END. /* del for each detec */


    END. /* del ec */
END PROCEDURE.
