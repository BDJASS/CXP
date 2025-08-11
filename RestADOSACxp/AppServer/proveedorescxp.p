@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : proveedorescxp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Sun Aug 10 15:18:14 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttProveedores
    FIELD IdProv    AS INTEGER 
    FIELD Nombre    AS CHARACTER
    FIELD Tipo      AS CHARACTER
    FIELD Tel       AS CHARACTER
    FIELD FecReg    AS DATE
    FIELD Email     AS CHARACTER    
    FIELD RFC       AS CHARACTER
    FIELD Bloqueado AS LOGICAL.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetProveedores:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttProveedores.


    FOR EACH ProvGastos NO-LOCK:
    
        CREATE ttProveedores.
        
        ASSIGN 
            ttProveedores.IdProv    = ProvGastos.Id-ProvGastos
            ttProveedores.Nombre    = ProvGastos.Nombre
            ttProveedores.Tipo      = "Gastos"
            ttProveedores.Tel       = ProvGastos.Tel1
            ttProveedores.FecReg    = TODAY //Este dato no existe en ProvGastos, hay que agregar la columna en la tabla para poder empezar a usarla.
            ttProveedores.Email     = ProvGastos.e-mail
            ttProveedores.RFC       = ProvGastos.RFC
            ttProveedores.Bloqueado = FALSE.  //Este campo no existe en ProvGastos, hay que agregar la columna en la tabla para poder empezar a usarla.
        
    END.
    
    FOR EACH Prov NO-LOCK:
    
        CREATE ttProveedores.
        
        ASSIGN 
            ttProveedores.IdProv    = Prov.Id-Prov
            ttProveedores.Nombre    = Prov.Nombre
            ttProveedores.Tipo      = "Proveedor"
            ttProveedores.Tel       = Prov.Tel1
            ttProveedores.FecReg    = Prov.FecReg
            ttProveedores.Email     = Prov.e-mail
            ttProveedores.RFC       = Prov.RFC
            ttProveedores.Bloqueado = Prov.Bloqueado.
        
    END.
    RETURN. 
END PROCEDURE.

