/* ----------------------------------------
CÛdigo exportado desde SAS Enterprise Guide
FECHA: miÈrcoles, 6 de noviembre de 2024     HORA: 13:49:40
PROYECTO: ProyectoESG_20241106
RUTA DEL PROYECTO: C:\Users\x100340\OneDrive - Santander Office 365\General - MS Loan Tapes Interno\Pillar III\02. C”DIGO\01. SAS Piloto Nov24\ProyectoESG_20241106.egp
---------------------------------------- */

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend _sas_pushchartsize;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend _sas_popchartsize;


ODS PROCTITLE;
OPTIONS DEV=SVG;
GOPTIONS XPIXELS=0 YPIXELS=0;
%macro HTML5AccessibleGraphSupported;
    %if %_SAS_VERCOMP(9, 4, 4) >= 0 %then ACCESSIBLE_GRAPH;
%mend;
FILENAME EGHTMLX TEMP;
ODS HTML5(ID=EGHTMLX) FILE=EGHTMLX
    OPTIONS(BITMAP_MODE='INLINE')
    %HTML5AccessibleGraphSupported
    ENCODING='utf-8'
    STYLE=HTMLBlue
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
;

/*   INICIO DEL NODO: S83   */
%LET _CLIENTTASKLABEL='S83';
%LET _CLIENTPROCESSFLOWNAME='Flujo del proceso';
%LET _CLIENTPROJECTPATH='C:\Users\x100340\OneDrive - Santander Office 365\General - MS Loan Tapes Interno\Pillar III\02. C”DIGO\01. SAS Piloto Nov24\ProyectoESG_20241106.egp';
%LET _CLIENTPROJECTPATHHOST='SA09649X100340P';
%LET _CLIENTPROJECTNAME='ProyectoESG_20241106.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

/*%MACRO S83(IN_PERIMETRO_83,IN_CC, TABLA_OUT);*/
/*SATELITE 83*/
*COMB_ID**************************************************************************************;
proc format;
    value $base_fmt /* COMB1 */
        'ASSETS' = 'B01';
    value $main_cat_fmt /* COMB2 */
        'SUBSIDIARI7ES', 'ASSOCIATES', 'JOINT VENTURES' = 'MC08'
        'LOANS AND ADVANCES' = 'MC02'
        'DEBT SECURITIES ACQUIRED' = 'MC04';
    value $acct_portfolio_fmt /* COMB3 */
        'CASH AND CASH BALANCES AT CENTRAL BANKS AND OTHER DEMAND DEPOSITS' = 'ACPF1'
        'HELD FOR TRADING' = 'ACPF2'
        'NON-TRADING FINANCIAL ASSETS MANDATORILY AT FAIR VALUE THROUGH PROFIT OR LOSS' = 'ACPF3'
        'FINANCIAL INSTRUMENT DESIGNATED AT FAIR VALUE THROUGH PROFIT OR LOSS' = 'ACPF4'
        'FINANCIAL ASSETS AT FAIR VALUE THROUGH OTHER COMPREHENSIVE INCOME' = 'ACPF5'
        'AMORTISED COST' = 'ACPF6';
    value $sector_fmt /* COMB4 */
        'GENERAL GOVERNMENTS' = 'SC0301'
        'OTHER FINANCIAL CORPORATION' = 'SC0302'
        'NON FINANCIAL CORPORATION' = 'SC0303'
        'HOUSEHOLDS' = 'SC0304'
        'CENTRAL BANKS' = 'SC01'
        'CREDIT INSTITUTIONS' = 'SC02';
    value $collateral_fmt /* COMB5 */
        'NO COLLATERAL REAL' = 'COLL1'
        'REAL ESTATE. RESIDENTIAL' = 'COLL2'
        'REAL ESTATE. COMMERCIAL' = 'COLL3'
        'OTHER REAL COLLATERAL' = 'COLL4';
    value $type_of_value_fmt /* COMB6 */
        'GROSS','NOMINAL','ACCRUED INTEREST','TRANSACTION COSTS','PREMIUM/DISCOUNT',
        'MICROHEDGE ADJUSTMENT','REST FAIR VALUE ADJUSTMENT THROUGH P&L','FAIR VALUE ADJUSTMENT VS OCI' = 'TYVA01'
        'ACCUMULATED IMPAIRMENT' = 'TYVA02';
    value $fair_value_fmt /* COMB7 */
        'FAIR VALUE GROSS' = 'FVAD1'
        'NON REQUIRED' = 'FVAD3';
run;

/* Carga y procesamiento; ejecutamos cada funciÛn que corresponde a los COMBs, del 1 al 7 */
data &TABLA_OUT (drop=COMB1-COMB7);
    set SAVE.DMAAS_ESG_83_202406;

    BASE_C = 					put(BASE, $base_fmt.);
    MAIN_CATEGORY_C = 			put(MAIN_CATEGORY, $main_cat_fmt.);
    ACCOUNTING_PORTFOLIO_C = 	put(ACCOUNTING_PORTFOLIO, $acct_portfolio_fmt.);
    SECTOR_C = 					put(SECTOR, $sector_fmt.);
    COLLATERAL_C = 				put(COLLATERAL, $collateral_fmt.);
    TYPE_OF_VALUE_C = 			put(TYPE_OF_VALUE, $type_of_value_fmt.);
    FAIR_VALUE_DETAIL_C = 		put(FAIR_VALUE_DETAIL, $fair_value_fmt.);

    /* ConcatenaciÛn en COMB_ID usando CATX para simplificar */
    COMB_ID = catx(";", BASE_C, MAIN_CATEGORY_C, ACCOUNTING_PORTFOLIO_C, SECTOR_C, COLLATERAL_C, TYPE_OF_VALUE_C, FAIR_VALUE_DETAIL_C);
run;

/*FILTROS*************************************************************************************/
/*DATA S83_1;
SET COMB_ID83_3;
WHERE type_of_value ='GROSS' AND MAIN_CATEGORY IN ('LOANS AND ADVANCES','DEBT SECURITIES ACQUIRED')
     AND COLLATERAL IN ('NO COLLATERAL REAL','REAL ESTATE. RESIDENTIAL','REAL ESTATE. COMMERCIAL', 'OTHER REAL COLLATERAL')
     AND SECTOR IN ('CREDIT INSTITUTIONS','CENTRAL BANKS','GENERAL GOVERNMENTS', 'OTHER FINANCIAL CORPORATION', 'NON FINANCIAL CORPORATION', 'HOUSEHOLDS');
RUN;*/

**DATOS*******************************************************************************************;
PROC SQL;
   CREATE TABLE S83_1_1 AS 
   SELECT t1.INFORMING_SOCIETY, 
          t1.Counterparty_Entity, 
          t1.CONTRATO, 
          t1.BASE, 
          t1.MAIN_CATEGORY, 
          t1.ACCOUNTING_PORTFOLIO, 
          t1.SECTOR, 
          t1.COLLATERAL, 
          t1.TYPE_OF_VALUE, 
          t1.FAIR_VALUE_DETAIL, 
          t1.CINT, 
          t1.PAISRE_TIT, 
          t1.CNAE, 
		   t1.CODPROD,
		  t1.CODSPROD,
		  t1.FECHAPER,
		  t1.SECBES,
          /* SUM_of_AMOUNT */
            (SUM(t1.AMOUNT)) FORMAT=BEST14. AS AMOUNT, 
          t1.BASE_C, 
          t1.MAIN_CATEGORY_C, 
          t1.ACCOUNTING_PORTFOLIO_C, 
          t1.SECTOR_C, 
          t1.COLLATERAL_C, 
          t1.TYPE_OF_VALUE_C, 
          t1.FAIR_VALUE_DETAIL_C, 
          t1.COMB_ID
      FROM WORK.COMB_ID83_3 t1
      GROUP BY t1.INFORMING_SOCIETY,
               t1.Counterparty_Entity,
               t1.CONTRATO,
               t1.BASE,
               t1.MAIN_CATEGORY,
               t1.ACCOUNTING_PORTFOLIO,
               t1.SECTOR,
               t1.COLLATERAL,
               t1.TYPE_OF_VALUE,
               t1.FAIR_VALUE_DETAIL,
               t1.CINT,
               t1.PAISRE_TIT,
               t1.CNAE,
			   t1.CODPROD,
				t1.CODSPROD,
		  		t1.FECHAPER,
			   t1.SECBES,
               t1.BASE_C,
               t1.MAIN_CATEGORY_C,
               t1.ACCOUNTING_PORTFOLIO_C,
               t1.SECTOR_C,
               t1.COLLATERAL_C,
               t1.TYPE_OF_VALUE_C,
               t1.FAIR_VALUE_DETAIL_C,
               t1.COMB_ID;
QUIT;
PROC SQL;
   CREATE TABLE S83_2 AS
   SELECT DISTINCT 
          A.*, 
          COALESCE(B.LEVEL1, "N/A") AS LEVEL1,
          COALESCE(B.LEVEL2, "N/A") AS LEVEL2,
          COALESCE(B.FLAG_SFCS, 0) AS FLAG_SFCS,
          B.DCON, 
          B.E0623_IDSUBPRD, 
          B.F_LENDING, 
          B.CODPROD, 
          B.EST_REF, 
          B.LEI, 
          B.NACE, 
          B.NACEL1, 
          B.NACEL1_CODE, 
          B.NACE_ESG, 
          B.NACE_ESG_CODE, 
          B.ESG_SUB
   FROM S83_1_1 A 
   LEFT JOIN SAVE.GRANULAR_SCIB_NFRD_JUN24 B
      ON A.CONTRATO = B.CONTRATO 
      AND A.CINT = B.CINT 
      AND A.PAISRE_TIT = B.PAISRE_TIT 
      AND A.CNAE = B.CNAE 
      AND A.SECBES = B.SECBES;
QUIT;
**NACES ******************************************************************************************;
DATA NACES;
    SET S83_2_1;

    IF SECTOR = "OTHER FINANCIAL CORPORATION" THEN DO;
        NACEL1 = ""; 
        NACEL1_CODE = "";

        IF NACE IN ("", "N/A") OR SUBSTR(NACE, 1, 2) NOT IN ("64", "65", "66") THEN DO;
            NACE_ESG = "6499"; 
            NACE_ESG_CODE = "NACE116499";
        END;
    END;
    ELSE 
	IF SECTOR = "NON FINANCIAL CORPORATION" THEN DO;
        IF NACE IN ("", "N/A") 
		OR (SUBSTR(NACE, 1, 2) IN ("64", "65", "66") 
		AND NACE NE "6420") 
		THEN DO;
            NACE_ESG = "9499"; 
            NACE_ESG_CODE = "NACE199499"; 
            NACEL1 = "S"; 
            NACEL1_CODE = "CNAEL18";
        END;
    END;
    ELSE 
	IF SECTOR = "HOUSEHOLDS" THEN DO;
        NACE_ESG = ""; 
        NACEL1 = ""; 
        NACE = ""; 
        NACEL1_CODE = ""; 
        NACE_ESG_CODE = "";
    END;

RUN;
**CSAES*******************************************************************************************;
DATA CSAES;
SET SAVE.EJERCICIO_CSAE;
         CODPER1 = PUT(CODPER, Z9.);   
         CINT = COMPRESS(TIPO)||  COMPRESS(CODPER1);
RUN;
PROC SQL;
         CREATE TABLE CSAES_2 AS   
                   SELECT DISTINCT A.*, B.CSAE
                   FROM NACES A LEFT JOIN CSAES B 
                   ON A.CINT = B.CINT;
QUIT;
PROC SQL;
         CREATE TABLE CSAES_2_1 AS 
                   SELECT DISTINCT A.*, B.NACE AS CSAE_NACE
                   FROM CSAES_2 A LEFT JOIN SAVE.RELACION_NACE_CNAE B 
                   ON A.CSAE = B.CNAE;
QUIT;
DATA CSAES_3;
SET CSAES_2_1 (DROP=CSAE RENAME= CSAE_NACE = CSAE);
         IF CSAE NE "" 
		AND NACE = '6420' 
		AND NACE_ESG = '6420' 
		THEN DO;
            	NACE_ESG_1 = CSAE;
         END;
RUN;
%NACE_CODE(CSAES_3,CSAES_4, NACE_ESG_1, NACE_ESG_CODE_1);
DATA CSAES_5;
SET CSAES_4;
         IF CSAE NE "" 
		AND NACE = '6420' 
		AND NACE_ESG = '6420' 
		THEN DO;
				NACE_ESG_F = NACE_ESG_1 ;
                NACE_ESG_CODE_F = NACE_ESG_CODE_1;
         END;
        ELSE DO;
                NACE_ESG_F = NACE_ESG;
                NACE_ESG_CODE_F = NACE_ESG_CODE;
         END;
         IF SUBSTR(NACE_ESG_F,1,2) IN ('64','65','66') 
		AND SECTOR = 'NON FINANCIAL CORPORATION' 
		THEN DO;
                NACE_ESG_F = '9499';
                NACE_ESG_CODE_F = 'NACE199499';
         END;
RUN;
**CHRONIC Y ACUTE********************************************************************************;
/*NACE para cruce*/
DATA NACE_CRUCE;
    SET CSAES_5;
    NACEL2 = SUBSTR(NACE_ESG_F, 1, 2);

    SELECT(NACEL2);
        WHEN ("01", "02", "03") 						NACEL1_79 = "A";
        WHEN ("05", "06", "07", "08", "09") 			NACEL1_79 = "B";
        WHEN ("10", "11", "12", "13", "14", 
              "15", "16", "17", "18", "19", 
              "20", "21", "22", "23", "24", 
			  "25", "26", "27", "28", "29", 
              "30", "31", "32", "33") /* 10-33 */		NACEL1_79 = "C";
        WHEN ("35") 									NACEL1_79 = "D";
        WHEN ("36", "37", "38", "39") 					NACEL1_79 = "E";
        WHEN ("41", "42", "43") 						NACEL1_79 = "F";
        WHEN ("45", "46", "47") 						NACEL1_79 = "G";
        WHEN ("49", "50", "51", "52", "53") 			NACEL1_79 = "H";
        WHEN ("55", "56") 								NACEL1_79 = "I";
        WHEN ("58", "59", "60", "61", "62", "63") 		NACEL1_79 = "J";
        WHEN ("64", "65", "66") 						NACEL1_79 = "K"; /* Seg˙n la reuniÛn, asignamos "K" como "S" */
        WHEN ("68") 									NACEL1_79 = "L";
        WHEN ("69", "70", "71", "72", "73", "74", "75") NACEL1_79 = "M";
        WHEN ("77", "78", "79", "80", "81", "82") 		NACEL1_79 = "N";
        WHEN ("84") 									NACEL1_79 = "O";
        WHEN ("85") 									NACEL1_79 = "P";
        WHEN ("86", "87", "88") 						NACEL1_79 = "Q";
        WHEN ("90", "91", "92", "93") 					NACEL1_79 = "R";
        WHEN ("94", "95", "96")							NACEL1_79 = "S";
        WHEN ("97", "98") 								NACEL1_79 = "T"; /* Seg˙n la reuniÛn, asignamos "T" como "S" */
        WHEN ("99") 									NACEL1_79 = "U"; /* Seg˙n la reuniÛn, asignamos "U" como "S" */
        OTHERWISE;
    END;
RUN;
DATA RISK;
    SET NACE_CRUCE;
    FORMAT NACE_CRUCE $9.;
    
    /* Crear la variable NACE_CRUCE combinando NACEL1_79 y los primeros dos caracteres de NACE_ESG_f */
    NACE_CRUCE = CATX('.', COMPRESS(NACEL1_79), COMPRESS(SUBSTR(NACE_ESG_f, 1, 2)));
    
    IF NACE_ESG_f = "" THEN NACE_CRUCE = "";
RUN;
* DefiniciÛn de Secured;
DATA RISK2;
    SET RISK;
    FORMAT NACE_CRUCE $9.;
    
    /* SimplificaciÛn de condiciones con IN para el primer bloque */
    IF (
		SECTOR IN ("HOUSEHOLDS", "NON FINANCIAL CORPORATION", "OTHER FINANCIAL CORPORATION") 
        AND COLLATERAL IN ("REAL ESTATE. COMMERCIAL", "REAL ESTATE. RESIDENTIAL") 
        AND TYPE_OF_VALUE = "GROSS" 
        AND FAIR_VALUE_DETAIL NE "ACCUMULATED NEGATIVE CHANGES IN FAIR VALUE DUE TO CREDIT RISK"
		) 
    OR 
		(
		MAIN_CATEGORY = "REAL ESTATE" 
        AND TYPE_OF_VALUE = "GROSS" 
        AND FAIR_VALUE_DETAIL NE "ACCUMULATED NEGATIVE CHANGES IN FAIR VALUE DUE TO CREDIT RISK"
		) 
    THEN NACE_CRUCE = "Secured";
RUN;

DATA RISK3_SECURED RISK3_NO_SECURED;
    SET RISK2;
    FORMAT NUTS_CRUCE $10.;

    /* Separar los datos en Secured y No Secured */
    IF NACE_CRUCE = 'Secured' 
	THEN 
		OUTPUT RISK3_SECURED;
    ELSE 
	IF NACE_CRUCE NE 'Secured' 
	THEN DO;
        NUTS_CRUCE = NUTS3_COUNTERPARTY;
        OUTPUT RISK3_NO_SECURED;
    END;
RUN;
**PEGO LOS DATOS DE EPCS PARA LA ELEGIBILIDAD DE HIPOTECAS*********;
PROC SQL;
   /* Crear tabla EPCS_TABLA con el factor de emisiÛn y los datos EPC */
   CREATE TABLE EPCS_TABLA AS
   SELECT DISTINCT A.*,
                   emission_factor AS THRESHOLD, 
                   EPC_LABEL_DATA, EPC_LABEL_DATA_CD, 
                   EPC_LABEL, EPC_LABEL_CODE, 
                   idcatast, g4124_biengar1 AS id_garant, 
                   G4091_CODPOSGR, G4134_IMGARANT, 
                   NUTS3_COLLATERAL, Clasificacion_final, anoedifi
   FROM RISK3_SECURED A 
   LEFT JOIN SAVE.garantia_contrato_NTS_EPCs_JUN24 B
   ON A.CONTRATO = B.CONTRATO
   WHERE B.NUTS3_COLLATERAL NE '';
QUIT;

*PONDERACIONES GROSS;
* Crear GROSS_CONTRATO: suma de Gross por combinaciÛn de variables clave;

PROC SQL;
   CREATE TABLE EPCS_TABLA2 AS
   SELECT CONTRATO, cint, cnae, paisre_tit, 
          COLLATERAL, SECTOR, ACCOUNTING_PORTFOLIO, 
          SUM(AMOUNT) AS GROSS_CONTRATO
   FROM EPCS_TABLA
   GROUP BY CONTRATO, cint, cnae, paisre_tit,  
            COLLATERAL, ACCOUNTING_PORTFOLIO, SECTOR;
QUIT;

*UniÛn de EPCS_TABLA y EPCS_TABLA2 para agregar GROSS_CONTRATO;

PROC SQL;
   CREATE TABLE EPCS_TABLA3 AS
   SELECT A.*, B.GROSS_CONTRATO
   FROM EPCS_TABLA A
   LEFT JOIN EPCS_TABLA2 B 
   ON A.CONTRATO = B.CONTRATO 
      AND A.cint = B.cint 
      AND A.paisre_tit = B.paisre_tit 
      AND A.CNAE = B.CNAE
      AND A.COLLATERAL = B.COLLATERAL 
      AND A.ACCOUNTING_PORTFOLIO = B.ACCOUNTING_PORTFOLIO 
      AND A.SECTOR = B.SECTOR;
QUIT;
* SUM_VAR_GARA: Se calcula la suma de las garantÌas por CONTRATO, cint, cnae, paisre_tit, accounting_portfolio, collateral, GROSS_CONTRATO;
PROC SQL;
CREATE TABLE EPCS_TABLA4 AS
SELECT DISTINCT *, sum(G4134_IMGARANT) as SUM_VAL_GARA
FROM EPCS_TABLA3  
group by CONTRATO, cint, cnae, paisre_tit, GROSS_CONTRATO , COLLATERAL, ACCOUNTING_PORTFOLIO, SECTOR ;
RUN;
proc sql;
     create table EPCS_TABLA5 as
     select *, count(contrato) as num_garanti
     from EPCS_TABLA4
     group by contrato, cint, cnae, paisre_tit, COLLATERAL, ACCOUNTING_PORTFOLIO, SECTOR ;
run;
* Se calcula el gross ponderado a partir de GROSS_CONTRATO y SUM_VAL_GARA;
DATA EPCS_TABLA6 (drop= AMOUNT rename= gross_ponderado = AMOUNT);
SET EPCS_TABLA5;
format GROSS_PONDERADO 18.2;
format NUTS_CRUCE $6.;
factor = G4134_IMGARANT / SUM_VAL_GARA;
if factor in (.) and G4134_IMGARANT= (.) and id_garant ne (.) then factor2 = (.); 
else if factor in (.) then factor2 = 1/num_garanti; else factor2 = factor;
GROSS_PONDERADO = GROSS_CONTRATO * factor2;
if id_garant = . then GROSS_PONDERADO = AMOUNT;

/*CAMBIOS MARZO 24*/
/*POR DEFECTO, EL NUTS DE LA GARANTIA*/
IF NUTS3_COLLATERAL NE "" THEN DO;
	NUTS_CRUCE = NUTS3_COLLATERAL;
	PAIS_CRUCE = substr(NUTS3_COLLATERAL, 1, 2);
END;
/*SI NO HAY GARANTIA, EL NUTS DEL CLIENTE (CONTRAPARTE)*/
ELSE IF NUTS3_COLLATERAL = "" THEN DO;
       NUTS_CRUCE = NUTS3_COUNTERPARTY;
	   PAIS_CRUCE = PAISRE_TIT;
END; 
/*SI NO, ESPA—A*/
ELSE IF NUTS3_COLLATERAL="" AND NUTS3_COUNTERPARTY="" THEN DO;
	NUTS_CRUCE = "";
	PAIS_CRUCE = "ES";
END;
IF PAISRE_TIT = "GB" 
AND NUTS3_COLLATERAL="" THEN PAIS_CRUCE = "UK";
ELSE 
  IF PAIS_CRUCE = "" 
	THEN DO;
	IF PAISRE_TIT NE "" THEN PAIS_CRUCE=PAISRE_TIT;
						ELSE PAIS_CRUCE="ES";
END; 
run;
DATA RISK3_NO_SECURED1;
SET RISK3_NO_SECURED;
FORMAT PAIS_CRUCE $3.;

IF PAISRE_TIT = "GB" 
AND NUTS3_COLLATERAL="" THEN PAIS_CRUCE = "UK";
ELSE 
  IF PAIS_CRUCE = "" 
	THEN DO;
	IF PAISRE_TIT NE "" THEN PAIS_CRUCE=PAISRE_TIT;
						ELSE PAIS_CRUCE="ES";
END;
RUN;
DATA RISK3;
SET EPCS_TABLA6 RISK3_NO_SECURED1;
RUN;
/*NO COLLATERAL*/
PROC SQL;
CREATE TABLE RISK4 AS
SELECT DISTINCT A.*, B.RS_CHRONIC AS CHRONIC_CNTRPRTY, B.RS_ACUTE AS ACUTE_CNTRPRTY
FROM RISK3 A LEFT JOIN SAVE.PHYSICAL_RISK B
ON A.NACE_CRUCE= B.NACE AND a.NUTS_CRUCE= B.LOCATION AND A.PAIS_CRUCE = B.COUNTRY;
QUIT;
PROC SQL;
CREATE TABLE RISK4_1 AS
SELECT DISTINCT A.*, B.RS_CHRONIC AS CHRONIC_CNTRPRTY2, B.RS_ACUTE AS ACUTE_CNTRPRTY2
FROM RISK4 A LEFT JOIN SAVE.PHYSICAL_RISK_CNTRY B
ON A.NACE_CRUCE= B.NACE AND A.PAIS_CRUCE = B.COUNTRY;
QUIT;
DATA RISK5;
SET RISK4_1;
format CHRONIC ACUTE $3.;
IF CHRONIC_CNTRPRTY NE "" THEN CHRONIC = PUT(CHRONIC_CNTRPRTY, 3.);
ELSE IF CHRONIC_CNTRPRTY2 NE "" THEN CHRONIC = PUT(CHRONIC_CNTRPRTY2, 3.);
IF ACUTE_CNTRPRTY NE "" THEN ACUTE = PUT(ACUTE_CNTRPRTY, 3.);
ELSE IF ACUTE_CNTRPRTY2 NE "" THEN ACUTE = PUT(ACUTE_CNTRPRTY2, 3.);
RUN;
/*Si no cruza-> No por defecto*/
DATA RISK6;
SET RISK5;
format CHRONIC ACUTE $3.;
format CHRONIC_CODE ACUTE_CODE $5.;
IF ACUTE = "" 			THEN ACUTE = "No";
IF CHRONIC = "" 		THEN CHRONIC = "No";
IF ACUTE = "Yes" 		THEN ACUTE_CODE= "ACUT1";
ELSE IF ACUTE = "No" 	THEN ACUTE_CODE= "ACUT2";
IF CHRONIC = "Yes" 		THEN CHRONIC_CODE= "CHRO1";
ELSE IF CHRONIC = "No" 	THEN CHRONIC_CODE= "CHRO2";
RUN;
**PEGO LOS DATOS DE ZONA CLIM¡TICA PARA LA ELEGIBILIDAD DE HIPOTECAS*********;
PROC SQL;
CREATE TABLE ZONA_CLIMATICA_TABLA1 AS
SELECT DISTINCT *
FROM RISK6 A LEFT JOIN SAVE.ZONA_CLIMATICA_JUN24 B  
ON A.idcatast = B.ID_CATASTRAL;
QUIT;

/*Tipo de finalidad*/
PROC SQL;
CREATE TABLE ZONA_CLIMATICA_TABLA AS
SELECT DISTINCT A.*, B.tipo_fina_prop
FROM ZONA_CLIMATICA_TABLA1 A LEFT JOIN SAVE.FINALIDAD_CONTRATOS_JUN24 B
ON A.CONTRATO= B.CONTRATO;
QUIT;
/*MODIFICAR - SE ELIMINAN DUPLICADOS DE M¡S*/
PROC SORT
DATA=WORK.ZONA_CLIMATICA_TABLA
OUT=WORK.ZONA_CLIMATICA_TABLA_SD/*OUTPUT DATASET WITH DEDUPLICATED RECORD*/
DUPSOUT=DUPLICADOS_FUERA NODUPKEY;
BY CONTRATO tipo_fina_prop AMOUNT id_garant;
RUN;

**GREEN FINANCIAL INSTRUMENT***************************************************************;
DATA METRICA_3_12;
SET ZONA_CLIMATICA_TABLA_SD ;
FORMAT GREENF GREENF_CODE $5.;
GREENF = "No"; GREENF_CODE = "GFI2";

/*SPECIALISED LENDING**************************************************************************
IF F_LENDING = 1 THEN DO; GREENF = "Yes"; GREENF_CODE = "GFI1"; END;*/

/*CLASIFICACI”N GFI1*/
/*MOTOR VEHICLE LOANS*/
/*INCORPORADO JUL24*/
/* DefiniciÛn de sectores que califican como Green Financial Instrument */
%let sector_verde = 'HOUSEHOLDS' 
					'NON FINANCIAL CORPORATION' 
					'OTHER FINANCIAL CORPORATION' 
					'CREDIT INSTITUTIONS' 
					'GENERAL GOVERNMENTS';

/* CondiciÛn principal para los sectores */
IF SECTOR IN (&sector_verde) THEN DO;

    /* Motor Vehicle Loans */
    IF LEVEL2 IN ("PR…STAMOS VEHÕCULOS A MOTOR", "PRESTAMOS VEHICULOS A MOTOR") THEN DO;
        GREENF = "Yes"; GREENF_CODE = "GFI1";
    END;
    
    /* Building Renovation Loans */
    ELSE IF LEVEL1 = "BUILDING RENOVATION LOANS" OR LEVEL2 IN ("PR…STAMOS PARA RENOVACI”N DE VIVIENDAS", "PRESTAMOS PARA RENOVACION DE VIVIENDAS") THEN DO;
        GREENF = "Yes"; GREENF_CODE = "GFI1";
    END;
    
    /* Ground Transport */
    ELSE IF LEVEL2 = "GROUND TRANSPORT" OR ACTIVIDAD_SFCS IN ("2.1 Land Transport", "2.1 LAND TRANSPORT") THEN DO;
        GREENF = "Yes"; GREENF_CODE = "GFI1";
    END;
    
    /* Building Renovation Loans con condiciones especÌficas */
    ELSE IF CODPROD = "143" AND CODSPROD = 542 THEN DO;
        IF EST_REF = "0000125" AND tipo_fina_prop IN ("C2", "F9", "M6", "76", "81", "82", "83", "84") THEN DO;
            GREENF = "Yes"; GREENF_CODE = "GFI1";
        END;
        ELSE IF EST_REF IN ("0000099", "0000115") THEN DO;
            GREENF = "Yes"; GREENF_CODE = "GFI1";
        END;
    END;
    
    /* Actividades relacionadas con eficiencia energÈtica en edificios */
    ELSE IF ACTIVIDAD_SFCS IN (
        "5.1 Construction, refurbishment and purchase of ìgreenî buildings", 
        "5.1 Green buildings", "5.2 Energy efficiency equipment in buildings", 
        "5.3. Renewable energy infrastructure in buildings", 
        "5.4. Instruments and devices to enhance buildingsí energy use"
    ) THEN DO;
        GREENF = "Yes"; GREENF_CODE = "GFI1";
    END;

END;

/*BUILDING ACQUISITION ES GFI1 SALVO LO VERDE DE GAR**********************************************************************************/
IF SECTOR = "GENERAL GOVERNMENTS" THEN DO;
	IF ESG_SUB = "LOCAL GOVERNMENTS" AND COLLATERAL IN ("REAL ESTATE. RESIDENTIAL", "REAL ESTATE. COMMERCIAL") THEN DO;
		IF  EPC_LABEL IN ('A','B','C') AND EPC_LABEL_DATA IN ('Real','Estimated') THEN DO;
			GREENF = "Yes"; GREENF_CODE = "GFI1";
		END; 	
		*Asturias, Canarias, Cantabria, CataluÒa, Extremadura, Murcia, PaÌs Vasco and Valencia;
		IF  EPC_LABEL = "D" AND EPC_LABEL_DATA IN ('Real','Estimated') AND PROVINCIA IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI",
		"BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE", "CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES") THEN DO;
			GREENF = "Yes"; GREENF_CODE = "GFI1";
		END; 
	END;
END;
IF SECTOR IN ("NON FINANCIAL CORPORATION", "OTHER FINANCIAL CORPORATION", "CREDIT INSTITUTIONS") AND COLLATERAL IN ("REAL ESTATE. RESIDENTIAL", "REAL ESTATE. COMMERCIAL") then do;
	IF  EPC_LABEL IN ('A','B','C') AND EPC_LABEL_DATA IN ('Real','Estimated') THEN DO;
		GREENF = "Yes"; GREENF_CODE = "GFI1";
	END;
		IF  EPC_LABEL = 'D' AND EPC_LABEL_DATA IN ('Real','Estimated') AND PROVINCIA IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI",
		"BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE", "CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES") THEN DO;
		GREENF = "Yes"; GREENF_CODE = "GFI1";
		END;
END;
IF SECTOR="HOUSEHOLDS" THEN DO; 
/*NO APLICA EL SECTOR*/
IF COLLATERAL IN ('REAL ESTATE. RESIDENTIAL','REAL ESTATE. COMMERCIAL') THEN DO;
		GREENF = "No"; GREENF_CODE = "GFI2";
		*Si etiqueta es ABC, real, antes del 2020 y tiene riesgo fÌsico, es gfi;
		IF EPC_LABEL IN ('A','B','C') AND anoedifi<=2020 AND COLLATERAL IN ('REAL ESTATE. RESIDENTIAL','REAL ESTATE. COMMERCIAL') THEN DO;
			IF CHRONIC_CODE = 'CHRO1' OR ACUTE_CODE = 'ACUT1' AND EPC_LABEL_DATA ='Real' THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
			END;
		end;
			*Si etiqueta es D, real, alguna de dichas provincias, antes del 2020 y tiene riesgo fÌsico, es gfi;/*CAMBIO MAYO24- Se ha aÒadido que antes de 2020 entra CRE tambiÈn*/
		IF EPC_LABEL = 'D' AND anoedifi<=2020 AND COLLATERAL IN ('REAL ESTATE. RESIDENTIAL','REAL ESTATE. COMMERCIAL') AND EPC_LABEL_DATA ='Real' AND PROVINCIA IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI",
		"BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE", "CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES")  THEN DO;
			IF CHRONIC_CODE = 'CHRO1' OR ACUTE_CODE = 'ACUT1' THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
			END;
		end;
		*Si etiqueta es ABC, ESTIMADO, antes del 2020  es gfi;
		IF EPC_LABEL IN ('A','B','C') AND anoedifi<=2020 AND COLLATERAL IN ('REAL ESTATE. RESIDENTIAL','REAL ESTATE. COMMERCIAL') THEN DO;
			IF EPC_LABEL_DATA ='Estimated' THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
			END;
		end;
			*Si etiqueta es D, estimado, alguna de dichas provincias, antes del 2020 es gfi;
		IF EPC_LABEL = 'D' AND anoedifi<=2020 AND COLLATERAL IN ('REAL ESTATE. RESIDENTIAL','REAL ESTATE. COMMERCIAL') AND EPC_LABEL_DATA ='Estimated' AND PROVINCIA IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI",
		"BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE", "CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES")  THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
		end;
	
		*Si etiqueta es ABC, real, despuÈs del 2020 y tiene riesgo fÌsico, es gfi;
		IF EPC_LABEL IN ('A','B','C') AND anoedifi>2020 AND COLLATERAL ="REAL ESTATE. RESIDENTIAL" AND EPC_LABEL_DATA IN ('Real','Estimated') THEN DO;
			IF CHRONIC_CODE = 'CHRO1' OR ACUTE_CODE = 'ACUT1' THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
		end;
		*Si etiqueta es de D, real, alguna de dichas provincias, despuÈs del 2020 y tiene riesgo fÌsico, es gfi;
		IF EPC_LABEL = 'D' AND anoedifi>2020 AND COLLATERAL ="REAL ESTATE. RESIDENTIAL" AND EPC_LABEL_DATA IN ('Real','Estimated') AND PROVINCIA IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI",
		"BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE", "CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES")  THEN DO;
			IF CHRONIC_CODE = 'CHRO1' OR ACUTE_CODE = 'ACUT1' THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
			END;
		end;
			
		*Si etiqueta es ABC, estimada, despuÈs del 2020 es gfi;
		IF EPC_LABEL IN ('A','B','C') AND anoedifi>2020 AND COLLATERAL ="REAL ESTATE. RESIDENTIAL"  AND EPC_LABEL_DATA ='Estimated' THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
		end;
		*Si etiqueta es de D, estimada, alguna de dichas provincias, despuÈs del 2020 es gfi;
		IF EPC_LABEL = 'D' AND anoedifi>2020 AND COLLATERAL ="REAL ESTATE. RESIDENTIAL" AND EPC_LABEL_DATA ='Estimated' AND PROVINCIA IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI",
		"BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE", "CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES")  THEN DO;
				GREENF = "Yes"; GREENF_CODE = "GFI1";
		end;
		*Building Acquisition por superar el threshold;
		IF EPC_LABEL IN ('A','B','C') AND anoedifi>2020 AND COLLATERAL ="REAL ESTATE. RESIDENTIAL" AND EPC_LABEL_DATA='Real' THEN DO;
				IF ZONA_CLIMATICA = "" THEN DO; /*INCLUIDO EN MARZO24 PORQUE NO SE INCLUIA CONTRATOS CON VIVIENDA SIN ZONA CLIMATICA ASIGNADA*/
					GREENF="Yes"; GREENF_CODE="GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'Œ' AND THRESHOLD>18 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'A' AND THRESHOLD>22.5 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'B' AND THRESHOLD>25.2 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'C' AND THRESHOLD>28.8 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'D' AND THRESHOLD>34.2 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'E' AND THRESHOLD>38.7 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF PROVINCIA IN ("CEUTA","MELILLA", "ILLES BA","LAS PALM","S.C. TEN") THEN DO;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'Œ' AND THRESHOLD>22.5 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'A' AND THRESHOLD>28.1 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'B' AND THRESHOLD>31.5 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'C' AND THRESHOLD>36.0 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'D' AND THRESHOLD>42.8 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'E' AND THRESHOLD>48.4 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
				END;
			END;
	END;
	IF EPC_LABEL = 'D' AND anoedifi>2020 AND COLLATERAL ="REAL ESTATE. RESIDENTIAL" AND EPC_LABEL_DATA='Real' AND PROVINCIA
	IN ("ASTURIAS", "S.C. TEN", "LAS PALM", "CANTABRI", "BARCELON", "GIRONA","LLEIDA", "TARRAGON", "BADAJOZ", "MURCIA", "ALICANTE",
	"CASTELL√", "VALENCIA", "C√CERES","C√ ÅCERES") THEN DO;
				IF ZONA_CLIMATICA = "" THEN DO;/*INCLUIDO EN MARZO24 PORQUE NO SE INCLUIA CONTRATOS CON VIVIENDA SIN ZONA CLIMATICA ASIGNADA*/
					GREENF="Yes"; GREENF_CODE="GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'Œ' AND THRESHOLD>18 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'A' AND THRESHOLD>22.5 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'B' AND THRESHOLD>25.2 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'C' AND THRESHOLD>28.8 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'D' AND THRESHOLD>34.2 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF SUBSTR(ZONA_CLIMATICA,1,1)= 'E' AND THRESHOLD>38.7 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
				END;
				IF PROVINCIA IN ("CEUTA","MELILLA", "ILLES BA","LAS PALM","S.C. TEN") THEN DO;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'Œ' AND THRESHOLD>22.5 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'A' AND THRESHOLD>28.1 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'B' AND THRESHOLD>31.5 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'C' AND THRESHOLD>36.0 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'D' AND THRESHOLD>42.8 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
					IF SUBSTR(ZONA_CLIMATICA,1,1)= 'E' AND THRESHOLD>48.4 THEN DO;
					GREENF = "Yes"; GREENF_CODE = "GFI1";
					END;
				END;
		/*END;*/
IF Clasificacion_final IN ("6. No cumple uso", "7. No aplica") THEN DO;
		GREENF = "No"; GREENF_CODE = "GFI2";
END;
END;
END;
/*REVISAR*/
/*SI CRUZA CON EL GD ES GFI1**************************************************************************/
IF FLAG_SFCS=1 THEN DO; GREENF = "Yes"; GREENF_CODE = "GFI1"; END;

IF DCON="BONOS VERDES" THEN DO; GREENF = "Yes"; GREENF_CODE = "GFI1"; END; 

RUN;
**PURPOSE ESG******************************************************************************;
DATA METRICA_3_13;
SET METRICA_3_12;
format     PURPOSE_ESG $50. ;
format     PURPOSE_ESG_CODE $50. ;
IF GREENF_CODE = "GFI2" 
THEN DO;
	PURPOSE_ESG = "N/A";
    PURPOSE_ESG_CODE = "N/A";
END;
IF GREENF_CODE = "GFI1" 
THEN DO;
	/*IF SECTOR NE "HOUSEHOLDS" THEN DO;
		PURPOSE_ESG = "N/A";
   		PURPOSE_ESG_CODE = "N/A";
	END;*/
	/*IF SECTOR = "HOUSEHOLDS" THEN DO;*/
/*GFI1*/
	
	IF SECTOR IN (&sector_verde) 
	THEN DO;
		/*MOTOR VEHICLE LOANS*/	
		IF LEVEL2 IN ("PR…STAMOS VEHÕCULOS A MOTOR","PR…STAMOS VEHÕCULOS A MOTOR")
		THEN DO;
		  	PURPOSE_ESG = "MOTOR VEHICLE LOANS"; PURPOSE_ESG_CODE = "PESG2";
		END;
		IF LEVEL2 IN ("PR…STAMOS VEHÕCULOS A MOTOR","PR…STAMOS VEHÕCULOS A MOTOR") 
		THEN DO;
			PURPOSE_ESG = "MOTOR VEHICLE LOANS"; PURPOSE_ESG_CODE = "PESG2";
		END;

		/*Segun indicado por guias Pilar3 - ACTIVIDAD SFCS del GreenDasboard*/
		IF ACTIVIDAD_SFCS IN ("2.1 LAND TRANSPORT") THEN DO;
		GREENF = "Yes"; GREENF_CODE = "GFI1";
		END;

		/*Segun indicado por guias Pilar3 - LEVEL2 del GreenDasboard*/
		IF LEVEL2 IN ("Ground Transport","GROUND TRANSPORT") 
		THEN DO;
		GREENF = "Yes"; GREENF_CODE = "GFI1";
		END;
		IF LEVEL1 IN ("TRANSPORTE","TRANSPORT") 
	   AND LEVEL2 IN ("OTROS","OTHERS") THEN DO;
		GREENF = "Yes"; GREENF_CODE = "GFI1";
		END;
		IF LEVEL1 = "BUILDING RENOVATION LOANS" 
		OR LEVEL2 IN ("PR…STAMOS PARA RENOVACI”N DE VIVIENDAS","PRESTAMOS PARA RENOVACION DE VIVIENDAS") 
		THEN DO;
			PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
		END;
	/*Segun indicado por guias Pilar3 - ACTIVIDAD P25 del GreenDasboard*/
		IF LEVEL1 = "BUILDING RENOVATION LOANS" 
		OR LEVEL2 
			IN ("PR…STAMOS PARA RENOVACI”N DE VIVIENDAS",
				"PRESTAMOS PARA RENOVACION DE VIVIENDAS") 
		THEN DO;
				PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
		END;
	/*Segun indicado por guias Pilar3 - ACTIVIDAD SFCS del GreenDasboard*/
		IF ACTIVIDAD_SFCS 
			IN ("5.1 CONSTRUCTION, REFURBISHMENT AND PURCHASE OF ìGREENî BUILDINGS", 
				"5.1 GREEN BUILDINGS","5.2 ENERGY EFFICIENCY EQUIPMENT IN BUILDINGS",
				"5.3. RENEWABLE ENERGY INFRASTRUCTURE IN BUILDINGS",
				"5.4. INSTRUMENTS AND DEVICES TO ENHANCE BUILDINGSí ENERGY USE",
				'5.1 CONSTRUCTION, REFURBISHMENT AND PURCHASE OF ìGREENî BUILDINGS',
				"5.1 GREEN BUILDINGS","5.2 ENERGY EFFICIENCY EQUIPMENT IN BUILDINGS",
				"5.3. RENEWABLE ENERGY INFRASTRUCTURE IN BUILDINGS",
				'5.4. INSTRUMENTS AND DEVICES TO ENHANCE BUILDINGSí ENERGY USE',
				'CONSTRUCTION, ACQUISITION, AND RENOVATION OF BUILDINGS', 
				'ENERGY EFFICIENCY EQUIPMENT IN BUILDINGS',
				'CONSTRUCTION AND MAINTENANCE OF OTHER STRUCTURES') 
		THEN DO;
			GREENF = "Yes"; GREENF_CODE = "GFI1";
		END;
	/*Segun indicado por guias Pilar3 - LEVEL2 del GreenDasboard*/
		IF LEVEL2 
			IN ("CONSTRUCTION REFURBISHMENT OR PURCHASE OF GREEN BUILDINGS",
			"ENERGY EFFICIENCY EQUIPMENT INSTRUMENTS AND DEVICES TO IMPROVE ENERGY PERFORMANCE OF BUILDINGS",
			"RENEWABLE ENERGY INFRASTRUCTURE IN BUILDINGS",
			'CONSTRUCTION, ACQUISITION, AND RENOVATION OF BUILDINGS', 
			'ENERGY EFFICIENCY EQUIPMENT IN BUILDINGS',
			'CONSTRUCTION AND MAINTENANCE OF OTHER STRUCTURES') 
		THEN DO;
			GREENF = "Yes"; GREENF_CODE = "GFI1";
		END;
	END;
/* DUDA: øPOR QU… ESTA PARTE NO VA POR SECTOR? */

	IF CODPROD = "143" 
	AND CODSPROD = 542 
	AND EST_REF IN ("0000125") 
	AND
		(
		tipo_fina_prop IN ("C2", "F9", "M6", "76", "81", "82", "83", "84")
		OR 
		EST_REF IN ("0000099","0000115") 
		) 
	THEN DO;
			PURPOSE_ESG = "MOTOR VEHICLE LOANS"; PURPOSE_ESG_CODE = "PESG2";
	END;
	/*Segun indicado por guias Pilar3 - ACTIVIDAD P25 del GreenDasboard*/
	IF SECTOR IN (&sector_verde)
	THEN DO;
		IF LEVEL1 = "GROUND TRANSPORT"
		  THEN DO;
		 		GREENF = "Yes"; GREENF_CODE = "GFI1";
		  END;
	END;
END;
	/*BUILDING RENOVATIONS*/
	IF CODPROD = "143" 
	AND CODSPROD = 542 
	AND EST_REF IN ("0000125") 
	AND tipo_fina_prop IN ("C6", "F8", "X1", "05", "16", "41") 
	THEN DO;
		PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
	END;
	IF CODPROD = "143" 
	AND CODSPROD = 542 
	AND EST_REF IN ("0000120","0000121","0000129") 
	THEN DO;
		PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
	END;

	IF CODPROD = "143" 
	AND CODSPROD = 512 
	AND EST_REF IN ("0000049","0000050") 
	THEN DO;
		PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
	END;

	IF CODPROD = "142" 
	AND CODSPROD = 513 
	AND EST_REF IN ("0000021") 
	THEN DO;
		PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
	END;

	IF CODPROD = "123" 
	AND CODSPROD = 346 
	AND EST_REF IN ("0000001","0000002") 
	THEN DO;
		PURPOSE_ESG = "BUILDING RENOVATION LOANS"; PURPOSE_ESG_CODE = "PESG1";
	END;

	


/*BUILDING ACQUISITION LOANS*/
IF COLLATERAL IN ("REAL ESTATE. RESIDENTIAL","REAL ESTATE. COMMERCIAL") 
AND GREENF="Yes" 
THEN DO;
		PURPOSE_ESG = "BUILDING ACQUISITION"; PURPOSE_ESG_CODE = "PESG3";
END;
IF MAIN_CATEGORY_C= "MC04" AND GREENF="Yes" THEN DO;
	PURPOSE_ESG = "OTHER PURPOSE"; PURPOSE_ESG_CODE = "PESG4";
END;
END;
RUN;
**FUNDING TYPE *****************************************************************************;

DATA METRICA_3_14;
SET METRICA_3_13;
FORMAT FTYPE FTYPE_CODE $50.;

IF GREENF_CODE = "GFI1" THEN DO;

    IF LEVEL1 
	   IN  ('AGRICULTURE, FORESTRY AND LIVESTOCK',
	   		'AGRICULTURA, SILVICULTURA Y GANADERÕA',
			/* NIVELES DE OTROS REPORTES: */

			'AGRO',
			'BIODIVERSITY AND CONSERVATION PROJECT',
			'AGRICULTURE, FORESTRY AND LIVESTOCK',
			'AGRICULTURA, SILVICULTURA Y GANADERIA') 
	OR LEVEL2 
		IN 	('AGRICULTURA, SILVICULTURA Y GANADERÕA'
			 'GROWING OF CROPS',
			 'SUSTAINABLE AND OR PROTECTED AGRICULTURAL ANIMAL OPERATIONS',
			 'SUSTAINABLE AQUACULTURE',
			 /* NIVELES DE OTROS REPORTES: */
	
			 'SUSTAINABLE AGRICULTURE',
			 'PROTECTED AGRICULTURE',
			 'AFFORESTATION / REFORESTATION',
			 'LAND CONSERVATION AND REFORESTATION & SOIL REMEDIATION',
			 'ANIMAL HUSBANDRY',
			 'SUSTAINABLE AQUACULTURE',
			 'FOREST MANAGEMENT',
			 'LAND CONSERVATION AND RESTORATION',
			 'SUSTAINABLE AGRICULTURE, ANIMAL HUSBANDRY AND FISHERY',
             'AGRICULTURA, SILVICULTURA Y GANADERIA') 
    THEN DO;
        FTYPE = "AGRICULTURE, FORESTRY AND LIVESTOCK";
        FTYPE_CODE = "FUTY1";     
    END;

    ELSE 
	IF  LEVEL1 
		IN ("SUSTAINABILITY LINKED",
			"VINCULADO A SOSTENIBILIDAD") 
	OR LEVEL2 
		IN ("SUSTAINABILITY LINKED","VINCULADO A SOSTENIBILIDAD") 
	THEN DO;
        FTYPE = "SUSTAINABILITY LINKED INSTRUMENTS (SLI)";
        FTYPE_CODE = "FUTY2";
    END;

    ELSE 
	IF LEVEL1 
		IN ("BIODIVERSITY AND CONSERVATION PROJECT")
	OR LEVEL2 
		IN ("8.2 BIODIVERSITY AND CONSERVATION PROJECTS",
			"BIODIVERSITY AND CONSERVATION PROJECTS")
	 THEN DO;
        FTYPE = "BIODIVERSITY AND CONSERVATION PROJECTS";
        FTYPE_CODE = "FUTY3";
    END;
	ELSE 
	IF LEVEL1 
		IN ("WATER AND WASTE MANAGEMENT",
			'GESTI”N DE AGUA Y RESIDUOS',
			'GESTION DE AGUA Y RESIDUOS')
	OR LEVEL2 
		IN ("6. WATER AND WASTE MANAGEMENT",
			"WATER AND WASTE MANAGEMENT",
			"Water and waste management",
			"Water supply and sewage",
			"WATER SUPPLY AND SEWAGE",y
			"Waste management and remediation activities",
			'WASTE MANAGEMENT AND REMEDIATION ACTIVITIES',
			'REPARATION ACTIVITIES',
			'GESTI”N DE AGUA Y RESIDUOS',
			'GESTION DE AGUA Y RESIDUOS') 
	THEN DO;
        FTYPE = "WATER AND WASTE MANAGEMENT";
        FTYPE_CODE = "FUTY4";
    END;

	ELSE 
	IF LEVEL1 
		IN ("ENERGY","ENERGIA","ENERGÕA")

	OR LEVEL2 
		IN ("5. ENERGY",
			'ENERGY',
			'Energy',
			'Renewable energy generation',
			'Hydrogen, Bioenergy and Biogas generation',
			'Energy storage',
			'Energy efficiency equipment instruments and devices to improve energy performance of buildings',
			'RENEWABLE ENERGY PRODUCTION',
			'HYDROGEN AND BIOENERGY PRODUCTION',
			'TRANSMISSION AND DISTRIBUTION OF ELECTRICITY',
			'ENERGY STORAGE''RENEWABLE ENERGY PROCUREMENT',
			'1.1 RENEWABLE ENERGY PRODUCTION',
			'1.2 HYDROGEN AND BIOENERGY PRODUCTION',
			'1.3 TRANSMISSION AND DISTRIBUTION OF ELECTRICITY',
			'1.4 ENERGY STORAGE',
			'1.5 RENEWABLE ENERGY PROCUREMENT',
			'RENEWABLE ENERGY GENERATION',
			'HYDROGEN BIOENERGY AND BIOGAS GENERATION',
			'DISTRIBUTION AND TRANSMISSION OF ELECTRICITY, GAS AND HEAT/COOL',
			'ENERGY STORAGE',
			'RENEWABLE ENERGY PROCUREMENT',
			"ENERGIA",
			"ENERGÕA")
		THEN DO;
        FTYPE = "ENERGY";
        FTYPE_CODE = "FUTY5";
    END;

    ELSE DO
        FTYPE = "OTHERS";
        FTYPE_CODE = "FUTY6";
    END;
END;

ELSE IF GREENF_CODE = "GFI2" THEN DO;
    FTYPE= "N/A";
    FTYPE_CODE = "N/A";
END;

/*COMMENTS*/
/*IF FTYPE_CODE = "FUTY6" THEN DCON = CAT(COMPRESS(LEVEL1),";",COMPRESS(LEVEL2));*/
/*IF DCON = "BONOS VERDES" AND GREENF="GFI1" THEN DO; */
/*	IF CONTRATO NE "00491999482RHMTYKJ" THEN DO;*/
/*		FTYPE = "Others"; FTYPE_CODE = "FUTY6";*/
/*	END;*/
/*END;*/
IF PURPOSE_ESG= "BUILDING RENOVATION LOANS" THEN DO; 
      FTYPE = "OTHERS";
      FTYPE_CODE = "FUTY6";
END;
RUN;
**RISK MITIGATED CCT Y CCP************************************************************************;
*Por defecto, mitiga riesgo de transiciÛn (Policy and Legal Risk), excepto en el caso de la actividad;
*4.5. Land conservation and restoration & Soil Remediation;
*que ser· considerada que mitiga riesgo fÌsico (Chronic Risk);
DATA METRICA_3_15;
SET METRICA_3_14;
FORMAT RM_CCT RM_CCT_CODE RM_CCP RM_CCP_CODE $50.;
IF GREENF_CODE = "GFI1" THEN DO;
    RM_CCT = "Policy and legal risk";
    RM_CCT_CODE = "RMCT1";
    IF LEVEL2 IN ("LAND CONSERVATION AND RESTORATION & SOIL REMEDIATION","CONSERVACI”N Y RESTAURACI”N DE TIERRAS Y REMEDIACI”N DE SUELOS") THEN DO;
        RM_CCP = "Chronic";
        RM_CCP_CODE = "RMCP1";
    END;
    ELSE DO;
        RM_CCP = "NON MITIGATED";
        RM_CCP_CODE = "RMCP3";
    END;
END;
ELSE DO;
    RM_CCT = "N/A";
    RM_CCP = "N/A";
    RM_CCT_CODE = "N/A";
    RM_CCP_CODE = "N/A";
END;
RUN;
**ADJUSTMENT CODE*************************************************************************************;
DATA S83_4 /*(DROP= COUNTERPARTY_ENTITY RENAME= COUNTERPARTY_ENTITY1 =COUNTERPARTY_ENTITY)*/;
SET METRICA_3_15;
/*COUNTERPARTY_ENTITY1 = PUT(COUNTERPARTY_ENTITY, z5.);*/
        INFORMING_SOCIETY = "00001";
        ADJUSTMENT_CODE = "BI"||INFORMING_SOCIETY;
RUN;
**ID_COMB*****************************************************************************************;
DATA SATELITE83_GRANULAR_JUN24;
SET S83_4;
	IF GREENF_CODE NE "N/A" THEN
        GREENF_IDCOMB = ";"||GREENF_CODE;
    ELSE GREENF_IDCOMB ="";
	IF PURPOSE_ESG_CODE NE "N/A" THEN
		PURPOSE_ESG_IDCOMB = ";" || PURPOSE_ESG_CODE;
	ELSE PURPOSE_ESG_IDCOMB = "";
	IF FTYPE_CODE NE "N/A" THEN
        FTYPE_IDCOMB = ";"||FTYPE_CODE;
    ELSE FTYPE_IDCOMB ="";
	IF RM_CCT_CODE NE "N/A" THEN
        RM_CCT_IDCOMB = ";"||RM_CCT_CODE;
    ELSE RM_CCT_IDCOMB ="";
	IF RM_CCP_CODE NE "N/A" THEN
        RM_CCP_IDCOMB = ";"||RM_CCP_CODE;
    ELSE RM_CCP_IDCOMB ="";
RUN;
DATA SAVE.SATELITE83_GRANULAR_JUN24;
SET SATELITE83_GRANULAR_JUN24;
format DCON $250.;
    ID_COMB = COMPRESS(COMB_ID)||COMPRESS(PURPOSE_ESG_IDCOMB)||COMPRESS(GREENF_IDCOMB)||COMPRESS(FTYPE_IDCOMB)||COMPRESS(RM_CCT_IDCOMB)||COMPRESS(RM_CCP_IDCOMB);
IF DCON = "N/A;N/A" THEN DO; DCON = "Others";END;
RUN;
PROC SQL;
CREATE TABLE SATELITE83_SEPARADO AS
SELECT DISTINCT INFORMING_SOCIETY AS REPORTING_SOC, COUNTERPARTY_ENTITY AS COUNTERPARTY_SOC, ADJUSTMENT_CODE, id_comb, BASE_C, MAIN_CATEGORY_C, ACCOUNTING_PORTFOLIO_C, SECTOR_C, COLLATERAL_C, TYPE_OF_VALUE_C, FAIR_VALUE_DETAIL_C, PURPOSE_ESG_CODE, GREENF_CODE, FTYPE_CODE, RM_CCT_CODE, RM_CCP_CODE, SUM(AMOUNT) AS AMOUNT, DCON
FROM SAVE.SATELITE83_GRANULAR_JUN24
GROUP BY INFORMING_SOCIETY, COUNTERPARTY_ENTITY, ADJUSTMENT_CODE, COMB_ID, PURPOSE_ESG_IDCOMB, GREENF_IDCOMB, FTYPE_IDCOMB, RM_CCT_IDCOMB, RM_CCP_IDCOMB, DCON;
QUIT;

DATA SATELITE83_JUNTO;
SET SAVE.SATELITE83_GRANULAR_JUN24;
/*AJUSTES ENTRE CARGABAL Y ANEXO*/
/*SC02 siempre tiene que ir a COLL1*/
IF ID_COMB = "B01;MC02;ACPF6;SC02;COLL3;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC02;ACPF6;SC02;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC02;ACPF6;SC02;COLL3;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC02;ACPF6;SC02;COLL1;TYVA01;FVAD3;GFI2"; END; 
/*MC04 siempre tiene que ir con COLL1*/
IF ID_COMB = "B01;MC04;ACPF5;SC02;COLL4;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC02;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC02;COLL4;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC02;COLL1;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC0302;COLL2;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC0302;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC0302;COLL3;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC0302;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC0303;COLL2;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC0303;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0302;COLL2;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0302;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0302;COLL4;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0302;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC02;COLL4;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC02;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC02;COLL4;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC02;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC0302;COLL2;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC0302;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC0302;COLL4;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC0302;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF3;SC0303;COLL4;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF3;SC0303;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC02;COLL4;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC02;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC0302;COLL4;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC0302;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0303;COLL4;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0303;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF3;SC0302;COLL2;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF3;SC0302;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC0302;COLL2;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC0302;COLL1;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF2;SC0302;COLL4;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF2;SC0302;COLL1;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF3;SC0302;COLL2;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF3;SC0302;COLL1;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF3;SC0303;COLL4;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF3;SC0303;COLL1;TYVA01;FVAD1;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC0302;COLL2;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC0302;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF5;SC0302;COLL4;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF5;SC0302;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0302;COLL2;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0302;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0302;COLL4;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0302;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0303;COLL4;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0303;COLL1;TYVA01;FVAD3;GFI1;FUTY4;RMCT1;RMCP3"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC02;COLL4;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC02;COLL1;TYVA01;FVAD3;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF3;SC0302;COLL4;TYVA01;FVAD1;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF3;SC0302;COLL1;TYVA01;FVAD1;GFI2"; END;
IF ID_COMB = "B01;MC04;ACPF6;SC0303;COLL2;TYVA01;FVAD3;GFI2" THEN DO;
	ID_COMB = "B01;MC04;ACPF6;SC0303;COLL1;TYVA01;FVAD3;GFI2"; END;
	
RUN;
PROC SQL;
CREATE TABLE SATELITE83_JUNTO_2 AS
SELECT DISTINCT INFORMING_SOCIETY AS REPORTING_SOC, COUNTERPARTY_ENTITY AS COUNTERPARTY_SOC, ADJUSTMENT_CODE, ID_COMB, SUM(AMOUNT) AS AMOUNT, DCON
FROM SATELITE83_JUNTO
GROUP BY REPORTING_SOC, COUNTERPARTY_SOC, ADJUSTMENT_CODE, ID_COMB, DCON;
QUIT;
DATA SAVE.SATELITE83_PREAJUSTES_JUN24;
SET SATELITE83_JUNTO_2;
IF SUBSTR(id_comb, length(id_comb)-10, 11) = 'RMCT1;RMCP3' AND (DCON = "") THEN
    DCON = "Not only Policy and legal Risk is mitigated, a general Transition Risk is mitigated";
IF DCON="N/A;N/A" THEN 
	DCON="";
FORMAT AMOUNT 18.;
RUN;
/*%MEND;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
