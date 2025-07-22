/*
Companion code for PHUSE 2025 SM02.
Generates a Sankey figure and/or related table.
Adapted from original macros created by Shane Rosanbalm, Rho Inc., PharmaSUG 2015 DV07.
https://github.com/RhoInc/sas-sankeybarchart
Original notes within program.      

Licensed under CC BY-NC 4.0:
https://creativecommons.org/licenses/by-nc/4.0/
Free for non-commercial use with attribution.

Notes:
DATA, SUBJECT, XVAR, YVAR must be supplied.
XVAR and YVAR must be in numeric formats in n+1 order starting from 1.
99 (missing) and 999 (total) are reserved values for YVAR, and need to be renumbered in original dataset if they exist.
If rerunning, it is recommended to delete old datasets/formats first.
It is recommended to use an attribute map to color groups informatively.

Table:
If generating the sankey table, only two visits can be compared (previous XVAR (2) to current XVAR (1)).
It is recommended to filter the dataset to just the two visits of interest prior to running macro for the table.

Some sample coloring for attribute maps:
(value would be the formatted value of the YVAR values)

data attr1;

    length fillcolor value $100;

    id = 'sev';
    do; value='Grade 1' ;fillcolor="cxFFE15D";linecolor=fillcolor;output;end;
    do; value='Grade 2' ;fillcolor="cxF49D1A";linecolor=fillcolor;output;end;
    do; value='Grade 3' ;fillcolor="cxDC3535";linecolor=fillcolor;output;end;
    do; value='Grade 4' ;fillcolor="cxB01E68";linecolor=fillcolor;output;end;
    do; value='Missing';fillcolor='lightgray';linecolor=fillcolor;output;end;

    id = 'pastel';
    do; value='Grade 1' ;fillcolor="cxA9D4C7";linecolor=fillcolor;output;end;
    do; value='Grade 2' ;fillcolor="cxEDE2D6";linecolor=fillcolor;output;end;
    do; value='Grade 3' ;fillcolor="cxB6C5DB";linecolor=fillcolor;output;end;
    do; value='Grade 4' ;fillcolor="cxFACDCD";linecolor=fillcolor;output;end;
    do; value='Missing';fillcolor='lightgray';linecolor=fillcolor;output;end;

    id = 'mardi';
    do; value='Grade 1' ;fillcolor="cxD5E7B5";linecolor=fillcolor;output;end;
    do; value='Grade 2' ;fillcolor="cx72BAA9";linecolor=fillcolor;output;end;
    do; value='Grade 3' ;fillcolor="cx7E5CAD";linecolor=fillcolor;output;end;
    do; value='Grade 4' ;fillcolor="cx474E93";linecolor=fillcolor;output;end;
    do; value='Missing';fillcolor='lightgray';linecolor=fillcolor;output;end;

    run;

*/



%macro m_sankey(
   data=
   ,outlib=work
   ,sankeylib=work
   ,subject=
   ,yvar=
   ,xvar=
   ,completecases=yes
   ,yvarord=
   ,xvarord=
   ,colorlist=
   ,barwidth=0.25
   ,yfmt=
   ,xfmt=
   ,legendtitle=
   ,interpol=cosine
   ,stat=percent
   ,datalabel=yes
   ,debug=N
   ,percents=

/*  ,ttl1=*/
/*  ,ttl2=*/
/*  ,ttl3=*/
/*  ,subtitlf=*/
/*  ,subtitlt=*/
   ,whrpop=
   ,filternode=
   ,filterpop=
   ,genfig=Y
   ,gentab=N
   ,colorrep=Y
   ,attrmap=
   ,attrid=
   ,band_lbl_pos=0
   ,xvarvis=2
   ,band_color=OUTFLOW
   ,band_anno=BOTH
   );


/*

*---------- high-level overview ----------;

-  This macro creates a stacked bar chart with Sankey-like links between the stacked bars. 
   The graphic is intended to display the change over time in categorical subject endpoint 
   values. These changes are depicted by bands flowing from left to right between the stacked 
   bars. The thickness of each band corresponds to the number of subjects moving from the left 
   to the right.
   -  The first inner macro, RawToSankey, performs a data transformation. Assuming an input  
      dataset that is vertical (i.e., one record per subject and visit), the macro 
      generates two sets of counts:
      (a)   The number of subjects at each endpoint*visit combination (aka, NODES).
            E.g., how many subjects had endpoint=1 at visit=3?
      (b)   The number of subjects transitioning between endpoint categories at adjacent 
            visits (aka LINKS).
            E.g., how many subjects had endpoint=1 at visit=3 and endpoint=3 at visit=4?
      -  By default the endpoint and visit values are sorted using the ORDER=DATA principle.
         The optional parameter yvarord= and xvarord= can be used to change the display order.
   -  The second inner macro, Sankey, uses SGPLOT to generate the bar chart (using the NODES 
      dataset) and the Sankey-like connectors (using the LINKS dataset).
      -  Any ODS GRAPHICS adjustments (e.g., HEIGHT=, WIDTH=, IMAGEFMT=, etc.) should be made 
         prior to calling the macro.
      -  There are a few optional parameters for changing the appearance of the graph (colors, 
         bar width, x-axis format, etc.), but it is likely that most seasoned graphers will want 
         to further customize the resulting figure. In that case, it is probably best to simply 
         make a copy of the %Sankey macro and edit away.
   - third inner macro generates the table.

*---------- required parameters ----------;

data=             Vertical dataset to be converted to sankey structures

subject=          Subject identifier

yvar=             Categorical y-axis variable
                  Converted to values 1-N for use in plotting
                  
xvar=             Categorical x-axis variable
                  Converted to values 1-N for use in plotting

*---------- optional parameters ----------;

completecases=    Whether or not to require non-missing yvar at all xvar values for a subject
                  Valid values: yes/no.
                  Default: yes.
                  
yvarord=          Sort order for y-axis conversion, in a comma separated list
                     e.g., yvarord=%quote(red rum, george smith, tree)
                  Default sort is equivalent to ORDER=DATA
                  
xvarord=          Sort order for x-axis conversion, in a comma separated list
                     e.g., xvarord=%quote(pink plum, fred funk, grass)
                  Default sort is equivalent to ORDER=DATA

colorlist=        A space-separated list of colors: one color per yvar group.
                  Not compatible with color descriptions (e.g., very bright green).
                  Default: the qualititive Brewer palette.

barwidth=         Width of bars.
                  Values must be in the 0-1 range.
                  Default: 0.25.
                  
yfmt=             Format for yvar/legend.
                  Default: values of yvar variable in original dataset.
                  Gotcha: user-defined formats must utilize converted yvar values 1-N.

xfmt=             Format for x-axis/time.
                  Default: values of xvar variable in original dataset.
                  Gotcha: user-defined formats must utilize converted xvar values 1-N.

legendtitle=      Text to use for legend title.
                     e.g., legendtitle=%quote(Response Value)

interpol=         Method of interpolating between bars.
                  Valid values are: cosine, linear.
                  Default: cosine.

stat=             Show percents or frequencies on y-axis.
                  Valid values: percent/freq.
                  Default: percent.
                  
datalabel=        Show percents or frequencies inside each bar.
                  Valid values: yes/no.
                  Default: yes.
                  Interaction: will display percents or frequences per stat=.
                  
whrpop=           subset dataset with where clause
                  Valid values: put in %str(where <cond>)

filternode=       display filter for nodes within bar to decrease noise
                  Valid values: numeric up to 100 

filterpop=        display filter for entire bar to decrease noise
                  Valid values: numeric up to 100 

genfig=           generates the figure (turn off if only want report)
                  Valid values: Y/N
                  Default: Y

gentab=           generates the report (turn off if only want figure)
                  Valid values: Y/N
                  Default: N

colorrep=         add color to the report rows
                  Valid values: Y/N
                  Default: Y

attrmap=          attribute map
                  Valid values: name of dataset with attribute map

attrid=           attribute ID
                  Valid values: name of attribute id within attrmap

band_lbl_pos=     shifts the labels on the inflow/outflow (recommended <0.5), if they start overlapping too much
                  Valid values: numeric values
                  Default: 0

xvarvis=2         index of XVARORD to compare in the table (default=2 means the second value in a two level X order)
                  Valid values: any numeric value
                  Default: 2

band_color=       colors the bands by OUTFLOW or INFLOW category
                  Valid values: OUTFLOW/INFLOW
                  Default: OUTFLOW

band_anno=        Displays the band annotation on the OUTFLOW, INFLOW, or BOTH sides (decrease noise)
                  Valid values: OUTFLOW/INFLOW/BOTH
                  Default: BOTH

debug=            Keep work datasets.
                  Valid values: yes/no.
                  Default: no.                  
                 
-------------------------------------------------------------------------------------------------*/

  %* -----------------------------------------------------------------------------; 
  %* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
  %*                Check parameters for problematic values                       ;
  %* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
  %* -----------------------------------------------------------------------------;

  ***- Check Required parameters are non-missing -***;   

  %if &gentab=Y and &xvarvis= %then %do;
        %put %str(ERR)OR: (from &sysmacroname)- Table can only be generated for two visits. Missing XVARVIS.;
        %put %str(ERR)OR: (from &sysmacroname)- Macro will stop execution;
    %goto exit;
  %end;
    

   %if &data eq %str() or &subject eq %str() or &yvar eq %str() or &xvar eq %str() %then %do;
      %put %str(W)ARNING: SankeyBarChart -> AT LEAST ONE REQUIRED PARAMETER IS MISSING.;
      %put %str(W)ARNING: SankeyBarChart -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;


/*
*---------- high-level overview ----------;

-  The Sankey diagram macro requires data in two structures:
   -  The NODES dataset must be one record per bar segment.
   -  The LINKS dataset must be one record per connection between bar segments. 
-  This macro transforms a vertical dataset (i.e., one record per SUBJECT and XVAR) into the 
   Sankey NODES and LINKS structures.

*---------- required parameters ----------;

data=             vertical dataset to be converted to sankey structures

subject=          subject identifier

yvar=             categorical y-axis variable
                  converted to values 1-N for use in plotting
                  
xvar=             categorical x-axis variable
                  converted to values 1-N for use in plotting

*---------- optional parameters ----------;

completecases=    whether or not to require non-missing yvar at all xvar values
                  valid values: yes/no.
                  default: yes.
                  
outlib=           library in which to save NODES and LINKS datasets
                  default is the WORK library
                  
yvarord=          sort order for y-axis conversion, in a comma separated list
                     e.g., yvarord=%quote(red rum, george smith, tree)
                  default sort is equivalent to ORDER=DATA
                  
xvarord=          sort order for x-axis conversion, in a comma separated list
                     e.g., xvarord=%quote(pink plum, fred funk, grass)
                  default sort is equivalent to ORDER=DATA

-------------------------------------------------------------------------------------------------*/

/*%macro rawtosankey();*/

*99 is reserved for missing values and 999 for Total; 
   
/*add 99 to yvarord, to account for missing, so user doesnt need to pass 99*/
 %let yvarord=%str(&yvarord.,99); 
 %let _mcr_yfmt = %sysfunc(compress(&yfmt,.));

/* add 99 missing and 999 total formats to user defined formats*/
proc format library = work cntlout=_mcr_fmtinfo;
select &_mcr_yfmt;
run;
proc sql noprint;
    select strip(put(count(fmtname)+1,best.)) into :_mcr_nfmt from _mcr_fmtinfo;
    quit;

data _mcr_fmtinfo2;
    length label $200;
    set _mcr_fmtinfo;
    by fmtname start end;

    length = 20;
    default = 20;
    output;
    if last.fmtname then do;
    start=put(&_mcr_nfmt,best.);
    end=put(&_mcr_nfmt,best.);
    label='Missing';
    output;
    start=put(999,best.);
    end=put(999,best.);
    label='Total';
    output;
    end;
    run;

proc format cntlin=_mcr_fmtinfo2;
run;
   
   %local i;
   %global yvarord_gbl xvarord_gbl;
   %let yvarord_gbl=%sysfunc(countw(&yvarord,%str(,)));
   %let xvarord_gbl=%sysfunc(countw(&xvarord,%str(,)));
   %put yvarord_gbl xvarord_gbl;
   
   
   %*---------- return code ----------;
   
   %global rts;
   %let rts = 0;
   

   %*-----------------------------------------------------------------------------------------;
   %*---------- display parameter values at the top (for debugging) ----------;
   %*-----------------------------------------------------------------------------------------;
   
   %put &=data;
   %put &=subject;
   %put &=yvar;
   %put &=xvar;
   %put &=outlib;
   %put &=yvarord;
   %put &=xvarord;
    %let percents=;
   
   
   
   %*-----------------------------------------------------------------------------------------;
   %*---------- basic parameter checks ----------;
   %*-----------------------------------------------------------------------------------------;
   
   
   %*---------- dataset exists ----------;
   
   %let _dataexist = %sysfunc(exist(&data));
   %if &_dataexist = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> DATASET [&data] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   
   %*---------- variables exist ----------;
   
   %macro varexist(data,var);
      %let dsid = %sysfunc(open(&data)); 
      %if &dsid %then %do; 
         %let varnum = %sysfunc(varnum(&dsid,&var));
         %if &varnum %then &varnum; 
         %else 0;
         %let rc = %sysfunc(close(&dsid));
      %end;
      %else 0;
   %mend varexist;
   
   %if %varexist(&data,&subject) = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> VARIABLE [&subject] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if %varexist(&data,&yvar) = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> VARIABLE [&yvar] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if %varexist(&data,&xvar) = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> VARIABLE [&xvar] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   

   %*---------- eject missing yvar records ----------;
   
   data _nodes00;
      set &data;
      %if &completecases = yes %then %do;
         where not missing(&yvar);
      %end;
   run;

   data _nodes00;
    set _nodes00;
    where &whrpop;
    run;

/*   mod to create dummies to force table to match correctly*/

proc sql;
    create table dum1 as select distinct usubjid from _nodes00;
    quit;

data dum2;
    set dum1;
    do &xvar=&xvarord;
    output;
    end;
    run;
proc sort data = dum2;by  &subject &xvar;run;

   data _nodes00;
    merge dum2(in=ina) _nodes00(in=inb);
    by &subject &xvar;

    if ina and not inb then do;
        &yvar=99;
        end;
    run;


   
   %*---------- convert numeric yvar to character (for easier processing) ----------;
   
   %let dsid = %sysfunc(open(&data)); 
   %let varnum = %sysfunc(varnum(&dsid,&yvar));
   %let vartype = %sysfunc(vartype(&dsid,&varnum));
   %if &vartype = N %then %do; 
      data _nodes00;
         set _nodes00 (rename=(&yvar=_&yvar));
         &yvar = compress(put(_&yvar,best.));
         drop _&yvar;
      run;
   %end;
   %let rc = %sysfunc(close(&dsid));
   
   
   %*---------- convert numeric xvar to character (for easier processing) ----------;
   
   %let dsid = %sysfunc(open(&data)); 
   %let varnum = %sysfunc(varnum(&dsid,&xvar));
   %let vartype = %sysfunc(vartype(&dsid,&varnum));
   %if &vartype = N %then %do; 
      data _nodes00;
         set _nodes00 (rename=(&xvar=_&xvar));
         &xvar = compress(put(_&xvar,best.));
         drop _&xvar;
      run;
   %end;
   %let rc = %sysfunc(close(&dsid));
   
   
   %*---------- left justify xvar and yvar values (inelegant solution) ----------;
   
   data _nodes00;
      set _nodes00;
      &yvar = left(&yvar);
      &xvar = left(&xvar);
   run;
   
   
   %*---------- if no yvarord specified, build one using ORDER=DATA model ----------;
   
   proc sql noprint;
      select   distinct &yvar
      into     :yvar1-
      from     _nodes00
      ;
      %global n_yvar;
      %let n_yvar = &sqlobs;
      %put &=n_yvar;
   quit;
      
   %if &yvarord eq %str() %then %do;
   
      proc sql noprint;
         select   max(length(&yvar))
         into     :ml_yvar
         from     _nodes00
         ;
         %put &=ml_yvar;
      quit;
   
      data _null_;
         set _nodes00 (keep=&yvar) end=eof;
         array ordered {&n_yvar} $&ml_yvar;
         retain filled ordered1-ordered&n_yvar;
      
         *--- first record seeds array ---;
         if _N_ = 1 then do;
            filled = 1;
            ordered[filled] = &yvar;
         end;
      
         *--- if subsequent records not yet in array, add them ---;
         else do;
            hit = 0;
            do i = 1 to &n_yvar;
               if ordered[i] = &yvar then hit = 1;
            end;
            if hit = 0 then do;
               filled + 1;
               ordered[filled] = &yvar;
            end;
         end;
      
         *--- concatenate array elements into one variable ---;
         if eof then do;
            yvarord = catx(', ',of ordered1-ordered&n_yvar);
            call symputx('yvarord',yvarord);
         end;
      run;
      
   %end;

   %put &=yvarord;


   %*---------- if no xvarord specified, build one using ORDER=DATA model ----------;
   
   proc sql noprint;
      select   distinct &xvar
      into     :xvar1-
      from     _nodes00
      ;
      %global n_xvar;
      %let n_xvar = &sqlobs;
      %put &=n_xvar;
   quit;
      
   %if &xvarord eq %str() %then %do;
   
      proc sql noprint;
         select   max(length(&xvar))
         into     :ml_xvar
         from     _nodes00
         ;
         %put &=ml_xvar;
      quit;
   
      data _null_;
         set _nodes00 (keep=&xvar) end=eof;
         array ordered {&n_xvar} $&ml_xvar;
         retain filled ordered1-ordered&n_xvar;
      
         *--- first record seeds array ---;
         if _N_ = 1 then do;
            filled = 1;
            ordered[filled] = &xvar;
         end;
      
         *--- if subsequent records not yet in array, add them ---;
         else do;
            hit = 0;
            do i = 1 to &n_xvar;
               if ordered[i] = &xvar then hit = 1;
            end;
            if hit = 0 then do;
               filled + 1;
               ordered[filled] = &xvar;
            end;
         end;
      
         *--- concatenate array elements into one variable ---;
         if eof then do;
            xvarord = catx(', ',of ordered1-ordered&n_xvar);
            call symputx('xvarord',xvarord);
         end;
      run;
      
   %end;

   %put &=xvarord;


   %*---------- parse yvarord ----------;
   
   %let commas = %sysfunc(count(%bquote(&yvarord),%bquote(,)));
   %let n_yvarord = %eval(&commas + 1);
   %put &=commas &=n_yvarord;
   
   %do i = 1 %to &n_yvarord;
      %global yvarord&i;      
      %let yvarord&i = %scan(%bquote(&yvarord),&i,%bquote(,));
      %put yvarord&i = [&&yvarord&i];      
   %end;
   
   
   %*---------- parse xvarord ----------;
   
   %let commas = %sysfunc(count(%bquote(&xvarord),%bquote(,)));
   %let n_xvarord = %eval(&commas + 1);
   %put &=commas &=n_xvarord;
   
   %do i = 1 %to &n_xvarord;      
      %global xvarord&i;
      %let xvarord&i = %scan(%bquote(&xvarord),&i,%bquote(,));
      %put xvarord&i = [&&xvarord&i];      
   %end;
      
   
   %*-----------------------------------------------------------------------------------------;
   %*---------- yvarord vs. yvar ----------;
   %*-----------------------------------------------------------------------------------------;
  
   
   %*---------- put yvarord and yvar into quoted lists ----------;
   
   proc sql noprint;
      select   distinct quote(trim(left(&yvar)))
      into     :_yvarlist
      separated by ' '
      from     _nodes00
      ;
   quit;
   
   %put &=_yvarlist;
   
   data _null_;
      length _yvarordlist $2000;
      %do i = 1 %to &n_yvarord;
         _yvarordlist = trim(_yvarordlist) || ' ' || quote("&&yvarord&i");
      %end;
      call symputx('_yvarordlist',_yvarordlist);
   run;
   
   %put &=_yvarordlist;
   
   %*---------- check lists in both directions ----------;
   
   data _null_;
      array yvarord (&n_yvarord) $200 (&_yvarordlist);
      array yvar (&n_yvar) $200 (&_yvarlist);
      call symputx('_badyvar',0);
      %do i = 1 %to &n_yvarord;
         if "&&yvarord&i" not in (&_yvarlist) then call symputx('_badyvar',1);
      %end;
      %do i = 1 %to &n_yvar;
         if "&&yvar&i" not in (&_yvarordlist) then call symputx('_badyvar',2);
      %end;
   run;
   
   
   %if &_badyvar eq 2 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN yvar= [&_yvarlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN yvarord= [&_yvarordlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
      

   %*-----------------------------------------------------------------------------------------;
   %*---------- xvarord vs. xvar ----------;
   %*-----------------------------------------------------------------------------------------;
   
   
   %*---------- same number of values ----------;
   
   %if &n_xvarord ne &n_xvar %then %do;
      %put %str(W)ARNING: RawToSankey -> NUMBER OF xvarord= VALUES [&n_xvarord];
      %put %str(W)ARNING: RawToSankey -> DOES NOT MATCH NUMBER OF xvar= VALUES [&n_xvar];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- put xvarord and xvar into quoted lists ----------;
   
   proc sql noprint;
      select   distinct quote(trim(left(&xvar)))
      into     :_xvarlist
      separated by ' '
      from     _nodes00
      ;
   quit;
   
   %put &=_xvarlist;
   
   data _null_;
      length _xvarordlist $2000;
      %do i = 1 %to &n_xvarord;
         _xvarordlist = trim(_xvarordlist) || ' ' || quote("&&xvarord&i");
      %end;
      call symputx('_xvarordlist',_xvarordlist);
   run;
   
   %put &=_xvarordlist;
   
   %*---------- check lists in both directions ----------;
   
   data _null_;
      array xvarord (&n_xvarord) $200 (&_xvarordlist);
      array xvar (&n_xvar) $200 (&_xvarlist);
      call symputx('_badxvar',0);
      %do i = 1 %to &n_xvarord;
         if "&&xvarord&i" not in (&_xvarlist) then call symputx('_badxvar',1);
      %end;
      %do i = 1 %to &n_xvar;
         if "&&xvar&i" not in (&_xvarordlist) then call symputx('_badxvar',2);
      %end;
   run;
   
   %if &_badxvar eq 1 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN xvarord= [&_xvarordlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN xvar= [&_xvarlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if &_badxvar eq 2 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN xvar= [&_xvarlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN xvarord= [&_xvarordlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
      

   %*-----------------------------------------------------------------------------------------;
   %*---------- enumeration ----------;
   %*-----------------------------------------------------------------------------------------;


   %*---------- enumerate yvar values ----------;
   
   proc sort data=_nodes00 out=_nodes05;
      by &yvar;
   run;
   
   data _nodes10;
      set _nodes05;
      by &yvar;
      %do i = 1 %to &n_yvarord;
         if &yvar = "&&yvarord&i" then y = &i;
      %end;
   run;
   
   %*---------- enumerate xvar values ----------;
   
   proc sort data=_nodes10 out=_nodes15;
      by &xvar;
   run;   
   
   data _nodes20;
      set _nodes15;
      by &xvar;
      %do i = 1 %to &n_xvarord;
         if &xvar = "&&xvarord&i" then x = &i;
      %end;
   run;
   
   %*---------- subset if doing complete cases ----------;
   
   proc sql noprint;
      select   max(x)
      into     :xmax
      from     _nodes20
      ;
      %put &=xmax;
   quit;
   
   proc sql;
      create table _nodes30 as
      select   *
      from     _nodes20
      %if &completecases eq yes %then 
         group by &subject
         having   count(*) eq &xmax
         ;
      ;
   quit;

   %*---------- count subjects in case not doing complete cases ----------;

   %global subject_n;
   
   proc sql noprint;
      select   count(distinct &subject)
      into     :subject_n
      %if &completecases eq yes %then
         from     _nodes30
         ;
      %if &completecases eq no %then
         from     _nodes10
         ;      
      ;
      %put &=subject_n;
   quit;
   
   
   %*-----------------------------------------------------------------------------------------;
   %*---------- transform raw data to nodes structure ----------;
   %*-----------------------------------------------------------------------------------------;


   proc sql;
      create table _nodes40 as
      select   x, y, count(*) as size
      from     _nodes30
      group by x, y
      ;
   quit;
   
   data &outlib..nodes;
      set _nodes40;
      length xc yc $200;
      %do i = 1 %to &n_xvarord;
         if x = &i then xc = "&&xvarord&i";
      %end;
      %do i = 1 %to &n_yvarord;
         if y = &i then yc = "&&yvarord&i";
      %end;
   run;

   
   %*-----------------------------------------------------------------------------------------;
   %*---------- transform raw data to links structure ----------;
   %*-----------------------------------------------------------------------------------------;


   proc sort data=_nodes30 out=_links00;
      by &subject x;
   run;
   
   data _links10;
      set _links00;
      by &subject x;
      retain lastx lasty;
      if first.&subject then call missing(lastx,lasty);
      else if lastx + 1 eq x then do;
         x1 = lastx;
         y1 = lasty;
         x2 = x;
         y2 = y;
         output;
      end;
      lastx = x;
      lasty = y;
   run;

   proc sql noprint;
      create table &outlib..links as
      select   x1, y1, x2, y2, count(*) as thickness
      from     _links10
      group by x1, y1, x2, y2
      ;
   quit;
   
   
   %*--------------------------------------------------------------------------------;
   %*---------- clean up ----------;
   %*--------------------------------------------------------------------------------;
   
   
   %if &debug ne Y %then %do;
   
      proc datasets library=work nolist;
         delete _nodes: _links:;
      run; quit;
   
   %end;
   
   
   %*---------- return code ----------;
   %let rts = ;
   %let rts = 1;
   


/*%mend rawtosankey;*/


/*

*---------- high-level overview ----------;

-  This macro creates a stacked bar chart with sankey-like links between the stacked bars. 
   It is intended to display the change over time in subject endpoint values.
   These changes are depicted by bands flowing from left to right between the stacked bars. 
   The thickness of each band corresponds to the number of subjects moving from the left to 
   the right.
-  The macro assumes two input datasets exist: NODES and LINKS.
   -  Use the macro %RawToSankey to help build NODES and LINKS from a vertical dataset.
   -  The NODES dataset must be one record per bar segment, with variables:
      -  X and Y (the time and response), 
      -  XC and YC (the character versions of X and Y),
      -  SIZE (the number of subjects represented by the bar segment).
      -  The values of X and Y should be integers starting at 1.
      -  Again, %RawToSankey will build this dataset for you.
   -  The LINKS dataset must be one record per link, with variables:
      -  X1 and Y1 (the time and response to the left), 
      -  X2 and Y2 (the time and response to the right), 
      -  THICKNESS (the number of subjects represented by the band). 
      -  The values of X1, Y1, X2, and Y2 should be integers starting at 1.
      -  Again, %RawToSankey will build this dataset for you.
-  The chart is produced using SGPLOT. 
   -  The procedure contains one HIGHLOW statement per node (i.e., per bar segment).
   -  The procedure contains one BAND statement per link (i.e., per connecting band).
   -  The large volume of HIGHLOW and BAND statements is necessary to get color consistency in 
      v9.3 (in v9.4 we perhaps could have used attribute maps to clean things up a bit).
-  Any ODS GRAPHICS adjustments (e.g., HEIGHT=, WIDTH=, IMAGEFMT=, etc.) should be made prior to 
   calling the macro.
-  Any fine tuning of axes or other appearance options will need to be done in (a copy of) the 
   macro itself.

*---------- required parameters ----------;

There are no required parameters for this macro.

*---------- optional parameters ----------;

sankeylib=        Library where NODES and LINKS datasets live.
                  Default: WORK
                  
colorlist=        A space-separated list of colors: one color per response group.
                  Not compatible with color descriptions (e.g., very bright green).
                  Default: the qualitative Brewer palette.

barwidth=         Width of bars.
                  Values must be in the 0-1 range.
                  Default: 0.25.
                  
yfmt=             Format for yvar/legend.
                  Default: values of yvar variable in original dataset.

xfmt=             Format for x-axis/time.
                  Default: values of xvar variable in original dataset.

legendtitle=      Text to use for legend title.
                     e.g., legendtitle=%quote(Response Value)

interpol=         Method of interpolating between bars.
                  Valid values are: cosine, linear.
                  Default: cosine.

stat=             Show percents or frequencies on y-axis.
                  Valid values: percent/freq.
                  Default: percent.
                  
datalabel=        Show percents or frequencies inside each bar.
                  Valid values: yes/no.
                  Default: yes.
                  Interaction: will display percents or frequences per stat=.
                 

-------------------------------------------------------------------------------------------------*/
 


   %if &rts = 1 %then %do;
/*%macro sankey();*/



   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- some preliminaries ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;



   %*---------- localization ----------;
   
   %local i j;
   
   %*---------- deal with percents= parameter ----------;
   
   %if &percents ne %then %do;
      %put %str(W)ARNING: Sankey -> PARAMETER percents= HAS BEEN DEPRICATED.;
      %put %str(W)ARNING: Sankey -> PLEASE SWITCH TO PARAMETER datalabel=.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- dataset exists ----------;
   
   %let _dataexist = %sysfunc(exist(&sankeylib..nodes));
   %if &_dataexist = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [&sankeylib..nodes] DOES NOT EXIST.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   data nodes;
      set &sankeylib..nodes;
   run;
   
   %let _dataexist = %sysfunc(exist(&sankeylib..links));
   %if &_dataexist = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [&sankeylib..links] DOES NOT EXIST.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   data links;
      set &sankeylib..links;
   run;
   
   %*---------- variables exist ----------;
   
   %macro varexist(data,var);
      %let dsid = %sysfunc(open(&data)); 
      %if &dsid %then %do; 
         %let varnum = %sysfunc(varnum(&dsid,&var));
         %if &varnum %then &varnum; 
         %else 0;
         %let rc = %sysfunc(close(&dsid));
      %end;
      %else 0;
   %mend varexist;
   
   %if %varexist(nodes,x) = 0 or %varexist(nodes,y) = 0 or %varexist(nodes,size) = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [work.nodes] MUST HAVE VARIABLES [x y size].;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if %varexist(links,x1) = 0 or %varexist(links,y1) = 0 or %varexist(links,x2) = 0 
         or %varexist(links,y2) = 0 or %varexist(links,thickness) = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [work.links] MUST HAVE VARIABLES [x1 y1 x2 y2 thickness].;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- preliminary sorts (and implicit dataset/variable checking) ----------;
   
   proc sort data=nodes;
      by y x size;
   run;

   proc sort data=links;
      by x1 y1 x2 y2 thickness;
   run;
   
   %*---------- break apart colors ----------;

   %if &colorlist eq %str() 
      %then %let colorlist = cxa6cee3 cx1f78b4 cxb2df8a cx33a02c cxfb9a99 cxe31a1c 
                             cxfdbf6f cxff7f00 cxcab2d6 cx6a3d9a cxffff99 cxb15928;
   %let n_colors = %sysfunc(countw(&colorlist));
   %do i = 1 %to &n_colors;
      %let color&i = %scan(&colorlist,&i,%str( ));
      %put color&i = [&&color&i];
   %end;
   
   %*---------- xfmt ----------;
   
   %if &xfmt eq %str() %then %do;
   
      %let xfmt = xfmt.;
      
      proc format;
         value xfmt
         %do i = 1 %to &n_xvar;
            &i = "&&xvarord&i"
         %end;
         ;
      run;
      
   %end;
   
   %put &=xfmt;
   
   %*---------- number of rows ----------;

   proc sql noprint;
      select   max(y)
      into     :maxy
      from     nodes
      ;
   quit;
   
   %*---------- number of time points ----------;

   proc sql noprint;
      select   max(x)
      into     :maxx
      from     nodes
      ;
   quit;
   
   %*---------- corresponding text ----------;
   
   proc sql noprint;
      select   distinct y, yc
      into     :dummy1-, :yvarord1-
      from     nodes
      ;
   quit;
   
   %do i = 1 %to &sqlobs;
      %put yvarord&i = [&&yvarord&i];
   %end;
   
   %*---------- validate interpol ----------;
   
   %let _badinterpol = 0;
   data _null_;
      if      upcase("&interpol") = 'LINEAR' then call symput('interpol','linear');
      else if upcase("&interpol") = 'COSINE' then call symput('interpol','cosine');
      else call symput('_badinterpol','1');
   run;
   
   %if &_badinterpol eq 1 %then %do;
      %put %str(W)ARNING: Sankey -> THE VALUE INTERPOL= [&interpol] IS INVALID.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   


   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- convert counts to percents for nodes ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   
  
   proc freq data=nodes noprint;
      table x*y/out=_ctfhl;
      weight size;
   run;
   
   data _highlow;
      set _ctfhl;
      by x;
      node = _N_;
      retain cumpercent;
      if first.x then cumpercent = 0;
      lowpercent = cumpercent;
      highpercent = cumpercent + 100*count/&subject_n;
      cumpercent = highpercent;   
      retain cumcount;
      if first.x then cumcount = 0;
      lowcount = cumcount;
      highcount = cumcount + count;
      cumcount = highcount;   
      keep x y node lowpercent highpercent lowcount highcount;   
   run;
   
   proc sql noprint;
      select   max(node)
      into     :maxhighlow
      from     _highlow
      ;
   quit;



   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- write a bunch of highlow statements ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;



   data _highlow_statements;
      set _highlow;
      by x;
      length highlow $200 color $20 legendlabel $40 scatter $200;

      %*---------- choose color based on y ----------;
      %do c = 1 %to &maxy;
         if y = &c then color = "&&color&c";
      %end;

      %*---------- create node specific x, low, high variables and write highlow statement ----------;
      %do j = 1 %to &maxhighlow;
         %let jc = %sysfunc(putn(&j,z%length(&maxhighlow).));
         %let jro = %sysfunc(mod(&j,&maxy));
         %if &jro = 0 %then %let jro = &maxy;
         if node = &j then do;
            xb&jc = x;
            lowb&jc = lowpercent;
            %if &stat eq freq %then
               lowb&jc = lowb&jc*&subject_n/100;;
            highb&jc = highpercent;
            %if &stat eq freq %then
               highb&jc = highb&jc*&subject_n/100;;
            %if &yfmt eq %then 
               legendlabel = "&&yvarord&jro" ;
            %else %if &yfmt ne %then
               legendlabel = put(y,&yfmt.) ;
            ;
            highlow = "highlow x=xb&jc low=lowb&jc high=highb&jc / type=bar barwidth=&barwidth" ||
               " fillattrs=(color=" || trim(color) || ")" ||
               " lineattrs=(color=black pattern=solid)" ||
               " name='" || trim(color) || "' legendlabel='" || trim(legendlabel) || "';";
            *--- sneaking in a scatter statement for percent annotation purposes ---;
            mean = mean(lowpercent,highpercent);
            %if &stat eq freq %then
               mean = mean(lowcount,highcount);;
            percent = highpercent - lowpercent;
            %if &stat eq freq %then
               percent = highcount - lowcount;;
            if percent >= 1 then do;
               meanb&jc = mean;
               textb&jc = compress(put(percent,3.))/* || '%'*/;
               %if &stat eq freq %then
                  textb&jc = compress(put(percent,3.));;
               scatter = "scatter x=xb&jc y=meanb&jc / x2axis markerchar=textb&jc;";
            end;
         end;
      %end;

   run;

/*   use attribute map*/

   %if &attrmap ne %then %do;
        data _highlow_statements2;
        set _highlow_statements(rename=(highlow=old_highlow));

        highlow=scan(old_highlow,1,'/')||"/ group=legendlabel attrid=&attrid type=bar barwidth=.25 name='txt"||strip(put(y,best.))||"';";

/*    drop missing when no missing on bar*/
        if lowpercent=highpercent=100 then delete;
        run;
    %end;

    %else %do;
        data _highlow_statements2;
        set _highlow_statements;
/*    drop missing when no missing on bar*/
        if lowpercent=highpercent=100 then delete;
        run;
        %end;

   proc sql noprint;
      select   distinct trim(highlow)
      into     :highlow
      separated by ' '
      from     _highlow_statements2
      where    highlow is not missing
      ;
   quit;

   %put highlow = [%nrbquote(&highlow)];

   proc sql noprint;
      select   distinct trim(scatter)
      into     :scatter
      separated by ' '
      from     _highlow_statements2
      where    scatter is not missing
      ;
   quit;

   %put scatter = [%nrbquote(&scatter)];
   
   
   %*---------- calculate offset based on bar width and maxx ----------;
   
   data _null_;
      if &maxx = 2 then offset = 0.25;
      else if &maxx = 3 then offset = 0.15;
      else offset = 0.05 + 0.03*((&barwidth/0.25)-1);
      call symputx ('offset',offset);
   run;   



   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- convert counts to percents for links ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;



   %*---------- left edge of each band ----------;
   
   proc sql;
      create   table _links1 as
      select   a.*, b.highcount as cumthickness 
      from     links as a
               inner join _highlow (where=(highcount~=lowcount)) as b
               on a.x1 = b.x 
                  and a.y1 = b.y
      order by x1, y1, x2, y2
      ;
   quit;
   
   data _links2;
      set _links1;
      by x1 y1 x2 y2;
      link = _N_;
      xt1 = x1;
      retain lastybhigh1;
      if first.x1 then 
         lastybhigh1 = 0;
      yblow1 = lastybhigh1;
      ybhigh1 = lastybhigh1 + thickness/&subject_n;
      lastybhigh1 = ybhigh1;
      if last.y1 then
         lastybhigh1 = cumthickness/&subject_n;
   run;
   
   %*---------- right edge of each band ----------;
   
   proc sql;
      create   table _links3 as
      select   a.*, b.highcount as cumthickness 
      from     links as a
               inner join _highlow (where=(highcount~=lowcount)) as b
               on a.x2 = b.x 
                  and a.y2 = b.y
      order by x2, y2, x1, y1
      ;
   quit;
   
   data _links4;
      set _links3;
      by x2 y2 x1 y1;
      retain lastybhigh2;
      if first.x2 then 
         lastybhigh2 = 0;
      xt2 = x2;
      yblow2 = lastybhigh2;
      ybhigh2 = lastybhigh2 + thickness/&subject_n;
      lastybhigh2 = ybhigh2;
      if last.y2 then
         lastybhigh2 = cumthickness/&subject_n;
   run;
   
   %*---------- make vertical ----------;
   
   proc sort data=_links2 out=_links2b;
      by x1 y1 x2 y2;
   run;
   
   proc sort data=_links4 out=_links4b;
      by x1 y1 x2 y2;
   run;
   
   data _links5;
      merge
         _links2b (keep=x1 y1 x2 y2 xt1 yblow1 ybhigh1 link)
         _links4b (keep=x1 y1 x2 y2 xt2 yblow2 ybhigh2)
         ;
      by x1 y1 x2 y2;
   run;
   
   data _links6;
      set _links5;
      
      xt1alt = xt1 + &barwidth*0.48;
      xt2alt = xt2 - &barwidth*0.48;
      
      %if &interpol eq linear %then %do;
      
         do xt = xt1alt to xt2alt by 0.01;
            *--- low ---;
            mlow = (yblow2 - yblow1) / (xt2alt - xt1alt);
            blow = yblow1 - mlow*xt1alt;
            yblow = mlow*xt + blow;
            *--- high ---;
            mhigh = (ybhigh2 - ybhigh1) / (xt2alt - xt1alt);
            bhigh = ybhigh1 - mhigh*xt1alt;
            ybhigh = mhigh*xt + bhigh;
            output;
         end;
         
      %end;

      %if &interpol eq cosine %then %do;
      
         do xt = xt1alt to xt2alt by 0.01;
            b = constant('pi')/(xt2alt-xt1alt);
            c = xt1alt;
            *--- low ---;
            alow = (yblow1 - yblow2) / 2;
            dlow = yblow1 - ( (yblow1 - yblow2) / 2 );
            yblow = alow * cos( b*(xt-c) ) + dlow;
            *--- high ---;
            ahigh = (ybhigh1 - ybhigh2) / 2;
            dhigh = ybhigh1 - ( (ybhigh1 - ybhigh2) / 2 );
            ybhigh = ahigh * cos( b*(xt-c) ) + dhigh;
            output;
         end;
         
      %end;
      
      keep xt yblow ybhigh link y1;
   run;
   
   proc sort data=_links6;
      by link xt;
   run;
   
   %*---------- number of links ----------;

   proc sql noprint;
      select   max(link)
      into     :maxband
      from     _links6
      ;
   quit;
   
   %*---------- write the statements ----------;
   
   data _band_statements;
      set _links6;
      by link xt;
      length band $200 color $20;

      %*---------- choose color based on y1 ----------;
      %do c = 1 %to &maxy;
         if y1 = &c then color = "&&color&c";
      %end;

      %*---------- create link specific x, y variables and write series statements ----------;
      %do j = 1 %to &maxband;
         %let jc = %sysfunc(putn(&j,z%length(&maxband).));
         if link = &j then do;
            xt&jc = xt;
            yblow&jc = 100*yblow;
            %if &stat eq freq %then
               yblow&jc = yblow&jc*&subject_n/100;;
            ybhigh&jc = 100*ybhigh;
            %if &stat eq freq %then
               ybhigh&jc = ybhigh&jc*&subject_n/100;;
            band = "band x=xt&jc lower=yblow&jc upper=ybhigh&jc / x2axis transparency=0.5" || 
               " fill fillattrs=(color=" || trim(color) || ")" ||
               " ;";
/*          add band scatter*/
/*              use actual pt count to calculate within band percent*/
/*              ybhigh&jc = ybhigh&jc*&subject_n/100;; < this value gives back the pts, from 50 tot*/
/*                      but we need to know the pts in the link and get that percent*/

               if first.link or last.link then do;
                    scatterband_y&jc=mean(yblow&jc,ybhigh&jc);
                    if first.link then scatterband_x&jc=xt&jc+&band_lbl_pos;
                    else if last.link then scatterband_x&jc=xt&jc - &band_lbl_pos;
                    testval='value_to_fill';
                    scatterband= "scatter x=scatterband_x&jc y=scatterband_y&jc / x2axis markerchar=testval;";

/*                  determine if the band is going as input or output to the true x*/
                    actx=round(xt,1.);

               end;


         end;
      %end;

   run;

/*linking the band values to the ns in the node segments*/
proc sort data = _band_statements out=band2;
by actx y1 link;
run;

data band3;
    set band2;
    by actx y1 link;

    if ^missing(actx) then do;

        sumpts=(ybhigh-yblow)*&subject_n.;

        if first.y1 then cumptx=0;
        cumpts+sumpts;

        end;
    run;


data den1;
    set _highlow;
    denom=highcount-lowcount;
    run;

proc transpose data = den1 out =den_low prefix=low;
by x;
id y;
var lowpercent;
run;
proc transpose data = den1 out =den_high prefix=high;
by x;
id y;
var highpercent;
run;
proc transpose data = den1 out =den_denom prefix=denom;
by x;
id y;
var denom;
run;

data den2;
    merge den_low(drop=_name_) den_high(drop=_name_) den_denom(drop=_name_);
    by x;
    rename x=actx;
    run;

proc sort data = band3;by actx;run;
data band4;
    merge band3 den2;
    by actx;
    run;

data band5;
    set band4;

    array low{*} low1-low&n_yvar.;
    array high{*} high1-high&n_yvar.;
    array den{*} denom1-denom&n_yvar.;

    do i = 1 to dim(low);

        if nmiss(low{i},ybhigh,high{i})=0 then do;
        if . < round(low{i},.000001)+0.02 <= round(ybhigh*100,.000001) <= round(high{i},.000001)+0.02/*need buffer to capture edge: using +.02 now but maybe too big?*/ then do;

            testlow=round(low{i},.0001);
            testyb=round(ybhigh*100,.0001);
            testhi=round(high{i},.0001);
            trueybar=i;
            denom=den{i};
            leave;
            end;
        end;
        end;

/*      calculate input/output percentage*/
    if ^missing(trueybar) then do;
        perc=put(round(sumpts/denom*100),3.);
        percn=round(sumpts/denom*100,.1);
        ct=put(sumpts,3.);


/*      optional parameter to remove percentages when the value is <z within the node segment*/
        %if &filternode. ne %then %do;
            if input(perc,best.)<&filternode. then do;perc='';ct='';end;
                        %end;

/*      optional parameter to remove percentages when the value is <z within the whole population*/
        %if &filterpop. ne %then %do;;
            if sumpts/&subject_n*100 < &filterpop then do;perc='';ct='';end;
                        %end;

/*      replace testval placeholder with actual percent of input/output*/
        %if &stat=percent %then %do;scatterband2=tranwrd(scatterband,'testval','perc');%end;
        %else %if &stat=freq %then %do;scatterband2=tranwrd(scatterband,'testval','ct');%end;
        end;
    run;

/*resort to draw bands correctly*/
proc sort data = band5;
      by link xt;
      run;


/*   use attribute map*/
   %if &attrmap ne %then %do;
        data band5_2;
    set band5(rename=(band=old_band));

    band=scan(old_band,1,'/')||"/ group=legendlabel attrid=&attrid x2axis transparency=0.5 fill;";


    laglnk=lag(link);
    if link ne laglnk then ord+1;
        run;
    %end;

    %else %do;
        data band5_2;
        set band5;
        

    laglnk=lag(link);
    if link ne laglnk then ord+1;

        run;
        %end;


/*   color band to destination target*/
data dest_color;
    set band5_2;
    if ^missing(ct);
    keep y1 link xt ct trueybar denom laglnk ord scatterband2;
    run;

data dest2;
    set dest_color;
    by ord y1 link xt;
    if last.ord;

    drop xt;

    length legendlabel_dest $200;
    legendlabel_dest=put(trueybar,&yfmt.);
    run;

/*switch inflow outflow scatterplotting*/
data sctr_switch;
    set dest_color;
    by ord y1 link xt;

    %if &band_anno=OUTFLOW %then %do;
        if last.ord then scatterband_dest='OFF';
        %end;
    %if &band_anno=INFLOW %then %do;
        if first.ord then scatterband_dest='OFF';
        %end;
    run;



data band5_3;
    merge band5_2 dest2(keep=ord y1 link legendlabel_dest) ;
    by ord y1 link;

    %if &band_color=INFLOW %then %do;
        band=tranwrd(band,'legendlabel','legendlabel_dest');
        %end;

    run;

    option minoperator;
%if &band_anno in (OUTFLOW INFLOW) %then %do;
data band5_3x;
    merge band5_3 sctr_switch(keep=ord y1 xt link scatterband_dest);
    by ord y1 link xt;

    RUN;

data band5_4;
    set band5_3x;

    if scatterband_dest='OFF' then delete;

    run;

    %end;
%else %do;
    data band5_4;
        set band5_3;
        run;
    %end;




   proc sql noprint;
      select   distinct trim(band)
      into     :band
      separated by ' '
      from     band5_4
      where    band is not missing
      ;
      select   distinct trim(scatterband2)
      into     :scatterband
      separated by ' '
      from     band5_4
      where    scatterband2 is not missing
      ;
   quit;

   %put band = [%nrbquote(&band)];
   %put scatterband = [%nrbquote(&scatterband)];
   
                     
   
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- plot it ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   
   
   data _all(rename=(newy=y));
      set _highlow_statements2  band5_4;

      newy=ifn(^missing(y),y,y1);

      legendlabel=put(newy,&yfmt.);
      drop y y1;

   run;
   
        %if &genfig=Y %then %do;
              
/*    modify paging/titles as needed for company standard*/
/*          title1 j=c "&ttl1";*/
/*          title2 j=c "&ttl2";*/
/*          title3 j=c "&ttl3";*/
/*          %if %length(&subtitlf)>0 %then %do; */
/*              title7 j=c "&subtitlf"; */
/*              %end;*/

           proc sgplot data=_all noautolegend  %if &attrmap ne %then %do; dattrmap=&attrmap %end;;
/*           by page;*/
              %*---------- plotting statements ----------;
              &band;
              &scatterband;
              &highlow;
              %if &datalabel eq yes %then &scatter;;
              %*---------- axis and legend statements ----------;
              x2axis display=(nolabel noticks) min=1 max=&maxx integer offsetmin=&offset offsetmax=&offset 
                 tickvalueformat=&xfmt;
              xaxis display=none type=discrete offsetmin=&offset offsetmax=&offset 
                 tickvalueformat=&xfmt;
              yaxis offsetmin=0.02 offsetmax=0.02 grid 
                 %if &stat eq freq %then label="Frequency";
                 %else label="Proportion (%)";
                 ;

                 %if &attrmap= %then %do;
              keylegend %do i = 1 %to &maxy; "&&color&i" %end; / title="&legendtitle";
                %end;
            %else %do;
                keylegend 'txt1' /title="&legendtitle";
                %end;
           run;
           
    %end;




/*%mend sankey;*/
%end;





   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- create table of percents ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;

/**/
/*create true counts of categories since some may be missing and n_var will be off*/


/*%macro sankey_tab();*/


   %if &gentab=Y %then %do;

/*need to renumber yvarord_gbl, because sankey will start all values from 1*/
/*so if missing numbers, then order will be messed up*/

    data _null_;
    set nodes;
    call symputx("xy_"||strip(put(x,best.))||"_"||strip(put(y,best.)),size);
    run;
    proc sql noprint;
        create table total_row as select x1,y1,999 as x2, 999 as y2,sum(thickness) as thickness
        from links
        group by x1,y1;
        create table total_col as select x1,y2,999 as x2, 999 as y1,sum(thickness) as thickness
        from links
        group by x1,y2;

        select sum(thickness) into :ntot from links where x1=&xvarvis-1;
        quit;

/*      total should be non-missing for table*/
    data total;
        x1=999;
        x2=x1;
        y1=x1;
        y2=x1;
        thickness=&ntot;
        run;

    data tab0;
        set links(in=ina) total_row(in=row) total_col(in=col) total(in=tot);
        by x1 y1;

        if ina then do;
        outputN=input(resolve('&xy_'||strip(put(x1,best.))||"_"||strip(put(y1,best.))),best.);
        inputN=input(resolve('&xy_'||strip(put(x2,best.))||"_"||strip(put(y2,best.))),best.);
        end;

        else if row then do;
        outputN=thickness;
        inputN=&ntot;
        end;

        else if col then do;
        outputN=&ntot;
        inputN=thickness;
        end;

        else if tot then do;
        outputN=thickness;
        inputN=thickness;
        end;

        rowpct='('||put(round(thickness/outputN*100,.1),5.1)||')';
        colpct='('||put(round(thickness/inputN*100,.1),5.1)||')';
        count=put(thickness,3.);

        tabpct=count||' '||rowpct||' '||colpct;

        run;

    data dummy;
        do x1=&xvarvis-1;*change to use formatshifted value if needed;
            do y1=1 to &yvarord_gbl,999;
                do y2=1 to &yvarord_gbl,999;
                do x2=&xvarvis,999;
                    output;
                end;
                end;
            end;
        end;
    run;
    data dummy2;
        set dummy;

        if y2 eq 999 and x2 ne 999 then delete;
        run;
    proc sort data = dummy2;by x1 y1 x2 y2;run;
    proc sort data = tab0;by x1 y1 x2 y2;run;

    data tab0d;
        merge dummy2 (in=ina) tab0;
        by x1 y1 x2 y2;

        if ina
    or  (x1=999 and x2=999 and y1=999 and y2=999) 
;*control which visit is pulled for the table;

        run;

    data tab0dx;
        set tab0d;
        run;

    proc transpose data = tab0d out = tab1ct prefix=ct;
    by x1 y1 x2;
    var count;
    id y2;
    run;
    proc transpose data = tab0d out = tab1r prefix=row;
    by x1 y1 x2;
    var rowpct;
    id y2;
    run;
    proc transpose data = tab0d out = tab1col prefix=col;
    by x1 y1 x2;
    var colpct;
    id y2;
    run;
 
    data tab1x;
        merge   tab1ct
                tab1r
                tab1col
    ;
    by x1 y1 x2;
    run;


    data tab2;
        set tab1x;
        by x1 y1;


        array arr{*} ct: row: col:;

        do i=1 to dim(arr);
            if missing(arr{i}) then arr{i}=put(0,3.);
        end;
        run;

    proc sort data = tab2;by y1;run;


data tab2_inner;
    set tab2;
    where y1 ne 999 and x2 ne 999;
    drop ct999 row999 col999 x1;
    run;

data tab2_outer_row;
    set tab2;
    where  x2 eq 999 and y1 ne 999;
    keep y1 x2 ct999 row999 col999;
    run;
/*add total columns*/
data tab2x;
    merge tab2_inner tab2_outer_row;
    by y1;
    run;


data tab2_outer_col_spec;
    set tab2;
    where y1 eq 999 and x2=999 and x1 ne 999;
    drop ct999 row999 col999 ;
    run;
data tab2_outer_col_tot;
    set tab2;
    where y1 eq 999 and x2=999 and x1 eq 999;
    keep y1 x2 ct999 row999 col999;
    run;
data tab2_outer_col;
    merge tab2_outer_col_spec tab2_outer_col_tot;
    by y1 x2;
    run;


data tab3;
    set tab2x tab2_outer_col;
    run;



    proc sort data = tab3;
        by y1;
        run;

    data report;
        length rowtxt $200;
        set tab3;
        

        if y1=999 then rowtxt='Total';
        else rowtxt=put(y1,&yfmt.);

        page=.;
        dum='';

        keep y1 page dum rowtxt ct: row: col:;
        run;
    
            /*  autogenerate header data*/
            proc sql;
                create table headers as select distinct y1,rowtxt from report;
                quit;
            proc transpose data = headers out = head1;
            var rowtxt;
            id y1;
            run;

            data head2;
                length _999 $200;
                set head1;

                if _999='*' then _999='Total';

                %macro fill();
                array arr{*} _:;
                %do i = 1 %to &yvarord_gbl;

                colcall&i="('"||strip(_&i.)||" (*ESC*)\brdrb\brdrs\brdrw7' ct&i row&i col&i)";

                %end;

                colcall999="('"||strip(_999)||" (*ESC*)\brdrb\brdrs\brdrw7' ct999 row999 col999)";
                %mend fill;
                %fill;

                call symput('colcall',"('Category (*ESC*)\brdrb\brdrs\brdrw7' "||catx(' dum ',of colcall:)||")");
                run;
            %put &colcall;

            /*color rows via attribute map, if needed*/
            %if &attrmap ne %then %do;
                data rowcolor;
                    set &attrmap;
                    where id="&attrid";

                    length rowcolor $200;
                    rowcolor="if rowtxt='"||strip(value)||"' then call define(_row_,'style','style={borderbottomcolor="||strip(fillcolor)||" borderrightcolor="||strip(fillcolor)||" background="||strip(fillcolor)||"}');";
                    run;

                proc transpose data = rowcolor out=rowcolor2;
                var rowcolor;
                run;

            data _null_;
                set rowcolor2;

/*              set total column to white*/
                call symput('rowcolor',catx(' ',of col:));
                run;
                %put &rowcolor;

            %end;

proc sort data = report;
              by page y1 rowtxt;
    run;
    

%if &gentab=Y %then %do;

/*modify paging/titles to company standard as needed;*/
/*          title1 j=c "&ttl1";*/
/*          title2 j=c "&ttl2";*/
/*          title3 j=c "&ttl3";*/
/*          %if %length(&subtitlf)>0 %then %do; */
/*              title7 j=c "&subtitlf"; */
/*              %end;*/
/*          title6 j=c "&subtitlt";*/
/*          title5 j=c "#byval3";*/

            proc report data=report split='|' missing spacing=1 NOFS nowd 
                      style(header)=[protectspecialchars=off] 
                      style(column)=[protectspecialchars=off] ;     

            ;

/*              by page  ;*/

              columns /*page*/ y1 rowtxt &colcall.;

/*              define page       /order order=internal noprint;*/
              define y1     / order order=internal noprint;
              define rowtxt     / style(column)=[cellwidth=12% just=l asis=on] style(header)=[just=l asis=on] "Previous|Category"; 

              %macro cyc;

              %do i=1 %to &yvarord_gbl.;

              define ct&i.      / style(column)=[cellwidth=4% just=c asis=on] style(header)=[just=c asis=on] "n"; 
              define row&i.     / style(column)=[cellwidth=6% just=c asis=on] style(header)=[just=c asis=on] "Row %"; 
              define col&i.     / style(column)=[cellwidth=6% just=c asis=on] style(header)=[just=c asis=on] "Col %"; 
              define dum        / style(column)=[cellwidth=1% just=l asis=on] style(header)=[just=l asis=on] ""; 

              %end;
              %mend cyc;

              %cyc;

              define ct999      / style(column)=[cellwidth=4% just=c asis=on] style(header)=[just=c asis=on] "n"; 
              define row999     / style(column)=[cellwidth=6% just=c asis=on] style(header)=[just=c asis=on] "Row %"; 
              define col999     / style(column)=[cellwidth=6% just=c asis=on] style(header)=[just=c asis=on] "Col %"; 
              define dum        / style(column)=[cellwidth=1% just=l asis=on] style(header)=[just=l asis=on] ""; 


              break after page/page;

              %if &colorrep=Y and &attrmap ne %then %do;
              compute rowtxt;
                    &rowcolor.;;
                endcomp;

/*              leave total column un-highlighted*/
            compute ct999;call define(_col_,'style','style={background=lightgray borderbottomcolor=lightgray borderrightcolor=lightgray}');endcomp;
            compute row999;call define(_col_,'style','style={background=lightgray borderbottomcolor=lightgray borderrightcolor=lightgray}');endcomp;
            compute col999;call define(_col_,'style','style={background=lightgray borderbottomcolor=lightgray borderrightcolor=lightgray}');endcomp;

                %end;

            run;
%end;
%end;

/*%mend sankey_tab;*/


  /***--- Exit label when Err|or encountered ---***/
  %exit:
  ;data _null_;
    run;
  %put &sysmacroname : ending ;


   %mend m_sankey;
