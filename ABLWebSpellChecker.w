&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

BLOCK-LEVEL ON ERROR UNDO, THROW.     

USING OpenEdge.Core.String.  
USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING OpenEdge.Net.URI.
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.UriSchemeEnum.     
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* DEFINE QUERY qSpellCheck FOR ttWordMatch */

DEFINE INPUT-OUTPUT PARAMETER opcSourceText AS CHARACTER.

DEFINE VARIABLE giOffSetPosition AS INTEGER INITIAL 0 NO-UNDO.

DEFINE TEMP-TABLE ttLanguages no-undo
    field langName AS CHARACTER
    field langCode AS CHARACTER
    field longCode AS CHARACTER.
    
    
        
/*             CREATE ttWordMatch.                                               */
/*                                                                               */
/*             ASSIGN                                                            */
/*                 ttWordMatch.STATUSMessage =  oMatch:GetCharacter("message")   */
/*                 ttWordMatch.WordStartPos  =  oMatch:GetInteger("offset") + 1. */
/*                 ttWordMatch.WordEndPos    =  oMatch:GetInteger("length").     */
               
    
DEFINE TEMP-TABLE ttWordMatch NO-UNDO
    FIELD  StatusMessage AS CHARACTER
    FIELD  WordStartPos  AS INTEGER
    FIELD  WordEndPos    AS INTEGER.
    
    
/* CREATE ttWordSuggestion.                                                                */
/*                                                                                         */
/*                 ASSIGN                                                                  */
/*                     ttWordSuggestion.WordMatch_ID = ROWID(ttWordMatch)                         */
/*                     ttWordSuggestion.SuggestWord =  oReplacement:GetCharacter("value"). */
/*                     .                                                                   */
    
    DEFINE TEMP-TABLE ttWordSuggestion NO-UNDO
        FIELD WordMatch_ID AS ROWID
        FIELD SuggestWord AS CHARACTER.
    
/*     DEFINE TEMP-TABLE software NO-UNDO                                                                           */
/*         FIELD name AS CHARACTER                                                                                  */
/*         FIELD version AS CHARACTER                                                                               */
/*         FIELD buildDate AS CHARACTER                                                                             */
/*         FIELD apiVersion AS INTEGER                                                                              */
/*                 XML-DATA-TYPE "byte"                                                                             */
/*         FIELD premium AS CHARACTER                                                                               */
/*         FIELD premiumHint AS CHARACTER                                                                           */
/*         FIELD status1 AS CHARACTER                                                                               */
/*                 XML-NODE-NAME "status" .                                                                         */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE warnings NO-UNDO                                                                               */
/*         FIELD incompleteResults AS CHARACTER .                                                                   */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE language NO-UNDO                                                                               */
/*         FIELD name AS CHARACTER                                                                                  */
/*         FIELD code AS CHARACTER .                                                                                */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE detectedLanguage NO-UNDO                                                                       */
/*         FIELD name AS CHARACTER                                                                                  */
/*         FIELD code AS CHARACTER                                                                                  */
/*         FIELD language_id AS RECID                                                                               */
/*                 XML-NODE-TYPE "HIDDEN" .                                                                         */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE matches NO-UNDO                                                                                */
/*         FIELD message1 AS CHARACTER                                                                              */
/*                 XML-NODE-NAME "message"                                                                          */
/*         FIELD shortMessage AS CHARACTER                                                                          */
/*         FIELD offset AS INTEGER                                                                                  */
/*                 XML-DATA-TYPE "byte"                                                                             */
/*         FIELD length AS INTEGER                                                                                  */
/*                 XML-DATA-TYPE "byte"                                                                             */
/*         FIELD sentence AS CHARACTER .                                                                            */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE replacements NO-UNDO                                                                           */
/*         FIELD value1 AS CHARACTER                                                                                */
/*                 XML-NODE-NAME "value"                                                                            */
/*         FIELD matches_id AS RECID                                                                                */
/*                 XML-NODE-TYPE "HIDDEN" .                                                                         */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE context NO-UNDO                                                                                */
/*         FIELD text1 AS CHARACTER                                                                                 */
/*                 XML-NODE-NAME "text"                                                                             */
/*         FIELD offset AS INTEGER                                                                                  */
/*                 XML-DATA-TYPE "byte"                                                                             */
/*         FIELD length AS INTEGER                                                                                  */
/*                 XML-DATA-TYPE "byte"                                                                             */
/*         FIELD matches_id AS RECID                                                                                */
/*                 XML-NODE-TYPE "HIDDEN" .                                                                         */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE type NO-UNDO                                                                                   */
/*         FIELD typeName AS CHARACTER                                                                              */
/*         FIELD matches_id AS RECID                                                                                */
/*                 XML-NODE-TYPE "HIDDEN" .                                                                         */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE rule NO-UNDO                                                                                   */
/*         FIELD id AS CHARACTER                                                                                    */
/*         FIELD description AS CHARACTER                                                                           */
/*         FIELD issueType AS CHARACTER                                                                             */
/*         FIELD matches_id AS RECID                                                                                */
/*                 XML-NODE-TYPE "HIDDEN" .                                                                         */
/*                                                                                                                  */
/* DEFINE TEMP-TABLE category NO-UNDO                                                                               */
/*         FIELD id AS CHARACTER                                                                                    */
/*         FIELD name AS CHARACTER                                                                                  */
/*         FIELD rule_id AS RECID                                                                                   */
/*                 XML-NODE-TYPE "HIDDEN" .                                                                         */
/*                                                                                                                  */
/* DEFINE DATASET spellcheck   SERIALIZE-HIDDEN                                                                     */
/*         FOR software, warnings, language, detectedLanguage, matches, replacements, context, type, rule, category */
/*         PARENT-ID-RELATION RELATION1 FOR language, detectedLanguage                                              */
/*                 PARENT-ID-FIELD language_id                                                                      */
/*         PARENT-ID-RELATION RELATION2 FOR matches, replacements                                                   */
/*                 PARENT-ID-FIELD matches_id                                                                       */
/*                 PARENT-FIELDS-BEFORE (message1,shortMessage)                                                     */
/*                 PARENT-FIELDS-AFTER (offset,length)                                                              */
/*         PARENT-ID-RELATION RELATION3 FOR matches, context                                                        */
/*                 PARENT-ID-FIELD matches_id                                                                       */
/*                 PARENT-FIELDS-AFTER (sentence)                                                                   */
/*         PARENT-ID-RELATION RELATION4 FOR matches, type                                                           */
/*                 PARENT-ID-FIELD matches_id                                                                       */
/*         PARENT-ID-RELATION RELATION5 FOR rule, category                                                          */
/*                 PARENT-ID-FIELD rule_id                                                                          */
/*         PARENT-ID-RELATION RELATION6 FOR matches, rule                                                           */
/*                 PARENT-ID-FIELD matches_id.                                                                      */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 cbLanguage edSource slWordSuggestion ~
bntOk bntCancel fiStatusMessage 
&Scoped-Define DISPLAYED-OBJECTS cbLanguage edSource slWordSuggestion ~
fiStatusMessage 

/* Custom List Definitions                                              */
/* BUTTONS,List-2,List-3,List-4,List-5,List-6                           */
&Scoped-define BUTTONS bntIgnoreOnce bntIgnoreAll slWordSuggestion ~
bntChange bntPrev 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bntCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bntChange 
     LABEL "Change" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bntIgnoreAll 
     LABEL "Ignore All" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bntIgnoreOnce 
     LABEL "Ignore Once" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bntOk 
     LABEL "Ok" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bntPrev 
     LABEL "Change All" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cbLanguage AS CHARACTER FORMAT "X(256)":U 
     LABEL "Text Language" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "none","none"
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE edSource AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 61 BY 6.25 NO-UNDO.

DEFINE VARIABLE fiStatusMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 76.5 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 13.25.

DEFINE VARIABLE slWordSuggestion AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 60.5 BY 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbLanguage AT ROW 1.75 COL 16 COLON-ALIGNED WIDGET-ID 2
     edSource AT ROW 3.75 COL 3.5 NO-LABEL WIDGET-ID 4
     bntIgnoreOnce AT ROW 3.75 COL 65.5 WIDGET-ID 36
     bntIgnoreAll AT ROW 5 COL 65.5 WIDGET-ID 38
     slWordSuggestion AT ROW 11 COL 3.5 NO-LABEL WIDGET-ID 16
     bntChange AT ROW 11 COL 65.5 WIDGET-ID 20
     bntPrev AT ROW 12.25 COL 65.5 WIDGET-ID 26
     bntOk AT ROW 16.75 COL 50 WIDGET-ID 34
     bntCancel AT ROW 16.75 COL 66.5 WIDGET-ID 32
     fiStatusMessage AT ROW 3 COL 1.75 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     "https://languagetool.org/" VIEW-AS TEXT
          SIZE 22.5 BY .75 AT ROW 17 COL 2.5 WIDGET-ID 44
          FGCOLOR 9 
     "Suggestions" VIEW-AS TEXT
          SIZE 11.5 BY .63 AT ROW 10.25 COL 3.5 WIDGET-ID 40
     RECT-1 AT ROW 3.25 COL 2 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.25 BY 17.13 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Web Spell Checker"
         HEIGHT             = 17.13
         WIDTH              = 82.25
         MAX-HEIGHT         = 64.56
         MAX-WIDTH          = 480
         VIRTUAL-HEIGHT     = 64.56
         VIRTUAL-WIDTH      = 480
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bntChange IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bntIgnoreAll IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bntIgnoreOnce IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bntPrev IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       edSource:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiStatusMessage:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR SELECTION-LIST slWordSuggestion IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Web Spell Checker */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Web Spell Checker */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntCancel C-Win
ON CHOOSE OF bntCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntChange C-Win
ON CHOOSE OF bntChange IN FRAME DEFAULT-FRAME /* Change */
DO:
    RUN changeWord          IN THIS-PROCEDURE.
    RUN correctNextSpelling IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntIgnoreAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntIgnoreAll C-Win
ON CHOOSE OF bntIgnoreAll IN FRAME DEFAULT-FRAME /* Ignore All */
DO:
    MESSAGE "Under Development".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntIgnoreOnce
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntIgnoreOnce C-Win
ON CHOOSE OF bntIgnoreOnce IN FRAME DEFAULT-FRAME /* Ignore Once */
DO:
    RUN correctNextSpelling IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntOk C-Win
ON CHOOSE OF bntOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  
  DO WITH FRAME {&FRAME-NAME}:
    opcSourceText = edSource:SCREEN-VALUE.
  END.
  
  APPLY "CLOSE" TO THIS-PROCEDURE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLanguage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLanguage C-Win
ON VALUE-CHANGED OF cbLanguage IN FRAME DEFAULT-FRAME /* Text Language */
DO:
    RUN spellCheck IN THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slWordSuggestion
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  RUN Initialise IN THIS-PROCEDURE.
  
  RUN spellCheck IN THIS-PROCEDURE.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeWord C-Win 
PROCEDURE changeWord PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cOrgSourceWord AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE cNewTargetWord AS CHARACTER     NO-UNDO.

  DO WITH FRAME {&frame-name}:
  
  
         cOrgSourceWord = edSource:SELECTION-TEXT. 
         cNewTargetWord = slWordSuggestion:SCREEN-VALUE.
         
         RUN updateWordOffset(INPUT cOrgSourceWord,
                              INPUT cNewTargetWord).  
  
         edSource:REPLACE-SELECTION-TEXT( cNewTargetWord ).
                  
  END.
  
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE correctNextSpelling C-Win 
PROCEDURE correctNextSpelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE inNewLines AS INTEGER     NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    
        DISABLE 
            {&BUTTONS}.
            
        /** Clear the words. **/
        slWordSuggestion:LIST-ITEMS  = ''.
        fiStatusMessage:SCREEN-VALUE = ''.

        FIND NEXT ttWordMatch 
            NO-ERROR.
    
        IF NOT AVAILABLE ttWordMatch THEN
        DO:
            MESSAGE "Spell Check Complete"
                VIEW-AS ALERT-BOX INFO.
            
            RETURN.
        END.
            
        
        ENABLE
            {&BUTTONS}.
            
        /* Count the number of lines  */    
        inNewLines = NUM-ENTRIES( SUBSTRING( edSource:SCREEN-VALUE, 1, ttWordMatch.WordStartPos), '~n' ) - 1 .
    
        edSource:SET-SELECTION (ttWordMatch.WordStartPos + giOffSetPosition + inNewLines, ttWordMatch.WordEndPos + giOffSetPosition + inNewLines).                   
        
        fiStatusMessage:SCREEN-VALUE = ttWordMatch.StatusMessage. 
              
        
        
        FOR EACH ttWordSuggestion
            WHERE ttWordSuggestion.WordMatch_ID EQ ROWID(ttWordMatch):
            slWordSuggestion:ADD-LAST( ttWordSuggestion.SuggestWord ).    
        END.
        
        /** Select the first Entry **/
        slWordSuggestion:SCREEN-VALUE = slWordSuggestion:ENTRY(1).
            
    END.        
    
    RETURN.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cbLanguage edSource slWordSuggestion fiStatusMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 cbLanguage edSource slWordSuggestion bntOk bntCancel 
         fiStatusMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKnownLanguages C-Win 
PROCEDURE getKnownLanguages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise C-Win 
PROCEDURE Initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FILE-INFO:FILE-NAME = SESSION:TEMP-DIRECTORY.
    
    
    IF SEARCH(FILE-INFO:FULL-PATHNAME + '\Languages.xml') EQ ? THEN
        RUN getKnownLanguages IN THIS-PROCEDURE.
    
    
    TEMP-TABLE ttLanguages:READ-XML("FILE", FILE-INFO:FULL-PATHNAME + '\Languages.xml', "EMPTY", ? , ?).  
                                  

    DO WITH FRAME {&FRAME-NAME}:
        cbLanguage:DELETE(1).
        
        edSource:SCREEN-VALUE = opcSourceText.
        
        FOR EACH ttLanguages:
            cbLanguage:ADD-LAST(ttLanguages.langName, ttLanguages.longCode)  .
        END.
        
         cbLanguage:SCREEN-VALUE = "en-GB".
         
         
    END.
    
    
    
    
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spellCheck C-Win 
PROCEDURE spellCheck PRIVATE :
DEFINE VARIABLE oURI       AS URI           NO-UNDO.
    DEFINE VARIABLE oRequest   AS IHttpRequest  NO-UNDO.
    DEFINE VARIABLE oResponse  AS IHttpResponse NO-UNDO.
    DEFINE VARIABLE oClient    AS IHttpClient   NO-UNDO.
    DEFINE VARIABLE oInputData AS JsonObject    NO-UNDO.
    DEFINE VARIABLE oJSONData  AS JsonObject     NO-UNDO.
    DEFINE VARIABLE oMatchesArray   AS JsonArray     NO-UNDO.
    DEFINE VARIABLE oReplacementArray AS JsonArray   NO-UNDO.
    DEFINE VARIABLE oMatch    AS JsonObject     NO-UNDO.
    DEFINE VARIABLE oReplacement    AS JsonObject     NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
            DEFINE VARIABLE iWordStartPos AS INTEGER     NO-UNDO.
            DEFINE VARIABLE iWordEndPos   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNumReplacements AS INTEGER     NO-UNDO.
    
    
    DEFINE VARIABLE oRequestBody AS String NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    
        SESSION:SET-WAIT-STATE("GENERAL").
        
        EMPTY TEMP-TABLE ttWordMatch.
        EMPTY TEMP-TABLE ttWordSuggestion.
    
        ASSIGN
            cbLanguage
            edSource.
        
        /* Create inputdata */
        oInputData = NEW JsonObject().
        oInputData:add("text":U, edSource).
        oInputData:add("language":U, cbLanguage).
        oInputData:add("enabledOnly":U, "false").

        /* Initialize URI */
        ASSIGN
            oURI      = NEW URI("https":U, "languagetool.org")
            oURI:Path = "/api/v2/check".
            
        oRequestBody = new String( SUBSTITUTE('text=&1&&language=&2&&enabledOnly=false',edSource,cbLanguage) ).

        ASSIGN 
            oRequest = RequestBuilder:Post(oURI, oRequestBody):ContentType('application/x-www-form-urlencoded'):AcceptJson():Request.
                                 
        /* Build Http Client */
        oClient = ClientBuilder:Build():Client.

        /* Build Http Response placeholder */
        oResponse = ResponseBuilder:Build():Response.
         
        /* Execute request and store response in placeholder */
        oClient:Execute(oRequest, oResponse).
           

                                                  
/*         MESSAGE "Statuscode  : " oResponse:StatusCode SKIP */
/*                 "ContentType : " oResponse:ContentType     */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.         */

        IF oResponse:StatusCode EQ 200 THEN
        DO:
            
            oJSONData = NEW JsonObject().        
            oJSONData = cast(oResponse:Entity, JsonObject).
            
    /*         DATASET spellcheck:READ-JSON("LONGCHAR", oJSONData:GetJsonText(), "EMPTY" ). */
            
            oMatchesArray = oJSONData:GetJsonArray('matches').

            
            DO i = 1 TO oMatchesArray:LENGTH:
            
                oMatch = oMatchesArray:GetJsonObject(i).
                oReplacementArray = oMatch:GetJSONArray('replacements').
                
                ASSIGN
                    iWordStartPos = oMatch:GetInteger("offset") + 1 
                    iWordEndPos   = iWordStartPos + oMatch:GetInteger("length").              
                
                CREATE ttWordMatch.
                
                ASSIGN 
                    ttWordMatch.StatusMessage = oMatch:GetCharacter("message")
                    ttWordMatch.WordStartPos  = iWordStartPos            
                    ttWordMatch.WordEndPos    = iWordEndPos.
               
               DO iNumReplacements = 1 TO oReplacementArray:LENGTH:
                
                    oReplacement = oReplacementArray:GetJsonObject(iNumReplacements).
                    
                    CREATE ttWordSuggestion.
                    
                    ASSIGN
                        ttWordSuggestion.WordMatch_ID = ROWID(ttWordMatch)
                        ttWordSuggestion.SuggestWord  = oReplacement:GetCharacter("value").
                        .
                    
                END.  
            END.
        END.
        SESSION:SET-WAIT-STATE("").
        
     END.   
     
     RUN correctNextSpelling IN THIS-PROCEDURE.

    RETURN.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateWordOffset C-Win 
PROCEDURE updateWordOffset PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pcOrgWord AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcNewWord AS CHARACTER   NO-UNDO.
    
    CASE TRUE:
        WHEN LENGTH(pcOrgWord) EQ LENGTH(pcNewWord) THEN
            . /* Do nothing*/
        WHEN LENGTH(pcOrgWord) GT LENGTH(pcNewWord) THEN
            giOffSetPosition = giOffSetPosition + (LENGTH(pcNewWord) - LENGTH(pcOrgWord)). 
            
        WHEN LENGTH(pcOrgWord) LT LENGTH(pcNewWord) THEN
            giOffSetPosition = giOffSetPosition + (LENGTH(pcNewWord) - LENGTH(pcOrgWord)).        
    
    
    END CASE.
    
/*     /** Update the semi-global varaible **/         */
/*     giOffSetPosition = MAXIMUM(0,giOffSetPosition). */
    
    RETURN. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

