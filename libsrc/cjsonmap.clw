!** cJSON mapper v1.01
!** 04.11.2024
!** mikeduglas@yandex.com
!** mikeduglas66@gmail.com

  MEMBER

  INCLUDE('cjsonmap.inc'), ONCE
  INCLUDE('cjsonpath.inc'), ONCE

  MAP
    DefaultPrintOptions(*typCJsonPrintMapOptions pOptions), PRIVATE
    MaxFieldNameSize(typCjsonMap pMap),LONG, PRIVATE
    GenProgramStart(STRING pJson, typCjsonMap pMap, *TStringBuilder sb), PRIVATE
    GenProgramEnd(STRING pJson, typCjsonMap pMap, typCJsonPrintMapOptions pOptions, *TStringBuilder sb), PRIVATE
    BuildParseOptions(typCjsonMap pMap,*TStringBuilder sb), PRIVATE
    BuildCreateOptions(typCjsonMap pMap,*TStringBuilder sb), PRIVATE

    NextPow2(LONG pVal), LONG, PRIVATE
    EscapeClaString(STRING pText), STRING, PRIVATE
    ValidateClarionName(STRING pClaName), STRING, PRIVATE

    !!!region cJSON extensions
    IsComplexArray(cJSON jItem), BOOL, PRIVATE
    IsReal(cJSON pitem), BOOL, PRIVATE
    GetMaxStringElementSize(cJSON pArray), LONG, PRIVATE
    IsNumericArrayOfReals(cJSON pArray), BOOL, PRIVATE
    ExtractPaths(cJSON jItem, LONG pCurLevel, STRING pCurPath, *typObjectPaths pPaths), PRIVATE
    ProcessObjectPath(cJSON jItem, STRING pPath), PRIVATE
    HasSameItem(cJSON jObject, cJSON jItem), BOOL, PRIVATE   !- returns true if jObject has a child with same name and type as jItem.
    GetTypeName(cJSON jItem), STRING, PRIVATE
    GetItemIndex(cJSON jObject, STRING pItemName), LONG, PRIVATE
    FixSameItemNames(cJSON jObject), PRIVATE
    !!!endregion

    INCLUDE('printf.inc'), ONCE
  END

typObjectPath                 GROUP, TYPE
sPath                           STRING(1024)
nType                           LONG
nSize                           LONG
nLevel                          LONG
                              END
typObjectPaths                QUEUE(typObjectPath), TYPE.

typCjsonStruct                GROUP, TYPE
ParentName                      STRING(64)
JsonName                        STRING(64)
FieldName                       STRING(64)
FieldType                       STRING(20)
FieldSize                       LONG
FieldDim                        LONG
ArraySize                       LONG  !- in json
IsNull                          BOOL
FieldLevel                      LONG
MultiTyped                      BOOL
                              END
typCjsonMap                   QUEUE(typCjsonStruct), TYPE.

ClaObjectType::QUEUE          EQUATE(1)   !- QUEUE
ClaObjectType::GROUPARRAY     EQUATE(2)   !- GROUP, DIM
ClaObjectType::GROUP          EQUATE(3)   !- GROUP
ClaObjectType::TYPEARRAY      EQUATE(4)   !- LONG, DIM
ClaObjectType::TYPE           EQUATE(5)   !- LONG

ClaDataType::STRING           EQUATE(1)
ClaDataType::LONG             EQUATE(2)
ClaDataType::REAL             EQUATE(3)
ClaDataType::BOOL             EQUATE(4)


DefaultPrintOptions           PROCEDURE(*typCJsonPrintMapOptions pOptions)
  CODE
  IF NOT pOptions.ObjectName
    pOptions.ObjectName = 'MyObject'
  END
  IF NOT pOptions.ArrayName
    pOptions.ArrayName = 'MyArray'
  END
  IF pOptions.PreferredColumn <= 0
    pOptions.PreferredColumn = 31
  ELSIF pOptions.PreferredColumn > 61
    pOptions.PreferredColumn = 61
  END
  IF pOptions.Indentation <= 0
    pOptions.Indentation = 2
  ELSIF pOptions.Indentation > 8
    pOptions.Indentation = 8
  END
  
MaxFieldNameSize              PROCEDURE(typCjsonMap pMap)
nMaxSize                        LONG(0)
i                               LONG, AUTO
  CODE
  LOOP i=1 TO RECORDS(pMap)
    GET(pMap, i)
    IF nMaxSize < LEN(CLIP(pMap.FieldName))
      nMaxSize = LEN(CLIP(pMap.FieldName))
    END
  END
  RETURN nMaxSize

GenProgramStart               PROCEDURE(STRING pJson, typCjsonMap pMap, *TStringBuilder sb)
sCodeStart                      STRING(   '  PROGRAM<13,10>'                          |
                                        & '  INCLUDE(''cjson.inc''), ONCE<13,10>'     |
                                        & '  MAP<13,10>'                              |
                                        & '    INCLUDE(''printf.inc''), ONCE<13,10>'  |
                                        & '  END')
  CODE
  sb.Cat(CLIP(sCodeStart))
  sb.Cat('<13,10,13,10>')
  sb.Cat(printf('sJson                        STRING(%S)', EscapeClaString('<13,10>' & pJson))) !- <13,10> at start allows to more smarter json formatting
  sb.Cat('<13,10,13,10>')
  
GenProgramEnd                 PROCEDURE(STRING pJson, typCjsonMap pMap, typCJsonPrintMapOptions pOptions, *TStringBuilder sb)
map1                            LIKE(typCjsonStruct)  !- properties of the field 1 (root)
sbParse                         TStringBuilder
sbParseOptions                  TStringBuilder
sbCreate                        TStringBuilder
sbCreateOptions                 TStringBuilder
sbCodeEnd                       TStringBuilder
nObjectType                     LONG(0)
nDataType                       LONG(0)
  CODE
  IF RECORDS(pMap) = 0
    RETURN
  END
  
  sbParse.Init(1024)
  sbParseOptions.Init(4096)
  sbCreate.Init(4096)
  sbCreateOptions.Init(4096)
  sbCodeEnd.Init(1024)

  !- get the properties of 1st (root) field
  GET(pMap, 1)
  map1 = pMap
  
  !- set default root field's name
  map1.FieldName = CHOOSE(map1.FieldType='QUEUE' OR map1.FieldDim > 0, pOptions.ArrayName, pOptions.ObjectName)
  
  !- root object type
  IF map1.FieldType = 'QUEUE'
    nObjectType = ClaObjectType::QUEUE
  ELSIF map1.FieldType = 'GROUP' AND map1.FieldDim > 0
    nObjectType = ClaObjectType::GROUPARRAY
  ELSIF map1.FieldType = 'GROUP'
    nObjectType = ClaObjectType::GROUP
  ELSIF map1.FieldDim > 0
    nObjectType = ClaObjectType::TYPEARRAY
  ELSE
    nObjectType = ClaObjectType::TYPE
  END
  
  !- data type
  CASE map1.FieldType 
  OF 'STRING'
    nDataType = ClaDataType::STRING
  OF 'LONG'
    nDataType = ClaDataType::LONG
  OF 'REAL'
    nDataType = ClaDataType::REAL
  OF 'BOOL'
    nDataType = ClaDataType::BOOL
  END

  !- build ToGroup() options
  BuildParseOptions(pMap, sbParseOptions)

  CASE nObjectType 
  OF ClaObjectType::QUEUE OROF ClaObjectType::GROUPARRAY OROF ClaObjectType::GROUP
    !- build json::CreateObject() options
    BuildCreateOptions(pMap, sbCreateOptions)
  END
  
  CASE nObjectType 
  OF ClaObjectType::QUEUE
    !- array of objects
    sbParse.Cat(printf('IF jParser.ToQueue(sJson, %s, FALSE, | %|%s)', map1.FieldName, sbParseOptions.Str()))
    sbCreate.Cat(printf('json::CreateArray(%s, TRUE, | %|%s)', map1.FieldName, sbCreateOptions.Str()))
  OF ClaObjectType::GROUPARRAY
    !- array of objects
    sbParse.Cat(printf('IF jParser.ToGroupArray(sJson, %s, FALSE, | %|%s)', map1.FieldName, sbParseOptions.Str()))
    sbCreate.Cat(printf('json::CreateArray(%s, TRUE, | %|%s)', map1.FieldName, sbCreateOptions.Str()))
  OF ClaObjectType::GROUP
    !- object
    sbParse.Cat(printf('IF jParser.ToGroup(sJson, %s, FALSE, | %|%s)', map1.FieldName, sbParseOptions.Str()))
    sbCreate.Cat(printf('json::CreateObject(%s, TRUE, | %|%s)', map1.FieldName, sbCreateOptions.Str()))
  OF ClaObjectType::TYPEARRAY
    !- simple array of primitives
    sbParse.Cat(printf('jRoot &= jParser.Parse(sJson)%|  IF NOT jRoot &= NULL%|    LOOP i=1 TO MAXIMUM(%s,1)%|      jItem &= jRoot.GetArrayItem(i)%|      %s[i]=jItem.GetValue()%|    END%|    jRoot.Delete()%|', | 
            map1.FieldName, map1.FieldName))
    
    sbCreate.Cat(printf('%s(%s)', CHOOSE(nDataType, 'json::CreateStringArray', 'json::CreateIntArray', 'json::CreateDoubleArray', 'NULL  ! json::CreateBoolArray not supported'), | 
            map1.FieldName))

  OF ClaObjectType::TYPE
    !- primitive
    sbParse.Cat(printf('jRoot &= jParser.Parse(sJson)%|  IF NOT jRoot &= NULL%|    %s=jRoot.GetValue()%|    jRoot.Delete()%|', map1.FieldName))
    sbCreate.Cat(printf('%s(%s)', CHOOSE(nDataType, 'json::CreateString', 'json::CreateNumber', 'json::CreateNumber', 'json::CreateBool'), | 
            map1.FieldName))
  END
  
  IF nObjectType = ClaObjectType::TYPEARRAY
    !- for LONG,DIM we use jItem and i additional variables
    sbCodeEnd.Cat('' |    
            & 'jParser                       cJSONFactory<13,10>'    |
            & 'jRoot                         &cJSON, AUTO<13,10>'    |
            & 'jItem                         &cJSON, AUTO<13,10>'    |
            & 'i                             LONG, AUTO<13,10>'      |
            & '  CODE<13,10>'                                        |
            & '  !- JSON -> Clarion<13,10>'                          |
            & '  %s<13,10>'                                          |
            & '    !- Clarion -> JSON<13,10>'                        |
            & '    jRoot &= %s<13,10>'                               |
            & '    IF NOT jRoot &= NULL<13,10>'                      |
            & '      !- check result in DebugView<13,10>'            |
            & '      printd(jRoot.ToString(TRUE))<13,10>'            |
            & '      jRoot.Delete()<13,10>'                          |
            & '    END<13,10>'                                       |
            & '  END')
  ELSE
    sbCodeEnd.Cat('' |    
            & 'jParser                       cJSONFactory<13,10>'    |
            & 'jRoot                         &cJSON, AUTO<13,10>'    |
            & '  CODE<13,10>'                                        |
            & '  !- JSON -> Clarion<13,10>'                          |
            & '  %s<13,10>'                                          |
            & '    !- Clarion -> JSON<13,10>'                        |
            & '    jRoot &= %s<13,10>'                               |
            & '    IF NOT jRoot &= NULL<13,10>'                      |
            & '      !- check result in DebugView<13,10>'            |
            & '      printd(jRoot.ToString(TRUE))<13,10>'            |
            & '      jRoot.Delete()<13,10>'                          |
            & '    END<13,10>'                                       |
            & '  END')
  END
  
  
  sb.Cat('<13,10>')
  sb.Cat(printf(sbCodeEnd.Str(), sbParse.Str(), sbCreate.Str()))
  sb.Cat('<13,10,13,10>')

BuildParseOptions             PROCEDURE(typCjsonMap pMap,*TStringBuilder sb)
i                               LONG, AUTO
qUniqueMap                      QUEUE(typCjsonMap).
qParseOptions                   QUEUE
Line                              STRING(4096)
                                END
  CODE
  !- make unique field list
  LOOP i=1 TO RECORDS(pMap)
    GET(pMap, i)
    qUniqueMap.FieldName = pMap.FieldName
    GET(qUniqueMap, qUniqueMap.FieldName)
    IF ERRORCODE()
      qUniqueMap :=: pMap
      ADD(qUniqueMap)
    END
  END
  
  !- add JsonName rules for the fields with completely different names/jsonnames (unique names only)
  LOOP i=1 TO RECORDS(qUniqueMap)
    GET(qUniqueMap, i)
    IF LOWER(qUniqueMap.FieldName) <> LOWER(qUniqueMap.JsonName)
      CLEAR(qParseOptions)
      qParseOptions.Line = printf('{{"name":"%s", "JsonName":"%s"}', qUniqueMap.FieldName, qUniqueMap.JsonName)
      ADD(qParseOptions)
    END
  END
  
  !- convert parse options list to the multi-line string.
  LOOP i=1 TO RECORDS(qParseOptions)
    GET(qParseOptions, i)
    IF i=1
      qParseOptions.Line = '['& CLIP(qParseOptions.Line)
    END
    IF i=RECORDS(qParseOptions)
      qParseOptions.Line = CLIP(qParseOptions.Line) &']'
    END
    
    IF i < RECORDS(qParseOptions)
      sb.Cat(printf('      %S & | %|', CLIP(qParseOptions.Line) &',', qParseOptions.Line))
    ELSE
      sb.Cat(printf('      %S', qParseOptions.Line))
    END
  END
  
BuildCreateOptions            PROCEDURE(typCjsonMap pMap,*TStringBuilder sb)
i                               LONG, AUTO
qUniqueMap                      QUEUE(typCjsonMap).
qCreateOptions                  QUEUE
Line                              STRING(4096)
                                END
  CODE
  !- make unique field list
  LOOP i=1 TO RECORDS(pMap)
    GET(pMap, i)
    qUniqueMap.FieldName = pMap.FieldName
    GET(qUniqueMap, qUniqueMap.FieldName)
    IF ERRORCODE()
      qUniqueMap :=: pMap
      ADD(qUniqueMap)
    END
  END
  
  qCreateOptions.Line = '{{"name":"*","IgnoreEmptyObject":true,"IgnoreEmptyArray":true,"EmptyString":"ignore"}'
  ADD(qCreateOptions)
  
  !- add IsBool rule
  CLEAR(qCreateOptions)
  LOOP i=1 TO RECORDS(qUniqueMap)
    GET(qUniqueMap, i)
    IF qUniqueMap.FieldType = 'BOOL'
      IF qCreateOptions.Line
        qCreateOptions.Line = CLIP(qCreateOptions.Line) &','
      END
      qCreateOptions.Line = CLIP(qCreateOptions.Line) & printf('"%s"', qUniqueMap.FieldName)
    END
  END
  IF qCreateOptions.Line
    qCreateOptions.Line = printf('{{"name":[%s], "IsBool":true}', qCreateOptions.Line)
    ADD(qCreateOptions)
  END
  
  !- add JsonName rules for the fields with completely different names/jsonnames (unique names only)
  CLEAR(qCreateOptions)
  LOOP i=1 TO RECORDS(qUniqueMap)
    GET(qUniqueMap, i)
    IF LOWER(qUniqueMap.FieldName) <> LOWER(qUniqueMap.JsonName)
      CLEAR(qCreateOptions)
      qCreateOptions.Line = printf('{{"name":"%s", "JsonName":"%s"}', qUniqueMap.FieldName, qUniqueMap.JsonName)
      ADD(qCreateOptions)
    END
  END
      
  !- add JsonName rules for the fields with case different names/jsonnames (json name is in camel case for example).
  CLEAR(qCreateOptions)
  LOOP i=1 TO RECORDS(qUniqueMap)
    GET(qUniqueMap, i)
    IF (LOWER(qUniqueMap.FieldName) <> qUniqueMap.JsonName) AND (LOWER(qUniqueMap.FieldName) = LOWER(qUniqueMap.JsonName))
      IF qCreateOptions.Line
        qCreateOptions.Line = CLIP(qCreateOptions.Line) &','
      END
      qCreateOptions.Line = CLIP(qCreateOptions.Line) & printf('"%s"', qUniqueMap.FieldName)
    END
  END
  IF qCreateOptions.Line
    qCreateOptions.Line = printf('{{"name":[%s], "JsonName":"*"}', qCreateOptions.Line)
    ADD(qCreateOptions)
  END
  
  !- convert create options list to the multi-line string.
  LOOP i=1 TO RECORDS(qCreateOptions)
    GET(qCreateOptions, i)
    IF i=1
      qCreateOptions.Line = '['& CLIP(qCreateOptions.Line)
    END
    IF i=RECORDS(qCreateOptions)
      qCreateOptions.Line = CLIP(qCreateOptions.Line) &']'
    END
    
    IF i < RECORDS(qCreateOptions)
      sb.Cat(printf('      %S & | %|', CLIP(qCreateOptions.Line) &',', qCreateOptions.Line))
    ELSE
      sb.Cat(printf('      %S', qCreateOptions.Line))
    END
  END  

NextPow2                      PROCEDURE(LONG pVal)
!https://stackoverflow.com/questions/466204/rounding-up-to-next-power-of-2
!
!unsigned int v; // compute the next highest power of 2 of 32-bit v
!
!v--;
!v |= v >> 1;
!v |= v >> 2;
!v |= v >> 4;
!v |= v >> 8;
!v |= v >> 16;
!v++;
  CODE
  pVal -= 1
  pVal = BOR(pVal, BSHIFT(pVal, -1))
  pVal = BOR(pVal, BSHIFT(pVal, -2))
  pVal = BOR(pVal, BSHIFT(pVal, -4))
  pVal = BOR(pVal, BSHIFT(pVal, -8))
  pVal = BOR(pVal, BSHIFT(pVal, -16))
  pVal += 1
  RETURN pVal

EscapeClaString               PROCEDURE(STRING pText)
nTextLen                        LONG, AUTO
sb                              TStringBuilder
nStart                          LONG, AUTO
nEnd                            LONG, AUTO
i                               LONG, AUTO
  CODE
  nTextLen = LEN(CLIP(pText))
  sb.Init(nTextLen*2)
  
  !- process <, {, ', New Line, TAB chars
  nStart = 1
  nEnd = 0
  LOOP i=1 TO nTextLen
    CASE pText[i]
    OF '<<' OROF '{{' OROF ''''
      !- save this char twice
      nEnd = i
      sb.Cat(pText[nStart : nEnd] & pText[i])
      nStart = i+1
    OF '<9>'  !- TAB
      !- replace TAB with 2 spacebars
      nEnd = i-1
      sb.Cat(pText[nStart : nEnd] & '  ')
      nStart = i+1
    OF '<13>' !- New line
      !- add line concatenation
      nEnd = i-1
      sb.Cat(pText[nStart : nEnd] & '''  |<13,10>    & ''')
      i += 1
      nStart = i+1
    END
  END
  IF nEnd < nStart AND nStart <= nTextLen
    sb.Cat(pText[nStart : nTextLen])
  END

  RETURN sb.Str()
  
ValidateClarionName           PROCEDURE(STRING pClaName)
i                               LONG, AUTO
  CODE
  IF NOT pClaName
    RETURN ''
  END
  
  !- replace space chars inside a string with underscore _ chars.
  LOOP i=1 TO LEN(CLIP(pClaName))
    IF pClaName[i] = ' '
      pClaName[i] = '_'
    END
  END
  !- The first character must be a letter or the underscore character
  IF pClaName <> '' AND (INRANGE(VAL(LOWER(pClaName[1])), VAL('a'), VAL('z')) OR pClaName[1]='_')
    RETURN pClaName
  ELSE
    RETURN '_'& pClaName
  END
  
  
!!!region cJSON extensions
IsComplexArray                PROCEDURE(cJSON jItem)
jChild                          &cJSON, AUTO
  CODE
  IF jItem.IsArray()
    jChild &= jItem.GetChild()
    IF NOT jChild &= NULL AND BAND(jChild.GetType(), 0FFh) = cJSON_Object
      RETURN TRUE
    END
  END
  RETURN FALSE

IsReal                        PROCEDURE(cJSON jItem)
rVal                            REAL, AUTO
  CODE
  IF jItem.IsNumber() 
    rVal = jItem.GetNumberValue()
    IF rVal <> INT(rVal) OR INSTRING('.', jItem.GetStringValue(), 1, 1) > 0
      !- if integer part <> numeric value and no "." in string value then it is floating number.
      RETURN TRUE
    END
  END
  RETURN FALSE
  
GetMaxStringElementSize       PROCEDURE(cJSON jArray)
jElement                        &cJSON, AUTO
nSize                           LONG, AUTO
nMaxSize                        LONG(0)
  CODE
  !- iterate array of strings elements and find max string size.
  jElement &= jArray.GetChild()
  IF NOT jElement &= NULL AND (jElement.IsString() OR jElement.IsRaw())
    LOOP
      nSize = jElement.GetStringSize()
      IF nMaxSize < nSize
        nMaxSize = nSize
      END
      jElement &= jElement.GetNext()
    WHILE NOT jElement &= NULL
  END
  RETURN nMaxSize
  
IsNumericArrayOfReals         PROCEDURE(cJSON jArray)
jElement                        &cJSON, AUTO
  CODE
  !- iterate array of numeric elements.
  jElement &= jArray.GetChild()
  IF NOT jElement &= NULL AND jElement.IsNumber()
    LOOP
      IF jElement.IsReal()
        RETURN TRUE
      END
      jElement &= jElement.GetNext()
    WHILE NOT jElement &= NULL
  END
  RETURN FALSE
  
ExtractPaths                  PROCEDURE(cJSON jItem, LONG pCurLevel, STRING pCurPath, *typObjectPaths pPaths)
jChild                          &cJSON, AUTO
sItemName                       STRING(64), AUTO
sItemPath                       STRING(1024), AUTO
nType                           LONG, AUTO
  CODE
  IF NOT (jItem.IsObject() OR jItem.IsComplexArray())
    RETURN
  END

  !- * means an array element (no name), $ - root
  sItemName = CHOOSE(pCurLevel=0, '$', printf('[%s]', CHOOSE(jItem.GetName()<>'', jItem.GetName(), '*')))
  !- concatenate current path with this name
  sItemPath = printf('%s%s', pCurPath, sItemName)
  nType = CHOOSE(jItem.IsObject()=TRUE, cJSON_Object, cJSON_Array)
  
  CLEAR(pPaths)
  pPaths.nLevel = pCurLevel
  pPaths.sPath = sItemPath
  pPaths.nType = nType
  GET(pPaths, pPaths.nLevel, pPaths.sPath, pPaths.nType)
  IF ERRORCODE()
    pPaths.nLevel = pCurLevel
    pPaths.sPath = sItemPath
    pPaths.nType = nType
    pPaths.nSize = 1
    ADD(pPaths)
  ELSE
    pPaths.nSize += 1
    PUT(pPaths)
  END

  jChild &= jItem.GetChild()
  LOOP WHILE NOT jChild &= NULL
    jChild.ExtractPaths(pCurLevel+1, sItemPath, pPaths)
    jChild &= jChild.GetNext()
  END
  
ProcessObjectPath             PROCEDURE(cJSON jRoot, STRING pPath)
output                          TCJsonPathResultAccumulator
jFisrtObject                    &cJSON, AUTO
jAnotherObject                  &cJSON, AUTO
jItem                           &cJSON, AUTO
jAnotherItem                    &cJSON, AUTO
sItemName                       STRING(64), AUTO
resCount                        LONG, AUTO
i                               LONG, AUTO
j                               LONG, AUTO
  CODE
!  printd('*** Path context: %s', pPath)
  !- Find all objects by the path
  resCount = jRoot.FindPathContext(pPath, output)
  IF resCount > 1
    jFisrtObject &= output.GetObject(1)
    
    !- merge all the objects into 1st one.
    LOOP i=2 TO resCount
      jAnotherObject &= output.GetObject(i)

      !- add items not exist in 1st object
      j=1
      LOOP
        jAnotherItem &= jAnotherObject.GetArrayItem(j)
        IF jAnotherItem &= NULL
          BREAK
        END

        sItemName = jAnotherItem.GetName()
        
        IF NOT jFisrtObject.HasItem(sItemName, TRUE)
          !- no such item name in 1st object - add it.
          jFisrtObject.AddItemToObject(sItemName, jAnotherObject.DetachItemViaPointer(jAnotherItem))

          !- j counter stays the same because we removed the item.
          
        ELSIF NOT jFisrtObject.HasSameItem(jAnotherItem)
          !- 1st object has an item with same name but different type - add (insert) it right after existing item.
          !- Don't add nulls.
          IF NOT jAnotherItem.IsNull()
            jFisrtObject.InsertItemInArray(jFisrtObject.GetItemIndex(sItemName)+1, jAnotherObject.DetachItemViaPointer(jAnotherItem))
            !- j counter stays the same because we removed the item.
          ELSE
            !- next another element
            j += 1
          END
          

        ELSE
          jItem &= jFisrtObject.GetObjectItem(sItemName, TRUE)
          CASE BAND(jItem.GetType(), 0FFh)
          OF cJSON_String
          OROF cJSON_Raw
            !- take the biggest string value
            IF jItem.GetStringSize() < jAnotherItem.GetStringSize()
              jItem.SetStringValue(jAnotherItem.GetStringRef())
            END
          OF cJSON_Number
            !- take REAL value
            IF NOT jItem.IsReal() AND jAnotherItem.IsReal()
              jItem.SetNumberValue(jAnotherItem.GetNumberValue())
            END
          END
          
          !- next another element
          j += 1
        END
      END
    END
    
    LOOP i=2 TO resCount
      jAnotherObject &= output.GetObject(i)
      LOOP jAnotherObject.GetArraySize() TIMES
        jAnotherObject.DeleteItemFromArray(1)
      END
    END
  END
  
HasSameItem                   PROCEDURE(cJSON jObject, cJSON jItem)
jChild                          &cJSON, AUTO
  CODE
  jChild &= jObject.GetChild()
  LOOP WHILE NOT jChild &= NULL
    IF jChild.GetType() = jItem.GetType() AND jChild.GetName() = jItem.GetName()
      RETURN TRUE
    END
    jChild &= jChild.GetNext()
  END
  RETURN FALSE
  
GetTypeName                   PROCEDURE(cJSON jItem)
  CODE
  CASE BAND(jItem.GetType(), 0FFh)
  OF cJSON_False OROF cJSON_True
    RETURN 'boolean'
  OF cJSON_NULL
    RETURN 'null'
  OF cJSON_Number
    RETURN 'numeric'
  OF cJSON_String OROF cJSON_Raw
    RETURN 'string'
  OF cJSON_Array
    RETURN 'array'
  OF cJSON_Object
    RETURN 'object'
  ELSE
    RETURN 'undefined_type'
  END
  
GetItemIndex                  PROCEDURE(cJSON jObject, STRING pItemName)
jChild                          &cJSON, AUTO
i                               LONG(0)
  CODE
  jChild &= jObject.GetChild()
  LOOP WHILE NOT jChild &= NULL
    i += 1
    IF jChild.GetName() = pItemName
      RETURN i
    END
    jChild &= jChild.GetNext()
  END
  RETURN 0
  
FixSameItemNames              PROCEDURE(cJSON jObject)
jChild1                         &cJSON, AUTO
jChild2                         &cJSON, AUTO
nArrSize                        LONG, AUTO
i                               LONG, AUTO
j                               LONG, AUTO
  CODE
  nArrSize = jObject.GetArraySize()
  IF jObject.IsArray() AND nArrSize > 0
    LOOP i=1 TO nArrSize
      jChild1 &= jObject.GetArrayItem(i)
      jChild1.FixSameItemNames()
    END
  ELSIF jObject.IsObject() AND nArrSize > 1
    LOOP i=1 TO nArrSize-1
      jChild1 &= jObject.GetArrayItem(i)
      LOOP j=i+1 TO nArrSize
        jChild2 &= jObject.GetArrayItem(j)
        
        IF jChild1.GetName() = jChild2.GetName()
          jChild2.SetName(printf('%s_as_%s', jChild2.GetName(), jChild2.GetTypeName()))
        END
      END
      jChild1.FixSameItemNames()
    END
    jChild1 &= jObject.GetArrayItem(nArrSize)
    jChild1.FixSameItemNames()
  END
!!!endregion
  
!!!region TCJsonMapper
TCJsonMapper.Construct        PROCEDURE()
  CODE
  SELF.qMap &= NEW typCjsonMap
  
TCJsonMapper.Destruct         PROCEDURE()
  CODE
  FREE(SELF.qMap)
  DISPOSE(SELF.qMap)
  
TCJsonMapper.MapItem          PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
  CODE
  CASE BAND(jItem.GetType(), 0FFh)
  OF cJSON_String
  OROF cJSON_Raw
    SELF.MapString(jItem, pLevel, pParentName)
  OF cJSON_Number
    SELF.MapNumber(jItem, pLevel, pParentName)
  OF   cJSON_False
  OROF cJSON_True
    SELF.MapBoolean(jItem, pLevel, pParentName)
  OF cJSON_NULL
    SELF.MapNull(jItem, pLevel, pParentName)
  OF cJSON_Array
    SELF.MapArray(jItem, pLevel, pParentName)
  OF cJSON_Object
    SELF.MapObject(jItem, pLevel, pParentName)
  ELSE
    printd('[cJsonMapper] Invalid json type %i.', jItem.GetType())
  END
  
TCJsonMapper.MapString        PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
  CODE
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapString(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  
  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.FieldType = 'STRING'
    SELF.qMap.FieldSize = CHOOSE(jItem.GetStringSize() > 0, jItem.GetStringSize(), 20)   !- empty string -> STRING(20)
    SELF.qMap.IsNull = CHOOSE(jItem.GetStringSize() = 0)
    SELF.qMap.FieldDim = 0
    SELF.qMap.ArraySize = 0
    ADD(SELF.qMap)
  ELSE
    !- yes - update if nedded.
    IF SELF.qMap.FieldSize < jItem.GetStringSize()
      SELF.qMap.FieldType = 'STRING' !- it might have 'NULL' type.
      SELF.qMap.FieldSize = jItem.GetStringSize()
      PUT(SELF.qMap)
    END
  END
   
TCJsonMapper.MapNumber        PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
  CODE
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapNumber(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)

  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.FieldType = CHOOSE(NOT jItem.IsReal(), 'LONG', 'REAL')
    SELF.qMap.FieldSize = 0
    SELF.qMap.FieldDim = 0
    SELF.qMap.ArraySize = 0
    ADD(SELF.qMap)
  ELSE
    !- yes - update if nedded.
    IF SELF.qMap.FieldType = 'LONG' AND jItem.IsReal()
      SELF.qMap.FieldType = 'REAL'
      PUT(SELF.qMap)
    END
  END

TCJsonMapper.MapBoolean       PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
  CODE
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapBoolean(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)

  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.FieldType = 'BOOL'
    SELF.qMap.FieldSize = 0
    SELF.qMap.FieldDim = 0
    SELF.qMap.ArraySize = 0
    ADD(SELF.qMap)
  ELSE
    !- already added.
  END

TCJsonMapper.MapNull          PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
  CODE
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapNull(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)

  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.IsNull = TRUE
    SELF.qMap.FieldType = 'STRING'        !- NULL -> STRING(20)
    SELF.qMap.FieldSize = 20
    SELF.qMap.FieldDim = 0
    SELF.qMap.ArraySize = 0
    ADD(SELF.qMap)
  ELSE
    !- already added.
  END

TCJsonMapper.MapObject        PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
jElement                        &cJSON, AUTO
  CODE
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapObject(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)

  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.FieldType = 'GROUP'
    SELF.qMap.FieldSize = 0
    SELF.qMap.FieldDim = 0
    SELF.qMap.ArraySize = 0
    ADD(SELF.qMap)
  END
  
  jElement &= jItem.GetChild()
  IF NOT jElement &= NULL
    LOOP WHILE NOT jElement &= NULL
      SELF.MapItem(jElement, pLevel+1, jItem.GetName())
      jElement &= jElement.GetNext()
    END
  ELSE
    !- empty object/array
    SELF.qMap.IsNull = TRUE
    IF jItem.IsArray()
      SELF.qMap.FieldDim = 2  !- just to declare GROUP,DIM(2)
    END
    
    PUT(SELF.qMap)
  END
  
TCJsonMapper.MapArray         PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
jElement                        &cJSON, AUTO
  CODE
  !- is the array of complex objects or simple strings/numbers/bools?
  jElement &= jItem.GetChild()
  IF NOT jElement &= NULL
    IF jElement.IsObject() OR jElement.IsArray()
      SELF.MapComplexArray(jItem, pLevel, pParentName)
    ELSE
      SELF.MapSimpleArray(jItem, pLevel, pParentName)
    END
  ELSE
    !- map empty array.
    SELF.MapObject(jItem, pLevel, pParentName)
  END
  
TCJsonMapper.MapSimpleArray   PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
jElement                        &cJSON, AUTO
nArraySize                      LONG, AUTO
nMaxStringSize                  LONG, AUTO
bIsReal                         BOOL, AUTO
  CODE
  nArraySize = jItem.GetArraySize()
  IF nArraySize=0
    printd('[cJsonMapper] Invalid simple array:%|%s', jItem.ToString(TRUE))
    RETURN
  END
  
  nMaxStringSize = jItem.GetMaxStringElementSize()
  bIsReal = jItem.IsNumericArrayOfReals()
  
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapSimpleArray(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)

  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldDim = CHOOSE(nArraySize > 1, nArraySize, 2) !- avoid DIM(0) and DIM(1) declarations.
    SELF.qMap.ArraySize = nArraySize

    jElement &= jItem.GetChild()
    CASE BAND(jElement.GetType(), 0FFh)
    OF cJSON_String
    OROF cJSON_Raw
      SELF.qMap.FieldType = 'STRING'
      SELF.qMap.FieldSize = CHOOSE(nMaxStringSize > 0, nMaxStringSize, 20) !- empty string -> STRING(20)
      SELF.qMap.IsNull = CHOOSE(jElement.GetStringSize() = 0)
    OF cJSON_Number
      SELF.qMap.FieldType = CHOOSE(NOT bIsReal, 'LONG', 'REAL')
    OF   cJSON_False
    OROF cJSON_True
      SELF.qMap.FieldType = 'BOOL'
    OF cJSON_NULL
      SELF.qMap.FieldType = 'STRING'
      SELF.qMap.FieldSize = 20
      SELF.qMap.IsNull = TRUE
    ELSE
      printd('[cJsonMapper] Invalid json type %i.', jElement.GetType())
    END

    ADD(SELF.qMap)
  ELSE
    !- yes - update if nedded.
    IF SELF.qMap.FieldDim < nArraySize
      SELF.qMap.FieldDim = CHOOSE(nArraySize > 1, nArraySize, 2) !- avoid DIM(0) and DIM(1) declarations.
      SELF.qMap.ArraySize = nArraySize
      PUT(SELF.qMap)
    END
    IF SELF.qMap.FieldSize < nMaxStringSize
      SELF.qMap.FieldSize = nMaxStringSize
      SELF.qMap.IsNull = FALSE
      PUT(SELF.qMap)
    END
    IF SELF.qMap.FieldType = 'LONG' AND bIsReal
      SELF.qMap.FieldType = 'REAL'
      PUT(SELF.qMap)
    END
  END
  
TCJsonMapper.MapComplexArray  PROCEDURE(*cJSON jItem, LONG pLevel, STRING pParentName)
jObject                         &cJSON, AUTO
jElement                        &cJSON, AUTO
nArraySize                      LONG, AUTO
  CODE
  !- has this field already been processed?
  SELF.qMap.FieldLevel = pLevel
  SELF.qMap.ParentName = pParentName
  SELF.qMap.FieldName = jItem.GetName()
!  printd('MapObject(%i: %s.%s)', SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)

  GET(SELF.qMap, SELF.qMap.FieldLevel, SELF.qMap.ParentName, SELF.qMap.FieldName)
  IF ERRORCODE()
    !- not yet - add it.
    nArraySize = jItem.GetArraySize()
    
    CLEAR(SELF.qMap)
    SELF.qMap.FieldLevel = pLevel
    SELF.qMap.ParentName = pParentName
    SELF.qMap.FieldName = jItem.GetName()
    SELF.qMap.FieldType = CHOOSE(pLevel=0, 'QUEUE', 'GROUP')
    SELF.qMap.FieldSize = 0
    SELF.qMap.FieldDim = CHOOSE(pLevel=0, 0, nArraySize)
    IF SELF.qMap.FieldDim = 1
      !- avoid DIM(1) declarations.
      SELF.qMap.FieldDim = 2
    END
    SELF.qMap.ArraySize = nArraySize

    ADD(SELF.qMap)
  END
  
  jObject &= jItem.GetChild() !- Map 1st array item
  IF NOT jObject &= NULL
    jElement &= jObject.GetChild()
    LOOP WHILE NOT jElement &= NULL
      SELF.MapItem(jElement, pLevel+1, jItem.GetName())
      jElement &= jElement.GetNext()
    END
  END
  
TCJsonMapper.MapJson          PROCEDURE(STRING pJson)
jParser                         cJSONFactory
jRoot                           &cJSON, AUTO
qPath                           QUEUE(typObjectPaths).
i                               LONG, AUTO
nNameSuffixPos                  LONG, AUTO
  CODE
  CLEAR(SELF.parseErrorPos)
  CLEAR(SELF.parseErrorString)

  FREE(SELF.qMap)
  
  jRoot &= jParser.Parse(pJson)
  IF jRoot &= NULL
    SELF.parseErrorPos = jParser.GetErrorPosition()
    SELF.parseErrorString = jParser.GetError()
    
    printd('[cJsonMapper] Error "%s" at position %i.', SELF.parseErrorString, SELF.parseErrorPos)
    RETURN FALSE
  END
  
  !- walk thru the json and extract all paths of arrays/objects.
  jRoot.ExtractPaths(0, '', qPath)
  
  !- simplify arrays
  IF RECORDS(qPath)
    !- walk from the deepest level up to the topmost, objects first.
    SORT(qPath, -qPath.nType, -qPath.nLevel)
    LOOP i=1 TO RECORDS(qPath)
      GET(qPath, i)
      IF qPath.nType = cJSON_Object AND qPath.nSize > 1
!        printd('Path %i: Level %i, %s (%s) size %i', i, qPath.nLevel, qPath.sPath, CHOOSE(qPath.nType = cJSON_Object, 'Object', 'Array'), qPath.nSize)
        jRoot.ProcessObjectPath(qPath.sPath)
      END
    END
  END
  
  !- now rename items with the same names in the object.
  jRoot.FixSameItemNames()
  
  !- at this point jRoot has the arrays with 1 element only, with the biggest possible strings and floating numerics if needed.
!  printd('RESULTING JSON:%|%s', jRoot.ToString(TRUE))
  
  SELF.MapItem(jRoot, 0, '')
  jRoot.Delete()
  
  !- validate Clarion names
  LOOP i=1 TO RECORDS(SELF.qMap)
    GET(SELF.qMap, i)
    nNameSuffixPos = INSTRING('_as_', SELF.qMap.FieldName, 1, 1)
    IF nNameSuffixPos > 1
      !- rename back json names 'url_as_object'
      SELF.qMap.JsonName = SELF.qMap.FieldName[1 : nNameSuffixPos-1]
      SELF.qMap.MultiTyped = TRUE
    ELSE
      SELF.qMap.JsonName = SELF.qMap.FieldName
      SELF.qMap.MultiTyped = FALSE
    END
    SELF.qMap.FieldName = ValidateClarionName(SELF.qMap.FieldName)
    PUT(SELF.qMap)
  END
  
  RETURN TRUE

TCJsonMapper.PrintMap         PROCEDURE(<STRING pJson>)
options                         LIKE(typCJsonPrintMapOptions)
  CODE
  RETURN SELF.PrintMap(options, pJson)
  
TCJsonMapper.PrintMap         PROCEDURE(typCJsonPrintMapOptions pOptions, <STRING pJson>)
sIndent                         STRING(256), AUTO
nTabSize                        LONG, AUTO
sType                           STRING(256), AUTO
sComment                        STRING(256), AUTO
nCommentColumn                  LONG, AUTO
nLevel                          LONG, AUTO
nMaxNameLen                     LONG, AUTO
sFieldName                      LIKE(typCjsonMap.FieldName), AUTO
nFieldSize                      LIKE(typCjsonMap.FieldSize), AUTO
sFieldStatement                 STRING(256), AUTO
sNameFmt                        STRING(5), AUTO
i                               LONG, AUTO
j                               LONG, AUTO
sbDecl                          TStringBuilder
sbResult                        TStringBuilder
  CODE
  IF NOT RECORDS(SELF.qMap)
    printd('[TCJsonMapper] PrintMap: no records.')
    RETURN SELF.GetError()
  END
  
  !- initialize uninitialized options
  DefaultPrintOptions(pOptions)
  
  nMaxNameLen = MaxFieldNameSize(SELF.qMap)
  IF nMaxNameLen <= pOptions.PreferredColumn-2
    nMaxNameLen = pOptions.PreferredColumn-2
  END
  sNameFmt = '@s'& nMaxNameLen
  
  nTabSize = pOptions.Indentation
  
  nLevel = 0
  sIndent = ''
  sbDecl.Init(8192)
  sbResult.Init(16384)
  
  LOOP i=1 TO RECORDS(SELF.qMap)
    GET(SELF.qMap, i)
    
    !- check for END of GROUP/QUEUE (the level decreased)
    IF SELF.qMap.FieldLevel < nLevel
      !- last group field was processed
      LOOP j=nLevel-1 TO SELF.qMap.FieldLevel BY -1
        sbDecl.Cat(printf('%z%zEND%|', FORMAT(' ', sNameFmt), SUB(sIndent, 1, j*nTabSize)))
      END
    END
    
    !- set name for unnamed root object
    sFieldName = SELF.qMap.FieldName
    IF sFieldName = '' AND SELF.qMap.FieldLevel = 0
      sFieldName = CHOOSE(SELF.qMap.FieldType='QUEUE' OR SELF.qMap.FieldDim > 0, pOptions.ArrayName, pOptions.ObjectName)
    END
    
    !- round up STRING size to the next power of 2.
    nFieldSize = CHOOSE(SELF.qMap.FieldSize = 0 OR SELF.qMap.IsNull OR pOptions.RoundUpStringSize=FALSE, SELF.qMap.FieldSize, NextPow2(SELF.qMap.FieldSize))
    
    !- format field type including NAME attribute.
    sType = printf('%s', SELF.qMap.FieldType) & | 
            CHOOSE(nFieldSize > 0, printf('(%i)', nFieldSize), '') & | 
            CHOOSE(SELF.qMap.FieldDim > 0, printf(',DIM(%i)', SELF.qMap.FieldDim), '') & |
            CHOOSE(pOptions.EnableExternalNames = TRUE AND SELF.qMap.FieldName <> '', printf(', NAME(%S)', SELF.qMap.JsonName), '')
    
    !- full field statement
    sFieldStatement = printf('%z%z%s', FORMAT(sFieldName, sNameFmt), SUB(sIndent, 1, SELF.qMap.FieldLevel*nTabSize), sType)
    sbDecl.Cat(printf('%s', sFieldStatement))

    !- format comment
    sComment = ''
    IF pOptions.EnableComments
      !- if string size was increased
      IF SELF.qMap.FieldType = 'STRING' AND nFieldSize > SELF.qMap.FieldSize
        sComment = printf('max string(%i)', SELF.qMap.FieldSize)
      END
      
      !- if array size was increased
      IF SELF.qMap.ArraySize < SELF.qMap.FieldDim
        IF sComment
          sComment = CLIP(sComment) &','
        END
      
        sComment = CLIP(sComment) & printf('max array[%i]', SELF.qMap.ArraySize)
      END
          
      !- null field
      IF SELF.qMap.IsNull
        IF sComment
          sComment = CLIP(sComment) &','
        END
      
        sComment = CLIP(sComment) & 'null'
      END
    END
    IF sComment
      sComment = printf('%z! %s', SUB(sIndent, 1, 4*nTabSize), sComment)
      nCommentColumn = 80
      IF  nCommentColumn-LEN(CLIP(sFieldStatement)) <= 0
        nCommentColumn += LEN(CLIP(sFieldStatement)) + 2*nTabSize
      END
      
      sbDecl.Cat(printf('%z%s', SUB(sIndent, 1, nCommentColumn-LEN(CLIP(sFieldStatement))), sComment))
    END
    
    !- end of line
    sbDecl.Cat(printf('%|'))
    
    !- empty GROUP - immediate END
    IF SELF.qMap.FieldType = 'GROUP' AND SELF.qMap.IsNull
      sbDecl.Cat(printf('%z%zEND%|', FORMAT(' ', sNameFmt), SUB(sIndent, 1, SELF.qMap.FieldLevel*nTabSize)))
    END
    
    nLevel = SELF.qMap.FieldLevel
    
    !- the end of a declaration: check for not closed GROUPs/QUEUEs.
    IF i=RECORDS(SELF.qMap)
      IF nLevel > 0
        LOOP j=nLevel-1 TO 0 BY -1
          sbDecl.Cat(printf('%z%zEND%|', FORMAT(' ', sNameFmt), SUB(sIndent, 1, j*nTabSize)))
        END
      END
    END
  END
  
  !- start of a test program
  IF pOptions.PrintCodeSample AND pJson
    GenProgramStart(pJson, SELF.qMap, sbResult)
  END
  
  !- struct declaration
  sbResult.Cat(sbDecl.Str())

  !- end of a test program
  IF pOptions.PrintCodeSample AND pJson
    GenProgramEnd(pJson, SELF.qMap, pOptions, sbResult)
  END

  RETURN sbResult.Str()
  
TCJsonMapper.GetError         PROCEDURE()
  CODE
  RETURN printf('Error in json near "%s" at position %i.', SELF.parseErrorString, SELF.parseErrorPos)
!!!endregion