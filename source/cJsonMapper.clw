  PROGRAM

  PRAGMA('link(cJsonMapper.EXE.manifest)')


_UI_ENHANCEMENTS_             EQUATE(0)


  INCLUDE('cjson.inc'), ONCE
  INCLUDE('cjsonmap.inc'), ONCE

  COMPILE('!***_UI_ENHANCEMENTS_***', _UI_ENHANCEMENTS_)
  INCLUDE('smartzoom.inc'), ONCE
  INCLUDE('cuebanner.inc'), ONCE
  INCLUDE('toggle.inc'), ONCE
  !***_UI_ENHANCEMENTS_***

  INCLUDE('ABRESIZE.INC'), ONCE

  MAP
    MODULE('Win api')
      winapi::GetACP(),ULONG,PASCAL,NAME('GetACP')
    END
    INCLUDE('printf.inc'), ONCE

    Main()
    ProcessString(STRING pJson, typCJsonPrintMapOptions pOptions, *TStringBuilder sbJson, *TStringBuilder sbClarion)
    ProcessFile(STRING pFileName, typCJsonPrintMapOptions pOptions, *TStringBuilder sbJson, *TStringBuilder sbClarion)
    GenerateCode(typCJsonPrintMapOptions pOptions, TStringBuilder sbJson, *TStringBuilder sbClarion)
    About(WINDOW pParentWindow)
  END

codePage                      LONG, AUTO
PROGRAM_NAME_AND_VERSION      EQUATE('cJSON Mapper v1.0.0')

  CODE
  !- set active charset
!  codePage = winapi::GetACP()
  SYSTEM{PROP:CharSet} = CHOOSE(codePage = 1251, CHARSET:CYRILLIC, CHARSET:DEFAULT)
  
  Main()
  
Main                          PROCEDURE()
sIniFileName                    STRING('.\cJsonMapper.ini')

options                         LIKE(typCJsonPrintMapOptions)

sJsonFileName                   STRING(FILE:MaxFilePath)
sClwFileName                    STRING(FILE:MaxFilePath)
sbInput                         TStringBuilder
sbOutput                        TStringBuilder

MainWindow                      WINDOW('cJSON mapper'),AT(,,628,400),CENTER,GRAY,SYSTEM,MAX, |
                                        ICON('Feodor.ico'),FONT('Segoe UI',9),RESIZE
                                  TOOLBAR,AT(0,0,628,42),USE(?TOOLBAR1)
                                    BUTTON('Select'),AT(5,16,62,18),USE(?btnOpen),ICON('Pick.gif'),TIP('Select' & |
                                            ' json file'),LEFT
                                    BUTTON('Paste'),AT(67,16,62,18),USE(?btnPaste),ICON('Paste.gif'), |
                                            TIP('Paste from clipboard'),LEFT
                                    BUTTON('Save'),AT(264,16,62,18),USE(?btnSave),DISABLE,ICON('Save.gif'), |
                                            TIP('Save CLW file'),LEFT
                                    BUTTON('Copy'),AT(326,16,62,18),USE(?btnCopy),DISABLE,ICON('Copy.gif'), |
                                            TIP('Copy to clipboard'),LEFT
                                    GROUP('Mapping options'),AT(388,4,188,38),USE(?grpMappingOptions),BOXED
                                      CHECK('  External names'),AT(400,16,78),USE(options.EnableExternalNames, |
                                              , ?chkEnableExternalNames),FONT(,8),TIP('Generate NAME attributes')
                                      CHECK('  Comments'),AT(490,16,78),USE(options.EnableComments,, |
                                              ?chkEnableComments),FONT(,8),TIP('Enable comments')
                                      CHECK('  Rounding up'),AT(400,27,78),USE(options.RoundUpStringSize,, |
                                              ?chkRoundUpStringSize),FONT(,8),TIP('Round up strings size to the n' & |
                                              'ext poweer of 2')
                                      CHECK('  Generate program'),AT(490,27,78),USE(options.PrintCodeSample,, |
                                              ?chkPrintCodeSample),FONT(,8),TIP('Generate complete program code')
                                      BUTTON('?'),AT(612,0,14,10),USE(?btnAbout),FLAT
                                    END
                                  END
                                  TEXT,AT(2,0,256),FULL,USE(?txtInput),HVSCROLL,FONT('Courier New'),READONLY, |
                                          DROPID('~FILE')
                                  TEXT,AT(262,0,364),FULL,USE(?txtOutput),HVSCROLL,FONT('Courier New'),READONLY
                                END

                                COMPILE('!***_UI_ENHANCEMENTS_***', _UI_ENHANCEMENTS_)
sInputCue                       STRING(64), AUTO
sOutputCue                      STRING(64), AUTO
dtFormat                        LONG(DT_CENTER + DT_VCENTER + DT_SINGLELINE)
zoomInput                       TSmartZoomBase
zoomOutput                      TSmartZoomBase
cueMgr                          TCueBannerMgr
toggleMgr                       TToggleManagerBase
!***_UI_ENHANCEMENTS_***

Resizer                         WindowResizeClass

  MAP
    RefreshControls()
  END

  CODE
  sbInput.Init(8192)
  sbOutput.Init(8192)
  
  OPEN(MainWindow)
  
  !- resize strategy
  Resizer.Init(AppStrategy:Surface, TRUE)
  Resizer.SetStrategy(?txtInput, Resize:FixLeft+Resize:FixTop, Resize:LockWidth+Resize:ConstantBottom)
  Resizer.SetStrategy(?txtOutput, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:ConstantBottom)

  COMPILE('!***_UI_ENHANCEMENTS_***', _UI_ENHANCEMENTS_)
  zoomInput.Init(?txtInput)
  zoomOutput.Init(?txtOutput)

  toggleMgr.AddControls()

  sInputCue = CHOOSE(codePage = 1251, 'Перетащите файл сюда!', 'Drop a JSON file here!')
  sOutputCue = CHOOSE(codePage = 1251, 'Здесь будет код на Clarion', 'Here will be the code for Clarion')
  
  cueMgr.AddControl(?txtInput, sInputCue)
  cueMgr.AddControl(?txtOutput, sOutputCue)
  cueMgr.DrawTextFormat(?txtInput, dtFormat)
  cueMgr.DrawTextFormat(?txtOutput, dtFormat)
  cueMgr.SetFont(?txtInput, '', 20)
  cueMgr.SetFont(?txtOutput, '', 20)
  cueMgr.ShowInFocusAll(TRUE)
  cueMgr.ShowInReadOnlyAll(TRUE)
!***_UI_ENHANCEMENTS_***

  !- translation
  IF codePage = 1251
    ?btnOpen{PROP:Text} = 'Выбрать'
    ?btnPaste{PROP:Text} = 'Вставить'  
    ?btnSave{PROP:Text} = 'Сохранить'  
    ?btnCopy{PROP:Text} = 'Копировать'  
  
    ?btnOpen{PROP:Tip} = 'Выбрать json файл'
    ?btnPaste{PROP:Tip} = 'Вставить из буфера обмена'
    ?btnSave{PROP:Tip} = 'Сохранить CLW файл'
    ?btnCopy{PROP:Tip} = 'Копировать в буфер обмена'
    
    ?chkEnableExternalNames{PROP:Text} = '  Внешние имена'
    ?chkEnableComments{PROP:Text} = '  Комментарии'
    ?chkRoundUpStringSize{PROP:Text} = '  Округление'
    ?chkPrintCodeSample{PROP:Text} = '  Программа'
  
    ?chkEnableExternalNames{PROP:Tip} = 'Добавлять атрибуты NAME'
    ?chkEnableComments{PROP:Tip} = 'Добавлять комментарии'
    ?chkRoundUpStringSize{PROP:Tip} = 'Округлять размеры строк до следующей степени 2'
    ?chkPrintCodeSample{PROP:Tip} = 'Генерировать код тестовой программы'
    
    ?grpMappingOptions{PROP:Text} = 'Опции генерации кода'
  END

  !- Default mapper options
  options.EnableExternalNames = GETINI('Mapper options', 'EnableExternalNames', TRUE, sIniFileName)
  options.EnableComments = GETINI('Mapper options', 'EnableComments', TRUE, sIniFileName)
  options.RoundUpStringSize = GETINI('Mapper options', 'RoundUpStringSize', FALSE, sIniFileName)
  options.PrintCodeSample = GETINI('Mapper options', 'PrintCodeSample', FALSE, sIniFileName)
  options.ObjectName = GETINI('Mapper options', 'ObjectName', '', sIniFileName)
  options.ArrayName = GETINI('Mapper options', 'ArrayName', '', sIniFileName)
  options.PreferredColumn = GETINI('Mapper options', 'PreferredColumn', 0, sIniFileName)
  options.Indentation = GETINI('Mapper options', 'Indentation', 0, sIniFileName)

  !- UI enhancements
  ?btnAbout{PROP:FontName} = 'Webdings'
  ?btnAbout{PROP:FontSize} = 9
  ?btnAbout{PROP:Text} = '<73h>'
  ?btnAbout{PROP:Xpos} = 0{PROP:Width} - ?btnAbout{PROP:Width} - 4

  ACCEPT
    CASE EVENT()
    OF EVENT:Drop
      ProcessFile(DROPID(), options, sbInput, sbOutput)
      RefreshControls()
      
    OF EVENT:Sized
      ?btnAbout{PROP:Xpos} = 0{PROP:Width} - ?btnAbout{PROP:Width} - 4
      ?txtOutput{PROP:Width} = 0{PROP:Width} - ?txtOutput{PROP:Xpos}
    END
    
    CASE ACCEPTED()
    OF ?btnOpen
      IF FILEDIALOG(, sJsonFileName, 'json files|*.json;*.js|All files|*.*', FILE:KeepDir+FILE:LongName)
        ProcessFile(sJsonFileName, options, sbInput, sbOutput)
        RefreshControls()
      END
      
    OF ?btnPaste
      ProcessString(CLIPBOARD(), options, sbInput, sbOutput)
      RefreshControls()
 
    OF ?btnSave
      IF FILEDIALOG(, sClwFileName, 'CLW files|*.CLW|All files|*.*', FILE:Save+FILE:KeepDir+FILE:LongName+FILE:AddExtension)
        json::SaveFile(sClwFileName, sbOutput.Str())
        
      END
      
    OF ?btnCopy
      IF sbOutput.StrLen()
        SETCLIPBOARD(sbOutput.Str())
      END
      
    OF ?chkEnableComments OROF ?chkEnableExternalNames OROF ?chkPrintCodeSample OROF ?chkRoundUpStringSize
      !- immediately re-generate Clarion code
      IF sbInput.StrLen()
        GenerateCode(options, sbInput, sbOutput)
        RefreshControls()
      END
      
    OF ?btnAbout
      About(MainWindow)
    END
  END

  !- Save options
  PUTINI('Mapper options', 'EnableExternalNames', options.EnableExternalNames, sIniFileName)
  PUTINI('Mapper options', 'EnableComments', options.EnableComments, sIniFileName)
  PUTINI('Mapper options', 'RoundUpStringSize', options.RoundUpStringSize, sIniFileName)
  PUTINI('Mapper options', 'PrintCodeSample', options.PrintCodeSample, sIniFileName)

RefreshControls               PROCEDURE()
  CODE
  ?txtInput{PROP:Text} = sbInput.Str()
  ?txtOutput{PROP:Text} = sbOutput.Str()
           
  ?btnSave{PROP:Disable} = CHOOSE(sbOutput.StrLen() = 0)
  ?btnCopy{PROP:Disable} = CHOOSE(sbOutput.StrLen() = 0)

ProcessString                 PROCEDURE(STRING pJson, typCJsonPrintMapOptions pOptions, *TStringBuilder sbJson, *TStringBuilder sbClarion)
jParser                         cJSONFactory
jRoot                           &cJSON, AUTO
jMapper                         TCJsonMapper
  CODE
  sbJson.Reset()
  sbClarion.Reset()

  !- trying to parse json
  IF pJson
    jRoot &= jParser.Parse(pJson, codePage)
    IF NOT jRoot &= NULL
      sbJson.Cat(jRoot.ToString(TRUE))
      jRoot.Delete()

      GenerateCode(pOptions, sbJson, sbClarion)
    ELSE
      sbJson.Cat(printf('Error in json near "%s" at position %i.', jParser.GetError(), jParser.GetErrorPosition()))
    END
  END
  
ProcessFile                   PROCEDURE(STRING pFileName, typCJsonPrintMapOptions pOptions, *TStringBuilder sbJson, *TStringBuilder sbClarion)
sFileContent                    &STRING, AUTO
  CODE
  sbJson.Reset()
  sbClarion.Reset()
  
  sFileContent &= json::LoadFile(pFileName)
  IF NOT sFileContent &= NULL
    !- check for big file size (32K)
    IF SIZE(sFileContent) < 32000
      ProcessString(sFileContent, pOptions, sbJson, sbClarion)
    ELSE
      sbJson.Cat('Error: The file is too big.')
    END
    DISPOSE(sFileContent)
  ELSE
    sbJson.Cat('Error: the file cannot be loaded.')
  END

GenerateCode                  PROCEDURE(typCJsonPrintMapOptions pOptions, TStringBuilder sbJson, *TStringBuilder sbClarion)
jMapper                         TCJsonMapper
  CODE
  sbClarion.Reset()
  IF jMapper.MapJson(sbJson.Str())
    sbClarion.Cat(jMapper.PrintMap(pOptions, sbJson.Str()))
  ELSE
    sbClarion.Cat(jMapper.GetError())
  END

About                         PROCEDURE(WINDOW pParentWindow)
AboutWindow                     WINDOW,AT(,,201,87),CENTER,GRAY,FONT('Segoe UI',9)
                                  IMAGE('Feodor.jpg'),AT(7,8,49,65),USE(?IMAGE1)
                                  PROMPT('set in code'),AT(74,15),USE(?lblAboutText1)
                                  PROMPT('2024 Mike Duglas'),AT(74,27),USE(?lblAboutText2)
                                  PROMPT('mikeduglas@yandex.ru'),AT(74,40),USE(?lblAboutText3)
                                  BUTTON('Close'),AT(144,66,44),USE(?btnAboutClose),STD(STD:Close)
                                END
  CODE
  OPEN(AboutWindow)
  ?lblAboutText1{PROP:Text} = PROGRAM_NAME_AND_VERSION
  !- center this window
  AboutWindow{PROP:Xpos} = pParentWindow{PROP:Xpos} + (pParentWindow{PROP:Width} - AboutWindow{PROP:Width})/2
  AboutWindow{PROP:Ypos} = pParentWindow{PROP:Ypos} + (pParentWindow{PROP:Height} - AboutWindow{PROP:Height})/2
  ACCEPT
  END
