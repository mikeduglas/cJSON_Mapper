!** cJSON mapper v1.01
!** 04.11.2024
!** mikeduglas@yandex.com
!** mikeduglas66@gmail.com

  INCLUDE('cjson.inc'), ONCE


typCJsonPrintMapOptions       GROUP, TYPE
ObjectName                      STRING(32)
ArrayName                       STRING(32)
RoundUpStringSize               BOOL
EnableExternalNames             BOOL    !- print NAME('externalname') attribute
EnableComments                  BOOL    !- print field comment
PreferredColumn                 LONG
Indentation                     LONG
PrintCodeSample                 BOOL
                              END


TCJsonMapper                  CLASS, TYPE, MODULE('cjsonmap.clw'),LINK('cjsonmap.clw')
qMap                            &typCjsonMap, PRIVATE
parseErrorString                STRING(20), PRIVATE
parseErrorPos                   LONG, PRIVATE

Construct                       PROCEDURE()
Destruct                        PROCEDURE(), VIRTUAL

MapJson                         PROCEDURE(STRING pJson), BOOL, PROC
PrintMap                        PROCEDURE(<STRING pJson>), STRING
PrintMap                        PROCEDURE(typCJsonPrintMapOptions pOptions, <STRING pJson>), STRING
GetError                        PROCEDURE(), STRING

MapItem                         PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapString                       PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapNumber                       PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapBoolean                      PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapNull                         PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapObject                       PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapArray                        PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapSimpleArray                  PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
MapComplexArray                 PROCEDURE(cJSON pitem, LONG pLevel, STRING pParentName), PRIVATE
                              END
