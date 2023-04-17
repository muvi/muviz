unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginType, ClientNetType, MPluginType, AuthNetType, DllStr,
  MainType, DefaultLogic, VisType;

procedure Init_Plugin;
procedure Plugin_Done;

implementation

const
  EMPTYVAR: Byte = 0;

procedure Init_Plugin;
begin
  //Das Plugin hier initialisieren
  with PluginSystem do begin
    RegisterVis('if','! IF    ',@TrigIf,['Bedingung'],[vBoolean],[],ConditionIV,['Then'],[oCall]);
    RegisterVis('if (with else)','! IF/!  ',@TrigIfElse,['Bedingung'],[vBoolean],[],ConditionIV,['Then','Else'],[oCall,oCall]);
    RegisterVis('while','! WHILE ',@TrigWhile,['Bedingung'],[vBoolean],[],ConditionIV,['Do'],[oCall]);
    RegisterVis('repeat .. until','! REPEAT',@TrigRepeat,['Bedingung'],[vBoolean],[],NotConditionIV,['Do'],[oCall]);
    RegisterVis('for (step 1)','! FOR+1 ',@TrigForStep1,['Startwert','Endwert'],[vInteger,vInteger],[],ForIV,['Do','I'],[oCall,oInteger]);
    RegisterVis('for (step -1)','! FOR-1 ',@TrigForStep_1,['Startwert','Endwert'],[vInteger,vInteger],[],ForIV,['Do','I'],[oCall,oInteger]);
    RegisterVis('for','! FOR   ',@TrigFor,['Startwert','Endwert','Schrittweite'],[vInteger,vInteger,vInteger],[],ForStepIV,['Do','I'],[oCall,oInteger]);

    RegisterVis('=','! Real= ',@TrigRealEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<>','! Real<>',@TrigRealNotEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<','! Real< ',@TrigRealLower,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('>','! Real> ',@TrigRealHigher,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<=','! Real<=',@TrigRealLowerOrEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('>=','! Real>=',@TrigRealHigherOrEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);

    RegisterVis('= (automatisch)','!AReal= ',nil,['Wert 1','Wert 2'],[vReal,vReal],[@TrigRealEqual,@TrigRealEqual],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<> (automatisch)','!AReal<>',nil,['Wert 1','Wert 2'],[vReal,vReal],[@TrigRealNotEqual,@TrigRealNotEqual],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('< (automatisch)','!AReal< ',nil,['Wert 1','Wert 2'],[vReal,vReal],[@TrigRealLower,@TrigRealLower],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('> (automatisch)','!AReal> ',nil,['Wert 1','Wert 2'],[vReal,vReal],[@TrigRealHigher,@TrigRealHigher],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<= (automatisch)','!AReal<=',nil,['Wert 1','Wert 2'],[vReal,vReal],[@TrigRealLowerOrEqual,@TrigRealLowerOrEqual],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('>= (automatisch)','!AReal>=',nil,['Wert 1','Wert 2'],[vReal,vReal],[@TrigRealHigherOrEqual,@TrigRealHigherOrEqual],RealCompareIV,['stimmt'],[oBoolean]);

    {RegisterVis('if (automatisch)','!ÂIF    ',@TrigIf,['Bedingung'],[vBoolean],[],ConditionIV,['Then'],[oCall]);
    RegisterVis('while (automatisch)','!ÂWHILE ',@TrigWhile,['Bedingung'],[vBoolean],[],ConditionIV,['Do'],[oCall]);
    RegisterVis('repeat .. until (automatisch)','!ÂREPEAT',@TrigRepeat,['Bedingung'],[vBoolean],[],NotConditionIV,['Do'],[oCall]);
    RegisterVis('for (step 1) (automatisch)','!ÂFOR+1 ',@TrigForStep1,['Startwert','Endwert'],[vInteger,vInteger],[],RoundIV,['Do','I'],[oCall,oInteger]);
    RegisterVis('for (step -1) (automatisch)','!ÂFOR-1 ',@TrigForStep_1,['Startwert','Endwert'],[vInteger,vInteger],[],RoundIV,['Do','I'],[oCall,oInteger]);
    RegisterVis('for (automatisch)','!ÂFOR   ',@TrigFor,['Startwert','Endwert','Schrittweite'],[vInteger,vInteger,vInteger],[],RoundIV,['Do','I'],[oCall,oInteger]);

    RegisterVis('= (automatisch)','!ÂReal= ',@TrigRealEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<> (automatisch)','!ÂReal<>',@TrigRealNotEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('< (automatisch)','!ÂReal< ',@TrigRealLower,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('> (automatisch)','!ÂReal> ',@TrigRealHigher,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('<= (automatisch)','!ÂReal<=',@TrigRealLowerOrEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);
    RegisterVis('>= (automatisch)','!ÂReal>=',@TrigRealHigherOrEqual,['Wert 1','Wert 2'],[vReal,vReal],[],RealCompareIV,['stimmt'],[oBoolean]);}
  end;
end;

procedure Plugin_Done;
begin
  //Das Plugin hier wieder freigeben
end;

end.

