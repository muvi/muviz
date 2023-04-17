const
  ptIntegerSize   = SizeOf(vpInt);
  ptRealSize      = SizeOf(vpReal);
  ptStringSize    = SizeOf(vpString);
  ptColorSize     = SizeOf(vpColor);
  ptBooleanSize   = SizeOf(vpBool);
  ptRealBufferSize= SizeOf(vpRealBuffer);

  TrueChars: set of Char = ['J','T','W','Y','S','j','t','w','y','s'];

function _RealBufferList(const ABuffer: vpRealBuffer_): vpString;
var
  I: Integer;
begin
  if ABuffer.Size<1 then begin
    Result:='';
    exit;
  end;
  Result:=FloatToStrF(ABuffer.Dest^[0],ffFixed,7,2);
  for I:=1 to ABuffer.Size-1 do Result+=' ;'+FloatToStrF(ABuffer.Dest^[I],ffFixed,7,2);
end;

{Sonderparametermethoden}

procedure _DoSetVisOutput__(const Data: TVisOutputData; const Value); stdcall;
begin
  //do nothing
end;

procedure _DoSetVisOutput_CallChanged_Call(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  AVis.CallVisProc(AProc);
end;

{Standardparamtermethoden}

procedure _DoSetVisOutput__Integer(const Data: TVisOutputData; const Value); stdcall;
begin
  Move(Value,Data.D^,ptIntegerSize);
end;

procedure _DoSetVisOutput__Real(const Data: TVisOutputData; const Value); stdcall;
begin
  Move(Value,Data.D^,ptRealSize);
end;

procedure _DoSetVisOutput__String(const Data: TVisOutputData; const Value); stdcall;
begin
  Move(Value,Data.D^,ptStringSize);
end;

procedure _DoSetVisOutput__Color(const Data: TVisOutputData; const Value); stdcall;
begin
  Move(Value,Data.D^,ptColorSize);
end;

procedure _DoSetVisOutput__Boolean(const Data: TVisOutputData; const Value); stdcall;
begin
  Move(Value,Data.D^,ptBooleanSize);
end;

procedure _DoSetVisOutput__RealBuffer(const Data: TVisOutputData; const Value); stdcall;
begin
  Move(Value,Data.D^,ptRealBufferSize);
end;

{Standardparametermethoden - CallChanged}

procedure _DoSetVisOutput_CallChanged_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  Move(Value,Data.D^,ptIntegerSize);
  //AProc(Dest,Source,AVis,AVis.VisParams^,AVis.Workspace^);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Real(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  Move(Value,Data.D^,ptRealSize);
  //AProc(Dest,Source,AVis,AVis.VisParams^,AVis.Workspace^);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_String(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  Move(Value,Data.D^,ptStringSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Color(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  Move(Value,Data.D^,ptColorSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  Move(Value,Data.D^,ptBooleanSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis : TVisualisation absolute Data.V;
  AProc: TVisProc absolute Data.P;
begin
  Move(Value,Data.D^,ptRealBufferSize);
  AVis.CallVisProc(AProc);
end;

{Integer}

procedure _DoSetVisOutput__Integer_Real(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpInt absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptRealSize);
end;

procedure _DoSetVisOutput__Integer_String(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpInt absolute Value;
begin
  NewVal:=IntToStr(InputVal);
  Move(NewVal,Data.D^,ptStringSize);
end;

procedure _DoSetVisOutput__Integer_Color(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpInt absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptColorSize);
end;

procedure _DoSetVisOutput__Integer_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpInt absolute Value;
begin
  NewVal:=(InputVal>0);
  Move(NewVal,Data.D^,ptBooleanSize);
end;

procedure _DoSetVisOutput__Integer_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpRealBuffer;
  //InputVal: vpInteger absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
end;

{Real}

procedure _DoSetVisOutput__Real_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpReal absolute Value;
begin
  NewVal:=Round(InputVal);
  Move(NewVal,Data.D^,ptIntegerSize);
end;

procedure _DoSetVisOutput__Real_String(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpReal absolute Value;
begin
  NewVal:=FloatToStrF(InputVal,ffFixed,7,2);
  Move(NewVal,Data.D^,ptStringSize);
end;

procedure _DoSetVisOutput__Real_Color(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpReal absolute Value;
begin
  NewVal:=Round(InputVal);
  Move(NewVal,Data.D^,ptColorSize);
end;

procedure _DoSetVisOutput__Real_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpReal absolute Value;
begin
  NewVal:=(InputVal>0.0);
  Move(NewVal,Data.D^,ptBooleanSize);
end;

procedure _DoSetVisOutput__Real_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpRealBuffer;
  //InputVal: vpReal absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
end;

{String}

procedure _DoSetVisOutput__String_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpString absolute Value;
begin
  NewVal:=StrToIntE(InputVal,0);
  Move(NewVal,Data.D^,ptIntegerSize);
end;

procedure _DoSetVisOutput__String_Real(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpString absolute Value;
begin
  NewVal:=StrToFloatE(InputVal,0.0);
  Move(NewVal,Data.D^,ptRealSize);
end;

procedure _DoSetVisOutput__String_Color(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpString absolute Value;
begin
  NewVal:=StrToIntB(InputVal,16,$00000000);
  Move(NewVal,Data.D^,ptColorSize);
end;

procedure _DoSetVisOutput__String_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpString absolute Value;
begin
  if Length(InputVal)>0
    then NewVal:=InputVal[1] in TrueChars
    else NewVal:=false;
  Move(NewVal,Data.D^,ptBooleanSize);
end;

procedure _DoSetVisOutput__String_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpRealBuffer;
  //InputVal: vpString absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
end;

{Color}

procedure _DoSetVisOutput__Color_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpColor absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptIntegerSize);
end;

procedure _DoSetVisOutput__Color_Real(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpColor absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptRealSize);
end;

procedure _DoSetVisOutput__Color_String(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpColor absolute Value;
begin
  NewVal:=IntToHex(InputVal,8);
  Move(NewVal,Data.D^,ptStringSize);
end;

procedure _DoSetVisOutput__Color_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpColor absolute Value;
begin
  NewVal:=(InputVal shr 24>$7F);
  Move(NewVal,Data.D^,ptBooleanSize);
end;

procedure _DoSetVisOutput__Color_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpRealBuffer;
  //InputVal: TColor32 absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
end;

{Boolean}

procedure _DoSetVisOutput__Boolean_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpBool absolute Value;
begin
  NewVal:=Ord(InputVal);
  Move(NewVal,Data.D^,ptIntegerSize);
end;

procedure _DoSetVisOutput__Boolean_Real(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpBool absolute Value;
begin
  NewVal:=Ord(InputVal);
  Move(NewVal,Data.D^,ptRealSize);
end;

procedure _DoSetVisOutput__Boolean_String(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpBool absolute Value;
begin
  if InputVal then NewVal:='Ja' else NewVal:='Nein';
  Move(NewVal,Data.D^,ptStringSize);
end;

procedure _DoSetVisOutput__Boolean_Color(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpBool absolute Value;
begin
  if InputVal then NewVal:=$FFFFFFFF else NewVal:=$00000000;
  Move(NewVal,Data.D^,ptBooleanSize);
end;

procedure _DoSetVisOutput__Boolean_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpRealBuffer;
  //InputVal: vpBoolean absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
end;

{RealBuffer}

procedure _DoSetVisOutput__RealBuffer_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpRealBuffer_ absolute Value;
begin
  NewVal:=InputVal.Size;
  Move(NewVal,Data.D^,ptIntegerSize);
end;
procedure _DoSetVisOutput__RealBuffer_Real(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpRealBuffer absolute Value;
begin
  NewVal:=RealBufferMiddle(InputVal);
  Move(NewVal,Data.D^,ptRealSize);
end;

procedure _DoSetVisOutput__RealBuffer_String(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpRealBuffer_ absolute Value;
begin
  NewVal:=_RealBufferList(InputVal);
  Move(NewVal,Data.D^,ptStringSize);
end;

procedure _DoSetVisOutput__RealBuffer_Color(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpRealBuffer_ absolute Value;
begin
  if InputVal.Size>0 then NewVal:=$FFFFFFFF else NewVal:=$00000000;
  Move(NewVal,Data.D^,ptColorSize);
end;

procedure _DoSetVisOutput__RealBuffer_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpRealBuffer_ absolute Value;
begin
  NewVal:=(InputVal.Size>0);
  Move(NewVal,Data.D^,ptBooleanSize);
end;

{Integer - CallChanged}

procedure _DoSetVisOutput_CallChanged_Integer_Real(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpReal;
  InputVal: vpInt absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptRealSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Integer_String(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpString;
  InputVal: vpInt absolute Value;
begin
  NewVal:=IntToStr(InputVal);
  Move(NewVal,Data.D^,ptStringSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Integer_Color(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpColor;
  InputVal: vpInt absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptColorSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Integer_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpBool;
  InputVal: vpInt absolute Value;
begin
  NewVal:=(InputVal>0);
  Move(NewVal,Data.D^,ptBooleanSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Integer_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpRealBuffer;
  //InputVal: vpInteger absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
  AVis.CallVisProc(AProc);
end;

{Real - CallChanged}

procedure _DoSetVisOutput_CallChanged_Real_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpInt;
  InputVal: vpReal absolute Value;
begin
  NewVal:=Round(InputVal);
  Move(NewVal,Data.D^,ptIntegerSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Real_String(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpString;
  InputVal: vpReal absolute Value;
begin
  NewVal:=FloatToStrF(InputVal,ffFixed,7,2);
  Move(NewVal,Data.D^,ptStringSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Real_Color(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpColor;
  InputVal: vpReal absolute Value;
begin
  NewVal:=Round(InputVal);
  Move(NewVal,Data.D^,ptColorSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Real_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpBool;
  InputVal: vpReal absolute Value;
begin
  NewVal:=(InputVal>0.0);
  Move(NewVal,Data.D^,ptBooleanSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Real_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpRealBuffer;
  //InputVal: vpReal absolute Value;
begin
  {NewVal.Size:=1;
  NewVal.Dest:=@Value;}
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
  AVis.CallVisProc(AProc);
end;

{String - CallChanged}

procedure _DoSetVisOutput_CallChanged_String_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpInt;
  InputVal: vpString absolute Value;
begin
  NewVal:=StrToIntE(InputVal,0);
  Move(NewVal,Data.D^,ptIntegerSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_String_Real(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpReal;
  InputVal: vpString absolute Value;
begin
  NewVal:=StrToFloatE(InputVal,0.0);
  Move(NewVal,Data.D^,ptRealSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_String_Color(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpColor;
  InputVal: vpString absolute Value;
begin
  NewVal:=StrToIntB(InputVal,16,$00000000);
  Move(NewVal,Data.D^,ptColorSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_String_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpBool;
  InputVal: vpString absolute Value;
begin
  if Length(InputVal)>0
    then NewVal:=InputVal[1] in TrueChars
    else NewVal:=false;
  Move(NewVal,Data.D^,ptBooleanSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_String_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpRealBuffer;
  //InputVal: vpString absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
  AVis.CallVisProc(AProc);
end;

{Color - CallChanged}

procedure _DoSetVisOutput_CallChanged_Color_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpInt;
  InputVal: vpColor absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptIntegerSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Color_Real(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpReal;
  InputVal: vpColor absolute Value;
begin
  NewVal:=InputVal;
  Move(NewVal,Data.D^,ptRealSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Color_String(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpString;
  InputVal: vpColor absolute Value;
begin
  NewVal:=IntToHex(InputVal,8);
  Move(NewVal,Data.D^,ptStringSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Color_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpBool;
  InputVal: vpColor absolute Value;
begin
  NewVal:=(InputVal shr 24>$7F);
  Move(NewVal,Data.D^,ptBooleanSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Color_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpRealBuffer;
  //InputVal: vpColor absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
  AVis.CallVisProc(AProc);
end;

{Boolean - CallChanged}

procedure _DoSetVisOutput_CallChanged_Boolean_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpInt;
  InputVal: vpBool absolute Value;
begin
  NewVal:=Ord(InputVal);
  Move(NewVal,Data.D^,ptIntegerSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Boolean_Real(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpReal;
  InputVal: vpBool absolute Value;
begin
  NewVal:=Ord(InputVal);
  Move(NewVal,Data.D^,ptRealSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Boolean_String(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpString;
  InputVal: vpBool absolute Value;
begin
  if InputVal then NewVal:='Ja' else NewVal:='Nein';
  Move(NewVal,Data.D^,ptStringSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Boolean_Color(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpColor;
  InputVal: vpBool absolute Value;
begin
  if InputVal then NewVal:=$FFFFFFFF else NewVal:=$00000000;
  Move(NewVal,Data.D^,ptColorSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_Boolean_RealBuffer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpRealBuffer;
  //InputVal: vpBoolean absolute Value;
begin
  NewVal:=VPEMPTYBUFFER;
  Move(NewVal,Data.D^,ptRealBufferSize);
  AVis.CallVisProc(AProc);
end;

{RealBuffer - CallChanged}

procedure _DoSetVisOutput_CallChanged_RealBuffer_Integer(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpInt;
  InputVal: vpRealBuffer_ absolute Value;
begin
  NewVal:=InputVal.Size;
  Move(NewVal,Data.D^,ptIntegerSize);
  AVis.CallVisProc(AProc);
end;
procedure _DoSetVisOutput_CallChanged_RealBuffer_Real(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpReal;
  InputVal: vpRealBuffer absolute Value;
begin
  NewVal:=RealBufferMiddle(InputVal);
  Move(NewVal,Data.D^,ptRealSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_RealBuffer_String(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpString;
  InputVal: vpRealBuffer_ absolute Value;
begin
  NewVal:=_RealBufferList(InputVal);
  Move(NewVal,Data.D^,ptStringSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_RealBuffer_Color(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpColor;
  InputVal: vpRealBuffer_ absolute Value;
begin
  if InputVal.Size>0 then NewVal:=$FFFFFFFF else NewVal:=$00000000;
  Move(NewVal,Data.D^,ptColorSize);
  AVis.CallVisProc(AProc);
end;

procedure _DoSetVisOutput_CallChanged_RealBuffer_Boolean(const Data: TVisOutputData; const Value); stdcall;
var
  AVis    : TVisualisation absolute Data.V;
  AProc   : TVisProc absolute Data.P;
  NewVal  : vpBool;
  InputVal: vpRealBuffer_ absolute Value;
begin
  NewVal:=(InputVal.Size>0);
  Move(NewVal,Data.D^,ptBooleanSize);
  AVis.CallVisProc(AProc);
end;

{
const
  _DoSetVisOutput_Converters_: array [Boolean] of array [vCall..vBuffer] of array [oCall..oBuffer] of TVisOutputSet =

    //false
    //vCall
    (((@_DoSetVisOutput__,@_DoSetVisOutput__,@_DoSetVisOutput__,@_DoSetVisOutput__,@_DoSetVisOutput__,@_DoSetVisOutput__,@_DoSetVisOutput__),
    //vInteger
    (@_DoSetVisOutput__,@_DoSetVisOutput__Integer,@_DoSetVisOutput__Real_Integer,@_DoSetVisOutput__String_Integer,@_DoSetVisOutput__Color_Integer,@_DoSetVisOutput__Boolean_Integer,@_DoSetVisOutput__RealBuffer_Integer),
    //vReal
    (@_DoSetVisOutput__,@_DoSetVisOutput__Integer_Real,@_DoSetVisOutput__Real,@_DoSetVisOutput__String_Real,@_DoSetVisOutput__Color_Real,@_DoSetVisOutput__Boolean_Real,@_DoSetVisOutput__RealBuffer_Real),
    //vString
    (@_DoSetVisOutput__,@_DoSetVisOutput__Integer_String,@_DoSetVisOutput__Real_String,@_DoSetVisOutput__String,@_DoSetVisOutput__Color_String,@_DoSetVisOutput__Boolean_String,@_DoSetVisOutput__RealBuffer_String),
    //vColor
    (@_DoSetVisOutput__,@_DoSetVisOutput__Integer_Color,@_DoSetVisOutput__Real_Color,@_DoSetVisOutput__String_Color,@_DoSetVisOutput__Color,@_DoSetVisOutput__Boolean_Color,@_DoSetVisOutput__RealBuffer_Color),
    //vBoolean
    (@_DoSetVisOutput__,@_DoSetVisOutput__Integer_Boolean,@_DoSetVisOutput__Real_Boolean,@_DoSetVisOutput__String_Boolean,@_DoSetVisOutput__Color_Boolean,@_DoSetVisOutput__Boolean,@_DoSetVisOutput__RealBuffer_Boolean),
    //vBuffer
    (@_DoSetVisOutput__,@_DoSetVisOutput__Integer_RealBuffer,@_DoSetVisOutput__Real_RealBuffer,@_DoSetVisOutput__String_RealBuffer,@_DoSetVisOutput__Color_RealBuffer,@_DoSetVisOutput__Boolean_RealBuffer,@_DoSetVisOutput__RealBuffer)),

    //true
    //vCall
    ((@_DoSetVisOutput__Call,@_DoSetVisOutput__Call,@_DoSetVisOutput__Call,@_DoSetVisOutput__Call,@_DoSetVisOutput__Call,@_DoSetVisOutput__Call,@_DoSetVisOutput__Call),
    //vInteger
    (@_DoSetVisOutput__Call,@_DoSetVisOutput_CallChanged_Integer,@_DoSetVisOutput_CallChanged_Real_Integer,@_DoSetVisOutput_CallChanged_String_Integer,@_DoSetVisOutput_CallChanged_Color_Integer,@_DoSetVisOutput_CallChanged_Boolean_Integer,@_DoSetVisOutput_CallChanged_RealBuffer_Integer),
    //vReal
    (@_DoSetVisOutput__Call,@_DoSetVisOutput_CallChanged_Integer_Real,@_DoSetVisOutput_CallChanged_Real,@_DoSetVisOutput_CallChanged_String_Real,@_DoSetVisOutput_CallChanged_Color_Real,@_DoSetVisOutput_CallChanged_Boolean_Real,@_DoSetVisOutput_CallChanged_RealBuffer_Real),
    //vString
    (@_DoSetVisOutput__Call,@_DoSetVisOutput_CallChanged_Integer_String,@_DoSetVisOutput_CallChanged_Real_String,@_DoSetVisOutput_CallChanged_String,@_DoSetVisOutput_CallChanged_Color_String,@_DoSetVisOutput_CallChanged_Boolean_String,@_DoSetVisOutput_CallChanged_RealBuffer_String),
    //vColor
    (@_DoSetVisOutput__Call,@_DoSetVisOutput_CallChanged_Integer_Color,@_DoSetVisOutput_CallChanged_Real_Color,@_DoSetVisOutput_CallChanged_String_Color,@_DoSetVisOutput_CallChanged_Color,@_DoSetVisOutput_CallChanged_Boolean_Color,@_DoSetVisOutput_CallChanged_RealBuffer_Color),
    //vBoolean
    (@_DoSetVisOutput__Call,@_DoSetVisOutput_CallChanged_Integer_Boolean,@_DoSetVisOutput_CallChanged_Real_Boolean,@_DoSetVisOutput_CallChanged_String_Boolean,@_DoSetVisOutput_CallChanged_Color_Boolean,@_DoSetVisOutput_CallChanged_Boolean,@_DoSetVisOutput_CallChanged_RealBuffer_Boolean),
    //vBuffer
    (@_DoSetVisOutput__Call,@_DoSetVisOutput_CallChanged_Integer_RealBuffer,@_DoSetVisOutput_CallChanged_Real_RealBuffer,@_DoSetVisOutput_CallChanged_String_RealBuffer,@_DoSetVisOutput_CallChanged_Color_RealBuffer,@_DoSetVisOutput_CallChanged_Boolean_RealBuffer,@_DoSetVisOutput_CallChanged_RealBuffer)));
}
