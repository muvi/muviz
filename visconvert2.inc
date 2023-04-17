{Sonderparametermethoden}

procedure _DoGetVisOutput__(const Data: TVisOutputData; out Value); stdcall;
begin
  //do nothing
end;

{Standardparamtermethoden}

procedure _DoGetVisOutput__Integer(const Data: TVisOutputData; out Value); stdcall;
begin
  Move(Data.D^,Value,ptIntegerSize);
end;

procedure _DoGetVisOutput__Real(const Data: TVisOutputData; out Value); stdcall;
begin
  Move(Data.D^,Value,ptRealSize);
end;

procedure _DoGetVisOutput__String(const Data: TVisOutputData; out Value); stdcall;
begin
  Move(Data.D^,Value,ptStringSize);
end;

procedure _DoGetVisOutput__Color(const Data: TVisOutputData; out Value); stdcall;
begin
  Move(Data.D^,Value,ptColorSize);
end;

procedure _DoGetVisOutput__Boolean(const Data: TVisOutputData; out Value); stdcall;
begin
  Move(Data.D^,Value,ptBooleanSize);
end;

procedure _DoGetVisOutput__RealBuffer(const Data: TVisOutputData; out Value); stdcall;
begin
  Move(Data.D^,Value,ptRealBufferSize);
end;

{Integer}

procedure _DoGetVisOutput__Integer_Call(const Data: TVisOutputData; out Value); stdcall;
var
  InputVal: vpInt absolute Value;
begin
  InputVal:=0;
end;

procedure _DoGetVisOutput__Integer_Real(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpInt absolute Value;
begin
  Move(Data.D^,NewVal,ptRealSize);
  InputVal:=Round(NewVal);
end;

procedure _DoGetVisOutput__Integer_String(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpInt absolute Value;
begin
  Move(Data.D^,NewVal,ptStringSize);
  InputVal:=StrToIntE(NewVal)
end;

procedure _DoGetVisOutput__Integer_Color(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpInt absolute Value;
begin
  Move(Data.D^,NewVal,ptColorSize);
  InputVal:=NewVal;
end;

procedure _DoGetVisOutput__Integer_Boolean(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpInt absolute Value;
begin
  Move(Data.D^,NewVal,ptBooleanSize);
  InputVal:=Ord(NewVal);
end;

procedure _DoGetVisOutput__Integer_RealBuffer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpRealBuffer_;
  InputVal: vpInt absolute Value;
begin
  Move(Data.D^,NewVal,ptRealBufferSize);
  InputVal:=NewVal.Size;
end;

{Real}

procedure _DoGetVisOutput__Real_Call(const Data: TVisOutputData; out Value); stdcall;
var
  InputVal: vpReal absolute Value;
begin
  InputVal:=0.0;
end;

procedure _DoGetVisOutput__Real_Integer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpReal absolute Value;
begin
  Move(Data.D^,NewVal,ptIntegerSize);
  InputVal:=NewVal;
end;

procedure _DoGetVisOutput__Real_String(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpReal absolute Value;
begin
  Move(Data.D^,NewVal,ptStringSize);
  InputVal:=StrToFloatE(NewVal,0.0);
end;

procedure _DoGetVisOutput__Real_Color(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpReal absolute Value;
begin
  Move(Data.D^,NewVal,ptColorSize);
  InputVal:=NewVal;
end;

procedure _DoGetVisOutput__Real_Boolean(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpReal absolute Value;
begin
  Move(Data.D^,NewVal,ptBooleanSize);
  InputVal:=Ord(NewVal);
end;

procedure _DoGetVisOutput__Real_RealBuffer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpRealBuffer;
  InputVal: vpReal absolute Value;
begin
  Move(Data.D^,NewVal,ptRealBufferSize);
  InputVal:=RealBufferMiddle(NewVal);
end;

{String}

procedure _DoGetVisOutput__String_Call(const Data: TVisOutputData; out Value); stdcall;
var
  InputVal: vpString absolute Value;
begin
  InputVal:='';
end;

procedure _DoGetVisOutput__String_Integer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpString absolute Value;
begin
  Move(Data.D^,NewVal,ptIntegerSize);
  InputVal:=IntToStr(NewVal);
end;

procedure _DoGetVisOutput__String_Real(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpString absolute Value;
begin
  Move(Data.D^,NewVal,ptRealSize);
  InputVal:=FloatToStrF(NewVal,ffFixed,7,2);
end;

procedure _DoGetVisOutput__String_Color(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpString absolute Value;
begin
  Move(Data.D^,NewVal,ptColorSize);
  InputVal:=IntToHex(NewVal,8);
end;

procedure _DoGetVisOutput__String_Boolean(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpString absolute Value;
begin
  Move(Data.D^,NewVal,ptBooleanSize);
  if NewVal then InputVal:='Ja' else InputVal:='Nein';
end;

procedure _DoGetVisOutput__String_RealBuffer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpRealBuffer_;
  InputVal: vpString absolute Value;
begin
  Move(Data.D^,NewVal,ptRealBufferSize);
  InputVal:=_RealBufferList(NewVal);
end;

{Color}

procedure _DoGetVisOutput__Color_Call(const Data: TVisOutputData; out Value); stdcall;
var
  InputVal: vpColor absolute Value;
begin
  InputVal:=$00000000;
end;

procedure _DoGetVisOutput__Color_Integer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpColor absolute Value;
begin
  Move(Data.D^,NewVal,ptIntegerSize);
  InputVal:=NewVal;
end;

procedure _DoGetVisOutput__Color_Real(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpColor absolute Value;
begin
  Move(Data.D^,NewVal,ptRealSize);
  InputVal:=Round(NewVal);
end;

procedure _DoGetVisOutput__Color_String(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpColor absolute Value;
begin
  Move(Data.D^,NewVal,ptStringSize);
  InputVal:=StrToIntB(NewVal,16,$00000000);
end;

procedure _DoGetVisOutput__Color_Boolean(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpBool;
  InputVal: vpColor absolute Value;
begin
  Move(Data.D^,NewVal,ptBooleanSize);
  if NewVal then InputVal:=$FFFFFFFF else InputVal:=$00000000;
end;

procedure _DoGetVisOutput__Color_RealBuffer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpRealBuffer_;
  InputVal: vpColor absolute Value;
begin
  Move(Data.D^,NewVal,ptRealBufferSize);
  if NewVal.Size>0 then InputVal:=$FFFFFFFF else InputVal:=$00000000;
end;

{Boolean}

procedure _DoGetVisOutput__Boolean_Call(const Data: TVisOutputData; out Value); stdcall;
var
  InputVal: vpBool absolute Value;
begin
  InputVal:=false;
end;

procedure _DoGetVisOutput__Boolean_Integer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpInt;
  InputVal: vpBool absolute Value;
begin
  Move(Data.D^,NewVal,ptIntegerSize);
  InputVal:=(NewVal>0);
end;

procedure _DoGetVisOutput__Boolean_Real(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpReal;
  InputVal: vpBool absolute Value;
begin
  Move(Data.D^,NewVal,ptRealSize);
  InputVal:=(NewVal>0.0);
end;

procedure _DoGetVisOutput__Boolean_String(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpString;
  InputVal: vpBool absolute Value;
begin
  Move(Data.D^,NewVal,ptStringSize);
  if Length(NewVal)>0
    then InputVal:=NewVal[1] in TrueChars
    else InputVal:=false;
end;

procedure _DoGetVisOutput__Boolean_Color(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpColor;
  InputVal: vpBool absolute Value;
begin
  Move(Data.D^,NewVal,ptBooleanSize);
  InputVal:=(NewVal shr 24>$7F);
end;

procedure _DoGetVisOutput__Boolean_RealBuffer(const Data: TVisOutputData; out Value); stdcall;
var
  NewVal  : vpRealBuffer_;
  InputVal: vpBool absolute Value;
begin
  Move(Data.D^,NewVal,ptRealBufferSize);
  InputVal:=(NewVal.Size>0);
end;

{RealBuffer}

procedure _DoGetVisOutput__RealBuffer_(const Data: TVisOutputData; out Value); stdcall;
var
  InputVal: vpRealBuffer absolute Value;
begin
  InputVal:=VPEMPTYBUFFER;
end;

{
const
  _DoGetVisOutput_Converters_: array [vCall..vBuffer] of array [oCall..oBuffer] of TVisOutputGet =
    //vCall
    ((@_DoGetVisOutput__,@_DoGetVisOutput__Integer_Call,@_DoGetVisOutput__Real_Call,@_DoGetVisOutput__String_Call,@_DoGetVisOutput__Color_Call,@_DoGetVisOutput__Boolean_Call,@_DoGetVisOutput__RealBuffer_),
    //vInteger
    (@_DoGetVisOutput__,@_DoGetVisOutput__Integer,@_DoGetVisOutput__Real_Integer,@_DoGetVisOutput__String_Integer,@_DoGetVisOutput__Color_Integer,@_DoGetVisOutput__Boolean_Integer,@_DoGetVisOutput__RealBuffer_),
    //vReal
    (@_DoGetVisOutput__,@_DoGetVisOutput__Integer_Real,@_DoGetVisOutput__Real,@_DoGetVisOutput__String_Real,@_DoGetVisOutput__Color_Real,@_DoGetVisOutput__Boolean_Real,@_DoGetVisOutput__RealBuffer_),
    //vString
    (@_DoGetVisOutput__,@_DoGetVisOutput__Integer_String,@_DoGetVisOutput__Real_String,@_DoGetVisOutput__String,@_DoGetVisOutput__Color_String,@_DoGetVisOutput__Boolean_String,@_DoGetVisOutput__RealBuffer_),
    //vColor
    (@_DoGetVisOutput__,@_DoGetVisOutput__Integer_Color,@_DoGetVisOutput__Real_Color,@_DoGetVisOutput__String_Color,@_DoGetVisOutput__Color,@_DoGetVisOutput__Boolean_Color,@_DoGetVisOutput__RealBuffer_),
    //vBoolean
    (@_DoGetVisOutput__,@_DoGetVisOutput__Integer_Boolean,@_DoGetVisOutput__Real_Boolean,@_DoGetVisOutput__String_Boolean,@_DoGetVisOutput__Color_Boolean,@_DoGetVisOutput__Boolean,@_DoGetVisOutput__RealBuffer_),
    //vBuffer
    (@_DoGetVisOutput__,@_DoGetVisOutput__Integer_RealBuffer,@_DoGetVisOutput__Real_RealBuffer,@_DoGetVisOutput__String_RealBuffer,@_DoGetVisOutput__Color_RealBuffer,@_DoGetVisOutput__Boolean_RealBuffer,@_DoGetVisOutput__RealBuffer));
}
