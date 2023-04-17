unit UltraSort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpectrumData, Dialogs, Math, PluginType;

type
  TUltraSortListItem= packed record
    Next,Prev,Lower,Higher,NextBPM                : Pointer;
    BPMSum,FirstBPM,BPMCount,StolenSum,StolenCount: MVFloat;
    StolenTime,BPMTime                            : MVInt;
    Index: Integer;
  end;
  TUltraSortListData= array of TUltraSortListItem;
  TDataCompareEvent = function (const Data1,Data2): Boolean of object;
  PUltraSortListItem= ^TUltraSortListItem;

  TUltraSortList    = class
  private
    FSize             : Integer;
    //FCount            : Integer;
    FData             : Pointer{TUltraSortListData};
    FBPMTolerance     : MVFloat;
    //FBPMTolerance2    : MVFLoat;
    FBPM              : MVFloat;
    //FExpectedBPM      : MVFloat;
    //FBPMContinuity    : MVInt;
    //FBPMSemiContinuity: MVInt;
    //FBPMAntiContinuity: MVInt;
    //FIsBeat           : Boolean;
    //FWasBeat          : Boolean;
    procedure SetSize(const Value: Integer);
  public
    FFirstBPM: Pointer{PUltraSortListItem};
    FFirst   : Pointer{PUltraSortListItem};
    FLast    : Pointer{PUltraSortListItem};
    FCenter  : Pointer{PUltraSortListItem};
    FDeleted : Pointer{PUltraSortListItem};
    FUsed    : Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    //procedure SetCount(const ABPM: MVFloat; const ABPMTime,ACount: MVInt);
    procedure AddBPM(const ABPM: MVFloat; const ABPMTime: MVInt);
    procedure DeleteBPM(const ABPM: MVFloat; const ABPMTime: MVInt);
    function FindBPM(const AConTime: MVInt): MVFloat;
  published
    property BPM: MVFloat read FBPM;
    property BPMTolerance: MVFloat read FBPMTolerance write FBPMTolerance;
    //property Count: Integer read FCount;
    property Size: Integer read FSize write SetSize;
  end;

implementation

const
  ln2        = 0.69314718055994530941723212145818;
  USLDataSize= SizeOf(TUltraSortListItem);

type
  TUSLData = array [0..0] of TUltraSortListItem;
  //PUSLData = ^TUSLData;

{TUltraSortList}

constructor TUltraSortList.Create;
begin
  inherited Create;
  FSize:=0;
  //FCount:=0;
  FFirst:=nil;
  FFirstBPM:=nil;
  FLast:=nil;
  FCenter:=nil;
  FDeleted:=nil;
  FData:=nil;
  FUsed:=0;
  //FBPMContinuity:=0;
  //FBPMSemiContinuity:=0;
  //FBPMAntiContinuity:=0;
  FBPMTolerance:=1.0;
  //FBPMTolerance2:=5.0;
  //FExpectedBPM:=0.0;
  //FIsBeat:=false;
  //FWasBeat:=false;
end;

destructor TUltraSortList.Destroy;
begin
  //SetSize(0);
  if FSize>0 then FreeMem(FData,FSize*USLDataSize);
  //SetLength(FData,0);
  inherited Destroy;
end;

procedure TUltraSortList.SetSize(const Value: Integer);
var
  I   : Integer;
  ABufPtr: Pointer;
  ABuf: PUltraSortListItem{PUSLData absolute FData} absolute ABufPtr;
begin
  //SetLength(FData,Value);
  if FSize>0 then FreeMem(FData,FSize*USLDataSize);
  GetMem(FData,Value*USLDataSize);
  FSize:=Value;
  if Value>0 then begin
    for I:=0 to Value-2 do begin
      ABufPtr:=FData+(I*USLDataSize);
      ABuf^.Next:=FData+((I+1)*USLDataSize);
    end;
    ABufPtr:=FData+((Value-1)*USLDataSize);
    ABuf^.Next:=nil;
    FDeleted:=FData;
  end else FDeleted:=nil;
  FFirst:=nil;
  FLast:=nil;
  FCenter:=nil;
  FUsed:=0;
  //Clear
end;

procedure TUltraSortList.Clear;
begin
  if FFirst<>nil then begin
    //if FLast=nil then ShowMessage('mist');
    PUltraSortListItem(FLast)^.Next:=FDeleted;
    FDeleted:=FFirst;
  end;

  {FFirst:=FDeleted;
  I:=0;
  while FFirst<>nil do begin
    PUltraSortListItem(FFirst)^.Lower:=nil;
    PUltraSortListItem(FFirst)^.Higher:=nil;
    FFirst:=PUltraSortListItem(FFirst)^.Next;
    //FFirst^.
    Inc(I);
  end;
  if I<>FSize then ShowMessage('wrong size'); }

  FFirst:=nil;
  //FFirstBPM:=nil;
  FLast:=nil;
  FCenter:=nil;
  FUsed:=0;
  //FDeleted:=nil;
  //FCount:=0;
end;

{procedure TUltraSortList.SetCount(const ABPM: MVFloat; const ABPMTime,ACount: MVInt);
begin
  Clear;
  FFirst:=FDeleted;
  FLast:=FDeleted;
  FCenter:=FDeleted;
  FDeleted:=FDeleted^.Next;
  with FCenter^ do begin
    Next:=nil;
    Prev:=nil;
    Lower:=nil
    Higher:=nil;
    BPMSum:=ABPM*ACount;
    FirstBPM:=ABPM;
    BPMCount:=ACount
    //StolenSum:=0;,StolenCount: MVFloat;
    //StolenTime,BPMTime                            : MVInt;
    BPMTime:=ABPMTime;
  end;
end; }

procedure TUltraSortList.DeleteBPM(const ABPM: MVFloat; const ABPMTime: MVInt);
var
  ANext   : Pointer;
  ANextPtr: ^Pointer;
  ANextR  : PUltraSortListItem absolute ANext;

  procedure DelItem;
  var
    ANext2   : Pointer;
    ANext2Ptr: ^Pointer;
    ANext2R  : PUltraSortListItem absolute ANext2;
  begin
    if ANextR^.Higher<>nil then begin
      if ANextR^.Lower<>nil then begin
        ANext2Ptr:=@(ANextR^.Lower);
        ANext2:=ANext2Ptr^;
        while ANext2<>nil do begin
          ANext2Ptr:=@(ANext2R^.Higher);
          ANext2:=ANext2Ptr^;
        end;
        ANext2Ptr^:=ANextR^.Higher;
        ANextPtr^:=ANextR^.Lower;
      end else begin
     {   ANextR^.Next:=FDeleted;
        FDeleted:=ANext;}
        ANextPtr^:=ANextR^.Higher;
      end;
      //ANextPtr^:=ANextR^.Higher;
    end else begin
      //if ANextR^.Lower<>nil then begin
      {ANextR^.Next:=FDeleted;
      FDeleted:=ANext;
      //end;             }
      ANextPtr^:=ANextR^.Lower;
    end;

    if ANextR^.Prev<>nil
      then PUltraSortListItem(ANextR^.Prev)^.Next:=ANextR^.Next
      else if FFirst=ANext then FFirst:=ANextR^.Next;
    if ANextR^.Next<>nil
      then PUltraSortListItem(ANextR^.Next)^.Prev:=ANextR^.Prev
      else if FLast=ANext then FLast:=ANextR^.Prev;
    Dec(FUsed);

    ANextR^.Next:=FDeleted;
    FDeleted:=ANext;
  end;

begin
  //ShowMessage('mist');
  if IsZero(ABPM) then exit;
  ANext:=FCenter;
  ANextPtr:=@FCenter;
  while ANext<>nil do begin
    if ABPM>ANextR^.FirstBPM then begin
      if ABPM-ANextR^.FirstBPM<FBPMTolerance then with ANextR^ do begin
        BPMSum-=ABPM;
        BPMTime-=ABPMTime;
        BPMCount-=1.0;
        if BPMCount<=0.00001 then DelItem;
        exit;
      end else begin
        ANextPtr:=@(ANextR^.Higher);
        ANext:=ANextPtr^;
      end;
    end else begin
      if ANextR^.FirstBPM-ABPM<FBPMTolerance then with ANextR^ do begin
        BPMSum-=ABPM;
        BPMTime-=ABPMTime;
        BPMCount-=1.0;
        if BPMCount<=0.00001 then DelItem;
        exit;
      end else begin
        ANextPtr:=@(ANextR^.Lower);
        ANext:=ANextPtr^;
      end;
    end;
  end;
end;

procedure TUltraSortList.AddBPM(const ABPM: MVFloat; const ABPMTime: MVInt);
var
  ANext    : Pointer{PUltraSortListItem};
  //APrev    : PUltraSortListItem;
  ANextPtr : ^Pointer{PUltraSortListItem};
  //ANextPtr2: ^PUltraSortListItem;
  ANextR: PUltraSortListItem absolute ANext;
begin
  if IsZero(ABPM) then exit;
  ANext:=FCenter;
  ANextPtr:=@FCenter;
  //ANextPtr2:=@FFirst;
  //APrev:=nil;
  while ANext<>nil do begin
    if ABPM>ANextR^.FirstBPM then begin
      if ABPM-ANextR^.FirstBPM<FBPMTolerance then with ANextR^ do begin
        BPMSum+=ABPM;
        BPMCount+=1.0;
        BPMTime+=ABPMTime;
        exit;
      end else begin
        ANextPtr:=@(ANextR^.Higher);
        //ANextPtr2:=@ANext^.Next;
        //APrev:=ANext;
        ANext:=ANextR^.Higher{ANextPtr^};
        if ANext<>nil then ANextR^.FirstBPM:=ANextR^.FirstBPM;
      end;
    end else begin
      if ANextR^.FirstBPM-ABPM<FBPMTolerance then with ANextR^ do begin
        BPMSum+=ABPM;
        BPMCount+=1.0;
        BPMTime+=ABPMTime;
        exit;
      end else begin
        ANextPtr:=@(ANextR^.Lower);
        //ANextPtr2:=@ANext^.Next;
        //APrev:=ANext;
        ANext:=ANextR^.Lower{ANextPtr^};
        if ANext<>nil then ANextR^.FirstBPM:=ANextR^.FirstBPM;
      end;
    end;
  end;

  FFirstBPM:=PUltraSortListItem(FDeleted)^.Next;

  with {ANextPtr^}PUltraSortListItem(FDeleted)^ do begin
    BPMCount:=1.0;
    BPMSum:=ABPM;
    FirstBPM:=ABPM;
    BPMTime:=ABPMTime;
    //StolenTime:=0;
    Next:=nil;
    Prev:=FLast{APrev};

    //APrev^.Next:=
    //ANext^.Next:=ANextPtr^; (***)
    //FLast:=FFirstBPM{ANextPtr^};
    //if FLast^.Next<>nil then ShowMessage('müll');
    //NextBPM:=nil;
    //StolenTime:=0;
    Lower:=nil;
    Higher:=nil;
  end;
  if FLast<>nil then PUltraSortListItem(FLast)^.Next:=FDeleted;
  if FFirst=nil then FFirst:=FDeleted;
  FLast:=FDeleted;
  ANextPtr^:=FDeleted;
  Inc(FUsed);
  if FUsed>FSize-1 then ShowMessage('to much');
  FDeleted:=FFirstBPM;

  {//if FDeleted<>nil then begin
  ANextPtr^:=FDeleted;
  //FFirstBPM:=FDeleted;
  if FFirst=nil
    then FFirst:=FDeleted
    else FLast^.Next:=FDeleted; }
  //{ANextPtr2}FLast^.Next:=FDeleted;
  //FDeleted:=FDeleted^.Next;
  {end else begin
    ANextPtr^:=@FData[FCount];
    ANextPtr2^:=@FData[FCount];
    Inc(FCount);
  end;}
end;

const
  MaxBPMDiff = 0.08;
  MinBPMDiff = 1-MaxBPMDiff;

function TUltraSortList.FindBPM(const AConTime: MVInt): MVFloat;
var
  ABPMFac,ABPMDiff: Real;

  {procedure ToleranceCheck;
  var
    ATolerance: MVFloat;
  begin
    if (FBPM>10.0) and (FBPM<1000.0) then begin
      if FWasBeat then begin
        ATolerance:=abs(FExpectedBPM-FBPM);
        if ATolerance<FBPMTolerance2 then begin
          if FBPMContinuity>200 then FBPM:=FExpectedBPM;
          if ATolerance<=0.01 then begin
            Inc(FBPMContinuity,AConTime);
            FBPMAntiContinuity:=0;
          end;
        end else begin
          ABPMDiff:=Frac(ln(FExpectedBPM/FBPM)/ln2);
          if (ABPMDiff>MinBPMDiff) or (ABPMDiff<MaxBPMDiff) then begin
            if FBPMContinuity>200 then begin
              FBPM:=FExpectedBPM;
              Inc(FBPMSemiContinuity,AConTime);
              FBPMAntiContinuity:=0;
            end else begin
               FBPMContinuity:=0;
               FBPMAntiContinuity:=0;
               FBPMSemiContinuity:=0;
               FExpectedBPM:=FBPM;
            end;
          end else begin
            Inc(FBPMAntiContinuity,AConTime);
            if (FBPMAntiContinuity>20000) or (FBPMAntiContinuity>(FBPMContinuity+FBPMSemiContinuity)) then begin
               FBPMContinuity:=0;
               FBPMAntiContinuity:=0;
               FBPMSemiContinuity:=0;
               FExpectedBPM:=FBPM;
            end else if FBPMContinuity>500 then FBPM:=FExpectedBPM;
          end;
        end;
      end else begin
        FExpectedBPM:=FBPM;
      end;
      FIsBeat:=true;
      FWasBeat:=true;
    end else begin
      Inc(FBPMAntiContinuity,AConTime);
      if (FBPMAntiContinuity>20000) or (FBPMAntiContinuity>(FBPMContinuity+FBPMSemiContinuity)) then begin
        FBPMContinuity:=0;
        FBPMAntiContinuity:=0;
        FBPMSemiContinuity:=0;
        FWasBeat:=false;
      end else if FBPMContinuity>500 then begin
        FBPM:=FExpectedBPM;
        FIsBeat:=true;
      end else FIsBeat:=false;
    end;
  end; }

var
  AMaxTime       : Integer;
  ANext,APtr,AMax: PUltraSortListItem;
  ANextPtr       : ^PUltraSortListItem;
label
  DoNotAdd;
begin
  if FFirst=nil{FCount<=0} then begin
    Result:=0.0;
    exit;
  end;
  FFirstBPM:=FFirst{@FData[0]};
  with PUltraSortListItem(FFirstBPM)^do begin
    NextBPM:=nil;
    StolenTime:=0;
    StolenCount:=0;
    StolenSum:=0;
  end;
  AMax:=FFirstBPM;
  AMaxTime:=PUltraSortListItem(FFirstBPM)^.BPMTime;
  //with FFirst^ do BPM:=BPMSum/BPMCount;
  //for I:=1 to FCount-1 do begin
  APtr:=PUltraSortListItem(FFirst)^.Next;
  while APtr<>nil do begin
    with APtr^do begin
      NextBPM:=nil;
      StolenTime:=0;
      StolenCount:=0;
      StolenSum:=0;
    end;
    ANext:=FFirstBPM;
    ANextPtr:=@FFirstBPM;
    //APtr:=APtr^.Next{@FData[I]};
    //with APtr^ do BPM:=BPMSum/BPMCount;
    while ANext<>nil do begin
      ABPMFac:=APtr^.FirstBPM/ANext^.FirstBPM;
      ABPMDiff:=Frac(ln(ABPMFac)/ln2);
      if (ABPMDiff>MinBPMDiff) or (ABPMDiff<MaxBPMDiff) then begin
        if ANext^.BPMTime>=APtr^.BPMTime then begin
          ANext^.StolenSum+=APtr^.BPMSum;
          ANext^.StolenCount+=(APtr^.BPMCount*{Round}(ABPMFac));
          ANext^.StolenTime+=APtr^.BPMTime;
          with ANext^ do if BPMTime+StolenTime>AMaxTime then begin
            AMaxTime:=BPMTime+StolenTime;
            AMax:=ANext;
          end;
        end else begin
          //ANextPtr
          APtr^.StolenSum:=ANext^.BPMSum;
          APtr^.StolenCount:=(ANext^.BPMCount/{Round}(ABPMFac));
          APtr^.StolenTime:=ANext^.BPMTime+ANext^.StolenTime;
          with APtr^ do if BPMTime+StolenTime>AMaxTime then begin
            AMaxTime:=BPMTime+StolenTime;
            AMax:=APtr;
          end;
          ANextPtr^:=APtr;
        end;
        goto DoNotAdd;
      end else begin
        ANextPtr:=@ANext^.NextBPM;
        ANext:=ANextPtr^;
      end;
    end;
    ANextPtr^:=APtr;
    DoNotAdd:
    APtr:=APtr^.Next;
  end;

  with AMax^ do FBPM:=(BPMSum+StolenSum)/(BPMCount+StolenCount);

  //ToleranceCheck;
  Result:=FBPM;
end;

end.

