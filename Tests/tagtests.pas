unit TagTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TagUnit, Dialogs;

type
  TTagTest= class (TTestCase)
  private
    FTagGroup    : TTagGroup;
    FTaggedObject: TObject;
    function TagsToStr(ATags: TTags): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetTag;
    procedure TestGetTagString;
    procedure TestCreateTagSet;
    procedure TestTagSetUnusedTag;
    procedure TestCreateTags;
    procedure TestCreateTagsNilObject;
    procedure TestAddTag;
    procedure TestRemoveTag;
    procedure TestHasTagFalse;
    procedure TestHasTagTrue;
    procedure TestAddTwoTagsBug;
  end;

implementation

procedure TTagTest.TestGetTag;
var
  ATag: TTag;
begin
  ATag:=FTagGroup['blubb'];
end;

procedure TTagTest.TestGetTagString;
var
  ATag: TTag;
begin
  ATag:=FTagGroup['blubb'];
  AssertEquals('blubb', FTagGroup.Strings[ATag]);
end;

procedure TTagTest.TestCreateTagSet;
var
  ATagSet: TTagSet;
begin
  ATagSet:=TTagSet.Create;
  AssertEquals(0, ATagSet.TagCount);
  ATagSet.Destroy;
end;

procedure TTagTest.TestTagSetUnusedTag;
var
  ATagSet: TTagSet;
  ATag   : TTag;
begin
  ATag:=FTagGroup['1337'];
  ATagSet:=TTagSet.Create;
  AssertEquals(0, ATagSet.TagCount);
  AssertEquals(0, ATagSet.TaggedObjectCount[ATag]);
  ATagSet.Destroy;
end;

procedure TTagTest.TestCreateTags;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, FTaggedObject);
  AssertEquals(0, ATags.TagCount);
  AssertEquals(0, ATags.Order);
  AssertTrue('wrong object tagged', FTaggedObject=ATags.&Object);
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.TestCreateTagsNilObject;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, nil, 42);
  AssertEquals(0, ATags.TagCount);
  AssertEquals(42, ATags.Order);
  AssertTrue('wrong object tagged', ATags.&Object=nil);
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.TestAddTag;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, FTaggedObject);
  ATags.Add(FTagGroup['blubb']);
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.TestRemoveTag;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, FTaggedObject);
  ATags.Add(FTagGroup['blubb']);
  ATags.Remove(FTagGroup['blubb']);
  AssertFalse(ATags.Has(FTagGroup['blubb']));
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.TestHasTagFalse;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, FTaggedObject);
  AssertFalse(ATags.Has(FTagGroup['blubb']));
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.TestHasTagTrue;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, FTaggedObject);
  ATags.Add(FTagGroup['42']);
  ATags.Add(FTagGroup['blubb']);
  ATags.Remove(FTagGroup['42']);
  AssertTrue(ATags.Has(FTagGroup['blubb']));
  AssertFalse(ATags.Has(FTagGroup['42']));
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.TestAddTwoTagsBug;
var
  ATagSet: TTagSet;
  ATags  : TTags;
begin
  ATagSet:=TTagSet.Create;
  ATags:=TTags.Create(ATagSet, FTaggedObject);
  ATags.Add(FTagGroup['system']);
  ATags.Add(FTagGroup['hidden']);
  AssertTrue('Tag system not found. All tags: ' + TagsToStr(ATags), ATags.Has(FTagGroup['system']));
  AssertTrue('Tag hidden not found. All tags: ' + TagsToStr(ATags), ATags.Has(FTagGroup['hidden']));
  ATags.Destroy;
  ATagSet.Destroy;
end;

procedure TTagTest.SetUp;
begin
  FTagGroup:=TTagGroup.Create;
  FTaggedObject:=TObject.Create;
end;

procedure TTagTest.TearDown;
begin
  FTaggedObject.Destroy;
  FTagGroup.Destroy;
end;

function TTagTest.TagsToStr(ATags: TTags): string;
var
  AArray: TTagArray;
  I     : Integer;
begin
  AArray:=ATags.ToArray;
  if Length(AArray)=0 then begin
    Result:='()';
    exit;
  end else Result:='(' + FTagGroup.Strings[AArray[0]];
  for I:=1 to Length(AArray)-1 do begin
    Result+=', ' + FTagGroup.Strings[AArray[I]];
  end;
  Result+=')';
end;

initialization
  RegisterTest(TTagTest);
end.

