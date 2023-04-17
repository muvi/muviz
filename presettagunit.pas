unit PresetTagUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  TagEditor, VisType2, TagType, MStrings;

type

  { TPresetTagEditForm }

  TPresetTagEditForm = class(TForm)
    TagEditor: TTagEditor;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure TagEditorRemoveConfirm(Sender: TObject; TagIndex: integer;
      const TagCaption: string; var CanRemove: boolean);
    procedure TagEditorTagAdded(Sender: TObject);
    procedure TagEditorTagRemoved(Sender: TObject);
  public
    procedure EditTags(X, Y: Integer; APreset: IPVisualisation);
  private
    FPreset: IPVisualisation;
    procedure DoRealign; inline;
  end;

var
  PresetTagEditForm: TPresetTagEditForm = nil;

implementation

procedure TPresetTagEditForm.TagEditorTagAdded(Sender: TObject);
begin
  DoRealign;
  FPreset.AddTag(TagEditor.Tags[TagEditor.Tags.Count-1]);
end;

procedure TPresetTagEditForm.TagEditorTagRemoved(Sender: TObject);
begin
  DoRealign;
end;

procedure TPresetTagEditForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TPresetTagEditForm.TagEditorRemoveConfirm(Sender: TObject;
  TagIndex: integer; const TagCaption: string; var CanRemove: boolean);
begin
  FPreset.RemoveTag(TagCaption);
  CanRemove:=true;
end;

procedure TPresetTagEditForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
  FPreset:=nil;
end;

procedure TPresetTagEditForm.DoRealign; inline;
begin
  Height:=TagEditor.Height;
end;

procedure TPresetTagEditForm.EditTags(X, Y: Integer; APreset: IPVisualisation);
var
  ATags: ITags;
  I    : Integer;
begin
  FPreset:=APreset;
  ATags:=FPreset.Tags;
  TagEditor.Tags.Clear;
  for I:=0 to ATags.Count-1
    do TagEditor.Tags.Add(ATags[I]);

  Left:=X;
  Top:=Y;
  DoRealign;
  Application.AddOnDeactivateHandler(@FormDeactivate);
  Show;
end;

initialization
  {$I PresetTagUnit.lrs}

end.

