unit csl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Enumerators;

type
  TList = class
  protected
    function GetCount: Integer; virtual; abstract;
  public
    function GetEnumerator: TEnumerator; virtual; abstract;
    //empties the list and destroys the contents
    procedure Clean; virtual; abstract;
    //empties the list, but leaves the contents usable
    procedure Clear; virtual; abstract;
    //adds an element at the end of the list
    procedure Add(AObject: TObject); virtual; abstract;
    //adds an element at the end of the list and returns an enumerator staying
    //at its position
    function AddAt(AObject: TObject): TEnumerator; virtual; abstract;

    property Count: Integer read GetCount;
    property Enumerator: TEnumerator read GetEnumerator;
  end;

  TMap  = class
  protected
    function GetCount: Integer; virtual; abstract;
    function GetItem(AKey: TObject): TObject; virtual; abstract;
    //not implemented yet because it's not needed anywhere
    //function GetKeyEnumerator: TEnumerator; virtual; abstract;
  public
    function GetEnumerator: TEnumerator; virtual; abstract;
    procedure Add(AKey: TObject; AValue: TObject); virtual; abstract;
    function Contains(AKey: TObject): Boolean; virtual; abstract;
    //empties the map and destroys the contents
    procedure Clean; virtual; abstract;
    //empties the map, but leaves the contents usable
    procedure Clear; virtual; abstract;
    //removes an item from the map
    function Remove(AKey: TObject): TObject; virtual; abstract;
    //removes an item from the map and destroys it
    function Delete(AKey: TObject): Boolean; virtual; abstract;

    property Count: Integer read GetCount;
    property Enumerator: TEnumerator read GetEnumerator;
    property Items[AKey: TObject]: TObject read GetItem; default;
    //property KeyEnumerator: TEnumerator read GetKeyEnumerator;
  end;

implementation

end.

