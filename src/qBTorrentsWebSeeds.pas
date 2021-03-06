{
  Torrent Web Seeds Container

  Copyright (c) 2017 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit qBTorrentsWebSeeds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, qBCommon;

type

{ TqBTorrentsWebSeeds }
  TqBTorrentsWebSeeds = class(TObject)
  private
    FWebSeeds: TStringList;

    function GetByIndex(Index: integer): String;
    procedure SetByIndex(Index: integer; AValue: String);

    function GetCount: Integer;

    procedure DoLoadFromJSON(const aJSON: String);
    procedure DoLoadFromJSONData(const aJSONData: TJSONData);
    procedure DoLoadFromJSONArray(const aJSONArray: TJSONArray);
    procedure DoLoadFromStream(const aStream: TStream);
  protected
  public
    constructor Create;
    constructor Create(const aJSON: String);
    constructor Create(const aJSONData: TJSONData);
    constructor Create(const aJSONArray: TJSONArray);
    constructor Create(const aStream: TStream);

    destructor Destroy; override;

    procedure Clear;

    procedure Load(const aJSON: String);
    procedure Load(const aJSONData: TJSONData);
    procedure Load(const aJSONArray: TJSONArray);
    procedure Load(const aStream: TStream);

    { TODO 99 -ogcarreno -cTqBTorrentsWebSeeds : Propbably needs Add and Remove ? }
    //procedure Add(const AUrl: String);
    //procedure Remove(const AUrl: String);

    property Items[Index: integer]: String
      read GetByIndex
      write SetByIndex; default;
    property Count: Integer
      read GetCount;
  end;

implementation

{ TqBTorrentsWebSeeds }

function TqBTorrentsWebSeeds.GetByIndex(Index: integer): String;
begin
  Result := FWebSeeds[Index];
end;

procedure TqBTorrentsWebSeeds.SetByIndex(Index: integer; AValue: String);
begin
  FWebSeeds[Index] := AValue;
end;

function TqBTorrentsWebSeeds.GetCount: Integer;
begin
  Result := FWebSeeds.Count;
end;

procedure TqBTorrentsWebSeeds.DoLoadFromJSON(const aJSON: String);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aJSON);
  try
    if jData.JSONType = jtArray then
    begin
      DoLoadFromJSONArray(jData as TJSONArray);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsWebSeeds.Load(const aJSON: String);
begin
  DoLoadFromJSON(aJSON);
end;

procedure TqBTorrentsWebSeeds.DoLoadFromJSONData(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtArray then
  begin
    DoLoadFromJSONArray(aJSONData as TJSONArray);
  end;
end;

procedure TqBTorrentsWebSeeds.Load(const aJSONData: TJSONData);
begin
  DoLoadFromJSONData(aJSONData);
end;

procedure TqBTorrentsWebSeeds.DoLoadFromJSONArray(const aJSONArray: TJSONArray);
const
  csWebSeedsUrl = 'url';
var
{$IF FPC_FULLVERSION >= 20604}
  jDataEnum: TJSONEnum;
{$ELSE}
  index: Integer;
  jData: TJSONData;
{$ENDIF}
  jWebSeeds: TJSONObject;
  url: String;
begin
  FWebSeeds.Clear;
{$IF FPC_FULLVERSION >= 20604}
  for jDataEnum in aJSONArray do
  begin
    if jDataEnum.Value.JSONType = jtObject then
    begin
      jWebSeeds := jDataEnum.Value as TJSONObject;
      url := jWebSeeds.Get(csWebSeedsUrl, '');
      if Length(url) > 0 then
      begin
        FWebSeeds.Add(url);
      end;
    end;
  end;
{$ELSE}
  for index := 0 to aJSONArray.Count - 1 do
  begin
    jData := aJSONArray[index];
    if jData.JSONType = jtObject then
    begin
      jWebSeeds := jData as TJSONObject;
      url := jWebSeeds.Get(csWebSeedsUrl, '');
      if Length(url) > 0 then
      begin
        FWebSeeds.Add(url);
      end;
    end;
  end;
{$ENDIF}
end;

procedure TqBTorrentsWebSeeds.Load(const aJSONArray: TJSONArray);
begin
  DoLoadFromJSONArray(aJSONArray);
end;

procedure TqBTorrentsWebSeeds.DoLoadFromStream(const aStream: TStream);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aStream);
  try
    if jData.JSONType = jtArray then
    begin
      DoLoadFromJSONArray(jData as TJSONArray);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsWebSeeds.Load(const aStream: TStream);
begin
  DoLoadFromStream(aStream);
end;

constructor TqBTorrentsWebSeeds.Create;
begin
  FWebSeeds := TStringList.Create;
end;

constructor TqBTorrentsWebSeeds.Create(const aJSON: String);
begin
  Create;
  DoLoadFromJSON(aJSON);
end;

constructor TqBTorrentsWebSeeds.Create(const aJSONData: TJSONData);
begin
  Create;
  DoLoadFromJSONData(aJSONData);
end;

constructor TqBTorrentsWebSeeds.Create(const aJSONArray: TJSONArray);
begin
  Create;
  DoLoadFromJSONArray(aJSONArray);
end;

constructor TqBTorrentsWebSeeds.Create(const aStream: TStream);
begin
  Create;
  DoLoadFromStream(aStream);
end;

destructor TqBTorrentsWebSeeds.Destroy;
begin
  FWebSeeds.Free;
  inherited Destroy;
end;

procedure TqBTorrentsWebSeeds.Clear;
begin
  FWebSeeds.Clear;
end;

end.
