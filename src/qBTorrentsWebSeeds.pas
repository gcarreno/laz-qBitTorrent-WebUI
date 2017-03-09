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
  Classes, SysUtils, fpjson, jsonparser, jsonscanner;

type

{ TqBTorrentsWebSeeds }
  TqBTorrentsWebSeeds = class(TObject)
  private
    FWebSeeds: TStringList;

    function GetByIndex(Index: integer): String;
    function GetCount: Integer;
    procedure SetByIndex(Index: integer; AValue: String);

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

    // TODO: Propbably needs Add and Remove ?

    property Items[Index: integer]: String
      read GetByIndex
      write SetByIndex; default;
    property Count: Integer
      read GetCount;
  end;

implementation

{ TqBTorrentsWebSeeds }

function TqBTorrentsWebSeeds.GetCount: Integer;
begin
  Result := FWebSeeds.Count;
end;

function TqBTorrentsWebSeeds.GetByIndex(Index: integer): String;
begin
  Result := FWebSeeds[Index];
end;

procedure TqBTorrentsWebSeeds.SetByIndex(Index: integer; AValue: String);
begin
  FWebSeeds[Index] := AValue;
end;

procedure TqBTorrentsWebSeeds.DoLoadFromJSON(const aJSON: String);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
{$IF FPC_FULLVERSION >= 30002}
  jParser := TJSONParser.Create(aJSON, [joUTF8, joIgnoreTrailingComma]);
{$ELSE}
  jParser := TJSONParser.Create(aJSON, True);
{$ENDIF}
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtArray then
      begin
        DoLoadFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
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
  index: Integer;
  jData: TJSONData;
  jWebSeeds: TJSONObject;
  url: String;
begin
  FWebSeeds.Clear;
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
end;

procedure TqBTorrentsWebSeeds.Load(const aJSONArray: TJSONArray);
begin
  DoLoadFromJSONArray(aJSONArray);
end;

procedure TqBTorrentsWebSeeds.DoLoadFromStream(const aStream: TStream);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
{$IF FPC_FULLVERSION >= 30002}
  jParser := TJSONParser.Create(aStream, [joUTF8, joIgnoreTrailingComma]);
{$ELSE}
  jParser := TJSONParser.Create(aStream, True);
{$ENDIF}
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtArray then
      begin
        DoLoadFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
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
