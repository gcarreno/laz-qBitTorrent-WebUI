{
  Torrent Files Container

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
unit qBTorrentsFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, fpjson, jsonparser, jsonscanner;

type

{ TqBTorrentsFile }
  TqBTorrentsFile = class(TObject)
  private
    FName: String;
    FSize: Integer;
    FProgress: Double;
    FPriority: Integer;
    FIsSeed: Boolean;

    procedure DoLoadFromJSON(const aJSON: String);
    procedure DoLoadFromJSONData(const aJSONData: TJSONData);
    procedure DoLoadFromJSONObj(const aJSONObj: TJSONObject);
    procedure DoLoadFromStream(const aStream: TStream);
  protected
  public
    constructor Create;
    constructor Create(const aJSON: String);
    constructor Create(const aJSONData: TJSONData);
    constructor Create(const aJSONObj: TJSONObject);
    constructor Create(const aStream: TStream);

    destructor Destroy; override;

    procedure Load(const aJSON: String);
    procedure Load(const aJSONData: TJSONData);
    procedure Load(const aJSONObj: TJSONObject);
    procedure Load(const aStream: TStream);

    property Name: String
      read FName
      write FName;
    property Size: Integer
      read FSize
      write FSize;
    property Progress: Double
      read FProgress
      write FProgress;
    property Priority: Integer
      read FPriority
      write FPriority;
    property IsSeed: Boolean
      read FIsSeed
      write FIsSeed;
  end;

{ TqBTorrentsFiles }
  TqBTorrentsFilesEnumerator = class; // Forward
  TqBTorrentsFiles = class(TFPObjectList)
  private
    function GetByIndex(Index: Integer): TqBTorrentsFile;
    procedure SetByIndex(Index: Integer; AValue: TqBTorrentsFile);

    function GetByFileName(FileName: String): TqBTorrentsFile;
    procedure SetByFileName(FileName: String; AValue: TqBTorrentsFile);
  protected
  public
    // Enumerator
    function GetEnumerator: TqBTorrentsFilesEnumerator;

    procedure LoadFiles(const aJSON: String);
    procedure LoadFiles(const aJSONData: TJSONData);
    procedure LoadFiles(const aJSONArray: TJSONArray);
    procedure LoadFiles(const aStream: TStream);

    procedure UpdateFiles(const aJSON: String);
    procedure UpdateFiles(const aJSONData: TJSONData);
    procedure UpdateFiles(const aJSONArray: TJSONArray);
    procedure UpdateFiles(const aStream: TStream);

    procedure UpdateFile(const aFileName: String; const aJSON: String);
    procedure UpdateFile(const aFileName: String; const aJSONData: TJSONData);
    procedure UpdateFile(const aFileName: String; const aJSONObj: TJSONObject);
    procedure UpdateFile(const aFileName: String; const aStream: TStream);

    property Items[Index: Integer]: TqBTorrentsFile
      read GetByIndex
      write SetByIndex; default;
    property Files[FileName: String]: TqBTorrentsFile
      read GetByFileName
      write SetByFileName;
  end;

{ TqBTorrentsFilesEnumerator }
  TqBTorrentsFilesEnumerator = class(TObject)
  private
    FFiles: TqBTorrentsFiles;
    FPosition: Integer;
  public
    constructor Create(AFiles: TqBTorrentsFiles);
    function GetCurrent: TqBTorrentsFile;
    function MoveNext: Boolean;

    property Current: TqBTorrentsFile
      read GetCurrent;
  end;

// Helper functions
function PriorityToStr(aPriority: Integer): String;

implementation

const
  csPriorityZero  = 'Do not download';
  csPriorityOne   = 'Normal priority';
  csPriorityTwo   = 'High priority';
  csPrioritySeven = 'Maximal priority';
  csPriorityUnknonwn = 'Unknown';

function PriorityToStr(aPriority: Integer): String;
begin
  Result := csPriorityUnknonwn;
  case aPriority of
    0: Result := csPriorityZero;
    1: Result := csPriorityOne;
    2: Result := csPriorityTwo;
    7: Result := csPrioritySeven;
  end;
end;

{ TqBTorrentsFilesEnumerator }

constructor TqBTorrentsFilesEnumerator.Create(AFiles: TqBTorrentsFiles);
begin
  FFiles := AFiles;
  FPosition := -1;
end;

function TqBTorrentsFilesEnumerator.GetCurrent: TqBTorrentsFile;
begin
  Result := FFiles[FPosition];
end;

function TqBTorrentsFilesEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FFiles.Count;
end;

{ TqBTorrentsFile }

procedure TqBTorrentsFile.DoLoadFromJSON(const aJSON: String);
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
      if jData.JSONType = jtObject then
      begin
        DoLoadFromJSONObj(jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsFile.DoLoadFromJSONData(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtObject then
  begin
    DoLoadFromJSONObj(aJSONData as TJSONObject);
  end;
end;

procedure TqBTorrentsFile.DoLoadFromJSONObj(const aJSONObj: TJSONObject);
begin
  FName := aJSONObj.Get('name', FName);
  FSize := aJSONObj.Get('size', FSize);
  FProgress := aJSONObj.Get('progress', FProgress);
  FPriority := aJSONObj.Get('priority', FPriority);
  FIsSeed := aJSONObj.Get('is_seed', FIsSeed);
end;

procedure TqBTorrentsFile.DoLoadFromStream(const aStream: TStream);
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
      if jData.JSONType = jtObject then
      begin
        DoLoadFromJSONObj(jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

constructor TqBTorrentsFile.Create;
begin
  FName := '';
  FSize := -1;
  FProgress := 0.0;
  FPriority := -1;
  FIsSeed := False;
end;

constructor TqBTorrentsFile.Create(const aJSON: String);
begin
  Create;
  DoLoadFromJSON(aJSON);
end;

constructor TqBTorrentsFile.Create(const aJSONData: TJSONData);
begin
  Create;
  DoLoadFromJSONData(aJSONData);
end;

constructor TqBTorrentsFile.Create(const aJSONObj: TJSONObject);
begin
  Create;
  DoLoadFromJSONObj(aJSONObj);
end;

constructor TqBTorrentsFile.Create(const aStream: TStream);
begin
  Create;
  DoLoadFromStream(aStream);
end;

destructor TqBTorrentsFile.Destroy;
begin
  inherited Destroy;
end;

procedure TqBTorrentsFile.Load(const aJSON: String);
begin
  DoLoadFromJSON(aJSON);
end;

procedure TqBTorrentsFile.Load(const aJSONData: TJSONData);
begin
  DoLoadFromJSONData(aJSONData);
end;

procedure TqBTorrentsFile.Load(const aJSONObj: TJSONObject);
begin
  DoLoadFromJSONObj(aJSONObj);
end;

procedure TqBTorrentsFile.Load(const aStream: TStream);
begin
  DoLoadFromStream(aStream);
end;

{ TqBTorrentsFiles }

function TqBTorrentsFiles.GetByIndex(Index: Integer): TqBTorrentsFile;
begin
  Result := inherited Items[Index] as TqBTorrentsFile;
end;

procedure TqBTorrentsFiles.SetByIndex(Index: Integer; AValue: TqBTorrentsFile);
begin
  inherited Items[Index] := AValue;
end;

function TqBTorrentsFiles.GetByFileName(FileName: String): TqBTorrentsFile;
var
  oFile: TqBTorrentsFile;
begin
  Result := nil;
  for oFile in Self do
  begin
    if oFile.Name = FileName then
    begin
      Result := oFile;
      break;
    end;
  end;
end;

procedure TqBTorrentsFiles.SetByFileName(FileName: String; AValue: TqBTorrentsFile);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Name = FileName then
    begin
      inherited Items[index] := aValue;
      break;
    end;
  end;
end;

function TqBTorrentsFiles.GetEnumerator: TqBTorrentsFilesEnumerator;
begin
  Result := TqBTorrentsFilesEnumerator.Create(Self);
end;

procedure TqBTorrentsFiles.LoadFiles(const aJSON: String);
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
    if jData.JSONType = jtArray then
    begin
      LoadFiles(jData as TJSONArray);
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsFiles.LoadFiles(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtArray then
  begin
    LoadFiles(aJSONData as TJSONArray);
  end;
end;

procedure TqBTorrentsFiles.LoadFiles(const aJSONArray: TJSONArray);
var
  index: Integer;
begin
  Clear;
  for index := 0 to aJSONArray.Count - 1 do
  begin
    if aJSONArray[index].JSONType = jtObject then
      Add(TqBTorrentsFile.Create(aJSONArray[index] as TJSONObject));
  end;
end;

procedure TqBTorrentsFiles.LoadFiles(const aStream: TStream);
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
    if jData.JSONType = jtArray then
    begin
      LoadFiles(jData as TJSONArray);
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsFiles.UpdateFiles(const aJSON: String);
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
        UpdateFiles(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsFiles.UpdateFiles(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtArray then
  begin
    UpdateFiles(aJSONData as TJSONArray);
  end;
end;

procedure TqBTorrentsFiles.UpdateFiles(const aJSONArray: TJSONArray);
var
  index, index1: Integer;
  oTracker: TqBTorrentsFile;
  jData: TJSONData;
begin
  for index := 0 to aJSONArray.Count - 1 do
  begin
    jData := aJSONArray[index];
    if jData.JSONType = jtObject then
    begin
      for index1 := 0 to Count - 1 do
      begin
        oTracker := Items[index1];
        if oTracker.Name = TJSONObject(jData).Get('name', '') then
        begin
          break;
        end
        else
        begin
          oTracker:= nil;
        end;
      end;
      if Assigned(oTracker) then
      begin
        oTracker.Load(jData as TJSONObject);
      end
      else
      begin
        Add(TqBTorrentsFile.Create(jData as TJSONObject));
      end;
    end;
  end;
end;

procedure TqBTorrentsFiles.UpdateFiles(const aStream: TStream);
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
        UpdateFiles(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsFiles.UpdateFile(const aFileName: String; const aJSON: String);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Name = aFileName then
    begin
      Items[index].Load(aJSON);
      break;
    end;
  end;
end;

procedure TqBTorrentsFiles.UpdateFile(const aFileName: String; const aJSONData: TJSONData);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Name = aFileName then
    begin
      Items[index].Load(aJSONData);
      break;
    end;
  end;
end;

procedure TqBTorrentsFiles.UpdateFile(const aFileName: String; const aJSONObj: TJSONObject);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Name = aFileName then
    begin
      Items[index].Load(aJSONObj);
      break;
    end;
  end;
end;

procedure TqBTorrentsFiles.UpdateFile(const aFileName: String; const aStream: TStream);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Name = aFileName then
    begin
      Items[index].Load(aStream);
      break;
    end;
  end;
end;

end.
