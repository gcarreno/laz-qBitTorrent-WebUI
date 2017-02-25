{
  Torrent file descriptor and container

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
unit qBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, DateUtils, fpjson, jsonparser, jsonscanner;

type
{ TqBTorrentStates }
  TqBTorrentState = (
    tsError,
    tsPausedUp,
    tsPausedDl,
    tsQueuedUp,
    tsQueuedDl,
    tsUploading,
    tsStalledUp,
    tsStalledDl,
    tsCheckingUp,
    tsCheckingDl,
    tsDownloading,
    tsMetaDl,
    tsUnknown
  );

{ TqBTorrentsFilter }
  TqBTorrentsFilter = class(TObject)
  private
    FFilters: TStringList;

    function GetFilters: String;
    function GetCount: Integer;
    procedure Add(const aName:String; const aValue: String);
    procedure Add(const aName:String; const aValue: Boolean);
    procedure Add(const aName:String; const aValue: Integer);
    procedure Remove(const aName: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function Clear: TqBTorrentsFilter;
    function withFilter(const aFilter: String): TqBTorrentsFilter;
    function withOutFilter: TqBTorrentsFilter;
    function withCategory(const aCategory: String): TqBTorrentsFilter;
    function withOutCategory: TqBTorrentsFilter;
    function withSort(const aSort: String): TqBTorrentsFilter;
    function withOutSort: TqBTorrentsFilter;
    function withReverse(const aReverse: Boolean): TqBTorrentsFilter;
    function withOutReverse: TqBTorrentsFilter;
    function withLimit(const aLimit: Integer): TqBTorrentsFilter;
    function withOutLimit: TqBTorrentsFilter;
    function withOffset(const aOffset: Integer): TqBTorrentsFilter;
    function withOutOffset: TqBTorrentsFilter;

    property Filters: String
      read GetFilters;
    property Count: Integer
      read GetCount;
  end;

{ TqBTorrent }
  TqBTorrent = class(TObject)
  private
    FHash: String;
    FName: String;
    FSize: Int64;
    FProgress: Double;
    FDlSpeed: Integer;
    FUpSpeed: Integer;
    FPriority: Integer; // -1 if no priority of is seeding
    FNumSeeds: Integer;
    FNumComplete: Integer;
    FNumLeechs: Integer;
    FNumIncomplete: Integer;
    FRatio: Double;
    FEta: Integer; //Seconds
    FState: TqBTorrentState;
    FSeqDl: Boolean;
    FFirstLastPiecePrioritized: Boolean;
    FCategory: String;
    FSuperSeeding: Boolean;
    FForceStart: Boolean;
    FSavePath: String;
    FAddedOn: TDateTime;
    FCompletionOn: TDateTime;

    procedure DoLoadFromJSON(const aJSON: String);
    procedure DoLoadFromJSONObj(const aJSONObj: TJSONObject);
    procedure DoLoadFromStream(const aStream: TStream);
  protected
  public
    constructor Create;
    constructor Create(const aJSON: String);
    constructor Create(const aJSONObj: TJSONObject);
    constructor Create(const aStream: TStream);

    destructor Destroy; override;

    //procedure LoadFromJSON(const aJSON: String);
    //procedure LoadFromJSONObj(const aJSONObj: TJSONObject);
    //procedure LoadFromStream(const aStream: TStream);

    property Hash: String
      read FHash
      write FHash;
    property Name: String
      read FName
      write FName;
    property Size: Int64
      read FSize
      write FSize;
    property Progress: Double
      read FProgress
      write FProgress;
    property DlSpeed: Integer
      read FDlSpeed
      write FDlSpeed;
    property UpSpeed: Integer
      read FUpSpeed
      write FUpSpeed;
    property Priority: Integer
      read FPriority
      write FPriority;
    property NumSeeds: Integer
      read FNumSeeds
      write FNumSeeds;
    property NumComplete: Integer
      read FNumComplete
      write FNumComplete;
    property NumLeechs: Integer
      read FNumLeechs
      write FNumLeechs;
    property NumIncomplete: Integer
      read FNumIncomplete
      write FNumIncomplete;
    property Ratio: Double
      read FRatio
      write FRatio;
    property Eta: Integer // Seconds
      read FEta
      write FEta;
    property State: TqBTorrentState
      read FState
      write FState;
    property SeqDl: Boolean
      read FSeqDl
      write FSeqDl;
    property FirstLastPiecePrioritized: Boolean
      read FFirstLastPiecePrioritized
      write FFirstLastPiecePrioritized;
    property Category: String
      read FCategory
      write FCategory;
    property SuperSeeding: Boolean
      read FSuperSeeding
      write FSuperSeeding;
    property ForceStart: Boolean
      read FForceStart
      write FForceStart;
    property SavePath: String
      read FSavePath
      write FSavePath;
    property AddedOn: TDateTime
      read FAddedOn
      write FAddedOn;
    property CompletionOn: TDateTime
      read FCompletionOn
      write FCompletionOn;
  end;

{ TqBTorrents }
  TqBTorrents = class(TFPObjectList)
  private
    function GetItem(Index: Integer): TqBTorrent;
    procedure SetItem(Index: Integer; AObject: TqBTorrent);
  protected
  public
    procedure LoadFromJSON(const aJSON: String);
    procedure LoadFromJSONArray(const aJSONArray: TJSONArray);
    procedure LoadFromStream(const aStream: TStream);
    procedure UpdateFromJSON(const aJSON: String);
    procedure UpdateFromJSONArray(const aJSONArray: TJSONArray);
    procedure UpdateFromStream(const aStream: TStream);

    property Items[Index: Integer]: TqBTorrent
      read GetItem
      write SetItem; default;
  end;

// Helper functions
function StrToqBState(const aState: String): TqBTorrentState;
function qBStateToStr(aState: TqBTorrentState): String;

implementation

const
  cNameFilter = 'filter';
  cNameCategory = 'category';
  cNameSort = 'sort';
  cNameReverse = 'reverse';
  cNameLimit = 'limit';
  cNameOffset = 'offset';

function StrToqBState(const aState: String): TqBTorrentState;
begin
  Result := tsUnknown;
  if aState = 'error' then
  begin
    Result := tsError;
    exit;
  end;
  if aState = 'pausedUP' then
  begin
    Result := tsPausedUp;
    exit;
  end;
  if aState = 'pausedDL' then
  begin
    Result := tsPausedDl;
    exit;
  end;
  if aState = 'queuedUP' then
  begin
    Result := tsQueuedUp;
    exit;
  end;
  if aState = 'queuedDL' then
  begin
    Result := tsQueuedDl;
    exit;
  end;
  if aState = 'uploading' then
  begin
    Result := tsUploading;
    exit;
  end;
  if aState = 'stalledUP' then
  begin
    Result := tsStalledUp;
    exit;
  end;
  if aState = 'stalledDL' then
  begin
    Result := tsStalledDl;
    exit;
  end;
  if aState = 'checkingUP' then
  begin
    Result := tsCheckingUp;
    exit;
  end;
  if aState = 'checkingDL' then
  begin
    Result := tsCheckingDl;
    exit;
  end;
  if aState = 'downloading' then
  begin
    Result := tsDownloading;
    exit;
  end;
  if aState = 'metaDL' then
  begin
    Result := tsMetaDl;
    exit;
  end;
end;

function qBStateToStr(aState: TqBTorrentState): String;
begin
  Result := 'unknown';
  case aState of
    tsError:       Result := 'error';
    tsPausedUp:    Result := 'pausedUP';
    tsPausedDl:    Result := 'pausedDL';
    tsQueuedUp:    Result := 'queuedUP';
    tsQueuedDl:    Result := 'queuedDL';
    tsUploading:   Result := 'uploading';
    tsStalledUp:   Result := 'stalledUP';
    tsStalledDl:   Result := 'stalledDL';
    tsCheckingUp:  Result := 'checkingUP';
    tsCheckingDl:  Result := 'checkingDL';
    tsDownloading: Result := 'downloading';
    tsMetaDl:      Result := 'metaDL';
  end;
end;

{ TqBTorrentsFilter }

function TqBTorrentsFilter.GetFilters: String;
begin
  Result := FFilters.DelimitedText;
end;

function TqBTorrentsFilter.GetCount: Integer;
begin
  Result := FFilters.Count;
end;

procedure TqBTorrentsFilter.Add(const aName:String; const aValue: String);
var
  sFilter: String;
  i, index: integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    FFilters.ValueFromIndex[index] := aValue;
  end
  else
  begin
    sFilter := aName + '=' + aValue;
    FFilters.Add(sFilter);
  end;
end;

procedure TqBTorrentsFilter.Add(const aName: String; const aValue: Boolean);
var
  sFilter: String;
  i, index: integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    if aValue then
    begin
      FFilters.ValueFromIndex[index] := 'true';
    end
    else
    begin
      FFilters.ValueFromIndex[index] := 'false';
    end;
  end
  else
  begin
    if aValue then
    begin
      sFilter := aName + '=true';
    end
    else
    begin
      sFilter := aName + '=false';
    end;
    FFilters.Add(sFilter);
  end;
end;

procedure TqBTorrentsFilter.Add(const aName: String; const aValue: Integer);
var
  sFilter: String;
  i, index: integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    FFilters.ValueFromIndex[index] := IntToStr(aValue);
  end
  else
  begin
    sFilter := aName + '=' + IntToStr(aValue);
    FFilters.Add(sFilter);
  end;
end;

procedure TqBTorrentsFilter.Remove(const aName: String);
var
  i, index: Integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    FFilters.Delete(index);
  end;
end;

constructor TqBTorrentsFilter.Create;
begin
  FFilters := TStringList.Create;
  FFilters.NameValueSeparator := '=';
  FFilters.AlwaysQuote := False;
  FFilters.Delimiter := '&';
end;

destructor TqBTorrentsFilter.Destroy;
begin
  FFilters.Free;
  inherited Destroy;
end;

function TqBTorrentsFilter.Clear: TqBTorrentsFilter;
begin
  FFilters.Clear;
  Result := Self;
end;

function TqBTorrentsFilter.withFilter(const aFilter: String): TqBTorrentsFilter;
begin
  Add(cNameFilter, aFilter);
  Result := Self;
end;

function TqBTorrentsFilter.withOutFilter: TqBTorrentsFilter;
begin
  Remove(cNameFilter);
  Result := Self;
end;

function TqBTorrentsFilter.withCategory(const aCategory: String): TqBTorrentsFilter;
begin
  Add(cNameCategory, aCategory);
  Result := Self;
end;

function TqBTorrentsFilter.withOutCategory: TqBTorrentsFilter;
begin
  Remove(cNameCategory);
  Result := Self;
end;

function TqBTorrentsFilter.withSort(const aSort: String): TqBTorrentsFilter;
begin
  Add(cNameSort, aSort);
  Result := Self;
end;

function TqBTorrentsFilter.withOutSort: TqBTorrentsFilter;
begin
  Remove(cNameSort);
  Result := Self;
end;

function TqBTorrentsFilter.withReverse(const aReverse: Boolean): TqBTorrentsFilter;
begin
  Add(cNameReverse, aReverse);
  Result := Self;
end;

function TqBTorrentsFilter.withOutReverse: TqBTorrentsFilter;
begin
  Remove(cNameReverse);
  Result := Self;
end;

function TqBTorrentsFilter.withLimit(const aLimit: Integer): TqBTorrentsFilter;
begin
  Add(cNameLimit, aLimit);
  Result := Self;
end;

function TqBTorrentsFilter.withOutLimit: TqBTorrentsFilter;
begin
  Remove(cNameLimit);
  Result := Self;
end;

function TqBTorrentsFilter.withOffset(const aOffset: Integer): TqBTorrentsFilter;
begin
  Add(cNameOffset, aOffset);
  Result := Self;
end;

function TqBTorrentsFilter.withOutOffset: TqBTorrentsFilter;
begin
  Remove(cNameOffset);
  Result := Self;
end;

{ TqBTorrent }

procedure TqBTorrent.DoLoadFromJSON(const aJSON: String);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  jParser := TJSONParser.Create(aJSON, [joUTF8, joIgnoreTrailingComma]);
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

procedure TqBTorrent.DoLoadFromJSONObj(const aJSONObj: TJSONObject);
var
  iUnixTime: Integer;
begin
  FHash := aJSONObj.Get('hash', '');
  FName := aJSONObj.Get('name', '');
  FSize := aJSONObj.Get('size', 0);
  FProgress := aJSONObj.Get('progress', 0.0);
  FDlSpeed := aJSONObj.Get('dlspeed', 0);
  FUpSpeed := aJSONObj.Get('upspeed', 0);
  FPriority := aJSONObj.Get('priority', 0);
  FNumSeeds := aJSONObj.Get('num_seeds', 0);
  FNumComplete := aJSONObj.Get('num_complete', 0);
  FNumLeechs := aJSONObj.Get('num_leechs', 0);
  FNumIncomplete := aJSONObj.Get('num_incomplete', 0);
  FRatio := aJSONObj.Get('ratio', 0.0);
  FEta := aJSONObj.Get('eta', 0);
  FState := StrToqBState(aJSONObj.Get('state', 'unknown'));
  FSeqDl := aJSONObj.Get('seq_dl', False);
  FFirstLastPiecePrioritized := aJSONObj.Get('f_l_piece_prio', False);
  FCategory := aJSONObj.Get('category', '');
  FSuperSeeding := aJSONObj.Get('super_seeding', False);
  FForceStart := aJSONObj.Get('force_start', False);
  FSavePath := aJSONObj.Get('save_path', '');

  iUnixTime := aJSONObj.Get('added_on', 0);
  if iUnixTime > 0 then
  begin
    FAddedOn := UnixToDateTime(iUnixTime);
  end;

  iUnixTime := aJSONObj.Get('completion_on', 0);
  if iUnixTime > 0 then
  begin
    FCompletionOn := UnixToDateTime(iUnixTime);
  end;
end;

procedure TqBTorrent.DoLoadFromStream(const aStream: TStream);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  jParser := TJSONParser.Create(aStream, [joUTF8, joIgnoreTrailingComma]);
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

constructor TqBTorrent.Create;
begin
  FName := '';
  FHash := '';
  FSize := 0;
  FProgress := 0.0;
  FDlSpeed := 0;
  FUpSpeed := 0;
  FPriority := -1;
  FNumSeeds := 0;
  FNumComplete := 0;
  FNumLeechs := 0;
  FNumIncomplete := 0;
  FRatio := 0.0;
  FEta := 0;
  FState := tsUnknown;
  FSeqDl := False;
  FFirstLastPiecePrioritized := False;
  FCategory := '';
  FSuperSeeding := False;
  FForceStart := False;
  FSavePath := '';
  FAddedOn := Now;
  FCompletionOn := Now;
end;

constructor TqBTorrent.Create(const aJSON: String);
begin
  Self.Create;
  DoLoadFromJSON(aJSON);
end;

constructor TqBTorrent.Create(const aJSONObj: TJSONObject);
begin
  Self.Create;
  DoLoadFromJSONObj(aJSONObj);
end;

constructor TqBTorrent.Create(const aStream: TStream);
begin
  Self.Create;
  DoLoadFromStream(aStream);
end;

destructor TqBTorrent.Destroy;
begin
  inherited Destroy;
end;

{ TqBTorrents }

function TqBTorrents.GetItem(Index: Integer): TqBTorrent;
begin
  Result := TqBTorrent(inherited GetItem(Index));
end;

procedure TqBTorrents.SetItem(Index: Integer; AObject: TqBTorrent);
begin
  inherited SetItem(Index, AObject);
end;

procedure TqBTorrents.LoadFromJSON(const aJSON: String);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  jParser := TJSONParser.Create(aJSON, [joUTF8, joIgnoreTrailingComma]);
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtArray then
      begin
        LoadFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.LoadFromJSONArray(const aJSONArray: TJSONArray);
var
  index: Integer;
begin
  Clear;
  for index := 0 to aJSONArray.Count - 1 do
  begin
    if aJSONArray[index].JSONType = jtObject then
    begin
      Add(TqBTorrent.Create(aJSONArray[index] as TJSONObject));
    end;
  end;
end;

procedure TqBTorrents.LoadFromStream(const aStream: TStream);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  jParser := TJSONParser.Create(aStream, [joUTF8, joIgnoreTrailingComma]);
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtArray then
      begin
        LoadFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateFromJSON(const aJSON: String);
begin
end;

procedure TqBTorrents.UpdateFromJSONArray(const aJSONArray: TJSONArray);
begin

end;

procedure TqBTorrents.UpdateFromStream(const aStream: TStream);
begin

end;

end.

