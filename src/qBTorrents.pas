{
  Torrent List Container

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
  Classes, Contnrs, SysUtils, DateUtils, fpjson, jsonparser, jsonscanner,
  qBTorrentsFilters, qBTorrentsProperties, qBTorrentsTrackers;

type
{ TqBTorrentStates }
  TqBTorrentState = (
    qtsError,
    qtsPausedUp,
    qtsPausedDl,
    qtsQueuedUp,
    qtsQueuedDl,
    qtsUploading,
    qtsStalledUp,
    qtsStalledDl,
    qtsCheckingUp,
    qtsCheckingDl,
    qtsDownloading,
    qtsMetaDl,
    qtsUnknown
  );

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

    FProperties: TqBTorrentProperties;
    FTrackers: TqBTorrentsTrackers;

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

    procedure Load(const aJSON: String);
    procedure Load(const aJSONObj: TJSONObject);
    procedure Load(const aStream: TStream);

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

    property Properties: TqBTorrentProperties
      read FProperties;
    property Trackers: TqBTorrentsTrackers
      read FTrackers;
  end;

{ TqBTorrents }
  TqBTorrents = class(TFPObjectList)
  private
    function GetByIndex(Index: Integer): TqBTorrent;

    function GetByHash(const Hash:String): TqBTorrent;
  protected
  public
    function HasTorrentHASH(const aHASH: String):Boolean;

    // Torrents
    procedure LoadTorrents(const aJSON: String);
    procedure LoadTorrents(const aJSONArray: TJSONArray);
    procedure LoadTorrents(const aStream: TStream);

    procedure UpdateTorrents(const aJSON: String);
    procedure UpdateTorrents(const aJSONArray: TJSONArray);
    procedure UpdateTorrents(const aStream: TStream);

    procedure UpdateTorrent(const aHash: String; const aJSON: String);
    procedure UpdateTorrent(const aHash: String; const aJSONObj: TJSONObject);
    procedure UpdateTorrent(const aHash: String; const aStream: TStream);

    procedure DeleteTorrent(const aHash: String);

    // Torrent Properties
    procedure UpdateTorrentProperties(const aHash: String; const aJSON: String);
    procedure UpdateTorrentProperties(const aHash: String; const aJSONObj: TJSONObject);
    procedure UpdateTorrentProperties(const aHash: String; const aStream: TStream);

    // Torrent Trackers
    procedure UpdateTorrentTrackers(const aHash: String; const aJSON: String);
    procedure UpdateTorrentTrackers(const aHash: String; const aJSONArray: TJSONArray);
    procedure UpdateTorrentTrackers(const aHash: String; const aStream: TStream);


    property Items[Index: Integer]: TqBTorrent
      read GetByIndex; default;
    property Hashes[Hash: String]: TqBTorrent
      read GetByHash;
  end;

// Helper functions
function StrToqBTorrentState(const aState: String): TqBTorrentState;
function qBTorrentStateToStr(aState: TqBTorrentState): String;

implementation

const
  csStateError = 'error';
  csStatePausedUp = 'pausedUP';
  csStatePausedDl = 'pausedDL';
  csStateQueuedUp = 'queuedUP';
  csStateQueuedDl = 'queuedDL';
  csStateUploading = 'uploading';
  csStateStalledUp = 'stalledUP';
  csStateStalledDl = 'stalledDL';
  csStateCheckingUp = 'checkingUP';
  csStateCheckingDl = 'checkingDL';
  csStateDownloading = 'downloading';
  csStateMetaDl = 'metaDL';
  csStateUnknown = 'unknown';

// Helper functions

function StrToqBTorrentState(const aState: String): TqBTorrentState;
begin
  Result := qtsUnknown;
  if aState = csStateError then
  begin
    Result := qtsError;
    exit;
  end;
  if aState = csStatePausedUp then
  begin
    Result := qtsPausedUp;
    exit;
  end;
  if aState = csStatePausedDl then
  begin
    Result := qtsPausedDl;
    exit;
  end;
  if aState = csStateQueuedUp then
  begin
    Result := qtsQueuedUp;
    exit;
  end;
  if aState = csStateQueuedDl then
  begin
    Result := qtsQueuedDl;
    exit;
  end;
  if aState = csStateUploading then
  begin
    Result := qtsUploading;
    exit;
  end;
  if aState = csStateStalledUp then
  begin
    Result := qtsStalledUp;
    exit;
  end;
  if aState = csStateStalledDl then
  begin
    Result := qtsStalledDl;
    exit;
  end;
  if aState = csStateCheckingUp then
  begin
    Result := qtsCheckingUp;
    exit;
  end;
  if aState = csStateCheckingDl then
  begin
    Result := qtsCheckingDl;
    exit;
  end;
  if aState = csStateDownloading then
  begin
    Result := qtsDownloading;
    exit;
  end;
  if aState = csStateMetaDl then
  begin
    Result := qtsMetaDl;
    exit;
  end;
end;

function qBTorrentStateToStr(aState: TqBTorrentState): String;
begin
  Result := csStateUnknown;
  case aState of
    qtsError:       Result := csStateError;
    qtsPausedUp:    Result := csStatePausedUp;
    qtsPausedDl:    Result := csStatePausedDl;
    qtsQueuedUp:    Result := csStateQueuedUp;
    qtsQueuedDl:    Result := csStateQueuedDl;
    qtsUploading:   Result := csStateUploading;
    qtsStalledUp:   Result := csStateStalledUp;
    qtsStalledDl:   Result := csStateStalledDl;
    qtsCheckingUp:  Result := csStateCheckingUp;
    qtsCheckingDl:  Result := csStateCheckingDl;
    qtsDownloading: Result := csStateDownloading;
    qtsMetaDl:      Result := csStateMetaDl;
  end;
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

procedure TqBTorrent.Load(const aJSON: String);
begin
  DoLoadFromJSON(aJSON);
end;

procedure TqBTorrent.DoLoadFromJSONObj(const aJSONObj: TJSONObject);
var
  iUnixTime: Integer;
  dtTime: TDateTime;
begin
  FHash := aJSONObj.Get('hash', FHash);
  FName := aJSONObj.Get('name', FName);
  FSize := aJSONObj.Get('size', FSize);
  FProgress := aJSONObj.Get('progress', FProgress);
  FDlSpeed := aJSONObj.Get('dlspeed', FDlSpeed);
  FUpSpeed := aJSONObj.Get('upspeed', FUpSpeed);
  FPriority := aJSONObj.Get('priority', FPriority);
  FNumSeeds := aJSONObj.Get('num_seeds', FNumSeeds);
  FNumComplete := aJSONObj.Get('num_complete', FNumComplete);
  FNumLeechs := aJSONObj.Get('num_leechs', FNumLeechs);
  FNumIncomplete := aJSONObj.Get('num_incomplete', FNumIncomplete);
  FRatio := aJSONObj.Get('ratio', FRatio);
  FEta := aJSONObj.Get('eta', FEta);
  FState := StrToqBTorrentState(aJSONObj.Get('state', qBTorrentStateToStr(FState)));
  FSeqDl := aJSONObj.Get('seq_dl', FSeqDl);
  FFirstLastPiecePrioritized := aJSONObj.Get('f_l_piece_prio', FFirstLastPiecePrioritized);
  FCategory := aJSONObj.Get('category', FCategory);
  FSuperSeeding := aJSONObj.Get('super_seeding', FSuperSeeding);
  FForceStart := aJSONObj.Get('force_start', FForceStart);
  FSavePath := aJSONObj.Get('save_path', FSavePath);

  iUnixTime := aJSONObj.Get('added_on', 0);
  dtTime := UnixToDateTime(iUnixTime);
  if (FAddedOn <> dtTime) and (iUnixTime > 0) then
  begin
    FAddedOn := dtTime;
  end;

  iUnixTime := aJSONObj.Get('completion_on', 0);
  dtTime := UnixToDateTime(iUnixTime);
  if (FCompletionOn <> dtTime) and (iUnixTime > 0) then
  begin
    FCompletionOn := dtTime;
  end;
end;

procedure TqBTorrent.Load(const aJSONObj: TJSONObject);
begin
  DoLoadFromJSONObj(aJSONObj);
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

procedure TqBTorrent.Load(const aStream: TStream);
begin
  DoLoadFromStream(aStream);
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
  FState := qtsUnknown;
  FSeqDl := False;
  FFirstLastPiecePrioritized := False;
  FCategory := '';
  FSuperSeeding := False;
  FForceStart := False;
  FSavePath := '';
  FAddedOn := 0.0;
  FCompletionOn := 0.0;

  FProperties := TqBTorrentProperties.Create;
  FTrackers := TqBTorrentsTrackers.Create(True);
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
  FTrackers.Free;
  FProperties.Free;
  inherited Destroy;
end;

{ TqBTorrents }

function TqBTorrents.GetByIndex(Index: Integer): TqBTorrent;
begin
  Result := inherited Items[Index] as TqBTorrent;
end;

function TqBTorrents.GetByHash(const Hash: String): TqBTorrent;
var
  index: Integer;
begin
  Result := nil;
  if Length(Hash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = Hash then
    begin
      Result := Items[index];
      break;
    end;
  end;
end;

function TqBTorrents.HasTorrentHASH(const aHASH: String): Boolean;
var
  index: Integer;
begin
  Result := False;
  if Length(aHASH) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHASH then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure TqBTorrents.LoadTorrents(const aJSON: String);
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
        LoadTorrents(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.LoadTorrents(const aJSONArray: TJSONArray);
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

procedure TqBTorrents.LoadTorrents(const aStream: TStream);
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
        LoadTorrents(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrents(const aJSON: String);
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
        UpdateTorrents(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrents(const aJSONArray: TJSONArray);
var
  index, index1: Integer;
  oTorrent: TqBTorrent;
  jData: TJSONData;
begin
  for index := 0 to aJSONArray.Count - 1 do
  begin
    jData := aJSONArray[index];
    oTorrent := nil;
    if jData.JSONType = jtObject then
    begin
      for index1 := 0 to Count - 1 do
      begin
        oTorrent := Items[index1];
        if oTorrent.Hash = TJSONObject(jData).Get('hash', '') then
        begin
          break;
        end;
      end;
      if Assigned(oTorrent) then
      begin
        oTorrent.Load(jData as TJSONObject);
      end
      else
      begin
        Add(TqBTorrent.Create(jData as TJSONObject));
      end;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrents(const aStream: TStream);
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
        UpdateTorrents(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrent(const aHash: String; const aJSON: String);
var
  index: Integer;
begin
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Load(aJSON);
      break;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrent(const aHash: String; const aJSONObj: TJSONObject);
var
  index: Integer;
begin
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Load(aJSONObj);
      break;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrent(const aHash: String; const aStream: TStream);
var
  index: Integer;
begin
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Load(aStream);
      break;
    end;
  end;
end;

procedure TqBTorrents.DeleteTorrent(const aHash: String);
var
  index: Integer;
begin
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Delete(index);
      break;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrentProperties(const aHash: String; const aJSON: String);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  if Length(aHash) <> 40 then
    exit;
  jParser := TJSONParser.Create(aJSON, [joUTF8, joIgnoreTrailingComma]);
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtObject then
      begin
        UpdateTorrentProperties(aHash, jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrentProperties(const aHash: String; const aJSONObj: TJSONObject);
var
  index: Integer;
begin
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Properties.LoadFromJSONObj(aJSONObj);
      break;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrentProperties(const aHash: String; const aStream: TStream);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  if Length(aHash) <> 40 then
    exit;
  jParser := TJSONParser.Create(aStream, [joUTF8, joIgnoreTrailingComma]);
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtObject then
      begin
        UpdateTorrentProperties(aHash, jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrentTrackers(const aHash: String; const aJSON: String);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Trackers.UpdateTrackers(aJSON);
      break;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrentTrackers(const aHash: String; const aJSONArray: TJSONArray);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Trackers.UpdateTrackers(aJSONArray);
      break;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrentTrackers(const aHash: String; const aStream: TStream);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Hash = aHash then
    begin
      Items[index].Trackers.UpdateTrackers(aStream);
      break;
    end;
  end;
end;

end.

