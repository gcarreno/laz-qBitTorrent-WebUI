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

{ TqBTorrentProperties }
  TqBTorrentProperties = class(TObject)
  private
    FSavePath: String;
    FCreationDate: TDateTime;
    FPieceSize: Integer;
    FComment: String;
    FTotalWasted: Integer;
    FTotalUploaded: Integer;
    FTotalUploadedSession: Integer;
    FTotalDownloaded: Integer;
    FTotalDownloadedSession: Integer;
    FUpLimit: Integer;
    FDlLimit: Integer;
    FTimeElapsed: Integer; // seconds
    FSeedingTime: Integer; // seconds
    FNbConnections: Integer;
    FNbConnectionsLimit: Integer;
    FShareRatio: Double;
    FAdditionDate: TDateTime;
    FCompletionDate: TDateTime;
    FCreatedBy: String;
    FDlSpeed: Integer;
    FDlSpeedAvg: Integer;
    FUpSpeed: Integer;
    FUpSpeedAvg: Integer;
    FEta: Integer; // seconds
    FLastSeen: TDateTime;
    FPeers: Integer;
    FPeersTotal: Integer;
    FPiecesHave: Integer;
    FPiecesNum: Integer;
    FReannounce: Integer;
    FSeeds: Integer;
    FSeedsTotal: Integer;
    FTotalSize: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJSON(const aJSON: String);
    procedure LoadFromJSONObj(const aJSONObj: TJSONObject);
    procedure LoadFromStream(const aStream: TStream);

    property SavePath: String
      read FSavePath
      write FSavePath;
    property CreationDate: TDateTime
      read FCreationDate
      write FCreationDate;
    property PieceSize: Integer
      read FPieceSize
      write FPieceSize;
    property Comment: String
      read FComment
      write FComment;
    property TotalWasted: Integer
      read FTotalWasted
      write FTotalWasted;
    property TotalUploaded: Integer
      read FTotalUploaded
      write FTotalUploaded;
    property TotalUploadedSession: Integer
      read FTotalUploadedSession
      write FTotalUploadedSession;
    property TotalDownloaded: Integer
      read FTotalDownloaded
      write FTotalDownloaded;
    property TotalDownloadedSession: Integer
      read FTotalDownloadedSession
      write FTotalDownloadedSession;
    property UpLimit: Integer
      read FUpLimit
      write FUpLimit;
    property DlLimit: Integer
      read FDlLimit
      write FDlLimit;
    property TimeElapsed: Integer
      read FTimeElapsed
      write FTimeElapsed;
    property SeedingTime: Integer
      read FSeedingTime
      write FSeedingTime;
    property NbConnections: Integer
      read FNbConnections
      write FNbConnections;
    property NbConnectionsLimit: Integer
      read FNbConnectionsLimit
      write FNbConnectionsLimit;
    property ShareRatio: Double
      read FShareRatio
      write FShareRatio;
    property AdditionDate: TDateTime
      read FAdditionDate
      write FAdditionDate;
    property CompletionDate: TDateTime
      read FCompletionDate
      write FCompletionDate;
    property CreatedBy: String
      read FCreatedBy
      write FCreatedBy;
    property DlSpeed: Integer
      read FDlSpeed
      write FDlSpeed;
    property DlSpeedAvg: Integer
      read FDlSpeedAvg
      write FDlSpeedAvg;
    property UpSpeed: Integer
      read FUpSpeed
      write FUpSpeed;
    property UpSpeedAvg: Integer
      read FUpSpeedAvg
      write FUpSpeedAvg;
    property Eta: Integer
      read FEta
      write FEta;
    property LastSeen: TDateTime
      read FLastSeen
      write FLastSeen;
    property Peers: Integer
      read FPeers
      write FPeers;
    property PeersTotal: Integer
      read FPeersTotal
      write FPeersTotal;
    property PiecesHave: Integer
      read FPiecesHave
      write FPiecesHave;
    property PiecesNum: Integer
      read FPiecesNum
      write FPiecesNum;
    property Reannounce: Integer
      read FReannounce
      write FReannounce;
    property Seeds: Integer
      read FSeeds
      write FSeeds;
    property SeedsTotal: Integer
      read FSeedsTotal
      write FSeedsTotal;
    property TotalSize: Integer
      read FTotalSize
      write FTotalSize;
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

    FProperties: TqBTorrentProperties;

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

    procedure LoadFromJSON(const aJSON: String);
    procedure LoadFromJSONObj(const aJSONObj: TJSONObject);
    procedure LoadFromStream(const aStream: TStream);

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
  end;

{ TqBTorrents }
  TqBTorrents = class(TFPObjectList)
  private
    function GetItem(Index: Integer): TqBTorrent;
    procedure SetItem(Index: Integer; AObject: TqBTorrent);
  protected
  public
    function HasTorrentHASH(const aHASH: String):Boolean;

    procedure LoadTorrentsFromJSON(const aJSON: String);
    procedure LoadTorrentsFromJSONArray(const aJSONArray: TJSONArray);
    procedure LoadTorrentsFromStream(const aStream: TStream);

    procedure UpdateTorrentsFromJSON(const aJSON: String);
    procedure UpdateTorrentsFromJSONArray(const aJSONArray: TJSONArray);
    procedure UpdateTorrentsFromStream(const aStream: TStream);

    procedure UpdateTorrent(const aHash: String; const aJSON: String);
    procedure UpdateTorrent(const aHash: String; const aJSONObj: TJSONObject);
    procedure UpdateTorrent(const aHash: String; const aStream: TStream);

    procedure UpdateTorrentProperties(const aHash: String; const aJSON: String);
    procedure UpdateTorrentProperties(const aHash: String; const aJSONObj: TJSONObject);
    procedure UpdateTorrentProperties(const aHash: String; const aStream: TStream);

    procedure DeleteTorrent(const aHash: String);

    // This should probably be a property
    function ByHash(const aHash: String): TqBTorrent;

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

{ TqBTorrentProperties }

constructor TqBTorrentProperties.Create;
begin
  FSavePath := '';
  FCreationDate := 0.0;
  FPieceSize := -1;
  FComment := '';
  FTotalWasted := -1;
  FTotalUploaded := -1;
  FTotalUploadedSession := -1;
  FTotalDownloaded := -1;
  FTotalDownloadedSession := -1;
  FUpLimit := -1;
  FDlLimit := -1;
  FTimeElapsed := -1;
  FSeedingTime := -1;
  FNbConnections := -1;
  FNbConnectionsLimit := -1;
  FShareRatio := 0.0;
  FAdditionDate := 0.0;
  FCompletionDate := 0.0;
  FCreatedBy := '';
  FDlSpeed := -1;
  FDlSpeedAvg := -1;
  FUpSpeed := -1;
  FUpSpeedAvg := -1;
  FEta := -1;
  FLastSeen := 0.0;
  FPeers := -1;
  FPeersTotal := -1;
  FPiecesHave := -1;
  FPiecesNum := -1;
  FReannounce := -1;
  FSeeds := -1;
  FSeedsTotal := -1;
  FTotalSize := -1;
end;

destructor TqBTorrentProperties.Destroy;
begin
  inherited Destroy;
end;

procedure TqBTorrentProperties.LoadFromJSON(const aJSON: String);
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
        LoadFromJSONObj(jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentProperties.LoadFromJSONObj(const aJSONObj: TJSONObject);
var
  iUnixTime: Integer;
  dtTime: TDateTime;
begin
  FSavePath := aJSONObj.Get('save_path', FSavePath);

  iUnixTime := aJSONObj.Get('save_path', DateTimeToUnix(FCreationDate));
  dtTime := UnixToDateTime(iUnixTime);
  if (FCreationDate <> dtTime) and (iUnixTime > 0) then
  begin
   FCreationDate := dtTime;
  end;

  FPieceSize := aJSONObj.Get('piece_size', FPieceSize);
  FComment := aJSONObj.Get('comment', FComment);
  FTotalWasted := aJSONObj.Get('total_wasted', FTotalWasted);
  FTotalUploaded := aJSONObj.Get('total_uploaded', FTotalUploaded);
  FTotalUploadedSession := aJSONObj.Get('total_uploaded_session', FTotalUploadedSession);
  FTotalDownloaded := aJSONObj.Get('total_downloaded', FTotalDownloaded);
  FTotalDownloadedSession := aJSONObj.Get('total_downloaded_session', FTotalDownloadedSession);
  FUpLimit := aJSONObj.Get('up_limit', FUpLimit);
  FDlLimit := aJSONObj.Get('dl_limit', FDlLimit);
  FTimeElapsed := aJSONObj.Get('time_elapsed', FTimeElapsed);
  FSeedingTime := aJSONObj.Get('seeding_time', FSeedingTime);
  FNbConnections := aJSONObj.Get('nb_connections', FNbConnections);
  FNbConnectionsLimit := aJSONObj.Get('nb_connections_limit', FNbConnectionsLimit);
  FShareRatio := aJSONObj.Get('share_ratio', FShareRatio);

  iUnixTime := aJSONObj.Get('addition_date', DateTimeToUnix(FAdditionDate));
  dtTime := UnixToDateTime(iUnixTime);
  if (FAdditionDate <> dtTime) and (iUnixTime > 0) then
  begin
    FAdditionDate := dtTime;
  end;

  iUnixTime := aJSONObj.Get('completion_date', DateTimeToUnix(FCompletionDate));
  dtTime := UnixToDateTime(iUnixTime);
  if (FCompletionDate <> dtTime) and (iUnixTime > 0) then
  begin
   FCompletionDate := dtTime;
  end;

  FCreatedBy := aJSONObj.Get('created_by', FCreatedBy);
  FDlSpeed := aJSONObj.Get('dl_speed', FDlSpeed);
  FDlSpeedAvg := aJSONObj.Get('dl_speed_avg', FDlSpeedAvg);
  FUpSpeed := aJSONObj.Get('up_speed', FUpSpeed);
  FUpSpeedAvg := aJSONObj.Get('up_speed_avg', FUpSpeedAvg);
  FEta := aJSONObj.Get('eta', FEta);

  iUnixTime := aJSONObj.Get('last_seen', DateTimeToUnix(FLastSeen));
  dtTime := UnixToDateTime(iUnixTime);
  if (FLastSeen <> dtTime) and (iUnixTime > 0) then
  begin
    FLastSeen := dtTime;
  end;

  FPeers := aJSONObj.Get('peers', FPeers);
  FPeersTotal := aJSONObj.Get('peers_total', FPeersTotal);
  FPiecesHave := aJSONObj.Get('pieces_have', FPiecesHave);
  FPiecesNum := aJSONObj.Get('pieces_num', FPiecesNum);
  FReannounce := aJSONObj.Get('reannounce', FReannounce);
  FSeeds := aJSONObj.Get('seeds', FSeeds);
  FSeedsTotal := aJSONObj.Get('seeds_total', FSeedsTotal);
  FTotalSize := aJSONObj.Get('total_size', FTotalSize);
end;

procedure TqBTorrentProperties.LoadFromStream(const aStream: TStream);
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
        LoadFromJSONObj(jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
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

procedure TqBTorrent.LoadFromJSON(const aJSON: String);
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
  FState := StrToqBState(aJSONObj.Get('state', qBStateToStr(FState)));
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

procedure TqBTorrent.LoadFromJSONObj(const aJSONObj: TJSONObject);
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

procedure TqBTorrent.LoadFromStream(const aStream: TStream);
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
  FState := tsUnknown;
  FSeqDl := False;
  FFirstLastPiecePrioritized := False;
  FCategory := '';
  FSuperSeeding := False;
  FForceStart := False;
  FSavePath := '';
  FAddedOn := 0.0;
  FCompletionOn := 0.0;

  FProperties := TqBTorrentProperties.Create;
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
  FProperties.Free;
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

function TqBTorrents.HasTorrentHASH(const aHASH: String): Boolean;
var
  index: Integer;
begin
  Result := False;
  if Length(aHASH) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Self[index].Hash = aHASH then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure TqBTorrents.LoadTorrentsFromJSON(const aJSON: String);
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
        LoadTorrentsFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.LoadTorrentsFromJSONArray(const aJSONArray: TJSONArray);
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

procedure TqBTorrents.LoadTorrentsFromStream(const aStream: TStream);
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
        LoadTorrentsFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrentsFromJSON(const aJSON: String);
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
        UpdateTorrentsFromJSONArray(jData as TJSONArray);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrents.UpdateTorrentsFromJSONArray(const aJSONArray: TJSONArray);
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
        oTorrent := Self[index1];
        if oTorrent.Hash = TJSONObject(jData).Get('hash', '') then
        begin
          break;
        end;
      end;
      if Assigned(oTorrent) then
      begin
        oTorrent.LoadFromJSONObj(jData as TJSONObject);
      end
      else
      begin
        Add(TqBTorrent.Create(jData as TJSONObject));
      end;
    end;
  end;
end;

procedure TqBTorrents.UpdateTorrentsFromStream(const aStream: TStream);
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
        UpdateTorrentsFromJSONArray(jData as TJSONArray);
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
    if Self[index].Hash = aHash then
    begin
      Self[index].LoadFromJSON(aJSON);
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
    if Self[index].Hash = aHash then
    begin
      Self[index].LoadFromJSONObj(aJSONObj);
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
    if Self[index].Hash = aHash then
    begin
      Self[index].LoadFromStream(aStream);
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
    if Self[index].Hash = aHash then
    begin
      Self[index].Properties.LoadFromJSONObj(aJSONObj);
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

procedure TqBTorrents.DeleteTorrent(const aHash: String);
var
  index: Integer;
begin
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Self[index].Hash = aHash then
    begin
      Delete(index);
      break;
    end;
  end;
end;

function TqBTorrents.ByHash(const aHash: String): TqBTorrent;
var
  index: Integer;
begin
  Result := nil;
  if Length(aHash) <> 40 then
    exit;
  for index := 0 to Count - 1 do
  begin
    if Self[index].Hash = aHash then
    begin
      Result := Self[index];
      break;
    end;
  end;
end;

end.

